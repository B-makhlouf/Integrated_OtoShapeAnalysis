# ============================================================================
# BATCH OTOLITH ALIGNMENT SCRIPT
# ============================================================================
# Purpose: Aligns all otolith samples using left-to-right axis orientation,
#          generates visualization plots, and updates the ShapeR object
# Author: [Your Name]
# Date: [Current Date]
# Version: 2.0
# ============================================================================

# LIBRARY DEPENDENCIES ======================================================
library(ggplot2)
library(gridExtra)
library(grid)  # Required for textGrob function

# ENVIRONMENT SETUP ==========================================================
# Clear the R environment for a fresh start
rm(list = ls())

# CONFIGURATION PARAMETERS ===================================================
# File paths
DATA_INPUT_PATH <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_outlinesQCd.RData"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Figures/Alignment"
DATA_OUTPUT_PATH <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/otoOutlines_Aligned.RData"

# Plot parameters
PLOT_SUBSAMPLE_POINTS <- 500  # Maximum points to plot (for performance)
PLOT_WIDTH <- 12             # Plot width in inches
PLOT_HEIGHT <- 5             # Plot height in inches  
PLOT_DPI <- 300             # Plot resolution

# Alignment parameters
TARGET_ANGLE_DEGREES <- 0    # Target angle for left-to-right axis (horizontal)

# DATA LOADING ===============================================================
cat("=== LOADING DATA ===\n")
if (!file.exists(DATA_INPUT_PATH)) {
  stop(paste("Input data file not found:", DATA_INPUT_PATH))
}

load(DATA_INPUT_PATH)
cat(paste("✓ Loaded data from:", DATA_INPUT_PATH, "\n"))

# Verify the required object exists
if (!exists("outlinesonly")) {
  stop("Required 'outlinesonly' object not found in loaded data")
}

# OUTPUT DIRECTORY MANAGEMENT ================================================
cat("\n=== SETTING UP OUTPUT DIRECTORY ===\n")

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(paste("✓ Created output directory:", OUTPUT_DIR, "\n"))
} else {
  cat(paste("✓ Output directory exists:", OUTPUT_DIR, "\n"))
}

# Clear existing files in the alignment folder
existing_files <- list.files(OUTPUT_DIR, pattern = "*.png$", full.names = TRUE)
if (length(existing_files) > 0) {
  cat(paste("  Removing", length(existing_files), "existing PNG files...\n"))
  file.remove(existing_files)
  cat("  ✓ Alignment folder cleared successfully\n")
} else {
  cat("  ✓ Alignment folder is already empty\n")
}

# CORE FUNCTIONS =============================================================

#' Align a Single Otolith Sample
#' 
#' This function takes an otolith outline and rotates it so that the line
#' connecting the furthest left and right points from the centroid becomes
#' horizontal (0 degrees).
#' 
#' @param outlinesonly ShapeR object containing otolith outlines
#' @param group_index Index of the group within the outline list
#' @param sample_name Name of the sample to align
#' @return List containing alignment results and transformed coordinates
align_otolith <- function(outlinesonly, group_index, sample_name) {
  
  cat(paste("  Processing:", sample_name, "\n"))
  
  # Extract outline data from ShapeR object
  outline_data <- outlinesonly@outline.list[[group_index]][[sample_name]]
  
  # Extract coordinate vectors
  x_coords <- outline_data$X
  y_coords <- outline_data$Y
  
  # Validate coordinate data
  if (is.null(x_coords) || is.null(y_coords) || length(x_coords) == 0) {
    warning(paste("No valid coordinates for", sample_name))
    return(list(success = FALSE, message = "No valid coordinates"))
  }
  
  # Calculate geometric centroid
  centroid_x <- mean(x_coords, na.rm = TRUE)
  centroid_y <- mean(y_coords, na.rm = TRUE)
  
  # Calculate distances from each point to centroid
  distances <- sqrt((x_coords - centroid_x)^2 + (y_coords - centroid_y)^2)
  
  # Find reference points for left-to-right axis alignment
  # Left reference: furthest point left of centroid
  left_points_indices <- which(x_coords < centroid_x)
  if (length(left_points_indices) == 0) {
    warning(paste("No points to the left of centroid for", sample_name))
    return(list(success = FALSE, message = "No left points found"))
  }
  
  left_distances <- distances[left_points_indices]
  max_left_dist_idx <- which.max(left_distances)
  left_reference_idx <- left_points_indices[max_left_dist_idx]
  
  left_x <- x_coords[left_reference_idx]
  left_y <- y_coords[left_reference_idx]
  
  # Right reference: furthest point right of centroid
  right_points_indices <- which(x_coords > centroid_x)
  if (length(right_points_indices) == 0) {
    warning(paste("No points to the right of centroid for", sample_name))
    return(list(success = FALSE, message = "No right points found"))
  }
  
  right_distances <- distances[right_points_indices]
  max_right_dist_idx <- which.max(right_distances)
  right_reference_idx <- right_points_indices[max_right_dist_idx]
  
  right_x <- x_coords[right_reference_idx]
  right_y <- y_coords[right_reference_idx]
  
  # Calculate current angle of left-to-right axis
  dx <- right_x - left_x
  dy <- right_y - left_y
  current_angle <- atan2(dy, dx)
  current_angle_degrees <- current_angle * 180 / pi
  
  # Calculate required rotation to achieve target angle
  target_angle <- TARGET_ANGLE_DEGREES * pi / 180
  rotation_angle <- target_angle - current_angle
  rotation_degrees <- rotation_angle * 180 / pi
  
  # Apply rotation transformation around centroid
  cos_rot <- cos(rotation_angle)
  sin_rot <- sin(rotation_angle)
  
  # Translation to origin -> rotation -> translation back
  x_centered <- x_coords - centroid_x
  y_centered <- y_coords - centroid_y
  
  # Rotation matrix application
  x_rotated <- x_centered * cos_rot - y_centered * sin_rot
  y_rotated <- x_centered * sin_rot + y_centered * cos_rot
  
  # Translate back to original centroid position
  x_aligned <- x_rotated + centroid_x
  y_aligned <- y_rotated + centroid_y
  
  # Calculate new positions of reference points for verification
  left_x_new <- x_aligned[left_reference_idx]
  left_y_new <- y_aligned[left_reference_idx]
  right_x_new <- x_aligned[right_reference_idx]
  right_y_new <- y_aligned[right_reference_idx]
  
  # Verify final angle (quality control)
  dx_new <- right_x_new - left_x_new
  dy_new <- right_y_new - left_y_new
  new_angle <- atan2(dy_new, dx_new)
  new_angle_degrees <- new_angle * 180 / pi
  
  # Log transformation details
  cat(paste("    Original angle:", round(current_angle_degrees, 2), "°\n"))
  cat(paste("    Rotation applied:", round(rotation_degrees, 2), "°\n"))
  cat(paste("    Final angle:", round(new_angle_degrees, 2), "°\n"))
  
  # Return comprehensive results
  return(list(
    success = TRUE,
    original_coords = cbind(x = x_coords, y = y_coords),
    aligned_coords = cbind(x = x_aligned, y = y_aligned),
    centroid = c(x = centroid_x, y = centroid_y),
    left_point_original = c(x = left_x, y = left_y),
    right_point_original = c(x = right_x, y = right_y),
    left_point_aligned = c(x = left_x_new, y = left_y_new),
    right_point_aligned = c(x = right_x_new, y = right_y_new),
    original_angle = current_angle_degrees,
    final_angle = new_angle_degrees,
    rotation_degrees = rotation_degrees
  ))
}

#' Create and Save Alignment Visualization
#' 
#' Generates side-by-side plots showing original and aligned otolith orientations
#' with reference points and alignment axis clearly marked.
#' 
#' @param alignment_result Result object from align_otolith function
#' @param sample_name Name of the sample for plot title and filename
#' @param output_dir Directory to save the plot
create_alignment_plot <- function(alignment_result, sample_name, output_dir) {
  
  # Subsample points for plotting performance
  n_points <- nrow(alignment_result$original_coords)
  if (n_points > PLOT_SUBSAMPLE_POINTS) {
    subsample_indices <- seq(1, n_points, by = max(1, round(n_points / PLOT_SUBSAMPLE_POINTS)))
  } else {
    subsample_indices <- 1:n_points
  }
  
  # Prepare original orientation data
  plot_data_orig <- data.frame(
    x = alignment_result$original_coords[subsample_indices, "x"],
    y = alignment_result$original_coords[subsample_indices, "y"]
  )
  
  # Prepare aligned orientation data
  plot_data_aligned <- data.frame(
    x = alignment_result$aligned_coords[subsample_indices, "x"],
    y = alignment_result$aligned_coords[subsample_indices, "y"]
  )
  
  # Create original orientation plot
  p1 <- ggplot(plot_data_orig, aes(x = x, y = y)) +
    geom_polygon(fill = "lightblue", alpha = 0.6, color = "blue", linewidth = 0.5) +
    # Mark centroid
    annotate("point", x = alignment_result$centroid["x"], y = alignment_result$centroid["y"], 
             color = "red", size = 3, shape = 16) +
    # Mark left reference point  
    annotate("point", x = alignment_result$left_point_original["x"], y = alignment_result$left_point_original["y"], 
             color = "darkgreen", size = 3, shape = 17) +
    # Mark right reference point
    annotate("point", x = alignment_result$right_point_original["x"], y = alignment_result$right_point_original["y"], 
             color = "purple", size = 3, shape = 15) +
    # Draw alignment axis
    annotate("segment", x = alignment_result$left_point_original["x"], y = alignment_result$left_point_original["y"],
             xend = alignment_result$right_point_original["x"], yend = alignment_result$right_point_original["y"],
             color = "red", linetype = "solid", linewidth = 1.2) +
    labs(title = "Original Orientation",
         subtitle = paste("Left-to-right axis:", round(alignment_result$original_angle, 1), "°"),
         x = "X coordinate", y = "Y coordinate") +
    coord_equal() +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10))
  
  # Create aligned orientation plot
  p2 <- ggplot(plot_data_aligned, aes(x = x, y = y)) +
    geom_polygon(fill = "lightcoral", alpha = 0.6, color = "darkred", linewidth = 0.5) +
    # Mark centroid
    annotate("point", x = alignment_result$centroid["x"], y = alignment_result$centroid["y"], 
             color = "red", size = 3, shape = 16) +
    # Mark left reference point
    annotate("point", x = alignment_result$left_point_aligned["x"], y = alignment_result$left_point_aligned["y"], 
             color = "darkgreen", size = 3, shape = 17) +
    # Mark right reference point  
    annotate("point", x = alignment_result$right_point_aligned["x"], y = alignment_result$right_point_aligned["y"], 
             color = "purple", size = 3, shape = 15) +
    # Draw alignment axis
    annotate("segment", x = alignment_result$left_point_aligned["x"], y = alignment_result$left_point_aligned["y"],
             xend = alignment_result$right_point_aligned["x"], yend = alignment_result$right_point_aligned["y"],
             color = "red", linetype = "solid", linewidth = 1.2) +
    labs(title = "Aligned Orientation",
         subtitle = paste("Left-to-right axis:", round(alignment_result$final_angle, 1), "°"),
         x = "X coordinate", y = "Y coordinate") +
    coord_equal() +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10))
  
  # Combine plots side by side
  combined_plot <- grid.arrange(p1, p2, ncol = 2, 
                                top = textGrob(paste("Otolith Alignment:", sample_name),
                                               gp = gpar(fontsize = 14, fontface = "bold")))
  
  # Generate filename and save plot
  safe_sample_name <- gsub("[^A-Za-z0-9_-]", "_", sample_name)  # Sanitize filename
  filename <- paste0(safe_sample_name, "_alignment.png")
  filepath <- file.path(output_dir, filename)
  
  ggsave(filepath, combined_plot, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  
  cat(paste("    ✓ Saved plot:", filename, "\n"))
}

# MAIN PROCESSING LOOP =======================================================

cat("\n=== STARTING BATCH ALIGNMENT PROCESSING ===\n")

# Initialize progress counters
total_processed <- 0
successful_alignments <- 0
failed_alignments <- 0
processing_start_time <- Sys.time()

# Process all groups in the ShapeR object
for (group_idx in seq_along(outlinesonly@outline.list)) {
  
  group_data <- outlinesonly@outline.list[[group_idx]]
  
  # Skip empty groups
  if (length(group_data) == 0) {
    cat(paste("⚠ Skipping empty group", group_idx, "\n"))
    next
  }
  
  cat(paste("\n--- Processing Group", group_idx, "---\n"))
  cat(paste("Samples in group:", length(group_data), "\n"))
  
  # Process all samples in current group
  sample_names <- names(group_data)
  
  for (sample_name in sample_names) {
    
    total_processed <- total_processed + 1
    
    # Attempt to align the otolith
    alignment_result <- align_otolith(outlinesonly, group_idx, sample_name)
    
    if (alignment_result$success) {
      
      # Update coordinates in the ShapeR object
      outlinesonly@outline.list[[group_idx]][[sample_name]]$X <- alignment_result$aligned_coords[, "x"]
      outlinesonly@outline.list[[group_idx]][[sample_name]]$Y <- alignment_result$aligned_coords[, "y"]
      
      # Generate visualization plot
      create_alignment_plot(alignment_result, sample_name, OUTPUT_DIR)
      
      successful_alignments <- successful_alignments + 1
      
    } else {
      cat(paste("    ✗ Failed:", alignment_result$message, "\n"))
      failed_alignments <- failed_alignments + 1
    }
  }
}

# Calculate processing time
processing_end_time <- Sys.time()
processing_duration <- round(as.numeric(difftime(processing_end_time, processing_start_time, units = "secs")), 1)

# RESULTS SUMMARY AND OUTPUT =================================================

save(outlinesonly, file = DATA_OUTPUT_PATH)
cat(paste("✓ Updated ShapeR object saved as:", DATA_OUTPUT_PATH, "\n"))

# Final status message
if (failed_alignments == 0) {
  cat("\n🎉 BATCH ALIGNMENT PROCESSING COMPLETED SUCCESSFULLY! 🎉\n")
} else {
  cat(paste("\n⚠ BATCH ALIGNMENT PROCESSING COMPLETED WITH", failed_alignments, "FAILURES ⚠\n"))
  cat("Check the console output above for details on failed samples.\n")
}

