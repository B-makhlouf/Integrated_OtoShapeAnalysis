# Batch alignment script for all otoliths in the dataset
# Aligns all samples using left-to-right axis, saves figures, and updates ShapeR object

library(ggplot2)
library(gridExtra)

# Load the data
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_outlines_redo.RData")

# Create output directory if it doesn't exist
output_dir <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Figures/Alignment"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to align a single otolith
align_otolith <- function(outlinesonly, group_index, sample_name) {
  
  cat(paste("Processing:", sample_name, "\n"))
  
  # Extract outline data
  outline_data <- outlinesonly@outline.list[[group_index]][[sample_name]]
  
  # Extract coordinates
  x_coords <- outline_data$X
  y_coords <- outline_data$Y
  
  # Check if coordinates exist and are valid
  if(is.null(x_coords) || is.null(y_coords) || length(x_coords) == 0) {
    cat(paste("  Warning: No valid coordinates for", sample_name, "- skipping\n"))
    return(list(success = FALSE, message = "No valid coordinates"))
  }
  
  # Calculate centroid
  centroid_x <- mean(x_coords)
  centroid_y <- mean(y_coords)
  
  # Calculate distances from centroid
  distances <- sqrt((x_coords - centroid_x)^2 + (y_coords - centroid_y)^2)
  
  # Find points to the LEFT of centroid
  left_points_indices <- which(x_coords < centroid_x)
  
  if(length(left_points_indices) == 0) {
    cat(paste("  Warning: No points to the left of centroid for", sample_name, "- skipping\n"))
    return(list(success = FALSE, message = "No left points found"))
  }
  
  # Find furthest left point
  left_distances <- distances[left_points_indices]
  max_left_dist_idx <- which.max(left_distances)
  left_reference_idx <- left_points_indices[max_left_dist_idx]
  
  left_x <- x_coords[left_reference_idx]
  left_y <- y_coords[left_reference_idx]
  
  # Find points to the RIGHT of centroid
  right_points_indices <- which(x_coords > centroid_x)
  
  if(length(right_points_indices) == 0) {
    cat(paste("  Warning: No points to the right of centroid for", sample_name, "- skipping\n"))
    return(list(success = FALSE, message = "No right points found"))
  }
  
  # Find furthest right point
  right_distances <- distances[right_points_indices]
  max_right_dist_idx <- which.max(right_distances)
  right_reference_idx <- right_points_indices[max_right_dist_idx]
  
  right_x <- x_coords[right_reference_idx]
  right_y <- y_coords[right_reference_idx]
  
  # Calculate the angle of the line from left point to right point
  dx <- right_x - left_x
  dy <- right_y - left_y
  current_angle <- atan2(dy, dx)
  current_angle_degrees <- current_angle * 180 / pi
  
  # Calculate rotation needed (target: 0 degrees for horizontal left-to-right)
  target_angle <- 0
  rotation_angle <- target_angle - current_angle
  rotation_degrees <- rotation_angle * 180 / pi
  
  # Apply rotation around the centroid
  cos_rot <- cos(rotation_angle)
  sin_rot <- sin(rotation_angle)
  
  # Translate to origin, rotate, translate back
  x_centered <- x_coords - centroid_x
  y_centered <- y_coords - centroid_y
  
  x_rotated <- x_centered * cos_rot - y_centered * sin_rot
  y_rotated <- x_centered * sin_rot + y_centered * cos_rot
  
  x_aligned <- x_rotated + centroid_x
  y_aligned <- y_rotated + centroid_y
  
  # Calculate new positions of reference points
  left_x_new <- x_aligned[left_reference_idx]
  left_y_new <- y_aligned[left_reference_idx]
  right_x_new <- x_aligned[right_reference_idx]
  right_y_new <- y_aligned[right_reference_idx]
  
  # Verify final angle
  dx_new <- right_x_new - left_x_new
  dy_new <- right_y_new - left_y_new
  new_angle <- atan2(dy_new, dx_new)
  new_angle_degrees <- new_angle * 180 / pi
  
  cat(paste("  Original left-to-right angle:", round(current_angle_degrees, 1), "degrees\n"))
  cat(paste("  Rotation applied:", round(rotation_degrees, 1), "degrees\n"))
  cat(paste("  Final left-to-right angle:", round(new_angle_degrees, 1), "degrees\n"))
  
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

# Function to create and save alignment plots
create_alignment_plot <- function(alignment_result, sample_name, output_dir) {
  
  # Subsample for plotting
  n_points <- nrow(alignment_result$original_coords)
  subsample_indices <- seq(1, n_points, by = max(1, round(n_points/500)))
  
  # Original plot data
  plot_data_orig <- data.frame(
    x = alignment_result$original_coords[subsample_indices, "x"],
    y = alignment_result$original_coords[subsample_indices, "y"]
  )
  
  # Aligned plot data
  plot_data_aligned <- data.frame(
    x = alignment_result$aligned_coords[subsample_indices, "x"],
    y = alignment_result$aligned_coords[subsample_indices, "y"]
  )
  
  # Create original plot
  p1 <- ggplot(plot_data_orig, aes(x = x, y = y)) +
    geom_polygon(fill = "lightblue", alpha = 0.6, color = "blue") +
    annotate("point", x = alignment_result$centroid["x"], y = alignment_result$centroid["y"], 
             color = "red", size = 3, shape = 16) +
    annotate("point", x = alignment_result$left_point_original["x"], y = alignment_result$left_point_original["y"], 
             color = "darkgreen", size = 3, shape = 17) +
    annotate("point", x = alignment_result$right_point_original["x"], y = alignment_result$right_point_original["y"], 
             color = "purple", size = 3, shape = 15) +
    annotate("segment", x = alignment_result$left_point_original["x"], y = alignment_result$left_point_original["y"],
             xend = alignment_result$right_point_original["x"], yend = alignment_result$right_point_original["y"],
             color = "red", linetype = "solid", size = 1.2) +
    labs(title = "Original Orientation",
         subtitle = paste("Left-to-right axis at", round(alignment_result$original_angle, 1), "degrees"),
         x = "X coordinate", y = "Y coordinate") +
    coord_equal() +
    theme_minimal() +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  # Create aligned plot
  p2 <- ggplot(plot_data_aligned, aes(x = x, y = y)) +
    geom_polygon(fill = "lightcoral", alpha = 0.6, color = "darkred") +
    annotate("point", x = alignment_result$centroid["x"], y = alignment_result$centroid["y"], 
             color = "red", size = 3, shape = 16) +
    annotate("point", x = alignment_result$left_point_aligned["x"], y = alignment_result$left_point_aligned["y"], 
             color = "darkgreen", size = 3, shape = 17) +
    annotate("point", x = alignment_result$right_point_aligned["x"], y = alignment_result$right_point_aligned["y"], 
             color = "purple", size = 3, shape = 15) +
    annotate("segment", x = alignment_result$left_point_aligned["x"], y = alignment_result$left_point_aligned["y"],
             xend = alignment_result$right_point_aligned["x"], yend = alignment_result$right_point_aligned["y"],
             color = "red", linetype = "solid", size = 1.2) +
    labs(title = "Aligned Orientation",
         subtitle = paste("Left-to-right axis at", round(alignment_result$final_angle, 1), "degrees"),
         x = "X coordinate", y = "Y coordinate") +
    coord_equal() +
    theme_minimal() +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  # Combine plots
  combined_plot <- grid.arrange(p1, p2, ncol = 2, 
                                top = paste("Otolith Alignment:", sample_name))
  
  # Save plot
  filename <- paste0(sample_name, "_alignment.png")
  filepath <- file.path(output_dir, filename)
  
  ggsave(filepath, combined_plot, width = 12, height = 5, dpi = 300)
  
  cat(paste("  Saved plot:", filepath, "\n"))
}

# Main processing loop
cat("=== Starting batch alignment processing ===\n\n")

# Initialize counters
total_processed <- 0
successful_alignments <- 0
failed_alignments <- 0

# Loop through all groups in outline.list
for(group_idx in 1:length(outlinesonly@outline.list)) {
  
  group_data <- outlinesonly@outline.list[[group_idx]]
  
  # Skip empty groups
  if(length(group_data) == 0) {
    next
  }
  
  cat(paste("Processing group", group_idx, "with", length(group_data), "samples\n"))
  
  # Loop through all samples in this group
  sample_names <- names(group_data)
  
  for(sample_name in sample_names) {
    
    total_processed <- total_processed + 1
    
    # Align the otolith
    alignment_result <- align_otolith(outlinesonly, group_idx, sample_name)
    
    if(alignment_result$success) {
      
      # Update coordinates in ShapeR object
      outlinesonly@outline.list[[group_idx]][[sample_name]]$X <- alignment_result$aligned_coords[, "x"]
      outlinesonly@outline.list[[group_idx]][[sample_name]]$Y <- alignment_result$aligned_coords[, "y"]
      
      # Create and save plot
      create_alignment_plot(alignment_result, sample_name, output_dir)
      
      successful_alignments <- successful_alignments + 1
      
    } else {
      cat(paste("  Failed:", alignment_result$message, "\n"))
      failed_alignments <- failed_alignments + 1
    }
  }
}

# Summary
cat("\n=== Processing Summary ===\n")
cat(paste("Total samples processed:", total_processed, "\n"))
cat(paste("Successful alignments:", successful_alignments, "\n"))
cat(paste("Failed alignments:", failed_alignments, "\n"))

# Save updated ShapeR object
output_file <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/otoOutlines_Aligned.RData"
save(outlinesonly, file = output_file)
cat(paste("\nUpdated ShapeR object saved as:", output_file, "\n"))

cat("\n=== Batch alignment processing complete! ===\n")