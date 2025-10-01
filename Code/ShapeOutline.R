# Otolith Shape Analysis: shapeR to Momocs Integration with Image Straightening
# Author: [Your Name]
# Date: September 26, 2025
################################################################################
### Step 1: Use ShapeR to extract the outlines, wavelet and fourier coeff.
# Load required libraries
library(shapeR)
library(Momocs)
library(here)
#install.packages("caret")
#install.packages("randomForest")
library(randomForest)
library(caret)
library(magick)  # For image straightening
library(ggplot2) # For alignment visualization
library(gridExtra) # For plot arrangement
# ###From the top 
# install.packages("shapeR")
library(shapeR)
library(here)
library(grid)
# shape<- shapeR("C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos","FISH.CSV")
# shape<- detect.outline(shape, threshold = .2, write.outline.w.org = TRUE)
# save(shape, file = "C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/oto_outlinesOnly.RData")
# 
# # outlines extracted 09262025
# ok, now lets load those in (so that this script can be used if they're already extracted)
# load(here("Data", "oto_outlinesOnly.RData"))
# 
# shape <- smoothout(shape, n = 100) # Smoothout 
# 
# # remove problematic 
# shape<-remove.outline(shape,"KK","2024_kk_163")
# shape<-remove.outline(shape,"NK","2019_nk_206")
# 
# shape<- generateShapeCoefficients(shape) # Generate Shape Coefficients 
# 
# #Save the coefficients
# save(shape,file = "C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/oto_coefficients.RData")
# Read in 
load(here("Data", "oto_coefficients.RData"))
# Remove .jpg extension from picname column
shape@master.list.org$picname <- gsub("\\.jpg$", "", shape@master.list.org$picname)
#Enrich master list 
shape<- enrich.master.list(shape)
# Plot the average 
plotWaveletShape(shape, "pop")


################################################################################
#### Step 2: Straighten along the axis and produce figures to QC
################################################################################

cat("\n=== APPLYING OTOLITH ALIGNMENT ===\n")

# Create output directory for alignment figures
alignment_output_dir <- here("Figures", "Alignment")
if (!dir.exists(alignment_output_dir)) {
  dir.create(alignment_output_dir, recursive = TRUE)
}

# Clear existing files in the alignment folder
existing_files <- list.files(alignment_output_dir, pattern = "*.png$", full.names = TRUE)
if (length(existing_files) > 0) {
  cat(paste("  Removing", length(existing_files), "existing PNG files...\n"))
  file.remove(existing_files)
  cat("  ✓ Alignment folder cleared successfully\n")
}

# Initialize tracking
total_processed <- 0
successful_alignments <- 0
alignment_results <- list()

# Process all groups and samples
for (group_idx in seq_along(shape@outline.list)) {
  group_data <- shape@outline.list[[group_idx]]
  if (length(group_data) == 0) next
  
  cat(paste("Processing Group", group_idx, "with", length(group_data), "samples\n"))
  
  for (sample_name in names(group_data)) {
    total_processed <- total_processed + 1
    
    # Extract coordinates
    outline_data <- shape@outline.list[[group_idx]][[sample_name]]
    x_coords <- outline_data$X
    y_coords <- outline_data$Y
    
    # Skip if invalid coordinates
    if (is.null(x_coords) || is.null(y_coords) || length(x_coords) == 0) {
      cat(paste("  Skipped:", sample_name, "- No valid coordinates\n"))
      next
    }
    
    # Calculate centroid
    centroid_x <- mean(x_coords, na.rm = TRUE)
    centroid_y <- mean(y_coords, na.rm = TRUE)
    
    # Calculate distances from centroid
    distances <- sqrt((x_coords - centroid_x)^2 + (y_coords - centroid_y)^2)
    
    # Find furthest left and right points from centroid
    left_points <- which(x_coords < centroid_x)
    right_points <- which(x_coords > centroid_x)
    
    if (length(left_points) == 0 || length(right_points) == 0) {
      cat(paste("  Skipped:", sample_name, "- No left/right points\n"))
      next
    }
    
    # Get extreme points
    left_idx <- left_points[which.max(distances[left_points])]
    right_idx <- right_points[which.max(distances[right_points])]
    
    left_x <- x_coords[left_idx]
    left_y <- y_coords[left_idx]
    right_x <- x_coords[right_idx]
    right_y <- y_coords[right_idx]
    
    # Calculate rotation angle to make left-right axis horizontal
    dx <- right_x - left_x
    dy <- right_y - left_y
    current_angle <- atan2(dy, dx)
    rotation_angle <- -current_angle  # Rotate to horizontal (0 degrees)
    rotation_degrees <- rotation_angle * 180 / pi
    
    # Apply rotation around centroid
    cos_rot <- cos(rotation_angle)
    sin_rot <- sin(rotation_angle)
    
    x_centered <- x_coords - centroid_x
    y_centered <- y_coords - centroid_y
    
    x_rotated <- x_centered * cos_rot - y_centered * sin_rot
    y_rotated <- x_centered * sin_rot + y_centered * cos_rot
    
    x_aligned <- x_rotated + centroid_x
    y_aligned <- y_rotated + centroid_y
    
    # Update coordinates in shapeR object
    shape@outline.list[[group_idx]][[sample_name]]$X <- x_aligned
    shape@outline.list[[group_idx]][[sample_name]]$Y <- y_aligned
    
    # CREATE INDIVIDUAL PLOT FOR EACH SAMPLE
    # Subsample points for plotting performance
    n_points <- length(x_coords)
    if (n_points > 500) {
      idx <- seq(1, n_points, by = max(1, round(n_points / 500)))
    } else {
      idx <- 1:n_points
    }
    
    # Prepare original orientation data
    plot_data_orig <- data.frame(
      x = x_coords[idx],
      y = y_coords[idx]
    )
    
    # Prepare aligned orientation data
    plot_data_aligned <- data.frame(
      x = x_aligned[idx],
      y = y_aligned[idx]
    )
    
    # Calculate new positions of reference points for verification
    left_x_new <- x_aligned[left_idx]
    left_y_new <- y_aligned[left_idx]
    right_x_new <- x_aligned[right_idx]
    right_y_new <- y_aligned[right_idx]
    
    # Calculate final angle for verification
    dx_new <- right_x_new - left_x_new
    dy_new <- right_y_new - left_y_new
    new_angle <- atan2(dy_new, dx_new)
    new_angle_degrees <- new_angle * 180 / pi
    
    # Create original orientation plot
    p1 <- ggplot(plot_data_orig, aes(x = x, y = y)) +
      geom_polygon(fill = "lightblue", alpha = 0.6, color = "blue", linewidth = 0.5) +
      # Mark centroid
      annotate("point", x = centroid_x, y = centroid_y, 
               color = "red", size = 3, shape = 16) +
      # Mark left reference point  
      annotate("point", x = left_x, y = left_y, 
               color = "darkgreen", size = 3, shape = 17) +
      # Mark right reference point
      annotate("point", x = right_x, y = right_y, 
               color = "purple", size = 3, shape = 15) +
      # Draw alignment axis
      annotate("segment", x = left_x, y = left_y,
               xend = right_x, yend = right_y,
               color = "red", linetype = "solid", linewidth = 1.2) +
      labs(title = "Original Orientation",
           subtitle = paste("Left-to-right axis:", round(current_angle * 180 / pi, 1), "°"),
           x = "X coordinate", y = "Y coordinate") +
      coord_equal() +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 10))
    
    # Create aligned orientation plot
    p2 <- ggplot(plot_data_aligned, aes(x = x, y = y)) +
      geom_polygon(fill = "lightcoral", alpha = 0.6, color = "darkred", linewidth = 0.5) +
      # Mark centroid
      annotate("point", x = centroid_x, y = centroid_y, 
               color = "red", size = 3, shape = 16) +
      # Mark left reference point
      annotate("point", x = left_x_new, y = left_y_new, 
               color = "darkgreen", size = 3, shape = 17) +
      # Mark right reference point  
      annotate("point", x = right_x_new, y = right_y_new, 
               color = "purple", size = 3, shape = 15) +
      # Draw alignment axis
      annotate("segment", x = left_x_new, y = left_y_new,
               xend = right_x_new, yend = right_y_new,
               color = "red", linetype = "solid", linewidth = 1.2) +
      labs(title = "Aligned Orientation",
           subtitle = paste("Left-to-right axis:", round(new_angle_degrees, 1), "°"),
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
    filepath <- file.path(alignment_output_dir, filename)
    
    ggsave(filepath, combined_plot, width = 12, height = 5, dpi = 300)
    
    successful_alignments <- successful_alignments + 1
    cat(paste("  Aligned:", sample_name, "- Rotation:", round(rotation_degrees, 1), "°", "- Plot saved\n"))
  }
}

# Report results
cat(paste("\nAlignment Summary:\n"))
cat(paste("- Total processed:", total_processed, "\n"))
cat(paste("- Successful alignments:", successful_alignments, "\n"))
cat(paste("- Individual plots saved to:", alignment_output_dir, "\n"))


plotWaveletShape(shape, "pop")


# Save the straightened outlines 
save(shape, file = file.path("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/aligned_straightened_outlines.RData"))
################################################################################
#### Step 3: Extract "Coo" object format for Momocs use 
################################################################################

cat("\n=== Converting shapeR to Momocs coo object ===\n")

# Extract outlines from aligned shapeR object
outlines <- shape@outline.list.org

# Remove empty elements
if("" %in% names(outlines)) {
  outlines <- outlines[names(outlines) != ""]
}

# Initialize storage
outline_list <- list()
outline_names <- character()
population_info <- character()
counter <- 1

# Convert each population's outlines
for(pop_name in names(outlines)) {
  cat("Processing population:", pop_name, "\n")
  pop_outlines <- outlines[[pop_name]]
  success_count <- 0
  
  for(individual_name in names(pop_outlines)) {
    coords_data <- pop_outlines[[individual_name]]
    
    # Validate coordinates
    if(is.list(coords_data) && all(c("X", "Y") %in% names(coords_data))) {
      X_coords <- coords_data$X
      Y_coords <- coords_data$Y
      
      # Quality check
      if(length(X_coords) == length(Y_coords) && 
         length(X_coords) >= 25 && 
         !any(is.na(c(X_coords, Y_coords)))) {
        
        # Store valid outline
        outline_list[[counter]] <- cbind(X = X_coords, Y = Y_coords)
        outline_names[counter] <- paste(pop_name, individual_name, sep = "_")
        population_info[counter] <- pop_name
        counter <- counter + 1
        success_count <- success_count + 1
      }
    }
  }
  cat("Population", pop_name, "completed:", success_count, "extractions\n")
}

# Summary of extraction
cat("\n=== EXTRACTION SUMMARY ===\n")
cat("Total outlines extracted:", length(outline_list), "\n")
print(table(population_info))

# Prepare data for Momocs
names(outline_list) <- outline_names

# Create factor data frame
fac_df <- data.frame(
  pop = factor(population_info),
  individual = outline_names,
  row.names = outline_names,
  stringsAsFactors = FALSE
)

# Validate matrix structure
for(i in 1:length(outline_list)) {
  if(!is.matrix(outline_list[[i]])) {
    outline_list[[i]] <- as.matrix(outline_list[[i]])
  }
}

# Create Momocs coo object
coo_outline <- Out(outline_list, fac = fac_df)

cat("SUCCESS: Momocs coo object created!\n")
cat("Number of outlines:", length(coo_outline), "\n")
cat("Population distribution:\n")
print(table(coo_outline$fac$pop))

# Perform Elliptic Fourier Analysis
cat("\n=== FOURIER ANALYSIS ===\n")
oto.f <- efourier(coo_outline, nb.h = 10)
cat("Fourier analysis completed with 10 harmonics\n")

# Basic visualization
boxplot(oto.f, main = "Fourier Coefficients Distribution")
bot.p <- PCA(oto.f)
plot(bot.p, main = "PCA of Fourier Coefficients")


# save the momocs object
save(coo_outline, file = file.path("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/aligned_momocs_outlines.RData"))
