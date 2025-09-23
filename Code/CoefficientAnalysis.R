#### Wavelet and Fourier Analysis
#### Data at this point has been QCd and Straightened 
#### This script extracts coefficients and creates Momocs coo objects

# Load required libraries
library(shapeR)
library(dplyr)
library(Momocs)

# =============================================================================
# PART 1: LOAD DATA AND EXTRACT COEFFICIENTS
# =============================================================================

cat("=== PART 1: Loading data and extracting coefficients ===\n")

# Read in the corrected data 
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/otoOutlines_Aligned.RData")

# First, smooth the contours 
cat("Smoothing contours...\n")
shape <- smoothout(outlinesonly, n = 100)

# Remove problematic outlines
cat("Removing problematic outlines...\n")
shape <- remove.outline(shape, "YK", "2021_yk_416")
shape <- remove.outline(shape, "YK", "2021_yk_434")

# Generate Coefficients 
cat("Generating shape coefficients...\n")
shape <- generateShapeCoefficients(shape)

# Save the coefficients as an rdata file
cat("Saving shape object with coefficients...\n")
save(shape, file = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/otoOutlines_Smoothed_Coefficients.RData")

# Extract the coefficients into dataframes
cat("Extracting coefficients to dataframes...\n")
shape_coef_df <- as.data.frame(shape@shape.coef.raw)
wavelet_coef_raw <- as.data.frame(shape@wavelet.coef.raw)
fourier_coef_raw <- as.data.frame(shape@fourier.coef.raw)

cat("Shape coefficients extracted:\n")
cat(paste("  - Shape variables:", ncol(shape_coef_df), "variables,", nrow(shape_coef_df), "individuals\n"))
cat(paste("  - Wavelet coefficients:", ncol(wavelet_coef_raw), "coefficients,", nrow(wavelet_coef_raw), "individuals\n"))
cat(paste("  - Fourier coefficients:", ncol(fourier_coef_raw), "coefficients,", nrow(fourier_coef_raw), "individuals\n"))

# =============================================================================
# PART 2: CONVERT TO MOMOCS COO OBJECT
# =============================================================================

cat("\n=== PART 2: Converting to Momocs coo object ===\n")

# Extract the outline list from shapeR object
outlines <- shape@outline.list.org

# Remove the empty first element if it exists
if("" %in% names(outlines)) {
  outlines <- outlines[names(outlines) != ""]
}

cat("Processing populations:", paste(names(outlines), collapse = ", "), "\n")

# Initialize storage for converted data
outline_list <- list()
outline_names <- character()
population_info <- character()
counter <- 1

# Process each population
for(pop_name in names(outlines)) {
  cat(paste("Processing population:", pop_name, "\n"))
  pop_outlines <- outlines[[pop_name]]
  success_count <- 0
  
  # Process each individual in the population
  for(individual_name in names(pop_outlines)) {
    
    # Get the outline coordinates (list with $X and $Y)
    coords_data <- pop_outlines[[individual_name]]
    
    # Extract X and Y coordinates
    if(is.list(coords_data) && "X" %in% names(coords_data) && "Y" %in% names(coords_data)) {
      
      X_coords <- coords_data$X
      Y_coords <- coords_data$Y
      
      # Create coordinate matrix
      if(length(X_coords) == length(Y_coords) && length(X_coords) >= 3) {
        coords_matrix <- cbind(X = X_coords, Y = Y_coords)
        
        # Check for valid data (no NAs)
        if(!any(is.na(coords_matrix))) {
          # Store the outline
          outline_list[[counter]] <- coords_matrix
          outline_names[counter] <- paste(pop_name, individual_name, sep = "_")
          population_info[counter] <- pop_name
          
          counter <- counter + 1
          success_count <- success_count + 1
        } else {
          cat(paste("  Skipping", individual_name, "- contains NA values\n"))
        }
      } else {
        cat(paste("  Skipping", individual_name, "- insufficient points or length mismatch\n"))
      }
    } else {
      cat(paste("  Skipping", individual_name, "- no X/Y coordinates found\n"))
    }
    
    # Print progress every 100 individuals
    if(success_count %% 100 == 0 && success_count > 0) {
      cat(paste("  ... processed", success_count, "individuals so far\n"))
    }
  }
  
  cat(paste("Population", pop_name, "completed:", success_count, "successful extractions\n"))
}

cat(paste("\n=== EXTRACTION SUMMARY ===\n"))
cat(paste("Total outlines extracted:", length(outline_list), "\n"))
cat("Population distribution:\n")
print(table(population_info))

# Create the Momocs coo object
if(length(outline_list) > 0) {
  
  # Name the outlines
  names(outline_list) <- outline_names
  
  # Create factor data frame for metadata
  fac_df <- data.frame(
    pop = factor(population_info),
    individual = outline_names,
    row.names = outline_names
  )
  
  # Create the coo object
  cat("\nCreating Momocs coo object...\n")
  
  tryCatch({
    coo_object <- Out(outline_list, fac = fac_df)
    
    cat("SUCCESS: Coo object created!\n")
    cat("Number of outlines:", length(coo_object), "\n")
    cat("Population distribution:\n")
    print(table(coo_object$fac$pop))
    
    # Basic validation - plot first few outlines
    cat("\nValidating with plot of first 6 outlines...\n")
    tryCatch({
      if(length(coo_object) >= 6) {
        plot(coo_object[1:6])
      } else {
        plot(coo_object)
      }
      cat("Plot successful!\n")
    }, error = function(e) {
      cat("Plot failed, but coo object was created successfully\n")
    })
    
  }, error = function(e) {
    cat("ERROR creating coo object:", e$message, "\n")
    cat("Let's examine the first outline to debug:\n")
    cat("First outline structure:\n")
    str(outline_list[[1]])
    cat("First few coordinates:\n")
    print(head(outline_list[[1]]))
  })
  
} else {
  cat("ERROR: No valid outlines were extracted!\n")
  cat("Check that your shape object has the expected structure\n")
}

# =============================================================================
# PART 3: SAVE ALL OUTPUTS
# =============================================================================

cat("\n=== PART 3: Saving all outputs ===\n")

# Define base path for outputs
output_path <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/"

# 1. Save coefficient dataframes as CSV files
cat("Saving coefficient dataframes as CSV files...\n")
write.csv(shape_coef_df, file = paste0(output_path, "shape_coefficients.csv"), row.names = TRUE)
write.csv(wavelet_coef_raw, file = paste0(output_path, "wavelet_coefficients.csv"), row.names = TRUE)
write.csv(fourier_coef_raw, file = paste0(output_path, "fourier_coefficients.csv"), row.names = TRUE)

# 2. Save coefficient dataframes as RData files
cat("Saving coefficient dataframes as RData files...\n")
save(shape_coef_df, file = paste0(output_path, "shape_coefficients.RData"))
save(wavelet_coef_raw, file = paste0(output_path, "wavelet_coefficients.RData"))
save(fourier_coef_raw, file = paste0(output_path, "fourier_coefficients.RData"))

# 3. Save the Momocs coo object
if(exists("coo_object")) {
  cat("Saving Momocs coo object...\n")
  save(coo_object, file = paste0(output_path, "momocs_coo_object.RData"))
  cat("Coo object saved successfully!\n")
}

# 4. Save all objects together
cat("Saving all objects in combined file...\n")
if(exists("coo_object")) {
  save(shape, shape_coef_df, wavelet_coef_raw, fourier_coef_raw, coo_object,
       file = paste0(output_path, "complete_analysis_objects.RData"))
} else {
  save(shape, shape_coef_df, wavelet_coef_raw, fourier_coef_raw,
       file = paste0(output_path, "complete_analysis_objects.RData"))
}

# =============================================================================
# PART 4: SUMMARY AND NEXT STEPS
# =============================================================================

cat("\n=== ANALYSIS COMPLETE! ===\n")
cat("Files saved:\n")
cat(paste("  - Shape object:", paste0(output_path, "otoOutlines_Smoothed_Coefficients.RData\n")))
cat(paste("  - Shape coefficients CSV:", paste0(output_path, "shape_coefficients.csv\n")))
cat(paste("  - Wavelet coefficients CSV:", paste0(output_path, "wavelet_coefficients.csv\n")))
cat(paste("  - Fourier coefficients CSV:", paste0(output_path, "fourier_coefficients.csv\n")))
cat(paste("  - Shape coefficients RData:", paste0(output_path, "shape_coefficients.RData\n")))
cat(paste("  - Wavelet coefficients RData:", paste0(output_path, "wavelet_coefficients.RData\n")))
cat(paste("  - Fourier coefficients RData:", paste0(output_path, "fourier_coefficients.RData\n")))
if(exists("coo_object")) {
  cat(paste("  - Momocs coo object:", paste0(output_path, "momocs_coo_object.RData\n")))
}
cat(paste("  - Complete analysis objects:", paste0(output_path, "complete_analysis_objects.RData\n")))

cat("\nNext steps for Momocs analysis:\n")
cat("# Load the coo object:\n")
cat("load('", paste0(output_path, "momocs_coo_object.RData"), "')\n\n", sep = "")

cat("# 1. Center and scale outlines:\n")
cat("coo_object <- coo_center(coo_object)\n")
cat("coo_object <- coo_scale(coo_object)\n\n")

cat("# 2. Perform elliptical Fourier analysis:\n")
cat("efa_object <- efourier(coo_object, nb.h = 10)\n\n")

cat("# 3. Visualize by population:\n")
cat("panel(coo_object, fac = 'pop')\n\n")

cat("# 4. PCA on Fourier coefficients:\n")
cat("pca_result <- PCA(efa_object)\n")
cat("plot(pca_result, 'pop')\n\n")

print("Script execution complete!")
