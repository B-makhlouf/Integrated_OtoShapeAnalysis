# =============================================================================
# OTOLITH SHAPE ANALYSIS - QUALITY CONTROL AND OUTLINE DETECTION
# =============================================================================
# Purpose: Process otolith images for shape analysis with iterative quality control
# =============================================================================

# Load required libraries
library(shapeR)
library(dplyr)
library(here)

# =============================================================================
# SECTION 1: DATA LOADING
# =============================================================================
# Load the initial otolith outlines data
cat("Loading otolith outlines data...\n")
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_outlinesOnly.RData")

# =============================================================================
# SECTION 2: FIRST ROUND OF QUALITY CONTROL - REMOVE BAD OUTLINES
# =============================================================================
# Remove previously identified problematic outlines from REDO1 folder

cat("Starting first round of quality control...\n")

# Define the folder containing problematic files from first QC round
main_folder_redo1 <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos/REDO/REDO1"

# Get all PNG files from the REDO1 folder (these are the bad ones to remove)
png_files_full_redo1 <- list.files(path = main_folder_redo1, 
                                   pattern = "\\.png$", 
                                   recursive = TRUE, 
                                   full.names = TRUE)

cat("Found", length(png_files_full_redo1), "files to remove from REDO1 folder\n")

# Create a dataframe with fish information for easier processing
fish_info_redo1 <- data.frame(
  fish_id = tools::file_path_sans_ext(basename(png_files_full_redo1)),
  full_path = png_files_full_redo1,
  subfolder = dirname(gsub(paste0(main_folder_redo1, "/"), "", png_files_full_redo1)),
  stringsAsFactors = FALSE
)

# Remove each identified bad outline from the shapeoutline object
cat("Removing bad outlines from REDO1...\n")
for(i in 1:nrow(fish_info_redo1)) {
  folder <- fish_info_redo1$subfolder[i]
  fish_id <- fish_info_redo1$fish_id[i]
  
  # Remove the outline from the shapeR object
  outlinesonly <- remove.outline(outlinesonly, folder, fish_id)
  
  # Print progress
  cat("Removed:", folder, "-", fish_id, "(", i, "of", nrow(fish_info_redo1), ")\n")
}

cat("Completed removal of", nrow(fish_info_redo1), "bad outlines from REDO1\n")

# =============================================================================
# SECTION 3: RE-DETECT OUTLINES WITH RELAXED THRESHOLD
# =============================================================================
# Re-run outline detection with easier threshold and manual verification

cat("Re-detecting outlines with relaxed threshold...\n")

# Update project path for outline detection
outlinesonly@project.path <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos"

# Re-detect outlines with relaxed threshold (0.1) and write output for QC
# Note: mouse.click = TRUE allows for manual verification during detection
outlinesonly <- detect.outline(outlinesonly, 
                               threshold = 0.1, 
                               write.outline.w.org = TRUE)

cat("Outline re-detection completed\n")
cat("Manual QC: Please review outlines in original_with_outlines folder\n")
cat("Move any remaining bad outlines to REDO2 folder before continuing\n")

# =============================================================================
# SECTION 4: SECOND ROUND OF QUALITY CONTROL - REMOVE REMAINING BAD OUTLINES
# =============================================================================
# Remove any remaining problematic outlines identified in manual QC (REDO2 folder)

cat("Starting second round of quality control...\n")

# Define the folder containing problematic files from second QC round
main_folder_redo2 <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos/REDO2"

# Check if REDO2 folder exists and contains files
if (dir.exists(main_folder_redo2)) {
  # Get all PNG files from the REDO2 folder
  png_files_full_redo2 <- list.files(path = main_folder_redo2, 
                                     pattern = "\\.png$", 
                                     recursive = TRUE, 
                                     full.names = TRUE)
  
  if (length(png_files_full_redo2) > 0) {
    cat("Found", length(png_files_full_redo2), "files to remove from REDO2 folder\n")
    
    # Create dataframe with fish information for REDO2 files
    fish_info_redo2 <- data.frame(
      fish_id = tools::file_path_sans_ext(basename(png_files_full_redo2)),
      full_path = png_files_full_redo2,
      subfolder = dirname(gsub(paste0(main_folder_redo2, "/"), "", png_files_full_redo2)),
      stringsAsFactors = FALSE
    )
    
    # Remove each identified bad outline from the shapeoutline object
    cat("Removing bad outlines from REDO2...\n")
    for(i in 1:nrow(fish_info_redo2)) {
      folder <- fish_info_redo2$subfolder[i]
      fish_id <- fish_info_redo2$fish_id[i]
      
      # Remove the outline from the shapeR object
      outlinesonly <- remove.outline(outlinesonly, folder, fish_id)
      
      # Print progress
      cat("Removed:", folder, "-", fish_id, "(", i, "of", nrow(fish_info_redo2), ")\n")
    }
    
    cat("Completed removal of", nrow(fish_info_redo2), "bad outlines from REDO2\n")
    
  } else {
    cat("No files found in REDO2 folder - no additional outlines to remove\n")
  }
} else {
  cat("REDO2 folder does not exist - no additional outlines to remove\n")
}

#########################################################################################
########### Ok, at this point all of the bad outlines should be removed and we're goign to rock 
# with what we have. Save the outlinesonly object and move on to the next script

# Save the cleaned outlinesonly object for further analysis
save(outlinesonly, file = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_outlinesQCd.RData")
