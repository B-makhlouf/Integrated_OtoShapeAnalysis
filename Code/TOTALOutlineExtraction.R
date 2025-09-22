library(shapeR)
library(dplyr)
library(here)

# Load your data
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_outlinesOnly.RData")


main_folder <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos/REDO/REDO1"
png_files_full <- list.files(path = main_folder, 
                             pattern = "\\.png$", 
                             recursive = TRUE, 
                             full.names = TRUE)

fish_info <- data.frame(
  fish_id = tools::file_path_sans_ext(basename(png_files_full)),
  full_path = png_files_full,
  subfolder = dirname(gsub(paste0(main_folder, "/"), "", png_files_full))
)

for(i in 1:nrow(fish_info)) {
  folder <- fish_info$subfolder[i]
  fish_id <- fish_info$fish_id[i]
  
  outlinesonly <- remove.outline(outlinesonly, folder, fish_id)
  
  # Optional: print progress
  cat("Removed:", folder, "-", fish_id, "\n")
}

######### Now, we can re do detect.outlines with an easier threshold and mouse.click = TRUE 

outlinesonly@project.path<- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos"

outlinesonly <- detect.outline(outlinesonly, threshold = 0.1, write.outline.w.org = TRUE)

# At this point, i've deleted all the outlines in the original_with_outlines folder so I can QC the new ones coming in. 




