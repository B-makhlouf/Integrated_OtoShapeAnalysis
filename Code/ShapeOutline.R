library("shapeR")

# Load your data
shape <- shapeR("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines", "FISH.csv")

# Detect outlines
outlinesonly <- detect.outline(shape, threshold = .4, write.outline.w.org = FALSE)


# Get the successful outline names 
names(outlinesonly@outline.list)
outline_names <- list()
for(folder in names(outlinesonly@outline.list)) {
  outline_names[[folder]] <- names(outlinesonly@outline.list[[folder]])
}
outline_names

# Now, read in the original csv 
original_data <- read.csv("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines/FISH.csv")

# remove the .jpg from 