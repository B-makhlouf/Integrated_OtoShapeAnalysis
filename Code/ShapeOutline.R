library("shapeR")

# Load your data
shape <- shapeR("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines", "FISH.csv")

# Detect outlines
outlinesonly <- detect.outline(shape, threshold = .4, write.outline.w.org = FALSE)

