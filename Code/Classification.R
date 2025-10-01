library(shapeR)
library(randomForest)
library(caret)

# Load the data
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_coefficients.RData")
shape = enrich.master.list(shape)

plotWaveletShape(shape, "watershed", show.angle = TRUE, lwd = 2,lty = 1)
