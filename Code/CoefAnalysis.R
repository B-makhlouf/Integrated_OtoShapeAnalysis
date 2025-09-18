#### This script works on the extracted coefficients.

### for now, toy script 

library(tidyverse)
library(shapeR)

### Extract the shapes
shape<- shapeR("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/TestingOutlines", "FISHtest.csv")
shape<- detect.outline(shape)

## Generate Fourier and Wavelet Coefficients
shape = generateShapeCoefficients(shape)
shape = enrich.master.list(shape) # note: Fish.csv needs to have the exact column names in lowercase

## The quality of the Wavelet and Fourier reconstruction can be estimated by comparing it to how it 
# deviates from out otolith outline.
est.list = estimate.outline.reconstruction(shape)
outline.reconstruction.plot(est.list, max.num.harmonics = 15)

### ON THIS TOY DATASET, 5 wavelet, 12 harmonics 

# mean and stdv of waveelt cofficients for all otliths. 
plotWavelet(shape, level = 5, class.name = "pop", useStdcoef = FALSE)

# Average Wavelet shape per 
plotWaveletShape(shape, "pop", show.angle = TRUE, lwd = 2,lty = 1)

# Shape Variation Analysis Using Canonical Analysis of Principal Coordinates (CAP)
# Author: [Your Name]
# Date: [Current Date]

# Load required libraries
library(vegan)    # For CAP analysis
library(ipred)    # For error estimation
library(MASS)     # For LDA

# =============================================================================
# CANONICAL ANALYSIS OF PRINCIPAL COORDINATES (CAP)
# =============================================================================

# Perform CAP analysis to examine shape variation among populations
# Uses wavelet coefficients as response variables and population as predictor
cap.res <- capscale(getWavelet(shape) ~ getMasterlist(shape)$pop)

# Test significance and partition variation among groups using ANOVA
# Uses 1000 permutations for robust p-values
cap.anova <- anova(cap.res, by = "terms", permutations = 1000)
print(cap.anova)

# =============================================================================
# VISUALIZATION - CAP ORDINATION PLOT
# =============================================================================

# Extract eigenvalues for the constrained axes
eig <- eigenvals(cap.res, model = "constrained")
eig.ratio <- eig / sum(eig)

# Extract CAP scores and population data
cap_scores <- scores(cap.res)$sites[, 1:2]
pop_groups <- getMasterlist(shape)$pop

# Method 1: Using base R with confidence ellipses
# Create color palette for populations
colors <- rainbow(length(unique(pop_groups)))
names(colors) <- unique(pop_groups)

# Create the plot
plot(cap_scores[, 1], cap_scores[, 2],
     col = colors[pop_groups],
     pch = 19,
     xlab = paste("CAP1 (", round(eig.ratio[1] * 100, 1), "%)", sep = ""),
     ylab = paste("CAP2 (", round(eig.ratio[2] * 100, 1), "%)", sep = ""),
     main = "CAP Ordination Plot",
     las = 1)

# Add confidence ellipses for each population
library(car)  # For confidence ellipses
for(pop in unique(pop_groups)) {
  pop_data <- cap_scores[pop_groups == pop, ]
  if(nrow(pop_data) > 2) {  # Need at least 3 points for ellipse
    dataEllipse(pop_data[, 1], pop_data[, 2],
                levels = 0.95,
                add = TRUE,
                plot.points = FALSE,
                col = colors[pop],
                lwd = 2)
  }
}

# Add legend
legend("topright", 
       legend = unique(pop_groups),
       col = colors[unique(pop_groups)],
       pch = 19,
       title = "Population")



# =============================================================================
# CLASSIFICATION ANALYSIS - LINEAR DISCRIMINANT ANALYSIS (LDA)
# =============================================================================

# Prepare data for classification
pop <- factor(getMasterlist(shape)$pop)  # Population factor
stdw <- getWavelet(shape)                # Wavelet coefficients

# Define custom prediction function for LDA
mypredict.lda <- function(object, newdata) {
  predict(object, newdata = newdata)$class
}

# Create data frame for analysis
dd <- data.frame(stdw = stdw, pop = pop)

# Perform cross-validation error estimation
# Uses 1000 bootstrap samples for robust error estimates
classification.error <- errorest(
  pop ~ .,                               # Formula: predict population from wavelet data
  data = dd,                            # Input data
  model = lda,                          # Linear Discriminant Analysis
  estimator = "cv",                     # Cross-validation
  predict = mypredict.lda,              # Custom prediction function
  est.para = control.errorest(nboot = 1000) # Bootstrap parameters
)

# Display classification results
print("Classification Error Rate:")
print(classification.error)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

# Display CAP results summary
print("CAP Analysis Summary:")
summary(cap.res)

# Calculate and display proportion of variance explained
total.var <- sum(eigenvals(cap.res))
constrained.var <- sum(eigenvals(cap.res, model = "constrained"))
prop.explained <- constrained.var / total.var

cat("\nProportion of total variance explained by population differences:", 
    round(prop.explained * 100, 2), "%\n")

# Display eigenvalue proportions for first few axes
cat("\nEigenvalue proportions for constrained axes:\n")
for(i in 1:min(3, length(eig.ratio))) {
  cat("CAP", i, ": ", round(eig.ratio[i] * 100, 2), "%\n", sep = "")
}



# =============================================================================
# CREATE FOURIER COEFFICIENTS DATA FRAME
# =============================================================================

# Extract Fourier coefficients
fourier_coef <- getFourier(shape)

# Extract master list with metadata
master_list <- getMasterlist(shape)

# Create comprehensive data frame with Fourier coefficients and metadata
fourier_df <- data.frame(
  # Individual identifier (assuming this is in the master list or can be extracted)
  individual = master_list$picname,  # or master_list$name, depending on your data structure
  
  # Population information
  pop = master_list$pop,
  
  # Fourier coefficients (all columns)
  fourier_coef,
  
  # Add row names for clarity
  row.names = NULL
)

# Alternative if individual names are in row names of Fourier coefficients
# fourier_df <- data.frame(
#   individual = rownames(fourier_coef),
#   pop = master_list$pop,
#   fourier_coef,
#   row.names = NULL
# )

# Display structure of the data frame
cat("Fourier coefficients data frame structure:\n")
str(fourier_df)

# Display first few rows
cat("\nFirst 6 rows of Fourier data frame:\n")
head(fourier_df)

# Check dimensions
cat(paste("\nData frame dimensions:", nrow(fourier_df), "individuals x", 
          ncol(fourier_df), "variables\n"))

# Summary of populations
cat("\nPopulation summary:\n")
table(fourier_df$pop)

# =============================================================================
# OPTIONAL: SAVE THE DATA FRAME
# =============================================================================

# Save as CSV if needed
write.csv(fourier_df, "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/fourier_coefficients_data.csv", row.names = FALSE)

