library("shapeR")

# Load your data
shape <- shapeR("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines", "FISH.csv")

# Detect outlines
outlinesonly <- detect.outline(shape, threshold = .4, write.outline.w.org = FALSE)

# Get list of samples that have outlines
successful_folders <- names(outlinesonly@outline.list)
successful_samples <- c()
for(folder in successful_folders) {
  folder_samples <- names(outlinesonly@outline.list[[folder]])
  successful_samples <- c(successful_samples, paste(folder, folder_samples, sep = "_"))
}

# Filter the master list to only successful samples
original_masterlist <- getMasterlist(outlinesonly, useFilter = FALSE)
original_masterlist$sample_id <- paste(original_masterlist$Folder, original_masterlist$Picname, sep = "_")
filtered_masterlist <- original_masterlist[original_masterlist$sample_id %in% successful_samples, ]

cat("Original samples:", nrow(original_masterlist), "\n")
cat("Successful samples:", nrow(filtered_masterlist), "\n")
cat("Filtered out:", nrow(original_masterlist) - nrow(filtered_masterlist), "samples\n")

# Update the outlinesonly object's master.list with filtered data
outlinesonly@master.list <- filtered_masterlist[, !names(filtered_masterlist) %in% "sample_id"]

# Generate coefficients
coef <- generateShapeCoefficients(outlinesonly)

# Enrich master list
combined <- enrich.master.list(coef,
                               folder_name = "Folder",    
                               pic_name = "Picname",      
                               calibration = "cal",       
                               include.wavelet = TRUE,
                               include.fourier = TRUE,
                               n.wavelet.levels = 5,
                               n.fourier.freq = 12)

# Verify filtering worked
masterlist <- getMasterlist(combined, useFilter = FALSE)
cat("Final dataset:", nrow(masterlist), "samples\n")
cat("Watersheds:", paste(unique(masterlist$Watershed), collapse = ", "), "\n")

# Standardize if length data is available
if("length_cm" %in% colnames(masterlist)) {
  combined <- stdCoefs(combined, classes="Watershed", std.by="length_cm")
  cat("Applied standardization using length_cm\n")
} else {
  cat("No length_cm column found - proceeding without standardization\n")
}

# Run PCA analysis
wavelet_data <- getStdWavelet(combined)
masterlist <- getMasterlist(combined)

pca_result <- prcomp(wavelet_data, center = TRUE, scale. = TRUE)
var_explained <- summary(pca_result)$importance[2,] * 100

# Plot PCA
library(ggplot2)
pca_scores <- data.frame(
  PC1 = pca_result$x[,1],
  PC2 = pca_result$x[,2],
  Watershed = masterlist$Watershed
)

p1 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Watershed)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95) +
  labs(
    x = paste0("PC1 (", round(var_explained[1], 1), "% variance)"),
    y = paste0("PC2 (", round(var_explained[2], 1), "% variance)"),
    title = "PCA of Otolith Shape by Watershed"
  ) +
  theme_minimal()

print(p1)

# Canonical analysis
library(vegan)
cap_result <- capscale(wavelet_data ~ masterlist$Watershed)
cap_scores <- scores(cap_result)$sites[,1:2]
eig <- eigenvals(cap_result, constrained=TRUE)
eig_ratio <- eig/sum(eig)

cluster.plot(cap_scores, masterlist$Watershed,
             plotCI = TRUE,
             xlab = paste("CAP1 (", round(eig_ratio[1]*100, 1), "%)", sep=""),
             ylab = paste("CAP2 (", round(eig_ratio[2]*100, 1), "%)", sep=""),
             main = "Canonical Analysis: Otolith Shape by Watershed")

# Test significance
anova_result <- anova(cap_result)
cat("\nSignificance test:\n")
print(anova_result)

cat("\nAnalysis complete!\n")