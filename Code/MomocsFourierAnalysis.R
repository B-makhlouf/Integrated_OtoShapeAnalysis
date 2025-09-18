# Comprehensive Otolith Shape Analysis
# Following the American Shad paper methodology but using Fourier analysis
# Combines shapeR and Momocs functionality

#===============================================================================
# 1. LOAD REQUIRED LIBRARIES
#===============================================================================

library(shapeR)
library(Momocs)
library(randomForest)
library(caret)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(viridis)
library(cluster)
library(factoextra)
library(MASS)
library(nnet)
library(reshape2)

#===============================================================================
# 2. DATA PREPARATION AND STRUCTURE
#===============================================================================

# Set your base path (adjust as needed)
base_path <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/TestingOutlines"

# Expected folder structure:
# base_path/
#   ├── Original/
#   │   ├── YK/
#   │   ├── KK/
#   │   └── NK/
#   └── FISHtest.csv (metadata file)

# Load and clean metadata
fish_data <- read.csv(file.path(base_path, "FISHtest.csv"))

# List all available image files
allfiles <- c(
  list.files(file.path(base_path, "Original/YK")),
  list.files(file.path(base_path, "Original/KK")),
  list.files(file.path(base_path, "Original/NK"))
)

# Clean data - remove missing files and standardize
clean_fish_data <- fish_data[fish_data$Picname %in% allfiles, ]
colnames(clean_fish_data) <- tolower(colnames(clean_fish_data))

# Save cleaned data
write.csv(clean_fish_data, 
          file.path(base_path, "CleanFish.csv"), 
          row.names = FALSE)

print(paste("Samples retained after cleaning:", nrow(clean_fish_data)))
print(paste("Populations:", paste(unique(clean_fish_data$pop), collapse = ", ")))

#===============================================================================
# 3. OUTLINE EXTRACTION USING SHAPER
#===============================================================================

# Initialize shapeR object
shape <- shapeR("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/TestingOutlines", "FISHtest.csv")

# Extract outlines with quality control
cat("Extracting otolith outlines...\n")
shape <- detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)

# # Optional: Smooth outlines to reduce pixel noise (similar to shad paper preprocessing)
# cat("Smoothing outlines to reduce pixel noise...\n")
# shape <- smoothout(shape, n = 100)

# Visual quality check - plot sample outlines
sample_outlines <- function(shape_obj, n_samples = 6) {
  par(mfrow = c(2, 3))
  populations <- names(shape_obj@outline.list)
  for (i in 1:min(n_samples, length(populations))) {
    pop <- populations[i]
    sample_ids <- names(shape_obj@outline.list[[pop]])
    if (length(sample_ids) > 0) {
      sample_id <- sample_ids[1]
      outline <- shape_obj@outline.list[[pop]][[sample_id]]
      plot(outline$X, outline$Y, type = 'l', 
           main = paste(pop, "-", sample_id),
           xlab = "X", ylab = "Y", lwd = 2)
    }
  }
  par(mfrow = c(1, 1))
}

sample_outlines(shape)

#===============================================================================
# 4. SHAPE COEFFICIENT EXTRACTION
#===============================================================================

# Generate both wavelet and Fourier coefficients
cat("Generating shape coefficients...\n")
shape <- generateShapeCoefficients(shape)

# Connect metadata to outlines
shape <- enrich.master.list(shape)

# Extract coefficients for analysis
fourier_coef <- shape@fourier.coef
wavelet_coef <- shape@wavelet.coef
master_list <- shape@master.list

# Basic statistics
print(paste("Fourier coefficients:", ncol(fourier_coef)))
print(paste("Wavelet coefficients:", ncol(wavelet_coef)))
print(paste("Total specimens analyzed:", nrow(master_list)))

#===============================================================================
# 6. PRINCIPAL COMPONENT ANALYSIS
#===============================================================================

# Handle NA values properly - keep track of which rows are retained
complete_rows <- complete.cases(fourier_coef)
fourier_coef_clean <- fourier_coef[complete_rows, ]
master_list_clean <- master_list[complete_rows, ]

print(paste("Removed", sum(!complete_rows), "samples due to missing data"))
print(paste("Initial cleaned sample size:", nrow(master_list_clean)))

# Check if we have enough samples
if(nrow(fourier_coef_clean) < 10) {
  stop("Too few complete samples for analysis. Check your data for quality issues.")
}

# PCA on cleaned Fourier coefficients
pca_result <- prcomp(fourier_coef_clean, center = TRUE, scale. = TRUE)

# Additional check - sometimes PCA can remove rows with zero variance
# Ensure master_list_clean matches the actual PCA result
if(nrow(pca_result$x) != nrow(master_list_clean)) {
  warning("PCA removed additional rows - adjusting master_list to match")
  # Find which rows PCA kept by matching row names
  pca_rownames <- rownames(pca_result$x)
  fourier_rownames <- rownames(fourier_coef_clean)
  
  if(is.null(pca_rownames)) {
    # If no row names, assume PCA kept first n rows
    master_list_clean <- master_list_clean[1:nrow(pca_result$x), ]
  } else {
    # Match by row names
    keep_indices <- match(pca_rownames, fourier_rownames)
    master_list_clean <- master_list_clean[keep_indices, ]
  }
}

print(paste("Final sample size after PCA:", nrow(master_list_clean)))
print(paste("Sample sizes by population:", paste(names(table(master_list_clean$pop)), "=", table(master_list_clean$pop), collapse = ", ")))

# Determine significant PCs using broken-stick model (as in shad paper)
broken_stick <- function(n) {
  result <- numeric(n)
  for (i in 1:n) {
    result[i] <- sum(1 / (i:n)) / n
  }
  return(result)
}

n_pcs <- ncol(fourier_coef_clean)
bs_values <- broken_stick(n_pcs)
eigenvalues <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
significant_pcs <- sum(eigenvalues > bs_values)

print(paste("Significant PCs (broken-stick criterion):", significant_pcs))
print(paste("Variance explained by first", significant_pcs, "PCs:", 
            round(sum(eigenvalues[1:significant_pcs]) * 100, 1), "%"))

# Extract significant PC scores
pc_scores <- pca_result$x[, 1:significant_pcs]

#===============================================================================
# 7. POPULATION DISCRIMINATION VISUALIZATION
#===============================================================================

# Create comprehensive PCA plots (similar to Figure 4 in shad paper)
plot_pca_analysis <- function(pc_scores, metadata, pc1 = 1, pc2 = 2) {
  
  # Prepare data for plotting
  plot_data <- data.frame(
    PC1 = pc_scores[, pc1],
    PC2 = pc_scores[, pc2],
    Population = factor(metadata$pop)
  )
  
  # Calculate variance explained
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
  
  # Main PCA plot
  p1 <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Population)) +
    geom_point(alpha = 0.7, size = 2) +
    stat_ellipse(level = 0.95, linetype = "dashed") +
    labs(
      title = "Otolith Shape Variation - PCA",
      x = paste0("PC", pc1, " (", round(var_explained[pc1], 1), "%)"),
      y = paste0("PC", pc2, " (", round(var_explained[pc2], 1), "%)")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_viridis_d()
  
  # Variance explained plot
  var_data <- data.frame(
    PC = 1:min(20, length(var_explained)),
    Variance = var_explained[1:min(20, length(var_explained))]
  )
  
  p2 <- ggplot(var_data, aes(x = PC, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = bs_values[1:nrow(var_data)] * 100, 
               color = "red", linetype = "dashed") +
    labs(
      title = "Variance Explained by PCs",
      x = "Principal Component",
      y = "Variance Explained (%)",
      caption = "Red line = Broken-stick criterion"
    ) +
    theme_minimal()
  
  return(list(pca_plot = p1, variance_plot = p2))
}

# Generate plots using the cleaned datasets
pca_plots <- plot_pca_analysis(pc_scores, master_list_clean)
grid.arrange(pca_plots$pca_plot, pca_plots$variance_plot, ncol = 2)

#===============================================================================
# 8. STATISTICAL TESTING (PERMANOVA equivalent)
#===============================================================================

# Test for population differences using MANOVA (using cleaned data)
manova_result <- manova(pc_scores ~ master_list_clean$pop)
manova_summary <- summary(manova_result)

print("MANOVA results:")
print(manova_summary)

# Pairwise comparisons between populations
populations <- unique(master_list_clean$pop)
pairwise_results <- data.frame(
  Comparison = character(),
  F_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

if (length(populations) > 2) {
  for (i in 1:(length(populations)-1)) {
    for (j in (i+1):length(populations)) {
      subset_indices <- master_list_clean$pop %in% c(populations[i], populations[j])
      subset_data <- pc_scores[subset_indices, ]
      subset_groups <- master_list_clean$pop[subset_indices]
      
      pairwise_manova <- manova(subset_data ~ subset_groups)
      pairwise_summary <- summary(pairwise_manova)
      
      pairwise_results <- rbind(pairwise_results, data.frame(
        Comparison = paste(populations[i], "vs", populations[j]),
        F_value = round(pairwise_summary$stats[1,3], 3),
        p_value = round(pairwise_summary$stats[1,6], 4)
      ))
    }
  }
  print("Pairwise comparisons:")
  print(pairwise_results)
}

#===============================================================================
# 9. CLASSIFICATION ANALYSIS (Neural Network like in shad paper)
#===============================================================================
install.packages("MLmetrics")
library(MLmetrics)

# Prepare data for classification (using cleaned data)
classification_data <- data.frame(
  pc_scores,
  pop = factor(master_list_clean$pop)
)

# Train multinomial neural network (similar to MLP in shad paper)
set.seed(123)

# Use cross-validation for model evaluation
train_control <- trainControl(
  method = "LOOCV",  # Leave-one-out cross-validation like in shad paper
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Train neural network
nnet_model <- train(
  pop ~ .,
  data = classification_data,
  method = "nnet",
  trControl = train_control,
  metric = "Accuracy",
  trace = FALSE,
  MaxNWts = 2000
)

# Extract results
classification_results <- nnet_model$results
best_accuracy <- max(classification_results$Accuracy)
best_kappa <- classification_results$Kappa[which.max(classification_results$Accuracy)]

print(paste("Overall Classification Accuracy:", round(best_accuracy * 100, 1), "%"))
print(paste("Cohen's Kappa:", round(best_kappa, 3)))

# Confusion matrix
predictions <- predict(nnet_model, classification_data)
confusion_matrix <- confusionMatrix(predictions, classification_data$pop)
print("Confusion Matrix:")
print(confusion_matrix$table)

#===============================================================================
# 10. MORPHOTYPE IDENTIFICATION (Hierarchical Clustering)
#===============================================================================

# Hierarchical clustering to identify morphotypes
distance_matrix <- dist(pc_scores, method = "euclidean")
hclust_result <- hclust(distance_matrix, method = "ward.D")

# Determine optimal number of clusters
# Using elbow method and silhouette analysis
optimal_clusters <- function(data, max_k = 10) {
  wss <- sapply(2:max_k, function(k) {
    kmeans(data, k, nstart = 10)$tot.withinss
  })
  
  # Silhouette analysis
  sil_scores <- sapply(2:max_k, function(k) {
    km <- kmeans(data, k, nstart = 10)
    sil <- silhouette(km$cluster, dist(data))
    mean(sil[,3])
  })
  
  return(list(wss = wss, silhouette = sil_scores, k_values = 2:max_k))
}

cluster_analysis <- optimal_clusters(pc_scores)

# Plot cluster analysis
par(mfrow = c(1, 2))
plot(cluster_analysis$k_values, cluster_analysis$wss, type = "b",
     xlab = "Number of Clusters", ylab = "Within Sum of Squares",
     main = "Elbow Method")

plot(cluster_analysis$k_values, cluster_analysis$silhouette, type = "b",
     xlab = "Number of Clusters", ylab = "Average Silhouette Width",
     main = "Silhouette Analysis")
par(mfrow = c(1, 1))

# Use optimal number of clusters (adjust based on your data)
n_morphotypes <- 3  # Reduced from 5 due to smaller sample size
morphotypes <- cutree(hclust_result, k = n_morphotypes)

# Add morphotypes to cleaned data
master_list_clean$morphotype <- factor(morphotypes)

# Visualize morphotypes (using cleaned data)
morphotype_plot <- ggplot(data.frame(pc_scores, 
                                     Population = master_list_clean$pop,
                                     Morphotype = factor(morphotypes)), 
                          aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Population, shape = Morphotype), size = 3, alpha = 0.8) +
  labs(title = "Morphotype Classification",
       x = paste0("PC1 (", round((pca_result$sdev[1]^2/sum(pca_result$sdev^2))*100, 1), "%)"),
       y = paste0("PC2 (", round((pca_result$sdev[2]^2/sum(pca_result$sdev^2))*100, 1), "%)")) +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

print(morphotype_plot)

#===============================================================================
# 11. MORPHOTYPE DISTRIBUTION ANALYSIS
#===============================================================================

# Calculate morphotype proportions by population (using cleaned data)
morphotype_summary <- master_list_clean %>%
  group_by(pop, morphotype) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(pop) %>%
  mutate(proportion = count / sum(count))

# Visualization of morphotype distributions (similar to Figure 9 in shad paper)
morphotype_barplot <- ggplot(morphotype_summary, 
                             aes(x = pop, y = proportion, fill = morphotype)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Morphotype Distribution Across Populations",
       x = "Population", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(name = "Morphotype")

print(morphotype_barplot)

#===============================================================================
# 12. RESULTS SUMMARY AND EXPORT
#===============================================================================

# Create comprehensive results summary (using cleaned data)
results_summary <- list(
  original_sample_size = nrow(master_list),
  final_sample_size = nrow(master_list_clean),
  samples_removed = nrow(master_list) - nrow(master_list_clean),
  sample_sizes = table(master_list_clean$pop),
  pca_variance = round((pca_result$sdev^2 / sum(pca_result$sdev^2))[1:significant_pcs] * 100, 1),
  classification_accuracy = round(best_accuracy * 100, 1),
  cohens_kappa = round(best_kappa, 3),
  morphotype_distribution = morphotype_summary,
  manova_results = manova_summary
)

print("=== ANALYSIS RESULTS SUMMARY ===")
print(paste("Original sample size:", results_summary$original_sample_size))
print(paste("Final sample size:", results_summary$final_sample_size))
print(paste("Samples removed due to missing data:", results_summary$samples_removed))
print("Sample sizes by population:")
print(results_summary$sample_sizes)
print(paste("PCA variance explained (first", significant_pcs, "PCs):", 
            paste(results_summary$pca_variance, collapse = "%, "), "%"))
print(paste("Classification accuracy:", results_summary$classification_accuracy, "%"))
print(paste("Cohen's Kappa:", results_summary$cohens_kappa))

# Save results
save(shape, pca_result, nnet_model, morphotypes, results_summary,
     file = file.path(base_path, "otolith_analysis_results.RData"))

# Export key data tables (using cleaned data)
write.csv(master_list_clean, file.path(base_path, "analysis_data_with_morphotypes.csv"), row.names = FALSE)
write.csv(data.frame(pc_scores, pop = master_list_clean$pop, morphotype = morphotypes),
          file.path(base_path, "pc_scores_and_classifications.csv"), row.names = FALSE)
write.csv(pairwise_results, file.path(base_path, "pairwise_comparisons.csv"), row.names = FALSE)

cat("Analysis complete! Results saved to:", base_path, "\n")
cat("Key files generated:\n")
cat("- otolith_analysis_results.RData (complete R workspace)\n")
cat("- analysis_data_with_morphotypes.csv (data with morphotype classifications)\n") 
cat("- pc_scores_and_classifications.csv (PC scores and group assignments)\n")
cat("- pairwise_comparisons.csv (statistical comparison results)\n")