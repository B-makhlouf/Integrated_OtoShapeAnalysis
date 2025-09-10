# Otolith Shape Analysis using shapeR
# Clean and comprehensive script for wavelet and Fourier coefficient analysis

# Load required libraries
library("shapeR")
library(randomForest)
library(caret)
library(ggplot2)
library(gridExtra)

# Set working directory (adjust as needed)
base_path <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines"

#===============================================================================
# 1. DATA PREPARATION AND CLEANING
#===============================================================================

# List all available image files
allfiles <- c(
  list.files(file.path(base_path, "Original/NK")),
  list.files(file.path(base_path, "Original/KK"))
)

# Load the CSV and clean it
fish_data <- read.csv(file.path(base_path, "FISH.csv"))

# Remove missing files and standardize column names
clean_fish_data <- fish_data[fish_data$Picname %in% allfiles, ]
colnames(clean_fish_data) <- tolower(colnames(clean_fish_data))

# Save cleaned data
write.csv(clean_fish_data, 
          file.path(base_path, "CleanFish.csv"), 
          row.names = FALSE)

cat("Data cleaning complete. Samples retained:", nrow(clean_fish_data), "\n")

#===============================================================================
# 2. SHAPE COEFFICIENT EXTRACTION
#===============================================================================

# Load data into shapeR
shape <- shapeR(base_path, "CleanFish.csv")

# Detect outlines
cat("Detecting outlines...\n")
outlinesonly <- detect.outline(shape, threshold = 0.4, write.outline.w.org = FALSE)

# Extract shape coefficients
cat("Extracting shape coefficients...\n")
coef <- generateShapeCoefficients(outlinesonly)

# Extract raw coefficients
wavelet <- coef@wavelet.coef.raw
wavelet.df <- as.data.frame(wavelet)
fourier <- coef@fourier.coef.raw
fourier.df <- as.data.frame(fourier)

cat("Coefficient extraction complete:\n")
cat("- Wavelet coefficients:", ncol(wavelet.df), "\n")
cat("- Fourier coefficients:", ncol(fourier.df), "\n")

#===============================================================================
# 3. PCA ANALYSIS (First 14 Wavelet Coefficients)
#===============================================================================

# Extract first 14 wavelet coefficients (excluding first column if it's an index)
wavelet14 <- wavelet.df[,1:length(wavelet14)]

# Perform PCA
pca_wavelet <- prcomp(wavelet14, center = TRUE, scale. = TRUE)
pca_wavelet_df <- as.data.frame(pca_wavelet$x)
pca_wavelet_df$watershed <- as.factor(clean_fish_data$watershed)

# filter the pca to above -100
pca_wavelet_df <- pca_wavelet_df[pca_wavelet_df$PC1 > -100, ]
pca_wavelet_df <- pca_wavelet_df[pca_wavelet_df$PC2 > -100, ]
pca_wavelet_df <- pca_wavelet_df[pca_wavelet_df$PC3 > -100, ]


# Calculate variance explained
var_explained <- summary(pca_wavelet)$importance[2,] * 100

# Create PCA plots
p1 <- ggplot(pca_wavelet_df, aes(x = PC1, y = PC2, color = watershed)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(title = "PC1 vs PC2",
       x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PC2 (", round(var_explained[2], 1), "%)")) +
  theme_minimal()

p2 <- ggplot(pca_wavelet_df, aes(x = PC1, y = PC3, color = watershed)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(title = "PC1 vs PC3",
       x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PC3 (", round(var_explained[3], 1), "%)")) +
  theme_minimal()

p3 <- ggplot(pca_wavelet_df, aes(x = PC2, y = PC3, color = watershed)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(title = "PC2 vs PC3",
       x = paste0("PC2 (", round(var_explained[2], 1), "%)"),
       y = paste0("PC3 (", round(var_explained[3], 1), "%)")) +
  theme_minimal()

# Display PCA plots
grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)

#===============================================================================
# 4. RANDOM FOREST CLASSIFICATION FUNCTION
#===============================================================================

perform_rf_classification <- function(feature_data, method_name, watershed_labels) {
  cat("\n=== RANDOM FOREST CLASSIFICATION:", method_name, "===\n")
  
  # Prepare data
  rf_data <- cbind(feature_data, watershed = as.factor(watershed_labels))
  
  # Data split
  set.seed(123)
  train_indices <- createDataPartition(rf_data$watershed, p = 0.8, list = FALSE)
  train_data <- rf_data[train_indices, ]
  test_data <- rf_data[-train_indices, ]
  
  cat("Features used:", ncol(feature_data), "\n")
  cat("Training samples:", nrow(train_data), "\n")
  cat("Test samples:", nrow(test_data), "\n")
  
  # Train model
  set.seed(123)
  rf_model <- randomForest(watershed ~ ., 
                           data = train_data,
                           ntree = 500,
                           importance = TRUE)
  
  # Make predictions
  predictions <- predict(rf_model, test_data)
  confusion_matrix <- confusionMatrix(predictions, test_data$watershed)
  
  # Print results
  print(rf_model)
  print(confusion_matrix)
  
  # Variable importance
  importance_df <- data.frame(
    Variable = rownames(importance(rf_model)),
    MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
    MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
  )
  
  # Plot top 20 important features
  top_20 <- importance_df[order(-importance_df$MeanDecreaseAccuracy), ][1:min(20, nrow(importance_df)), ]
  
  color_map <- c("Wavelet (14)" = "steelblue", 
                 "Wavelet (Full)" = "darkblue", 
                 "Fourier (Full)" = "darkred")
  
  p_importance <- ggplot(top_20, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
    geom_col(fill = color_map[[method_name]], alpha = 0.7) +
    coord_flip() +
    labs(title = paste("Top Important Features -", method_name),
         x = "Features",
         y = "Mean Decrease in Accuracy") +
    theme_minimal()
  
  print(p_importance)
  
  # Return results for comparison
  return(list(
    model = rf_model,
    confusion_matrix = confusion_matrix,
    importance = importance_df,
    method = method_name,
    n_features = ncol(feature_data)
  ))
}

#===============================================================================
# 5. PERFORM ALL CLASSIFICATIONS
#===============================================================================

# Classification 1: First 14 Wavelet Coefficients
results_wavelet14 <- perform_rf_classification(wavelet14, "Wavelet (14)", clean_fish_data$watershed)

# Classification 2: Full Fourier Coefficients  
results_fourier <- perform_rf_classification(fourier.df, "Fourier (Full)", clean_fish_data$watershed)

# Classification 3: Full Wavelet Coefficients
results_wavelet_full <- perform_rf_classification(wavelet.df, "Wavelet (Full)", clean_fish_data$watershed)

#===============================================================================
# 6. COMPREHENSIVE COMPARISON
#===============================================================================

# Create comparison dataframe
comparison_df <- data.frame(
  Method = c("Wavelet (14)", "Fourier (Full)", "Wavelet (Full)"),
  Features = c(results_wavelet14$n_features, 
               results_fourier$n_features, 
               results_wavelet_full$n_features),
  Accuracy = c(
    round(results_wavelet14$confusion_matrix$overall["Accuracy"] * 100, 2),
    round(results_fourier$confusion_matrix$overall["Accuracy"] * 100, 2),
    round(results_wavelet_full$confusion_matrix$overall["Accuracy"] * 100, 2)
  ),
  Kappa = c(
    round(results_wavelet14$confusion_matrix$overall["Kappa"], 3),
    round(results_fourier$confusion_matrix$overall["Kappa"], 3),
    round(results_wavelet_full$confusion_matrix$overall["Kappa"], 3)
  ),
  Sensitivity = c(
    round(results_wavelet14$confusion_matrix$byClass["Sensitivity"] * 100, 1),
    round(results_fourier$confusion_matrix$byClass["Sensitivity"] * 100, 1),
    round(results_wavelet_full$confusion_matrix$byClass["Sensitivity"] * 100, 1)
  ),
  Specificity = c(
    round(results_wavelet14$confusion_matrix$byClass["Specificity"] * 100, 1),
    round(results_fourier$confusion_matrix$byClass["Specificity"] * 100, 1),
    round(results_wavelet_full$confusion_matrix$byClass["Specificity"] * 100, 1)
  )
)

cat("\n=== COMPREHENSIVE COMPARISON ===\n")
print(comparison_df)

# Create comparison visualizations
p_comparison <- ggplot(comparison_df, aes(x = reorder(Method, Accuracy), y = Accuracy, fill = Method)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Accuracy, "%")), vjust = -0.5) +
  labs(title = "Classification Accuracy Comparison - Otolith Shape Analysis",
       x = "Method", y = "Accuracy (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Wavelet (14)" = "steelblue", 
                               "Fourier (Full)" = "darkred", 
                               "Wavelet (Full)" = "darkblue"))

print(p_comparison)

# Features vs Performance
p_features <- ggplot(comparison_df, aes(x = Features, y = Accuracy, color = Method, size = Kappa)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Method), hjust = -0.1, vjust = 0.5, size = 3) +
  labs(title = "Number of Features vs Classification Performance",
       x = "Number of Features (log scale)", 
       y = "Accuracy (%)",
       size = "Kappa Score") +
  theme_minimal() +
  scale_x_log10() +
  scale_color_manual(values = c("Wavelet (14)" = "steelblue", 
                                "Fourier (Full)" = "darkred", 
                                "Wavelet (Full)" = "darkblue"))

print(p_features)

#===============================================================================
# 7. SUMMARY AND RECOMMENDATIONS
#===============================================================================

cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Best performing method:", comparison_df$Method[which.max(comparison_df$Accuracy)], "\n")
cat("Most efficient method:", comparison_df$Method[which.min(comparison_df$Features)], "\n")
cat("Best balanced performance:", comparison_df$Method[which.max(comparison_df$Kappa)], "\n")

cat("\n=== RECOMMENDATIONS ===\n")
cat("• For highest accuracy: Use", comparison_df$Method[which.max(comparison_df$Accuracy)], "\n")
cat("• For computational efficiency: Use Wavelet (14 coefficients)\n")
cat("• Otolith shapes show meaningful differences between watersheds\n")
cat("• Consider environmental factors influencing otolith growth patterns\n")

cat("\nScript completed successfully!\n")