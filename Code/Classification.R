library(shapeR)
library(randomForest)
library(caret)

# Load the data
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_coefficients.RData")

#####################################
# WAVELET ANALYSIS
#####################################

# Extract wavelets 
Wavelets <- as.data.frame(shape@wavelet.coef.raw) 
# Remove mean radii, the first column 
Wavelets <- Wavelets[,-1]

# Get the labels from the first two letters of row names
label <- substr(rownames(Wavelets), 1, 2)
# Make sure it's a factor
label <- as.factor(label)

# Create training and test sets (80/20 split)
set.seed(123)
train_indices <- createDataPartition(label, p = 0.8, list = FALSE)
train_data <- Wavelets[train_indices, ]
test_data <- Wavelets[-train_indices, ]
train_label <- label[train_indices]
test_label <- label[-train_indices]

# Check the split
cat("=== WAVELET ANALYSIS ===\n")
cat("Training samples:", nrow(train_data), "\n")
cat("Test samples:", nrow(test_data), "\n")
table(train_label)
table(test_label)

# Train random forest model
set.seed(123)
rf_model_wavelet <- randomForest(x = train_data, 
                                 y = train_label,
                                 ntree = 1000,
                                 mtry = 4, 
                                 importance = TRUE)

# View training results
print(rf_model_wavelet)

# Make predictions on test set
predictions <- predict(rf_model_wavelet, test_data)

# Confusion matrix on test set
confusion_matrix_wavelet <- confusionMatrix(predictions, test_label)
print(confusion_matrix_wavelet)

# Variable importance
importance(rf_model_wavelet)
varImpPlot(rf_model_wavelet, main = "Wavelet Variable Importance")

# Get error rate
plot(rf_model_wavelet, main = "Wavelet Random Forest Error Rate")
legend("topright", colnames(rf_model_wavelet$err.rate), 
       col = 1:ncol(rf_model_wavelet$err.rate), 
       lty = 1:ncol(rf_model_wavelet$err.rate))

#####################################
# FOURIER ANALYSIS
#####################################

# Extract fourier
Fourier <- as.data.frame(shape@fourier.coef.raw)
# Remove mean radii, the first column
Fourier <- Fourier[,-1]

# Get the labels from the first two letters of row names
label_fourier <- substr(rownames(Fourier), 1, 2)
# Make sure it's a factor
label_fourier <- as.factor(label_fourier)

# Create training and test sets (80/20 split)
set.seed(123)
train_indices_fourier <- createDataPartition(label_fourier, p = 0.8, list = FALSE)
train_data_fourier <- Fourier[train_indices_fourier, ]
test_data_fourier <- Fourier[-train_indices_fourier, ]
train_label_fourier <- label_fourier[train_indices_fourier]
test_label_fourier <- label_fourier[-train_indices_fourier]

# Check the split
cat("\n=== FOURIER ANALYSIS ===\n")
cat("Training samples:", nrow(train_data_fourier), "\n")
cat("Test samples:", nrow(test_data_fourier), "\n")
table(train_label_fourier)
table(test_label_fourier)

# Train random forest model
set.seed(123)
rf_model_fourier <- randomForest(x = train_data_fourier, 
                                 y = train_label_fourier,
                                 ntree = 1000,
                                 mtry = 4, 
                                 importance = TRUE)

# View training results
print(rf_model_fourier)

# Make predictions on test set
predictions_fourier <- predict(rf_model_fourier, test_data_fourier)

# Confusion matrix on test set
confusion_matrix_fourier <- confusionMatrix(predictions_fourier, test_label_fourier)
print(confusion_matrix_fourier)

# Variable importance
importance(rf_model_fourier)
varImpPlot(rf_model_fourier, main = "Fourier Variable Importance")

# Get error rate
plot(rf_model_fourier, main = "Fourier Random Forest Error Rate")
legend("topright", colnames(rf_model_fourier$err.rate), 
       col = 1:ncol(rf_model_fourier$err.rate), 
       lty = 1:ncol(rf_model_fourier$err.rate))