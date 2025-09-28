# Otolith Shape Analysis: shapeR to Momocs Integration
# Author: [Your Name]
# Date: September 26, 2025

# Load required libraries
library(shapeR)
library(Momocs)
library(here)
install.packages("caret")
install.packages("randomForest")
library(randomForest)
library(caret)

################################################################################
# 1. LOAD SHAPER DATA
################################################################################

# Load pre-processed shapeR data with coefficients
load(here("Data", "oto_coefficients.RData"))

# Clean up file names (remove .jpg extension)
shape@master.list.org$picname <- gsub("\\.jpg$", "", shape@master.list.org$picname)

# Enrich master list and plot average shapes
shape <- enrich.master.list(shape)
plotWaveletShape(shape, "pop")

################################################################################
# 2. CONVERT SHAPER TO MOMOCS FORMAT
################################################################################

cat("\n=== Converting shapeR to Momocs coo object ===\n")

# Extract outlines from shapeR object
outlines <- shape@outline.list.org

# Remove empty elements
if("" %in% names(outlines)) {
  outlines <- outlines[names(outlines) != ""]
}

# Initialize storage for outline conversion
outline_list <- list()
outline_names <- character()
population_info <- character()
counter <- 1

# Convert each population's outlines
for(pop_name in names(outlines)) {
  cat("Processing population:", pop_name, "\n")
  pop_outlines <- outlines[[pop_name]]
  success_count <- 0
  
  for(individual_name in names(pop_outlines)) {
    coords_data <- pop_outlines[[individual_name]]
    
    # Validate coordinates
    if(is.list(coords_data) && all(c("X", "Y") %in% names(coords_data))) {
      X_coords <- coords_data$X
      Y_coords <- coords_data$Y
      
      # Quality check
      if(length(X_coords) == length(Y_coords) && 
         length(X_coords) >= 25 && 
         !any(is.na(c(X_coords, Y_coords)))) {
        
        # Store valid outline
        outline_list[[counter]] <- cbind(X = X_coords, Y = Y_coords)
        outline_names[counter] <- paste(pop_name, individual_name, sep = "_")
        population_info[counter] <- pop_name
        counter <- counter + 1
        success_count <- success_count + 1
      }
    }
  }
  cat("Population", pop_name, "completed:", success_count, "extractions\n")
}

# Summary of extraction
cat("\n=== EXTRACTION SUMMARY ===\n")
cat("Total outlines extracted:", length(outline_list), "\n")
print(table(population_info))

################################################################################
# 3. CREATE CLEAN MOMOCS COO OBJECT
################################################################################

# Prepare data for Momocs
names(outline_list) <- outline_names

# Create factor data frame
fac_df <- data.frame(
  pop = factor(population_info),
  individual = outline_names,
  row.names = outline_names,
  stringsAsFactors = FALSE
)

# Validate all outlines have proper matrix structure
for(i in 1:length(outline_list)) {
  if(!is.matrix(outline_list[[i]])) {
    outline_list[[i]] <- as.matrix(outline_list[[i]])
  }
}

# Create Momocs coo object
coo_outline <- Out(outline_list, fac = fac_df)

cat("SUCCESS: Momocs coo object created!\n")
cat("Number of outlines:", length(coo_outline), "\n")
cat("Population distribution:\n")
print(table(coo_outline$fac$pop))

################################################################################
# 4. FOURIER ANALYSIS
################################################################################

cat("\n=== FOURIER ANALYSIS ===\n")

# Perform Elliptic Fourier Analysis
oto.f <- efourier(coo_outline, nb.h = 10)
cat("Fourier analysis completed with 10 harmonics\n")
cat("Coefficients matrix dimensions:", dim(oto.f$coe), "\n")

# Create alias for consistency
oto_fourier <- oto.f

################################################################################
# 5. BASIC ANALYSIS AND VISUALIZATION
################################################################################

# Boxplot of Fourier coefficients
boxplot(oto.f, main = "Fourier Coefficients Distribution")

# Principal Component Analysis
bot.p <- PCA(oto.f)
plot(bot.p, main = "PCA of Fourier Coefficients")

################################################################################

