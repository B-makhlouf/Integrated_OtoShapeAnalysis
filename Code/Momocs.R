library(Momocs)

# First, read in the coo-specified outline data 
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/oto_coo_outline.RData")

# Get list of all files in the REDO Alignment folder
redoloc <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/REDO Alignment"
redo_files <- list.files(redoloc, full.names = FALSE)

# Create a vector to store names to remove
names_to_remove <- c()

# Loop through each file and extract the specimen name
for (file in redo_files) {
  # Extract the base name without extension
  base_name <- tools::file_path_sans_ext(file)
  
  # Remove "_alignment" suffix if present
  base_name <- sub("_alignment$", "", base_name)
  
  # Extract folder code (NK, KK, or YK) from the filename
  parts <- strsplit(base_name, "_")[[1]]
  
  if (length(parts) >= 2) {
    folder_code <- toupper(parts[2])  # Convert to uppercase (NK, KK, YK)
    
    # Construct the full name as it appears in coo_outline
    full_name <- paste0(folder_code, "_", base_name)
    
    names_to_remove <- c(names_to_remove, full_name)
  }
}

# Get specimen names correctly
available_names <- names(coo_outline$coo)

# Check which names are actually in the coo object
matched_names <- names_to_remove[names_to_remove %in% available_names]
cat("Found", length(matched_names), "specimens to remove out of", length(names_to_remove), "requested\n")

# Get indices to KEEP
indices_to_remove <- which(available_names %in% matched_names)
indices_to_keep <- setdiff(1:length(coo_outline$coo), indices_to_remove)

cat("Keeping", length(indices_to_keep), "specimens out of", length(coo_outline$coo), "total\n")

# Create a new Out object properly by subsetting the list and fac separately
coo_list_cleaned <- coo_outline$coo[indices_to_keep]
fac_cleaned <- coo_outline$fac[indices_to_keep, , drop = FALSE]

# Create new Out object - the first argument is the list of coordinates
coo_outline_cleaned <- Out(coo_list_cleaned, fac = fac_cleaned)

# Verify the structure
cat("\nCleaned object structure:\n")
print(coo_outline_cleaned)
cat("\nNumber of shapes:", length(coo_outline_cleaned$coo), "\n")
cat("Number of fac rows:", nrow(coo_outline_cleaned$fac), "\n")

# Check that they match
if(length(coo_outline_cleaned$coo) != nrow(coo_outline_cleaned$fac)) {
  stop("Mismatch between number of shapes and fac rows!")
}

# Now proceed with Fourier analysis
fourier <- efourier(coo_outline_cleaned, nb.h=13, norm=FALSE)

# Verify fourier object
cat("\nFourier object structure:\n")
print(fourier)
cat("Number of coefficient rows:", nrow(fourier$coe), "\n")
cat("Number of fac rows:", nrow(fourier$fac), "\n")

# PCA 
pca <- PCA(fourier)
plot_PCA(pca, ~pop)
