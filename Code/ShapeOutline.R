# ###From the top 
# install.packages("shapeR")
library(shapeR)
library(here)
# shape<- shapeR("C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos","FISH.CSV")
# shape<- detect.outline(shape, threshold = .2, write.outline.w.org = TRUE)
# save(shape, file = "C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/oto_outlinesOnly.RData")
# 
# # outlines extracted 09262025
# ok, now lets load those in (so that this script can be used if they're already extracted)
# load(here("Data", "oto_outlinesOnly.RData"))
# 
# shape <- smoothout(shape, n = 100) # Smoothout 
# 
# # remove problematic 
# shape<-remove.outline(shape,"KK","2024_kk_163")
# shape<-remove.outline(shape,"NK","2019_nk_206")
# 
# shape<- generateShapeCoefficients(shape) # Generate Shape Coefficients 
# 
# #Save the coefficients
# save(shape,file = "C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/oto_coefficients.RData")
# Read in 
load(here("Data", "oto_coefficients.RData"))
# Remove .jpg extension from picname column
shape@master.list.org$picname <- gsub("\\.jpg$", "", shape@master.list.org$picname)
#Enrich master list 
shape<- enrich.master.list(shape)
# Plot the average 
plotWaveletShape(shape, "pop")


################################################################################
#===============================================================================
# CONVERT SHAPER OUTLINES TO MOMOCS COO FORMAT
#===============================================================================

cat("\n=== Converting shapeR to Momocs coo object ===\n")

# Extract outlines from shapeR object
outlines <- shape@outline.list.org

# Remove empty elements if they exist
if("" %in% names(outlines)) {
  outlines <- outlines[names(outlines) != ""]
}

cat("Processing populations:", paste(names(outlines), collapse = ", "), "\n")

# Initialize storage
outline_list <- list()
outline_names <- character()
population_info <- character()
counter <- 1

# Process each population
for(pop_name in names(outlines)) {
  cat("Processing population:", pop_name, "\n")
  pop_outlines <- outlines[[pop_name]]
  success_count <- 0
  
  # Process each individual
  for(individual_name in names(pop_outlines)) {
    coords_data <- pop_outlines[[individual_name]]
    
    # Validate and extract coordinates
    if(is.list(coords_data) && all(c("X", "Y") %in% names(coords_data))) {
      X_coords <- coords_data$X
      Y_coords <- coords_data$Y
      
      # Check data quality
      if(length(X_coords) == length(Y_coords) && length(X_coords) >= 3) {
        coords_matrix <- cbind(X = X_coords, Y = Y_coords)
        
        if(!any(is.na(coords_matrix))) {
          # Store valid outline
          outline_list[[counter]] <- coords_matrix
          outline_names[counter] <- paste(pop_name, individual_name, sep = "_")
          population_info[counter] <- pop_name
          counter <- counter + 1
          success_count <- success_count + 1
        } else {
          cat("  Skipping", individual_name, "- contains NA values\n")
        }
      } else {
        cat("  Skipping", individual_name, "- insufficient points\n")
      }
    } else {
      cat("  Skipping", individual_name, "- no X/Y coordinates\n")
    }
    
    # Progress update every 100 individuals
    if(success_count %% 100 == 0 && success_count > 0) {
      cat("  ... processed", success_count, "individuals\n")
    }
  }
  
  cat("Population", pop_name, "completed:", success_count, "extractions\n")
}

# Summary
cat("\n=== EXTRACTION SUMMARY ===\n")
cat("Total outlines extracted:", length(outline_list), "\n")
cat("Population distribution:\n")
print(table(population_info))

# Create Momocs coo object
if(length(outline_list) > 0) {
  # Prepare data
  names(outline_list) <- outline_names
  fac_df <- data.frame(
    pop = factor(population_info),
    individual = outline_names,
    row.names = outline_names
  )
  
  # Create coo object with error handling
  cat("\nCreating Momocs coo object...\n")
  tryCatch({
    coo_object <- Out(outline_list, fac = fac_df)
    
    cat("SUCCESS: Coo object created!\n")
    cat("Number of outlines:", length(coo_object), "\n")
    cat("Population distribution:\n")
    print(table(coo_object$fac$pop))
    
    # Validation plot
    cat("\nValidation plot...\n")
    tryCatch({
      plot(coo_object[1:min(6, length(coo_object))])
      cat("Plot successful!\n")
    }, error = function(e) {
      cat("Plot failed, but coo object created successfully\n")
    })
    
  }, error = function(e) {
    cat("ERROR creating coo object:", e$message, "\n")
    cat("Debugging first outline:\n")
    str(outline_list[[1]])
    print(head(outline_list[[1]]))
  })
  
} else {
  cat("ERROR: No valid outlines extracted!\n")
  cat("Check shapeR object structure\n")
}


library(Momocs)

# make it officially a coo objct 
# Complete the coo object creation (replace your incomplete line at the end)
coo_outline <- Out(outline_list, fac = fac_df)

  
################################################################################
### MOMOCS 
################################################################################
################################################################################


#install.packages("Momocs")
library(Momocs)

# Remove outlines with too few points
min_points <- 25  # Increased to ensure nb.h=10 works
point_counts <- sapply(outline_list, nrow)
good_outlines <- point_counts >= min_points

# Keep only good outlines
outline_list_clean <- outline_list[good_outlines]

cat("Removed", sum(!good_outlines), "bad outlines\n")
cat("Keeping", length(outline_list_clean), "good outlines\n")

# Run Fourier analysis
oto.f <- efourier(coo_outline, nb.h = 10)
oto_fourier <- oto.f



boxplot(oto.f)

bot.p <- PCA(oto.f)


