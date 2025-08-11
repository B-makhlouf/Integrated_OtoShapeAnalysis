################### 
###### This script is for data organizing and tracking, will produce metadata file photoMetadata.csv 
library(here)
library(tidyverse)

### List out the three directories 
KuskoDIR <- here("Raw_Images/Kusko") 
YukonDIR <- here("Raw_Images/Yukon")
NushDIR <- here("Raw_Images/Nush")

# Start a data frame with Columns "Watershed", "Year", "Folder", "Picname", "cal" 
photoMetadata <- data.frame(Watershed = character(),
                            Year = integer(),
                            Folder = character(),
                            Picname = character(),
                            cal = character(),
                            stringsAsFactors = FALSE)

# Function to extract metadata from filename
extract_metadata <- function(filename) {
  # Remove .jpg extension and split by underscore
  name_parts <- str_split(str_remove(filename, "\\.jpg$"), "_")[[1]]
  
  if(length(name_parts) >= 2) {
    # Check if first part looks like a year (4 digits)
    year_string <- name_parts[1]
    if(!str_detect(year_string, "^\\d{4}$")) {
      cat("WARNING: File", filename, "- first part '", year_string, "' doesn't look like a 4-digit year\n")
      year <- NA_integer_
    } else {
      year <- as.integer(year_string)
    }
    
    code <- name_parts[2]
    
    # Determine watershed from code
    watershed <- case_when(
      code == "kk" ~ "Kuskokwim",
      code == "yk" ~ "Yukon", 
      code == "nk" ~ "Nushagak",
      TRUE ~ NA_character_
    )
    
    # Warn about unrecognized codes
    if(is.na(watershed)) {
      cat("WARNING: File", filename, "- unrecognized code '", code, "'\n")
    }
    
    return(list(year = year, watershed = watershed, filename = filename))
  } else {
    cat("WARNING: File", filename, "- doesn't have expected format (need at least 2 parts separated by _)\n")
    return(list(year = NA_integer_, watershed = NA_character_, filename = filename))
  }
}

# Function to process a directory
process_directory <- function(dir_path, folder_name) {
  if(dir.exists(dir_path)) {
    # Get all .jpg files in the directory
    jpg_files <- list.files(dir_path, pattern = "\\.jpg$", full.names = FALSE, ignore.case = TRUE)
    
    if(length(jpg_files) > 0) {
      # Extract metadata for each file
      metadata_list <- map(jpg_files, extract_metadata)
      
      # Create dataframe for this directory
      dir_df <- data.frame(
        Watershed = map_chr(metadata_list, "watershed"),
        Year = map_int(metadata_list, "year"),
        Folder = folder_name,
        Picname = jpg_files,
        cal = "200",
        stringsAsFactors = FALSE
      )
      
      # Filter out any rows where watershed extraction failed
      dir_df <- filter(dir_df, !is.na(Watershed))
      
      return(dir_df)
    }
  }
  
  # Return empty dataframe if no files found or directory doesn't exist
  return(data.frame(Watershed = character(),
                    Year = integer(),
                    Folder = character(),
                    Picname = character(),
                    cal = character(),
                    stringsAsFactors = FALSE))
}

# Process each directory
cat("Processing Kusko directory...\n")
kusko_data <- process_directory(KuskoDIR, "Kusko")

cat("Processing Yukon directory...\n")
yukon_data <- process_directory(YukonDIR, "Yukon")

cat("Processing Nush directory...\n") 
nush_data <- process_directory(NushDIR, "Nush")

# Combine all data
photoMetadata <- bind_rows(kusko_data, yukon_data, nush_data)

# Display summary
cat("\n=== PROCESSING COMPLETE ===\n")
cat("Total photos processed:", nrow(photoMetadata), "\n")

# Check for any rows with NAs
na_rows <- photoMetadata[is.na(photoMetadata$Watershed) | is.na(photoMetadata$Year), ]
if(nrow(na_rows) > 0) {
  cat("\nFiles with missing data (excluded from final dataset):\n")
  print(na_rows[c("Folder", "Picname")])
}

cat("\nPhotos by watershed:\n")
print(table(photoMetadata$Watershed, useNA = "ifany"))
cat("\nPhotos by year:\n")
print(table(photoMetadata$Year, useNA = "ifany"))

# Save to CSV
write_csv(photoMetadata, here("photoMetadata.csv"))
cat("\nMetadata saved to photoMetadata.csv\n")

# Display first few rows
cat("\nFirst few rows of photoMetadata:\n")
print(head(photoMetadata))