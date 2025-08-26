################### 
###### This script is for data organizing and tracking, will produce metadata file photoMetadata.csv 
library(here)
library(tidyverse)

### List out the three directories 
KuskoDIR <- here("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines/KK") 
#YukonDIR <- here("Raw_Images/Yukon")
NushDIR <- here("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines/NK")

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

#cat("Processing Yukon directory...\n")
#yukon_data <- process_directory(YukonDIR, "Yukon")

cat("Processing Nush directory...\n") 
nush_data <- process_directory(NushDIR, "Nush")

# Combine all data
photoMetadata <- bind_rows(kusko_data, nush_data)

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
watershed_counts <- table(photoMetadata$Watershed, useNA = "ifany")
print(watershed_counts)

cat("\nPhotos by watershed and year:\n")
print(table(photoMetadata$Watershed, photoMetadata$Year, useNA = "ifany"))

cat("\nSampling progress (out of 500 target per watershed):\n")
for(watershed in names(watershed_counts)) {
  count <- watershed_counts[watershed]
  percent <- round((count/500) * 100, 1)
  cat(sprintf("%-12s: %3d/500 (%5.1f%%)\n", watershed, count, percent))
}

cat("\nSampling progress (out of 500 target per watershed):\n")
for(watershed in names(watershed_counts)) {
  count <- watershed_counts[watershed]
  percent <- round((count/500) * 100, 1)
  cat(sprintf("%-12s: %3d/500 (%5.1f%%)\n", watershed, count, percent))
}

# Save to CSV
write_csv(photoMetadata, "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines/FISH.csv")
cat("\nMetadata saved to photoMetadata.csv\n")

# Create summary figures
library(ggplot2)
library(gridExtra)

# Prepare data for plotting
watershed_summary <- photoMetadata %>%
  count(Watershed, name = "Count") %>%
  mutate(
    Target = 500,
    Percent = round((Count/Target) * 100, 1),
    Status = ifelse(Count >= Target, "Complete", "In Progress")
  )

# Plot 1: Sampling progress toward 500 target
p1 <- ggplot(watershed_summary, aes(x = Watershed, y = Count, fill = Status)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = paste0(Count, "\n(", Percent, "%)")), 
            vjust = -0.5, size = 3.5) +
  annotate("text", x = 0.7, y = 520, label = "Target: 500", color = "red", size = 3) +
  labs(title = "Sampling Progress by Watershed", 
       subtitle = "Progress toward 500 photos per watershed",
       x = "Watershed", y = "Number of Photos") +
  scale_fill_manual(values = c("In Progress" = "steelblue", "Complete" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Plot 2: Photos by watershed and year
year_summary <- photoMetadata %>%
  count(Watershed, Year) %>%
  complete(Watershed, Year, fill = list(n = 0))

p2 <- ggplot(year_summary, aes(x = factor(Year), y = n, fill = Watershed)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  labs(title = "Photo Distribution by Year and Watershed",
       x = "Year", y = "Number of Photos") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Combine plots
combined_plot <- grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))

# Save the figure
ggsave(here("sampling_summary.png"), combined_plot, width = 10, height = 8, dpi = 300)
cat("Summary figure saved to sampling_summary.png\n")

# Display first few rows
cat("\nFirst few rows of photoMetadata:\n")
print(head(photoMetadata))
