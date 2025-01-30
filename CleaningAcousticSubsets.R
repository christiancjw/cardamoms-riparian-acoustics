

# Load required packages
library(dplyr) 
  # filter(): Used to remove rows containing <missing row> due to acoustic subsetting (1 in 5 minutes recorded)
  # mutate(): Used to create new columns (date, time, site, device)
  # select(): Used to reorder the columns in the cleaned dataframe
library(stringr)
  # str_detect(): Used to detect the string <missing row> in the FileName column
library(readr)
  # read_csv(): Used to read in the csv files better than base R read.csv()
  # write_csv(): Used to save clean data more efficiently than base R write.csv()

# Function to clean and export acoustic index data
clean_acoustic_data <- function(base_dir, site, device, start_date, end_date, output_dir = "clean_data") {
  
  # Convert start and end dates to Date format
  start_date <- as.Date(start_date, "%Y%m%d")
  end_date <- as.Date(end_date, "%Y%m%d")
  
  # Construct the device folder path
  device_path <- file.path(base_dir, site, device)
  
  # Get all date subdirectories within the device folder
  date_folders <- list.dirs(device_path, full.names = TRUE, recursive = FALSE)
  
  # Filter date folders within the specified range
  valid_folders <- date_folders[basename(date_folders) %in% format(seq.Date(start_date, end_date, by = "day"), "%Y%m%d")]
  
  print(valid_folders)  # Debugging: Check selected folders
  
  # Ensure output directory exists (create site and device folders)
  site_output_dir <- file.path(output_dir, site)
    if (!dir.exists(site_output_dir)) dir.create(site_output_dir)
  
  device_output_dir <- file.path(site_output_dir, device)
   if (!dir.exists(device_output_dir)) dir.create(device_output_dir)
  
  # Process each valid folder
  for (folder in valid_folders) {
    date_value <- basename(folder)  # Extract date from folder name
    
    # Find all "Towsey.Acoustic.Indices.csv" files in the folder (recursive search)
    csv_files <- list.files(folder, pattern = "Towsey.Acoustic.Indices.csv$", full.names = TRUE, recursive = TRUE)
    
    print(csv_files)  # Debugging: Check found CSV files
    
    for (csv_file in csv_files) {
      # Read the CSV file
      df <- read_csv(csv_file, show_col_types = FALSE)
      
      # Remove rows with "<missing row>" in FileName
      df_clean <- df %>%
        filter(!str_detect(FileName, "<missing row>")) %>%
        mutate(
          Date = substr(FileName, 1, 8),  # Extract YYYYMMDD from FileName
          Time = substr(FileName, 10, 15),  # Extract HHMMSS from FileName
          Site = site,
          Device = device
        ) %>%
        select(Site, Device, Date, Time, everything())  # Reorder columns
      
      # Define output file path within site/device subfolders
      output_file <- file.path(device_output_dir, paste0(site, "_", device, "_", date_value, "_cleaned.csv"))
      
      # Save cleaned file
      write_csv(df_clean, output_file)
      
      print(paste("Cleaned file saved:", output_file))
    }
  }
  
  print("Donezo my friend")
}


# Example Usage
base_directory <- "/Volumes/CChing SSD/Acoustics/Pursat/2023-2024/_data/concatenated_output"

# Run for specific sites, devices, and date ranges

# 
clean_acoustic_data(
  base_dir = base_directory,
  site = "Arai",     # Select specific sites (NULL for all)
  device = "CI02",    # Select specific devices (NULL for all)
  start_date = "20231122",        # Start date in YYYYMMDD format (NULL for all)
  end_date = "20231122")          # End date in YYYYMMDD format (NULL for all)
