# This script has a function to take concatenated outputs from Analysis.programs to clean up the sheets (remove empty rows), 
    ## as well as tag on additional metadata. 
# the Function allows the input of a range of dates, which is usually the entire dataset.


#### Load required packages ----
library(dplyr) 
  # filter(): Used to remove rows containing <missing row> due to acoustic subsetting (1 in 5 minutes recorded)
  # mutate(): Used to create new columns (date, time, site, device)
  # select(): Used to reorder the columns in the cleaned dataframe
library(stringr)
  # str_detect(): Used to detect the string <missing row> in the FileName column
library(readr)
  # read_csv(): Used to read in the csv files better than base R read.csv()
  # write_csv(): Used to save clean data more efficiently than base R write.csv()
library(lubridate)

# Define Mapping of Sites to Strahler Orders extracted using PCRaster in QGIS --------------
strahler_map <- list("TaChey" = 1, 
                     "TaCheyHill" = 1,
                     "Arai" = 2, 
                     "Oda" = 3,
                     "KnaongBatSa" = 1, 
                     "TaSay" = 2, 
                     "Kronomh" = 3, 
                     "DamFive" = 4, 
                     "TangRang" = 4, 
                     "Kravanh Bridge" = 4, 
                     "PursatTown" = 5)


# Define Mapping of Sites to disturbance index: Calculated using Riparian Health Score (QBR)
# (1 = Undisturbed, 2 = Regenerating, 3 = Trail Stop, 4 = Modified with natural vegetation, 5 = Heavily Modified, no natural vegetation)
QBR_map <- list("TaChey" = 100, 
                   "TaCheyHill" = 100,
                   "Arai" = 95, 
                   "Oda" = 80,
                   "KnaongBatSa" = 100, 
                   "TaSay" = 95, 
                   "Kronomh" = 70, 
                   "DamFive" = 40, 
                   "TangRang" = 40, 
                   "Kravanh Bridge" = 0, 
                   "PursatTown" = 5)

# Define Mapping of Sites to Branch: 
branch_map <- list("TaChey" = 1, 
                   "TaCheyHill" = 1,
                        "Arai" = 1, 
                        "Oda" = 1,
                        "KnaongBatSa" = 2, 
                        "TaSay" = 2, 
                        "Kronomh" = 2, 
                        "DamFive" = 2, 
                        "TangRang" = 3, 
                        "Kravanh Bridge" = 3, 
                        "PursatTown" = 3)
   
#### Function to clean and export acoustic index data ----
#### Input being concatenated files produced by AcousticAnalyses Python Program - pulls towsey acoustic indices out
#### This function allows you to remove (missing no. columns due to time sub-sampling)
#### Also adds metadata such as disturbance, strahler order
clean_acoustic_data <- function(base_dir, site, device, start_date, end_date, output_dir = "clean_data/daily_indices") {
  
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
    if (!dir.exists(site_output_dir)) dir.create(site_output_dir, recursive = TRUE, showWarnings = FALSE)

  
  device_output_dir <- file.path(site_output_dir, device)
   if (!dir.exists(device_output_dir)) dir.create(device_output_dir, recursive = TRUE, showWarnings = FALSE)
  
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
          # Extract datetime from END of FileName, works for both formats
          Date = substr(FileName, nchar(FileName) - 14, nchar(FileName) - 7),  # YYYYMMDD
          Time = substr(FileName, nchar(FileName) - 5, nchar(FileName)),       # HHMMSS
          Time = sprintf("%06d", as.numeric(Time)),  # Ensure HHMMSS format
          Site = site,
          Device = device,
          Strahler = strahler_map[[site]],  # Add Strahler order column
          QBR_Score = QBR_map[[site]], 
          Branch = branch_map[[site]],
          Month = format(as.Date(Date, "%Y%m%d"), "%B%Y")  # Full month name + year (e.g. January2024)
        ) %>%
        select(Site, Device, Date, Time, Strahler, QBR_Score, Month, everything())  # Reorder columns
      
      # Define output file path within site/device subfolders
      output_file <- file.path(device_output_dir, paste0(site, "_", device, "_", date_value, "_cleaned.csv"))
      
      # Save cleaned file
      write_csv(df_clean, output_file)
      
      print(paste("Cleaned file saved:", output_file))
    }
  }
  
  print("Congratulations! It worked!")
}


##### Run for specific sites, devices, and date ranges ----


#### Setting Directory
base_directory <- "/Volumes/CChing SSD/Acoustics/Pursat/2023-2024/_data/concatenated_output"
dir.exists(base_directory)


#####  Sites 

### Ta Chey River 2024 ------------------------------------------------------------------------------------------

# Ta Chey Stream
clean_acoustic_data(
  base_dir = base_directory,
  site = "TaChey",     # Select specific sites (NULL for all)
  device = "CC01",    # Select specific devices (NULL for all)
  start_date = "20240110",        # Start date in YYYYMMDD format (NULL for all)
  end_date = "20240620")          # End date in YYYYMMDD format (NULL for all)

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaChey",    
  device = "CI10",    
  start_date = "20240110",        
  end_date = "20240620")          

# Arai
clean_acoustic_data(
  base_dir = base_directory,
  site = "Arai",    
  device = "CI02",    
  start_date = "20231121",        
  end_date = "20240629")

clean_acoustic_data(
  base_dir = base_directory,
  site = "Arai",    
  device = "CI14",    
  start_date = "20231121",        
  end_date = "20240111")

# Oda
clean_acoustic_data(
  base_dir = base_directory,
  site = "Oda",    
  device = "CI01",    
  start_date = "20231121",        
  end_date = "20240707")

clean_acoustic_data(
  base_dir = base_directory,
  site = "Oda",    
  device = "CI13",    
  start_date = "20231121",        
  end_date = "20240707")


### Ta Say River 2024 ------------------------------------------------------------------------------------------

# Knaong Bat Sa
clean_acoustic_data(
  base_dir = base_directory,
  site = "KnaongBatSa",    
  device = "CI05",    
  start_date = "20240116",        
  end_date = "20240128")

clean_acoustic_data(
  base_dir = base_directory,
  site = "KnaongBatSa",    
  device = "CI17",    
  start_date = "20240116",        
  end_date = "20240627")

# Ta Say
clean_acoustic_data(
  base_dir = base_directory,
  site = "TaSay",    
  device = "CC12",    
  start_date = "20240118",        
  end_date = "20240129")

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaSay",    
  device = "CI12",    
  start_date = "20240118",        
  end_date = "20240627")

# Kronomh
clean_acoustic_data(
  base_dir = base_directory,
  site = "Kronomh",    
  device = "CI19",    
  start_date = "20240405",        
  end_date = "20240626")

clean_acoustic_data(
  base_dir = base_directory,
  site = "Kronomh",    
  device = "CI20",    
  start_date = "20240405",        
  end_date = "20240626")

# Dam 5
clean_acoustic_data(
  base_dir = base_directory,
  site = "DamFive",    
  device = "CC11",    
  start_date = "20240113",        
  end_date = "20240623")

clean_acoustic_data(
  base_dir = base_directory,
  site = "DamFive",    
  device = "CI11",    
  start_date = "20240113",        
  end_date = "20240624")

### Pursat River 2024 ------------------------------------------------------------------------------------------

# Tang Rang
clean_acoustic_data(
  base_dir = base_directory,
  site = "TangRang",    
  device = "CC07",    
  start_date = "20231123",        
  end_date = "20240630")

clean_acoustic_data(
  base_dir = base_directory,
  site = "TangRang",    
  device = "CI07",    
  start_date = "20231123",        
  end_date = "20240705")

# Kravanh Bridge
clean_acoustic_data(
  base_dir = base_directory,
  site = "Kravanh Bridge",    
  device = "CC08",    
  start_date = "20231123",        
  end_date = "20231203")

clean_acoustic_data(
  base_dir = base_directory,
  site = "Kravanh Bridge",    
  device = "CI08",    
  start_date = "20231123",        
  end_date = "20231203")

# Pursat

clean_acoustic_data(
  base_dir = base_directory,
  site = "PursatTown",    
  device = "CC09",    
  start_date = "20231123",        
  end_date = "20240628")

clean_acoustic_data(
  base_dir = base_directory,
  site = "PursatTown",    
  device = "CI09",    
  start_date = "20231123",        
  end_date = "20240710")

 

### Basin Survey 2025 --------------
base_directory <- "/Volumes/C Media SSD/2025 Acoustics/_data/concatenated_output"
dir.exists(base_directory)


# Ta Chey Hill Camp
clean_acoustic_data(
  base_dir = base_directory,
  site = "TaCheyHill",     # Select specific sites (NULL for all)
  device = "CI07",    # Select specific devices (NULL for all)
  start_date = "20250608",        # Start date in YYYYMMDD format (NULL for all)
  end_date = "20250611")          # End date in YYYYMMDD format (NULL for all)

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaCheyHill",    
  device = "CC05",  
  start_date = "20250608",        
  end_date = "20250611")          

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaCheyHill",    
  device = "CI12",  
  start_date = "20250608",        
  end_date = "20250611")    

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaCheyHill",    
  device = "SM3352",  
  start_date = "20250608",        
  end_date = "20250611")   ## CHECK

# Ta Chey Lowlands = CI01, CI13, CC13 
clean_acoustic_data(
  base_dir = base_directory,
  site = "TaChey",    
  device = "CI01",  
  start_date = "20250608",        
  end_date = "20250612")   

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaChey",    
  device = "CI13",  
  start_date = "20250608",        
  end_date = "20250612")   

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaChey",    
  device = "CC13",  
  start_date = "20250608",        
  end_date = "20250612")   


clean_acoustic_data(
  base_dir = base_directory,
  site = "TaChey",    
  device = "SM3293",  
  start_date = "20250608",        
  end_date = "20250612")   

# Arai = CI10, CI15, CC06
clean_acoustic_data(
  base_dir = base_directory,
  site = "Arai",    
  device = "SM2967",  
  start_date = "20250608",        
  end_date = "20250612")   

clean_acoustic_data(
  base_dir = base_directory,
  site = "Arai",    
  device = "CI10",  
  start_date = "20250608",        
  end_date = "20250612")   

clean_acoustic_data(
  base_dir = base_directory,
  site = "Arai",    
  device = "CI15",  
  start_date = "20250608",        
  end_date = "20250612")   

clean_acoustic_data(
  base_dir = base_directory,
  site = "Arai",    
  device = "CC06",  
  start_date = "20250608",        
  end_date = "20250612")   

# Oda = CI09, CI19, CC07
clean_acoustic_data(
  base_dir = base_directory,
  site = "Oda",    
  device = "CI09",  
  start_date = "20250607",        
  end_date = "20250611") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "Oda",    
  device = "CI19",  
  start_date = "20250607",        
  end_date = "20250611") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "Oda",    
  device = "CC07",  
  start_date = "20250607",        
  end_date = "20250611") 

# Knaong Bat Sa = CI07, CI01, CC05
clean_acoustic_data(
  base_dir = base_directory,
  site = "KnaongBatSa",    
  device = "CI07",  
  start_date = "20250704",        
  end_date = "20250708") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "KnaongBatSa",    
  device = "CI01",  
  start_date = "20250704",        
  end_date = "20250708") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "KnaongBatSa",    
  device = "CC05",  
  start_date = "20250704",        
  end_date = "20250708") 

# Ta Say = CI19, CI09, CI15, SM3293
clean_acoustic_data(
  base_dir = base_directory,
  site = "TaSay",    
  device = "CI19",  
  start_date = "20250705",        
  end_date = "20250709") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaSay",    
  device = "CI09",  
  start_date = "20250705",        
  end_date = "20250709") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaSay",    
  device = "CI15",  
  start_date = "20250705",        
  end_date = "20250709") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "TaSay",    
  device = "SM3293",  
  start_date = "20250705",        
  end_date = "20250709") 

# Kronomh
clean_acoustic_data(
  base_dir = base_directory,
  site = "Kronomh",    
  device = "CI13",  
  start_date = "20250702",        
  end_date = "20250705") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "Kronomh",    
  device = "CI10",  
  start_date = "20250702",        
  end_date = "20250705") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "Kronomh",    
  device = "SM3352",  
  start_date = "20250702",        
  end_date = "20250705") 

# Tang Rang
clean_acoustic_data(
  base_dir = base_directory,
  site = "TangRang",    
  device = "CC01",  
  start_date = "20250702",        
  end_date = "20250705") 

clean_acoustic_data(
  base_dir = base_directory,
  site = "TangRang",    
  device = "CC02",  
  start_date = "20250606",        
  end_date = "20250610") 


