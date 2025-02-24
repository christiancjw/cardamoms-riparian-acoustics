# Accept inputs for site, device, and date range
param (
    [string]$site,                  # e.g., "TaSay"
    [string]$deviceID,              # e.g., "CI12"
    [string]$startDate,             # Start date (format: "20240622")
    [string]$endDate                # End date (format: "20240623")
)

# Base directories
$baseDrive = "/Volumes/CChing SSD/Acoustics/Pursat/2023-2024"
$baseOutputDirectory = "$baseDrive/_data/raw_output"

# Paths for Analysis Programs and Config Files
$AnalysisProgramsDirectory = "/Users/christianching/Documents/AnalysisPrograms"
$ConfigDirectory = "/Users/christianching/Documents/AnalysisPrograms/src/AnalysisConfigFiles"
$ConfigFile = "$ConfigDirectory/Towsey.Acoustic.yml"

# Generate date range
$dates = @()
$currentDate = [datetime]::ParseExact($startDate, "yyyyMMdd", $null)
$endDateParsed = [datetime]::ParseExact($endDate, "yyyyMMdd", $null)

while ($currentDate -le $endDateParsed) {
    $dates += $currentDate.ToString("yyyyMMdd")
    $currentDate = $currentDate.AddDays(1)
}

# Loop through each date in the specified date range
foreach ($date in $dates) {
    # Set data directory for each date
    $dataDirectory = "$baseDrive/$site/$deviceID/$date"
    $dateOutputDirectory = "$baseOutputDirectory/$site/$deviceID/$date"
    
    # Create the date-specific output directory if it doesn't exist
    if (!(Test-Path -Path $dateOutputDirectory)) {
        mkdir $dateOutputDirectory -Force
    }

    # Retrieve list of audio files for the current date
    $files = Get-ChildItem "$dataDirectory\*" -Include "*.wav"
    $fileCount = $files.Count
    
    # Process each file
    $counter = 0
    foreach ($file in $files) { 
        $counter++
        $fileName = $file.Name
        $fileOutputDirectory = "$dateOutputDirectory/$fileName"  # Unique folder for each file

        Write-Output ("####### $counter/$fileCount. Processing: $fileName for $date")
        
        # Create output directory for each file if it doesn't exist
        if (!(Test-Path -Path $fileOutputDirectory)) {
            mkdir $fileOutputDirectory -Force
        }

        # Run AP command and store output in the file-specific folder
        $command = "AP audio2csv `"$file`" `"$ConfigFile`" `"$fileOutputDirectory`" -n --quiet"
        try {
            Invoke-Expression $command
        } catch {
            Write-Output "Error processing $fileName on $date. Skipping file."
        }
    }
}
