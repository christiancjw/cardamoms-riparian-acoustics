param (
    [string]$site,                  # e.g., "Arai"
    [string]$deviceID,              # e.g., "CI02"
    [string]$startDate,             # Start date (format: "20231122")
    [string]$endDate                # End date (format: "20231125")
)

# Base directories
$baseDrive = "/Volumes/CChing SSD/Acoustics/Pursat/2023-2024/_data/raw_output"
$baseOutputDirectory = "/Volumes/CChing SSD/Acoustics/Pursat/2023-2024/_data/concatenated_output"

# Paths for Analysis Programs and Config Files
$IndexConfig = "/Users/christianching/Documents/AnalysisPrograms/src/AnalysisConfigFiles/IndexPropertiesConfig.yml"
$SpectrogramConfig = "/Users/christianching/Documents/AnalysisPrograms/src/AnalysisConfigFiles/SpectrogramFalseColourConfig.yml"

# Generate date range
$dates = @()
$currentDate = [datetime]::ParseExact($startDate, "yyyyMMdd", $null)
$endDateParsed = [datetime]::ParseExact($endDate, "yyyyMMdd", $null)

while ($currentDate -le $endDateParsed) {
    $dates += $currentDate.ToString("yyyyMMdd")
    $currentDate = $currentDate.AddDays(1)
}

# Loop through each date
foreach ($date in $dates) {
    # Set paths for input and output
    $inputDirectory = "$baseDrive/$site/$deviceID/$date"
    
    # Create output directory dynamically based on site and device, without the date level
    $outputDirectory = "$baseOutputDirectory/$site/$deviceID"
    # Ensure the output directory exists
    if (-not (Test-Path $outputDirectory)) {
        New-Item -Path $outputDirectory -ItemType Directory -Force
    }

    # Construct and execute the command
    $command = "AP concatenateIndexfiles --input-data-directory `"$inputDirectory`" --output-directory `"$outputDirectory`" --directory-filter `*.wav` --file-stem-name `"$date`" --time-span-offset-hint '00:00:00' --index-properties-config `"$IndexConfig`" --false-colour-spectrogram-config `"$SpectrogramConfig`" --gap-rendering EchoGaps --concatenate-everything $null --draw-images $null"

    Write-Output "Executing command for $site, $deviceID on $date"

    try {
        Invoke-Expression $command
    } catch {
        Write-Output "Error processing concatenation for $site, $deviceID on $date. Skipping."
    }
}
