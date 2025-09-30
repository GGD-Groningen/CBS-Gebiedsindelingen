#####################################################################
# Complete adreslijst + pc6 obv CBS Gebiedsindeling
#####################################################################
#
# Nederlands
# ------------
# Gebruikershandleiding: 
# Download de gebiedsindelingen van de CBS-website: 
#   - Provincie-gemeente indeling: https://www.cbs.nl/nl-nl/onze-diensten/methoden/classificaties/overig/gemeentelijke-indelingen-per-jaar/indeling-per-jaar/gemeentelijke-indeling-op-1-januari-2020
#   - Buurt, wijk, gemeente: https://www.cbs.nl/nl-nl/maatwerk/2020/39/buurt-wijk-en-gemeente-2020-voor-postcode-huisnummer
# - Pak de bestanden uit en zet ze in dezelfde map als dit script.
# - Set de working directory van R (studio) naar de map waarin dit script zich bevindt.
# - Run het script
# - Je krijgt 2 bestanden terug van het script:
#     - pc_*jaartal*_pc6_only.csv: Alleen pc6
#     - pc_*jaartal*_pc6_huisnummer.csv: pc6 en huisnummer
# - De gekoppelde databestanden bevatten: pc6 (+evt huisnummer), buurt, buurtnaam, wijk, wijknaam, gemeente, gemeentenaam, provincie, provincienaam.
#
# English
# ------------
# User Manual: 
# Download the area classifications from the CBS website: 
#     - Province-municipality classification: https://www.cbs.nl/nl-nl/onze-diensten/methoden/classificaties/overig/gemeentelijke-indelingen-per-jaar/indeling-per-jaar/gemeentelijke-indeling-op-1-januari-2020
#     - Neighborhood, district, municipality: https://www.cbs.nl/nl-nl/maatwerk/2020/39/buurt-wijk-en-gemeente-2020-voor-postcode-huisnummer
# - Extract the files and place them in the same folder as this script.
# - Set de working directory van R (studio) naar de map waarin dit script zich bevindt.
# - Run the script
# - You will get 2 files back from the script:
#     - pc_*jaartal*_pc6_only.csv: Only pc6
#     - pc_*jaartal*_pc6_huisnummer.csv: pc6 and house number
# - The linked data files contain: pc6 (+optionally house number), neighborhood, neighborhood name, district, district name, municipality, municipality name, province, province name.
## 
#####################################################################

# Function to install and load packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

# Load required libraries (install if not present)
install_and_load("dplyr")
install_and_load("readr")
install_and_load("stringr")
install_and_load("readxl")
install_and_load("testthat")

# Input variables
year <- "2020"
huisnummer <- "huisnummer"
postcode <- "pc6"

# Read the buurt data
buurt_data <- read_delim(paste0("brt", year, ".csv"), 
                        delim = ";", 
                        locale = locale(encoding = "UTF-8"))

# Read the wijk data
wijk_data <- read_delim(paste0("wijk", year, ".csv"), 
                       delim = ";", 
                       locale = locale(encoding = "UTF-8"))

# Read the gemeente data
gemeente_data <- read_delim(paste0("gem", year, ".csv"), 
                           delim = ";", 
                           locale = locale(encoding = "UTF-8"))

# Read the postal code data with correct delimiter and encoding
pc_data <- read_delim(paste0("pc6hnr20200801_gwb.csv"), 
                     delim = ";", 
                     locale = locale(encoding = "UTF-8"))

# Read provincie data (Excel version)
provincie_data <- read_excel(paste0("Gemeenten alfabetisch 2020.xlsx"), sheet = "Gemeenten_alfabetisch_2020")

#---------------------------------------------------------
# Data Cleaning

# Make all column names lowercase
names(pc_data) <- tolower(names(pc_data))
names(buurt_data) <- tolower(names(buurt_data))
names(wijk_data) <- tolower(names(wijk_data))
names(gemeente_data) <- tolower(names(gemeente_data))
names(provincie_data) <- tolower(names(provincie_data))

# Drop columns gemeentecodeGM, gemeentenaam
provincie_data <- provincie_data %>% 
  select(-gemeentenaam)

# Drop columns gm_2022, gm2022, gmnaam, wk_2022, wk_naam from buurt_data
buurt_data <- buurt_data %>% 
  select(-gm2020, -gm_2020, -gm_naam, -wk_2020, -wk2020, -wk_naam)

# Rename columns to include year
provincie_data <- provincie_data %>% 
  rename(!!paste0("gemeente", year) := gemeentecode,
         !!paste0("gemeentecodegm", year) := gemeentecodegm,
         !!paste0("provincie", year) := provinciecode,
         !!paste0("provinciecodepv", year) := provinciecodepv,
         !!paste0("provincienaam", year) := provincienaam)

# Rename columns where they mismatch
buurt_data <- buurt_data %>% 
  rename(!!paste0("buurt", year) := buurtcode2020)

wijk_data <- wijk_data %>% 
  rename(!!paste0("wijk", year) := wijkcode2020)

gemeente_data <- gemeente_data %>% 
  rename(!!paste0("gemeente", year) := gemcode2020)

#---------------------------------------------------------
# Merge the data

# Merge buurt data into postal code data
pc_data_export <- pc_data %>% 
  left_join(buurt_data, by = paste0("buurt", year))

# Merge wijk data into postal code data
pc_data_export <- pc_data_export %>% 
  left_join(wijk_data, by = paste0("wijk", year))

# Convert gemeente columns to character to ensure compatible types for joining
pc_data_export <- pc_data_export %>% 
  mutate(!!paste0("gemeente", year) := as.numeric(.data[[paste0("gemeente", year)]]))

gemeente_data <- gemeente_data %>% 
  mutate(!!paste0("gemeente", year) := as.numeric(.data[[paste0("gemeente", year)]]))

provincie_data <- provincie_data %>% 
  mutate(!!paste0("gemeente", year) := as.numeric(.data[[paste0("gemeente", year)]]))

# Merge gemeente data into postal code data
pc_data_export <- pc_data_export %>% 
  left_join(gemeente_data, by = paste0("gemeente", year))

# Merge provincie data into postal code data
pc_data_export <- pc_data_export %>% 
  left_join(provincie_data, by = paste0("gemeente", year))  
  

#---------------------------------------------------------
# Add full CBS Coding system

pc_data_export <- pc_data_export %>%
  mutate(
    !!paste0("buurtcodebu", year) := paste0("BU", .data[[paste0("buurt", year)]]),
    !!paste0("wijkcodewk", year) := paste0("WK", .data[[paste0("wijk", year)]])
  )

#---------------------------------------------------------
# Reduce a second dataset to pc6 only

# Remove huisnummer column
pc_data_export_pc6 <- pc_data_export %>% 
  select(-!!huisnummer)

# Remove lines with duplicate pc6
pc_data_export_pc6 <- pc_data_export_pc6 %>% 
  distinct(!!sym(postcode), .keep_all = TRUE)

#---------------------------------------------------------
# Data validation

# Test that postcode column in pc_data matches pc_data_export
test_that("postcode column in pc_data matches pc_data_export", {
  # Get the postcode column from both datasets
  pc_data_postcodes <- pc_data[[postcode]]
  pc_data_export_postcodes <- pc_data_export[[postcode]]
  
  # Check if the postcode columns are identical
  expect_equal(pc_data_postcodes, pc_data_export_postcodes, 
               info = "Postcode columns should be identical between pc_data and pc_data_export")
  
  # Additional check: verify both datasets have the same number of rows
  expect_equal(nrow(pc_data), nrow(pc_data_export),
               info = "Both datasets should have the same number of rows")
  
  # Check that postcode column exists in both datasets
  expect_true(postcode %in% names(pc_data),
              info = paste("Postcode column", postcode, "should exist in pc_data"))
  expect_true(postcode %in% names(pc_data_export),
              info = paste("Postcode column", postcode, "should exist in pc_data_export"))
})

# Test that every row in pc_data_export is present in pc_data
test_that("every row in pc_data_export is present in pc_data", {
  expect_equal(nrow(pc_data_export), nrow(pc_data),
               info = "Every row in pc_data_export should be present in pc_data")
})

# Test that every row in pc_data_export is fully filled
test_that("every row in pc_data_export is fully filled", {
  # Check for NA values (resulting from failed left joins)
  rows_with_na <- pc_data_export %>% 
    filter_all(any_vars(is.na(.)))
  
  # Check for empty strings
  rows_with_empty <- pc_data_export %>% 
    filter_all(any_vars(. == ""))
  
  # Report specific issues
  if (nrow(rows_with_na) > 0) {
    cat("Found", nrow(rows_with_na), "rows with NA values (likely from failed left joins)\n")
    print(head(rows_with_na, 3))
  }
  
  if (nrow(rows_with_empty) > 0) {
    cat("Found", nrow(rows_with_empty), "rows with empty strings\n")
    print(head(rows_with_empty, 3))
  }
  
  # Test should fail if there are any missing values
  expect_equal(nrow(rows_with_na), 0,
               info = paste("Found", nrow(rows_with_na), "rows with NA values in pc_data_export"))
  
  expect_equal(nrow(rows_with_empty), 0,
               info = paste("Found", nrow(rows_with_empty), "rows with empty strings in pc_data_export"))
})

# Test that every row in pc_data_export_pc6 is fully filled
test_that("every row in pc_data_export_pc6 is fully filled", {
  # Check for NA values (resulting from failed left joins)
  rows_with_na <- pc_data_export_pc6 %>% 
    filter_all(any_vars(is.na(.)))
  
  # Check for empty strings
  rows_with_empty <- pc_data_export_pc6 %>% 
    filter_all(any_vars(. == ""))
  
  # Report specific issues
  if (nrow(rows_with_na) > 0) {
    cat("Found", nrow(rows_with_na), "rows with NA values in pc_data_export_pc6 (likely from failed left joins)\n")
    print(head(rows_with_na, 3))
  }
  
  if (nrow(rows_with_empty) > 0) {
    cat("Found", nrow(rows_with_empty), "rows with empty strings in pc_data_export_pc6\n")
    print(head(rows_with_empty, 3))
  }
  
  # Test should fail if there are any missing values
  expect_equal(nrow(rows_with_na), 0,
               info = paste("Found", nrow(rows_with_na), "rows with NA values in pc_data_export_pc6"))
  
  expect_equal(nrow(rows_with_empty), 0,
               info = paste("Found", nrow(rows_with_empty), "rows with empty strings in pc_data_export_pc6"))
})

#---------------------------------------------------------
# Writing the files

# Write the data to a csv file
tryCatch({
  output_file <- paste0("pc_", year, "_", postcode, "_", huisnummer, ".csv")
  write_delim(pc_data_export, output_file, delim = ";")
  cat("✅ Successfully exported data to", output_file, "\n")
  cat("   - Total rows exported:", format(nrow(pc_data_export), big.mark = ","), "\n")
  cat("   - Total columns exported:", ncol(pc_data_export), "\n")
  cat("   - File size:", format(object.size(pc_data_export) / 1024 / 1024, digits = 2), "MB\n")
}, error = function(e) {
  cat("❌ ERROR: Failed to export data to CSV\n")
  cat("   Error:", e$message, "\n")
  quit(status = 1)
})

tryCatch({
  output_file <- paste0("pc_", year, "_pc6_only.csv")
  write_delim(pc_data_export_pc6, output_file, delim = ";")
  cat("✅ Successfully exported data to", output_file, "\n")
  cat("   - Total rows exported:", format(nrow(pc_data_export_pc6), big.mark = ","), "\n")
  cat("   - Total columns exported:", ncol(pc_data_export_pc6), "\n")
  cat("   - File size:", format(object.size(pc_data_export_pc6) / 1024 / 1024, digits = 2), "MB\n")
}, error = function(e) {
  cat("❌ ERROR: Failed to export data to CSV\n")
  cat("   Error:", e$message, "\n")
  quit(status = 1)
})