# Load necessary libraries
library(tidyverse)
library(tidycensus)
library(sf)

# Set options for caching and API key
options(tigris_use_cache = TRUE)
census_api_key('d2f0fe8f1e5a7984a67bbff53c703bc9c6ea9da9', install = TRUE, overwrite = TRUE)

# Define ACS variables for the desired demographics
acs_vars <- c(
  median_age = "B01002_001",          # Median Age
  median_income = "B19013_001",       # Median Household Income
  median_home_value = "B25077_001",   # Median Home Value
  total_housing_units = "B25001_001", # Total Housing Units
  occupied_housing_units = "B25002_002", # Total Occupied Housing Units
  two_units = "B25024_003",           # Housing units with 2 units
  three_or_four_units = "B25024_004", # Housing units with 3 or 4 units
  five_to_nine_units = "B25024_005",  # Housing units with 5 to 9 units
  ten_to_nineteen_units = "B25024_006", # Housing units with 10 to 19 units
  twenty_or_more_units = "B25024_007"  # Housing units with 20 or more units
)

# Fetch data for Census Block Groups in Florida
census_data_file <- "florida_demographics_cbg_with_multifamily.rds" # Cache file

if (file.exists(census_data_file)) {
  # Load cached data
  florida_data <- readRDS(census_data_file)
} else {
  # Retrieve ACS data
  florida_data <- get_acs(
    geography = "block group",
    state = "FL",
    variables = acs_vars,
    year = 2021,  # Most recent ACS data
    survey = "acs5",
    geometry = TRUE # Include geometries for land area calculation
  )
  
  # Save the data locally
  saveRDS(florida_data, census_data_file)
}

# Calculate Total Land Area in Square Miles and Multi-Family Units
florida_data <- florida_data %>%
  mutate(
    land_area_sq_miles = st_area(geometry) / 2.59e+6 # Convert square meters to square miles
  ) %>%
  st_drop_geometry() # Drop geometry if not needed

# Reshape data to wide format
florida_data_wide <- florida_data %>%
  select(GEOID, variable, estimate, land_area_sq_miles) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    # Calculate total multi-family units
    multi_family_units = `two_units` + `three_or_four_units` + `five_to_nine_units` + `ten_to_nineteen_units` + `twenty_or_more_units`
  ) 

lane_miles <- read_csv('lane_miles.csv') %>% janitor::clean_names() %>% mutate(GEOID = as.character(geoid))
florida_cdp <- read.csv('competition_cbg_florida_state_j24.csv') %>% janitor::clean_names() %>% mutate(GEOID = as.character(census_block_grp))

florida_data_joined <- florida_data_wide %>%
  left_join(lane_miles, by = "GEOID") %>% 
  left_join(florida_cdp, by = "GEOID") %>%
  select(
    GEOID,
    usps_zip_pref_state,
    county_name,
    usps_zip_pref_city,
    total_residential_location_ids,
    providers,
    land_area_sq_miles,
    median_age,
    median_income,
    median_home_value,
    total_housing_units,
    occupied_housing_units,
    multi_family_units,
    lane_miles
  ) %>%
  mutate(
    land_area_sq_miles = as.numeric(gsub("\\[m\\^2\\]", "", land_area_sq_miles)),
    hu_per_lane_mile = if_else(
      total_housing_units > 1 & lane_miles >= 1, 
      total_housing_units / lane_miles, 
      total_housing_units),
    mdu_pct = if_else(
      total_housing_units > 0,  # Avoid division by zero
      multi_family_units / total_housing_units,
      0  # Assign 0 or NA if housing_units is 0
    )
  )

head(florida_data_joined)
View(florida_data_joined)
# Save or inspect the data
write.csv(florida_data_joined, "florida_demographics_by_cbg.csv", row.names = FALSE)
