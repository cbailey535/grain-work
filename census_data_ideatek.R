options(tigris_use_cache = TRUE)
census_api_key('d2f0fe8f1e5a7984a67bbff53c703bc9c6ea9da9', install = TRUE, overwrite = TRUE)

# Path to the saved data file within your app's directory
census_data_file <- "census_data.rds"

# Check if data file exists
if (file.exists(census_data_file)) {
  # Load cached data if it exists
  geo_blocks <- readRDS(census_data_file)
} else {
  # Fetch the data from the API
  geo_blocks <- get_decennial(
    geography = "block",
    variables = "P1_001N",
    year = 2020,
    summary_var = "P1_001N",
    state = c("KS","OK","MO"),
    geometry = TRUE
  ) %>%
    mutate(GEOID = as.character(GEOID))
  
  # Save the data locally
  saveRDS(geo_blocks, census_data_file)
}
