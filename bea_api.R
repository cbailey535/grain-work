# Load library
library(bea.R)
library(tidyverse)

# Set your BEA API key
beaKey <- "483CE1EB-77CE-40A5-8C51-ECFD66654996"

# Define parameters for GDP by MSA
params <- list(
  UserID = beaKey,
  Method = "GetData",
  DataSetName = "Regional",
  TableName = "CAGDP2", # GDP by MSA
  LineCode = 1,        # Total GDP
  GeoFIPS = "19740",     # All MSAs
  Year = "ALL"         # All available years
)

colorado_msa_codes <- c("14500", "19740", "17820", "22660", "24540") # Boulder, Denver, Colorado Springs, Fort Collins, Greeley
all_colorado_gdp <- lapply(colorado_msa_codes, function(geo) {
  params <- list(
    UserID = beaKey,
    Method = "GetData",
    DataSetName = "Regional",
    TableName = "CAGDP2",
    LineCode = 1,
    GeoFIPS = geo,
    Year = "ALL"
  )
  beaGet(params, asWide = FALSE)
})

colorado_gdp_data <- bind_rows(all_colorado_gdp) %>% janitor::clean_names() # Combine results into one data frame

# Fetch data
# gdp_msa_data <- beaGet(params, asWide = FALSE) %>% janitor::clean_names()

# Filter for Colorado MSAs
#colorado_msa_codes <- c("19740", "17820", "14500", "22660", "24540")
#colorado_gdp_data <- gdp_msa_data[gdp_msa_data$GeoFIPS %in% colorado_msa_codes, ]

# Clean data
colorado_gdp <- colorado_gdp_data %>%
  mutate(
    data_value = as.numeric(data_value),  # Ensure GDP values are numeric
    time_period = as.numeric(time_period) # Ensure years are numeric
  ) %>%
  arrange(geo_fips, time_period)          # Sort by MSA and Year

# Calculate year-over-year growth
colorado_gdp_growth <- colorado_gdp %>%
  group_by(geo_fips, geo_name) %>%
  mutate(
    growth_pct = (data_value - lag(data_value)) / lag(data_value) * 100
  ) %>%
  ungroup()

colorado_gdp_cumulative <- colorado_gdp %>%
  group_by(geo_fips, geo_name) %>%
  mutate(
    first_yr_gdp = first(data_value),                 # First year's GDP
    cumulative_growth = (data_value / first_yr_gdp) - 1 # Cumulative growth
  ) %>%
  ungroup()

# View the first few rows
head(colorado_gdp_growth)
print(colorado_gdp_growth, n=100)

# Filter to show only growth percentages (e.g., for Denver)
#denver_growth <- colorado_gdp_growth %>% filter(geo_fips == "19740")
#print(denver_growth, n=100)

# Plot year-over-year growth for all MSAs in Colorado
ggplot(colorado_gdp_cumulative, aes(x = time_period, y = cumulative_growth, color = geo_name)) +
  geom_line() +
  labs(
    title = "Year-over-Year GDP Growth by MSA (Colorado)",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  theme_minimal()
