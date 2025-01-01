################################################################################
# HOA SALES LIST PROCESSING
# Date:  December 2024
# Developer:  Chris Bailey, Grain Management (cbailey@graingp.com)
################################################################################

# Load packages
# install.packages(c(
#   "tidyverse", 
#   "sf", 
#   "memoise", 
#   "janitor", 
#   "tidygeocoder",
#   "leaflet",
#   "corrplot",
#   "RColorBrewer",
#   "factoextra",
#   "ggrepel"
#   ))

library(tidyverse)
library(sf)
library(memoise)

################################################################################
# STEP 1:  LOAD & TRANSFORM SOURCE DATA
################################################################################

fl_hoa_src <- read.csv('HOA_BOARD_LIST_FLORIDA_DEC24_COPY.csv') %>%
  janitor::clean_names() %>% 
  mutate(src = "HOABL") %>%
  select(src, everything()) %>%
  mutate(
    board_id = as.character(board_id),
    last_name = toupper(last_name),
    first_name = toupper(first_name),
    middle_init = toupper(middle_init),
    suffix = toupper(suffix),
    title = toupper(title),
    extra = toupper(extra),
    route = toupper(route),
    address = toupper(address),
    city = toupper(city),
    state = toupper(state),
    zip = as.character(zip),
    zip4 = as.character(zip4),
    postal = as.character(postal),
    country = toupper(country),
    usps_verified = toupper(usps_verified),
    mail_match = toupper(mail_match),
    new_director = toupper(new_director),
    homestead = toupper(homestead),
    entity_filing_type = toupper(entity_filing_type),
    entity_name = toupper(entity_name),
    entity_mail_address_1 = toupper(entity_mail_address_1),
    entity_mail_address_2 = toupper(entity_mail_address_2),
    entity_mail_address_3 = toupper(entity_mail_address_3),
    entity_mail_city = toupper(entity_mail_city),
    entity_mail_state = toupper(entity_mail_state),
    entity_mail_zip = as.character(entity_mail_zip),
    entity_mail_zip4 = as.character(entity_mail_zip4),
    entity_mail_usps_verified = toupper(entity_mail_usps_verified),
    last_transaction_description = toupper(last_transaction_description),
    master_association = toupper(master_association),
    property_type = toupper(property_type),
    conversion = toupper(conversion),
    property_name = toupper(property_name),
    property_county = toupper(property_county),
    property_address = toupper(property_address),
    property_city = toupper(property_city),
    property_state = toupper(property_state),
    property_zip = as.character(property_zip),
    property_addr_verified = toupper(property_addr_verified),
    property_metro_area = toupper(property_metro_area),
    fips_code = format(as.numeric(fips_code), scientific = FALSE),
    developer_board = toupper(developer_board),
    agent_type = toupper(agent_type),
    agent_first_name = toupper(agent_first_name),
    agent_last_name = toupper(agent_last_name),
    agent_addr_1 = toupper(agent_addr_1),
    agent_addr_2 = toupper(agent_addr_2),
    agent_addr_3 = toupper(agent_addr_3),
    agent_city = toupper(agent_city),
    agent_state = toupper(agent_state),
    agent_zip5 = toupper(agent_zip5),
    agent_zip4 = toupper(agent_zip4),
    agent_usps_verified = toupper(agent_usps_verified),
    phone_found = toupper(phone_found),
    phone_1 = as.character(phone_1),
    phone_1_type = toupper(phone_1_type),
    phone_2 = as.character(phone_2),
    phone_2_type = toupper(phone_2_type),
    phone3 = as.character(phone3),
    phone_3_type = toupper(phone_3_type),
    email_found = toupper(email_found),
    cap_first = toupper(cap_first),
    cap_last = toupper(cap_last),
    president = if_else(president=="President",1,0),
    vice_president = if_else(vice_president=="Vice President",1,0),
    treasurer = if_else(treasurer=="Treasurer",1,0),
    secretary = if_else(secretary=="Secretary",1,0),
    director = if_else(director=="Director",1,0),
    primary_title = toupper(primary_title),
    entity_id = trimws(entity_id)
  ) %>%
  mutate(housing_unit_bin = 
           ifelse(total_units==0,"0 Housing Units",
            ifelse(total_units>=50 & total_units<=75,"50 to 75 Housing Units",
              ifelse(total_units>75 & total_units<=117,"76 to 117 Housing Units",
                ifelse(total_units>75 & total_units<=117,"76 to 117 Housing Units",
                  ifelse(total_units>117 & total_units<=215,"118 to 215 Housing Units",
                         "216+ Housing Units")))))
  ) %>%
  mutate(load_dt = as.Date("12-16-2024", format = "%m-%d-%Y"))

competition_by_cbg <- read.csv('NAMED_COMPETITORS_BY_CBG_20241115.csv') %>%
  janitor::clean_names() %>%
  filter(provider_id == 130235 | provider_id == 130317) %>%
  mutate(GEOID = as.character(census_block_grp),
         spectrum = ifelse(provider_id == 130235,1,0),
         comcast = ifelse(provider_id == 130317,1,0)) %>%
  select(GEOID, spectrum, comcast) %>%
  group_by(GEOID) %>%
  summarize(
    spectrum = max(spectrum),
    comcast = max(comcast),
    .groups = 'drop' # Ensures the result is ungrouped after summarization
  )
  
# Read the KML file once and simplify it for performance
kml_file <- sf::read_sf('summit_network_jul24.kml') 

# Simplify the geometry for performance
kml_file <- st_simplify(kml_file, dTolerance = 0.01)

# Ensure CRS is consistent between your data and KML file
st_crs(kml_file) <- 4326  

################################################################################
# STEP 2:  FORWARD-GEOCODE THE PROPERTY ADDRESS TO OBTAIN THE GEOID
################################################################################
   
 # Create the data frame to be fed into the geocoder
 some_addresses <- fl_hoa_src %>%
   filter(addr_correction==0) %>%  # remove addresses from the src data that were manually corrected
   select(
     entity_id,
     property_address,
     property_city,
     property_state,
     property_zip
   ) %>%
   mutate(
     addr = paste(property_address, property_city, property_state, property_zip, sep = ", ")
   ) %>%
   select(entity_id,addr)
 
 library(tidygeocoder)
 
 # Define a function to process batches of data
 geocode_batch <- function(batch) {
   lat_longs <- batch %>%
     geocode(addr, 
             method = "census", 
             full_results = TRUE, 
             api_options = list(census_return_type = 'geographies')
     )
   return(lat_longs)
 }
 
 # Split the data into chunks of 10,000 rows each
 batch_size <- 10000
 batches <- split(some_addresses, ceiling(seq_along(some_addresses$addr) / batch_size))
 
 # Initialize an empty list to store the results
 all_results <- list()
   
 # Loop over each batch and geocode it
 for (i in 1:length(batches)) {
   cat("Processing batch", i, "of", length(batches), "\n")
   
   # Process the current batch
   batch_result <- geocode_batch(batches[[i]])
   
   # Store the result in the list
   all_results[[i]] <- batch_result
 }
 
 # Combine all the batch results into one dataframe
 final_results <- bind_rows(all_results) %>%
   mutate(
     GEOID = paste0(
       ifelse(is.na(state_fips), "", state_fips),  # Handle NA in col1
       ifelse(is.na(county_fips), "", county_fips),  # Handle NA in col2
       ifelse(is.na(census_tract), "", census_tract),  # Handle NA in col3
       ifelse(is.na(census_block), "", substr(census_block, 1, 1))  # Handle NA in col4 and extract first digit
     )
   ) %>%
   select(
     entity_id,
     GEOID,
     addr
     )
   
 # Save the result to a CSV
 write.csv(final_results, "geocoded_results.csv", row.names = FALSE)

################################################################################
# STEP 3:  APPEND THE DATA WITH COMPETITION & DEMOGRAPHICS ATTRIBUTES
################################################################################

# Load the demographics file that I created from the census_demos_cbg.R code
florida_demographics <- read.csv("florida_demographics_by_cbg.csv") %>% 
  mutate(GEOID = trimws(as.character(GEOID)))

geocoded_results <- read.csv("geocoded_results.csv") %>% 
  mutate(GEOID = as.character(GEOID), entity_id = trimws(entity_id))

# Perform the left joins
fl_hoa_src_join_1 <- fl_hoa_src %>%
  left_join(geocoded_results, by = "entity_id") %>%
  mutate(GEOID = if_else(is.na(GEOID) | nchar(GEOID) == 11, fips_code, GEOID)) %>%
  mutate(GEOID = trimws(as.character(GEOID)))

fl_hoa_src_join_2 <- fl_hoa_src_join_1 %>%
  left_join(florida_demographics, by = "GEOID") %>%
  mutate(fcc_match = if_else(!is.na(total_residential_location_ids), 1, 0))

fl_hoa_src_join_3 <- fl_hoa_src_join_2 %>%
  left_join(competition_by_cbg, by = "GEOID") %>%
  mutate(
    spectrum = replace_na(spectrum, 0),
    comcast = replace_na(comcast, 0)
  )

################################################################################
# STEP 4:  COMPUTE NETWORK PROXIMITY
################################################################################

# Remove any records with null lat/lon  
fl_hoa_src_sf_tranform <- fl_hoa_src_join_3 %>% 
  select(entity_id,latitude,longitude) %>% 
  filter(!is.na(latitude))

# Convert data frame into an object class sf
fl_hoa_src_sf <- st_as_sf(fl_hoa_src_sf_tranform, 
                          coords = c("longitude", "latitude"), 
                          crs = 4326)

# Memoized distance calculation for performance
memoized_distance <- memoise(function(df1, df2) {
  nearest <- st_nearest_feature(df1, df2)
  dist <- st_distance(df1, df2[nearest,], by_element = TRUE)
  list(nearest = nearest, dist = dist)
})

distance_data <- memoized_distance(fl_hoa_src_sf, kml_file)
nearest <- distance_data$nearest
dist <- distance_data$dist/1609.34  # Convert meters to miles

# Prepare the joined data with the KML file
pljoin <- cbind(fl_hoa_src_sf, st_drop_geometry(kml_file)[nearest,])
pljoin$dist <- as.numeric(dist)
  
# Create the processed data frame
processed_df <- pljoin %>%
  mutate(
    dist_bin = case_when(
      dist < 1 ~ "On-net (<1 Mile)",
      dist >= 1 & dist < 2 ~ "Near-net (1 to 2 Miles)",
      dist >= 2 & dist < 5 ~ "Off-net (2 to 5 Miles)",
      TRUE ~ "Far Off-net (5+ Miles)"
    ),
    enity_id = trimws(as.character(entity_id))
  ) %>%
  select(entity_id,dist,dist_bin) %>%
  st_drop_geometry() 

# Final output
final_analytic_df <- fl_hoa_src_join_3 %>%
  left_join(processed_df, by = "entity_id") %>%
  mutate(
    dist = as.numeric(dist)
  )

print(final_analytic_df)

################################################################################
# STEP 5:  VISUALLY INSPECT LAT/LON COORDINATES ON A MAP
################################################################################

library(leaflet)

leaflet()  %>% 
  addTiles(group = "OSM") %>%  # OpenStreetMap
  addCircleMarkers(
    data = final_analytic_df,
    lat = ~latitude,
    lng = ~longitude,
    radius = 3,  # Adjust the radius of the circles
    color = "blue",  # Specify the color of the circles
    stroke = FALSE,  # Remove the circle border
    fillOpacity = 0.8  # Set the fill opacity
  )

################################################################################
# STEP 6:  APPEND PAYBACK TO final_analytic_df
################################################################################
# Input values
bulk_arpu <- 35.00
take_percent <- 0.15
margin <- 0.65
cost_per_unit <- 2000.00
cost_per_foot <- 17.00
type_ii_cost_per_month <- 2000.00
#num_units <- 250
distance_adj <- 1.2
#network_prox <- 2500
bulk_arpu_growth <- 1.04
take_rev_growth <-  0.90
type_ii_cost_growth <- 1.03

# Offnet Payback Function
offnet_payback <- function(bulk_arpu, take_percent, margin, cost_per_unit, type_ii_cost_per_month, num_units, years = 10) {
  # Initialize variables for Year 1
  initial_bulk_arpu <- bulk_arpu
  bulk_revenue <- bulk_arpu * 12 * num_units
  take_revenue <- bulk_revenue * take_percent
  annual_revenue <- bulk_revenue + take_revenue
  
  direct_cost <- annual_revenue * -(1 - margin)
  type_ii_cost <- type_ii_cost_per_month * 12
  capex <- cost_per_unit * num_units
  
  # Initialize cash flow and cumulative cash flow
  cash_flows <- numeric(years + 1)
  cumulative_cf <- numeric(years + 1)
  
  # Period 0: Initial Capex (do not include in payback period)
  cash_flows[1] <- -capex
  cumulative_cf[1] <- cash_flows[1]
  
  # Loop through each year to calculate cash flow, cumulative cash flow, and payback period
  for (i in 2:(years + 1)) {
    # Apply annual growth rates starting from Year 2
    if (i > 2) {
      bulk_arpu <- bulk_arpu * bulk_arpu_growth  # Increase bulk ARPU by 4% annually
      bulk_revenue <- bulk_arpu * 12 * num_units  # Recalculate bulk revenue using updated bulk_arpu
      take_revenue <- take_revenue * take_rev_growth  # Decrease take revenue by 10% annually
      type_ii_cost <- type_ii_cost * type_ii_cost_growth  # Increase type II cost by 3% annually
    }
    
    # Recalculate annual revenue and direct cost for the current year
    annual_revenue <- bulk_revenue + take_revenue
    direct_cost <- annual_revenue * -(1 - margin)
    
    # Contribution margin calculation
    contribution_margin <- annual_revenue - abs(direct_cost) - type_ii_cost
    
    # Calculate cash flows and update cumulative cash flow
    cash_flows[i] <- contribution_margin
    cumulative_cf[i] <- cumulative_cf[i - 1] + cash_flows[i]
  }
  
  # Determine the first year where cumulative cash flow becomes positive (excluding Year 0)
  first_positive_year <- which(cumulative_cf >= 0)[1] - 1
  
  if (is.na(first_positive_year)) { # Handle case where payback is not achieved
    return(NA)
  }
  
  # Calculate the fractional year based on remaining cumulative CF in the first positive year
  remaining <- abs(cumulative_cf[first_positive_year])  # Remaining cumulative CF before full payback
  next_year_cf <- cash_flows[first_positive_year + 1]  # Cash flow in the next period
  fractional_year <- remaining / next_year_cf  # Calculate the fractional year
  
  # Final payback period calculation, excluding Year 0
  payback_period <- (first_positive_year + fractional_year) - 1
  
  # Return result
  return(payback_period)
}

# On-net Payback Function
onnet_payback <- function(bulk_arpu, take_percent, margin, cost_per_unit, cost_per_foot, num_units, distance_adj, network_prox, years = 10) {
  # Initialize variables for Year 1
  initial_bulk_arpu <- bulk_arpu
  bulk_revenue <- bulk_arpu * 12 * num_units
  take_revenue <- bulk_revenue * take_percent
  annual_revenue <- bulk_revenue + take_revenue
  
  direct_cost <- annual_revenue * -(1 - margin)
  non_transport <- num_units * cost_per_unit
  transport <- network_prox * cost_per_foot * distance_adj
  capex <- non_transport + transport
  
  # Initialize cash flow and cumulative cash flow
  cash_flows <- numeric(years + 1)
  cumulative_cf <- numeric(years + 1)
  
  # Period 0: Initial Capex (do not include in payback period)
  cash_flows[1] <- -capex
  cumulative_cf[1] <- cash_flows[1]
  
  # Loop through each year to calculate cash flow, cumulative cash flow, and payback period
  for (i in 2:(years + 1)) {
    # Apply annual growth rates starting from Year 2
    if (i > 2) {
      bulk_arpu <- bulk_arpu * bulk_arpu_growth  # Increase bulk ARPU by 4% annually
      bulk_revenue <- bulk_arpu * 12 * num_units  # Recalculate bulk revenue using updated bulk_arpu
      take_revenue <- take_revenue * take_rev_growth  # Decrease take revenue by 10% annually
    }
    
    # Recalculate annual revenue and direct cost for the current year
    annual_revenue <- bulk_revenue + take_revenue
    direct_cost <- annual_revenue * -(1 - margin)
    
    # Contribution margin calculation
    contribution_margin <- annual_revenue - abs(direct_cost)
    
    # Calculate cash flows and update cumulative cash flow
    cash_flows[i] <- contribution_margin
    cumulative_cf[i] <- cumulative_cf[i - 1] + cash_flows[i]
  }
  
  # Determine the first year where cumulative cash flow becomes positive (excluding Year 0)
  first_positive_year <- which(cumulative_cf >= 0)[1] - 1
  
  if (is.na(first_positive_year)) { # Handle case where payback is not achieved
    return(NA)
  }
  
  # Calculate the fractional year based on remaining cumulative CF in the first positive year
  remaining <- abs(cumulative_cf[first_positive_year])  # Remaining cumulative CF before full payback
  next_year_cf <- cash_flows[first_positive_year + 1]  # Cash flow in the next period
  fractional_year <- remaining / next_year_cf  # Calculate the fractional year
  
  # Final payback period calculation, excluding Year 0
  payback_period <- (first_positive_year + fractional_year) - 1
  
  # Return result
  return(payback_period)
}

# Function Calls
final_analytic_pb <- final_analytic_df %>%
  rowwise() %>%
  mutate(
    offnet_pb = ifelse(
      !is.na(total_units) & total_units > 0,
      {
        payback <- offnet_payback(
          bulk_arpu = bulk_arpu,
          take_percent = take_percent,
          margin = margin,
          cost_per_unit = cost_per_unit,
          type_ii_cost_per_month = type_ii_cost_per_month,
          num_units = total_units
        )
      },
      NA
    )
  ) %>%
  mutate(
    onnet_pb = ifelse(
      !is.na(total_units) & total_units > 0 & dist >= 0,
      {
        payback <- onnet_payback(
          bulk_arpu = bulk_arpu,
          take_percent = take_percent,
          margin = margin,
          cost_per_unit = cost_per_unit,
          cost_per_foot = cost_per_foot,
          num_units = total_units,
          distance_adj = distance_adj,
          network_prox = dist
        )
      },
      NA
    )
  ) %>%
  ungroup()

################################################################################
# STEP 7:  SCALE DATA & RUN CORRELATION ANALYSIS
################################################################################

library(corrplot) 
library(RColorBrewer) 
library(factoextra)
library(ggrepel)

theme_set(theme_minimal())

# cluster_analytic_df <- final_analytic_df %>%
#   filter(!is.na(median_age) & !is.na(dist))

# Add/remove variables and use complete.cases to remove NAs
cluster_analytic_df <- final_analytic_pb[complete.cases(final_analytic_pb[, c(
 "median_age",
 "dist"
)]), ]

cluster_df_scaled <- cluster_analytic_df %>%
  select_if(is.numeric) %>%
  scale()
  
summary(cluster_df_scaled)

corr_df_scaled <- as.data.frame(cluster_df_scaled) %>%
  select(
    median_age,
    #hu_per_lane_mile,
    dist
  )

M<-cor(corr_df_scaled) 
head(round(M,2)) 

col <- colorRampPalette(c("#BB4444", "#EE9988",  
                          "#FFFFFF", "#77AADD", 
                          "#4477AA")) 

par(mar = c(5, 4, 4, 4))  # Adjust margins as needed: bottom, left, top, right
corrplot(M, 
         method = "color", 
         col = col(200),   
         type = "upper", 
         order = "hclust",  
         addCoef.col = "black", # Add coefficient of correlation 
         tl.col="black", 
         tl.srt = 45, # Text label color and rotation 
         # p.mat = p.mat, 
         sig.level = 0.01, 
         insig = "blank",  
         diag = FALSE # Hide correlation on the diagonal
)

################################################################################
# STEP 8:  CLUSTER ANALYSIS
################################################################################

# Create data table of entity_id labels
hoa_df_labels <- cluster_analytic_df$entity_id
table(hoa_df_labels)
  
# Method 1:  Determining and visualizing the optimal number of clusters
fviz_nbclust(corr_df_scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# Method 2:  Determining and visualizing the optimal number of clusters
# Note that the Silhouette Method requires a lot of memory.
#fviz_nbclust(corr_df_scaled, kmeans, method = "silhouette") + 
#  labs(subtitle = "Silhouette Method")

# Compute the clusters
set.seed(80108) #Set seed to ensure reproducible modelling results
km_out <- kmeans(corr_df_scaled, 
                 centers = 4, 
                 nstart = 100)

# Inspect the cluster results
print(km_out)

# Visualize the cluster results
km_clusters <- km_out$cluster
rownames(corr_df_scaled) <- paste(cluster_analytic_df$entity_id, 1:dim(cluster_analytic_df)[1], sep = "_")
par(mar = c(5, 4, 4, 2) + 0.1)  # Default margins

# Method 1: Visualize cluster output with point geometry
fviz_cluster(list(data=corr_df_scaled, cluster = km_clusters), geom = "point", ellipse.type = "convex") +
  labs(title = "K-Means Clustering", subtitle = "Cluster Visualization")

# Method 2: Visualize cluster output excluding point geometry
#fviz_cluster(list(data=corr_df_scaled, cluster = km_clusters), geom = "arrow") +
#  labs(title = "Cluster Visualization with Variable Contributions")

# Compute cluster means and inspect results
table(km_clusters, cluster_analytic_df$entity_id)

# Cluster means function
seg_sum <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

# Call cluster means function and inspect the results
seg_sum(cluster_analytic_df,km_clusters)

################################################################################
# STEP 9:  APPEND CLUSTER SCORES AND SAVE TO CSV
################################################################################

clus_scores <- km_clusters

hoa_scored <- tibble(cluster_analytic_df,clus_scores) %>% 
  mutate(cluster = as.character(clus_scores), entity_id = trimws(entity_id)) %>% 
  select(entity_id,cluster)

hoa_scored <- as.data.frame(hoa_scored) %>% 
  mutate(entity_id = trimws(as.character(entity_id)))

# Append cluster segment names
summit_hoa_sales_list_final <- final_analytic_pb %>%
  left_join(hoa_scored, by = "entity_id") %>%
  mutate(cluster = if_else(is.na(cluster),"Other",cluster)) %>%
  mutate(segment_name =
           ifelse(cluster==1,"Established Retirement Communities",
            ifelse(cluster==2,"Atlantic Coastal Corridor",
              ifelse(cluster==3,"Panhandle, Low-Density Competitive Markets",
                ifelse(cluster==4,"Central and Coastal Near Net Opportunities",
                       "Other"))))) %>%
  select(-c(
    cluster,
    fips_code,
    addr,
    usps_zip_pref_state,
    county_name,
    usps_zip_pref_city)
    )

# Number of rows in final output should equal src data from Step 1.
nrow(summit_hoa_sales_list_final)

# Write final output to CSV and export results
write.csv(summit_hoa_sales_list_final, 
          "SUMMIT_HOA_SALES_LIST_ENHANCED_DEC24_V3.csv", #rename as needed
          row.names = FALSE)

################################################################################
# STEP 10:  OPTIONAL - ALTERNATE METHOD FOR INSPECTING CLUSTER MEANS
################################################################################

# Compute means by cluster
mean_values_by_cluster <- summit_hoa_sales_list_final %>%
  filter(median_age > 0, # Exclude values <= 0
         median_income > 0, 
         median_home_value > 0, 
         total_units > 0, 
         hu_per_lane_mile > 0, 
         providers > 0) %>%  
  group_by(cluster) %>%
  summarise(
    mean_median_age = mean(median_age, na.rm = TRUE),    # Mean for median_age
    mean_median_income = mean(median_income, na.rm = TRUE), # Mean for median_income
    mean_median_home_value = mean(median_home_value, na.rm = TRUE), # Mean for median_income
    mean_total_units = mean(total_units, na.rm = TRUE), # Mean for median_income
    mean_mdu_pct = mean(mdu_pct, na.rm = TRUE), # Mean for median_income
    mean_hu_per_lane_mile = mean(hu_per_lane_mile, na.rm = TRUE), # Mean for median_income
    mean_distance_miles = mean(dist, na.rm = TRUE), # Mean for median_income
    property_cnt = n_distinct(entity_id),
    mean_providers = mean(providers, na.rm = TRUE) # Mean for median_income
  )

# Compute overall means
overall_means <- summit_hoa_sales_list_final %>%
  filter(median_age > 0, # Exclude values <= 0
         median_income > 0, 
         median_home_value > 0, 
         total_units > 0, 
         hu_per_lane_mile > 0, 
         providers > 0) %>%  
  summarise(
    cluster = "Overall",  # Add a label for the overall row
    mean_median_age = mean(median_age, na.rm = TRUE),    # Mean for median_age
    mean_median_income = mean(median_income, na.rm = TRUE), # Mean for median_income
    mean_median_home_value = mean(median_home_value, na.rm = TRUE), # Mean for median_income
    mean_total_units = mean(total_units, na.rm = TRUE), # Mean for median_income
    mean_mdu_pct = mean(mdu_pct, na.rm = TRUE), # Mean for median_income
    mean_hu_per_lane_mile = mean(hu_per_lane_mile, na.rm = TRUE), # Mean for median_income
    mean_distance_miles = mean(dist, na.rm = TRUE), # Mean for median_income
    property_cnt = n_distinct(entity_id),
    mean_providers = mean(providers, na.rm = TRUE) # Mean for median_income
  )

# Combine both
final_means <- bind_rows(mean_values_by_cluster, overall_means)

# View result
print(final_means)

#END############################################################################