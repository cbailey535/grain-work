library(fuzzyjoin)
library(tidyverse)

hoa_df <- read.csv('HOA_BOARD_LIST_SRC_20240903.csv') %>%
  janitor::clean_names() %>%
  mutate(
    hoa_src = "HOA",
    hoa_property_name = toupper(property_name),
    hoa_property_address = toupper(property_address),
    hoa_property_city = toupper(property_city),
    hoa_property_state= toupper(property_state),
    hoa_property_zip = as.character(property_zip),
    hoa_id = id,
    hoa_latitude = latitude,
    hoa_longitude = longitude,
    hoa_property_type = property_type,
    hoa_total_units = total_units,
    hoa_property_age = this_property_age,
    hoa_property_cnty = property_county,
    hoa_fips = fips,
    hoa_board_title = board_title,
    hoa_board_lname = board_last_name,
    hoa_board_fname = board_first_name,
    hoa_board_address = board_address,
    hoa_board_city = board_city,
    hoa_board_state = board_state,
    hoa_board_zip = as.character(board_zip),
    hoa_hoa_fee = 0,
    hoa_hoa_fee_min = 0,
    hoa_hoa_fee_max = 0,
    hoa_hoa_fee_type = "",
    hoa_hoa_fee_freq = ""
         ) %>%
  select(
    hoa_src,
    hoa_property_name,
    hoa_property_address,
    hoa_property_city,
    hoa_property_state,
    hoa_property_zip,
    hoa_id,
    hoa_latitude,
    hoa_longitude,
    hoa_property_type,
    hoa_total_units,
    hoa_property_age,
    hoa_property_cnty,
    hoa_fips,
    hoa_board_title,
    hoa_board_lname,
    hoa_board_fname,
    hoa_board_address,
    hoa_board_city,
    hoa_board_state,
    hoa_board_zip,
    hoa_hoa_fee,
    hoa_hoa_fee_min,
    hoa_hoa_fee_max,
    hoa_hoa_fee_type,
    hoa_hoa_fee_freq
  )
hoa_df$hoa_property_name <- toupper(hoa_df$hoa_property_name)
hoa_df$hoa_property_name <- gsub("[[:punct:]]", "", hoa_df$hoa_property_name)
hoa_df$hoa_property_name <- trimws(gsub("\\s+", " ", hoa_df$hoa_property_name))
hoa_df$hoa_property_name <- gsub("\\s+INC\\b", "", hoa_df$hoa_property_name, ignore.case = TRUE)
hoa_df$hoa_property_name <- trimws(hoa_df$hoa_property_name)
hoa_df$hoa_property_name <- gsub("\\b(CONDOS|CONDOMINIUM|CONDOMINIUMS)\\b", "CONDO", hoa_df$hoa_property_name, ignore.case = TRUE)
hoa_df$hoa_property_name <- gsub("\\s?\\b(ASSOC|ASSOCIATION|ASSOCIATIONS|ASSN|A|HOMEOWNERS|OWNERS)\\b", "", hoa_df$hoa_property_name, ignore.case = TRUE)
hoa_df$hoa_property_name <- trimws(hoa_df$hoa_property_name)
hoa_df$hoa_property_address <- gsub("\\b(DRIVE)\\b", "DR", hoa_df$hoa_property_address, ignore.case = TRUE)


twg_df <- read.csv('SummitBroadbandHOA09172024.csv') %>%
  janitor::clean_names() %>%
  mutate(
    twg_src = "TWG",
    twg_property_name = toupper(hoa1name),
    twg_property_address = toupper(paste(situshousenbr, situsstreet, situs_street_suffix, sep = " ")),
    twg_property_city = toupper(situscity),
    twg_property_state = toupper(situsstate),
    twg_property_zip = as.character(situszip5),
    twg_id = propertyid,
    twg_latitude = lat,
    twg_longitude = lon,
    twg_property_type = hoa1type,
    twg_total_units = 0,
    twg_property_age = 0,
    twg_property_cnty = "",
    twg_fips = fips,
    twg_board_title = "",
    twg_board_lname = str_to_upper(word(hoa1contactname1, -1)),
    twg_board_fname = str_to_upper(word(hoa1contactname1, 1)),
    twg_board_address = toupper(hoa1address1),
    twg_board_city = toupper(hoa1city1),
    twg_board_state = toupper(hoa1state1),
    twg_board_zip = as.character(hoa1zip1),
    twg_hoa_fee = hoa1feevalue,
    twg_hoa_fee_min = hoa1feeminimumvalue,
    twg_hoa_fee_max = hoa1feemaximumvalue,
    twg_hoa_fee_type = hoa1feetype,
    twg_hoa_fee_freq = hoa1feefrequency
    ) %>%
  select (
    twg_src,
    twg_property_name,
    twg_property_address,
    twg_property_city,
    twg_property_state,
    twg_property_zip,
    twg_id,
    twg_latitude,
    twg_longitude,
    twg_property_type,
    twg_total_units,
    twg_property_age,
    twg_property_cnty,
    twg_fips,
    twg_board_title,
    twg_board_lname,
    twg_board_fname,
    twg_board_address,
    twg_board_city,
    twg_board_state,
    twg_board_zip,
    twg_hoa_fee,
    twg_hoa_fee_min,
    twg_hoa_fee_max,
    twg_hoa_fee_type,
    twg_hoa_fee_freq
  )
twg_df$twg_property_name <- toupper(twg_df$twg_property_name)
twg_df$twg_property_name <- gsub("[[:punct:]]", "", twg_df$twg_property_name)
twg_df$twg_property_name <- trimws(gsub("\\s+", " ", twg_df$twg_property_name))
twg_df$twg_property_name <- gsub("\\s+INC\\b", "", twg_df$twg_property_name, ignore.case = TRUE)
twg_df$twg_property_name <- trimws(twg_df$twg_property_name)
twg_df$twg_property_name <- gsub("\\b(CONDOS|CONDOMINIUM|CONDOMINIUMS)\\b", "CONDO", twg_df$twg_property_name, ignore.case = TRUE)
twg_df$twg_property_name <- gsub("\\s?\\b(ASSOC|ASSOCIATION|ASSOCIATIONS|ASSN|A|HOMEOWNERS|OWNERS)\\b", "", twg_df$twg_property_name, ignore.case = TRUE)
twg_df$twg_property_name <- trimws(twg_df$twg_property_name)



sun_df <- read.csv('sunshine_list_sf_out.csv') %>%
  janitor::clean_names() %>%
  mutate(
    sun_src = "SUNSHINE",
    sun_property_name = toupper(association_name),
    sun_property_address = toupper(address),
    sun_property_city = toupper(city),
    sun_property_state = toupper(state),
    sun_property_zip = as.character(zip_code),
    sun_id = "",
    sun_latitude = latitude,
    sun_longitude = longitude,
    sun_property_type = "",
    sun_total_units = housing_units,
    sun_property_age = 0,
    sun_property_cnty = county_name,
    sun_fips = 0,
    sun_board_title = "",
    sun_board_lname = "",
    sun_board_fname = "",
    sun_board_address = "",
    sun_board_city = "",
    sun_board_state = "",
    sun_board_zip = "",
    sun_hoa_fee = 0,
    sun_hoa_fee_min = 0,
    sun_hoa_fee_max = 0,
    sun_hoa_fee_type = "",
    sun_hoa_fee_freq = ""
  ) %>%
  select (
    sun_src,
    sun_property_name,
    sun_property_address,
    sun_property_city,
    sun_property_state,
    sun_property_zip,
    sun_id,
    sun_latitude,
    sun_longitude,
    sun_property_type,
    sun_total_units,
    sun_property_age,
    sun_property_cnty,
    sun_fips,
    sun_board_title,
    sun_board_lname,
    sun_board_fname,
    sun_board_address,
    sun_board_city,
    sun_board_state,
    sun_board_zip,
    sun_hoa_fee,
    sun_hoa_fee_min,
    sun_hoa_fee_max,
    sun_hoa_fee_type,
    sun_hoa_fee_freq
  )
sun_df$sun_property_name <- toupper(sun_df$sun_property_name)
sun_df$sun_property_name <- gsub("[[:punct:]]", "", sun_df$sun_property_name)
sun_df$sun_property_name <- trimws(gsub("\\s+", " ", sun_df$sun_property_name))
sun_df$sun_property_name <- gsub("\\s+INC\\b", "", sun_df$sun_property_name, ignore.case = TRUE)
sun_df$sun_property_name <- trimws(sun_df$sun_property_name)
sun_df$sun_property_name <- gsub("\\b(CONDOS|CONDOMINIUM|CONDOMINIUMS)\\b", "CONDO", sun_df$sun_property_name, ignore.case = TRUE)
sun_df$sun_property_name <- gsub("\\s?\\b(ASSOC|ASSOCIATION|ASSOCIATIONS|ASSN|A|HOMEOWNERS|OWNERS)\\b", "", sun_df$sun_property_name, ignore.case = TRUE)
sun_df$sun_property_name <- trimws(sun_df$sun_property_name)



# Perform the fuzzy join between HOA and TWG
hoa_twg_match <- stringdist_full_join(twg_df, hoa_df, by = c("twg_property_name" = "hoa_property_name"), max_dist = 0.1, method = "jw", distance_col = "dist")

# Perform the fuzzy join between the result of HOA_TWG_match and SUN
final_match_join <- stringdist_full_join(hoa_twg_match, sun_df, by = c("hoa_property_name" = "sun_property_name"), method = "jw", max_dist = 0.1)

final_match <- final_match_join %>%
  mutate(
    src =  pmax(hoa_src, twg_src, sun_src, na.rm = TRUE),
    property_name = pmax(hoa_property_name, twg_property_name, sun_property_name, na.rm = TRUE),
    property_address = pmax(hoa_property_address, twg_property_address, sun_property_address, na.rm = TRUE),
    property_city = pmax(hoa_property_city, twg_property_city, sun_property_city, na.rm = TRUE),
    property_state = pmax(hoa_property_state, twg_property_state, sun_property_state, na.rm = TRUE),
    property_zip = pmax(hoa_property_zip, twg_property_zip, sun_property_zip, na.rm = TRUE),
    match_dist = pmin(dist, na.rm = TRUE),
    
    # Ensure latitude and longitude match the selected property_address
    latitude = case_when(
      property_address == hoa_property_address ~ hoa_latitude,
      property_address == twg_property_address ~ twg_latitude,
      property_address == sun_property_address ~ sun_latitude,
      TRUE ~ NA_real_
    ),
    longitude = case_when(
      property_address == hoa_property_address ~ hoa_longitude,
      property_address == twg_property_address ~ twg_longitude,
      property_address == sun_property_address ~ sun_longitude,
      TRUE ~ NA_real_
    ),
    property_type = pmax(hoa_property_type, twg_property_type, sun_property_type, na.rm = TRUE),
    total_units = pmax(hoa_total_units, twg_total_units, sun_total_units, na.rm = TRUE),
    property_age = pmax(hoa_property_age, twg_property_age, sun_property_age, na.rm = TRUE),
    property_cnty = pmax(hoa_property_cnty, twg_property_cnty, sun_property_cnty, na.rm = TRUE),
    fips = pmax(hoa_fips, twg_fips, sun_fips, na.rm = TRUE),
    board_title = pmax(hoa_board_title, twg_board_title, sun_board_title, na.rm = TRUE),
    board_lname = pmax(hoa_board_lname, twg_board_lname, sun_board_lname, na.rm = TRUE),
    board_fname = pmax(hoa_board_fname, twg_board_fname, sun_board_fname, na.rm = TRUE),
    board_address = pmax(hoa_board_address, twg_board_address, sun_board_address, na.rm = TRUE),
    board_city = pmax(hoa_board_city, twg_board_city, sun_board_city, na.rm = TRUE),
    board_state = pmax(hoa_board_state, twg_board_state, sun_board_state, na.rm = TRUE),
    board_zip = pmax(hoa_board_zip, twg_board_zip, sun_board_zip, na.rm = TRUE),
    hoa_fee = pmax(hoa_hoa_fee, twg_hoa_fee, sun_hoa_fee, na.rm = TRUE),
    hoa_fee_min = pmax(hoa_hoa_fee_min, twg_hoa_fee_min, sun_hoa_fee_min, na.rm = TRUE),
    hoa_fee_max = pmax(hoa_hoa_fee_max, twg_hoa_fee_max, sun_hoa_fee_max, na.rm = TRUE),
    hoa_fee_type = pmax(hoa_hoa_fee_type, twg_hoa_fee_type, sun_hoa_fee_type, na.rm = TRUE),
    hoa_fee_freq = pmax(hoa_hoa_fee_freq, twg_hoa_fee_freq, sun_hoa_fee_freq, na.rm = TRUE)) %>% 
  #filter(stringr::str_detect(property_name, "ARDEN PARK")) %>%
  group_by(property_name,property_address) %>%  # Group by key columns
  slice_min(order_by = match_dist, with_ties = FALSE) %>%  # Select the row with the lowest match_dist
  ungroup() %>% 
  #View()
  #select(property_name, address, everything()) %>%
  select(
    src,
    property_name,
    property_address,
    property_city,
    property_state,
    property_zip,
    #id = coalesce(hoa_id, twg_id, sun_id),
    latitude,
    longitude,
    property_type,
    total_units,
    property_age,
    property_cnty,
    fips,
    board_title,
    board_lname,
    board_fname,
    board_address,
    board_city,
    board_state,
    board_zip,
    hoa_fee,
    hoa_fee_min,
    hoa_fee_max,
    hoa_fee_type,
    hoa_fee_freq
  )


# Optional: Coalesce the property names and other columns to handle potential matches
#final_match <- final_match %>%
#  mutate(
#    src =  coalesce(hoa_src, twg_src, sun_src),
#    property_name = coalesce(hoa_property_name, twg_property_name, sun_property_name),
#    property_address = coalesce(hoa_property_address, twg_property_address, sun_property_address),
#    property_city = coalesce(hoa_property_city, twg_property_city, sun_property_city),
#    property_state = coalesce(hoa_property_state, twg_property_state, sun_property_state),
#    property_zip = coalesce(hoa_property_zip, twg_property_zip, sun_property_zip),
#    #id = coalesce(hoa_id, twg_id, sun_id),
#    latitude = coalesce(hoa_latitude, twg_latitude, sun_latitude),
#    longitude = coalesce(hoa_longitude, twg_longitude, sun_longitude),
#    property_type = coalesce(hoa_property_type, twg_property_type, sun_property_type),
#    total_units = coalesce(hoa_total_units, twg_total_units, sun_total_units),
#    property_age = coalesce(hoa_property_age, twg_property_age, sun_property_age),
#    property_cnty = coalesce(hoa_property_cnty, twg_property_cnty, sun_property_cnty),
#    fips = coalesce(hoa_fips, twg_fips, sun_fips),
#    board_title = coalesce(hoa_board_title, twg_board_title, sun_board_title),
#    board_lname = coalesce(hoa_board_lname, twg_board_lname, sun_board_lname),
#    board_fname = coalesce(hoa_board_fname, twg_board_fname, sun_board_fname),
#    board_address = coalesce(hoa_board_address, twg_board_address, sun_board_address),
#    board_city = coalesce(hoa_board_city, twg_board_city, sun_board_city),
#    board_state = coalesce(hoa_board_state, twg_board_state, sun_board_state),
#    board_zip = coalesce(hoa_board_zip, twg_board_zip, sun_board_zip),
#    hoa_fee = pmax(hoa_hoa_fee, twg_hoa_fee, sun_hoa_fee, na.rm = TRUE),
#    hoa_fee_min = coalesce(hoa_hoa_fee_min, twg_hoa_fee_min, sun_hoa_fee_min),
#    hoa_fee_max = coalesce(hoa_hoa_fee_max, twg_hoa_fee_max, sun_hoa_fee_max),
#    hoa_fee_type = coalesce(hoa_hoa_fee_type, twg_hoa_fee_type, sun_hoa_fee_type),
#    hoa_fee_freq = coalesce(hoa_hoa_fee_freq, twg_hoa_fee_freq, sun_hoa_fee_freq)
#  ) %>%
#  filter(property_name %in% c("ANDOVER LAKES PHASE 3","101 EOLA CONDO")) %>%
#  select(src,
#         property_name,
#         property_address,
#         property_city,
#         hoa_property_name,
#         twg_property_name,
#         sun_property_name,
#         hoa_property_address,
#         twg_property_address,
#         sun_property_address,
#         hoa_latitude, 
#         hoa_longitude,
#         twg_latitude,
#         twg_longitude, 
#         sun_latitude, 
#         sun_longitude,
#         hoa_hoa_fee, 
#         twg_hoa_fee, 
#         sun_hoa_fee
#         
#         
#         )
##
##View(final_match)
#
## Select relevant columns, keeping the coalesced versions
#final_match_out <- final_match %>%
#  #select(property_name, address, everything()) %>%
#  select(
#    src,
#    property_name,
#    property_address,
#    property_city,
#    property_state,
#    property_zip,
#    #id = coalesce(hoa_id, twg_id, sun_id),
#    latitude,
#    longitude,
#    property_type,
#    total_units,
#    property_age,
#    property_cnty,
#    fips,
#    board_title,
#    board_lname,
#    board_fname,
#    board_address,
#    board_city,
#    board_state,
#    board_zip,
#    hoa_fee,
#    hoa_fee_min,
#    hoa_fee_max,
#    hoa_fee_type,
#    hoa_fee_freq
#    ) %>%
#  distinct()
#skimr::skim(final_match_out)
## View the final result
#View(final_match_out)

n_distinct(final_match$property_name)
summary(hoa_df)

write.csv(final_match, "hoa_master_list_transformed.csv", row.names = FALSE)


skimr::skim(hoa_df)
skimr::skim(twg_df)
skimr::skim(sun_df)


hoa_twg_join <- stringdist_inner_join(twg_df, hoa_df, by = c("twg_property_name" = "hoa_property_name"), max_dist = 0.1, method = "jw", distance_col = "dist")
skimr::skim(hoa_twg_join)
n_distinct(hoa_twg_join$twg_property_name)

hoa_sun_join <- stringdist_inner_join(hoa_df, sun_df, by = c("hoa_property_name" = "sun_property_name"), max_dist = 0.1, method = "jw", distance_col = "dist")
skimr::skim(hoa_sun_join)
n_distinct(hoa_sun_join$sun_property_name)

twg_sun_join <- stringdist_inner_join(twg_df, sun_df, by = c("twg_property_name" = "sun_property_name"), max_dist = 0.1, method = "jw", distance_col = "dist")
skimr::skim(twg_sun_join)
n_distinct(twg_sun_join$twg_property_name)

hoa_sun_join %>% select(hoa_property_name,sun_property_name,dist) %>% View()































# Preprocess PROPERTY_NAME by removing punctuation, trimming whitespace, and converting to uppercase
#hoa_preprocess_names <- function(df) {
#  df$property_name <- toupper(df$hoa_property_name)
#  df$property_name <- gsub("[[:punct:]]", "", df$hoa_property_name)
#  df$property_name <- trimws(gsub("\\s+", " ", df$hoa_property_name))
#  df$property_name <- gsub("\\s+INC\\b", "", df$hoa_property_name, ignore.case = TRUE)
#  df$property_name <- trimws(df$hoa_property_name)
#  df$property_name <- gsub("\\b(CONDOS|CONDOMINIUM|CONDOMINIUMS)\\b", "CONDO", df$hoa_property_name, ignore.case = TRUE)
#  df$property_name <- gsub("\\s?\\b(ASSOC|ASSOCIATION|ASSOCIATIONS|ASSN|A|HOMEOWNERS|OWNERS)\\b", "", df$hoa_property_name, ignore.case = TRUE)
#  df$property_name <- trimws(df$hoa_property_name)
#  return(df)
#}
#
#twg_preprocess_names <- function(df) {
#  df$property_name <- toupper(df$twg_property_name)
#  df$property_name <- gsub("[[:punct:]]", "", df$twg_property_name)
#  df$property_name <- trimws(gsub("\\s+", " ", df$twg_property_name))
#  df$property_name <- gsub("\\s+INC\\b", "", df$twg_property_name, ignore.case = TRUE)
#  df$property_name <- trimws(df$twg_property_name)
#  df$property_name <- gsub("\\b(CONDOS|CONDOMINIUM|CONDOMINIUMS)\\b", "CONDO", df$twg_property_name, ignore.case = TRUE)
#  df$property_name <- gsub("\\s?\\b(ASSOC|ASSOCIATION|ASSOCIATIONS|ASSN|A|HOMEOWNERS|OWNERS)\\b", "", df$twg_property_name, ignore.case = TRUE)
#  df$property_name <- trimws(df$twg_property_name)
#  return(df)
#}

# Apply the preprocessing to each dataframe
#hoa_proc <- hoa_preprocess_names(hoa_df)
#twg_proc <- twg_preprocess_names(twg_df)
#sun_proc <- sun_preprocess_names(twg_df)
#SUNSHINE <- preprocess_names(SUNSHINE)


# Fuzzy join between HOA and TWG on PROPERTY_NAME
#hoa_twg_match <- stringdist_full_join(twg_df, hoa_df, by = c("twg_property_name" = "hoa_property_name"), max_dist = 0.1, method = "jw", distance_col = "dist")
#View(hoa_twg_match)
## Rename the distance column if needed (you can customize the name here)
#hoa_twg_match <- hoa_twg_match %>%
#  rename(distance_hoa_twg = dist) # Change "distance_hoa_twg" to your preferred name
#
#
#all_matches <- stringdist_full_join(hoa_twg_match, sun_df, by = c("twg_property_name" = "sun_property_name"), max_dist = 0.1, method = "jw", distance_col = "dist")
#
## Rename the distance column in the final matches
#all_matches <- all_matches %>%
#  rename(distance_all_matches = dist)  # Change "distance_all_matches" to your preferred name
##hoa_twg_match <- stringdist_inner_join(hoa_proc, twg_proc, by = c("property_name" = "property_name"), max_dist = 2, distance_col = "dist")
#
#View(all_matches)
#
## Select the row with the lowest match score (dist) for each hoa_proc property_name
#best_matches <- all_matches %>%
#  mutate(ID = row_number()) %>% # Add row number as ID
#  select(
#    ID,
#    twg_property_name,
#    hoa_property_name,
#    sun_property_name,
#    twg_property_address,
#    hoa_property_address,
#    sun_property_address,
#    twg_property_city,
#    hoa_property_city,
#    sun_property_city,
#    twg_property_state,
#    hoa_property_state,
#    hoa_property_state,
#    twg_property_zip,
#    hoa_property_zip,
#    sun_property_zip,
#    twg_id,
#    hoa_id,
#    sun_id,
#    distance_hoa_twg
#    ) %>% 
#  group_by(hoa_property_name) %>%# Group by the property_name from hoa_proc
#  slice_min(order_by = distance_hoa_twg, with_ties = FALSE) %>%  # Get the lowest distance, without ties
#  ungroup() 
#
#best_matches 
#View(best_matches)
#
#
#nrow(hoa_df)
#nrow(twg_df)
#nrow(sun_df)
#
#hoa_df %>% summarise(n_distinct(property_name))
#twg_df %>% summarise(n_distinct(hoa1name))
#sun_df %>% summarise(n_distinct(association_name))
#
#best_matches %>% filter(distance_all_matches>=0)
#
#write.csv(best_matches, "best_matches.csv", row.names = FALSE)


############################ALTERNATE CODE

# Assuming your data frames are named HOA, TWG, and SUN, each with a property_name column

