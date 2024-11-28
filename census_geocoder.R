# Source: https://cran.r-project.org/web/packages/tidygeocoder/vignettes/tidygeocoder.html

#install.packages("tidygeocoder")
library(tidygeocoder)
library(tidyverse)
library(leaflet)

# create a dataframe with addresses
some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "Grain",                "1900 K St NW, Washington DC 20006",
  "HOA",                  "1350 Main St, Sarasota, FL, 34236",
  "HOA",                  "1463 Oakfield Dr, Brandon, FL, 33511",
  "HOA",                  "5751 Soldier Cir, Sarasota, FL, 34233",
  "HOA",                  "1162 Indian Hills Blvd, Venice, FL, 34293",
  "HOA",                  "800 N Tamiami Trl, Sarasota, FL, 34236",
  "HOA",                  "9031 Town Center Pkwy, Bradenton, FL, 34202",
  "HOA",                  "1800 2nd St, Sarasota, FL, 34236",
  "HOA",                  "8588 Potter Park Dr, Sarasota, FL, 34238",
  "HOA",                  "5400 26th St W, Bradenton, FL, 34207",
  "HOA",                  "9031 Town Center Pkwy, Bradenton, FL, 34202",
  "HOA",                  "2654 Cypress Ridge Blvd, Wesley Chapel, FL, 33544",
  "HOA",                  "3701 S Osprey Ave, Sarasota, FL, 34239",
  "HOA",                  "4110 S Florida Ave, Lakeland, FL, 33813",
  "HOA",                  "1062 E Venice Ave, Venice, FL, 34285",
  "HOA",                  "2848 Proctor Rd, Sarasota, FL, 34231",
  "HOA",                  "4370 S Tamiami Trl, Sarasota, FL, 34231",
  "HOA",                  "9031 Town Center Pkwy, Bradenton, FL, 34202",
  "HOA",                  "4301 32nd St W, Bradenton, FL, 34205",
  "HOA",                  "5602 Marquesas Cir, Sarasota, FL, 34233",
  "Home",                 "6831 Northstar Cir, Castle Rock, CO, 80108"
)

lat_longs <- some_addresses %>%
  geocode(addr, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies')) #method "osm"

View(lat_longs)

# visualizing using leaflet
leaflet()  %>% 
  addTiles(group = "OSM") %>%
  addMarkers(
    data = lat_longs,
    lat =  ~ lat,    
    lng =  ~ long,
    label = ~ name,
    labelOptions = labelOptions(noHide = TRUE, direction = 'auto')
  )
