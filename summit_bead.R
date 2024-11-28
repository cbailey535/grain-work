library(sf)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(RColorBrewer)

kml_file <- '/cloud/project/wa.kml'
kml <- readr::read_file(kml_file)

# Read the KML file into a spatial object
# kml <- st_read(kml_file) 

# Now, you can work with the geospatial data in R
# For example, you can plot it with ggplot2:
# library(ggplot2)
# ggplot() +
#   geom_sf(data = kml_data)


# Create a basic Leaflet map
leaflet() %>% 
  # setView(-77.0369, 38.9072, 12) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # addWebGLKMLHeatmap(kml, size = 20, units = "px") %>%
  addKML(
    kml,
    markerType = "circleMarker",
    stroke = FALSE, fillColor = "blue", fillOpacity = 1,
    markerOptions = markerOptions(radius = 1))



# Load the sf library
library(sf)

# Read the KML file with LINESTRING using sf::read_sf

kml_file <- '/cloud/project/summit.kml'
kml_file <- sf::read_sf(kml_file)
View(kml_file)
# Read the CSV file with LAT/LON Coordinates

stores_file <- '/cloud/project/ftmeyer.csv'
stores_file <- read.csv(stores_file)
stores_sf <- st_as_sf(stores_file, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(stores_file))

# stores_sf <- st_as_sf(stores_file, coords = c("longitude", "latitude"))
st_crs(stores_sf) <- st_crs(kml_file)  # Set the CRS of stores_sf to match WGS 84 (EPSG: 4326)

# Compute the distance between stores and LINESTRING:
distances <- st_distance(kml_file, stores_sf)

# stores data frame will contain a new column named "distance_to_LINESTRING" with the distances from each store to the LINESTRING in the KML file. You can then further analyze or visualize this data as needed in R Studio.
stores_file$distance_to_LINESTRING <- distances

kml_file$distance_to_LINESTRING <- distances
stores_file

1148438.399/1609.344

View(kml_file)

library(tidycensus)

census_api_key('d2f0fe8f1e5a7984a67bbff53c703bc9c6ea9da9', install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")
# You can check it with:
# Sys.getenv("CENSUS_API_KEY")

fl_tracts <- get_acs(geography = "tract", 
                        # variables = "B25077_001",
                        variables = "B23010_001",  #Households with children
                        # variables = "B09005_002",  #Married couple households
                        state = c("FL"),
                        # county = "Salt Lake County",
                        geometry = TRUE) 

summit_tracts <- inner_join(summit_tracts_fcc, fl_tracts, by = join_by(TRACT == GEOID))
summit_tracts_sf <- sf::st_as_sf(summit_tracts)

summit_kml_file <- '/cloud/project/summit.kml'
kml <- readr::read_file(summit_kml_file)

# kml <- st_read('/cloud/project/summit.kml') 

# Create a basic Leaflet map
leaflet() %>% 
  # setView(-77.0369, 38.9072, 12) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = summit_tracts_sf, fillColor = "blue", weight = 2, opacity = 1, color = "white",
              fillOpacity = 0.2, popup = ~NAME) %>%
  # addWebGLKMLHeatmap(kml, size = 20, units = "px") %>%
  addKML(
    kml,
    # markerType = "marker",
    stroke = TRUE, fillColor = "black", fillOpacity = 0, weight = 1,
    markerOptions = markerOptions(radius = 1))




--------------------------------
--------------------------------
  
 

kml_file <- '/cloud/project/wa.kml'
kml <- readr::read_file(kml_file)

# Read the KML file into a spatial object
# kml <- st_read(kml_file) 

# Now, you can work with the geospatial data in R
# For example, you can plot it with ggplot2:
# library(ggplot2)
# ggplot() +
#   geom_sf(data = kml_data)


# Create a basic Leaflet map
leaflet() %>% 
  # setView(-77.0369, 38.9072, 12) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # addWebGLKMLHeatmap(kml, size = 20, units = "px") %>%
  addKML(
    kml,
    markerType = "circleMarker",
    stroke = FALSE, fillColor = "blue", fillOpacity = 1,
    markerOptions = markerOptions(radius = 1))



# Load the sf library
library(sf)

# Read the KML file with LINESTRING using sf::read_sf

kml_file <- '/cloud/project/summit.kml'
kml_file <- sf::read_sf(kml_file)
View(kml_file)
# Read the CSV file with LAT/LON Coordinates

stores_file <- '/cloud/project/ftmeyer.csv'
stores_file <- read.csv(stores_file)
stores_sf <- st_as_sf(stores_file, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(stores_file))

# stores_sf <- st_as_sf(stores_file, coords = c("longitude", "latitude"))
st_crs(stores_sf) <- st_crs(kml_file)  # Set the CRS of stores_sf to match WGS 84 (EPSG: 4326)

# Compute the distance between stores and LINESTRING:
distances <- st_distance(kml_file, stores_sf)

# stores data frame will contain a new column named "distance_to_LINESTRING" with the distances from each store to the LINESTRING in the KML file. You can then further analyze or visualize this data as needed in R Studio.
stores_file$distance_to_LINESTRING <- distances

kml_file$distance_to_LINESTRING <- distances
stores_file

1148438.399/1609.344

View(kml_file)

library(tidycensus)

census_api_key('d2f0fe8f1e5a7984a67bbff53c703bc9c6ea9da9', install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")
# You can check it with:
# Sys.getenv("CENSUS_API_KEY")

fl_county <- get_acs(geography = "county", 
                     # variables = "B25077_001",
                     variables = "B23010_001",  #Households with children
                     # variables = "B09005_002",  #Married couple households
                     state = c("FL"),
                     # county = "Salt Lake County",
                     geometry = TRUE) 

fl_county_df <- inner_join(fl_fcc_stats, fl_county, by = join_by(GEOID == GEOID)) %>% mutate(BEAD_COVERAGE = TOTAL_BEAD_LOCATIONS/TOTAL_LOCATIONS)
fl_county_sf <- sf::st_as_sf(fl_county_df)

summit_kml_file <- '/cloud/project/summit.kml'
kml <- readr::read_file(summit_kml_file)

# kml <- st_read('/cloud/project/summit.kml') 

factpal <- colorNumeric(rev(brewer.pal(n = 5, name ="RdBu")), fl_county_sf$TOTAL_BEAD_LOCATIONS) 

labels <- paste0(
  "<strong> County: </strong>",
  fl_county_sf$NAME, "<br/>",
  "<strong> BEAD Locations: </strong>",
  scales::comma(fl_county_sf$TOTAL_BEAD_LOCATIONS, accuracy = 0.1), "<br/>",
  "<strong> BEAD Coverage: </strong>",
  scales::percent(fl_county_sf$BEAD_COVERAGE, accuracy = 0.1), "<br/>"
) %>%
  lapply(htmltools::HTML)



# Create a basic Leaflet map
leaflet() %>% 
  # setView(-77.0369, 38.9072, 12) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(provider = "OpenStreetMap", options = providerTileOptions(opacity = 1)) %>%
  addPolygons(
    data = fl_county_sf, 
    fillColor = ~factpal(fl_county_sf$TOTAL_BEAD_LOCATIONS), 
    weight = 1, 
    opacity = 1, 
    color = "white",
    fillOpacity = 0.5, popup = labels
    ) %>%
  # addWebGLKMLHeatmap(kml, size = 20, units = "px") %>%
  addKML(
    kml,
    # markerType = "marker",
    stroke = TRUE, fillColor = "black", fillOpacity = 0, weight = 1,
    markerOptions = markerOptions(radius = 1)) %>%
  addLegend(
    position = "bottomright",
    pal = factpal,
    values = fl_county_sf$TOTAL_BEAD_LOCATIONS,
    title = "BEAD Locations",
    # labFormat = labelFormat(prefix = "$"),
    opacity = 1)

