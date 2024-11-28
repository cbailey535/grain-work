# rm(list=ls())

library(tidyverse)
library(tidyquant)
library(ggrepel)
# library(cluster)
# library(psych)

theme_set(theme_minimal())

# LOAD DATA

src_data <- HUNTER_CLUSTER_MODEL_SQL

# FETCH NEW ACS VARIABLES

library(tidycensus)

census_api_key('d2f0fe8f1e5a7984a67bbff53c703bc9c6ea9da9', install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")
# You can check it with:
# Sys.getenv("CENSUS_API_KEY")

child_hh_acs <- get_acs(geography = "tract", 
                     # variables = "B25077_001",
                     variables = "B23010_001",  #Households with children
                     # variables = "B09005_002",  #Married couple households
                     state = c("OR","CA"),
                     # county = "Salt Lake County",
                     geometry = FALSE) %>%
            mutate(HH_CHLD = estimate) %>%
            select(c(GEOID,NAME,HH_CHLD))

hh_65plus <- get_acs(geography = "tract", 
                        variables = "B16007_015",  #Households with 65+
                        state = c("OR","CA"),
                        geometry = FALSE) %>%
  mutate(HH_65PLUS = estimate) %>%
  select(c(GEOID,NAME,HH_65PLUS))

hh_married <- get_acs(geography = "tract", 
                     variables = "B09005_002",  #Households with Married+
                     state = c("OR","CA"),
                     geometry = FALSE) %>%
  mutate(HH_MARRIED = estimate) %>%
  select(c(GEOID,NAME,HH_MARRIED))

# JOIN NEW ACS VARIABLES TO SRC_DATA

src_data_ext <- inner_join(src_data,child_hh_acs, by = join_by(TRACT == GEOID))
src_data_ext <- inner_join(src_data_ext,hh_65plus, by = join_by(TRACT == GEOID))
src_data_ext <- inner_join(src_data_ext,hh_married, by = join_by(TRACT == GEOID))

src_data_ext <- src_data_ext %>% 
  mutate(HH_CHLD_PCT = if_else(HH_CHLD==0,0,HH_CHLD/TOTAL_HOUSEHOLDS_CENSUS_2020),
         HH_65PLUS_PCT = if_else(HH_65PLUS==0,0,HH_65PLUS/TOTAL_HOUSEHOLDS_CENSUS_2020),
         HH_MARRIED_PCT = if_else(HH_MARRIED==0,0,HH_MARRIED/TOTAL_HOUSEHOLDS_CENSUS_2020))

summary(src_data_ext)


# REMOVE NAs
src_data_trans <- src_data_ext[complete.cases(src_data_ext[, c(
  "MEDIAN_AGE_ACS", 
  "MEDIAN_HHI"
  )]), ] %>%
  select(!c("TOTAL_ROAD_DISTANCE_MILES","HHLDS_PER_ROAD_MILE")) 
  
summary(src_data_trans)

# EDA

# Correlation
scaled_data <- src_data_trans %>%
  select_if(is.numeric) %>%
  scale()

scaled_df <- as.data.frame(scaled_data) %>%
  select(!c(LONGITUDE,LATITUDE))

summary(scaled_df)
# Correlogram in R 
# required package 
library(corrplot) 
library(RColorBrewer) 


M<-cor(scaled_df) 
head(round(M,2)) 

# customize the correlogram 

col <- colorRampPalette(c("#BB4444", "#EE9988",  
                          "#FFFFFF", "#77AADD", 
                          "#4477AA")) 

corrplot(M, method = "color", col = col(200),   
         type = "upper", order = "hclust",  
         addCoef.col = "black", # Add coefficient of correlation 
         tl.col="black", tl.srt = 45, # Text label color and rotation 
         
         # Combine with significance 
         # p.mat = p.mat, 
         sig.level = 0.01, insig = "blank",  
         
         # hide correlation coefficient 
         # on the principal diagonal 
         diag = FALSE 
)

# CLUSTER MODEL

library(factoextra)

names(src_data_trans)

hunter_df_labels <- src_data_trans$TRACT
table(hunter_df_labels)

hunter_df_corr <- src_data_trans %>%
  select(
    # FTTH_COMPETITORS,
    # FTTH_COVERAGE_PCT,
    MEDIAN_AGE_ACS,
    MEDIAN_HHI,
    # TOTAL_POPULATION_UNDER18_ACS,
    POP_UNDER18_PCT,
    # HH_CHLD_PCT
    # HUNTER_COVERAGE_PCT,
    TOTAL_COMPETITORS
    # HHLDS_PER_ROAD_MILE
    # HH_MARRIED_PCT
  )

# Scale data (so distance metrics are unweighted)
hunter_df_scale <- scale(hunter_df_corr)


# Calculate distance metric
#hunter_df_dist <- dist(hunter_df_scale)

# Calculate k based on WSS
# fviz_nbclust(hunter_df_scale, kmeans, method = "wss") + 
#   labs(subtitle = "Elbow method")

fviz_nbclust(hunter_df_scale, kmeans, method = "silhouette")

# Kmeans clustering
set.seed(80108)
km_out <- kmeans(hunter_df_scale, centers = 5, nstart = 100)
print(km_out)

# Visualize the cluster results
km_clusters <- km_out$cluster
rownames(hunter_df_scale) <- paste(src_data_trans$TRACT, 1:dim(src_data_trans)[1], sep = "_")
fviz_cluster(list(data=hunter_df_scale, cluster = km_clusters))
table(km_clusters, src_data_trans$TRACT)

seg_sum <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg_sum(src_data_trans,km_clusters)

clus_scores <- km_clusters
sales_scored <- tibble(src_data_trans,clus_scores)
write.csv(sales_scored, "hunter_clus_scores.csv")
# sales_scored %>% filter(clus_scores==4) %>% View()

# Make Map
# https://juliasilge.com/blog/using-tidycensus/


library(leaflet)
library(stringr)
library(sf)
library(RColorBrewer)



map_data <- inner_join(sales_scored,slc_value, by = join_by(TRACT == GEOID))
View(map_data)
map_data <- map_data %>%
  mutate(cluster = as_factor(clus_scores))

map_data <- data.frame(map_data)

map_data_sf <- sf::st_as_sf(map_data)


factpal <- colorFactor(brewer.pal(n = 5, name ="Spectral") , map_data$cluster) 

labels <- paste0(
  "<strong> Cluster: </strong>",
  map_data$cluster, "<br/>",
  # "<strong> Build Multiple: </strong>",
  # scales::number(block_map$'Build Multiple', accuracy = 0.1), "<br/>",
  "<strong> Tract: </strong>",
  map_data$NAME, "<br/>"
) %>%
  lapply(htmltools::HTML)

map_data_sf %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap", options = providerTileOptions(opacity = 1)) %>%
  # addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>%
  addPolygons(
              popup = ~cluster,
              stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
              color = "black", opacity = 1, weight = 1,
              fillColor = ~factpal(map_data$cluster), 
              label = labels) %>%
  # addLabelOnlyMarkers(
  #                     lng = ~LONGITUDE, lat = ~LATITUDE, label = ~HUNTER_AVAILABLE,
  #                     labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
  addLegend(
            position = "bottomright",
            pal = factpal,
            values = map_data$cluster,
            title = "Cluster Score",
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1)



geometry_att <- get_acs(geography = "tract", 
                        # variables = "B25077_001",
                        variables = "B23010_001",  #Households with children
                        # variables = "B09005_002",  #Married couple households
                        state = c("OR","CA"),
                        # county = "Salt Lake County",
                        geometry = TRUE)

get_acs(geography = "zcta",
        state = c("CO"),
        variables = "B23010_001",
        year = 2020, survey = "acs5",
        output = "wide", geometry = TRUE,
        keep_geo_vars=TRUE)
