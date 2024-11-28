# rm(list=ls())

library(tidyverse)
library(tidyquant)
library(ggrepel)
# library(cluster)
# library(psych)

theme_set(theme_minimal())

# LOAD DATA

src_data <- FCC_CLUSTER_DF

summary(src_data)


# REMOVE NAs
src_data_trans <- src_data[complete.cases(src_data[, c(
  "MEDIAN_AGE_ACS_16_20", 
  "MED_HHD_INC_ACS_16_20",
  "OCCP_UNITS",
  "POP_65PLUS",
  "HHD_UND_18",
  "HHD_MOVED",
  "SFU",
  "HHD_MRDCPLE_FMLY",
  "HHD_W_BROADBAND",
  "HHD_W_ONLYMOBILE",
  "HHD_DENSITY_LAND_AREA"
)]), ]  

summary(src_data_trans)

# EDA

# Correlation
scaled_data <- src_data_trans %>%
  select_if(is.numeric) %>%
  scale()

summary(scaled_data)

scaled_df <- as.data.frame(scaled_data) %>%
  select(c(
    NUM_COMPETITORS_ALL,
    OCCP_UNITS,
    POP_65PLUS,
    HHD_UND_18,
    HHD_MOVED,
    SFU,
    HHD_MRDCPLE_FMLY,
    HHD_W_BROADBAND,
    HHD_W_ONLYMOBILE,
    HHD_DENSITY_LAND_AREA,
    BEAD_ELIGIBLE,
    MEDIAN_AGE_ACS_16_20,
    MED_HHD_INC_ACS_16_20,
    MAX_DOWN_ALL_SPEEDS,
    NUM_CABLE_COMPETITORS,
    NUM_COPPER_COMPETITORS,
    NUM_FTTH_COMPETITORS,
    NUM_FWA_COMPETITORS,
    HHD_PER_ROAD_MILE,
    
  ))

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

hunter_df_labels <- src_data_trans$CENSUS_TRACT
table(hunter_df_labels)

fcc_df_corr <- src_data_trans %>%
  select(
    # HHD_W_ONLYMOBILE,
    # HHD_W_BROADBAND,
    # NUM_COMPETITORS_ALL,
    # SFU,
    # HHD_MOVED,
    # HHD_PER_ROAD_MILE,
    # NUM_FTTH_COMPETITORS,
    MEDIAN_AGE_ACS_16_20,
    MED_HHD_INC_ACS_16_20
  )

# Scale data (so distance metrics are unweighted)
fcc_df_scale <- scale(fcc_df_corr)


# Calculate distance metric
#hunter_df_dist <- dist(hunter_df_scale)

# Calculate k based on WSS
# fviz_nbclust(fcc_df_scale, kmeans, method = "wss") +
#   labs(subtitle = "Elbow method")

# fviz_nbclust(fcc_df_scale, kmeans, method = "silhouette")    #MEMORY INTENSIVE

# Kmeans clustering
set.seed(80108)
km_out <- kmeans(fcc_df_scale, centers = 7, nstart = 100)
print(km_out)

# Visualize the cluster results
km_clusters <- km_out$cluster
rownames(fcc_df_scale) <- paste(src_data_trans$CENSUS_TRACT, 1:dim(src_data_trans)[1], sep = "_")
fviz_cluster(list(data=fcc_df_scale, cluster = km_clusters))
table(km_clusters, src_data_trans$CENSUS_TRACT)

seg_sum <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg_sum(src_data_trans,km_clusters)

clus_scores <- km_clusters
sales_scored <- tibble(src_data_trans,clus_scores)
write.csv(sales_scored, "fcc_clus_scores.csv")