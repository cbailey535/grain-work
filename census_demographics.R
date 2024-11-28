library(tidyverse)
library(tidycensus)

fl_hoa_stats <- read.csv('hoa_master_list.csv') %>%
  filter(latitude !=0, property_state=="FL") %>%
  mutate(token = tolower(gsub("\\s+", "", paste0(src,property_name, property_address)))) %>%
  mutate(housing_unit_bin = 
           ifelse(total_units==0,"0 HUs",
                  ifelse(total_units>0 & total_units<50,"1 to 50 HUs",
                         ifelse(total_units>=50 & total_units<=88,"50 to 88 HUs",
                                ifelse(total_units>=89 & total_units<=136,"89 to 136 HUs",
                                       ifelse(total_units>=137 & total_units<=235,"137 to 235 HUs","236+ HUs"))))),
         housing_unit_bin = as.factor(housing_unit_bin),
         median_age = suppressWarnings(as.numeric(median_age)),
         median_hhi = suppressWarnings(as.numeric(median_hhi)),
         tract = as.character(tract),
         monthly_hoa_fee = case_when(
           hoa_fee_freq == 'A' ~ hoa_fee / 12,
           hoa_fee_freq == 'Q' ~ hoa_fee / 4,
           hoa_fee_freq == 'S' ~ hoa_fee / 6,
           TRUE ~ hoa_fee
         ) 
  ) %>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

desired_order <- c("0 HUs","1 to 50 HUs","50 to 88 HUs", "89 to 136 HUs", "137 to 235 HUs", "236+ HUs")  # Modify this based on your actual bin labels

# Reorder the levels of the factor
fl_hoa_stats$housing_unit_bin <- factor(fl_hoa_stats$housing_unit_bin, levels = desired_order)

census_api_key('d2f0fe8f1e5a7984a67bbff53c703bc9c6ea9da9', install = TRUE, overwrite = TRUE)

# Pull ACS data for median home values by Census Tract in California (2021)
home_values <- get_acs(
  geography = "tract",
  variables = "B25077_001",  # Median home value variable
  state = "FL",              # Replace with your state
  year = 2021,               # ACS 5-year estimates
  survey = "acs5",           # Use ACS 5-year survey
  output = "wide"            # Output in wide format
)

# Inspect data
head(home_values)

# Optional: Filter data for a specific county (e.g., Los Angeles County)
home_values_filtered <- home_values %>%
  filter(str_detect(NAME, "Orange County"))

# View filtered data
home_values_oc <- home_values_filtered %>% 
  filter(!is.na(B25077_001E)) %>% 
  select(-B25077_001M)

home_values_joined <- inner_join(home_values_oc,fl_hoa_stats, by = c("GEOID" = "tract")) %>% 
  select(GEOID,token,monthly_hoa_fee,median_age,median_hhi,B25077_001E,total_units,pct_pop_65plus,pct_hhd_with_computer,pct_hhd_only_sphne,pct_hhd_with_broadband)

home_values_analytic_df <- home_values_joined %>% 
  rename(house_value = B25077_001E) %>% 
  filter(total_units>0) %>% 
  filter(monthly_hoa_fee>0)

#library(tidyverse)
#library(tidyquant)
#library(ggrepel)
# library(cluster)
# library(psych)

theme_set(theme_minimal())

# REMOVE NAs
src_data_trans <- home_values_analytic_df[complete.cases(home_values_analytic_df[, c(
  "monthly_hoa_fee", 
  "median_age",
  "median_hhi",
  "house_value",
  "total_units",
  "pct_pop_65plus",
  "pct_hhd_with_computer",
  "pct_hhd_only_sphne",
  "pct_hhd_with_broadband"
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
    monthly_hoa_fee,
    median_age,
    median_hhi,
    house_value,
    total_units,
    pct_pop_65plus,
    pct_hhd_with_computer,
    pct_hhd_only_sphne,
    pct_hhd_with_broadband
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


psych::pairs.panels(scaled_df,
                    gap = 0,
                    # bg = c("red","blue","green","gray80")[sent_raw$company],
                    pch = 21,
                    cex.cor = 1.5,
                    cex.labels = 1.5,
                    stars = T)

ggpubr::ggscatter(scaled_df, x = "hoa_fee", y = "house_value", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "price", ylab = "internet_outages")

corrplot::corrplot(M, method = 'number', order = 'alphabet', diag = F) # colorful number
