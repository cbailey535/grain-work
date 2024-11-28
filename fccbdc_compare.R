https://walker-data.com/tidycensus/articles/other-datasets.html#migration-flows

library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(gghighlight)
library(scales)
library(showtext)
font_add_google("Roboto")
showtext_auto()

# STEP 1:  Create net migration rates by County

options(tigris_use_cache = TRUE)

# us_components <- get_estimates(geography = "state", product = "components", vintage = 2023)
# 
# us_components

net_migration_cnty <- get_estimates(geography = "county",
                               variables = "RNETMIG",
                               vintage = 2023,
                               geometry = TRUE,
                               resolution = "20m") %>%
                shift_geometry()

net_migration_cnty

order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration_cnty_df <- net_migration_cnty %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order)) %>%
  filter(GEOID != "72")

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration_cnty_df, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  geom_sf_label(data = state_overlay, aes(label = STUSPS)) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = "none") +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto", base_size = 18)+
  labs(title = "Population net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2023 Population Estimates",
       fill = "Rate",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "",
       y = "")

#########################################################################

# STEP 2:  Create net migration rate by State

options(tigris_use_cache = TRUE)

# us_components <- get_estimates(geography = "state", product = "components", vintage = 2023)
# 
# us_components %>% filter(variable %in% "RNETMIG") %>% select(GEOID,NAME,variable,value)

net_migration_state <- get_estimates(geography = "state",
                               variables = "RNETMIG",
                               vintage = 2023,
                               geometry = TRUE,
                               resolution = "20m") %>%
                              shift_geometry()

net_migration_state

order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration_state_df <- net_migration_state %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order)) %>%
  filter(GEOID != "72")

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration_state_df, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  geom_sf_label(data = state_overlay, aes(label = STUSPS)) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = "none") +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  labs(title = "Population net migration per 1000 residents by state",
       subtitle = "US Census Bureau 2023 Population Estimates",
       fill = "Rate",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "",
       y = "")

#########################################################################

# STEP 3:  Create change in FCC locations by state, June-to-Dec '23

bsl_chg_order = c("3%+", "2% to 3%", "1% to 2%", "0% to 1%", "-1% to 0%", "-2% to 1%", "Below -2%")

fcc_df <- read_csv('decjun2023.csv') %>% 
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(bsl_grp = case_when(
    PCT_CHG_BSL  >= 0.03 ~ "3%+",
    PCT_CHG_BSL  >= 0.02 & PCT_CHG_BSL < 0.03 ~ "2% to 3%",
    PCT_CHG_BSL  >= 0.01 & PCT_CHG_BSL < 0.02 ~ "1% to 2%",
    PCT_CHG_BSL  >= 0.0 & PCT_CHG_BSL < 0.01 ~ "0% to 1%",
    PCT_CHG_BSL  >= -0.01 & PCT_CHG_BSL < 0 ~ "-1% to 0%",
    PCT_CHG_BSL  >= -0.02 & PCT_CHG_BSL < -0.01 ~ "-2% to 1%",
    PCT_CHG_BSL  < -0.02 ~ "Below -2%",
    TRUE ~ "Below -2%"
  )) %>%
  mutate(bsl_grp = factor(bsl_grp, levels = bsl_chg_order))

fcc_pop_joined <- inner_join(fcc_df,net_migration_state, by = "GEOID")

fcc_pop_sf <- sf::st_as_sf(fcc_pop_joined)

ggplot() +
  geom_sf(data = fcc_pop_sf, aes(fill = bsl_grp, color = bsl_grp), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  geom_sf_label(data = state_overlay, aes(label = STUSPS)) +
  scale_fill_brewer(palette = "PuOr", direction = 1) +
  scale_color_brewer(palette = "PuOr", direction = 1, guide = "none") +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  labs(title = "Percent change in Broadband Serviceable Locations",
       subtitle = "FCC Broadband Data Collection, December vs June '23",
       fill = "% Change",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "",
       y = "")

#########################################################################

# STEP 4:  Create change in FTTH FCC locations by state, June-to-Dec '23

bsl_chg_order_ftth = c("20%+", "15% to 20%", "10% to 15%", "5% to 10%", "0% to 5%", "-5% to 0%", "Below -5%")

fcc_df_ftth <- read_csv('decjun2023_ftth.csv') %>% 
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(bsl_grp = case_when(
    PCT_CHG_BSL  >= 0.20 ~ "20%+",
    PCT_CHG_BSL  >= 0.15 & PCT_CHG_BSL < 0.20 ~ "15% to 20%",
    PCT_CHG_BSL  >= 0.10 & PCT_CHG_BSL < 0.15 ~ "10% to 15%",
    PCT_CHG_BSL  >= 0.05 & PCT_CHG_BSL < 0.10 ~ "5% to 10%",
    PCT_CHG_BSL  >= 0 & PCT_CHG_BSL < 0.05 ~ "0% to 5%",
    PCT_CHG_BSL  >= -0.05 & PCT_CHG_BSL < 0.0 ~ "-5% to 0%",
    PCT_CHG_BSL  < -0.05 ~ "Below -5%",
    TRUE ~ "Below -5%"
  )) %>%
  mutate(bsl_grp = factor(bsl_grp, levels = bsl_chg_order_ftth))

fcc_pop_joined_ftth <- inner_join(fcc_df_ftth,net_migration_state, by = "GEOID")

fcc_pop_ftth_sf <- sf::st_as_sf(fcc_pop_joined_ftth)

ggplot() +
  geom_sf(data = fcc_pop_ftth_sf, aes(fill = bsl_grp, color = bsl_grp), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  geom_sf_label(data = state_overlay, aes(label = STUSPS)) +
  scale_fill_brewer(palette = "PuOr", direction = 1) +
  scale_color_brewer(palette = "PuOr", direction = 1, guide = "none") +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  labs(title = "Percent change in FTTH Broadband Serviceable Locations",
       subtitle = "FCC Broadband Data Collection, December vs June '23",
       fill = "% Change",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "",
       y = "")


#########################################################################

# STEP 5:  Create change in housing units, 2010 to 2020
options(tigris_use_cache = TRUE)

housing_states_20 <- get_decennial(
  geography = "state",
  variables = "H1_001N",
  # state = "OK",
  # county = "Cimarron",
  year = 2020,
  sumfile = "pl",
  geometry = TRUE,
  resolution = "20m"
  ) %>%
  shift_geometry()

housing_20_df <- data.frame(housing_states_20) %>% filter(GEOID!="72")
# Load variables for the 2010 decennial census, summary file PL
# variables_2010 <- load_variables(2010, "pl", cache = TRUE)

# Search for total households in the variables data frame
# View(variables_2010)

# Inspect variables related to households

housing_states_10 <- get_decennial(
  geography = "state",
  variables = "H001001",
  # state = "OK",
  # county = "Cimarron",
  year = 2010,
  sumfile = "pl",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()

housing_10_df <- data.frame(housing_states_10) %>% filter(GEOID!="72")
# housing_states_10 %>% summarise(total = sum(HU_10))

housing_states_join_df <- left_join(housing_20_df,housing_10_df, by = "GEOID") %>%
  select(GEOID,NAME=NAME.x,HU_20=value.x,HU_10=value.y,gemometry=geometry.x) %>%
  mutate(CHG = HU_20-HU_10,
         RATE = CHG/HU_10)

housing_states_join_sf <- st_as_sf(housing_states_join_df)

hu_chg_order = c("10%+", "8% to 10%", "6% to 8%", "4% to 6%", "2% to 4%", "0% to 2%", "-1% to 0%", "-2% to 1%", "Below -2%")

hu_df <- housing_states_join_sf %>%
  mutate(hu_grp = case_when(
    RATE  >= 0.10 ~ "10%+",
    RATE  >= 0.08 & RATE < 0.10 ~ "8% to 10%",
    RATE  >= 0.06 & RATE < 0.08 ~ "6% to 8%",
    RATE  >= 0.04 & RATE < 0.06 ~ "4% to 6%",
    RATE  >= 0.02 & RATE < 0.04 ~ "2% to 4%",
    RATE  >= 0.0 & RATE < 0.02 ~ "0% to 2%",
    RATE  >= -0.01 & RATE < 0 ~ "-1% to 0%",
    RATE  >= -0.02 & RATE < -0.01 ~ "-2% to 1%",
    RATE  < -0.02 ~ "Below -2%",
    TRUE ~ "Below -2%"
  )) %>%
  mutate(hu_grp = factor(hu_grp, levels = hu_chg_order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

hu_df_sf <- sf::st_as_sf(hu_df)

ggplot() +
  geom_sf(data = hu_df_sf, aes(fill = hu_grp, color = hu_grp), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  geom_sf_label(data = state_overlay, aes(label = STUSPS)) +
  scale_fill_brewer(palette = "PuOr", direction = 1) +
  scale_color_brewer(palette = "PuOr", direction = 1, guide = "none") +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  labs(title = "Percent change in Total Housing Units",
       subtitle = "US Census Bureau, 2010 vs 2020",
       fill = "% Change",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "",
       y = "") 


#########################################################################

# STEP 6:  Origin/Destination migration: 2016-2020
options(tigris_use_cache = TRUE)

denver_flows <- get_flows(
  geography = "metropolitan statistical area",
  # msa = 38060,
  # msa = 35300,  # Denver MSA
  msa = 45540,
  # state = "SC",
  year = 2020
  # geometry = TRUE
)

# phx_flows %>% 
#   head()

# library(mapdeck)
# library(mapboxapi)
# key <- mb_access_token("pk.eyJ1IjoiZ3JhaW5ncCIsImEiOiJjbHhsOGprazgwMXkxMmlvZWo1eDJuODhlIn0.d-YeX_a4RrHj7BSVQmNE7g", overwrite = TRUE)

top_move_in_denver <- denver_flows %>% 
  filter(!is.na(GEOID2), variable == "MOVEDIN") %>% 
  slice_max(n = 10, order_by = estimate) %>%
  mutate(
    width = estimate / 500,
    tooltip = paste0(
      scales::comma(estimate * 5, 1),
      " people moved from ", str_remove(FULL2_NAME, "Metro Area"),
      " to ", str_remove(FULL1_NAME, "Metro Area"), " between 2016 and 2020"
    )
  )

top_move_in_denver

ggplot(top_move_in_denver, aes(x = reorder(FULL2_NAME, estimate*5), y = estimate*5)) +
  geom_segment(aes(x = reorder(FULL2_NAME, estimate*5),
                   xend = reorder(FULL2_NAME, estimate*5),
                   y = 0, yend = estimate*5),
               color = "gray", lwd = 1) +
  geom_point(size = 16, pch = 21, bg = 4, col = 1) +
  geom_text(aes(label = estimate*5), color = "white", size = 4, fontface = "bold") +
  labs(title = "Most common origin for people moving to The Villages, FL MSA",
       subtitle = "US Census Bureau, 2016-2020",
       # fill = "% Change",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "Origin",
       y = "People Moving In") +
  coord_flip() +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0, size = 18, family = "Roboto", face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 14, family = "Roboto"),
    plot.caption = element_text(hjust = 0, size = 12, family = "Roboto"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )


#########################################################################

# STEP 7:  Origin/Destination migration: 2016-2020
options(scipen = 999)

summary_stats <- read_csv('summary_stats.csv') %>%
  pivot_longer(cols = CURRENT_BSL:PCT_SATELLITE_CHG,
               names_to = "METRIC",
               values_to = "VALUE")

summary_stats_chg <- summary_stats %>% 
  filter(METRIC %in% c("BSL_CHG","FIBER_CHG","COAX_CHG","COPPER_CHG","FWA_CHG","SATELLITE_CHG")) %>%
  mutate(METRIC = recode(METRIC,
                         "BSL_CHG" = "TOTAL BSL",
                         "FIBER_CHG" = "FTTH BSL",
                         "COAX_CHG" = "COAX BSL",
                         "COPPER_CHG" = "COPPER BSL",
                         "FWA_CHG" = "FWA BSL",
                         "SATELLITE_CHG" = "SATELLITE BSL"
                         ))

library(forcats)
# Reorder the factor levels
summary_stats_order <- summary_stats_chg %>%
  mutate(METRIC = fct_relevel(METRIC, "TOTAL BSL", "FTTH BSL", "COAX BSL", "COPPER BSL", "FWA BSL", "SATELLITE BSL"))

# Reverse the order of the factor levels
summary_stats_reorder <- summary_stats_order %>%
  mutate(METRIC = fct_relevel(METRIC, rev(levels(METRIC))))

# Create the bar chart with reordered metrics
ggplot(summary_stats_reorder, aes(x = METRIC, y = VALUE, fill = METRIC)) +
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_text(aes(label = scales::comma(VALUE)), vjust = 0, size = 4) +
  scale_y_continuous(labels = scales::comma) +  # Format the y-axis labels
  scale_fill_manual(values = c("TOTAL BSL" = "green", "FTTH BSL" = "green", "COAX BSL" = "green", "COPPER BSL" = "red", "FWA BSL" = "green", "SATELLITE BSL" = "green") ) +
  labs(title = "Change in FCC Broadband Serviceable Locations by Technology",
       subtitle = "FCC Broadband Data Collection, December vs June '23",
       # fill = "% Change",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "",
       y = "Broadband Serviceable Locations (BSL)") +
  coord_flip() +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0, size = 18, family = "Roboto", face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 14, family = "Roboto"),
    plot.caption = element_text(hjust = 1, vjust = -1, size = 12, family = "Roboto"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "none",
    axis.text.x = element_blank()
  )

#########################################################################

# STEP 8:  Create change in housing units, 2010 to 2020
options(tigris_use_cache = TRUE)

pop_states_20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  # state = "OK",
  # county = "Cimarron",
  year = 2020,
  sumfile = "pl",
  # geometry = TRUE,
  resolution = "20m"
) 

pop_20_df <- data.frame(pop_states_20) %>% filter(GEOID!="72")
# Load variables for the 2010 decennial census, summary file PL
# variables_2010 <- load_variables(2010, "pl", cache = TRUE)
# View(variables_2010)
# Search for total households in the variables data frame
# View(variables_2010)

# Inspect variables related to households

pop_states_10 <- get_decennial(
  geography = "state",
  variables = "P001001",
  # state = "OK",
  # county = "Cimarron",
  year = 2010,
  sumfile = "pl",
  # geometry = TRUE,
  resolution = "20m"
) 

pop_10_df <- data.frame(pop_states_10) %>% filter(GEOID!="72")

pop_states_joined <- inner_join(pop_states_20, pop_states_10, by ="GEOID") 

pop_states_stats <- pop_states_joined %>%
  mutate(POP_20 = value.x,
         POP_10 = value.y,
         POP_CHG = POP_20-POP_10,
         POP_RATE = (POP_20-POP_10)/POP_10
         ) %>%
  select(GEOID,NAME=NAME.x,POP_20,POP_10,POP_CHG,POP_RATE)

pop_hu_model_data <- inner_join(pop_states_stats,housing_states_join_df, by = "GEOID") %>%
   select(GEOID,NAME.x,POP_20,POP_10,POP_CHG,POP_RATE,HU_20,HU_10,HU_CHG=CHG,HU_RATE=RATE)

library(ggrepel)
# Create the scatter plot with a regression line
ggplot(pop_hu_model_data, aes(x = POP_RATE, y = HU_RATE)) +
  geom_point() +  # Plot the bivariate points
  geom_smooth(method = "lm", se = FALSE) +  # Add a regression line
  geom_text_repel(aes(label = NAME.x), size = 3) +  # Add state labels with repelling
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +  # Scale x-axis as percentage
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Scale y-axis as percentage
  theme_minimal(base_family = "Roboto", base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0, size = 18, family = "Roboto", face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 14, family = "Roboto"),
    plot.caption = element_text(hjust = 1, vjust = -1, size = 12, family = "Roboto"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "none",
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),  # Reduce x-axis label font size
    axis.text.y = element_text(size = 10)  # Reduce y-axis label font size
  ) +
  labs(title = "Population vs Housing Unit Change Over the Last Decade (2010 to 2020)",
       subtitle = "US Census Bureau",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "Percentage change in population",
       y = "Percentage change in housing")

#########################################################################

# STEP 9:  Correlation

library(corrplot) 
library(RColorBrewer) 

scaled_df <- as.data.frame(pop_hu_model_data) %>% 
  select_if(is.numeric) %>%
  scale()

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

#########################################################################

# STEP 10:  Housing Units per Lane Mile

hu_road_mile <- read_csv('hu_roadmile.csv') 

overall_housing_units_per_road_mile = 140498736 / 8757498


# Create the bar chart
ggplot(hu_road_mile, aes(x = reorder(USPS_STATE, HU_LANE_MILE), y = HU_LANE_MILE)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  geom_text(aes(label = scales::comma(HU_LANE_MILE)), hjust = -1, size = 4) +
  geom_hline(yintercept = overall_housing_units_per_road_mile, linetype = "solid", color = "red", size = 1) +  # Add the horizontal dotted line
  coord_flip() +  # Flip the coordinates to make it horizontal
  labs(title = "Housing Units per Lane Mile",
       subtitle = "US Census Bureau",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "State",
       y = "Housing units per lane mile") +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0),      # Title font size and bold
    axis.title.x = element_text(size = 16),                  # X-axis title font size
    axis.title.y = element_text(size = 16,margin = margin(r = 10)),                  # Y-axis title font size
    axis.text.x = element_text(size = 14),                   # X-axis text font size
    axis.text.y = element_text(size = 12),                        # Increase the left margin (top, right, bottom, left)
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  annotate("text", 
           x = length(hu_road_mile$USPS_STATE), 
           y = overall_housing_units_per_road_mile, 
           label = paste0("Overall: ", round(overall_housing_units_per_road_mile, 1)), 
           color = "red", hjust = -0.1, vjust = 50, fontface = "bold")  # Annotate the horizontal line with the overall value

#########################################################################

# STEP 11:  Average providers per Census Tract

hu_road_mile <- read_csv('hu_roadmile.csv') 

overall_competitors_by_tract = 8


# Create the bar chart
ggplot(hu_road_mile, aes(x = reorder(USPS_STATE, MEDIAN_COMPETITORS_BY_TRACT), y = MEDIAN_COMPETITORS_BY_TRACT)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  geom_text(aes(label = scales::comma(MEDIAN_COMPETITORS_BY_TRACT)), hjust = -1, size = 4) +
  geom_hline(yintercept = overall_competitors_by_tract, linetype = "solid", color = "red", size = 1) +  # Add the horizontal dotted line
  coord_flip() +  # Flip the coordinates to make it horizontal
  labs(title = "Median Competitors per Census Tract",
       subtitle = "FCC Broadband Data Collection, Dec. '23 (latest vintage)",
       caption = "Grain Portfolio Dynamics Group | cbailey@graingp.com",
       x = "State",
       y = "Median competitors per Census Tract") +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0),      # Title font size and bold
    axis.title.x = element_text(size = 16),                  # X-axis title font size
    axis.title.y = element_text(size = 16,margin = margin(r = 10)),                  # Y-axis title font size
    axis.text.x = element_text(size = 14),                   # X-axis text font size
    axis.text.y = element_text(size = 12),                        # Increase the left margin (top, right, bottom, left)
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  annotate("text", 
           x = length(hu_road_mile$MEDIAN_COMPETITORS_BY_TRACT), 
           y = overall_competitors_by_tract, 
           label = paste0("Overall: ", round(overall_competitors_by_tract, 1)), 
           color = "red", hjust = -0.5, vjust = 52, fontface = "bold")  # Annotate the horizontal line with the overall value


#########################################################################

speed_df <- read_csv('speed.csv')

summary(speed_df)

ggplot(speed_df, aes(x = CURRENT_STATE, y = MAX_ADVERTISED_DOWNLOAD_SPEED, color = CURRENT_STATE)) +
  geom_boxplot() +
  scale_color_manual(values = c("#0099f8", "#e74c3c", "#2ecc71")) +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0),      # Title font size and bold
    axis.title.x = element_text(size = 16),                  # X-axis title font size
    axis.title.y = element_text(size = 16,margin = margin(r = 10)),                  # Y-axis title font size
    axis.text.x = element_text(size = 14),                   # X-axis text font size
    axis.text.y = element_text(size = 12),                        # Increase the left margin (top, right, bottom, left)
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  coord_flip()






















































# Ensure STATE is a character to reorder and then convert to factor
fcc_pop_sf$CURRENT_STATE <- as.character(fcc_pop_sf$CURRENT_STATE)
fcc_pop_sf$CURRENT_STATE <- factor(fcc_pop_sf$CURRENT_STATE, levels = fcc_pop_sf$CURRENT_STATE[order(fcc_pop_sf$value, decreasing = TRUE)])

# Ensure GROWTH_RATE_1 and GROWTH_RATE_2 are numeric
fcc_pop_sf$value <- as.numeric(fcc_pop_sf$value)
fcc_pop_sf$PCT_CHG_BSL <- as.numeric(fcc_pop_sf$PCT_CHG_BSL)

# Convert GROWTH_RATE_2 to a proportion for plotting
fcc_pop_sf$PCT_CHG_BSL <- fcc_pop_sf$PCT_CHG_BSL * 100

ggplot(fcc_pop_sf, aes(x = CURRENT_STATE)) +
  geom_bar(aes(y = value), stat = "identity", fill = "skyblue") +
  geom_line(aes(y = PCT_CHG_BSL * max(PCT_CHG_BSL), group = 1), color = "red", size = 1) +
  geom_point(aes(y = PCT_CHG_BSL * max(PCT_CHG_BSL)), color = "red", size = 2) +
  scale_y_continuous(
    name = "Growth Rate 1",
    sec.axis = sec_axis(~./max(fcc_pop_sf$PCT_CHG_BSL), name = "Growth Rate 2 (%)")
  ) +
  theme_minimal() +
  labs(title = "Dual-Axis Chart: Growth Rate 1 and Growth Rate 2") +
  coord_flip()



# Filter and sort the data to get the top 15 counties
top_15_counties <- net_migration_cnty %>%
  arrange(desc(value)) %>%
  slice(1:15)

# Create the bar chart
ggplot(top_15_counties, aes(x = reorder(NAME, value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip the coordinates to make it horizontal
  labs(title = "Top 15 counties by 2022/2023 net migration per 1000 residents",
       x = "",
       y = "Net migration per 1000 residents") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0),      # Title font size and bold
    axis.title.x = element_text(size = 16),                  # X-axis title font size
    axis.title.y = element_text(size = 16),                  # Y-axis title font size
    axis.text.x = element_text(size = 14),                   # X-axis text font size
    axis.text.y = element_text(size = 14),                    # Y-axis text font size
    plot.margin = unit(c(1, 1, 1, 2), "cm")                          # Increase the left margin (top, right, bottom, left)
  )


population <- sf::st_drop_geometry(net_migration)

write.csv(population, file = "population.csv")



View(top_move_in)

# top_move_in %>% 
#   mapdeck(token = key, style = mapdeck_style("satellite-streets"), pitch = 45) %>% 
#   add_arc(
#     origin = "centroid1",
#     destination = "centroid2",
#     stroke_width = "width",
#     auto_highlight = TRUE,
#     highlight_colour = "#8c43facc",
#     tooltip = "tooltip"
#   )