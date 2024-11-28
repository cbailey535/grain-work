library(tidycensus)
library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE)

us_components <- get_estimates(geography = "state", product = "components", vintage = 2023)

us_components

net_migration <- get_estimates(geography = "county",
                               variables = "RNETMIG",
                               vintage = 2023,
                               geometry = TRUE,
                               resolution = "20m") %>%
                              shift_geometry()

net_migration

library(showtext)
font_add_google("Roboto")
showtext_auto()

order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration <- net_migration %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = "none") +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  labs(title = "2022/2023 net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2023 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | cbailey@graingp.com")



# Filter and sort the data to get the top 15 counties
top_15_counties <- net_migration %>%
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



phx_flows <- get_flows(
  geography = "metropolitan statistical area",
  msa = 38060,
  year = 2018,
  geometry = TRUE
)

phx_flows %>% 
  head()

library(mapdeck)
library(mapboxapi)
key <- mb_access_token("pk.eyJ1IjoiZ3JhaW5ncCIsImEiOiJjbHhsOGprazgwMXkxMmlvZWo1eDJuODhlIn0.d-YeX_a4RrHj7BSVQmNE7g", overwrite = TRUE)

top_move_in <- phx_flows %>% 
  filter(!is.na(GEOID2), variable == "MOVEDIN") %>% 
  slice_max(n = 25, order_by = estimate) %>% 
  mutate(
    width = estimate / 500,
    tooltip = paste0(
      scales::comma(estimate * 5, 1),
      " people moved from ", str_remove(FULL2_NAME, "Metro Area"),
      " to ", str_remove(FULL1_NAME, "Metro Area"), " between 2014 and 2018"
    )
  )

top_move_in %>% 
  mapdeck(token = key, style = mapdeck_style("satellite-streets"), pitch = 45) %>% 
  add_arc(
    origin = "centroid1",
    destination = "centroid2",
    stroke_width = "width",
    auto_highlight = TRUE,
    highlight_colour = "#8c43facc",
    tooltip = "tooltip"
  )