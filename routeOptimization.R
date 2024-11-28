# install.packages(c("shiny", "leaflet", "leaflet.extras", "sf", "osrm", "tidyverse"))

# library(shiny)
# library(leaflet)
# library(osrm)
# library(sf)
# library(tidyverse)



library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(osrm)
library(tidyverse)

ui <- fluidPage(
  textInput("origin", "Origin"),
  textInput("destination", "Destination"),
  actionButton("get_route", "Get Route"),
  leafletOutput("map")
)

server <- function(input, output) {
  observeEvent(input$get_route, {
    origin <- input$origin
    destination <- input$destination
    
    # Construct the API request URL with your API key and parameters
    api_url <- paste0("https://api.openrouteservice.org/v2/directions/driving-car?api_key=5b3ce3597851110001cf62486f51a98d14094d04bf63c72e64bd4da3&start=",
                      origin, "&end=", destination)
    
    # Make GET request to OpenRouteService API
    api_response <- GET(api_url)
    
    # Process API response (parse JSON, extract route coordinates)
    if (http_error(api_response)) {
      status_code <- status_code(api_response)
      content <- content(api_response, "text")
      showNotification(paste("Error:", status_code, content), type = "error")
      return(NULL)
    }
    
    route_data <- content(api_response, as = "parsed")
    coordinates <- route_data$features[[1]]$geometry$coordinates
    
    # Extract coordinates for plotting the route
    coordinates <- lapply(coordinates, function(coord) {
      rev(coord[c(1, 2)])  # Reverse the coordinates to match Leaflet's expected format
    })
    
    # Use leaflet to plot the route on the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolylines(data = coordinates)  # Plot the route using addPolylines
    })
  })
}

shinyApp(ui, server)
