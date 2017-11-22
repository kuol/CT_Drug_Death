library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Death Count by City", min(dff$death_count), max(dff$death_count),
                            value = range(dff$death_count), step = 5
                )
                # selectInput("colors", "Color Scheme",
                #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                # )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    dff[dff$death_count >= input$range[1] & dff$death_count <= input$range[2],]
  })
  
  # # This reactive expression represents the palette function,
  # # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })

  pal <- colorNumeric("YlGn", c(0,1000))
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(conn) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "#BDBDC3", weight = 1, 
                  fillColor = ~pal(death_count), fillOpacity = 0.5,
                  popup = county_popup) 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addMarkers(lng = filteredData()$long, lat = filteredData()$lat, 
                 popup = paste(filteredData()$city, filteredData()$death_count, sep = ': '))
  })
} 

shinyApp(ui, server)