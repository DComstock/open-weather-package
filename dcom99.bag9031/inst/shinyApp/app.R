#' Weather View - Shiny App
#' Initial version - still in development!
#' @importFrom magrittr '%>%'
#' @usage shinyWeather()

ui <- shiny::navbarPage("Weather Viewer!",
                 shiny::sidebarLayout(
                   shiny::sidebarPanel(
                     shiny::textInput("zip", label = "Enter City Name/Zip",
                                  value = "07094"),
                        shiny::sliderInput("radius", "Radius:",
                                    min = 0, max = 10,
                                    value = 1),
                     shiny::submitButton("action", "Find Cities")
                      ),
                   shiny::mainPanel(
                        shiny::verbatimTextOutput("city_table"),
                        # verbatimTextOutput("sel_zip"),
                        # verbatimTextOutput("sel_radius")
                        leaflet::leafletOutput("mymap",height = 1000)
                      )
              )
)



server <- function(input, output,session) {
selected_data <- shiny::reactive({
  df_s4 <- dcom99.bag9031::get_weather( input$zip,weather_api,input$radius)
  df_s4@cities_within_radius
})

# output$city_table <- renderPrint({
#   df_s4 <- dcom99.bag9031::get_weather( input$zip,'1ed599affbde7b13a8ee11c6a792e23e',input$radius)
#   df_s4@cities_within_radius
#   })

output$city_table <- shiny::renderPrint({selected_data()})


output$mymap <- leaflet::renderLeaflet({
  plot_lft <- leaflet::leaflet(selected_data()) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lat = ~latitude, lng = ~longitude,
                              #label = ~ htmlEscape(Temperature),
                              #label = ~as.character(Temperature),
                              labelOptions = leaflet::labelOptions(noHide = T))
  plot_lft
})

}


# Run the application
shiny::shinyApp(ui = ui, server = server)
