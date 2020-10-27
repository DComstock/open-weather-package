#' This function runs the shiny app
#' 
#' @description Runs a shiny app for an example gui.
#' MUST FIRST RUN setup_weather_api() and store your api.
#' Currently the gui will show you a map with the zip codes within the specified
#' radius marked for visual reference of where your weather readings are coming 
#' from. Eventually more data can be worked into the displayed map.
#' @usage shinyWeather()
#' @importFrom  shiny runApp
#' @export

shinyWeather <- function(){
  shiny::runApp(system.file('shinyApp', package = "dcom99.bag9031"))
}