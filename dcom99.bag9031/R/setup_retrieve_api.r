#' Function to store api key
#'
#' This function takes your api key and saves it for convenience
#' @return returns a string containing the saved key
#' @param api_key your api key
#' @export
setup_weather_api <- function(api_key){
  weather_api <- NULL
    weather_api <<- api_key
}

retrieve_weather_api <- function() { return(weather_api)}