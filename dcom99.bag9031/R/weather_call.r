#' Generate weather API call
#' 
#' This function will create the API call for forecast data from/for
#' a single Zip code to be used in other functions. You could also 
#' paste the string into your browser to see the raw return if you 
#' so desire.
#'
#' @return returns a string 
#' @param zip_code Zip code for the desired city
#' @param weatherAPIkey your openweathermap.org API key
#' @examples \donttest{
#' weatherCall(zip_code = 94305, weatherAPIkey = "your_api_key")
#' weatherCall(94305, "your_api_key")
#' }
#' @export


weather_call <- function(zip_code, weatherAPIkey){
    paste0("https://api.openweathermap.org/data/2.5/forecast?zip=", zip_code,
                      "&APPID=", weatherAPIkey)}
