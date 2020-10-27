#' Generate forecast data set
#'
#' This function when used without the third argument "radius"
#' will get the forecast data for a single specified Zip code using
#' your openweathermap.org API key by calling get_set. However, if you
#' specify a "radius" it will generate a "city_list" S4 object with the
#' primary city and those within the radius including the Zip code,
#' city, state, latitude, longitude, and Distance (from original Zip).
#' It will then loop through these Zip codes to create a full set of forecasts
#' for the cities within the specified radius.
#' 
#' *Must run setup_weather_api before running examples.*
#' @return returns a data frame
#' @param zip_code zipcode for the desired city
#' @param weatherAPIkey your openweathermap.org API key
#' @param radius (optional) radius centered on specified zip from which to collect data
#' @examples \donttest{
#' a <- get_weather(19901, weather_api)
#' b <- get_weather(zip_code = 94305, weatherAPIkey = weather_api, radius = 5)
#' c <- get_weather(94305, weather_api, 5)
#' }
#' @export

get_weather <- function(zip_code, weatherAPIkey = weather_api, radius){
  # initialize object of class 'weather_class'
  #if a radius is not specified get forecast
  if(missing(radius)){
    city_list <- weather_class()
    t_df <- get_set(zip_code = zip_code, weatherAPIkey = weatherAPIkey)
  } else {
    #If radius is specified build table from zips in radius
    city_list <- get_city_list(zip_code = zip_code, radius = radius)
    #Use collect_forecasts to pull forecasts.
    t_df <- collect_forecasts(zip_list = city_list@cities_within_radius$zip,
                                weatherAPIkey = weatherAPIkey)
      }
  city_list@forecast_data <- t_df
  return(city_list)
  }
