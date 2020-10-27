#' Generate a collection of forecasts
#' 
#' This loop calls get_set for each zip code in the city_list generated 
#' by get_city_list or a vector of Zip codes created by the user directly.
#' A column is added to the get_set return to log the corresponding Zip code.
#' If info on a zip code in the list is not available (DNE - 404) 
#' it is simply left out and prints a message to notify you that there
#' is no data available for that Zip code.
#'
#' @return returns a single data frame 
#' @param zip_list vector containing Zip codes for all desired cities.
#' @param weatherAPIkey your openweathermap.org API key
# #' @examples
# #' zL <- c("35071", "35039", "35205")
# #' fL <- collect_forecasts(zip_list = zL, weatherAPIkey = weather_api)


collect_forecasts <- function(zip_list, weatherAPIkey){ 
  forecast_list <- NULL
  for (z in zip_list){
    tryCatch({
      tmp_FC <- get_set(zip_code =  z, weatherAPIkey = weatherAPIkey) 
      tmp_FC$zip <- z
      forecast_list <- rbind(forecast_list, tmp_FC)
    }, error=function(e){print(paste0("No Data available for ", z))})
  }
  return(forecast_list)
}
