#' Generate forecast data set
#' 
#' This function will get the set of forecast data for a single 
#' specified Zip code using your openweathermap.org API key.
#' @return returns a data frame
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr '%>%'
#' @param zip_code zipcode for the desired city
#' @param weatherAPIkey your openweathermap.org API key
#' @examples \donttest{
#' get_set(zip_code = "94305", weatherAPIkey = "your_api_key")
#' }
#' @export

get_set <-  function(zip_code=zip_code, weatherAPIkey=weather_api){
  options(stringsAsFactors=FALSE)
  #create api call for forecast and construct table
  weatherCall <- weather_call(zip_code=zip_code, weatherAPIkey=weatherAPIkey)
  #Fetch forecast data for zip
  weatherRaw <- jsonlite::fromJSON(weatherCall)
  #pull description from layers
  weatherDesc <- do.call(rbind, lapply(weatherRaw$list$weather, function(x) data.frame(x$description)))
  #Build table
  Forecast <- cbind(weatherRaw$list$dt_txt, weatherRaw$list$main,
                    weatherRaw$list$wind,  weatherRaw$list$clouds,
                    weatherDesc, weatherRaw$city$name,weatherRaw$city$coord$lat,
                    weatherRaw$city$coord$lon) %>%
    tibble::as_tibble() %>%
    `colnames<-`(c("Time", "Temperature", "Temp_min", "Temp_max", "Pressure",
                   "Sea_Level", "Ground_Level", "Humidity", "temp_kf", "Wind_Speed",
                   "Wind_Direction", "Cloudcover", "Description", "City",
                   "Latitude", "Longitude")) 
  
  Forecast <- Forecast %>% 
    dplyr::mutate(`rain3h` = ifelse(is.null(weatherRaw$list$rain),0, weatherRaw$list$rain),
                  zip = zip_code)
  
  return(Forecast)
}
