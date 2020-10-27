#' function will create a list of cities around a K mile radius of the specified location
#' @return returns a S4 object with the primary city and list of cities within the specified radius
#' @import dplyr
#' @import ZipRadius
#' @param zip_code zipcode for the city in reference
#' @param radius radius around the primary city of reference
#' @examples
#' get_city_list(zip_code = "10016", radius = 5)
#' @export

get_city_list <- function( zip_code = NULL, radius = 5){
  #have to add and NULL variables to appease R CMD check
  Distance <- NULL
  zip <- NULL
  state <- NULL
  city <- NULL
  latitude <- NULL
  longitude <- NULL
  
 city_list <- ZipRadius::zipRadius(zipcode = zip_code, radius = radius) %>%
                dplyr::as_tibble()
 
 primary_city_info <-   city_list %>% 
    dplyr::filter(Distance == 0) %>% 
   as.data.frame() #%>% 
  # dplyr::select(zip, city, state, latitude, longitude,radius)
 
 primary_city_info$radius <- radius
 
 temp_weather_class <- weather_class()
   
 temp_weather_class <-  save_primary_city(temp_weather_class, primary_city_info)
   
 temp_weather_class <- save_cities(temp_weather_class, city_list)
   
   return(temp_weather_class)
}
