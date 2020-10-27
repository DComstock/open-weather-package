#' An S4 class which holds weather information for cities
#'
#' Identifies cities within radius and obtains the weather
#'
#'@name weather_class-class
#'@slot primary_city city being referenced (if provided)
#'@slot state state to which the city belongs
#'@slot zip zipcode of the city (if provided)
#'@slot latitude latitude of the city
#'@slot longitude longitude of the city
#'@slot radius_ref radius in miles
#'@slot cities_within_radius dataframe which saves the information about primary city and cities within the radius
#'@slot forecast_data dataframe which holds forecast data for cities
#'@slot current_data dataframe which holds the current weather data
#'@slot twitter_data dataframe holds the number of tweets for each city by date
weather_class <- setClass("weather_class",
                          representation(
                            primary_city = "character",
                            state = "character",
                            zip = "character",
                            latitude = "numeric",
                            longitude = "numeric",
                            radius_ref = "numeric",
                            cities_within_radius = "data.frame",  # list of cities within the radius
                            forecast_data = "data.frame",
                            current_data = "data.frame",
                            twitter_data = "data.frame"
                          )

                          ### still need to define the validation function
                          # validity <- function(object){
                          #
                          # } # end of validity function

) # end of weather class

#' Display data
#' @rdname weather_class-class
setGeneric(name = "show",
           def = function(object) {standardGeneric("show")}
)

#' Display the data within the class
#' @aliases show
#' @rdname weather_class-class
#' @param object S4 object of weather_class
setMethod(f = "show",
          signature = "weather_class",
          definition = function(object){
            cat("Primary City name: ", object@primary_city,
                "\nPrimary City zipcode: ", object@zip,
                "\nState: ", object@state,
                "\nlatitude: ", object@latitude,
                "\nlongitude: ", object@longitude,
                "\nRadius: ", object@radius_ref)
            print(object@cities_within_radius)
            print(object@forecast_data)
            print(object@current_data)
          })

#' Subset data for zipcodes of choice
# @rdname weather_class-class
#' setGeneric(name = "[",
#'            def = function(object) {standardGeneric("[")}
#' )

# Subset data to display information pertaining only to zipcode of choice
# @aliases [
# @rdname weather_class-class
# @return returns S4 object of 'weather_class' with information pertaining only to selected zipcode
# @param z zipcode of choice for subsetting the data
# setMethod(f = "[",
#           signature = "weather_class",
#           definition = function(object, z){
#
#           return(new_obj)
#           }
# )

#' save list of cities within proximity to S4 class
#' @rdname weather_class-class
setGeneric(name = "save_cities",
           def = function(object,df){standardGeneric("save_cities")}
)

#' save list of cities within proximity to S4 class
#' @rdname weather_class-class
# @name save_cities
# @aliases  save_cities
# @param object S4 object of class weather_class
#' @param df list of cities within radius 'r' of primary city; returned from get_cities
setMethod(f = "save_cities",
          signature = "weather_class",
          definition = function(object, df){
            object@cities_within_radius <- df
            return(object)
          }
)

#' saves information about the primary city into S4 object
#' @rdname weather_class-class
setGeneric(name = "save_primary_city",
           def = function(object,df){standardGeneric("save_primary_city")}
)

#' saves information about the primary city into S4 object
#' @rdname weather_class-class
# @name save_primary_city
# @aliases save_primary_city
# @param object S4 object of class weather_class,
# @param df dataframe with information pertaining to primary city
setMethod(f = "save_primary_city",
          signature = "weather_class",
          definition = function(object, df){
            object@primary_city <- df$city
            object@zip <- df$zip
            object@state <- df$state
            object@latitude <- df$latitude
            object@longitude <- df$longitude
            object@radius_ref <- df$radius
            return(object)
          }
)

#' plots the information obtained from weather forecast
#' @rdname weather_class-class
setGeneric(name = "weather_plot_forecasts",
           def = function(object) {standardGeneric("weather_plot_forecasts")}
)

#' plots the information obtained from weather forecast
#' @rdname weather_class-class
# @name plot_forecasts
# @aliases plot_forecasts, weather_class
# @param object S4 object of 'weather_class' containg
#' @export
setMethod(f  = "weather_plot_forecasts",
          signature = "weather_class",
          definition = function(object){

            temp_df_plot <- object@forecast_data
            # assumes that there is a field 'time_stamp' in the forecast data
            temp_df_plot$time_stamp <- lubridate::ymd_hms(temp_df_plot$Time)

       plot_method  <-    temp_df_plot %>%
              dplyr::select(-c(Temp_max, Temp_min, Description)) %>%
              tidyr::gather(key = weather_variable, value = weather_value, -c(Time, City:zip, time_stamp)) %>%
              ggplot2::ggplot(ggplot2::aes(x = weather_value)) +
              ggplot2::geom_density(ggplot2::aes(fill = zip), alpha = 0.2) +
              ggplot2::facet_wrap(~weather_variable, scales = "free") +
              ggplot2::theme_minimal() +
              ggplot2::labs(x = "", y = "Frequency (%)") +
              ggplot2::scale_y_continuous(labels = scales::percent) +
              ggplot2::theme(legend.position = "bottom")


       #suppressWarnings(plot_method)
       base::suppressMessages(plot_method)

          })

