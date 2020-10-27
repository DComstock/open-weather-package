#' Function to display the locations of the selected citie(s)
#'
#' @param weather_object S4 object obtained from get_cities and get_current_weather function
#' @return returns a plot with zipcodes plotted on th map
#' @export
display_zipcodes <- function(weather_object){

  if(is.null(weather_object@forecast_data) & is.null(weather_object@cities_within_radius))
    stop("No data in the object. Please run get_cities function")

temp_plot_df <- weather_object@cities_within_radius

plot_lft <- leaflet::leaflet(temp_plot_df) %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(lat = ~latitude, lng = ~longitude,
                            #label = ~ htmlEscape(Temperature),
                            #label = ~as.character(Temperature),
                            labelOptions = leaflet::labelOptions(noHide = T))

return(plot_lft)
}



#' Function to plot forecast data
#' @param weather_object S4 object obtained from get_cities and get_current_weather function
# @param typ specific weather element to plot
#' @export
plot_forecast <- function(weather_object){

  typ_list <- c("Temperature", "Humidity", "Wind_Speed", "Sea_Level")

  if (is.null(weather_object@forecast_data)) stop("No Forecast data in the object. Call get_forecast")

  temp_forecast_df <- weather_object@forecast_data

  #temp_stanf_data <- readRDS("/Users/gopakumb/Documents/Personal/Stanford 290/sampleStanford10mile.rds")
  # convert into date time
  temp_forecast_df$Time <- lubridate::ymd_hms(temp_forecast_df$Time)

  p1 <- g_plot(temp_forecast_df, "Temperature")
  p1_1 <- g_plot(temp_forecast_df, "Temperature", "none")
  p2 <- g_plot(temp_forecast_df, "Humidity", "none")
  p3 <- g_plot(temp_forecast_df, "Wind_Speed", "none")
  p4 <- g_plot(temp_forecast_df, "Sea_Level", "none")

  # extracts legend in order shared legend
  mylegend <- legend_extract(p1)

  p_all <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p1_1, p2, p3, p4, nrow = 2), mylegend, nrow = 2, heights=c(10, 1))


  }


#' plot function
#' @param df data frame containing forecasted weather for selected cities
#' @param typ_w weather element to display
#' @param display_legend toggle disply of legend
#' @return returns plot of forecast data
g_plot <- function(df, typ_w, display_legend = "bottom"){

    p_temp <- NULL

    var_1 <- as.symbol(typ_w)

    # modifying the display for y axis
    plot_label <- dplyr::case_when(typ_w == "Wind_Speed" ~ "Wind Speed",
                                   typ_w == "Sea_Level" ~ "Sea_Level",
                                 TRUE ~ typ_w)
    plot_label <- unique(plot_label)

  p_temp <-  df %>%
    dplyr::select(Time, zip, typ_w)  %>%
    ggplot2::ggplot(ggplot2::aes( Time, y = !!var_1)) +
    ggplot2::geom_line(ggplot2::aes(color = as.factor(zip))) +
    ggplot2::geom_point(fill = "darkblue") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = display_legend, legend.title = ggplot2::element_blank()) +
    ggplot2::labs(x = "", y = plot_label)


  return(p_temp)
}




#' helper function to extract legend
#' @param plot_a a plot from which extract legend
#' @return returns legend from the plot

legend_extract <- function(plot_a){

  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot_a))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]

  return(legend)
}
