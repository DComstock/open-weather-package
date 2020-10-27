#' An OpenWeatherMap.org data retrieval/visualization tool
#' 
#' dcom99.bag9031 provides functions to retrieve data for a city based on Zip, or an area based on 
#' a Zip code and radius as well as some functions for visualizing and seeing what people
#' are saying about the weather on twitter in that area.
#' 
#' @section Detail:
#' \describe{
#' \code{dcom99.bag9031} is intended for use with a free openweathermap.org api. Before moving 
#' forward with the package it is useful to store your api using \code{setup_weather_api()}
#' Featured are functions to retrieve data for a city based on Zip, or an area based on 
#' a Zip code and radius: \code{get_weather}. As well as some functions for visualizing 
#' the data in different ways like \code{weather_plot_forecast} and \code{plot_forecast}.
#' Tools for seeing what people are saying about the weather on twitter in that area
#' \code{call_twitter}. You can even see where the data is coming from with 
#' \code{display_zipcodes}. To give a few examples. Sample output is included, 
#' and can be explored by assigning \code{sampleStanford10mile} to a variable.
#' }
#'
#'@section More:
#'\describe{
#' For a more complete understanding of dcom99.bag9031, see the the vignette:
#' `Vignette("Report", package = "dcom99.bag9031")`
#' }
#' 
#' @docType package
#' @name dcom99.bag9031
NULL