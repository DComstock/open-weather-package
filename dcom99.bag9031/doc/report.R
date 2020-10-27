## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, results='hide'-----------------------------------------
library(dcom99.bag9031)
setup_weather_api("d130afd947394f43508c64f78b8e9ccd")

## ------------------------------------------------------------------------
library(dcom99.bag9031)
#a <- get_weather(94305, weather_api)
#b <- get_weather(94305, weather_api, radius = 10)
#or
#setup_weather_api("your_API_here")
a <- get_weather(94305)
b <- get_weather(94305, radius = 10)

a@forecast_data

## ------------------------------------------------------------------------
slotNames(b)
b@primary_city
b@state
b@zip
b@latitude
b@longitude
b@cities_within_radius
a@forecast_data

## ---- eval=TRUE----------------------------------------------------------
sd <- sampleStanford10mile
sd@cities_within_radius

## ------------------------------------------------------------------------
display_zipcodes(b)

## ---- eval=FALSE---------------------------------------------------------
#  plot_forecast(b)
#  

## ----fig.width= 5, fig.height= 6-----------------------------------------
weather_plot_forecasts(b)

## ----eval = FALSE--------------------------------------------------------
#  call_twitter(b, num_tweets = 30,rtweets_token = rtweets_token )

