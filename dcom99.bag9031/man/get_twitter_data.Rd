% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allergy_flu_tweet_count.R
\name{get_twitter_data}
\alias{get_twitter_data}
\title{function which retrieves the tweets based on user provided latitude, longitude and search radius}
\usage{
get_twitter_data(search_radius = 1, lat = NULL, long = NULL,
  rtweets_token, num_tweets = 20)
}
\arguments{
\item{search_radius}{radius around the city}

\item{lat}{latitude of the city}

\item{long}{longitude of the city}

\item{rtweets_token}{information on the twitter tokens}

\item{num_tweets}{number of tweets to retrieve, defaults to 20}
}
\description{
function which retrieves the tweets based on user provided latitude, longitude and search radius
}
\keyword{tweets}
