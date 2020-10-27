#' Function to setup twitter parameters
#'
#' This function takes 4 inputs  and saves it
#' @importFrom methods new
#' @importFrom stringr str_detect
#' @return returns the saved token
#' @param app_name name of the app
#' @param consumer_key consumer key
#' @param consumer_secret consumer secret
#' @param access_token access token
#' @param access_secret access secret
#' @export
tweet_token_create <- function(app_name, consumer_key = NULL, consumer_secret = NULL, access_token = NULL, access_secret = NULL ){
  create_token <- NULL
  temp_token <- create_token(app = app_name,
                             consumer_key <- consumer_key,
                             consumer_secret <- consumer_secret,
                             access_token <- access_token,
                             access_secret  <- access_secret
  )
  return(temp_token)
}


##This is a fake tweet token info
# rtweets_token <- create_token(app = "Get_Flu_data",
#                               consumer_key <- '5zOeUt84SRFmyHR0uhNMVBXW4',
#                               consumer_secret <- 'Fy7VHUVCfdGTtVNyY6x5lfNs7kjoDAUCh4EAEQfI6WHQPtTu6l',
#                               access_token <- '1094382284201963521-6RPUx2tnlmhyfRE10gCGsBfvsU5fM4',
#                               access_secret  <- '7uUeHLCcHm9gA9xrHNHyscKC11ZAptDCQZHlzDbH1NdhE'
# )



#' Gets tweets in the recent 6-9 days pertaining to 'asthma', 'flu' & 'allergies'
#' @param s4_object  the object returned from the get_cities function.
#' @param num_tweets  the number of tweets to be retrieved.
#' @param  rtweets_token this is the token for accessing twitter
#' @return function returns a dataframe with number of tweets in the past 6-9 days
#'
#' @export
call_twitter <- function(s4_object, num_tweets, rtweets_token){

  # get latitude and longitude information about the primary city;
  # use radius to determine the geocode
  temp_lat <- s4_object@latitude
  temp_long <- s4_object@longitude
  temp_radius <- s4_object@radius_ref

  # get list of raw tweets
 # raw_tweets <- NULL
  raw_tweets <- get_twitter_data(lat = temp_lat, long = temp_long , search_radius = temp_radius, num_tweets = num_tweets, rtweets_token = rtweets_token)

  # if no tweets are available in that geocode region ,set df to null, otherwise calculate the number of tweets by created date
  daily_ct <- summarise_tweets(raw_tweets)

  print(daily_ct)
}


#' function which retrieves the tweets based on user provided latitude, longitude and search radius
#' @keywords tweets
#'@param rtweets_token information on the twitter tokens
#'@param lat latitude of the city
#'@param long longitude of the city
#'@param search_radius radius around the city
#'@param num_tweets number of tweets to retrieve, defaults to 20
#' @export
get_twitter_data <- function(search_radius = 1, lat = NULL, long = NULL, rtweets_token, num_tweets = 20){

   geo_code <- NULL

  # setting possible search strings
  search_string <- paste("allergies", "asthma", "flu", sep = " OR ")

  # create a radius indicator:
  search_radius_temp <- paste0(search_radius, "mi")

  # create geocode_format
  geo_code <- NULL
  geo_code <- paste(lat, long, search_radius_temp, sep = ",")

  temp_tweets <- rtweet::search_tweets(search_string, n = num_tweets, include_rts = FALSE, token = rtweets_token, geocode = geo_code, verbose = FALSE)
  return(temp_tweets)
}




#' helper function to summarise tweets: count number of tweets by day
#' @param raw_tweet_df output from get_twitter function
#' @return returns count of weather specific tweets for each day
summarise_tweets <- function(raw_tweet_df){
    created_at <- NULL
    created_dt <- NULL
    raw_tweets <- NULL
    geo_code <- NULL
    
  ct_by_date <- raw_tweet_df %>%
    dplyr::mutate(cr_dt = lubridate::make_date(lubridate::year(created_at),
                                               lubridate::month(created_at),
                                               lubridate::day(created_at)
    ),
    allergy_ct = stringr::str_detect(text, "allergy"),
    flu_ct = stringr::str_detect(text, "flu"),
    asthma_ct = stringr::str_detect(text, "asthma"),
    all_selected  = allergy_ct + flu_ct + asthma_ct
    ) 

  tw_by_dates <- ct_by_date %>%  
    dplyr::filter(all_selected > 0) %>% 
    dplyr::group_by(cr_dt) %>%
    dplyr::summarise(number_twt = n()) %>% dplyr::rename("Tweet_Date" = cr_dt)
  
  tw_by_typ <- ct_by_date %>%
    dplyr::select(contains("_ct")) %>% 
    dplyr::summarise_all(sum) %>% 
    dplyr::mutate("Value" = "Total Tweets") %>% 
    dplyr::select("Value", everything())
  
  return(list(tw_by_dates, tw_by_typ))
}
