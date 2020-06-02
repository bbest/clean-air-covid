# use librarian to load libraries, installing if needed
if (!require("librarian")) install.packages("librarian")
library("librarian")

pkgs <- c(
  # utility
  "here","glue",
  "readr","dplyr","tidyr","purrr","scales",
  "lubridate","stringr","units",
  # api
  "jsonlite",
  # plotting
  "ggplot2","plotly",
  # spatial
  "sf",
  # text
  "rtweet","tidytext","textdata",
  # tensorflow
  "tensorflow","keras")
shelf(pkgs)

city2zip <- function(city, city_sfx, redo = F){
  # city = "Delhi, India"
  
  # load libraries ----
  library(jsonlite)
  library(sf)
  library(purrr)
  library(stringr)
  library(glue)
  library(here)
  library(dplyr)
  library(units)
  
  # set variables ----
  # use API: https://nominatim.org/release-docs/develop/api/Search/
  osm_url    <- "https://nominatim.openstreetmap.org/search.php"
  city_url   <- glue("{osm_url}?q={city_sfx}&polygon_geojson=1&format=json")
  city_json  <- here(glue("data/city_{city_sfx}.json"))
  city_geo   <- here(glue("data/city_{city_sfx}.geojson"))
  city_shp   <- here(glue("data/city_{city_sfx}/city_{city_sfx}.shp"))
  city_zip   <- here(glue("data/city_{city_sfx}.zip"))
  city_paths <- list(geojson = city_geo, zip = city_zip)
  
  # skip if already done, unless redo = T
  if (file.exists(as.character(city_paths)) & !redo)
    return(city_paths)
  
  # download data using API ----
  download.file(city_url, city_json)
  
  # extract and convert to formats of interest ----
  
  # extract city polygon from json
  city_lst <- read_json(city_json)
  idx <- map_lgl(
    city_lst, ~ 
      .x$class == "boundary" & 
      .x$type == "administrative")
  city_ply <- city_lst[idx][[1]]
  
  # write geojson
  f <- paste0(
    '{
      "type": "Feature",
      "properties": {',
        '"place_id":'     , city_ply$place_id    , ',',
        '"osm_id":'      , city_ply$osm_id      , ',',
        '"display_name":"', city_ply$display_name, '"',
      '},',
      '"geometry":', toJSON(city_ply$geojson, pretty=T, auto_unbox=T),
    '}')
  writeLines(f, city_geo)
  
  # add centroid lon/lat and radius in miles ----
  p <- read_sf(city_geo)
  ctr <- st_centroid(p$geometry) %>% st_coordinates()
  area_sqmi <- st_area(p$geometry) %>% set_units(mi^2)
  area_sqm  <- st_area(p$geometry) %>% set_units(m^2)
  p <- p %>%
    mutate(
      lon  = ctr[,'X'],
      lat  = ctr[,'Y'],
      r_mi = sqrt(area_sqmi/pi),
      r_m  = sqrt(area_sqm/pi))
  write_sf(p, city_geo, delete_dsn = T)
  
  # write shapefile zip for upload to GEE ----
  p <- read_sf(city_geo)
  dir.create(dirname(city_shp), showWarnings = F)
  write_sf(p, city_shp, delete_dsn = T, delete_layer = T)
  zip(city_zip, dirname(city_shp))
  
  # cleanup intermediary files ----
  unlink(c(
    dirname(city_shp), 
    city_json), recursive = T, force = T)
  
  # return paths to files generated
  city_paths
}

get_tweets_premium <- function(
  env_label, appname, key, secret, query,
  date_beg = NULL, date_end = NULL, 
  loc_lonlat = NULL, loc_radius_mi = NULL,
  tweets_csv = NULL,
  language = "en",
  max_results = 500,
  silent = F){
  # 
  # Extract tweets from Twitter premium API and save as CSV
  #
  # original: https://github.com/FilipeamTeixeira/TweetAPI-premium/blob/master/TweetAPI_premium.R
  # Python example: https://github.com/twitterdev/search-tweets-python
  #
  # arguments:
  # - date_beg, date_end: since the timestamp (hours & seconds) is set to 0000 for both, 
  #                       the results are incude all of date_beg and exclude date_end
  # - loc_radius_mi: max is 25 miles
  # 
  # language = "en"; max_results = 500; silent = F
  
  library(glue)
  library(here)
  library(readr)
  library(dplyr)
  library(httr)
  library(base64enc)
  
  # check arguments valid
  if (!is.null(loc_lonlat))
    stopifnot(all(is.numeric(loc_lonlat), length(loc_lonlat) == 2))
  if (!is.null(loc_radius_mi))
    stopifnot(all(is.numeric(loc_radius_mi), loc_radius_mi <= 25))
  if (!is.null(max_results))
    stopifnot(all(is.numeric(loc_radius_mi), max_results <= 500))
  
  # prep query parameters
  # TODO: handle optional parameters if NULL: date_beg, date_end, loc_lonlat, loc_radius_mi
  fromDate    <- format(date_beg, "%Y%m%d0000") %>% as.numeric()
  toDate      <- format(date_end, "%Y%m%d0000") %>% as.numeric()
  loc         <- glue(
    "point_radius:[{paste(loc_lonlat, collapse = ' ')} {loc_radius_mi}mi]")
  q  <- glue("({query}) {loc} lang:{language}")
  # TODO: allow for cancelling of retweets (but not seeming to work)
  #       q <- glue("({query}) {loc} lang:{language} -filter:retweets")

  # check query not too long
  stopifnot(nchar(q) <= 1024)
  
  # formulate full body of query
  q_body <- glue(
    '{
    "query"     : "{{q}}", 
    "maxResults": {{max_results}},
    "fromDate"  : {{fromDate}},
    "toDate"    : {{toDate}}
    }',
    .open = "{{", .close = "}}")
  
  # base64 encoding
  base64kands  <- base64encode(charToRaw(glue("{key}:{secret}")))
  base64kandsb <- paste("Basic", base64kands, sep=" ")
  
  # request bearer token
  resToken <- POST(
    url = "https://api.twitter.com/oauth2/token",
    add_headers(
      "Authorization" = base64kandsb, 
      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
    body = "grant_type=client_credentials")
  
  # get bearer token
  bearer       <- content(resToken)
  bearerToken  <- bearer[["access_token"]]
  bearerTokenb <- paste("Bearer", bearerToken, sep=" ")
  
  # search for tweets
  api_env_url <- glue("https://api.twitter.com/1.1/tweets/search/fullarchive/{env_label}.json")
  # resTweets_0 <- resTweets
  # resTweets   <- resTweets_0
  resTweets <- POST(
    url = api_env_url,
    add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
    body = q_body)
  
  # parse tweets into data.frame
  tweets        <- content(resTweets)
  tweets_result <- tweets$results
  
  tbl <- tibble()
  
  if (length(tweets_result) == 0){
    write_csv(tbl, tweets_csv)
    message(glue("    n_tweets: 0!"))
    return(tbl)
  }
  
  # TODO: iterate over pages if hit max_results limit
  
  for (i in 1:length(tweets_result)){ # i = 2
    
    tbl <- tbl %>% 
      bind_rows(
        with(
          tweets_result[[i]],
          tibble(
            created_at  = created_at %>% 
              as.POSIXct(format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT"),
            id          = id,
            id_str      = id_str,
            text        = text,
            user_name   = user$name,
            user_screen = user$screen_name,
            longitude   = ifelse(
              is.null(coordinates), NA,
              coordinates$coordinates[[1]]),
            latitude   = ifelse(
              is.null(coordinates), NA,
              coordinates$coordinates[[2]]),
            lang        = lang,
            retweeted   = retweeted)))
  }
  
  if (!silent){
    message(glue("    n_tweets: {nrow(tbl)}"))
  }
  
  if (!is.null(tweets_csv))
    write_csv(tbl, tweets_csv)
  
  tbl
}

clean_tweet <- function(text){
  text %>% 
    str_replace_all("[^[:ascii:]]", "_") %>% 
    tolower() %>% 
    str_replace_all("@[^ ]+", "_usr_") %>% 
    str_replace_all("http[^ ]+", "_url_")
}