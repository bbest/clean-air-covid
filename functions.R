city2zip <- function(city, redo = F){
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
  city_s     <-  str_replace(city, "[ ,]+", "-")
  city_url   <- glue("{osm_url}?q={city_s}&polygon_geojson=1&format=json")
  city_json  <- here(glue("data/city_{city_s}.json"))
  city_geo   <- here(glue("data/city_{city_s}.geojson"))
  city_shp   <- here(glue("data/city_{city_s}/city_{city_s}.shp"))
  city_zip   <- here(glue("data/city_{city_s}.zip"))
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
  
  # write shapefile package for upload to GEE ----
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