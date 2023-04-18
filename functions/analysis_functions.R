require(tidyverse)
require(lubridate)
require(sf)


#' @description A simple function that will take a dataframe, filter by records inside ANP, and return a
#' cleaned dataframe. IMPORTANT: This function only work for lat long data seperated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the dataframe you have read in.
#' @param park The quoted name of the national park/monument that you want to filter records by. REQUIRES
#' name format to be exact. Find a list of the 427 park names at this link: https://rpubs.com/klima21/filternps.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a dataframe of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @example
#'
#' # Read in data from working directory
#' bird.dat <- read.csv("ebird_mappingloc_20220217.csv")
#'
#' # Use filter_nps function to filter the bird.dat dataframe to records inside Acadia National Park
#' bird.anp <- filter_nps(bird.dat, "Acadia National ParK", lat = "y", long = "x")
#'
#' @export

filter_nps <- function(dat, park, lat, long) {
  
  sf::sf_use_s2(FALSE)
  
  if (park == "Acadia National Park") {
    
    acad.bounds <- sf::read_sf("data/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
      st_transform(4326)
    
    
    dat2 <- dat %>% 
      rename(x = paste(long), y = paste(lat)) %>% 
      mutate(longitude.keep = x,
             latitude.keep = y) %>% 
      sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(acad.bounds))
    
    
    dat2 %>% 
      mutate(intersect = as.integer(st_intersects(geometry, acad.bounds))) %>% 
      filter(!is.na(intersect))
    
    
    output <- sf::st_join(dat2, acad.bounds, left = F) %>% 
      st_set_geometry(., NULL) %>% 
      select(-c(CLASS, Acres, Hectares, SHAPE_Leng, SHAPE_Area)) %>% 
      select(everything(), latitude = latitude.keep, longitude = longitude.keep)
    
  } else {
    
    nps.bounds <- sf::read_sf("data/nps_boundary/nps_boundary.shp") %>% 
      st_transform(4326) %>% 
      filter(UNIT_NAME == paste(park))
    
    
    if (length(nps.bounds) < 1) {
      stop("Function returned a park with 0 polygons. The park name does not exist. Go to https://rpubs.com/klima21/filternps for a list of valid park names.")
    }
    
    
    dat2 <- dat %>% 
      rename(x = paste(long), y = paste(lat)) %>% 
      mutate(longitude.keep = x,
             latitude.keep = y) %>% 
      sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(nps.bounds))
    
    
    dat2 %>% 
      mutate(intersect = as.integer(st_intersects(geometry, nps.bounds))) %>% 
      filter(!is.na(intersect))
    
    
    output <- sf::st_join(dat2, nps.bounds, left = F) %>% 
      st_set_geometry(., NULL) %>%
      select(-c(OBJECTID:Shape_Area)) %>% 
      select(everything(), latitude = latitude.keep, longitude = longitude.keep)
  }
  
  return(output)
}




#' @description A simple function that will take a data frame, filter by records inside ANP, and return a
#' cleaned data frame. IMPORTANT: This function only work for lat long data separated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the data frame you have read in.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a data frame of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @example
#'
#' # Read in data from working directory
#' bird.dat <- read.csv("ebird_mappingloc_20220217.csv")
#'
#' # Use filter_nps function to filter the bird.dat data frame to records inside Acadia National Park
#' bird.anp <- filter_nps(bird.dat, lat = "y", long = "x")
#'
#' @export

filter_acad_gbif <- function(dat, lat, long) {
  
  sf::sf_use_s2(FALSE)
  
  
  acad.bounds <- sf::read_sf("data/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
    st_transform(4326)
  
  
  dat2 <- dat %>% 
    rename(x = paste(long), y = paste(lat)) %>% 
    mutate(longitude.keep = x,
           latitude.keep = y) %>% 
    sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(acad.bounds))
  
  
  dat2 %>% 
    mutate(intersect = as.integer(st_intersects(geometry, acad.bounds))) %>% 
    filter(!is.na(intersect))
  
  
  output <- sf::st_join(dat2, acad.bounds, left = F) %>% 
    st_set_geometry(., NULL) %>% 
    select(-c(CLASS, Acres, Hectares, SHAPE_Leng, SHAPE_Area)) %>% 
    select(everything(), latitude = latitude.keep, longitude = longitude.keep)
  
  
  return(output)
  
}




#' @description A simple function that will take a dataframe, filter by records inside a given
#' park, and return a cleaned dataframe. This function only work for lat long data separated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the dataframe you have read in.
#' @param park The quoted name of the national park/monument that you want to filter records by. REQUIRES
#' name format to be exact. Find a list of the 427 park names at this link: https://rpubs.com/klima21/filternps.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a dataframe of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @export

filter_gbif_to_park <- function(dat, park, lat, long) {
  
  if (!file.exists("data/nps_boundary")) {
    download('https://irma.nps.gov/DataStore/DownloadFile/673366', destfile = "data/nps_boundary.zip")
  }
  
  
  if (!file.exists("data/nps_boundary")) {
    unzip("data/nps_boundary.zip", exdir = "data/nps_boundary")
  }
  

  nps.bounds <- readOGR("data/nps_boundary/nps_boundary.shp", verbose = FALSE)
  
  
  select.bounds <- nps.bounds[nps.bounds@data$UNIT_NAME==paste(park), ]
  
  
  df <- df %>% rename(latitude = paste(lat), longitude = paste(long))
  
  df$"long" <- df$longitude
  df$"lat" <- df$latitude
  
  coordinates(df) <- c("long", "lat")
  
  slot(df, "proj4string") <- slot(select.bounds, "proj4string")
  
  output <- over(select.bounds, df, returnList = TRUE)
  
  output.df <- data.frame(output) %>% 
    rename_with(~str_replace(., "X15.", ""), everything())
  
  return(output.df)
  
}

