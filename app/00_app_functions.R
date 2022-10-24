require(tidyverse)
require(rinat)
require(lubridate)
require(leaflet)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(bslib)
require(fresh)
require(png)
require(grid)
require(purrr)
require(readxl)
require(rebird)
require(downloader)
require(sp)
require(rgdal)


## List of functions
# inat_recent()
# ebird_recent()
# te_species()
# watchlist_species()
# new_species()
# make_leaflet()
# leaflet_summary()
# download_photos()
# make_tree_page()


#' Function returns summaries and a data frame of recent iNaturalist observations 
#'
#' This function takes a recent time span and returns all iNaturalist records from
#' inside a desired location during that time span. Additionally, this 
#' function produces four data frames with summary statistics from the iNaturalist data.
#'
#' @inheritParams None
#' @return A data frame of recent iNaturalist observations.
#' @param timespan: The recent time span of interest. Options 
#' are "week", "threedays", or "yesterday" as inputs for the "timespan" parameter.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @param place: An iNaturalist slug (the place name) as a single string with words separated by hyphens. 
#' For example, the place Acadia National Park would be "acadia-national-park" as found in the place page
#' url: https://www.inaturalist.org/observations?place_id=142267
#' @seealso None
#' @export

inat_recent <- function(place_id, timespan, output.path) {
  
  ## Check to make sure that parameter inputs are correct
  # timespan
  if (timespan != "week") {
    if(timespan != "threeday") {
      if(timespan != "yesterday") {
        stop("Entered time span is not accepted. Must be 'week', 'threeday' or 'yesterday'")
      }
    }
  } 
  
  
  # output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  if (str_sub(output.path, start = -1) == "`\`") {
    stop("Directory path cannot end with '\'")
  }
  
  
  # Get the past week's dates and format
  date.filter <- format(Sys.Date()-1:7, "%Y-%m-%d") %>% 
    as_tibble() %>% 
    rename(date = value) %>% 
    mutate(year = as.numeric(year(date)),
           month = as.numeric(month(date)),
           day = as.numeric(day(date)))
  
  
  # List the month and year for get_inat_obs sub-function
  year <- date.filter$year
  month <- date.filter$month
  
  
  # This is the function that starts the download of inat data inside park boundary
  get_inat_data <- function(obs_year, obs_month) {
    
    get_inat_obs(quality = "research",
                 place_id = place_id,
                 geo = TRUE,
                 year = obs_year, 
                 month = obs_month, 
                 maxresults = 10000) %>% 
      as_tibble() %>% 
      select(scientific_name, common_name, iconic_taxon_name, observed_on, place_guess, 
             latitude, longitude, positional_accuracy, user_login, user_id, captive_cultivated, url, image_url) %>% 
      mutate(common_name = tolower(common_name)) %>% 
      rename_all( ~ str_replace_all(., "_", "."))
    
  }
  
  
  if(exists("get_inat_data")) {
    message("Crawling data over from iNaturalist...")
  }
  
  
  # Runs if week is called
  if (timespan == "week") {
    
    # Pull the previous week of inat data
    inat_obs <- map2_dfr(year, month, get_inat_data) %>% 
      filter(observed.on >= date.filter$date[7] & observed.on <= date.filter$date[1])
    
  }
  
  
  # Runs if threedays is called
  if (timespan == "threeday") {
    
    # Subset this to three days
    inat_obs <- map2_dfr(year, month, get_inat_data) %>% 
      filter(observed.on >= date.filter$date[3])
    
  }
  
  
  # Runs if yesterday is called
  if (timespan == "yesterday") {
    
    # Subset this to only yesterday
    inat_obs <- map2_dfr(year, month, get_inat_data) %>% 
      filter(observed.on == date.filter$date[1])
    
  }
  
  
  inat_obs <- inat_obs %>% 
    mutate(dup = duplicated(.),
           observed.on = as.Date(observed.on)) %>% 
    filter(dup == "FALSE") %>% 
    select(-dup)
  
  
  # if(length(inat_obs) >= 1) {
  #   message("Caluclating summary statistics for time span...")
  # }
  
  
  # Stop summarise output message
  options(dplyr.summarise.inform = FALSE)
  
  
  ## Create summary tables
  # Number of obs in each taxon
  summary_taxon <- inat_obs %>% 
    group_by(iconic.taxon.name) %>% 
    summarise(count = length(iconic.taxon.name)) %>% 
    arrange(desc(count))
  
  # Number of observers = length, number of obs by user
  summary_observers <- inat_obs %>% 
    group_by(user.id, user.login) %>% 
    summarise(count = length(user.id)) %>% 
    arrange(desc(count))
  
  # Number of obs for each species
  summary_species <- inat_obs %>% 
    group_by(scientific.name, common.name) %>% 
    summarise(count = length(user.id)) %>% 
    arrange(desc(count))
  
  
  # Write out the summary tables
  write.csv(summary_taxon, paste(output.path, "summary_taxon.csv", sep = "/"))
  write.csv(summary_observers, paste(output.path, "summary_observers.csv", sep = "/"))      
  write.csv(summary_species, paste(output.path, "summary_species.csv", sep = "/"))
  
  
  if(length(inat_obs) >= 1) {
    message("Data retrieval successful!")
  }
  
  
  return(inat_obs) 
  
}




#' Function returns a data frame of recent eBird observations.
#'
#' This function takes a recent time span and returns all eBird records from
#' inside a designated area during that time span.
#'
#' @inheritParams None
#' @return A data frame of recent eBird observations.
#' @param ebird_loc: An eBird place name as a single string with components separated by hyphens. 
#' For example, the Hancock County, Maine, USA is "US-ME-009". A full list of codes can be found 
#' here: https://support.ebird.org/en/support/solutions/articles/48000838205-download-ebird-data
#' @export

ebird_recent <- function(ebird_loc, parkname) {
  
  # Starting message
  message("Flying in the data from eBird...")
  
  
  # Get code list
  codelist <- ebirdregion(loc = ebird_loc, back = 7, key = "kjh86bnmkpfh") %>% 
    pull(speciesCode)
  
  
  # Create a run function to pull the data for each species in the code list
  run <- function(ebird_loc, code) {
    
    data <- ebirdregion(loc = ebird_loc, species = code, back = 7, key = "kjh86bnmkpfh") %>% 
      select(comName, sciName, obsDt, lat, lng, subId) %>% 
      mutate(url = paste0("https://ebird.org/checklist/", subId))
    
    return(data)
  }
  
  
  # Map over this function and clean
  mid <- map2_dfr(ebird_loc, codelist, run) %>% 
    mutate(iconic.taxon.name = "Aves",
           obsDt = as.Date(obsDt)) %>% 
    select(scientific.name = sciName, common.name = comName, iconic.taxon.name,
           observed.on = obsDt, latitude = lat, longitude = lng, url)
  
  # Select records inside the designated area
  output <- filter_nps(mid, parkname, lat = "latitude", long = "longitude")
  
  
  if(length(output) >= 1) {
    message("Data retrieval successful!")
  } else {
    stop("There are no recent eBird records inside this park.")
  }
  
  
  return(output)
  
}




#' Function returns a combined data frame of recent citsci observations.
#'
#' @inheritParams None
#' @return A combined data frame of recent citsci observations.
#' @param x: One data frame of citsci data from either the inat_recent or the
#' ebird_recent functions.
#' @param y: Another data frame of citsci data from either the inat_recent or the
#' ebird_recent functions.
#' @export

combine_citsci_data <- function(x, y) {
  
  output <- bind_rows(x, y)
  
  return(output)
  
}




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

filter_nps <- function(df, park, lat, long) {
  
  # if (!file.exists("app/www/nps_boundary.zip")) {
  #   download('https://irma.nps.gov/DataStore/DownloadFile/673366', destfile = "app/www/nps_boundary.zip")
  # }
  # 
  # 
  # if (!dir.exists("app/www/nps_boundary")) {
  #   unz("app/www/nps_boundary.zip")
  # }
  
  
  nps.bounds <- readOGR("app/www/nps_boundary/nps_boundary.shp", verbose = FALSE)
  
  
  select.bounds <- nps.bounds[nps.bounds@data$UNIT_NAME==paste(park), ]
  
  
  if (length(select.bounds@polygons) < 1) {
    stop("Function returned a park with 0 polygons. The park name does not exist. Go to https://rpubs.com/klima21/filternps for a list of valid park names.")
  }
  
  
  df <- df %>% rename(latitude=paste(lat), longitude=paste(long))
  
  
  df$"long" <- df$longitude
  df$"lat" <- df$latitude
  
  
  coordinates(df) <- c("long", "lat")
  
  
  slot(df, "proj4string") <- slot(select.bounds, "proj4string")
  
  
  output <- over(select.bounds, df, returnList = TRUE)
  
  
  output.df <- data.frame(output) %>% 
    rename_with(~str_replace(., "X15.", ""), everything())
  
  
  return(output.df)
  
}




#' Function summarizes iNaturalist observations for watchlist species
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#' files of all the species in the data frame listed as non-native and invasive as well 
#' as some summary statistics. This is done at both the federal and state levels. Data is 
#' written out to the provided directory.
#'
#' @inheritParams None
#' @return A dataframe of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @seealso None
#' @export
#' @examples  
#' watchlist_species(inat_lastweek, "outputs/te_species")

watchlist_species <- function(x, output.path) {
  
  ## Check to make sure that parameter inputs are correct
  # output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  # Stop this output from showing
  options(readr.show_col_types = FALSE)
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  ### THREATENED/ENDANGERED
  ## Federal
  # Read in the file and filter for the T, E, and SC species
  fed_te_sp <- read_csv("app/www/federal_list_maine.csv") %>% 
    rename_with(tolower, everything()) %>% 
    select(scientific.name = "scientific name", common.name = "common name",
           listing.status = "esa listing status") %>% 
    mutate(level = "federal",
           listing.status = tolower(listing.status))
  
  
  ## State
  # Read in the file and filter for the T, E, and SC species
  state_te_sp <- read_csv("app/www/maine_thrt_end_list.csv") %>% 
    mutate(level = "state",
           status = tolower(listing.status))
  
  
  # All T, E species from the last week
  te_specieslist_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, taxon = iconic.taxon.name, listing.status) %>% 
    distinct()
  
  
  # Number of species per T&E category
  te_summary_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, taxon = iconic.taxon.name, listing.status) %>% 
    distinct() %>% 
    group_by(listing.status) %>% 
    summarise(count = length(listing.status))
  
  
  # All T, E species from the last week
  te_specieslist_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(state_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, taxon = iconic.taxon.name, listing.status) %>% 
    distinct()
  
  
  # Number of species per T&E category
  te_summary_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(state_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, taxon = iconic.taxon.name, listing.status) %>% 
    distinct() %>% 
    group_by(listing.status) %>% 
    summarise(count = length(listing.status))
  
  
  
  ## RARE, INVASIVE, PESTS
  # Rare native species list
  listsp <- read_excel("app/www/acad_watchlist_species.xlsx", .name_repair = custom_name_repair) 
  
  rarenative <- listsp %>% 
    filter(status == "rare native")
  
  invasive_ne <- listsp %>% 
    filter(status == "invasive not established")
  
  invasive_est <- listsp %>% 
    filter(status == "invasive established")
  
  pests <- listsp %>% 
    filter(status == "pest disease")
  
  otherinsects <- listsp %>% 
    filter(status == "insect")
  
  
  # Native but rare
  rarenative_obs <- x %>% 
    filter(scientific.name %in% rarenative$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name)
  
  
  # Invasive but not yet established in ACAD
  invasive_ne_obs <- x %>% 
    filter(scientific.name %in% invasive_ne$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name)
  
  
  # Invasive and established in ACAD
  invasive_est_obs <- x %>% 
    filter(scientific.name %in% invasive_est$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name)
  
  
  # Vegetation pests and disease
  pest_obs <- x %>% 
    filter(scientific.name %in% pests$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name)
  
  # Other insects
  insect_obs <- x %>% 
    filter(scientific.name %in% otherinsects$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name)
  
  
  ## Writing out data and displaying totals by category
  # Federal TE
  if(length(te_specieslist_federal$scientific.name) >= 1) {
    write.csv(te_specieslist_federal, paste(output.path, "te_specieslist_federal.csv", sep = "/"), row.names = F)
    message(paste0("Number of federally threatened/endangered species: ", length(te_specieslist_federal$scientific.name)))
  } else {
    message(paste0("Number of federally threatened/endangered species: ", length(te_specieslist_federal$scientific.name)))
  }
  
  # if(length(te_summary_federal$listing.status) >= 1) {
  #   write.csv(te_summary_federal, paste(output.path, "te_summary_federal.csv", sep = "/"), row.names = F)
  # }
  
  
  # State TE
  if(length(te_specieslist_state$scientific.name) >= 1) {
    write.csv(te_specieslist_state, paste(output.path, "te_specieslist_state.csv", sep = "/"), row.names = F)
    message(paste0("Number of state threatened/endangered species: ", length(te_specieslist_state$scientific.name)))
  } else {
    message(paste0("Number of state threatened/endangered species: ", length(te_specieslist_state$scientific.name)))
  }
  
  # if(length(te_summary_state$listing.status) >= 1) {
  #   write.csv(te_summary_state, paste(output.path, "te_summary_state.csv", sep = "/"), row.names = F)
  # }
  
  
  # Rare, natives
  if(length(rarenative_obs$scientific.name) >= 1) {
    write.csv(rarenative_obs, paste(output.path, "rarenative_species.csv", sep = "/"), row.names = F)
    message(paste0("Number of rare, native plant species: ", length(rarenative_obs$scientific.name)))
    
  } else {
    message(paste0("Number of rare, native plant species: ", length(rarenative_obs$scientific.name)))
  }
  
  
  # Invasive not established in park
  if(length(invasive_ne_obs$scientific.name) >= 1) {
    write.csv(invasive_ne_obs, paste(output.path, "invasive_ne_species.csv", sep = "/"), row.names = F)
    message(paste0("Number of non-established invasive plant species: ", length(invasive_ne_obs$scientific.name)))
  } else {
    message(paste0("Number of non-established invasive plant species: ", length(invasive_ne_obs$scientific.name)))
    
  }
  
  
  # Invasive established in park
  if(length(invasive_est_obs$scientific.name) >= 1) {
    write.csv(invasive_est_obs, paste(output.path, "invasive_est_species.csv", sep = "/"), row.names = F)
    message(paste0("Number of established invasive plant species: ", length(invasive_est_obs$scientific.name)))
  } else {
    message(paste0("Number of established invasive plant species: ", length(invasive_est_obs$scientific.name)))
  }
  
  
  # Tree pests
  if(length(pest_obs$scientific.name) >= 1) {
    write.csv(pest_obs, paste(output.path, "pest_species.csv", sep = "/"), row.names = F)
    message(paste0("Number of tree pest species: ", length(pest_obs$scientific.name)))
  } else {
    message(paste0("Number of tree pest species: ", length(pest_obs$scientific.name)))
  }
  
  
  # Other insects
  if(length(insect_obs$scientific.name) >= 1) {
    write.csv(insect_obs, paste(output.path, "otherinsect_species.csv", sep = "/"), row.names = F)
    message(paste0("Number of other insects of concern: ", length(insect_obs$scientific.name)))
  } else {
    message(paste0("Number of other insects of concern: ", length(insect_obs$scientific.name)))
  }
  
  
  # Final completion message
  if(exists("pests")) {
    message("Results have been saved to designated directory if > 0 species were detected.")
  }
  
}




#' Function summarizes any new species observed within the designated area.
#'
#' This function takes a data frame of observations, path to the species list,
#' and the path for the outputs. It creates a CSV file of all the species in 
#' the data frame that have not been recorded in the park before. The results are
#' written out to the provided directory.
#'
#' @inheritParams None
#' @return A data frame of species not yet recorded in the designated area.
#' @param x: Data frame of citizen science observations.
#' @param species.list.path The path to your created species list for the designated area.
#' @param output.path: The path you want the summary CSV to be written to.
#' @seealso None
#' @export

new_npspecies <- function(x, species.list.path, output.path) {
  
  ## Check to make sure that parameter inputs are correct
  # output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  # Get the full species list
  park_sp_list <- read.csv(paste(species.list.path))
  
  
  # New species according to the NPSpecies list
  new_species <- x %>% 
    filter(scientific.name %in% park_sp_list$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name) %>% 
    slice(1)
  
  
  # Print summary and write out if appropriate
  if(length(new_species$scientific.name) >= 1) {
    write.csv(new_species, paste(output.path, "new_species.csv", sep = "/"))
    message(paste0("Number of new species: ", length(new_species$scientific.name)))
  } else {
    message(paste0("Number of new species: ", length(new_species$scientific.name)))
  }
  
  
  # Final print
  if(length(new_species$scientific.name) >= 1) {
    message("Results were saved to designated directory.")
  } else {
    message("No results were saved to designated directory as there were 0 new species")
  }
  
}




#' Function to produce an interactive leaflet map widget of recent observations.
#'
#' This function takes a data frame of recent observations and produces a leaflet map widget 
#' with satellite imagery base layer, labels of common names that appear when the mouse is 
#' hovered over a marker, and a link to the observation online accessible by clicking a marker.
#'
#' @inheritParams None
#' @return A leaflet map widget of recent iNaturalist observations.
#' @param x: Data frame of citizen science observations.
#' @seealso None
#' @export

make_leaflet <- function (x) {
  
  # Create an extra column with the url
  formap <- x %>% 
    mutate(url = paste0("<b><a href='", url, "'>View observation<br>online</a></b>")) 
  
  
  # Make the leaflet map
  map <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TerrainLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
               popup = formap$url)
  
  
  # Call the map for display
  return(map)
  
}




#' Function to produce an interactive leaflet map widget of iNaturalist observations
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and produces a leaflet map widget with satellite 
#' imagery base layer, labels of common names that appear when the mouse is hovered over a
#' marker, and a link to the observation on iNaturalist accessible by clicking a marker.
#'
#' @inheritParams None
#' @return A leaflet map widget of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export

leaflet_summary <- function (x) {
  
  formap <- x
  
  
  maxLong = max(formap$longitude) #- 0.05
  maxLat = max(formap$latitude) #+ 0.05 
  minLong = min(formap$longitude) #+ 0.05
  minLat = min(formap$latitude) #- 0.05
  
  
  map <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TerrainLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, clusterOptions = markerClusterOptions()) %>% 
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  
  return(map)
  
}




#' Function to download photos of 10 random iNaturalist observations
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and downloads the images associated with 10 random and
#' unique species.
#'
#' @inheritParams None
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export

download_photos <- function(x, output.path) {
  
  # Check that output.path is okay
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  if (exists("output.path") == FALSE) {
    stop("Directory does not exist")
  }
  
  # Filter the inat obs data frame so that there are no duplicate species
  pic_data <- x %>% 
    filter(!is.na(image.url)) %>% 
    group_by(scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    distinct() %>% 
    slice(1) %>% 
    ungroup()
  
  # Specify the sample size for random sampling
  sample_size <- if(nrow(pic_data) > 10) { 
    10 
  } else { 
    nrow(pic_data) 
  }
  
  # Filter and create the photo names
  pic_10random <- pic_data %>% 
    sample_n(size = sample_size) %>% 
    rownames_to_column(., var = "rowid") %>% 
    mutate(pic.name = paste0("inat_obs_", rowid)) %>% 
    select(-rowid)
  
  # Create the list of image urls from the 10 (or less) random iNat obs
  loop_list <- pic_10random %>% 
    select(image.url) %>% 
    unlist()
  
  # Create the list of names for the photos
  loop_filenames <- pic_10random %>% 
    select(pic.name) %>% 
    unlist()
  
  # Write the function for naming and downloading the photos
    get_pics <- function (imurl, imname) {
      download.file(imurl, paste0(output.path, "/", imname, ".jpg"), mode = 'wb')
    }
  
  # Run the loop
    repeat {
      
      tmp <- try(map2(loop_list, loop_filenames, get_pics))
      
      if (!(inherits(tmp, "try-error"))) 
        break
      
    }
  #map2(loop_list, loop_filenames, get_pics)
  
  # Write the data frame
  write.csv(pic_10random, paste(output.path, "summary_10random.csv", sep = "/"), row.names = F)
  
}




#' Function to create the tree page that summarizes obs by taxon
#'
#' This function takes a data frame of summarized iNaturalist records (created specifically by the
#' output of the "inat_recent()" function) and creates the tree image page
#'
#' @inheritParams None
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export
#' @examples  
#' make_tree_page() 

make_tree_page <- function() {
    
  img <- readPNG("www/treescape_npf.png")
  
  insecta <- readPNG("www/insecta.png")
  plantae <- readPNG("www/plantae.png")
  aves <- readPNG("www/aves.png")
  fungi <- readPNG("www/fungi.png")
  arachnida <- readPNG("www/arachnida.png")
  reptilia <- readPNG("www/reptilia.png")
  amphibia <- readPNG("www/amphibia.png")
  animalia <- readPNG("www/animalia.png")
  mammalia <- readPNG("www/mammalia.png")
  chromista <- readPNG("www/chromista.png")
  mollusca <- readPNG("www/mollusca.png")
  protozoa <- readPNG("www/protozoa.png")
  
  
  data <- read.csv("input_data/summary_taxon.csv") %>% 
    select(taxon = iconic.taxon.name, count)
  
  
  data <- data.frame(taxon = c("Insecta", "Plantae", "Aves", "Fungi", "Arachnida", "Reptilia", 
                               "Amphibia", "Animalia", "Mammalia", "Chromista", "Mollusca", "Protozoa"),
                     name = c("Insects", "Plants", "Birds", "Fungi", "Spiders", "Reptiles", 
                              "Amphibians", "Other Animals", "Mammals", "Seaweeds", "Mollusks", "Protozoans"),
                     count2 = rep(0, 12),
                     x = c(1.038, 1.012, 0.973, 1.035, 1.023, 1.007, 1.018, 0.988, 0.9985, 0.978, 0.968, 1.025),
                     y = c(1.011, 1.004, 0.992, 0.980, 1.005, 0.979, 0.993, 0.984, 0.991, 0.982, 0.982, 0.979)) %>% 
    left_join(data) %>% 
    mutate(count = replace_na(count, 0),
           label = paste(name, count, sep = "\n")) %>% 
    select(-count2)
  
  
  get_coords <- function(input) {
    
    pic_loc <- data %>% 
      select(taxon, x, y) %>% 
      mutate(xmin = x - 0.0038,
             xmax = x + 0.0038,
             ymin = y - 0.0038,
             ymax = y + 0.0038)
    
    input <- pic_loc %>% 
      filter(taxon == paste(input))
    
    return(input)
  }
  
  pic_loc <- data %>% 
    select(taxon) %>%
    list() %>% 
    unlist() %>% 
    map(., get_coords)
  
  
  plot <- ggplot(data) +
    annotation_custom(rasterGrob(img, interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    annotation_custom(rasterGrob(insecta, interpolate=TRUE), xmin=pic_loc$taxon1$xmin,
                      xmax=pic_loc$taxon1$xmax, ymin=pic_loc$taxon1$ymin, ymax=pic_loc$taxon1$ymax) +
    
    annotation_custom(rasterGrob(plantae, interpolate=TRUE), xmin=pic_loc$taxon2$xmin,
                      xmax=pic_loc$taxon2$xmax, ymin=pic_loc$taxon2$ymin, ymax=pic_loc$taxon2$ymax) +
    
    annotation_custom(rasterGrob(aves, interpolate=TRUE), xmin=pic_loc$taxon3$xmin,
                      xmax=pic_loc$taxon3$xmax, ymin=pic_loc$taxon3$ymin, ymax=pic_loc$taxon3$ymax) +
    
    annotation_custom(rasterGrob(fungi, interpolate=TRUE), xmin=pic_loc$taxon4$xmin,
                      xmax=pic_loc$taxon4$xmax, ymin=pic_loc$taxon4$ymin, ymax=pic_loc$taxon4$ymax) +
    
    annotation_custom(rasterGrob(arachnida, interpolate=TRUE), xmin=pic_loc$taxon5$xmin,
                      xmax=pic_loc$taxon5$xmax, ymin=pic_loc$taxon5$ymin, ymax=pic_loc$taxon5$ymax) +
    
    annotation_custom(rasterGrob(reptilia, interpolate=TRUE), xmin=pic_loc$taxon6$xmin,
                      xmax=pic_loc$taxon6$xmax, ymin=pic_loc$taxon6$ymin, ymax=pic_loc$taxon6$ymax) +
    
    annotation_custom(rasterGrob(amphibia, interpolate=TRUE), xmin=pic_loc$taxon7$xmin,
                      xmax=pic_loc$taxon7$xmax, ymin=pic_loc$taxon7$ymin, ymax=pic_loc$taxon7$ymax) +
    
    annotation_custom(rasterGrob(animalia, interpolate=TRUE), xmin=pic_loc$taxon8$xmin,
                      xmax=pic_loc$taxon8$xmax, ymin=pic_loc$taxon8$ymin, ymax=pic_loc$taxon8$ymax) +
    
    annotation_custom(rasterGrob(mammalia, interpolate=TRUE), xmin=pic_loc$taxon9$xmin,
                      xmax=pic_loc$taxon9$xmax, ymin=pic_loc$taxon9$ymin, ymax=pic_loc$taxon9$ymax) +
    
    annotation_custom(rasterGrob(chromista, interpolate=TRUE), xmin=pic_loc$taxon10$xmin,
                      xmax=pic_loc$taxon10$xmax, ymin=pic_loc$taxon10$ymin, ymax=pic_loc$taxon10$ymax) +
    
    annotation_custom(rasterGrob(mollusca, interpolate=TRUE), xmin=pic_loc$taxon11$xmin,
                      xmax=pic_loc$taxon11$xmax, ymin=pic_loc$taxon11$ymin, ymax=pic_loc$taxon11$ymax) +
    
    annotation_custom(rasterGrob(protozoa, interpolate=TRUE), xmin=pic_loc$taxon12$xmin,
                      xmax=pic_loc$taxon12$xmax, ymin=pic_loc$taxon12$ymin, ymax=pic_loc$taxon12$ymax) +
    
    geom_text(aes(x = x, y = y, label = name), vjust = -4.4, size = 7, fontface = "bold") +
    geom_text(aes(x = x, y = y, label = count), vjust = 5.3, size = 7, fontface = "bold") +
    scale_x_continuous(expand = c(0.005, 0.005)) +
    scale_y_continuous(expand = c(0.005, 0.005)) +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "none")
  
  return(plot)

}

