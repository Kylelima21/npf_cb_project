require(tidyverse)
require(rinat)
require(purrr)
require(readxl)
require(leaflet)

## List of functions
# inat_recent()
# te_species()
# watchlist_species()
# new_npspecies()
# make_leaflet()
# download_photos()



#' Function returns summaries and a data frame of recent iNaturalist observations 
#'
#' This function takes a recent time span and returns all iNaturalist records from
#' inside Acadia National Park boundaries during that time span. Additionally, this 
#' function produces four data frames with summary statistics from the iNaturalist data.
#'
#' @inheritParams None
#' @return A data frame of recent iNaturalist observations.
#' @param timespan: The recent time span of interest. Options 
#' are "week", "threeday", or "yesterday" as inputs for the "timespan" parameter.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @param place: An iNaturalist slug (the place name) as a single string with words separated by hyphens. 
#' For example, the place Acadia National Park would be "acadia-national-park" as found in the place page
#' url: https://www.inaturalist.org/observations?place_id=142267
#' @seealso None
#' @export
#' @examples  
#' example_data <- inat_recent("acadia-national-park", "week", "outputs/inat_summary")


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
  
  
  # Get the past week's dates and format
  date.filter <- format(Sys.Date()-1:7, "%Y-%m-%d") %>% 
    as_tibble() %>% 
    rename(date = value) %>% 
    mutate(year = as.numeric(str_replace(date, "(\\d*)\\-\\d*\\-\\d*", "\\1")),
           month = as.numeric(str_replace(date, "\\d*\\-(\\d*)\\-\\d*", "\\1")),
           day = as.numeric(str_replace(date, "\\d*\\-\\d*\\-(\\d*)", "\\1")))
  
  
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
             latitude, longitude, positional_accuracy, user_id, captive_cultivated, url, image_url) %>% 
      mutate(common_name = tolower(common_name)) %>% 
      rename_all( ~ str_replace_all(., "_", "."))
    
  }
  
  
  if(exists("get_inat_data")) {
    message("Asking nicely for data from iNaturalist...")
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
  
  
  
  if(length(inat_obs) >= 1) {
    message("Caluclating summary statistics for time span...")
  }
  
  # Stop summarise output mesage
  options(dplyr.summarise.inform = FALSE)
  
  ## Create summary tables
  # Number of obs in each taxon
  summary_taxon <- inat_obs %>% 
    group_by(iconic.taxon.name) %>% 
    summarise(count = length(iconic.taxon.name)) %>% 
    arrange(desc(count))
  
  # Number of observers = length, number of obs by user
  summary_observers <- inat_obs %>% 
    group_by(user.id) %>% 
    summarise(count = length(user.id)) %>% 
    arrange(desc(count))
  
  # Number of obs for each species
  summary_species <- inat_obs %>% 
    group_by(scientific.name, common.name) %>% 
    summarise(count = length(user.id)) %>% 
    arrange(desc(count))
  
  # # Random selection of 10 species
  # summary_basedata <- inat_obs %>% 
  #   group_by(scientific.name) %>% 
  #   arrange(desc(observed.on)) %>% 
  #   distinct() %>% 
  #   slice(1) %>% 
  #   ungroup()
  # 
  # sample_size <- if(nrow(summary_basedata) > 10) { 
  #   10 
  # } else { 
  #   nrow(summary_basedata) 
  # }
  # 
  # summary_10random <- summary_basedata %>% 
  #   sample_n(size = sample_size)
  
  
  # Write out the summary tables
  write.csv(summary_taxon, paste(output.path, "summary_taxon.csv", sep = "/"))
  write.csv(summary_observers, paste(output.path, "summary_observers.csv", sep = "/"))      
  write.csv(summary_species, paste(output.path, "summary_species.csv", sep = "/"))
  # write.csv(summary_10random, paste(output.path, "summary_10random.csv", sep = "/"))
  
  
  if(length(inat_obs) >= 1) {
    message("Calculations complete!")
  }
  
  
  return(inat_obs) 
  
}




#' Function summarizes iNaturalist observations for threatened/endangered species
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#' files of all the species in the data frame listed as species of concern, threatened, 
#' and endangered as well as some summary statistics. This is done at both the federal and 
#' state levels. Nothing is returned in R, only written out to the provided directory.
#'
#' @inheritParams None
#' @return A data frame of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @seealso None
#' @export
#' @examples  
#' te_species(inat_lastweek, "outputs/te_species")


te_species <- function(x, output.path) {
  
  
  ##Check to make sure that parameter inputs are correct
  #output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }

  
  #Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  #Need to figure out if we can get this file directly from URL, no luck with current tries
  #download.file("https://irma.nps.gov/NPSpecies/Search/DownloadSpeciesListFullListWithDetailsResults", "data/raw/nonnative_table.xlsx")
  
  
  if(exists("custom_name_repair")) {
    message("Gathering a list of threatened and endangered species...")
  }
  
  
  ##FEDERAL - An alternative data set from NPSpecies
  #Read in the file and filter for the T, E, and SC species
  fed_te_sp <- read_excel("data/raw/NPSpecies_ACAD_20220612.xlsx", .name_repair = custom_name_repair) %>% 
    select(order, family, scientific.name, common.names, category, 
           occurrence, te.species = 't&e', state.status) %>% 
    filter(te.species == "E" | te.species == "E*" | te.species == "EmE" | te.species == "EmT" |
             te.species == "T" | te.species == "SC" | te.species == "SOC") %>% 
    mutate(te.species = ifelse(te.species == "E" | te.species == "E*" | te.species == "EmE", "E", paste(te.species)),
           te.species = ifelse(te.species == "T" | te.species == "EmT", "T", paste(te.species)),
           te.species = ifelse(te.species == "SC" | te.species == "SOC", "SC", paste(te.species)))
  
  
  ##STATE - An alternative data set from NPSpecies
  #Read in the file and filter for the T, E, and SC species
  state_te_sp <- read_excel("data/raw/NPSpecies_ACAD_20220612.xlsx", .name_repair = custom_name_repair) %>% 
    select(order, family, scientific.name, common.names, category, 
           occurrence, te.species = 't&e', state.status) %>% 
    filter(state.status != "")
  
  
  if(exists("state_te_sp")) {
    message("Performing calculations...")
  }
  
  
  #All T, E, SC species from the last week
  te_specieslist_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name, taxon = iconic.taxon.name, te.species) %>% 
    distinct()
  
  
  #Number of species per T&E category
  te_summary_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name, taxon = iconic.taxon.name, te.species) %>% 
    distinct() %>% 
    group_by(te.species) %>% 
    summarise(count = length(te.species))
  
  
  #All T, E, SC species from the last week
  te_specieslist_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(state_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name, taxon = iconic.taxon.name, state.status) %>% 
    distinct()
  
  
  #Number of species per T&E category
  te_summary_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(state_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name, taxon = iconic.taxon.name, state.status) %>% 
    distinct() %>% 
    group_by(state.status) %>% 
    summarise(count = length(state.status))
  
  
  if(exists("te_summary_state")) {
    message("Saving results to designated directory...")
  }
  
  
  if(length(te_specieslist_federal$scientific.name) >= 1) {
    write.csv(te_specieslist_federal, paste(output.path, "te_specieslist_federal.csv", sep = "/"))
  } else {
    message("There are no federally threatened/endangered species recorded in these data...")
  }
  
  
  if(length(te_summary_federal$te.species) >= 1) {
    write.csv(te_summary_federal, paste(output.path, "te_summary_federal.csv", sep = "/"))
  }
  
  
  if(length(te_specieslist_state$scientific.name) >= 1) {
    write.csv(te_specieslist_state, paste(output.path, "te_specieslist_state.csv", sep = "/"))
  } else {
    message("There are no state threatened/endangered species recorded in these data...")
  }
  
  
  if(length(te_summary_state$state.status) >= 1) {
    write.csv(te_summary_state, paste(output.path, "te_summary_state.csv", sep = "/"))
  }
  
  
  if(exists("te_summary_state")) {
    message("Calculations complete!")
  }
  
}




#' Function summarizes iNaturalist observations for watchlist species
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#' files of all the species in the data frame listed as non-native and invasive as well 
#' as some summary statistics. This is done at both the federal and state levels. Nothing 
#' is returned in R, only written out to the provided directory.
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
  
  
  ##Check to make sure that parameter inputs are correct
  #output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  #Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  if(exists("custom_name_repair")) {
    message("Gathering a list of watchlist species...")
  }
  
  
  #Non-native list according to NPSpecies distinctions, though I don't think this is really
  #what we want?
  nonnative <- read_excel("data/raw/NPSpecies_ACAD_20220612.xlsx", .name_repair = custom_name_repair) %>% 
    select(order, family, scientific.name, common.names, category, 
           occurrence, nativeness, abundance) %>% 
    filter(nativeness == "Non-native")
  
  
  if(exists("nonnative")) {
    message("Performing calculations...")
  }
  
  
  #What species have been seen recently that are non-native?
  nonnative_species <- x %>% 
    filter(scientific.name %in% nonnative$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name) %>% 
    slice(1)
  
  
  if(exists("nonnative_species")) {
    message("Saving results to designated directory...")
  }
  
  
  if(length(nonnative_species$scientific.name) >= 1) {
    write.csv(nonnative_species, paste(output.path, "nonnative_species.csv", sep = "/"))
  } else {
    message("There are no non-native species recorded in these data...")
  }
  
  
  if(exists("nonnative")) {
    message("Calculations complete!")
  }
  
  
  #How to get invasive species list? Does NPS have a watch-list?
  
}





#' Function summarizes iNaturalist observations for new park species
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#' files of all the species in the data frame that have not been confirmed in the park 
#' before. Nothing is returned in R, only written out to the provided directory.
#'
#' @inheritParams None
#' @return A dataframe of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @seealso None
#' @export
#' @examples  
#' new_npspecies(inat_lastweek, "outputs/new_park_species") 


new_npspecies <- function(x, output.path) {
  
  
  ##Check to make sure that parameter inputs are correct
  #output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  #Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  if(exists("custom_name_repair")) {
    message("Gathering a list of confirmed species...")
  }
  
  
  #Get the full species list
  acad_sp_list <- read_excel("data/raw/NPSpecies_ACAD_20220612.xlsx", .name_repair = custom_name_repair) %>% 
    select(order, family, scientific.name, common.names, category, occurrence)
  
  
  if(exists("acad_sp_list")) {
    message("Performing calculations...")
  }
  
  
  #New species according to the NPSpecies list
  new_species <- x %>% 
    filter(!scientific.name %in% acad_sp_list$scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    group_by(scientific.name) %>% 
    slice(1)
  
  # #What about unconfirmed or species recorded as not present inside the park?
  # unconfirmed <- acad_sp_list %>% filter(occurrence == "Unconfirmed" | occurrence == "Not In Park")
  # 
  # test %>% 
  #   filter(scientific.name %in% unconfirmed$scientific.name)
  
  
  if(exists("new_species")) {
    message("Saving results to designated directory...")
  }
  
  
  if(length(new_species$scientific.name) >= 1) {
    write.csv(new_species, paste(output.path, "new_species.csv", sep = "/"))
  } else {
    message("There are no new species recorded in these data...")
  }
  
  
  if(exists("new_species")) {
    message("Calculations complete!")
  }
  
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
#' @examples  
#' make_leaflet(inat_lastweek) 


make_leaflet <- function (x) {
  
  formap <- x %>% 
    mutate(url = paste0("<b><a href='", url, "'>View observation<br>on iNaturalist</a></b>")) 
  
  map <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TerrainLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
               popup = formap$url)
  
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
#' @examples  
#' download_photos(inat_lastweek) 

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
  
  # Create the list of image urls from the 10 (or less) random inat obs
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
  map2(loop_list, loop_filenames, get_pics)
  
  # Write the data frame
  write.csv(pic_10random, paste(output.path, "summary_10random.csv", sep = "/"), row.names = F)
  
}




