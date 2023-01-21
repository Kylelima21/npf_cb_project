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
  
  ## Check to make sure that parameter inputs are correct
  # output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  # Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  if(exists("custom_name_repair")) {
    message("Gathering a list of threatened and endangered species...")
  }
  
  
  ## FEDERAL
  fed_te_sp <- read_csv("data/federal_list_maine.csv") %>% 
    rename_with(tolower, everything()) %>% 
    select(scientific.name = "scientific name", common.name = "common name",
           listing.status = "esa listing status") %>% 
    mutate(level = "federal",
           listing.status = tolower(listing.status))
  
  
  ## STATE
  state_te_sp <- read_csv("data/maine_thrt_end_list.csv") %>% 
    mutate(level = "state",
           status = tolower(listing.status))
  
  
  if(exists("state_te_sp")) {
    message("Performing calculations...")
  }
  
  
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
  
  
  if(exists("te_summary_state")) {
    message("Saving results to designated directory...")
  }
  
  
  if(length(te_specieslist_federal$scientific.name) >= 1) {
    write.csv(te_specieslist_federal, paste(output.path, "te_specieslist_federal.csv", sep = "/"))
  } else {
    message("There are no federally threatened/endangered species recorded in these data...")
  }
  
  
  if(length(te_summary_federal$listing.status) >= 1) {
    write.csv(te_summary_federal, paste(output.path, "te_summary_federal.csv", sep = "/"))
  }
  
  
  if(length(te_specieslist_state$scientific.name) >= 1) {
    write.csv(te_specieslist_state, paste(output.path, "te_specieslist_state.csv", sep = "/"))
  } else {
    message("There are no state threatened/endangered species recorded in these data...")
  }
  
  
  if(length(te_summary_state$listing.status) >= 1) {
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
    mutate(url = paste0("<center><b><a href='", url, "'>View observation<br>online</a></b></center>"),
           latitude = jitter(latitude, factor = 16),
           longitude = jitter(longitude, factor = 16)) 
  
  maxLong = max(formap$longitude) - 0.05
  maxLat = max(formap$latitude) + 0.05 
  minLong = min(formap$longitude) + 0.05
  minLat = min(formap$latitude) - 0.05
  
  map <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TerrainLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
               popup = formap$url, #options = popupOptions(minWidth = 600),
               labelOptions = labelOptions(textsize = "15px")) %>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  
  return(map)
}




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
  fed_te_sp <- read_csv("app/www/datasets/federal_list_maine.csv") %>% 
    rename_with(tolower, everything()) %>% 
    select(scientific.name = "scientific name", common.name = "common name",
           listing.status = "esa listing status") %>% 
    mutate(level = "federal",
           listing.status = tolower(listing.status))
  
  
  ## State
  # Read in the file and filter for the T, E, and SC species
  state_te_sp <- read_csv("app/www/datasets/maine_thrt_end_list.csv") %>% 
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
  listsp <- read_excel("app/www/datasets/acad_watchlist_species.xlsx", .name_repair = custom_name_repair) 
  
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
