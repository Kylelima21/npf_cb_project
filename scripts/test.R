source("functions/inat_functions.R")

acad <- "49610"
kaww <- "142267"

test <- inat_recent(acad, "week", "outputs")
test2 <- ebird_recent("US-ME-009")


new_npspecies(test, "outputs")

te_species(test, "outputs")

watchlist_species(test, "outputs")


make_leaflet(test)
download_photos(test, "outputs/photos")





te_species <- function(x, output.path) {
  
  
  ##Check to make sure that parameter inputs are correct
  #output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  #Custom name repair function to be used later
  custom_name_repair <- function(x) { tolower(gsub(" ", ".", x)) }
  
  
  if(exists("custom_name_repair")) {
    message("Gathering a list of threatened and endangered species...")
  }
  
  
  ##FEDERAL - An alternative data set from NPSpecies
  #Read in the file and filter for the T, E, and SC species
  fed_te_sp <- read_csv("data/federal_list_maine.csv") %>% 
    rename_with(tolower, everything()) %>% 
    select(scientific.name = "scientific name", common.name = "common name",
           listing.status = "esa listing status") %>% 
    mutate(level = "federal",
           listing.status = tolower(listing.status))
  
  
  ##STATE - An alternative data set from NPSpecies
  #Read in the file and filter for the T, E, and SC species
  state_te_sp <- read_csv("data/maine_thrt_end_list.csv") %>% 
    mutate(level = "state",
           status = tolower(listing.status))
  
  
  if(exists("state_te_sp")) {
    message("Performing calculations...")
  }
  
  
  #All T, E species from the last week
  te_specieslist_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, taxon = iconic.taxon.name, listing.status) %>% 
    distinct()
  
  
  #Number of species per T&E category
  te_summary_federal <- x %>% 
    filter(scientific.name %in% fed_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(fed_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, taxon = iconic.taxon.name, listing.status) %>% 
    distinct() %>% 
    group_by(listing.status) %>% 
    summarise(count = length(listing.status))
  
  
  #All T, E species from the last week
  te_specieslist_state <- x %>% 
    filter(scientific.name %in% state_te_sp$scientific.name) %>% 
    select(scientific.name, common.name, iconic.taxon.name) %>% 
    left_join(state_te_sp, by = "scientific.name") %>% 
    select(scientific.name, common.name = common.name.x, taxon = iconic.taxon.name, listing.status) %>% 
    distinct()
  
  
  #Number of species per T&E category
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



# download.file("https://irma.nps.gov/NPSpecies/Search/DownloadSpeciesListFullListWithDetailsResults", 
#               "data/raw/test",
#               method = "curl")
# 
# library(xml2)
# webpage_url <- "http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"
# webpage <- xml2::read_html(webpage_url)
# 
# ExOffndrsRaw <- rvest::html_table(webpage)[[1]] %>% 
#   tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
# ExOffndrsRaw %>% dplyr::glimpse(45)
# 
# webpage_url <- "https://irma.nps.gov/NPSpecies/Search/SpeciesList"
# webpage <- xml2::read_html(webpage_url)
# 
# rvest::html_table(webpage)[[2]] %>% 
#         as_tibble()
      
      