#FUNCTION
#Schoodic Institute at Acadia National Park

#te_species 

#This function takes a data frame of iNaturalist records (created specifically for the
#output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#files of all the species in the data frame listed as species of concern, threatened, 
#and endangered as well as some summary statistics. This is done at both the federal and 
#state levels. Nothing is returned in R, only written out to the provided directory.


#Parameter "x": The data frame of iNaturalist records.
#Parameter "output.path": The path you want the summary statistic tables to be written to.

#example: 
#te_species(inat_lastweek, "outputs/te_species")



te_species <- function(x, output.path) {

  
  ##Check to make sure that parameter inputs are correct
  #output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }
  
  
  #Setting up with packages
  require(tidyverse)
  require(readxl)
  
  
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


saveRDS(te_species, 'functions/te_species.rds')

