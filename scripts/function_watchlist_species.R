#FUNCTION
#Schoodic Institute at Acadia National Park

#watchlist_species 

#This function takes a data frame of iNaturalist records (created specifically for the
#output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#files of all the species in the data frame listed as non-native and invasive as well 
#as some summary statistics. This is done at both the federal and state levels. Nothing 
#is returned in R, only written out to the provided directory.


#Parameter "x": The data frame of iNaturalist records.
#Parameter "output.path": The path you want the summary statistic tables to be written to.

#example: 
#watchlist_species(inat_lastweek, "outputs/te_species")



watchlist_species <- function(x, output.path) {


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



saveRDS(watchlist_species, 'functions/watchlist_species.rds')





