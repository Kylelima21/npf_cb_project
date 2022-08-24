#FUNCTION
#Schoodic Institute at Acadia National Park

#new_npspecies 

#This function takes a data frame of iNaturalist records (created specifically for the
#output of the "inat_recent()" function) and the path for the outputs. It creates .csv
#files of all the species in the data frame that have not been confirmed in the park 
#before. Nothing is returned in R, only written out to the provided directory.


#Parameter "x": The data frame of iNaturalist records.
#Parameter "output.path": The path you want the summary statistic tables to be written to.

#example: 
#new_npspecies(inat_lastweek, "outputs/new_park_species") 




new_npspecies <- function(x, output.path) {
  
  
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



saveRDS(new_npspecies, 'functions/new_npspecies.rds')






