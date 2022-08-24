#FUNCTION
#Schoodic Institute at Acadia National Park

#inat_recent 

#This function takes a recent time span and returns all iNaturalist records from
#inside Acadia National Park boundaries during that time span. Additionally, this 
#function produces four data frames with summary statistics from the iNaturalist data.


#Parameter "timespan": The recent time span of interest. Options 
#are "week", "threeday", or "yesterday" as inputs for the "timespan" parameter.
#Parameter "output.path": The path you want the summary statistic tables to be written to.

#example: 
#example_data <- inat_recent("week", "outputs/inat_summary")



inat_recent <- function(timespan, output.path) {

  
  ##Check to make sure that parameter inputs are correct
  #timespan
  if (timespan != "week") {
    if(timespan != "threeday") {
      if(timespan != "yesterday") {
        stop("Entered time span is not accepted. Must be 'week', 'threeday' or 'yesterday'")
      }
    }
  } 
  
  
  #output.path
  if (str_sub(output.path, start = -1) == "/") {
    stop("Directory path cannot end with '/'")
  }

  
  #Setting up with packages
  require(tidyverse)
  require(rinat)
  require(purrr)
  
  
  #Get the past week's dates and format
  date.filter <- format(Sys.Date()-1:7, "%Y-%m-%d") %>% 
    as_tibble() %>% 
    rename(date = value) %>% 
    mutate(year = as.numeric(str_replace(date, "(\\d*)\\-\\d*\\-\\d*", "\\1")),
           month = as.numeric(str_replace(date, "\\d*\\-(\\d*)\\-\\d*", "\\1")),
           day = as.numeric(str_replace(date, "\\d*\\-\\d*\\-(\\d*)", "\\1")))


  #List the month and year for get_inat_obs sub-function
  year <- date.filter$year
  month <- date.filter$month

  
  #This is the function that starts the download of inat data inside park boundary
  get_inat_data <- function(obs_year, obs_month) {
  
    get_inat_obs(quality = "research",
                 place_id = "49610&amp",
                 geo = TRUE,
                 year = obs_year, 
                 month = obs_month, 
                 maxresults = 10000) %>% 
      as_tibble() %>% 
      select(scientific_name, common_name, iconic_taxon_name, observed_on, place_guess, 
             latitude, longitude, positional_accuracy, user_id, captive_cultivated) %>% 
      mutate(common_name = tolower(common_name)) %>% 
      rename_all( ~ str_replace_all(., "_", "."))
  
  }


  if(exists("get_inat_data")) {
    message("Asking nicely for data from iNaturalist...")
  }
  
  
  #Runs if week is called
  if (timespan == "week") {
    
  #Pull the previous week of inat data
  inat_obs <- map2_dfr(year, month, get_inat_data) %>% 
    filter(observed.on >= date.filter$date[7] & observed.on <= date.filter$date[1])
  
  }

  
  #Runs if threedays is called
  if (timespan == "threeday") {
    
  #Subset this to three days
  inat_obs <- map2_dfr(year, month, get_inat_data) %>% 
    filter(observed.on >= date.filter$date[3])
  
  }
  
  
  #Runs if yesterday is called
  if (timespan == "yesterday") {
    
  #Subset this to only yesterday
  inat_obs <- map2_dfr(year, month, get_inat_data) %>% 
    filter(observed.on == date.filter$date[1])
  
  }
  
  
  
  if(length(inat_obs) >= 1) {
    message("Caluclating summary statistics for time span...")
  } else {
    stop("There are no records in iNaturalist from the requested time span.")
  }
  
  
  ##Create summary tables
  #Number of obs in each taxon
  summary_taxon <- inat_obs %>% 
    group_by(iconic.taxon.name) %>% 
    summarise(count = length(iconic.taxon.name)) %>% 
    arrange(desc(count))
  
  #Number of observers = length, number of obs by user
  summary_observers <- inat_obs %>% 
    group_by(user.id) %>% 
    summarise(count = length(user.id)) %>% 
    arrange(desc(count))
  
  #Number of obs for each species
  summary_species <- inat_obs %>% 
    group_by(scientific.name) %>% 
    summarise(count = length(user.id)) %>% 
    arrange(desc(count))
  
  #Random selection of 10 species
  summary_10random <- inat_obs %>% 
    group_by(scientific.name) %>% 
    arrange(desc(observed.on)) %>% 
    distinct() %>% 
    slice(1) %>% 
    ungroup() %>% 
    sample_n(size = 10)
  
  
  #Write out the summary tables
  write.csv(summary_taxon, paste(output.path, "summary_taxon.csv", sep = "/"))
  write.csv(summary_observers, paste(output.path, "summary_observers.csv", sep = "/"))      
  write.csv(summary_species, paste(output.path, "summary_species.csv", sep = "/"))
  write.csv(summary_10random, paste(output.path, "summary_10random.csv", sep = "/"))
  
  
  if(length(inat_obs) >= 1) {
    message("Calculations complete!")
  }
  
  
  return(inat_obs) 
  
}

  

saveRDS(inat_recent, 'functions/inat_recent.rds')





  

  