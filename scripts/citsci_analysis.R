### Acadia National Park iNaturalist and eBird summaries
### Schoodic Institute at Acadia National Park, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(data.table)
library(ggplot2)

source("functions/inat_functions.R")



#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

rginat <- tibble(fread("data/all_inat.csv")) %>% 
  filter(year != 2023) %>% 
  filter_gbif_to_park(., "Acadia National Park", "decimalLatitude", "decimalLongitude")

allinat <- tibble(read.csv("data/inat_totobs_2022.csv"))




#------------------------------------------------#
####             Plots and Maps               ####
#------------------------------------------------#

rgtemp <- rginat %>% 
  select(gbifID, species, latitude, longitude, day, month, year) %>% 
  group_by(year) %>% 
  summarise(rgobs = length(species))

alltemp <- allinat %>% 
  select(year, total.observations) %>% 
  mutate(total.observations = total.observations-shift(total.observations,1,type="lag")) %>% 
  filter(year > 1960)


ggplot() + 
  geom_line(aes(x = rgtemp$year, y = rgtemp$rgobs), linetype = 2) +
  geom_line(aes(x = alltemp$year, y = alltemp$total.observations)) +
  xlim(2000, 2022)












