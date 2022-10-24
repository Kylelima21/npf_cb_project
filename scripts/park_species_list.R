# Creating the species list for the park

# Packages
library(tidyverse)
library(data.table)
library(ridigbio)
source("functions/inat_functions.R")


# Read in the raw data from GBIF
inat <- fread("data/all_inat.csv") %>% 
  select(species, kingdom, phylum, class, latitude = decimalLatitude, longitude = decimalLongitude, coordinateUncertaintyInMeters)

ebird <- fread("data/all_ebird.csv") %>% 
  select(species, kingdom, phylum, class, latitude = decimalLatitude, longitude = decimalLongitude)


# Get iDigBio records  -  there are no records from waldo co. in the park so now excluded
hancock <- tibble(idig_search_records(rq = list(stateprovince = "Maine", county = "Hancock"))) %>% 
  select(species = scientificname, latitude = geopoint.lat, longitude = geopoint.lon) %>% 
  mutate(species = str_extract(species, "\\w*\\s\\w*"),
         species = str_to_sentence(species)) %>% 
  filter(!is.na(species) & !is.na(latitude) & !is.na(longitude))


# Read in SCAN collection records
scan <- tibble(read.csv("data/SCAN_collection_data.csv")) %>% 
  mutate(species = paste(genus, specificEpithet, sep = " ")) %>% 
  select(species, kingdom, phylum, class, latitude = decimalLatitude, longitude = decimalLongitude) %>% 
  filter(!is.na(latitude) & !is.na(longitude))


# Filter obs to those within ANP
inat_park <- filter_gbif_to_park(inat, "Acadia National Park", "latitude", "longitude")
ebird_park <- filter_gbif_to_park(ebird, "Acadia National Park", "latitude", "longitude")
hancock_park <- filter_gbif_to_park(hancock, "Acadia National Park", "latitude", "longitude") %>% 
  select(species)
scan_park <- filter_gbif_to_park(scan, "Acadia National Park", "latitude", "longitude")

eis <- tibble(bind_rows(inat_park, ebird_park)) %>% 
  bind_rows(., scan_park) %>% 
  select(species, kingdom, phylum, class) %>% 
  distinct() %>% 
  mutate(species = na_if(species, "")) %>% 
  filter(!is.na(species))

eis %>% 
  group_by(kingdom) %>% 
  tally()

all_species <- eis %>% 
  #select(species) %>% 
  bind_rows(., hancock_park) %>% 
  distinct()

# Write out the species list
write.csv(all_species, "outputs/acad_species_list.csv", row.names = F)
write.csv(all_species, "app/www/acad_species_list.csv", row.names = F)







