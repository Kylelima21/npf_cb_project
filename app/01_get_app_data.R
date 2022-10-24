# This script downloads the data and images that are to be updated daily.

# Source the functions
source("app/00_app_functions.R")

## Raw data
# Pull iNaturalist and eBird data
inat <- inat_recent("49610", "week", "app/www")
ebird <- ebird_recent("US-ME-009", "Acadia National Park")

# Make a df with 'groups' to add to the data
groups <- data.frame(iconic.taxon.name = c("Plantae", "Mammalia", "Animalia", "Aves", "Insecta", 
                                 "Reptilia", "Amphibia", "Fungi", "Protozoa", "Chromista",
                                 "Arachnida", "Mullusca"),
           groups = c("Plants", "Mammals", "Other animals", "Birds", "Insects", "Reptiles",
                      "Amphibians", "Fungi and lichens", "Protozoans", "Kelp and seaweeds", 
                      "Spiders", "Mullusks"))

# Combine the two data frames
final_data <- bind_rows(inat, ebird) %>% 
  mutate(common.name = tolower(common.name)) %>% 
  left_join(., groups, by = "iconic.taxon.name") %>% 
  select(scientific.name, common.name, iconic.taxon.name, groups, everything()) %>% 
  arrange(groups)

# Write out the data
write_csv(final_data, "app/the_data.csv")


## iNaturalist images
# Download the images to the www folder
download_photos(final_data, "app/www")



