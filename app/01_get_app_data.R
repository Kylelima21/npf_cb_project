# This script downloads the data and images that are to be updated daily.

# Source the functions
source("app/00_app_functions.R")

## Raw data
# Pull iNaturalist and eBird data
inat <- inat_recent("49610", "week", "app/www")
ebird <- ebird_recent("US-ME-009", "Acadia National Park")

# Combine the two data frames
final_data <- bind_rows(inat, ebird) %>% 
  mutate(common.name = tolower(common.name))

# Write out the data
write_csv(final_data, "app/the_data.csv")


## iNaturalist images
# Download the images to the www folder
download_photos(final_data, "app/www")



