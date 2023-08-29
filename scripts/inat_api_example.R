### This script downloads the data and images that are to be updated daily.

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(lubridate)
library(rinat)
library(purrr)
library(rebird)
library(sf)




#------------------------------------------------#
####           iNaturalist data               ####
#------------------------------------------------#

## Place ID comes from iNaturalist, this is for Hancock County
place_id <- "1647"

## Gather a year and month for data if you want
obs_year <- year(Sys.Date())
obs_month <- month(Sys.Date())

## This is the function that downloads inat data, but only so many records can be pulled
get_inat_obs(quality = "research",
             place_id = place_id,
             geo = TRUE,
             year = obs_year,
             month = obs_month,
             maxresults = 10000) %>%
  as_tibble() %>%
  select(scientific_name, common_name, iconic_taxon_name, observed_on, place_guess,
         latitude, longitude, positional_accuracy, user_login, user_id, captive_cultivated, url, image_url, license) %>%
  mutate(common_name = tolower(common_name)) %>%
  rename_all( ~ str_replace_all(., "_", "."))




