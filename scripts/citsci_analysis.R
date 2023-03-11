### Acadia National Park iNaturalist and eBird summaries
### Schoodic Institute at Acadia National Park, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(sf)
library(lwgeom)
library(purrr)

source("functions/analysis_functions.R")



#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

## Read, format, filter to ACAD, and clean the iNaturalist data
inat <- tibble(read.csv("data/acad_inat_obs.csv")) %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude") %>% 
  mutate(year = year(observed_on),
         month = month(observed_on)) %>% 
  rename_with(~str_replace_all(., "_", "."), .cols = everything()) %>% 
  select(-c(id, observed.on.string, created.at, updated.at, 
            license:num.identification.disagreements, 
            oauth.application.id, private.place.guess:species.guess)) %>% 
  mutate(taxon = str_extract(scientific.name, "^\\w*"),
         species = str_remove(scientific.name, "^\\w*"),
         species = str_trim(species, "both"),
         subspecies = str_extract(species, "\\s\\w*"),
         subspecies = str_trim(subspecies, "both"),
         species = str_extract(species, "[^\\s]+"),
         species = ifelse(species == "×", NA, species)) %>% 
  select(common.name, scientific.name, taxon, species, subspecies, 
         iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
         longitude, user.login, everything()) 


## Read, format, filter to ACAD, and clean the eBird data
ebd <- tibble(fread("data/all_ebird.csv")) %>% 
  select(species, genus, family, order, latitude = decimalLatitude, longitude = decimalLongitude,
         count = individualCount, day, month, year) %>%  
  filter_nps(., "Acadia National Park", "latitude", "longitude")





#------------------------------------------------#
####             Summary Stats                ####
#------------------------------------------------#

### Total species from iNaturalist
## Manipulate the data
inat_splist <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " ")) %>% 
  select(sci.name) %>% 
  distinct() %>% 
  arrange(sci.name)

## Determine number of species
paste0("There have been ", length(inat_splist$sci.name), " species recorded from research grade observations")


### Calculate the percent of all obs that are research grade
## Filter data to only research grade obs
rg <- inat %>% 
  filter(quality.grade == "research") 

## Determine percentage
paste0(round(length(rg$species)/length(inat$species)*100, digits = 2), "% of all observations are research grade.")




#------------------------------------------------#
####        Total Observations Plots          ####
#------------------------------------------------#

### Plotting iNat observations over time
## Create a df of all year/months that span iNat observations
alldates <- data.frame(date = seq.Date(as.Date("1976-01-01"), as.Date("2022-12-31"), by = "month"))

## Calculate number of research grade obs/month and format
rgtemp <- inat %>% 
  filter(quality.grade == "research") %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>% 
  right_join(., alldates, by = "date", all.y = T) %>% 
  arrange(date) %>% 
  mutate(data = "Research-grade observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))

## Calculate total number of obs/month and format
alltemp <- inat %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>%
  right_join(., alldates, by = "date", all.y = T) %>% 
  arrange(date) %>% 
  mutate(data = "Total observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))

## Bind the data sets for plotting
tempco <- bind_rows(alltemp, rgtemp)

## Plot 
tempco %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
  theme_classic() +
  labs(x = "Time", y = "Number of observations") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2022-12-31"))) +
  guides(color = guide_legend(override.aes = list(size = 6, shape = 15))) +
  theme(legend.position = c(0.23, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total observations" = "gray50", "Research-grade observations" = "black")) +
  scale_alpha_manual(values = c("Total observations" = 0.7, "Research-grade observations" = 1)) +
  scale_linetype_manual(values = c("Total observations" = 1, "Research-grade observations" = 1))
#"#138E0D", "#302c64"

## Export figure  
ggsave(paste0("outputs/forpub/monthly_obs_", str_replace_all(today(), "-", ""), ".png"), 
       height = 5.28, width = 8, units = "in", dpi = 350)





#------------------------------------------------#
####      Cumulative sp/observers Plots       ####
#------------------------------------------------#

### Plot cumulative species and observers from iNat data
## Calculate cumulative species (rg only)
cumulativesp <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " "),
         observed.on = as.Date(observed.on)) %>% 
  group_by(sci.name) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>%
  right_join(., alldates, by = "date", all.y = T) %>% 
  arrange(date) %>% 
  mutate(cumsum = ifelse(is.na(tot.obs), 0, tot.obs),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Species") %>% 
  select(date, cumsum, data)

## Calculate cumulative observers
cumulativeob <- inat %>% 
  group_by(user.login) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(observers = length(user.login)) %>% 
  right_join(., alldates, by = "date", all.y = T) %>% 
  arrange(date) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Observers") %>% 
  select(date, cumsum, data)
  
## Bind these data for plotting
cumdata <- bind_rows(cumulativesp, cumulativeob)

## Plot
cumdata %>% 
  ggplot(aes(x = date, y = cumsum, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
  theme_classic() +
  labs(x = "Time", y = "Number of observations") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2022-12-31"))) +
  #guides(color = guide_legend(override.aes = list(size = 6, shape = 15))) +
  theme(legend.position = c(0.13, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) +
    scale_color_manual(values = c("Observers" = "gray60", "Species" = "black")) +
    scale_alpha_manual(values = c("Observers" = 1, "Species" = 1)) +
    scale_linetype_manual(values = c("Observers" = 1, "Species" = 6))

## Export figure
ggsave(paste0("outputs/forpub/monthly_cumulatives_", str_replace_all(today(), "-", ""), ".png"), 
       height = 5.28, width = 8, units = "in", dpi = 350)




#------------------------------------------------#
####           Observation Heat Map           ####
#------------------------------------------------#

acad.bounds <- sf::read_sf("data/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
  st_transform(4326)

ht <- sf::read_sf("data/ACAD_HikingTrails_2018/ACAD_HikingTrails_2018.shp") %>% 
  st_transform(4326) %>% 
  mutate(id = seq(1:length(TRAILTYPE)))

bp <- sf::read_sf("data/ACAD_SCH_BikePaths/SchWds_BikePaths_LN.shp") %>% 
  st_transform(4326)

cr <- sf::read_sf("data/acad_carriageroads/CRd_LinesGPS2003.shp") %>% 
  st_transform(4326)

rds <- sf::read_sf("data/MaineDOT_Public_Roads/MaineDOT_Public_Roads.shp") %>% 
  st_transform(4326) %>% 
  st_crop(acad.bounds$geometry, .$geometry) %>% 
  mutate(ferry = str_detect(strtname, "FERRY$")) %>% 
  filter(ferry == F)


heatdat <- inat %>% 
  filter(positional.accuracy < 50)

  
leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 300) %>%
  addMapPane("polylines", zIndex = 301) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
  addPolygons(data = acad.bounds, color = "white", fill = T, fillColor = "black", opacity = 1, fillOpacity = 0.4,
               weight = 2.5, options = pathOptions(pane = "polygons")) %>%
  addPolylines(data = rds, color = "orange", opacity = 1, weight = 1.8, options = pathOptions(pane = "polylines")) %>% 
  addPolylines(data = ht, color = "blue", opacity = 1, weight = 1.8, options = pathOptions(pane = "polylines")) %>%
  addPolylines(data = cr, color = "limegreen", opacity = 1, weight = 1.8, options = pathOptions(pane = "polylines")) %>%
  addPolylines(data = bp, color = "purple", opacity = 1, weight = 1.8, options = pathOptions(pane = "polylines")) %>%
  addHeatmap(lng = heatdat[1,]$longitude, lat = heatdat[1,]$latitude, intensity = 2, max = 1, blur = 10, radius = 8)


dat <- inat %>% 
  filter(positional.accuracy < 10) %>% 
  #slice(1:3) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326)


get_dist <- function (rownum) {
  
  hike <- dat %>% 
    filter(row_number() == rownum) %>%
    st_distance(ht) %>% 
    as.data.frame() %>% 
    pivot_longer(everything()) %>% 
    filter(value == min(value)) %>% 
    mutate(row = rownum) %>% 
    select(row, hiking_trail = value)

  carriage <- dat %>% 
    filter(row_number() == rownum) %>%
    st_distance(cr) %>% 
    as.data.frame() %>% 
    pivot_longer(everything()) %>% 
    filter(value == min(value)) %>% 
    mutate(row = rownum) %>% 
    select(row, carriage_road = value)
  
  road <- dat %>% 
    filter(row_number() == rownum) %>%
    st_distance(rds) %>% 
    as.data.frame() %>% 
    pivot_longer(everything()) %>% 
    filter(value == min(value)) %>% 
    mutate(row = rownum) %>% 
    select(row, public_road = value)
  
  bike <- dat %>% 
    filter(row_number() == rownum) %>%
    st_distance(bp) %>% 
    as.data.frame() %>% 
    pivot_longer(everything()) %>% 
    filter(value == min(value)) %>% 
    mutate(row = rownum) %>% 
    select(row, bike_path = value)
  
  final.dat <- left_join(hike, carriage, by = "row") %>% 
    left_join(road, by = "row") %>% 
    left_join(bike, by = "row") %>%
    pivot_longer(cols = c(hiking_trail:bike_path)) %>% 
    rename(access.type = name, distance = value)
  
  
  return(final.dat)
  
}



points <- 1:length(dat$common.name)

point_distances <- map_dfr(points, ~get_dist(.))







  
leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 300) %>%
  addMapPane("polylines", zIndex = 301) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
  addPolygons(data = acad.bounds, color = "white", fill = T, fillColor = "black", opacity = 1, fillOpacity = 0.4,
              weight = 2.5, options = pathOptions(pane = "polygons")) %>%
  addPolylines(data = ht, color = "blue", opacity = 1, weight = 1.8, options = pathOptions(pane = "polylines")) %>%
  addPoints(lng = heatdat[1,]$longitude, lat = heatdat[1,]$latitude, intensity = 2, max = 1, blur = 10, radius = 8)
  

saveWidget(inatheat, "outputs/temp.html", selfcontained = FALSE)
webshot("outputs/temp.html", file = "outputs/forpub/testplot.png",
        vwidth = 1100, vheight = 800,
        cliprect = "viewport")


