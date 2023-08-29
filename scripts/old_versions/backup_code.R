### Acadia National Park iNaturalist and eBird summaries
### Schoodic Institute at Acadia National Park, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(data.table)
library(auk)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(sf)
library(ggmap)
library(lwgeom)
library(purrr)
library(directlabels)
library(cowplot)
library(readxl)
library(raster)
library(geosphere)

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
  dplyr::select(-c(id, observed.on.string, created.at, updated.at, 
                   license:num.identification.disagreements, 
                   oauth.application.id, private.place.guess:species.guess)) %>% 
  mutate(taxon = str_extract(scientific.name, "^\\w*"),
         species = str_remove(scientific.name, "^\\w*"),
         species = str_trim(species, "both"),
         subspecies = str_extract(species, "\\s\\w*"),
         subspecies = str_trim(subspecies, "both"),
         species = str_extract(species, "[^\\s]+"),
         species = ifelse(species == "×", NA, species)) %>% 
  dplyr::select(common.name, scientific.name, taxon, species, subspecies, 
                iconic.taxon.name, observed.on, year, month, quality.grade, latitude, 
                longitude, user.login, everything()) %>% 
  mutate(scientific.name = ifelse(scientific.name == "Heterocampa umbrata", "Heterocampa pulverea", scientific.name),
         species = ifelse(species == "umbrata", "pulverea", species))

## Read, format, filter to ACAD, and clean the eBird data
ebd <- tibble(read.delim("data/ebd_US-ME_relFeb-2023.txt", header = T, quote = "")) %>% 
  dplyr::select(c('COMMON.NAME', 'SCIENTIFIC.NAME', 'CATEGORY', 'OBSERVATION.DATE', 'OBSERVATION.COUNT', 
                  'DURATION.MINUTES', 'SAMPLING.EVENT.IDENTIFIER', 'OBSERVER.ID', 'NUMBER.OBSERVERS',
                  'PROTOCOL.TYPE', 'EFFORT.DISTANCE.KM', 'LOCALITY', 'COUNTY', 'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'checklist.id'='SAMPLING.EVENT.IDENTIFIER', 'latitude'='LATITUDE', 'longitude'='LONGITUDE',
         'observer.id'='OBSERVER.ID', 'category'='CATEGORY', 'county'='COUNTY', 'protocol'='PROTOCOL.TYPE',
         'duration.min'='DURATION.MINUTES', 'num.observers'='NUMBER.OBSERVERS', 'distance.km'='EFFORT.DISTANCE.KM') %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude") %>% 
  filter(obs.date <= "2022-12-31")


## Read in the basemap for figures
acad.bm <- sf::read_sf("data/acad_boundary/formapping.shp")

## Read in the Acadia boundary layer
acad.bounds <- sf::read_sf("data/acad_boundary/acad_feeboundary_polygon.shp")




#------------------------------------------------#
####          Methods - Study Area            ####
#------------------------------------------------#

### Study area map
## Read in US map
states <- map_data("state")


## Plot context map
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = "gray50", show.legend = F) + 
  coord_fixed(1.3) +
  lims(x = c(-80, -66), y = c(38, 48)) +
  theme_nothing()


## Create Acadia bounds map
acadmap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 201) %>%
  addMapPane("labels", zIndex = 300) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(pane = "labels")) %>% 
  addPolygons(data = acad.bounds, color = "black", fill = T, fillColor = "forestgreen", opacity = 1, fillOpacity = 0.9,
              weight = .5, options = pathOptions(pane = "polygons")) %>% 
  addLegend("bottomright", values = ~acad.bounds,
            colors = c("forestgreen"),
            labels = c("Acadia National Park"),
            opacity = 1)
acadmap

## Export figure
saveWidget(acadmap, "outputs/temp.html", selfcontained = FALSE)
webshot("outputs/temp.html", file = "outputs/forpub/acadplot.png",
        vwidth = 1000, vheight = 700,
        cliprect = "viewport")




#------------------------------------------------#
####       Results - Taxonomic Summary        ####
#------------------------------------------------#

### eBird taxonomy stats
## Read in the eBird taxonomy for merging with ebd
tax <- read.csv("data/ebird_taxonomy_v2022.csv") %>% 
  dplyr::select(scientific.name = SCI_NAME, order = ORDER1, family = FAMILY,
                species.group = SPECIES_GROUP)


## Join the two 
ebdtax <- left_join(ebd, tax, by = "scientific.name")


## Determine how many orders were recorded
unique(ebdtax$order)


## Determine the percent of data made up by each order
ebdtax %>% 
  group_by(order) %>% 
  summarise(count = length(scientific.name)) %>% 
  arrange(-count) %>% 
  mutate(count = (count/length(ebd$scientific.name))*100)


## Calculate frequency of obs for each species
sp_freq <- ebd %>% 
  mutate(count = ifelse(count == "X", 1, count),
         count = as.numeric(count)) %>% 
  group_by(common.name, scientific.name) %>% 
  summarize(frequency = round((length(scientific.name)/length(unique(ebd$checklist.id))), 2)) %>% 
  arrange(-frequency)
sp_freq

sp_freq %>% 
  mutate(taxa = ifelse(contains("sp."), F, T))



## Calculate mean +- SE species per complete checklist
avg.ch <- ebd %>% 
  filter(category == "species") %>% 
  filter(protocol == "Traveling" | protocol == "Stationary" & duration.min >= 5) %>% 
  group_by(checklist.id) %>% 
  summarise(richness = length(scientific.name))
mean(avg.ch$richness)
sd(avg.ch$richness)/sqrt(length(avg.ch$richness))


#------------------------------------------------#


### iNaturalist taxonomy stats
## Read in the taxonomy data for merging
extratax <- tibble(data.frame(scientific.name = c("Datana contracta", "Pterospora andromedea"),
                              common.name = c("contracted datana moth", "woodland pinedrops"),
                              taxon.kingdom.name = c("Animalia", "Plantae"),
                              taxon.phylum.name = c("Arthopoda", "Tracheophyta"),
                              taxon.class.name = c("Insecta", "Magnoliopsida"),
                              taxon.order.name = c("Lepidoptera", "Ericales"),
                              taxon.family.name = c("Notodontidae", "Ericaceae")))

intax <- tibble(read.csv("data/inat_taxonomy.csv")) %>% 
  distinct() %>% 
  select_all(~gsub("\\_", ".", .)) %>% 
  mutate(scientific.name = ifelse(scientific.name == "Habronattus calcaratus maddisoni", "Habronattus calcaratus", scientific.name),
         scientific.name = ifelse(scientific.name == "Phytomyza minuscula group", "Phytomyza minuscula", scientific.name)) %>% 
  filter(!grepl("complex", common.name)) %>% 
  bind_rows(extratax)


## Total observations per species
sptots <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " ")) %>% 
  group_by(sci.name) %>% 
  summarise(total = length(sci.name)) %>% 
  arrange(-total) %>% 
  rename(scientific.name = sci.name) %>% 
  mutate(scientific.name = ifelse(scientific.name == "Boloria selene", "Boloria myrina", scientific.name),
         scientific.name = ifelse(scientific.name == "Heterocampa umbrata", "Heterocampa pulverea", scientific.name),
         scientific.name = ifelse(scientific.name == "Phalacrocorax auritus", "Nannopterum auritum", scientific.name),
         scientific.name = ifelse(scientific.name == "Speyeria aphrodite", "Argynnis aphrodite", scientific.name),
         scientific.name = ifelse(scientific.name == "Speyeria atlantis", "Argynnis atlantis", scientific.name),
         scientific.name = ifelse(scientific.name == "Lycaena phlaeas", "Lycaena hypophlaeas", scientific.name))


sptots2 <- left_join(sptots, intax, by = "scientific.name") %>%
  group_by(scientific.name) %>% 
  slice(1) %>% 
  arrange(-total)


## Totals species per kingdom
sptots2 %>% 
  group_by(taxon.kingdom.name) %>% 
  summarise(count = length(taxon.kingdom.name)) %>% 
  arrange(-count)


## Totals obs per kingdom
inat %>% 
  mutate(iconic.taxon.name = ifelse(iconic.taxon.name == "Insecta"  | iconic.taxon.name == "Mammalia" |
                                      iconic.taxon.name == "Aves"     | iconic.taxon.name == "Aves" |
                                      iconic.taxon.name == "Amphibia" | iconic.taxon.name == "Actinopterygii" |
                                      iconic.taxon.name == "Mollusca" | iconic.taxon.name == "Reptilia" |
                                      iconic.taxon.name == "Arachnida", "Animalia", iconic.taxon.name)) %>% 
  group_by(iconic.taxon.name) %>% 
  summarise(count = length(iconic.taxon.name)) %>% 
  arrange(-count)


## Total species per order Plantae
sptots2 %>% 
  filter(taxon.kingdom.name == "Plantae") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total obs per order Plantae
inat %>% 
  dplyr::select(scientific.name, observed.on:positional.accuracy) %>% 
  left_join(., intax, by = "scientific.name") %>% 
  filter(taxon.kingdom.name == "Plantae") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total species per order Animalia
sptots2 %>% 
  filter(taxon.kingdom.name == "Animalia") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total obs per order Animalia
inat %>% 
  dplyr::select(scientific.name, observed.on:positional.accuracy) %>% 
  left_join(., intax, by = "scientific.name") %>% 
  filter(taxon.kingdom.name == "Animalia") %>% 
  group_by(taxon.order.name) %>%
  summarise(count = length(taxon.order.name)) %>% 
  arrange(-count)


## Total orders
orders <- sptots2 %>% 
  filter(taxon.order.name != "") %>% 
  group_by(taxon.order.name) %>% 
  summarise(length = length(taxon.order.name))

length(orders$taxon.order.name)





