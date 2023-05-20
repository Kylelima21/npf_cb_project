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
library(lwgeom)
library(purrr)
library(directlabels)
library(cowplot)

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
         longitude, user.login, everything()) %>% 
  mutate(scientific.name = ifelse(scientific.name == "Heterocampa umbrata", "Heterocampa pulverea", scientific.name),
         species = ifelse(species == "umbrata", "pulverea", species))

## Read, format, filter to ACAD, and clean the eBird data
ebd <- tibble(read.delim("data/ebd_US-ME_relFeb-2023.txt", header = T, quote = "")) %>% 
  select(c('COMMON.NAME', 'SCIENTIFIC.NAME', 'CATEGORY', 'OBSERVATION.DATE', 'OBSERVATION.COUNT', 
           'DURATION.MINUTES', 'SAMPLING.EVENT.IDENTIFIER', 'OBSERVER.ID', 'NUMBER.OBSERVERS',
           'PROTOCOL.TYPE', 'EFFORT.DISTANCE.KM', 'LOCALITY', 'COUNTY', 'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'checklist.id'='SAMPLING.EVENT.IDENTIFIER', 'latitude'='LATITUDE', 'longitude'='LONGITUDE',
         'observer.id'='OBSERVER.ID', 'category'='CATEGORY', 'county'='COUNTY', 'protocol'='PROTOCOL.TYPE',
         'duration.min'='DURATION.MINUTES', 'num.observers'='NUMBER.OBSERVERS', 'distance.km'='EFFORT.DISTANCE.KM') %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude") %>% 
  filter(obs.date <= "2022-12-31")




#------------------------------------------------#
####             Study area maps              ####
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


## Export figure
# ggsave(paste0("outputs/forpub/context_map_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 5.28, units = "in", dpi = 350)


## Read in the Acadia boundary layer
acad.bounds <- sf::read_sf("data/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
  st_transform(4326)

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
####             Summary Stats                ####
#------------------------------------------------#

### Total iNaturalist obs
## All observations
length(inat$common.name)

## Average submissions per observer
length(inat$common.name)/length(unique(inat$user.login))

## Get all obs that are research grade
rg <- inat %>% 
  filter(quality.grade == "research") 

length(rg$common.name)

## Percentage of observations that are research grade
paste0(round(length(rg$species)/length(inat$species)*100, digits = 2), "% of all observations are research grade.")


### Total species from iNaturalist
## Manipulate the data
inat_splist <- inat %>% 
  filter(!is.na(species) & quality.grade == "research") %>% 
  mutate(sci.name = paste(taxon, species, sep = " ")) %>% 
  select(sci.name) %>% 
  distinct() %>% 
  arrange(sci.name)

## Determine number of species
paste0("There have been ", length(inat_splist$sci.name), " species recorded from iNaturalist research grade observations")


#------------------------------------------------#


### Total eBird obs
length(ebd$common.name) 
  
## Total checklists
ebird_chk <- ebd %>% 
  distinct(checklist.id)

paste0("There have been ", length(ebird_chk$checklist.id), " checklists submitted by eBird users.")

## Average checklists per observer
length(ebird_chk$checklist.id)/length(unique(ebd$observer.id))

## Get all complete checklists
ebirdcomp <- ebd %>% 
  filter(protocol == "Traveling" | protocol == "Stationary" & duration.min >= 5) %>% 
  distinct(checklist.id)

length(ebirdcomp$checklist.id)

## Percentage of checklists that are complete
paste0(round(length(ebirdcomp$checklist.id)/length(ebird_chk$checklist.id)*100, digits = 2), "% of all checklists are complete.")


### Total species from eBird
## Manipulate the data
ebird_splist <- ebd %>% 
  filter(category == "species") %>% 
  select(scientific.name) %>% 
  distinct() %>% 
  arrange(scientific.name)

## Determine number of species
paste0("There have been ", length(ebird_splist$scientific.name), " species recorded by eBird users.")




#------------------------------------------------#
####        Total Observations Plots          ####
#------------------------------------------------#

### Plotting iNat observations over time
## Calculate number of research grade obs/month and format
rgtemp <- inat %>% 
  filter(quality.grade == "research") %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>% 
  arrange(date) %>% 
  mutate(data = "Research grade observations",
         tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))


## Calculate total number of obs/month and format
alltemp <- inat %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(common.name)) %>%
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
  labs(x = "Year", y = "Number of observations") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2022-12-31"))) +
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
  scale_color_manual(values = c("Total observations" = "gray50", "Research grade observations" = "black")) +
  scale_alpha_manual(values = c("Total observations" = 0.7, "Research grade observations" = 1)) +
  scale_linetype_manual(values = c("Total observations" = 1, "Research grade observations" = 1))


## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_inat_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 350)



### Summary stats
## Create full date sequence to add zeros into the data
datesinat <- tibble(date = seq(as.Date("1976/1/1"), as.Date("2022/12/1"), by = "month"))

## Create data frame for calculations
inatavg <- datesinat %>% 
  full_join(alltemp, keep.all = T) %>% 
  select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))

## Calculate average obs/month to 2014
earlyi <- inatavg %>% 
  filter(date <= "2014-01-01" & date >= "2000-01-01") 
mean(earlyi$tot.obs)
sd(earlyi$tot.obs)/sqrt(length(earlyi$tot.obs))

## Calculate average obs/month to current
toti <- inatavg %>% 
  filter(date >= "2000-01-01")
mean(toti$tot.obs)
sd(toti$tot.obs)/sqrt(length(toti$tot.obs))

## Winter months avg 2018 - 2022
winteri <- inatavg %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2017-12-01" & date <= "2022-02-01") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(winteri$tot.obs)
sd(winteri$tot.obs)/sqrt(length(winteri$tot.obs))


#------------------------------------------------#


### Plotting eBird observations over time
## Calculate number of complete checklists/month and format
ck_comp <- ebd %>% 
  mutate(month = month(obs.date),
         year = year(obs.date)) %>% 
  filter(year > 1957) %>%
  filter(protocol == "Traveling" | protocol == "Stationary" & duration.min >= 5) %>% 
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(unique(checklist.id))) %>% 
  arrange(date) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs),
         data = "Complete checklists")


## Calculate total number of checklists/month and format
tempck <- ebd %>% 
  mutate(month = month(obs.date),
         year = year(obs.date)) %>% 
  filter(year > 1957) %>%
  mutate(date = ym(paste0(year, "-", month))) %>% 
  group_by(date) %>% 
  summarise(tot.obs = length(unique(checklist.id))) %>% 
  arrange(date) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs),
         data = "Total checklists")


## Bind the data sets for plotting
ckcomb <- bind_rows(tempck, ck_comp)

  
## Plot 
ckcomb %>% 
  ggplot(aes(x = date, y = tot.obs, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
  theme_classic() +
  labs(x = "Year", y = "Number of checklists") +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "2 years"), 
               date_labels =  "%Y", 
               limits = c(as.Date("2004-01-01"), as.Date("2022-12-31"))) +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "12",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Total checklists" = "gray50", "Complete checklists" = "black")) +
  scale_alpha_manual(values = c("Total checklists" = 0.7, "Complete checklists" = 1)) +
  scale_linetype_manual(values = c("Total checklists" = 1, "Complete checklists" = 1))

## Export figure  
# ggsave(paste0("outputs/forpub/monthly_obs_ebird_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 350)



### Summary stats
## Create full date sequence to add zeros into the data
datesebird <- tibble(date = seq(as.Date("1958/1/1"), as.Date("2022/12/1"), by = "month"))

## Create data frame for calculations
ebirdavg <- datesebird %>% 
  full_join(tempck, keep.all = T) %>% 
  select(date, tot.obs) %>% 
  mutate(tot.obs = ifelse(is.na(tot.obs), 0, tot.obs))

## Calculate average checklists/month to 2010
earlye <- ebirdavg %>% 
  filter(date <= "2010-01-01" & date >= "2000-01-01")
mean(earlye$tot.obs)
sd(earlye$tot.obs)/sqrt(length(earlye$tot.obs))

## Calculate average checklists/month to current
tote <- ebirdavg %>% 
  filter(date >= "2000-01-01")
mean(tote$tot.obs)
sd(tote$tot.obs)/sqrt(length(tote$tot.obs))


## Winter months avg 2018 - 2022
wintere <- ebirdavg %>% 
  mutate(month = month(date)) %>% 
  filter(date >= "2017-12-01" & date <= "2022-02-01") %>% 
  filter(month == 12 | month == 1 | month == 2)
mean(wintere$tot.obs)
sd(wintere$tot.obs)/sqrt(length(wintere$tot.obs))




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
  #mutate(date = ym(paste0(year, "-", month))) %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(tot.obs = length(common.name)) %>%
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(tot.obs), 0, tot.obs),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Species") %>% 
  select(year, cumsum, data)


## Calculate cumulative observers
cumulativeob <- inat %>% 
  group_by(user.login) %>% 
  filter(observed.on == min(observed.on)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  #mutate(date = ym(paste0(year, "-", month))) %>% 
  mutate(year = year(observed.on)) %>% 
  group_by(year) %>% 
  summarise(observers = length(user.login)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Observers") %>% 
  select(year, cumsum, data)
  

## Bind these data for plotting
cumdata <- bind_rows(cumulativesp, cumulativeob)


## Plot
cumdata %>% 
  ggplot(aes(x = year, y = cumsum, color = data, alpha = data, linetype = data)) + 
  geom_line(size = 0.8) +
  geom_dl(data = subset(cumulativeob, year == 2022), aes(label = cumsum), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_dl(data = subset(cumulativesp, year == 2022), aes(label = cumsum), color = "black",
          method = list(dl.trans(x = x + 0.2), "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Count (n)") +
  scale_x_continuous(limits = c(1977, 2026), breaks = seq(1960, 2026, by = 10)) +
  # scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "2 years"), 
  #              date_labels =  "%Y", 
  #              limits = c(as.Date("2004-01-01"), as.Date("2022-12-31"))) +
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
# ggsave(paste0("outputs/forpub/monthly_cumulative_inat_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5.28, width = 8, units = "in", dpi = 350)


#------------------------------------------------#


### Plot cumulative species and observers from eBird data
## Calculate cumulative species
cumulativespe <- ebd %>%
  filter(category == "species") %>% 
  group_by(scientific.name) %>% 
  filter(obs.date == min(obs.date)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  # mutate(date = ym(paste0(year(obs.date), "-", month(obs.date)))) %>%
  mutate(year = year(obs.date)) %>% 
  group_by(year) %>% 
  summarise(tot.obs = length(scientific.name)) %>%
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(tot.obs), 0, tot.obs),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Species") %>% 
  select(year, cumsum, data)


## Calculate cumulative observers
cumulativeobe <- ebd %>% 
  group_by(observer.id) %>% 
  filter(obs.date == min(obs.date)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>% 
  # mutate(date = ym(paste0(year(obs.date), "-", month(obs.date)))) %>% 
  mutate(year = year(obs.date)) %>% 
  group_by(year) %>% 
  summarise(observers = length(observer.id)) %>% 
  arrange(year) %>% 
  mutate(cumsum = ifelse(is.na(observers), 0, observers),
         cumsum = cumsum(cumsum),
         cumsum = ifelse(is.na(cumsum), 0, cumsum),
         data = "Observers") %>% 
  select(year, cumsum, data)


## Plot observers
obse <- cumulativeobe %>% 
  ggplot(aes(x = year, y = cumsum)) + 
  geom_line(size = 0.8, color = "black") +
  geom_dl(data = subset(cumulativeobe, year == 2022), aes(label = cumsum),
          method = list(dl.trans(x = x + 0.2), cex = 1.4, "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Number of observers") +
  scale_x_continuous(limits = c(1957, 2029), breaks = seq(1960, 2026, by = 15)) +
  theme(legend.position = c(0.13, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "16",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "16"),
        axis.title = element_text(color = "black", size = "16"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))


## Plot species
spee <- cumulativespe %>% 
  ggplot(aes(x = year, y = cumsum)) + 
  geom_line(size = 0.8, color = "black") +
  geom_dl(data = subset(cumulativespe, year == 2021), aes(label = cumsum),
          method = list(dl.trans(x = x + 0.2), cex = 1.4, "last.points")) +
  theme_classic() +
  labs(x = "Year", y = "Number of species") +
  scale_x_continuous(limits = c(1957, 2029), breaks = seq(1960, 2026, by = 15)) +
  theme(legend.position = c(0.13, 0.85),
        legend.background = element_rect(color = "black", size = 0.4),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = "16",  margin = margin(0, 0, 0, 0.2, "cm")),
        axis.text = element_text(color = "black", size = "16"),
        axis.title = element_text(color = "black", size = "16"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))


## Create panelled figure
plot_grid(obse, spee, nrow = 1, labels = c('a', 'b'), align = "h", label_size = 16)


## Export figure
# ggsave(paste0("outputs/forpub/cumulative_ebird_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5, width = 11, units = "in", dpi = 350)




#------------------------------------------------#
####              Taxonomy stats              ####
#------------------------------------------------#

### eBird taxonomy stats
## Read in the eBird taxonomy for merging with ebd
tax <- read.csv("data/ebird_taxonomy_v2022.csv") %>% 
  select(scientific.name = SCI_NAME, order = ORDER1, family = FAMILY,
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
ebd %>% 
  mutate(count = ifelse(count == "X", 1, count),
         count = as.numeric(count)) %>% 
  group_by(common.name, scientific.name) %>% 
  summarize(frequency = round((length(scientific.name)/length(unique(ebd$checklist.id))), 2)) %>% 
  arrange(-frequency)


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
  select(scientific.name, observed.on:positional.accuracy) %>% 
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
  select(scientific.name, observed.on:positional.accuracy) %>% 
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




#------------------------------------------------#
####           Observation Heat Map           ####
#------------------------------------------------#

heat_inat <- inat %>% 
  filter(positional.accuracy < 50) %>% 
  select(scientific.name, latitude, longitude)

heat_ebd <- ebd %>% 
  select(scientific.name, latitude, longitude)

heatdat <- bind_rows(heat_inat, heat_ebd)
  

leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 300) %>%
  #addMapPane("polylines", zIndex = 301) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
  addPolygons(data = acad.bounds, color = "white", fill = T, fillColor = "black", opacity = 1, fillOpacity = 0.4,
              weight = 2.5, options = pathOptions(pane = "polygons")) %>%
  #addPolylines(data = ht, color = "blue", opacity = 1, weight = 1.8, options = pathOptions(pane = "polylines")) %>%
  addHeatmap(lng = heatdat$longitude, lat = heatdat$latitude, intensity = 2, max = 1, blur = 10, radius = 8)


saveWidget(inatheat, "outputs/temp.html", selfcontained = FALSE)
webshot("outputs/temp.html", file = "outputs/forpub/testplot.png",
        vwidth = 1100, vheight = 800,
        cliprect = "viewport")



leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addMapPane("polygons", zIndex = 300) %>%
  #addMapPane("polylines", zIndex = 301) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
  addPolygons(data = acad.bounds, color = "white", fill = T, fillColor = "black", opacity = 1, fillOpacity = 0.4,
              weight = 2.5, options = pathOptions(pane = "polygons")) %>%
  #addPolylines(data = ht, color = "blue", opacity = 1, weight = 1.8, options = pathOptions(pane = "polylines")) %>%
  addHeatmap(lng = heat_ebd$longitude, lat = heat_ebd$latitude, intensity = 2, max = 1, blur = 10, radius = 8)


saveWidget(inatheat, "outputs/temp.html", selfcontained = FALSE)
webshot("outputs/temp.html", file = "outputs/forpub/testplot.png",
        vwidth = 1100, vheight = 800,
        cliprect = "viewport")





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


# dat <- inat %>% 
#   filter(positional.accuracy < 10) %>% 
#   #slice(1:3) %>% 
#   st_as_sf(coords = c("longitude", "latitude")) %>% 
#   st_set_crs(4326)
# 
# 
# get_dist <- function (rownum) {
#   
#   hike <- dat %>% 
#     filter(row_number() == rownum) %>%
#     st_distance(ht) %>% 
#     as.data.frame() %>% 
#     pivot_longer(everything()) %>% 
#     filter(value == min(value)) %>% 
#     mutate(row = rownum) %>% 
#     select(row, hiking_trail = value)
# 
#   carriage <- dat %>% 
#     filter(row_number() == rownum) %>%
#     st_distance(cr) %>% 
#     as.data.frame() %>% 
#     pivot_longer(everything()) %>% 
#     filter(value == min(value)) %>% 
#     mutate(row = rownum) %>% 
#     select(row, carriage_road = value)
#   
#   road <- dat %>% 
#     filter(row_number() == rownum) %>%
#     st_distance(rds) %>% 
#     as.data.frame() %>% 
#     pivot_longer(everything()) %>% 
#     filter(value == min(value)) %>% 
#     mutate(row = rownum) %>% 
#     select(row, public_road = value)
#   
#   bike <- dat %>% 
#     filter(row_number() == rownum) %>%
#     st_distance(bp) %>% 
#     as.data.frame() %>% 
#     pivot_longer(everything()) %>% 
#     filter(value == min(value)) %>% 
#     mutate(row = rownum) %>% 
#     select(row, bike_path = value)
#   
#   final.dat <- left_join(hike, carriage, by = "row") %>% 
#     left_join(road, by = "row") %>% 
#     left_join(bike, by = "row") %>%
#     pivot_longer(cols = c(hiking_trail:bike_path)) %>% 
#     rename(access.type = name, distance = value)
#   
#   
#   return(final.dat)
#   
# }
# 
# 
# 
# points <- 1:length(dat$common.name)
# 
# point_distances <- map_dfr(points, ~get_dist(.))



