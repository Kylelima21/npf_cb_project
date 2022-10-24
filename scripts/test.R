source("functions/inat_functions.R")

acad <- "49610"
kaww <- "142267"

test <- inat_recent(acad, "week", "outputs")
test2 <- ebird_recent("US-ME-009", "Acadia National Park")

test_comb <- combine_citsci_data(test, test2)

new_npspecies(test_comb, "outputs/acad_species_list.csv", "outputs")

watchlist_species(test_comb, "outputs/watchlist_summary")

make_leaflet(test_comb)

leaflet_summary(test_comb)

download_photos(test, "outputs/photos")




