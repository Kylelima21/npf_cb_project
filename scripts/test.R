source("functions/inat_functions.R")

acad <- "49610"
kaww <- "142267"

test <- inat_recent(acad, "week", "outputs")


new_npspecies(test, "outputs")

te_species(test, "outputs")

watchlist_species(test, "outputs")


make_leaflet(test)
download_photos(test, "outputs/photos")



library(rebird)

eb <- ebirdregion(loc = "US-ME-009", max = 10000, back = 7, key = "kjh86bnmkpfh")








# download.file("https://irma.nps.gov/NPSpecies/Search/DownloadSpeciesListFullListWithDetailsResults", 
#               "data/raw/test",
#               method = "curl")
# 
# library(xml2)
# webpage_url <- "http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"
# webpage <- xml2::read_html(webpage_url)
# 
# ExOffndrsRaw <- rvest::html_table(webpage)[[1]] %>% 
#   tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
# ExOffndrsRaw %>% dplyr::glimpse(45)
# 
# webpage_url <- "https://irma.nps.gov/NPSpecies/Search/SpeciesList"
# webpage <- xml2::read_html(webpage_url)
# 
# rvest::html_table(webpage)[[2]] %>% 
#         as_tibble()
      
      