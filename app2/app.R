## NPF funded project to present citizen science data to the public and land managers
## This app is specific to Acadia National Park, though the area of interest is easily changed


#### Starting up ####

## Packages
# Check for packages and install if not installed
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(rinat)) install.packages("rinat")
if(!require(lubridate)) install.packages("lubridate")
if(!require(leaflet)) install.packages("leaflet")
if(!require(shiny)) install.packages("shiny")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(bslib)) install.packages("bslib")
if(!require(fresh)) install.packages("fresh")
if(!require(png)) install.packages("png")
if(!require(grid)) install.packages("grid")
if(!require(purrr)) install.packages("purrr")
if(!require(readxl)) install.packages("readxl")
if(!require(rebird)) install.packages("rebird")
if(!require(downloader)) install.packages("downloader")
if(!require(sp)) install.packages("sp")
if(!require(rgdal)) install.packages("rgdal")
if(!require(fullPage)) install.packages("fullPage")
if(!require(hashids)) install.packages("hashids")

## Functions
# Source the functions
source("00_app_functions.R")
source('src/lightbox.R')

## Read in the data
# iNaturalist with some mods
the_data <- read.csv("www/datasets/the_data.csv") %>% 
  arrange(common.name)

# Observer summary
observers <- read.csv("www/datasets/summary_observers.csv")

# Species summary
species <- read.csv("www/datasets/summary_species.csv")

# Taxon summary
taxon <- read.csv("www/datasets/summary_taxon.csv")

# Images
images <- data.frame(src = list.files('www/img/obs')) %>%
  tidyr::separate(col = 'src', c('id', 'user', "img.num", "type"), sep = '_|\\.', remove = FALSE) %>%
  rowwise() %>%
  mutate(key = hashids::encode(1e3 + as.integer(img.num), hashid_settings(salt = 'this is my salt')),
         user = str_replace_all(user, "\\+", "_"))




#### Shiny ui ####

ui <- function() {
  
  pagePiling(
    sections.color = c('white', 'white', 'white', '#607B8B', 'white'),
    opts = list(easing = "swing"),
    menu = c(
      "Home" = "home",
      "Summary Map" = "map",
      "Species Explorer" = "species",
      "Gallery" = "gallery",
      "About" = "about"
    ),
    
    
    ## Home
    pageSectionImage(
      center = TRUE,
      img = "img/monarch.jpg",
      menu = "home",
      
      h1(textOutput("title"), class = "headerw shadow-dark",
         tags$head(includeCSS("www/css/styles.css"))),
      
      h3("Citizen Science Explorer", class = "subheader shadow-dark"),
      
      absolutePanel(id = "logo", class = "panel", bottom = "7.5%", left = "1%", width = "auto", fixed = TRUE, draggable = FALSE, height = "11%",
                    tags$a(href = 'https://schoodicinstitute.org/', tags$img(src = 'img/schoodic_horizontal.jpeg', height = '100%', width = 'auto'))),

      absolutePanel(id = "botbord", bottom = 0, left = 0, width = "100%",  height = "6%", fixed = TRUE, draggable = FALSE),
      
      absolutePanel(bottom = 0, left = 0, width = "100%", height = "8%", fixed = TRUE, draggable = FALSE,
                    tags$img(src = 'img/botstrip.JPG', height = 'auto', width = '100%')),
      
      absolutePanel(id = "loglink", bottom = "2%", left = 190, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("twitter_share", label = "", icon = icon("twitter"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://twitter.com/SchoodicInst"))),
      
      absolutePanel(id = "loglink", bottom = "2%", left = 150, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("facebook_share", label = "", icon = icon("facebook"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://www.facebook.com/SchoodicInstitute"))),
      
      absolutePanel(id = "loglink", bottom = "2%", left = 110, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("instagram_share", label = "", icon = icon("instagram"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://www.instagram.com/schoodicinst/"))),
      
      absolutePanel(id = "loglink", bottom = "2%", left = 70, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("youtube_share", label = "", icon = icon("youtube"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://www.youtube.com/user/SchoodicInstitute"))),
      
      absolutePanel(id = "loglink", bottom = "2%", left = 30, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("linkedin_share", label = "", icon = icon("linkedin"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://www.linkedin.com/company/schoodicinstitute/"))),
    ),
    
    
    ## Summary Map
    pageSection(
      center = TRUE,
      menu = "map",
      leafletOutput("mapsum", height = "100%"),
      
      absolutePanel(id = "instruct", class = "panel",
                    top = 15, right = 40, 
                    width = 360, height = "auto",
                    fixed = TRUE, draggable = FALSE,
                    
                    h2("Observations from the past week:"),
                    tags$br(),
                    h4(textOutput("total_observers"), align = "left"),
                    tags$br(),
                    h4(textOutput("top_taxa"), align = "left"),
                    tags$br(),
                    h4(textOutput("top_sp"), align = "left")
                    ),
      
      absolutePanel(id = "controls", class = "panel",
                    bottom = 20, left = 10, 
                    width = 360, height = "auto",
                    fixed = TRUE, draggable = FALSE,
                    
                    h4("How to use this page:"),
                    h5("Click on a circle marker to expand the sightings.", align = "left"),
                    h5("Hover your cursor over any blue marker to see the species.",
                       align = "left"),
                    h5("Click on any blue marker to open a window with a link to the observation.",
                       align = "left"))
    ), 
    
    
    ## Species Explorer
    pageSection(
      center = TRUE,
      menu = "species",
      leafletOutput("reactspmap", height = "100%"),

      absolutePanel(id = "instruct", class = "panel panel-default",
                    top = 40, right = 40, 
                    width = 325, height = "auto",
                    fixed = TRUE, draggable = FALSE,

                    h1("Species Explorer"),
                    tags$br(),

                    pickerInput("spselect",
                                label = "Select a species:",
                                choices = unique(the_data$common.name),
                                options = list(`live-search` = TRUE,
                                               size = 18,
                                               header = "Search Menu"),
                                selected = unique(the_data$common.name)[1],
                                multiple = FALSE))
    ),
    
    
    ## Gallery
    pageSection(
      center = TRUE,
      menu = "species",
      
      column(10, uiOutput('lb'))

    ),
    
    
    ## About
    pageSectionImage(
      center = FALSE,
      img = "img/frog.jpg",
      menu = "about",
      
      absolutePanel(id = "logo", class = "panel", bottom = "1.5%", right = "2%", width = "auto", fixed = TRUE, draggable = FALSE, height = "11%",
                    tags$a(href = 'https://schoodicinstitute.org/', tags$img(src = 'img/schoodic_horizontal.jpeg', height = '100%', width = 'auto'))),
      
      absolutePanel(id = "sources", class = "panel panel-default",
                    top = "10%", left = 25, 
                    width = "35%", height = "86%",
                    fixed = TRUE, draggable = FALSE,
              
                    tags$h4("Last updated"),
                    "This site is updated once daily.",
              
                    tags$br(),tags$br(),tags$h4("Background"),
                    "Info about the website, how, why it was built.",
              
                    tags$br(),tags$br(),tags$h4("Code"),
                    "Code and required elements to generate this Shiny app are available on ", tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
              
                    tags$br(),tags$br(),tags$h4("Sources"),
                    "Data supplied by ", tags$a(href="https://www.inaturalist.org/", "iNaturalist"), " and ", 
                    tags$a(href="https://www.ebird.org/", "eBird"),
              
                    tags$br(),tags$br(),tags$h4("Authors"),
                    "Kyle Lima, Schoodic Institute at Acadia National Park",tags$br(),
                    "Nicholas Fisichelli, Schoodic Institute at Acadia National Park",tags$br(),
                    "Peter Nelson, Schoodic Institute at Acadia National Park",tags$br(),
                    "Seth Benz, Schoodic Institute at Acadia National Park",tags$br(),
                    "Catherine Schmidt, Schoodic Institute at Acadia National Park",
              
                    tags$br(),tags$br(),tags$h4("Get in touch!"),
                    "Kyle Lima - klima@schoodicinstitute.org"),
                    
      absolutePanel(id = "button1", class = "panel",
                    bottom = "8%", left = "10%",
                    height = "auto", width = 200, 
                    fixed = TRUE, draggable = FALSE,
                    
                    pageButtonTo(h4("Return home", class = "white", align = "center"), section = 1)),
      
      absolutePanel(id = "data", class = "panel panel-default",
                    bottom = "1.5%", right = "41%", 
                    width = "20%", height = "27%",
                    fixed = TRUE, draggable = FALSE,
                    
                    h3("Download the past week's data here:"),
                    
                    downloadButton("downloadCsv", "Download as CSV"), tags$br(), tags$br(),
                    
                    "Data supplied by ", tags$a(href="https://www.inaturalist.org/", "iNaturalist"), " and ", 
                    tags$a(href="https://www.ebird.org/", "eBird"), " and modified by ",
                    tags$a(href="https://schoodicinstitute.org/", "Schoodic Institute at Acadia National Park."))
                   
    )
  )
}





### SHINY SERVER ###

server <- function(input, output, session) {
  
  output$lb <- renderUI({
    lightbox_gallery(images[sample(1:nrow(images), 8, replace = FALSE),], 'gallery', display = TRUE)
  })
  
  ## Title page header
  output$title <- renderText("Acadia National Park")
  
  ## Leaflet for the total festival
  output$mapsum <- renderLeaflet({ 
    leaflet_summary(read.csv("www/datasets/the_data.csv"))
  })
  
  ## Reactive data frame for Species Explorer tab
  speciesreact <- reactive({
    the_data %>%
      filter(common.name == input$spselect)
  })
  
  ## Reactive map to display species obs for Species Explorer tab - uses species_reactive_db
  output$reactspmap <- renderLeaflet({ 
    species_leaflet(speciesreact())
  })
  
  ## Text output for the top recorded species
  output$top_sp <- renderText({
    paste("The species with the most observations was", species$common.name[1],
          "with", species$count[1], "observations.", sep = " ")
  })
  
  ## Text output for the group with the most observations
  output$top_taxa <- renderText({
    paste("The group with the most observations was", taxon$iconic.taxon.name[1],
          "with", taxon$count[1], "observations.", sep = " ")
  })
  
  ## Text output for the number of total obs
  output$total_observers <- renderText({
    paste(sum(observers$count), "research-grade observations by", 
          length(observers$user.id), "observers.", sep = " ")
  })
  
  ## Output to download data as a CSV
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0("anp_citsci_data_", str_replace_all(today(), "-", ""), ".csv")
    },
    content = function(file) {
      inat_data_download = the_data %>% select(-user.id)
      
      write.csv(inat_data_download, file)
    })
  
}



#### Run the app ####

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)



