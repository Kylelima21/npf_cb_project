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

# New park species
new_species <- read.csv("www/datasets/new_species.csv")

# T and E species
te_species <- read.csv("www/datasets/te_specieslist.csv")

# Rare species
rare_species <- read.csv("www/datasets/rare_specieslist.csv")

# Invasives and pests
invasive_species <- read.csv("www/datasets/invasive_pestslist.csv")

# Images
images <- data.frame(src = list.files('www/img/obs')) %>%
  tidyr::separate(col = 'src', c('id', 'user', "img.num", "type"), sep = '_|\\.', remove = FALSE) %>%
  rowwise() %>%
  mutate(#key = hashids::encode(1e3 + as.integer(img.num), hashid_settings(salt = 'this is my salt')),
         user = str_replace_all(user, "\\+", "_"),
         src = paste0("img/obs/", src)) %>% 
  arrange(img.num)




#### Shiny ui ####

ui <- function() {
  
  pagePiling(
    tags$head(includeCSS("www/css/styles.css")),
    sections.color = c('white', 'white', 'white', 'white', 'white', 'white'),
    opts = list(easing = "swing"),
    menu = c(
      "Home" = "home",
      "Summary Map" = "map",
      "Species Explorer" = "species",
      "Species of Interest" = "rare",
      "Gallery" = "gallery",
      "About" = "about"
    ),
    
    
    ## Home
    pageSectionImage(
      center = TRUE,
      img = "img/monarch.jpg",
      menu = "home",
      
      h1(textOutput("title"), class = "headerw shadow-dark"),
      
      h3("Citizen Science Explorer", class = "subheader shadow-dark"),
      
      absolutePanel(id = "logo", class = "panel", left = "1.5%", width = "auto", fixed = TRUE, draggable = FALSE,
                    a(tags$img(src = 'img/schoodic_horizontal.jpeg', height = '100%', width = 'auto'),
                      href = 'https://schoodicinstitute.org/', target = "_blank")),

      absolutePanel(id = "botbord", fixed = TRUE, draggable = FALSE),

      absolutePanel(class = "loglink twitter", fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("twitter_share", label = "", icon = icon("twitter"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://twitter.com/SchoodicInst"))),
      
      absolutePanel(class = "loglink fb", fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("facebook_share", label = "", icon = icon("facebook"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://www.facebook.com/SchoodicInstitute"))),
    
      absolutePanel(class = "loglink insta", fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("instagram_share", label = "", icon = icon("instagram"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://www.instagram.com/schoodicinst/"))),
      
      absolutePanel(class = "loglink youtube", fixed = TRUE, draggable = FALSE, height = "auto",
                    actionButton("youtube_share", label = "", icon = icon("youtube"), style = 'padding:6px',
                                 onclick = sprintf("window.open('%s')", 
                                                   "https://www.youtube.com/user/SchoodicInstitute"))),
      
      absolutePanel(class = "loglink linkedin", fixed = TRUE, draggable = FALSE, height = "auto",
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
                    
                    h2("Observations from the past week:"),br(),
                    h4(textOutput("total_observers"), align = "left"),br(),
                    h4(textOutput("top_taxa"), align = "left"),br(),
                    h4(textOutput("top_sp"), align = "left")
                    ),
      
      h1("?", class = "help"),
      
      HTML("<selection class='help-desc'>
            <h3>How to use this page:</h3>
            <h5><br/>1. Click on a circle marker to expand the sightings.<br/><br/>
               2. Hover your cursor over any blue marker to see the species.<br/><br/>
               3. Click on any blue marker to open a window with a link to the observation.</h5>")
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

                    h1("Species Explorer"),br(),

                    pickerInput("spselect",
                                label = "Select a species:",
                                choices = unique(the_data$common.name),
                                options = list(`live-search` = TRUE,
                                               size = 18,
                                               header = "Search Menu"),
                                selected = unique(the_data$common.name)[1],
                                multiple = FALSE))
    ),
    
    
    ## Rare/Threat Species
    pageSectionImage(
      img = "img/pollen.jpg",
      center = TRUE,
      menu = "rare",
      
      #h2("New Park Species")
      # textOutput("newsp"),
      # tableOutput("newsp_tab"), br(),
      # 
      # h2("Threatened/Endangered Species"),
      # textOutput("te"),
      # dataTableOutput("te_tab")
      
      h2("How are your observations being used?", class = "headerw"),
      textOutput("descrip_sp"),
      
      # pageRow(
      #   pageColumn(
      #     h2("New Park Species", class = "white"),
      #     textOutput("newsp"),br(),br(),br()),
      #   pageColumn(
      #     h2("Threatened/Endangered Species", class = "white"),
      #     textOutput("te")))
          
      # pageRow(
      #   pageColumn(
      #     h2("New Park Species", class = "white"),
      #     textOutput("newsp"),br(),br(),br()),
      #   pageColumn(
      #     h2("Threatened/Endangered Species", class = "white"),
      #     textOutput("te"),br(),br(),br()),
      #   pageColumn(
      #     h2("Rare Species", class = "white"),
      #     textOutput("rare")),
      #   pageColumn(
      #     h2("Species of Conservation Concern", class = "white"),
      #     textOutput("invasive")))
    ),
    
    
    ## Gallery
    pageSectionImage(
      img = "img/sch.JPG",
      center = TRUE,
      menu = "species",
      
      
      div(class='product',
          div(class='container',
              div(class='row',
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[1])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[1]),
                                      h5("©", images$user[1])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[2])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[2]),
                                      h5("©", images$user[2])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[3])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[3]),
                                      h5("©", images$user[3])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[4])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[4]),
                                      h5("©", images$user[4])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[5])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[5]),
                                      h5("©", images$user[5])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[6])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[6]),
                                      h5("©", images$user[6])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[7])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[7]),
                                      h5("©", images$user[7])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[8])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[8]),
                                      h5("©", images$user[8])))))),
                  div(class='col-xs-12 col-sm-6 col-md-4',
                      div(class='single-product',
                          div(class='box-area',
                              div(class='box-front',
                                  img(src=images$src[9])),
                              div(class='box-back text-center',
                                  div(class='back-content',
                                      h2(images$id[9]),
                                      h5("©", images$user[9]))))))
                  )))

      # column(10, uiOutput('lb'))
    ),
    
    
    ## About
    pageSectionImage(
      center = FALSE,
      img = "img/frog.jpg",
      menu = "about",
      
      absolutePanel(id = "logo", class = "panel", bottom = "1.5%", right = "2%", width = "auto", fixed = TRUE, draggable = FALSE,
                    a(tags$img(src = 'img/schoodic_horizontal.jpeg', height = '100%', width = 'auto'),
                      href = 'https://schoodicinstitute.org/', target = "_blank")),
    
      absolutePanel(id = "sources", class = "panel panel-default",
                    top = "10%", left = 25, 
                    width = "35%", height = "86%",
                    fixed = TRUE, draggable = FALSE,
              
                    h4("Last updated"),
                    textOutput("today"),
              
                    br(),h4("Background"),
                    "Info about the website, how, why it was built.",
              
                    br(),br(),h4("Code"),
                    "Code and required elements to generate this Shiny app are available on ", 
                    a("Github.", href = "https://github.com/Kylelima21/npf_cb_project", 
                      target = "_blank"),
              
                    br(),br(),h4("Sources"),
                    "Data supplied by ", a("iNaturalist",
                                           href = "https://www.inaturalist.org/",
                                           target = "_blank"), " and ", 
                    a("eBird", href = "https://www.ebird.org/", target = "_blank"),
              
                    br(),br(),h4("Authors"),
                    "Kyle Lima, Schoodic Institute at Acadia National Park",br(),
                    "Nicholas Fisichelli, Schoodic Institute at Acadia National Park",br(),
                    "Peter Nelson, Schoodic Institute at Acadia National Park",br(),
                    "Seth Benz, Schoodic Institute at Acadia National Park",br(),
                    "Catherine Schmidt, Schoodic Institute at Acadia National Park",
              
                    br(),br(),h4("Get in touch!"),
                    "Kyle Lima - klima@schoodicinstitute.org"),
                   
      absolutePanel(id = "data", class = "panel panel-default",
                    bottom = "1.5%", right = "41%", 
                    width = "20%", height = "27%",
                    fixed = TRUE, draggable = FALSE,
                    
                    h3("Download the past week's data here:"),
                    
                    downloadButton("downloadCsv", "Download as CSV"), br(), br(),
                    
                    "Data supplied by ", a("iNaturalist", 
                                           href = "https://www.inaturalist.org/", 
                                           target = "_blank"), " and ", 
                    a("eBird", href = "https://www.ebird.org/", target = "_blank"), 
                    " and modified by ",
                    a("Schoodic Institute at Acadia National Park.", 
                      href = 'https://schoodicinstitute.org/', target = "_blank"))
    )
  )
}





### SHINY SERVER ###

server <- function(input, output, session) {
  
  # output$lb <- renderUI({
  #   lightbox_gallery(images[sample(1:nrow(images), 8, replace = FALSE),], 'gallery', display = TRUE)
  # })
  
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
    paste("The most commonly reported species was", species$common.name[1],
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
          length(observers$user.id), "observers were reported to iNaturalist.", sep = " ")
  })
  
  ## Text output for species of interest description
  output$descrip_sp <- renderText({
    "Scientists at Acadia National Park are using your eBird and iNaturalist observations
         to find out when and where certain species are being seen. We have compiled a list of 
         species that are important to park managers. If they are reported by people like you,
         our program summarizes and reports that information to park managers."
  })

  ## Text output for new park species
  output$newsp <- renderText({
    if (length(new_species$scientific.name) >= 1) {
      return(paste0("There were ", length(unique(new_species$scientific.name)), " species recorded 
                    in the last 7 days that have not been recorded in the park before."))
    } else {
      return(paste0("There were no species recorded in the last 7 days that have not been 
                    recorded in the park before."))
    }
  })
  
  ## Text output for T/E species
  output$te <- renderText({
    if (length(te_species$scientific.name) >= 1) {
      return(paste0("There were ", length(unique(te_species$scientific.name)), " species recorded 
                    in the last 7 days that have not been recorded in the park before."))
    } else {
      return(paste0("There were no species recorded in the last 7 days that have not been 
                    recorded in the park before."))
    }
  })
  
  ## Text for rare species
  output$rare <- renderText({
    if (length(rare_species$scientific.name) >= 1) {
      return(paste0("There were ", length(unique(rare_species$scientific.name)), " species recorded 
                    in the last 7 days that have not been recorded in the park before."))
    } else {
      return(paste0("There were no species recorded in the last 7 days that have not been 
                    recorded in the park before."))
    }
  })
  
  ## Text for invasives and pests
  output$invasive <- renderText({
    if (length(invasive_species$scientific.name) >= 1) {
      return(paste0("There were ", length(unique(invasive_species$scientific.name)), 
                    " invasive and/or pest species recorded in the last 7 days."))
    } else {
      return(paste0("There were no invasive or pest species recorded in the last 7 days 
                    in the park."))
    }
  })
  
  ## Text for today's date
  output$today <- renderText({
    date <- today()
    format(date, "%B %d, %Y")
  })
  
  ## Output to download data as a CSV
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0("anp_citsci_data_", str_replace_all(today(), "-", ""), ".csv")
    },
    content = function(file) {
      inat_data_download = the_data %>% select(-user.id)
      
      write.csv(inat_data_download, file, row.names = F)
    })
  
}



#### Run the app ####

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)



