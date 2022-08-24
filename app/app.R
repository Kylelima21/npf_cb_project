## NPF funded project to present citizen science data to the public and land managers
## This app is specific to Acadia National Park, though the area of interest is easily changed



#### Starting up ####

# Source the functions
source("inat_functions_public.R")

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

# Get the data
#inat_data <- inat_recent(49610, "week", "outputs") # 49610 is the iNaturalist place_id for Acadia NP 
#write.csv(inat_data, "app/inat_data.csv")
inat_data <- read.csv("inat_data.csv")

#download_photos(inat_data, "www")
photo_labs <- read.csv("www/summary_10random.csv")
observers <- read.csv("outputs/summary_observers.csv")
species <- read.csv("outputs/summary_species.csv")
taxon <- read.csv("outputs/summary_taxon.csv")


# mytheme <- create_theme(
#   theme = "default",
#   bs_vars_navbar(
#     default_bg = "#75b8d1",
#     default_color = "#FFFFFF",
#     default_link_color = "#FFFFFF",
#     default_link_active_color = "#75b8d1",
#     default_link_active_bg = "#FFFFFF",
#     default_link_hover_color = "firebrick"
#   ),
#   output_file = NULL
# )


#### Shiny ui ####

ui <- bootstrapPage(
  #tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style=";text-decoration:none;cursor:default;color:#71C546;" class="active" href="#">NPF Project</a>'), id="nav",
             windowTitle = "NPF Project",
  
  # navbarPage(
  #   title = "Custom navbar",
  #   header = tagList(use_theme(mytheme)), # <-- use your theme
             
             tabPanel("Summary",
                      div(class = "outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymapsum", width = "100%", height = "78.5%"),
                          
                          absolutePanel(id = "photobar", class = "card",
                                        bottom = 0, left = 0, width = "100%", fixed = TRUE,
                                        draggable = FALSE, height = "20%",
                                        
                                        tags$img(src = 'inat_obs_1.jpg', height = "100%", width = "auto"),
                                        tags$img(src = 'inat_obs_2.jpg', height = "100%", width = "auto"),
                                        tags$img(src = 'inat_obs_3.jpg', height = "100%", width = "auto"),
                                        tags$img(src = 'inat_obs_4.jpg', height = "100%", width = "auto"),
                                        tags$img(src = 'inat_obs_5.jpg', height = "100%", width = "auto"),
                                        tags$img(src = 'inat_obs_6.jpg', height = "100%", width = "auto"),
                                        tags$img(src = 'inat_obs_7.jpg', height = "100%", width = "auto"),
                                        tags$img(src = 'inat_obs_8.jpg', height = "100%", width = "auto")
                                        ),
                          
                          absolutePanel(id = "schoodic", class = "card", bottom = 180, left = 15, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                                        tags$a(href = 'https://schoodicinstitute.org/', tags$img(src = 'Schoodic_Stacked.jpeg', height = "100%", width = "auto"))
                                        ),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, right = 20, width = 300, fixed = TRUE,
                                        draggable = FALSE, height = "auto",
                                        
                                        h2("Observations from the past week:"),
                                        tags$br(),
                                        h4(textOutput("total_observers"), align = "left"),
                                        tags$br(),
                                        h4(textOutput("top_taxa"), align = "left"),
                                        tags$br(),
                                        h4(textOutput("top_sp"), align = "left")
                                        )
                                        
                      )
              ),
             
             tabPanel("Observation map",
                      div(class = "outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymapbig", width = "100%", height = "100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, right = 20, width = 250, fixed = TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        #span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                        h2(textOutput("box_title"), align = "left")
                                        #h4(textOutput("taxon_check_header"), align = "left"),
                                        
                                        # pickerInput("iconic.taxon.name",
                                        #             label = h3("Select groups of organisms to display on the map:"),
                                        #             choices = as.character(unique(inat_data$iconic.taxon.name)),
                                        #             options = list(`actions-box` = TRUE,
                                        #                            `none-selected-text` = span("Please select a taxon!", style = "color:white")),
                                        #             selected = as.character(unique(inat_data$iconic.taxon.name)),
                                        #             multiple = TRUE)
                                        
                                        #h6(textOutput("clean_date_reactive"), align = "right"),
                                        #h6(textOutput("reactive_country_count"), align = "right"),
                                        #plotOutput("epi_curve", height="130px", width="100%"),
                                        #plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        # sliderTextInput("observed.on",
                                        #                 label = h5("Select dates:"),
                                        #                 choices = format(as.Date(inat_data$observed.on, format = "%Y-%m-%d"), "%d %b %y"),
                                        #                 selected = format(today(), "%d %b %y"),
                                        #                 grid = FALSE,
                                        #                 animate = animationOptions(interval = 1, loop = FALSE))
                                        
                          ),

                          
                          absolutePanel(id = "logo", class = "card", bottom = 28, right = 58, width = 40, fixed = TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href = 'https://schoodicinstitute.org/', tags$img(src = 'Schoodic_Stacked.jpeg', height = '80', width = '80')))
                          
                      )
             ),
             
             
             tabPanel("Recent photos",
                      div(

                        HTML('<center><img src="inat_obs_1.jpg"></center>'),
                        h3(textOutput("phlab_1"), align = "center"),
                        tags$br(),tags$br(),

                        HTML('<center><img src="inat_obs_2.jpg"></center>'),
                        h3(textOutput("phlab_2"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_3.jpg"></center>'),
                        h3(textOutput("phlab_3"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_4.jpg"></center>'),
                        h3(textOutput("phlab_4"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_5.jpg"></center>'),
                        h3(textOutput("phlab_5"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_6.jpg"></center>'),
                        h3(textOutput("phlab_6"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_7.jpg"></center>'),
                        h3(textOutput("phlab_7"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_8.jpg"></center>'),
                        h3(textOutput("phlab_8"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_9.jpg"></center>'),
                        h3(textOutput("phlab_9"), align = "center"),
                        tags$br(),tags$br(),
                        
                        HTML('<center><img src="inat_obs_10.jpg"></center>'),
                        h3(textOutput("phlab_10"), align = "center"),
                        tags$br(),tags$br(),tags$br()
                        
                     )
   
              ),

             tabPanel("For land managers"),
             
             tabPanel("Data",
                      numericInput("maxrows", "Number of rows to display:", 25),
                      verbatimTextOutput("rawtable"),
                      downloadButton("downloadCsv", "Download as CSV"), tags$br(), tags$br(),
                      "Data supplied by ", tags$a(href="https://www.inaturalist.org/", "iNaturalist"), " and modified by ", 
                      tags$a(href="https://schoodicinstitute.org/", "Schoodic Institute at Acadia National Park."),
                      tags$br(),tags$br(),tags$br()
             ),
             
             tabPanel("About this site",
                      tags$div(
                        
                        HTML('<center><img src="Schoodic_Horizontal.jpeg" width="300" height="140px"></center>'),
                        
                        tags$h4("Last update"), 
                        "This site is updated once daily.",
                      
                        tags$br(),tags$br(),tags$h4("Background"), 
                        "Infr about the website, how, why it was built.",
                        
                        tags$br(),tags$br(),tags$h4("Code"),
                        "Code and required elements to generate this Shiny app are available on ", tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                        
                        tags$br(),tags$br(),tags$h4("Sources"),
                        tags$b("iNaturalist: "), "The data used to create this app are provided by",
                        tags$a(href="https://www.inaturalist.org/", "iNaturalist"), ".",
                
                        tags$br(),tags$br(),tags$h4("Authors"),
                        "Kyle Lima, Schoodic Institute at Acadia National Park",tags$br(),
                        "Nicholas Fisichelli, Schoodic Institute at Acadia National Park",tags$br(),
                        "Peter Nelson, Schoodic Institute at Acadia National Park",tags$br(),
                        "Seth Benz, Schoodic Institute at Acadia National Park",tags$br(),
                        "Catherine Schmidt, Schoodic Institute at Acadia National Park",
                        
                        tags$br(),tags$br(),tags$h4("Contact"),
                        "info@schoodicinstitute.org",tags$br(),tags$br(),tags$br()
                      ),
                      
                      absolutePanel(id = "logo", class = "card", bottom = 20, right = 180, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                                    actionButton("twitter_share", label = "", icon = icon("twitter"), style = 'padding:6px',
                                                 onclick = sprintf("window.open('%s')", 
                                                                   "https://twitter.com/SchoodicInst"))),
                      
                      absolutePanel(id = "logo", class = "card", bottom = 20, right = 140, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                                    actionButton("facebook_share", label = "", icon = icon("facebook"), style = 'padding:6px',
                                                 onclick = sprintf("window.open('%s')", 
                                                                   "https://www.facebook.com/SchoodicInstitute"))),
                      
                      absolutePanel(id = "logo", class = "card", bottom = 20, right = 100, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                                    actionButton("instagram_share", label = "", icon = icon("instagram"), style = 'padding:6px',
                                                 onclick = sprintf("window.open('%s')", 
                                                                   "https://www.instagram.com/schoodicinst/"))),
                      
                      absolutePanel(id = "logo", class = "card", bottom = 20, right = 60, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                                    actionButton("youtube_share", label = "", icon = icon("youtube"), style = 'padding:6px',
                                                 onclick = sprintf("window.open('%s')", 
                                                                   "https://www.youtube.com/user/SchoodicInstitute"))),
                      
                      absolutePanel(id = "logo", class = "card", bottom = 20, right = 20, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                                    actionButton("linkedin_share", label = "", icon = icon("linkedin"), style = 'padding:6px',
                                                 onclick = sprintf("window.open('%s')", 
                                                                   "https://www.linkedin.com/company/schoodicinstitute/")))
             ) 
             
  )         
)





### SHINY SERVER ###

server = function(input, output, session) {
  
  output$mymapsum <- renderLeaflet({ 
    make_leaflet(inat_data)
  })
  
  output$mymapbig <- renderLeaflet({ 
    make_leaflet(inat_data)
  })
  
  output$box_title <- renderText({
    paste("iNaturalist observations from the past 7 days")
  })
  
  # formatted_date = reactive({
  #   format(as.Date(inat_data$observed_on, format="%Y-%m-%d"), "%Y-%m-%d")
  # })
  # 
  # output$clean_date_reactive <- renderText({
  #   format(as.POSIXct(formatted_date()),"%d %B %Y")
  # })
  
  reactive_db = reactive({
    inat_data %>% 
      filter(date == formatted_date()) 
  })
   
  output$phlab_1 <- renderText({
    paste0(photo_labs$common.name[1], " (", photo_labs$scientific.name[1], ")")
  })
  
  output$phlab_2 <- renderText({
    paste0(photo_labs$common.name[2], " (", photo_labs$scientific.name[2], ")")
  })
  
  output$phlab_3 <- renderText({
    paste0(photo_labs$common.name[3], " (", photo_labs$scientific.name[3], ")")
  })
  
  output$phlab_4 <- renderText({
    paste0(photo_labs$common.name[4], " (", photo_labs$scientific.name[4], ")")
  })
  
  output$phlab_5 <- renderText({
    paste0(photo_labs$common.name[5], " (", photo_labs$scientific.name[5], ")")
  })
  
  output$phlab_6 <- renderText({
    paste0(photo_labs$common.name[6], " (", photo_labs$scientific.name[6], ")")
  })
  
  output$phlab_7 <- renderText({
    paste0(photo_labs$common.name[7], " (", photo_labs$scientific.name[7], ")")
  })
  
  output$phlab_8 <- renderText({
    paste0(photo_labs$common.name[8], " (", photo_labs$scientific.name[8], ")")
  })
  
  output$phlab_9 <- renderText({
    paste0(photo_labs$common.name[9], " (", photo_labs$scientific.name[9], ")")
  })
  
  output$phlab_10 <- renderText({
    paste0(photo_labs$common.name[10], " (", photo_labs$scientific.name[10], ")")
  })
  
  output$top_sp <- renderText({
    paste("The species with the most observations was", species$common.name[1],
          "with", species$count[1], "observations.", sep = " ")
  })
  
  output$top_taxa <- renderText({
    paste("The group with the most observations was", taxon$iconic.taxon.name[1],
          "with", taxon$count[1], "observations.", sep = " ")
  })
  
  output$total_observers <- renderText({
    paste(sum(observers$count), "research-grade observations by", 
          length(observers$user.id), "observers.", sep = " ")
  })
  

  
  # observeEvent(input$input_date, {
  #   leafletProxy("mymap") %>%
  #     # clearMarkers() %>%
  #     # clearShapes() %>%
  # 
  #     addMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude,
  #                      group = input$,
  #                      label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$cases_per_million, reactive_db()$deaths_per_million) %>% lapply(htmltools::HTML),
  #                      labelOptions = labelOptions(
  #                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
  #                        textsize = "15px", direction = "auto")) %>%
  # 
  #     addCircleMarkers(data = reactive_db_last7d(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5.5),
  #                      fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
  #                      label = sprintf("<strong>%s (7-day average)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db_last7d()$country, round(reactive_db_last7d()$new_cases/7,0), round(reactive_db_last7d()$new_deaths/7,0), round(reactive_db_last7d()$new_cases_per_million/7,1), round(reactive_db_last7d()$new_deaths_per_million/7,1)) %>% lapply(htmltools::HTML),
  #                      labelOptions = labelOptions(
  #                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
  #                        textsize = "15px", direction = "auto")) %>%
  # 
  #     addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
  #                      fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
  #                      label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per million: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$cases_per_million) %>% lapply(htmltools::HTML),
  #                      labelOptions = labelOptions(
  #                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
  #                        textsize = "15px", direction = "auto")) %>%
  # 
  #     addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4),
  #                      fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
  #                      label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
  #                      labelOptions = labelOptions(
  #                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
  #                        textsize = "15px", direction = "auto")) %>%
  # 
  #     addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
  #                      fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
  #                      label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
  #                      labelOptions = labelOptions(
  #                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
  #                        textsize = "15px", direction = "auto"))
  # })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0("inat_data_", str_replace_all(today(), "-", ""), ".csv")
    },
    content = function(file) {
      inat_data_download = inat_data %>% select(-user.id)
      
      write.csv(inat_data_download, file)
    }
  )
  
  output$rawtable <- renderPrint({
    inat_data_download = data.frame(inat_data) %>% select(-user.id)
    
    orig <- options(width = 1000)
    print(head(inat_data_download, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
}



#### Run the app ####

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)



