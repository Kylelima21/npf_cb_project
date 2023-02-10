## NPF funded project to present citizen science data to the public and land managers
## This app is specific to Acadia National Park, though the area of interest is easily changed


#### Starting up ####

## Functions
# Source the functions
source("00_app_functions.R")

## Data
# Read in the base data
the_data <- read.csv("www/datasets/the_data.csv") %>% 
  arrange(common.name)

# # Observer summary
# observers <- read.csv("www/datasets/summary_observers.csv")
# 
# # Species summary
# species <- read.csv("www/datasets/summary_species.csv")
# 
# # Taxon summary
# taxon <- read.csv("www/datasets/summary_taxon.csv")
# 
# # New park species
# new_species <- read.csv("www/datasets/new_species.csv")
# 
# # T and E species
# te_species <- read.csv("www/datasets/te_specieslist.csv")
# 
# # Rare species
# rare_species <- read.csv("www/datasets/rare_specieslist.csv")
# 
# # Invasives and pests
# invasive_species <- read.csv("www/datasets/invasive_pestslist.csv")

# Images
images <- data.frame(src = list.files('www/img/obs')) %>%
  tidyr::separate(col = 'src', c('id', 'user', "img.num", "type"), sep = '_|\\.', remove = FALSE) %>%
  rowwise() %>%
  mutate(user = str_replace_all(user, "\\+", "_"),
         src = paste0("img/obs/", src)) %>% 
  arrange(img.num)

options(dplyr.summarise.inform = FALSE)


#### Shiny ui ####

ui <- fluidPage(
  
  ## SET UP
  tags$head(
    tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("Acadia National Park Citizen Science Explorer"),
    tags$script(src = "css/index.js", type = "module", "defer")
  ),

  ### BODY
  tags$body(
    #div(class = "back-box"),
    
    ## Navigation
    div(class = "primary-header",
     div(class = "logo-one",
         tags$img(src = "img/cit_sci_explorer.png", alt = "Citizen science explorer logo",
                  class = "cs-logo")),
     div(class = "menu-nav-box",
         tags$nav(tags$ul(`aria-label` = "Primary navigation", role = "list",
                          tags$li(tags$a(href = "#", "Home")),
                          tags$li(tags$a(href = "#intro", "Introduction")),
                          tags$li(tags$a(href = "#summary", "Summary")),
                          tags$li(tags$a(href = "#spex", "Species Explorer")),
                          tags$li(tags$a(href = "#science", "Science")),
                          tags$li(tags$a(href = "#gallery", "Gallery")),
                          tags$li(tags$a(href = "#about", "About"))))),
     div(class = "logo-two",
         tags$img(src = "img/schoodic_stacked.jpeg", alt = "Schoodic Institute at Acadia National Park",
                  class = "logo"))),
    
    ## Home
    div(class = "titlebox",
       h1(textOutput("title"), class = "title-homepage"),
       h3("Citizen Science Explorer", class = "subtitle-homepage")
    ),
    div(class = "photo-cred",
        "Photo by ",
        a("Ben Tero", href = "https://www.instagram.com/btero/",
          target = "_blank")
        ),
    
    ## Intro
    div(class = "spacer", 
        div(class = "intro-box",
            div(class = "anchors",  id = "intro"),
            div(class = "body-title-box",
                icon("book-open",  class = "body-box-icon"), 
                h4("Introduction", class = "body-titles")),
            div(class = "intro-content",
                img(src = "img/citsci.jpg", alt = "A group of citizen scientists collecting data
                    with iNatuarlist", class = "citsci-image"),
                div(class = "intro-text",
                    h2("Welcome to the Acadia National Park citizen science explorer!"),
                    h3("Here you will find summaries of Acadia National Park iNaturalist and eBird records from the last week.
                        In addition to summaries, we present some recent science that has been made possible
                        by your contributions to these citizen science projects. We hope that you enjoy
                        exploring recent sightings in the area, and understand how critical to science your
                        time and effort are when you contribute to citizen science efforts."),
                    div(class = "intro-three",
                        img(src = "img/trsw.jpeg", alt = "Tree Swallow sitting on a wooden post"),
                        img(src = "img/frog.jpg", alt = "Northern Leopard Frog in a gravel path"),
                        img(src = "img/monarch.jpg", alt = "Monarch feeding from a red clover flower"))
                    ))
                    
    )),
    
    ## Data summary
    div(class = "summary-box", 
        div(class = "anchors", id = "summary"),
        div(class = "body-title-box",
            icon("table",  class = "body-box-icon"), 
            h4("Data Summary", class = "body-titles")),
        div(class = "inat-box", 
            img(src = "img/inat.png", alt = "iNaturalist", class = "obs-logos"),
            div(class = "sep-line"),
            div(class = "inat-display-grid",
                div(class = "observers-format",
                    h4(tags$b("Observers")),
                    icon("users"),
                    h2(textOutput("total_observers"), class = "summary-stat-text")),
                div(class = "observations-format",
                    h4(tags$b("Observations")),
                    icon("camera-retro"),
                    h2(textOutput("total_observations"), class = "summary-stat-text")),
                div(class = "comgroup-format",
                    h4(tags$b("Most Common Group")),
                    icon("bacteria"),
                    h2(textOutput("top_taxa"), class = "summary-stat-text")),
                div(class = "comsp-format",
                    h4(tags$b("Most Common Species")),
                    icon("leaf"),
                    h2(textOutput("top_sp"), class = "summary-stat-text")),
                div(class = "percent-format",
                    icon("database"),
                    h2(textOutput("percent_text_i"), class = "percent-stat-text")))),
        div(class = "ebird-box",
            img(src = "img/ebird.png", alt = "eBird", class = "obs-logos"),
            div(class = "sep-line"),
            div(class = "ebird-display-grid",
                div(class = "observers-format",
                    h4(tags$b("Checklists")),
                    icon("list-check"),
                    h2(textOutput("total_checklists_e"), class = "summary-stat-text")),
                div(class = "observations-format",
                    h4(tags$b("Total Species")),
                    icon("feather"),
                    h2(textOutput("total_sp_e"), class = "summary-stat-text")),
                div(class = "comgroup-format",
                    h4(tags$b("Total Birds")),
                    icon("binoculars"),
                    h2(textOutput("total_birds_e"), class = "summary-stat-text")),
                div(class = "comsp-format",
                    h4(tags$b("Most Frequent Species")),
                    icon("crow"),
                    h2(textOutput("top_sp_e"), class = "summary-stat-text")),
                div(class = "percent-format",
                    icon("database"),
                    h2(textOutput("percent_text_e"), class = "percent-stat-text"))))
    ),

    ## Species explorer
    div(class = "spex-box",
        div(class = "anchors", id = "spex"),
        div(class = "body-title-box",
            icon("tree",  class = "body-box-icon"), 
            h4("Species Explorer", class = "body-titles")),
        div(class = "picker-box",
            pickerInput("spselect",
                        label = "Select a species:",
                        choices = unique(the_data$common.name),
                        options = list(`live-search` = TRUE,
                                       size = 11,
                                       header = "Search Menu"),
                        selected = unique(the_data$common.name)[1],
                        width = "100%",
                        multiple = FALSE)),
          div(class = "map-box",
              leafletOutput("reactspmap", height = "100%"),
              div(class = "help",
                  icon("circle-info")),
              div(class = "help-desc",
                  h5("1. Click on a circle marker to expand the sightings."),
                  h5("2. Hover your cursor over any blue marker to see the species."),
                  h5("3. Click on any blue marker to open a window with a link to the observation.")))),
    
    ## Science
    div(class = "science-box",
        div(class = "anchors", id = "science"),
        div(class = "body-title-box",
            icon("microscope",  class = "body-box-icon"), 
            h4("Science", class = "body-titles")),
        div(class = "science-content",
            img(src = "img/atsp.jpg", alt = "An American Tree Sparrow perched in a shrub", class = "science-img"),
            div(class = "science-text",
                h2("Acadia National Park Winter Birds: 51 Years of Change Along the Coast of Maine"),
                h3("This project analyzed 51 years of citizen science effort from the National Audubon Society
                   Christmas Bird Counts of Acadia National Park."),
                a(href = "https://schoodicinstitute.org/new-research-shows-dramatic-declines-in-acadia-national-park-winter-bird-populations/", 
                  target = "_blank", tabindex = "-1", tags$button(class = "btn-green", "Learn more"))))),
                
    ## Gallery
    div(class = "box-photo-gallery",
        div(class = "anchors", id = "gallery"),
        div(class = "body-title-box",
            icon("image",  class = "body-box-icon"),
            h4("Photo Gallery", class = "body-titles")),
        div(class = "grid-wrapper",
            div(tabindex = 0, class = "img-container",
                img(src = images$src[1]),
                div(class = "img-label",
                    h3(images$id[1]),
                    h4("©", images$user[1]))),
            div(tabindex = 0, class = "img-container",
                img(src = images$src[2]),
                div(class = "img-label",
                    h3(images$id[2]),
                    h4("©", images$user[2]))),
            div(tabindex = 0, class = "img-container",
                img(src = images$src[3]),
                div(class = "img-label",
                    h3(images$id[3]),
                    h4("©", images$user[3]))),
            div(tabindex = 0, class = "img-container",
                img(src = images$src[4]),
                div(class = "img-label",
                    h3(images$id[4]),
                    h4("©", images$user[4]))),
            div(tabindex = 0, class = "img-container",
                img(src = images$src[5]),
                div(class = "img-label",
                    h3(images$id[5]),
                    h4("©", images$user[5]))),
            div(tabindex = 0, class = "img-container",
                img(src = images$src[6]),
                div(class = "img-label",
                    h3(images$id[6]),
                    h4("©", images$user[6]))),
            div(tabindex = 0, class = "img-container",
                img(src = images$src[7]),
                div(class = "img-label",
                    h3(images$id[7]),
                    h4("©", images$user[7]))),
            div(tabindex = 0, class = "img-container",
                img(src = images$src[8]),
                div(class = "img-label",
                    h3(images$id[8]),
                    h4("©", images$user[8])))
            )
        ),
    
    ## About
    div(class = "about-grid-box",
        div(class = "anchors", id = "about"),
        div(class = "body-title-box",
            icon("circle-info",  class = "body-box-icon"),
            h4("About This Page", class = "body-titles")),
        div(class = "about-info-box",
            h4("Background"),
            "This project was made possible in part by a grant from the National Park Foundation. 
            Due to this grant, ",
            a("Schoodic Institute at Acadia National Park", href = "https://schoodicinstitute.org/",
              target = "_blank"),
            "was able to create this Shiny application to communicate citizen science in parks.",
            h4("Code"),
            "Code and required elements to generate this Shiny app are available on ",
            a("Github.", href = "https://github.com/Kylelima21/npf_cb_project",
              target = "_blank"),
            h4("Sources"),
            "Data supplied by ", a("iNaturalist",
                                   href = "https://www.inaturalist.org/",
                                   target = "_blank"), " and ",
                                 a("eBird.", href = "https://www.ebird.org/", 
                                   target = "_blank"),
            h4("Get in Touch!"),
            "Contact Kyle Lima with any questions or concerns at klima@schoodicinstitute.org",
            h6(textOutput("today"))),
        div(class = "about-download-box",
            tags$img(src = "img/cit_sci_explorer.png", alt = "Citizen science explorer logo", class = "about-logo"),
            h4("Download the past week's data here:"),
            div(class = "download-button",
                downloadButton("downloadCsv", "Download as CSV", class = "btn-purple")),
            h5("Data supplied by iNaturalist and eBird and modified by Schoodic Institute at Acadia National Park.",
               )
            )
        ),
        
    ## Footer
    tags$footer(
          div(class = "footer-background",
              div(class = "footer-grid-box",
                  div(class = "footer-left-box",
                      div(tags$img(src = "img/schoodic_horizontal.jpeg", alt = "Schoodic Institute", class = "footer-logo")),
                      div(class = "logo-text", tags$em("Our mission is inspiring science, learning, and community for a changing world."))),
  
                  div(class = "footer-nav-box",
                      tags$ul(`aria-label` = "Footer navigation", role = "list",
                              tags$li(tags$a(href = "#", "Home")),
                              tags$li(tags$a(href = "#intro", "Introduction")),
                              tags$li(tags$a(href = "#summary", "Summary")),
                              tags$li(tags$a(href = "#spex", "Species Explorer")),
                              tags$li(tags$a(href = "#science", "Science")),
                              tags$li(tags$a(href = "#gallery", "Gallery")),
                              tags$li(tags$a(href = "#about", "About")))),
  
                  div(class = "footer-copyright-box",
                      textOutput("copyright_txt")),
  
                  div(class = "footer-link-box",
                      div(class = "footer-button-position",
                          tags$a(href = "https://schoodicinstitute.org/", target = "_blank", tabindex = "-1",
                                 tags$button(class = "btn-purple", "Visit our website!"))),
                      div(class = "footer-link-wrapper",
                          tags$ul(`aria-list` = "Social media links", role = "list",
                                  tags$li(tags$a(`aria-label` = "Facebook", class = "footer-links",
                                                 href = "https://www.facebook.com/SchoodicInstitute",
                                                 target = "_blank",
                                                 icon("facebook"))),
                                  tags$li(tags$a(`aria-label` = "Twitter", class = "footer-links",
                                                 href = "https://twitter.com/SchoodicInst",
                                                 target = "_blank",
                                                 icon("twitter"))),
                                  tags$li(tags$a(`aria-label` = "Instagram", class = "footer-links",
                                                 href = "https://www.instagram.com/schoodicinst/",
                                                 target = "_blank",
                                                 icon("instagram"))),
                                  tags$li(tags$a(`aria-label` = "Youtube", class = "footer-links",
                                                 href = "https://www.youtube.com/user/SchoodicInstitute",
                                                 target = "_blank",
                                                 icon("youtube"))),
                                  tags$li(tags$a(`aria-label` = "Linked in", class = "footer-links",
                                                 href = "https://www.linkedin.com/company/schoodicinstitute/",
                                                 target = "_blank",
                                                 icon("linkedin"))))))
                  )
                  )
      )
  )
)







### SHINY SERVER ###

server <- function(input, output, session) {
  
  ## Title page header
  output$title <- renderText("Acadia National Park")
  
  ## Leaflet for eBird obs
  output$emap <- renderLeaflet({ 
    leaflet_summary(the_data %>% filter(source == "eBird"))
  })
  
  ## Leaflet for eBird obs
  output$imap <- renderLeaflet({ 
    leaflet_summary(the_data %>% filter(source == "iNaturalist"))
  })
  
  ## Pie Chart
  output$percentplot <- renderPlot({
    the_data %>%
      group_by(source) %>% 
      summarise(count = length(source)) %>% 
      mutate(category = "citsci",
             percent = round((count/sum(count)*100), 0)) %>%
      ggplot(aes(x = category, y = percent, fill = source)) +
      geom_col() + 
      coord_flip() +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(aes(x = category, y = percent, label = percent, group = source),
                position = position_stack(vjust = 0.5), size = 7, color = "#eae7e7") +
      scale_fill_manual(values = c("#0d042ad9", "#0c3e13d9")) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            plot.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 20, color = "black"),
            legend.key.size = unit(1, "cm"),
            legend.spacing.x = unit(1, "cm"),
            legend.position = "bottom")
  }, bg = "transparent")
  
  ## Percent plot text output
  output$percent_text_i <- renderText({
    dat <- the_data %>%
      group_by(source) %>% 
      summarise(count = length(source)) %>% 
      mutate(percent = round((count/sum(count)*100), 0))
    
    paste0(dat$percent[2], "% of the data was collected by iNaturalist")
  })
  
  output$percent_text_e <- renderText({
    dat <- the_data %>%
      group_by(source) %>% 
      summarise(count = length(source)) %>% 
      mutate(percent = round((count/sum(count)*100), 0))
    
    paste0(dat$percent[1], "% of the data was collected by eBird")
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
  
  ### iNat
  ## Text output for the top recorded species
  output$top_sp <- renderText({
    species <- the_data %>% 
      filter(source == "iNaturalist") %>% 
      group_by(scientific.name, common.name) %>%
      summarise(count = length(user.id)) %>%
      arrange(desc(count))
      
    paste(species$common.name[1])
          #, " (", species$count[1], " observations)")
  })
  
  ## Text output for the group with the most observations
  output$top_taxa <- renderText({
    taxon <- the_data %>%
      filter(source == "iNaturalist") %>% 
      group_by(iconic.taxon.name) %>%
      summarise(count = length(iconic.taxon.name)) %>%
      arrange(desc(count))
    
    paste(taxon$iconic.taxon.name[1])
          #" (", taxon$count[1], " observations)")
  })
  
  ## Text output for the number of total observers
  output$total_observers <- renderText({
    observers <- the_data %>%
      filter(source == "iNaturalist") %>% 
      group_by(user.id, user.login) %>%
      summarise(count = length(user.id)) %>%
      arrange(desc(count))
    
    paste0(length(observers$user.id))
  })
  
  ## Total observations
  output$total_observations <- renderText({
    observers <- the_data %>%
      filter(source == "iNaturalist") %>% 
      group_by(user.id, user.login) %>%
      summarise(count = length(user.id)) %>%
      arrange(desc(count))
    
    paste0(sum(observers$count))
  })
  
  ### eBird
  ## Text output for the top recorded species
  output$top_sp_e <- renderText({
    species <- the_data %>% 
      filter(source == "eBird") %>% 
      group_by(scientific.name, common.name) %>%
      summarise(count = length(common.name)) %>%
      arrange(desc(count))
    
    paste(species$common.name[1])
  })
  
  ## Text output for the group with the most observations
  output$total_sp_e <- renderText({
    taxon <- the_data %>%
      filter(source == "eBird") %>% 
      group_by(common.name) %>%
      summarise(count = length(common.name)) %>% 
      arrange(desc(count))
    
    paste(length(taxon$common.name))
  })
  
  ## Text output for the number of total checklists
  output$total_checklists_e <- renderText({
    observers <- the_data %>%
      filter(source == "eBird") %>% 
      group_by(checklist) %>%
      summarise(count = length(unique(checklist))) %>%
      arrange(desc(count))
    
    paste(length(observers$checklist))
  })
  
  ## Text output for the number of total birds
  output$total_birds_e <- renderText({
    obs <- the_data %>%
      filter(source == "eBird") %>% 
      mutate(count = replace(count, is.na(count), 1))
    
    paste(sum(obs$count))
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
    date <- format(date, "%B %d, %Y")
    paste0("Last updated: ", date)
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
  
  ## Copyright text
  output$copyright_txt <- renderText({
    date <- today()
    paste0("© ", year(date), " Schoodic Institute at Acadia National Park")
  })
  
}



#### Run the app ####

shinyApp(ui, server)



