

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(htmltools)
library(stringr)
library(readxl)
library(plotly)
library(rsconnect)
library(shinycssloaders)

# Read data

full <- readRDS("data/full.rds")
#full <- readRDS("./masterShiny/data/full.rds")
full <- sf::st_as_sf(
  full,
  sf_column_name = "geometry",
  crs = 4326
)


sectors <- readRDS("data/sectors.rds")
#sectors <- readRDS("./masterShiny/data/sectors.rds")

spot_povs <- c(
  "Europa" = "spots/Europa.png",
  "SandyBay" = "spots/SandyBay.png",
  "PrinceGeorge" = "spots/PrinceGeorge.png"
)
spots <- read_xlsx("data/datasheet_landsurvey.xlsx", sheet = "spots")
#spots <- read_xlsx("./masterShiny/data/datasheet_landsurvey.xlsx", sheet = "spots")
spots <- spots %>% 
  filter(spot_name %in% c("Europa", "SandyBay", "PrinceGeorge")) %>% 
  mutate(pov = spot_povs[spot_name])



species_icons <- c(
  "common_dolphin" = "icons/common_dolphin.svg",
  "bottlenose" = "icons/bottlenose_dolphin.svg",
  "striped_dolphin" = "icons/Striped.svg",
  "fin_whale" = "icons/finwhale.svg",
  "delphinidae" = "icons/other.svg",
  "unidentified" = "icons/other.svg"
)

full <- full %>%
  filter(!sf::st_is_empty(geometry)) %>% 
  mutate(icon = species_icons[species]) %>% 
  mutate(clean_species = species %>%
           str_replace_all("_", " ") %>%
           str_to_title()) %>% 
  mutate(date.x = as.Date(date.x))


cluster_options <- markerClusterOptions(disableClusteringAtZoom = 13)



ui <- fluidPage(
  # Link external stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg")
  ),
  
    # Define a theme
  theme = bs_theme(
    bg = "#fbf7f5",        # Background color
    fg = "#282b2d",        # Foreground color (text color)
    primary = "#4e5b9c",   # Primary accent color
    base_font = font_google("Roboto"), # Use a Google Font
    heading_font = font_google("Lora"),
    base_line_height = 2 
  ),

    # Sidebar with a slider input for number of bins 
    page_navbar(
      title = div(class = "app-title", h1("Explore Ceteaceans around Gibraltar"), div("By Simon Gendrisch", class = "app-subtitle")), 
      
      # add space between title and nav bar elements
      nav_spacer(),
      
      # 1st nav table
      nav_panel("Data", icon = icon( name="chart-simple", family="whiteboard", variant="semibold"),
                div( style = "gap: 3px;", h3("Welcome") ,"to this interactive exploration website. It provides an interactive way to explore cetacean sightings observed within the British Gibraltar Territorial Waters (BGTW) during my Masters project. Discover where and when sightings occurred using the interactive map, learn more about the species recorded during the survey, and explore the methods and background behind this research in the Species, Methods, and About sections."),
                
                div(layout_column_wrap(
                  uiOutput("vb_sightings"),
                  uiOutput("vb_individuals"),
                  uiOutput("vb_species"),
                  uiOutput("vb_days")
                )),
                
                #Card inside the first page
                layout_columns(
                  div(
                    p(# Text
                      "This interactive map shows cetacean sightings recorded during the survey period within British Gibraltar Territorial Waters. Sightings are displayed by species, and additional information can be viewed by clicking on the markers. Use the map to explore where cetaceans were observed, view the survey locations, and use the slider below to explore changes over time."),
                    # Date slider
                    sliderInput("date", strong("Select a date range:"), min = min(full$date.x), max = max(full$date.x), value = c(min(full$date.x), max(full$date.x)), timeFormat = "%d %b %Y"),
                  
                  # Species Checkbox
                  checkboxGroupInput(
                    "species",
                    strong("Select a species here:"),
                    choices = unique(full$clean_species),
                    selected = unique(full$clean_species)
                  ),
                  ),
                 
                # Map
                card(leafletOutput("map", height = "70vh")),
                
                col_widths = c(3,9)
                ),
                # Text underneath the first card
                card(
                  full_screen = TRUE,
                  plotlyOutput("sightings_time", height = 300)
                ),
                #
                ),
      
      # Species
      nav_panel("Species", icon = icon(name="otter", family="whiteboard", variant="semibold"),
                layout_columns(
                div(h3("Keeping a Safe Distance"),
                p("During this study, it was often observed that boats approached cetaceans very closely. While seeing dolphins and whales in the wild is an unforgettable experience, these animals need space to behave naturally and avoid unnecessary stress. If you encounter cetaceans, slow down and keep a respectful distance. Avoid chasing or surrounding them, and allow them to decide whether to approach your vessel. By giving wildlife the space it needs, you help protect these remarkable animals while ensuring that everyone can continue to enjoy the unique marine environment around Gibraltar.")),
                div(img(src = "infographic.png", height = 350))),
                # Species -----
                h3("Species in the BGTW"),
                p("You can find a selection of cetacean species commonly observed around Gibraltar here. These species are not exclusive to the region, and additional cetacean species can also be encountered in the surrounding waters. This page provides an overview of the species most frequently observed during the short survey period of this project."),
                hr(),
                layout_columns(col_widths = c(6,6),
                  # Common dolphin block-------------
                  card(
                    div(h4("Common dolphin", style = "margin-bottom: 0rem; margin-top: 1.5rem;"), div("Delphinus delphis", style=" opacity: 0.5; font-style: italic;")),
                    div(strong("Family:"), "Delphinidae"),
                    div(strong("Genus:"), em("Delphinus")),
                    div(strong("Conservation Staus:"), span("Endangered", style = "color: red")),
                    div(strong("Individuals recorded:"), span(sum(filter(full, species == "common_dolphin")$group_size, na.rm = TRUE))),
                    p("The common dolphin is a highly social and widely distributed cetacean species found in temperate and tropical waters worldwide. It is known for forming large groups and is often observed travelling, feeding, and interacting with other individuals. In the Strait of Gibraltar and surrounding waters, common dolphins are one of the most frequently encountered cetacean species, with sightings occurring throughout the year. Their streamlined body, distinctive hourglass colour pattern, and energetic behaviour make them one of the most recognisable dolphin species in the region.
", style = "margin-top: 20px;"),
                  div(img(src = "icons/common_dolphin.svg", height = 400),
                     p(class  = "fig-description","Common dolphins can be easily distinguished from other dolphin species by their characteristic yellow-orange patches on both sides of their body.
"))),
                  
                  
                  # Striped dolphin block----------------
                  card(
                    div(
                      h4("Striped dolphin", style = "margin-bottom: 0rem; margin-top: 1.5rem;"),
                      div("Stenella coeruleoalba", style = "opacity: 0.5; font-style: italic;")
                    ),
                    div(strong("Family:"), "Delphinidae"),
                    div(strong("Genus:"), em("Stenella")),
                    div(strong("Conservation Status:"), span("Vulnerable", style = "color: orange")),
                    div(strong("Individuals recorded:"), span(sum(filter(full, species == "striped_dolphin")$group_size, na.rm = TRUE))),
                    p(
                      "The striped dolphin is a highly active and social species found in warm-temperate and tropical oceans worldwide. It typically forms groups ranging from a few individuals to several hundred animals and is known for its energetic behaviour, including frequent leaps and bow-riding. In the Strait of Gibraltar and surrounding waters, striped dolphins are regularly observed and can often be identified by their distinctive colour pattern and slender body shape. Compared to common dolphins they tend to be found further off shore in deeper waters.",
                      style = "margin-top: 20px;"
                    )
                  ,
                  div(
                    img(src = "icons/Striped.svg", height = 400),
                    p(
                      class = "fig-description",
                      "Striped dolphins can be identified by their characteristic dark stripes extending from the eye towards the flank and their contrasting light and dark body pattern."
                    )
                  )),
                  
                  # Bottlenose dolphin block---------
                  card(
                    div(
                      h4("Bottlenose dolphin", style = "margin-bottom: 0rem; margin-top: 1.5rem;"),
                      div("Tursiops truncatus", style = "opacity: 0.5; font-style: italic;")
                    ),
                    div(strong("Family:"), "Delphinidae"),
                    div(strong("Genus:"), em("Tursiops")),
                    div(strong("Conservation Status:"), span("Least Concern", style = "color: green")),
                    div(strong("Individuals recorded:"), span(sum(filter(full, species == "bottle_nose")$group_size, na.rm = TRUE))),
                    p(
                      "The bottlenose dolphin is one of the most widely recognised and studied cetacean species worldwide. It occurs in a variety of coastal and offshore habitats and is known for its adaptability, intelligence, and complex social behaviour. Bottlenose dolphins often form smaller groups compared to common and striped dolphins and can frequently be observed travelling, foraging, and interacting with other individuals. In the Strait of Gibraltar, they are regularly encountered and can be identified by their robust body shape, short beak, and uniformly grey colouration.",
                      style = "margin-top: 20px;"
                    )
                  ,
                  div(
                    img(src = "icons/bottlenose_dolphin.svg", height = 400),
                    p(
                      class = "fig-description",
                      "Bottlenose dolphins can be distinguished by their robust body shape, short rounded beak, and relatively uniform grey coloration compared to other dolphin species."
                    )
                  ),
                  ),
                  
                  # Fin whale block---------------
                  card(
                    div(
                      h4("Fin whale", style = "margin-bottom: 0rem; margin-top: 1.5rem;"),
                      div("Balaenoptera physalus", style = "opacity: 0.5; font-style: italic;")
                    ),
                    div(strong("Family:"), "Balaenopteridae"),
                    div(strong("Genus:"), em("Balaenoptera")),
                    div(strong("Conservation Status:"), span("Endangered", style = "color: red")),
                    div(strong("Individuals recorded:"), span(sum(filter(full, species == "fin_whale")$group_size, na.rm = TRUE))),
                    p(
                      "The fin whale is the second-largest whale species in the world and is widely distributed throughout the world's oceans. Despite its large size, it is a fast-swimming and highly migratory species, often travelling long distances between feeding and breeding areas. In the Strait of Gibraltar, fin whales are occasionally observed during migration periods and can be recognised by their elongated body shape, tall dorsal fin, and characteristic asymmetrical colouration of the head.",
                      style = "margin-top: 20px;"
                    )
                  ,
                  div(
                    img(src = "icons/finwhale.svg", height = 400),
                    p(
                      class = "fig-description",
                      "Fin whales can be distinguished by their large size, streamlined body, prominent dorsal fin, and asymmetrical pigmentation with a lighter right side of the head."
                    )
                  ),
                  ),
                  
                  # Orca block -----------
                  card(
                    div(
                      h4("Killer whale / Orca", style = "margin-bottom: 0rem; margin-top: 1.5rem;"),
                      div("Orcinus orca", style = "opacity: 0.5; font-style: italic;")
                    ),
                    div(strong("Family:"), "Delphinidae"),
                    div(strong("Genus:"), em("Orcinus")),
                    div(strong("Conservation Status:"), span("Data Deficient / Least Concern (Europe)", style = "color: grey")),
                    div(strong("Individuals recorded:"), span(sum(filter(full, species == "orca")$group_size, na.rm = TRUE))),
                    p(
                      "The orca is the largest member of the dolphin family and is found in oceans worldwide. It is a highly social and intelligent species that lives in stable family groups and exhibits diverse hunting strategies depending on the population. In the Strait of Gibraltar, orcas are seasonally associated with the migration of Atlantic bluefin tuna and are occasionally observed in the region. Their distinctive black-and-white colouration, large dorsal fin, and robust body make them easily recognisable.",
                      style = "margin-top: 20px;"
                    )
                  ,
                  div(
                    img(src = "icons/orca.svg", height = 400),
                    p(
                      class = "fig-description",
                      "Orcas can be easily identified by their distinctive black-and-white colour pattern, white eye patch, and prominent dorsal fin."
                    )
                  ),
                  ),
                  
                  )
                ),
      
      # Methods 
      nav_panel("Methods", icon = icon(name="wrench", family="whiteboard", variant="semibold"),
                div(class = "about-text",
                    style = "
                    max-width: 900px;
                    margin: 40px auto;
                    text-align: left;",
                h3("Survey Design"),
                layout_columns(
                  div(
                  p("This study implemented the first land-based distance sampling framework in Gibraltar. Distance sampling is a method used to spot cetaceans and estimate the distance from shore or on a vessel. It utilises binoculars with reticles, small calibrated scales within the field of view, to measure the distance of a sighting.", br(), 
                    "Compared to vessel-based surveys, land-based distance sampling provides a cost-effective and accessible approach that can be implemented with relatively limited equipment while still allowing the collection of valuable ecological data. This website presents the results of this project, including the sightings recorded during the survey period and the methods used to collect the data.", br(), 
                    "The majority of sightings displayed here were collected through land-based surveys. Additionally, two vessel-based surveys were conducted as pilot studies, and these observations are also included in the dataset. Further information on the vessel-based survey approach can be found in the associated publication within the supplemental materials.", br(), 
                    "The following section provides more details on the survey methodology, data collection process, and the approaches used to analyse the collected observations."),),
                  
                  # Image of the transects with fig description
                 div(img(src = "sector_map.png", height = 500), 
                     p(class = "fig-description","The three vantage points that were used to sample a majority of the BGTW."))
                 ),
                
                hr(),
                
                h3("Distance Sampling"),
                p("Distance sampling is a technique used to estimate how many animals are present in an area while accounting for the fact that not all individuals are equally likely to be detected. Animals close to the observer are generally easier to see than animals further away, and this difference in detectability is used to improve population estimates. During surveys, observers record each sighting together with the estimated distance from the observation point. These measurements can then be used to calculate the likelihood of detecting animals at different distances and provide more accurate estimates of abundance and distribution. In this study the three spots were sampled 54 times each. Using 10 minutes of scanning and 20 minute breaks. Every sighting recorded within the 10 minutes was recorded as on-effort. Using the recorded sightings a distribution map, as seen on the data page, can be created.", br(), "If you would like to learn more about distance sampling another interactive website can be found under this", a("link", href = "https://distancesampling.org/resources/whatisds.html", target = "_blank"), "if you have any specific questions feel free to reach out to me.")
                
        
      )),
      
      
       # About----------------
      nav_panel("About", icon = icon( name="circle-info", family="whiteboard", variant="semibold"),
                div(class = "about-text",
                  style = "
                    max-width: 900px;
                    margin: 40px auto;
                    text-align: left;",
                     h3("About this website"),
                    p("This interactive web application was developed as part of my Master's thesis at the University of Gibraltar.", br(),  
                      "The project investigates the distribution and relative abundance of cetaceans within British Gibraltar Territorial Waters (BGTW) using land-based survey methods. The application provides an interactive platform to explore cetacean sightings collected during the survey period, including species identification, spatial distribution, survey locations, and temporal patterns. Additional information about the recorded species and survey methodology is provided to support interpretation of the data. The aim of this application is to make the results of the research more accessible and provide an interactive way to explore the cetacean community observed around Gibraltar. The full article will be available", a("here", href = "", target = "_blank"),"as soon as it is published."),
                    
                    h3("About Me"),
                    layout_columns(
                      p("I am Simon, a young researcher from Germany.", br(), 
                      "I completed my undergraduate degree in Biology and am currently pursuing a Master's degree in Marine Science and Climate Change at the University of Gibraltar. My main interests lie in cetacean research, particularly dolphins, as well as data science and effective science communication.", br(), "This website was developed as part of my Master's dissertation and aims to provide an accessible insight into the research conducted during my time in Gibraltar. It presents the data collected during the project and highlights the methods used to investigate cetacean distribution within British Gibraltar Territorial Waters.", br(),"If you would like to learn more about this project or cetacean research in Gibraltar, please feel free to get in touch. My contact details and further links can be found below."),
                      
                  div(
                    img(src = "me.jpg", height = 600, style = "border-radius: 15px;"),
                    p(
                      class = "fig-description",
                      "This is me during one of the surveys trying to get usable photos from a group of common dolphins off Gibraltar."))
                ),
                div(style = "text-align:center;
                    margin-top:30px;",
                h3("Find me here:"),
                
                a(
                  href = "https://github.com/datsimon-design",
                  target = "_blank",
                  class = "social-link",
                  icon("github"),
                  " GitHub"
                ),
                a(
                  href = "https://www.researchgate.net/profile/Simon-Gendrisch",
                  target = "_blank",
                  class = "social-link",
                  icon("graduation-cap"),
                  " ResearchGate"
                ),
                
                a(
                  href = "mailto:206708@unigib.edu.gi",
                  class = "social-link",
                  icon("envelope"),
                  " Email"
                ), 
                div(class = "app-subtitle", br(), div(style = "gap: 6px", "This website was made without the help of AI.", tooltip(span (icon("circle-info"), class = "info-icon", style = "font-size: 0.7rem; "), "Good old reading documentations, YouTube Videos and Stack Overflow discussions.")))
                ),
                
                )),
      id = "page"
    )
)



# Define server logic 
server <- function(input, output) {
  
  # Add function to button
  observeEvent(input$button, {
    showNotification(strong("Hi this is a test message"), p("More information"), type = "error")
  })
  
  
  # Reactive df
  filtered_full <- reactive({
    full %>% 
      filter(
        date.x >= input$date[1],
        date.x <= input$date[2],
      ) %>% 
      filter(clean_species %in% input$species)
  })
  
  # Create value boxes for the data overview
  output$vb_sightings <- renderUI({
    
    value_box(
      title = div(style = "display:flex; gap: 6px", "Total Sightings", tooltip(span (icon("circle-info"), class = "info-icon", style = "font-size: 0.7rem; "), "This is the sum of all the sightings within the selected timeframe.")),
      value = nrow(filtered_full()),
      showcase = icon("binoculars"),
      theme = "primary"
    )
  })
  
  output$vb_individuals <- renderUI({
    
    value_box(
      title = div(style = "display:flex; gap: 6px", "Individuals (estimated)", tooltip(span (icon("circle-info"), class = "info-icon", style = "font-size: 0.7rem; "), "The number of individuals is calculated by multiplying the number of sightings with the group estimate.")),
      value = sum(filtered_full()$group_size, na.rm = TRUE),
      showcase = icon("fish-fins"),
      theme = "text-primary"
    )
  })
  
  output$vb_species <- renderUI({
    
    value_box(
      title = "Species",
      value = n_distinct(filtered_full()$clean_species),
      showcase = icon("fish"),
      theme = "text-primary"
    )
  })
  
  output$vb_days <- renderUI({
    
    value_box(
      title = "Survey days",
      value = n_distinct(filtered_full()$date.x),
      showcase = icon("calendar"),
      theme = "text-primary"
    )
  })
  
  # Plotly sightings over time-----------
  output$sightings_time <- renderPlotly({
    
    plot_data <- filtered_full() %>%
      count(date.x)
    
    sight_time <- ggplot(plot_data, aes(
      x = date.x,
      y = n,
      group = 1,
      text = paste(
        "Date:", date.x,
        "<br>Sightings:", n
      ))) +
      geom_line(linewidth = 1, colour = "#4e5b9c") +
      geom_point(size = 2.5, colour = "#4e5b9c") +
      labs(
        x = NULL,
        y = "Number of Sightings"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA)
      )
    
    ggplotly(sight_time, tooltip = "text") %>% 
      layout(
        hovermode = "x unified",
        dragmode = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>% 
      config(
        displayModeBar = FALSE,
        scrollZoom = FALSE,
        displaylogo = FALSE,
        responsive = TRUE
      )
    })
  
  # Leaflet map--------------
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "Light map"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Satellite"
      ) %>%
      
      addAwesomeMarkers(
        data = spots,
        group = "Observation Spots",
        layerId = ~spot_name,
        icon = awesomeIcons(
          icon = "binoculars",
          markerColor = "lightblue",
          iconColor = "white",
          library = "fa"
        ),
        popup = ~paste(
          "<div style='width:300px;'>",
          
          "<h4 style='margin-bottom:10px;'>", spot_name, "</h4>",
          
          "<img src='", pov,
          "' style='width:100%; border-radius:5px;'>",
          
          "<br><br><b>Height above sea level:</b> ", observer_height, " m",
          "<br><b>Bearing:</b> ", spot_bearing1, " - ", spot_bearing2, "°",
          
          "</div>"
          
        )
      ) %>%
      
      # Add spot sectors
      addPolygons(
        data = sectors %>% filter(name == "Europa"),
        group = "Europa",
        fillColor = "blue",
        fillOpacity = 0.2
      ) %>% 
      addPolygons(
        data = sectors %>% filter(name == "SandyBay"),
        group = "SandyBay",
        fillColor = "blue",
        fillOpacity = 0.2
      ) %>% 
      addPolygons(
        data = sectors %>% filter(name == "PrinceGeorge"),
        group = "PrinceGeorge",
        fillColor = "blue",
        fillOpacity = 0.2
      ) %>% 
      
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(imperial = FALSE)
      ) %>%
      
      addControl(
        html = paste0(
          "<div class='species-legend'>",
          "<b>Species</b><br>",
          "<img src='icons/common_dolphin.svg'> Common dolphin<br>",
          "<img src='icons/bottlenose_dolphin.svg'> Bottlenose dolphin<br>",
          "<img src='icons/Striped.svg'> Striped dolphin<br>",
          "<img src='icons/finwhale.svg'> Fin whale<br>",
          "<img src='icons/other.svg'> Unidentified dolphins<br>",
          "</div>"
        ),
        position = "bottomright"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Light map", "Satellite"),
        overlayGroups = c("Cetacean Sightings", "Observation Spots"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>% 
      
      setView(-5.3493, 36.1097, zoom = 11.5)
  })
  
  observe({
    
    data <- filtered_full()
    
    cetacean_icons <- icons(
      iconUrl = data$icon,
      iconWidth = 50,
      iconHeight = 50,
      iconAnchorX = 20,
      iconAnchorY = 20
    )
    
    leafletProxy("map", data = data) %>%
      clearGroup("Cetacean Sightings") %>%
      addMarkers(
        group = "Cetacean Sightings",
        icon = cetacean_icons,
        clusterOptions = cluster_options,
        popup = ~paste(
          "<h4 style='margin-bottom:5px;'>", clean_species, "</h4>",
          "<br><b>Group size:</b>", group_size,
          "<br><b>Behaviour:</b>", behaviour,
          "<br><br><b>Date:</b>", date.x
        )
      )
    
  })
  
  observe({
    leafletProxy("map") %>%
      hideGroup("Europa") %>%
      hideGroup("SandyBay") %>%
      hideGroup("PrinceGeorge")
  })
  
  # Spot sector per spot
  observeEvent(input$map_marker_click, {
    
    click <- input$map_marker_click
    
    spot <- click$id
    
    leafletProxy("map") %>%
      hideGroup("Europa") %>%
      hideGroup("SandyBay") %>%
      hideGroup("PrinceGeorge") %>%
      showGroup(spot)
  })
  
  observeEvent(input$map_click, {
    
    leafletProxy("map") %>%
      hideGroup("Europa") %>%
      hideGroup("SandyBay") %>%
      hideGroup("PrinceGeorge")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
