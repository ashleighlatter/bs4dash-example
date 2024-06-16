library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(fresh)

plot_colour <- "#8965CD"

theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)


# Load and wrangle data ---------------------------------------------------

data_path <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C"

data <- read_csv(data_path) %>% 
  # select and rename relevant columns
  select(
    sighting_date = `Sighting Date`,
    common_name = `Common Name`,
    scientific_name = `Scientific Name`,
    sighting_count = `Sighting Count`,
    lat,
    lon,
    location_desc = loc1_desc,
    site_name
  )

# Info box values ---------------------------------------------------------

num_sightings <- sum(data$sighting_count)
unique_birds <- length(unique(data$common_name))
unique_locations <- length(unique(data$site_name))
avg_daily_sightings <- data %>% 
  group_by(sighting_date) %>% 
  summarise(sighting_count = sum(sighting_count)) %>% 
  pull(sighting_count) %>% 
  mean()


# User Interface ----------------------------------------------------------

ui <- dashboardPage(
  title = "Bird Sightings",
  
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  # Header ----
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "Bird Sightings",
      color = "olive",
      image = "https://images.unsplash.com/photo-1539664030485-a936c7d29c6e?q=80&w=1160&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
    ),
    controlbarIcon = icon("circle-info"),
    fixed = TRUE,
    rightUi = dropdownMenu(
      badgeStatus = "info",
      type = "notifications",
      notificationItem(
        text = "Success",
        status = "success",
        icon = icon("circle-check")
      ),
      notificationItem(
        text = "Warning",
        status = "warning",
        icon = icon("circle-exclamation")
      ),
      notificationItem(
        text = "Error",
        status = "danger",
        icon = icon("circle-xmark")
      )
    )
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("bar-chart")
      )
    )
  ),
  
  # Control bar ----
  controlbar = dashboardControlbar(),
  
  # Footer ----
  footer = dashboardFooter(
    left = "Ashleigh Latter",
    right = "2024"
  ),
  
  # Body ----
  body = dashboardBody(
    tabItems(
      
      # Home tab ----
      tabItem(
        tabName = "home",
        
        jumbotron(
          title = "Welcome!",
          status = "info",
          lead = "Visualising bird survey results from the City of Melbourne in February and March 2018",
          href = "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C",
          btnName = "Download",
          "Data available from the City of Melbourne Open Data Portal"
        ),
        
        fluidRow(
          
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Ashleigh Latter",
              subtitle = "Developer",
              image = "https://yt3.googleusercontent.com/kABO8qsiX0FKrvAsjdbU8q98mxSydtE4vpwu03omQ-WtRli9Lo1OTlDhjN05FNsUof2YhuHYvQ=s176-c-k-c0x00ffffff-no-rj",
              type = 1
            ),
            status = "purple",
            "Super impressive bio."
          ),
          
          box(
            title = "My favourite quote",
            width = 6,
            collapsible = FALSE,
            blockQuote("Just because you're trash, doesn't mean you can't do great things. It's called garbage can, not garbage cannot.", color = "purple")
          )
          
        )
        
      ),
      
      # Dashboard tab ----
      tabItem(
        tabName = "dashboard",
        
        ## Info boxes ----
        fluidRow(
          
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Total Bird Sightings",
              value = num_sightings,
              icon = icon("list"),
              color = "primary"
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              value = unique_birds,
              title = "Species Identified",
              icon = icon("dove"),
              color = "primary"
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              value = unique_locations,
              title = "Total Sites",
              icon = icon("location-dot"),
              color = "primary"
            )
          )
          
        ),
        
        ## Sortable boxes ----
        fluidRow(
          sortable(
            width = 6,
            
            box(
              title = "Unique Birds Found at each Location", 
              width = 12, 
              status = "olive",
              collapsible = FALSE, 
              ribbon(
                text = "NEW",
                color = "olive"
              ),
              
              plotlyOutput("plot_unique_birds_site")
            ),
            
            box(
              title = "Birds Sighted Per Day",
              width = 12, 
              closable = TRUE, 
              status = "olive",
              
              plotlyOutput("plot_unique_birds_by_day")
            )
            
          ),
          
          sortable(
            width = 6,
            
            box(
              title = "Bird Sightings by Location",
              width = 12,  
              status = "olive",
              collapsible = FALSE,
              maximizable = TRUE,
              
              leafletOutput("plot_sightings_by_location")
              
            ),
            
            box(
              title = "Total Sightings For Each Bird",
              width = 12, 
              status = "olive",
              collapsible = FALSE, 
              label = boxLabel(
                text = "Label", 
                status = "primary", 
                tooltip = "I'm a label!"),
              
              sidebar = boxSidebar(
                id = "boxsidebarid",
                numericInput(
                  inputId = "show_top_n",
                  label = "Show Top N",
                  value = 6,
                  min = 1,
                  max = 50,
                  width = "97%"
                )
              ),
              
              plotlyOutput("plot_bird_totals_per_day")
            )
            
          )
        ),
        
        ## Tab box ----
        tabBox(
          title = "Data",
          width = 12,
          type = "tabs",
          status = "olive",
          solidHeader = TRUE,
          
          tabPanel(
            "Site Locations",
            DTOutput("table_sites")
          ),
          tabPanel(
            "Birds",
            DTOutput("table_birds")
          )
          
        )
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Number of birds sighted per day
  output$plot_unique_birds_by_day <- renderPlotly({
    
    data %>% 
      group_by(sighting_date) %>% 
      summarise(birds_sighted = sum(sighting_count)) %>% 
      plot_ly(
        x = ~sighting_date,
        y = ~birds_sighted,
        type = "scatter",
        mode = "lines",
        line = list(color = plot_colour)
      ) %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Birds Sighted")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Number of unique bird types by site
  output$plot_unique_birds_site <- renderPlotly({
    
    data %>% 
      group_by(site_name) %>% 
      summarise(unique_birds = n_distinct(common_name)) %>% 
      arrange(unique_birds) %>% 
      mutate(site_name = if_else(site_name == "Dynon Road Tidal Canal Wildlife Sanctuary", "Dynon Road Tidal Canal<br>Wildlife Sanctuary", site_name)) %>% 
      mutate(site_name = factor(site_name, levels = .$site_name)) %>% 
      plot_ly(
        x = ~unique_birds,
        y = ~site_name,
        type = "bar",
        marker = list(color = plot_colour),
        orientation = "h"
      ) %>% 
      layout(
        xaxis = list(title = "Unique Birds"),
        yaxis = list(title = "") 
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Total Sightings per Day per Bird
  output$plot_bird_totals_per_day <- renderPlotly({
    
    data %>% 
      group_by(common_name) %>%
      summarise(sighting_count = sum(sighting_count)) %>% 
      arrange(desc(sighting_count)) %>% 
      mutate(common_name = factor(common_name, levels = rev(.$common_name))) %>% 
      slice(1:input$show_top_n) %>% 
      plot_ly(
        x = ~sighting_count,
        y = ~common_name,
        type = "bar",
        marker = list(color = plot_colour),
        orientation = "h"
      ) %>% 
      layout(
        xaxis = list(title = "Count"),
        yaxis = list(title = "")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Sightings by location
  output$plot_sightings_by_location <- renderLeaflet({
    
    sites <- data %>% 
      group_by(site_name, lat, lon) %>% 
      summarise(sighting_count = sum(sighting_count))
    
    leaflet(data = sites) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        ~lon,
        ~lat,
        radius = ~sighting_count/100,
        color = plot_colour,
        fillOpacity = 1,
        popup = ~paste0(site_name, "<br>Birds: ", sighting_count)
      )
  })
  
  output$table_sites <- renderDT({
    
    data %>% 
      select(site_name, location_desc, lat, lon) %>% 
      unique() %>% 
      rename(
        `Site Name` = site_name,
        `Location Description` = location_desc,
        Latitude = lat,
        Longitude = lon
      )
    
  })
  
  output$table_birds <- renderDT({
    
    data %>% 
      group_by(common_name, scientific_name) %>% 
      summarise(`Total Sightings` = sum(sighting_count)) %>% 
      rename(
        `Common Name` = common_name,
        `Scientific Name` = scientific_name
      )
    
  })
  
}

shinyApp(ui, server)
