# Title: California Crash Data (2014-2023) 
# Description: 
# Tab1- Three bar plots of: Total crashes per month, top counties with most crashes that can be facet, and weather vs collision severity 
# Tab2- Leaflet map of city showing location of crashes. Circles can be color-coded by collision type or collision severity, with larger circles representing a larger party.  
# Author: Christy Yau
# Date: 12/6/24

# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(leaflet)      # web interactive maps
library(plotly)       # web interactive graphics

# =======================================================
# Import data
#
# For demo purposes, in this "template" we use storms data
# (but you will have to replace this with the crash data)
#
# Uncomment the lines below in order to import the crash data!!!
# We're assuming the CSV file is in the working directory of your app.
# =======================================================
crashes = read_csv(
  file = "crashes_california_2014_2023.csv", 
  col_types = list(
    col_double(),    #  1) CASE_ID
    col_double(),    #  2) ACCIDENT_YEAR
    col_date(),      #  3) COLLISION_DATE 
    col_double(),    #  4) COLLISION_TIME 
    col_double(),    #  5) HOUR 
    col_integer(),   #  6) DAY_OF_WEEK 
    col_character(), #  7) WEATHER_1 
    col_character(), #  8) WEATHER_2 
    col_character(), #  9) STATE_HWY_IND
    col_character(), # 10) COLLISION_SEVERITY 
    col_integer(),   # 11) NUMBER_KILLED 
    col_integer(),   # 12) NUMBER_INJURED 
    col_integer(),   # 13) PARTY_COUNT 
    col_character(), # 14) PCF_VIOL_CATEGORY 
    col_character(), # 15) TYPE_OF_COLLISION 
    col_character(), # 16) ROAD_SURFACE 
    col_character(), # 17) ROAD_COND_1 
    col_character(), # 18) ROAD_COND_2 
    col_character(), # 19) LIGHTING 
    col_character(), # 20) PEDESTRIAN_ACCIDENT 
    col_character(), # 21) BICYCLE_ACCIDENT 
    col_character(), # 22) MOTORCYCLE_ACCIDENT 
    col_character(), # 23) TRUCK_ACCIDENT 
    col_character(), # 24) NOT_PRIVATE_PROPERTY 
    col_character(), # 25) ALCOHOL_INVOLVED 
    col_character(), # 26) COUNTY 
    col_character(), # 27) CITY 
    col_character(), # 28) PO_NAME
    col_double(),    # 29) ZIP_CODE
    col_double(),    # 30) POINT_X 
    col_double()     # 31) POINT_Y 
  ))

#filter years to2021-2023
crashes = filter(crashes, ACCIDENT_YEAR >= 2021)|>
  mutate(ACCIDENT_MONTH = month(COLLISION_DATE))

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("CA Crash Data"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Exploratory Analysis"),
        # widgets
        sliderInput(inputId = "widget1",
                    label = "Select Year",
                    min = 2021,
                    max = 2023,
                    value = 2023,
                    sep = ""),
        selectInput(inputId = "widget2",
                    label = "Accident involving",
                    choices = list("pedestrian", 
                                   "bicycle", 
                                   "motorcycle", 
                                   "truck",
                                   "none of above", 
                                   "all crashes"),
                    selected = "all crashes"),
        radioButtons(inputId = "widget3",
                     label = "(Plot 2) Facet by",
                     choices = list("month", "day of week", "collision severity","none"),
                     selected = "none"),
        numericInput(inputId = "widget4",
                     label = "(Plot 2) Top n Counties",
                     value= 5)
      ), # closes 1st panel
      
      # ---------------------------------------------
      # input widgets of second tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Map"),
        # widgets
        sliderInput(inputId = "widget5",
                    label = "Select Year",
                    min = 2021,
                    max = 2023,
                    value = 2023,
                    sep = ""),
        selectInput(inputId = "widget6",
                    label = "Enter city name",
                    choices = unique(crashes$PO_NAME),
                    selected = "Berkeley"),
        selectInput(inputId = "widget7",
                    label = "Select Violation category",
                    choices = c("all", unique(crashes$PCF_VIOL_CATEGORY)),
                    selected = "all"),
        radioButtons(inputId = "widget8",
                     label = "Color Code by",
                     choices = c("collision type", "collision severity", "none"), 
                     selected = "collision type")
      ), # closes 2nd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 2 tabsets: 
    # tab1: exploratory analysis
    # tab2: map of crashes
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (graphic)
        tabPanel(title = "Number of Crashes per Year",
                 value = 1,
                 plotlyOutput(outputId = "plot1"),
                 hr(),
                 plotlyOutput(outputId = "plot2"),
                 hr(),
                 plotlyOutput(outputId = "plot3")),
        # second tab (map)
        tabPanel(title = "Map of Crashes",
                 value = 2,
                 leafletOutput("map", height = 600)),
        # selected tab
        id = "tabselected"
        
      ) # closes tabsetPanel
    ) # closes mainPanel
    
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)

# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  # reactive table with month column & filtered years
  crash_filtered = reactive({
    filter(crashes, ACCIDENT_YEAR==input$widget1)
  })
  
  #reactive table with filtered accident type
  crash_type = reactive({
    if (input$widget2 == "pedestrian") {
      crash_type = filter (crash_filtered(), PEDESTRIAN_ACCIDENT == "yes") 
    } else if (input$widget2 == "bicycle"){
      crash_type = filter(crash_filtered(), BICYCLE_ACCIDENT == "yes") 
    } else if (input$widget2 == "motorcycle") {
      crash_type = filter(crash_filtered(), MOTORCYCLE_ACCIDENT == "yes") 
    } else if (input$widget2 == "truck") {
      crash_type = filter(crash_filtered(), TRUCK_ACCIDENT == "yes")
    } else if (input$widget2 == "none of above") {
      crash_type = filter(crash_filtered(), PEDESTRIAN_ACCIDENT == "no" & BICYCLE_ACCIDENT == "no" & MOTORCYCLE_ACCIDENT == "no" & TRUCK_ACCIDENT == "no") 
    } else {
      crash_type = crash_filtered()
    }
  })
  
  # reactive table of top n counties with most accidents 
  top_counties = reactive({
    crash_type() |> 
      group_by(COUNTY) |>
      count() |>
      arrange(desc(n))|>
      ungroup()|>
      slice_head(n=input$widget4) 
  })
  
  # ------------------------------------------------
  # Output for first TAB (i.e. summary plots)
  # (adapt code to your analysis)
  # ------------------------------------------------
  output$plot3 <- renderPlotly({
    #plot of number of crashes per year  
    crash_type() |> 
      group_by(COLLISION_SEVERITY, WEATHER_1) |>
      count() |>
      mutate(num = n/100) |>
      ggplot(aes(x = COLLISION_SEVERITY, y = num, fill = WEATHER_1)) + 
      geom_col(color = "black", position = "dodge") +
      labs(title = paste0("Weather during Collision (", input$widget1, ")"),
           y = "Count (per 100 crashes)", x = "Collision Severity") +
      theme_minimal()+
      facet_wrap(~COLLISION_SEVERITY, scales = "free")+
      scale_fill_discrete(name = "Weather") +
      scale_x_discrete(labels = NULL)
  })
  
  output$plot1 <- renderPlotly({
      # plot number of crashes per month  
      crash_type() |> 
        group_by(ACCIDENT_MONTH) |>
        count()|>
        mutate(num = n/100)|>
        ggplot(aes(x = ACCIDENT_MONTH, y = num)) + 
        geom_col(color = "black", fill = "#17B0E8") +
        labs(title = paste0("Number of Crashes per Month (", input$widget1, ")"),
             y = "Count (per 100 crashes)", x = "Month") +
        theme_minimal() +
        scale_x_continuous(breaks = 1:12) 
  })
  
  
  output$plot2 <- renderPlotly({
    #plot of number of crashes per county 
    county = filter(crash_type(),COUNTY %in% top_counties()$COUNTY)
    
    #facet by month, day of week, severity  
    if (input$widget3 != "none") {
       if (input$widget3 == "month"){
        county = county |>
          group_by(ACCIDENT_MONTH, COUNTY)|>
          count()
      } else if (input$widget3 == "day of week"){
        county = county |>
          group_by(DAY_OF_WEEK, COUNTY)|>
          count()
      } else if (input$widget3 == "collision severity"){
        county = county |>
          group_by(COLLISION_SEVERITY, COUNTY)|>
          count()
      }
      county1 = county |>
        mutate(num = n/100) |>
        ggplot(aes(x=reorder(COUNTY, desc(num)), y=num))+
        geom_col(color = "black",fill = "#EBAF76") +
        facet_wrap(colnames(county[1]), scales="free_y")
    } else {
      #no facet
      county1 = county|>
        group_by(COUNTY)|>
        count()|>
        mutate(num = n/100) |>
        ggplot(aes(x=reorder(COUNTY, desc(num)), y=num))+
        geom_col(color = "black",fill = "#EBAF76")
    }
    #bar plot of counties 
    county1 +labs(title = paste("Top",input$widget4,"Counties with Most Crashes (", input$widget1, ")"),
                     y = "Total Count (per 100 crashes)", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, size = 7))
  })
  
  # -----------------------------------------------
  # Output for second TAB (i.e. map)
  # (adapt code to make your map of crashes)
  # -----------------------------------------------
  output$map <- renderLeaflet({
    #filter year and city 
    crash_city = filter(crashes, PO_NAME == input$widget6 & ACCIDENT_YEAR == input$widget5)
    
    #filter violation category
    if (input$widget7 != "all"){
      crash_map = filter(crash_city, PCF_VIOL_CATEGORY == input$widget7)
    } else {
      crash_map = crash_city 
    }
    
    #color code base on collision type or severity
    if(input$widget8!= "none") {
      if (input$widget8 =="collision type"){
        type = unique(crashes$TYPE_OF_COLLISION)
        pal = colorFactor(palette = "Set3", domain = type)
        crash_map |>
          leaflet() |>
          addProviderTiles("CartoDB.Positron") |> 
          addCircles(lng = ~POINT_X,
                     lat = ~POINT_Y,
                     color = ~pal(TYPE_OF_COLLISION), 
                     fillOpacity = 0.7,
                     weight = 4,
                     radius= ~PARTY_COUNT * 10,
                     label = ~paste0(COLLISION_DATE,": ", COLLISION_SEVERITY,", ", PARTY_COUNT, " people")) |>
          addLegend(position = "bottomleft", 
                    pal = pal,
                    values = type,
                    title = "Collision Type",
                    opacity = 1)
      } else {
        type = unique(crashes$COLLISION_SEVERITY)
        pal = colorFactor(palette = "Spectral", domain = type)
        crash_map |>
          leaflet() |>
          addProviderTiles("CartoDB.Positron") |> 
          addCircles(lng = ~POINT_X,
                     lat = ~POINT_Y,
                     color = ~pal(COLLISION_SEVERITY), 
                     fillOpacity = 0.7,
                     weight = 4,
                     radius= ~PARTY_COUNT * 10,
                     label = ~paste0(COLLISION_DATE,": ", TYPE_OF_COLLISION,", ", PARTY_COUNT, " people")) |>
          addLegend(position = "bottomleft", 
                    pal = pal,
                    values = type,
                    title = "Collision Severity",
                    opacity = 1)
      } 
    } else {
      crash_map|>
        leaflet() |>
        addProviderTiles("CartoDB.Positron") |> 
        addCircles(lng = ~POINT_X,
                   lat = ~POINT_Y,
                   weight = 3,
                   radius= ~PARTY_COUNT * 10,
                   label = ~paste0(COLLISION_DATE,": ", COLLISION_SEVERITY,", ", TYPE_OF_COLLISION, ", ",PARTY_COUNT, " people"))
    } 
    
  })
} # closes server

# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
