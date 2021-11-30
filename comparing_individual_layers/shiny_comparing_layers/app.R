library(shiny)
library(raster)
library(sf)
library(here)
library(tidyverse)
library(rgdal)


# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# *************************** LOADING RASTERS **************************************

raster_files <-list.files(path = here('combine_layers',
                                      'sb_matched_layers_timeseries',
                                      'aridity'),
                          full.names = TRUE)

aridity <- raster::stack(raster_files)

raster_files <-list.files(path = here('combine_layers',
                                      'ca_matched_layers_timeseries',
                                      'climate'),
                          full.names = TRUE)

climate <- raster::stack(raster_files)

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# *************************** LOADING CHANGE FUNCTIONS *****************************

source(here("comparing_individual_layers","change_functions.R"))

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# *************************** LOADING SHAPEFILES **********************************

sb_shp <- readOGR(here("shapefiles","sb-county-boundary", "data","commondata","county_boundaries","SB_only.shp"))
sb_shp <- spTransform(sb_shp, crs(aridity[[1]]))
    
# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# *************************** USER INTERFACE **************************************


ui <- fluidPage(
    navbarPage("Comparing individual layers",
        # ----------------------------------------------------------------------------------
        # --- tab1: ARIDITY ---  
        tabPanel("aridity",
            sidebarLayout(
                sidebarPanel(
                    selectInput("tab1_year1", 
                                label = h3("Initial year"), 
                                choices = list("2001" = 1, 
                                               "2008" = 2, 
                                               "2019" = 3),
                                selected=2
                                ),
                    radioButtons(inputId = "tab1_i", 
                                 label = h3("From"),
                                 choices = list("(1) desertic" = 1, 
                                                "(2) dry" = 2, 
                                                "(3) humid" = 3),
                                 selected = 3
                                 ),
                    
                    selectInput("tab1_year2", 
                                label = h3("Compare with year"), 
                                choices = list("2001" = 1, 
                                               "2008" = 2, 
                                               "2019" = 3),
                                selected = 3
                    ),
                    radioButtons(inputId = "tab1_j", 
                                 label = h3("To"),
                                 choices = list("(1) desertic" = 1, 
                                                "(2) dry" = 2, 
                                                "(3) humid" = 3),
                                 selected = 2
                    )
                ),
                mainPanel(align = "center",
                          plotOutput("tab1_plot")
                )
            )
        ), # tab1 closed
        # ----------------------------------------------------------------------------------
        # --- tab2: CLIMATE ---  
        tabPanel("climate",
                 sidebarPanel(
                     selectInput("tab2_year1",
                                 label = h3("Initial year"),
                                 choices = list("2001" = 1,
                                                "2008" = 2,
                                                "2019" = 3),
                                 selected=2
                     ),
                     radioButtons(inputId = "tab2_i",
                                  label = h3("From"),
                                  choices = list("(1) polar" = 1,
                                                 "(2) boreal" = 2,
                                                 "(3) cold temperate" = 3,
                                                 "(4) warm temperate" = 4,
                                                 "(5) subtropical" = 5,
                                                 "(6) tropical" = 6),
                                  selected = 3
                     ),

                     selectInput("tab2_year2",
                                 label = h3("Compare with year"),
                                 choices = list("2001" = 1,
                                                "2008" = 2,
                                                "2019" = 3),
                                 selected = 3
                     ),
                     radioButtons(inputId = "tab2_j",
                                  label = h3("To"),
                                  choices = list("(1) polar" = 1,
                                                 "(2) boreal" = 2,
                                                 "(3) cold temperate" = 3,
                                                 "(4) warm temperate" = 4,
                                                 "(5) subtropical" = 5,
                                                 "(6) tropical" = 6),
                                  selected = 3
                     )
                 ),
                 mainPanel(align = "center",
                           plotOutput("tab2_plot")
                 )
        ), # tab2 closed
        
        # ----------------------------------------------------------------------------------
        # --- tab3: LAND COVER ---  
        tabPanel("land cover",
                 sidebarLayout(
                     sidebarPanel(
                     ),
                     mainPanel(
                     )
                 )
        ) # tab3 closed
    ) #navbarPage closed
) # fluidPage closed


# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# *************************** SERVER **************************************

server <- function(input, output) {
    #--------------------------------------------------------------
    # --- TAB 1 : aridity 
    
    tab1_result_raster <- reactive({
        arid_from <- aridity[[as.integer(input$tab1_year1)]]
        arid_to <- aridity[[as.integer(input$tab1_year2)]]
        i <- as.integer(input$tab1_i)
        j <- as.integer(input$tab1_j)
        raster_change_ij(arid_from, i, arid_to, j)    
    })
    
    output$tab1_plot <- renderPlot({
        plot(tab1_result_raster())
        plot(sb_shp, add=TRUE)
    })
    #--------------------------------------------------------------
    # --- TAB 2 : climate 
    
    tab2_result_raster <- reactive({
        raster_from <- climate[[as.integer(input$tab2_year1)]]
        raster_to <- climate[[as.integer(input$tab2_year2)]]
        i <- as.integer(input$tab2_i)
        j <- as.integer(input$tab2_j)
        raster_change_ij(raster_from, i, raster_to, j)    
    })
    
    output$tab2_plot <- renderPlot({
        plot(tab2_result_raster())
        #plot(sb_shp, add=TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
