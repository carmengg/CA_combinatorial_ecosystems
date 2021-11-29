library(shiny)
library(raster)
library(sf)
library(here)
library(tidyverse)


# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# ***************************LOADING RASTERS **************************************


clim <- raster(paste(root,'/climate/ca_clim_match_',year,'.tif',sep=""))
arid <- raster(paste(root,'/aridity/ca_arid_match_',year,'.tif',sep=""))
forms <- raster(paste(root,'/landforms/ca_landforms_match.tif',sep=""))
lcover <- raster(paste(root,'/landcover/ca_landcover_regions_match_',year,'.tif',sep=""))

here('combine_layers','matched_layers_timeseries')
raster_files <-list.files(path = here('combine_layers',
                                      'matched_layers_timeseries',
                                      'aridity'),
                          full.names = TRUE)

ca_aridity <- raster::stack(raster_files)


# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# *************************** USER INTERFACE **************************************


ui <- fluidPage(
    navbarPage("Comparing individual layers",
        # ----------------------------------------------------------------------------------
        # --- ARIDITY TAB ---  
        tabPanel("aridity",
            sidebarLayout(
                sidebarPanel(
                ),
                mainPanel(
                )
            )
        ), # tab1 closed
        # ----------------------------------------------------------------------------------
        # --- CLIMATE TAB ---  
        tabPanel("climate",
                 sidebarLayout(
                     sidebarPanel(
                     ),
                     mainPanel(
                     )
                 )
        ), # tab2 closed
        
        # ----------------------------------------------------------------------------------
        # --- LAND COVER TAB ---  
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

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
