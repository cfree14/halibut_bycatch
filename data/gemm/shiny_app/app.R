
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(nutriR)
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
# datadir <- "data" # for actual app
# codedir <- "code"  # for actual app
datadir <- "data/gemm/shiny_app/data" # when testing
codedir <- "data/gemm/shiny_app/code" # when testing

# Read data
data <- readRDS(file.path(datadir, "GEMM_2002_2020_data.Rds"))

# Read helper functions
source(file.path(codedir, "helper_functions.R"))


# Parameters
################################################################################

# Species
spp <- sort(unique(data$species))


# User interface
################################################################################

# User interface
ui <- navbarPage("GEMM bycatch explorer",

                 # Explore by nutrient
                 tabPanel("Explore by species",

                          # Select by species
                          selectInput(inputId = "spp_dropdown", label = "Select a species:",
                                      choices = spp,  multiple = F, selected="White Croaker"),
                          br(),

                          # Plot total catch by species (across all sectors)
                          plotOutput(outputId = "plot_tot_catch")

                 ),

                 # Explore by country
                 tabPanel("Explore by sector",

                 )

)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot total catch by species (across all sectors)
  output$plot_tot_catch <- renderPlot({
    g <- plot_tot_catch(species=input$spp_dropdown)
    g
  })


}

shinyApp(ui = ui, server = server)







