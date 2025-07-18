# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here: https://shiny.posit.co/

# Author: Laura Di Domenico, ISPM, University of Bern
# Date: May 2025

library(shiny)

# Define UI for application
ui <- source("ui.R")
# Define server logic required to draw a histogram
server <- source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)