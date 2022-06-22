#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(readxl)
library(RODBC)
library(DT)

options(shiny.maxRequestSize = 200*1024^2)

source('global.R', local = T)

# Define UI for application
ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(
        title = "Outil - Analyse des données",
        titleWidth = 300),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data démographique", tabName = "démo", icon = icon("tachometer-alt")),
            menuItem("Santé", tabName = "santé", icon = icon("list-ol")),
            menuItem("Prévoyance", tabName = "prev", icon = icon("list-alt")),
            menuItem("Calcul des TS", tabName = "TS", icon = icon("font-awesome"))  # find some icons here at http://fontawesome.io/icons/
        )
    ),

    dashboardBody(
        tabItems(
            tabItem(
                "démo",
                box( title = "Démographique", footer = "en €", status = "success", solidHeader = TRUE, width = 8, #Choose the background color(here success means green)
                     "graphique à insérer ici"
                ),
                infoBox( title = "Progression", value = "+ ??%", icon = icon("chart-line"), fill = TRUE, color = "light-blue", width = 4
                )
            ),
            tabItem(
                "santé"
                
            ),
            tabItem(
                "prev"
                
            ),
            tabItem(
                "TS",
                fileInput(inputId = "file_TS",    #Nom que l'on donne dans R au fichier que l'on importe
                          label = "Choose the Excel file : ",
                          accept = c(".xlsb"),
                          buttonLabel = "Uploading files",  #Ce qu'il y a marquÃ© sur le bouton
                          placeholder = "No file selected"
                          ),
                box(title = "Cotisations", footer = "en €", status = "success", solidHeader = TRUE, width = 6,
                    tableOutput('recap_cot')
                    ),
                box(title = "Prestations", footer = "en €", status = "success", solidHeader = TRUE, width = 6,
                    tableOutput('recap_presta')
                    ),
                box(title = "Résultat TS", footer = "en €", status = "success", solidHeader = TRUE, width = 12,
                    tableOutput('recap_TS')
                )
            )
        ),
        title = "HAHA",
        skin = "green"
    )
)
)

# Define server
server <-  function(input, output) {
  
  ts <- reactive({
    req(input$file_TS)
    calcul_TS(input$file_TS$datapath,"1- Cotisations 2020", "2- Prestations 2020")
  })
  
  output$recap_cot <- renderTable({ts()[1]})
  
  output$recap_presta <- renderTable({ts()[2]})
  
  output$recap_TS <- renderTable({ts()[3]})

}

# Run the application 
shinyApp(ui = ui, server = server)
