#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Sidepanels
# Velg fylke
# Velg nettleverandør
# 1. Strømpris NÅ
# 2. Estimert strømstøtte
# 3. Estimert strømpris



library(data.table)

dt_nettleie <- fread("../data/database_nettleie_simple.csv")
dt_nettleie_kl6 <- fread("../data/database_nettleie_simple_kl_6.csv")
dt_hourly <- fread("../data/database_nordpool_hourly.csv")
dt_comp <- fread("../data/current_estimated_compensation.csv")


library(shiny)
library(shinydashboard)

## app.R ##
library(shinydashboard)

header <- dashboardHeader(title = "Estimert reell strømpris",titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Min strømpris", tabName = "dashboard", icon = icon("dashboard")),
    selectInput("fylkevalg","Velg Fylke",dt_nettleie[,unique(Fylke)]),
    menuItem("Historisk estimering", tabName = "historic", icon = icon("th")),
    menuItem("Metodikk", icon = icon("file-code-o"),
             href = "https://martinjullum.com/sideprojects/stromstotte")
  )

)

body <-  dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(selectInput("fylkevalg","Velg Fylke",dt_nettleie[,unique(Fylke)])),
              box(plotOutput("plot1", height = 250)),
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),

    # Second tab content
    tabItem(tabName = "historic",
            h2("Putt inn et eller annet her.")
    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  output$dt_nettleie <- dt_nettleie[Fylke==input$fylkevalg]
}

shinyApp(ui, server)
