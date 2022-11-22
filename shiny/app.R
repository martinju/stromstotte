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

Sys.setlocale("LC_ALL", "en_US.UTF-8") # UTF-8 to get latin letters


library(data.table)

dt_nettleie <- fread("../data/database_nettleie_simple.csv")
dt_nettleie_kl6 <- fread("../data/database_nettleie_simple_kl_6.csv")
dt_hourly <- fread("../data/database_nordpool_hourly.csv")
dt_comp <- fread("../data/current_estimated_compensation.csv")


library(shiny)
library(shinydashboard)

## app.R ##
library(shinydashboard)

header <- dashboardHeader(title = "Strømpris med strømstøtte",titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Strømpris NÅ", tabName = "strompris_naa", icon = icon("dashboard",verify_fa = FALSE)),
    selectInput("fylkevalg","Velg Fylke",dt_nettleie[,unique(Fylke)]),
    selectInput("nettselskap","Velg Nettselskap",""),
    menuItem("Strømstøtte", tabName = "stromstotte", icon = icon("bolt",verify_fa = FALSE)),
    menuItem("Fremtidig strømpris", tabName = "strompris", icon = icon("bolt",verify_fa = FALSE)),
    menuItem("Historisk estimering", tabName = "historic", icon = icon("bolt",verify_fa = FALSE)),
    menuItem("Avanserte innstillinger", tabName = "settings", icon = icon("gear",verify_fa = FALSE)),
    menuItem("Metodikk", icon = icon("globe"),
             href = "https://martinjullum.com/sideprojects/stromstotte")
  )

)

body_strompris_naa <- tabItem(tabName = "strompris_naa",
                               tableOutput("data_nettleie"),
                              h2("NEXT: Try to make a ggplotly plot of the hourly prices here.")
                             )

body_stromstotte <- tabItem(tabName = "stromstotte",
                h2("Putt inn noe om strømstøtte her."),
                fluidRow(
                  box(plotOutput("plot1", height = 250)),
                  box(title = "Controls",
                      sliderInput("slider", "Number of observations:", 1, 100, 50)
                  )
                )
)

body_strompris <- tabItem(tabName = "strompris",
        h2("Putt inn noe om strømpris her.")
)

body_historic <- tabItem(tabName = "historic",
        h2("Putt inn noe om historisk tilpasning her.")
)

body_settings <- tabItem(tabName = "settings",
        h2("Putt inn avanserte innstillinger her, som dato for strømstøtteestimering, konfidensnivå, datoRange for visning av strømpris.")
)



body <-  dashboardBody(
  tabItems(body_strompris_naa,
           body_stromstotte,
           body_strompris,
           body_historic,
           body_settings)
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output,session) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  dt_nettleie_new <- reactive({
    as.data.frame(dt_nettleie[Fylke==input$fylkevalg])
  })

  output$data_nettleie <- renderTable(dt_nettleie_new())

  new_nettselskap <- reactive({
    dt_nettleie[Fylke==input$fylkevalg,unique(Nettselskap)]
  })

  observe({
    updateSelectInput(session, "nettselskap",choices = new_nettselskap()
    )})

}

shinyApp(ui, server)
