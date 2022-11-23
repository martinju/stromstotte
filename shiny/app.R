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
dt_comp <- fread("../data/historic_estimated_compensation.csv")
dt_postnr_nettselskap_prisomraader_map <- fread("../data/simple_postnr_nettselskap_prisomraader_dt.csv")

# Should do this in another file!
dt_postnr_nettselskap_prisomraader_map[,postnr:=as.character(postnr)]
dt_postnr_nettselskap_prisomraader_map[,nchar_postnr:=nchar(postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==1,postnr:=paste0("000",postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==2,postnr:=paste0("00",postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==3,postnr:=paste0("0",postnr)]
dt_postnr_nettselskap_prisomraader_map[,nchar_postnr:=NULL]

dt_postnr_nettselskap_prisomraader_map[,prisomraade:=gsub(" ","",prisomraade,fixed=T)]

today <- Sys.Date()-30


input_mapper <- copy(dt_postnr_nettselskap_prisomraader_map)

unique_postnr <- unique(input_mapper[,postnr])

library(shiny)
library(shinydashboard)

## app.R ##

header <- dashboardHeader(title = "Strømpris med strømstøtte",titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Strømpris NÅ", tabName = "strompris_naa", icon = icon("dashboard",verify_fa = FALSE)),
    selectInput("postnr","Tast inn postnummer",choices=unique_postnr),
    selectInput("nettselskap","Velg Nettselskap",""),
    selectInput("prisomraade","Velg Prisområde",""),
    menuItem("Strømstøtte", tabName = "stromstotte", icon = icon("bolt",verify_fa = FALSE)),
    menuItem("Fremtidig strømpris", tabName = "strompris", icon = icon("bolt",verify_fa = FALSE)),
    menuItem("Historisk estimering", tabName = "historic", icon = icon("bolt",verify_fa = FALSE)),
    menuItem("Avanserte innstillinger", tabName = "settings", icon = icon("gear",verify_fa = FALSE)),
    menuItem("Metodikk", icon = icon("globe"),
             href = "https://martinjullum.com/sideprojects/stromstotte")
  )

)

body_strompris_naa <- tabItem(tabName = "strompris_naa",
                              h2("NEXT: Try to make a ggplotly plot of the hourly prices here."),
                              tableOutput("data_nettleie"),
                              tableOutput("data_spot"),
                              tableOutput("data_comp")
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

  # Update the nettselskap input
  updated_nettselskap <- reactive({
    input_mapper[postnr==input$postnr,unique(nettselskap)]
  })

  observe({
    updateSelectInput(session, "nettselskap",choices = updated_nettselskap()
    )})

  # Update the prisomraade input
  updated_prisomraade <- reactive({
    input_mapper[postnr==input$postnr & nettselskap ==input$nettselskap,unique(prisomraade)]
  })

  observe({
    updateSelectInput(session, "prisomraade",choices = updated_prisomraade()
    )})

   # Filter dt_nettleie based on input
   updated_dt_nettleie <- reactive({
     input_mapper[postnr==input$postnr & nettselskap ==input$nettselskap & prisomraade ==input$prisomraade,]
   })

   # Filter dt_hourly based on input
   updated_dt_hourly <- reactive({
     dt_hourly[area ==input$prisomraade & date==today,]
   })

   # Filter dt_comp based on input
   updated_dt_comp <- reactive({
     dt_comp[area == input$prisomraade & estimation_date==today,]
   })

   output$data_spot <- renderTable(updated_dt_hourly())
   output$data_comp <- renderTable(updated_dt_comp())


   output$data_nettleie <- renderTable(updated_dt_nettleie())


  ### OLD ###
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })


  # dt_nettleie_new <- reactive({
  #   as.data.frame(dt_nettleie[Fylke==input$fylkevalg])
  # })
  #
  # output$data_nettleie <- renderTable(dt_nettleie_new())
  #
  # new_nettselskap <- reactive({
  #   dt_nettleie[Fylke==input$fylkevalg,unique(Nettselskap)]
  # })
  #
  # observe({
  #   updateSelectInput(session, "nettselskap",choices = new_nettselskap()
  #   )})

}

shinyApp(ui, server)
