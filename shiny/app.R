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

dt_nettleie[,Energiledd:=Energiledd/100]

today <- Sys.Date()


input_mapper <- copy(dt_postnr_nettselskap_prisomraader_map)

unique_postnr <- unique(input_mapper[,postnr])

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(pammtools)

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
                              plotlyOutput("spotplot"),
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
     dt_comp[area == input$prisomraade & estimation_date==today-1,]
   })

   # Filter dt_comp based on input
   updated_dt_nettleie <- reactive({
     dt_nettleie[Nettselskap == input$nettselskap]
   })


   # TESTING
   output$data_spot <- renderTable(updated_dt_hourly())
   output$data_comp <- renderTable(updated_dt_comp())
   output$data_nettleie <- renderTable(updated_dt_nettleie())






   output$spotplot <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     updated_dt_nettleie0 <- updated_dt_nettleie()#dt_nettleie[Nettselskap=="ELVIA AS"]
     updated_dt_hourly0 <- updated_dt_hourly()#dt_hourly[area=="NO1" & date==today]
     updated_dt_comp0 <- updated_dt_comp()#dt_comp[area == "NO1" & estimation_date==today-1,]

     #updated_dt_nettleie0 <- dt_nettleie[Nettselskap=="ELVIA AS"]
     #updated_dt_hourly0 <- dt_hourly[area=="NO1" & date==today]
     #updated_dt_comp0 <- dt_comp[area == "NO1" & estimation_date==today-1,]


     plot_strompris_naa_dt <- copy(updated_dt_hourly0)
     setnames(plot_strompris_naa_dt,"price","spotpris")

     tmp_comp_dt <- updated_dt_comp0[type%in%c("median","quantile_0.025","quantile_0.975","lower_bound"),.(type,compensation)]
     tmp_comp_dt[,type:=c("stotte_median","stotte_lower_CI","stotte_upper_CI","stotte_lower_bound")]

     plot_strompris_naa_dt[start_hour %in% seq(6,21),nettleie:=updated_dt_nettleie0[pristype=="Dag",Energiledd]]
     plot_strompris_naa_dt[is.na(nettleie),nettleie:=updated_dt_nettleie0[pristype=="Natt",Energiledd]]
     plot_strompris_naa_dt[,totalpris_median := spotpris+nettleie-tmp_comp_dt[type=="stotte_median",compensation]]
     plot_strompris_naa_dt[,totalpris_upper_CI := spotpris+nettleie-tmp_comp_dt[type=="stotte_lower_CI",compensation]]
     plot_strompris_naa_dt[,totalpris_lower_CI := spotpris+nettleie-tmp_comp_dt[type=="stotte_upper_CI",compensation]]
     plot_strompris_naa_dt[,totalpris_upper_bound := spotpris+nettleie-tmp_comp_dt[type=="stotte_lower_bound",compensation]]

     tmp_melting_dt <- plot_strompris_naa_dt[,.(start_hour,nettleie,spotpris,totalpris_median,totalpris_lower_CI, totalpris_upper_CI, totalpris_upper_bound)]
     plot_strompris_naa_dt_melted <- melt(tmp_melting_dt,id.vars = "start_hour",variable.name = "type",value.name = "pris")
     plot_strompris_naa_dt_ints <- plot_strompris_naa_dt[,.(start_hour,totalpris_lower_CI,totalpris_upper_CI)]
     plot_strompris_naa_dt_ints2 <- copy(plot_strompris_naa_dt_ints)
     plot_strompris_naa_dt_ints2[,start_hour:=start_hour+1]

     p <- ggplot(mapping=aes(x=start_hour,y=pris))+
       geom_step(data=plot_strompris_naa_dt_melted[type=="spotpris"],direction = "hv",col=scales::hue_pal()(3)[3],size=1)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="nettleie"],direction = "hv",col=scales::hue_pal()(3)[2],size=1)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_median"],direction = "hv",col=scales::hue_pal()(3)[1],size=1)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_lower_CI"],direction = "hv",col=scales::hue_pal()(3)[1],alpha=0.5)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_upper_CI"],direction = "hv",col=scales::hue_pal()(3)[1],alpha=0.5)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_upper_bound"],direction = "hv",col=scales::hue_pal()(3)[1],linetype=2,alpha=0.5)

     for(i in seq_len(nrow(plot_strompris_naa_dt_ints))){
       p <- p + geom_ribbon(data=rbind(plot_strompris_naa_dt_ints[i],
                                       plot_strompris_naa_dt_ints2[i]),alpha=0.3,inherit.aes=FALSE, direction = "hv",fill=scales::hue_pal()(3)[1],
                   mapping=aes(ymin=totalpris_lower_CI,
                               ymax=totalpris_upper_CI,
                               x=start_hour))
     }
      p <- p + ylim(0,NA)+
       ggtitle("Estimert reell strømpris")

     #p <- ggplot(data = updated_dt_hourly(),aes(x=start_hour,y=price))+
    #   geom_line()+
    #   geom_point(aes(y=price-1))


     height <- session$clientData$output_p_height
     width <- session$clientData$output_p_width
     ggplotly(p, height = height, width = width) %>%
       layout(hovermode = "x unified")
    })

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
