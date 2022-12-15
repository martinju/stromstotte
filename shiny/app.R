# TODO:

#TODO før release
# Sjekk at nettleie-navn er kompatibelt på tvers av datasett
# Sjekk bug med postnr 2863 + "SØR AURDAL ENERGI AS"
#DONE## Lag tab med endringslog (som henter .md fil som oppdateres)
#DON## Lag fane med strømstøtte der Rmarkdown-fila legges inn.
#DONE# Legg til feedback-knapp som linker til issues på github, samt epostdresse
#DONE # sotte -> støtte i plotlytab-vinduet
# Gjør "Oversikt" litt bedre (få inn fast og effektbasert nettleie her)
#DONE for minstrompris (ikke dinstrompris)# Legg til google analytics.
# Legg til fast og effektbasert nettleie i oppsummeringen under til høyre.
#DONE# Skaler y-aksen slik at den passer til input
#DONE (oppdatert plotly)# Sjekk hvorfor ting ikke blir riktig i linux.
#DONE # Ha to desimaler på y-aksen i plot.
# Diskuter med ELin: Utforming av tekst + hvordan få frem at dette er noe annnet enne alle andre gjør.
# Nevn at det senere blir mulig å legge inn strømavtale
#DONE# Send mail til drift om å sette kjøre dette på virtuell maskin.
# La elin vurdere farger og bakgrunn.
# Fix datoer -- litt på vei der med tickformat

# Neste på lista:
#DONE # Reduser størrelse på hovertext, la det også kun stå klokkeslett og pris
#DONE # Legg til farge på zeroline
# Legg til "din strømpris nå (detaljert)" (samme som i enkel bare med mulighet til å velge de øvrige)
# "Oversikt" -> "Tallgrunnlag"
# Oppdater tekst med hva du ser (link til detaljert oversikt, hva som kommer sendere og ikke er inkludert o.l.)
# La Elin vurdere
# Prøv mer kontraster  # farger,
#JA  # om jeg skal ha postnummer i header
#NEI  # om jeg KAN ha hele dagens + morgendagens hvis mulig (altså alltid mellom 24 og 48 timer, kontra mellom 12 og 36) -- da kan jeg ha knapper, men default er å vise totalen
#OK slik det er  # Er det problematisk at man ikke får opp valg av postnummer som førstevalg på mobil?
# Se nedenfor  # Hva bør jeg kalle boksen med tallgrunnlag? Priskomponenter kanskje?
##DONE Fiks setlocale = norsk
### END ###

### tilbakemeldigner fra ELin
# Prøv hvit bakgrunn
# Større NÅ
# færre tall på y-aksen
# bruk vindu nå + fremover
# ja til postummer i header på plott ( kanskje i en annen farge)
# tallgrunnlagboks: Din strømpris består av, og nederest i samme boks kan "din strømregning inkluderer også" der


# Før release:
#Vis kun nettleverandør + prisområde som selective hvis ikke unik
# vis postnummer i plotly-header
# oppdater "estimering av strømstøtte"
# finpuss modellbeskrivelsen ref slider
# Få interne linker til å fungere
# Legg til info om effekttariff

# Automatisk deployment ved opplasting til GitHub

#DONE# Editer hoovertekst
#DONE (for now)# Plot innværende døgn hvis før kl 13, plot fra nå og ut neste døgn hvis neste døgn er kjørt. # 1
#DONE # Legg til tekst på siden som viser nettleie, estimert strømstøtte osv for aktuelt valg.
# Sjekk at nettleie-navn er kompatibelt på tvers av datasett
# Legg til valg av konfidensgrad i avanserte innstillinger.
# Legg til en tab som heter "Om siden" der du forklarer hva som gjøres, hvor data er hentet fra osv. # 2
#DONE # Legg til "Laget av Martin Jullum, Norsk Regnesentral" nederst på side panel
# Sjekk bug med postnr 2863 + "SØR AURDAL ENERGI AS"
#LATER # Kan jeg vise nettselskap/prisområdevalg kun dersom det er flere valg?
# Bruk visningsdato + estimeringsdato i plottet. # 0
# Historisk estimering
# Lagre simulerte stier for fremtidige strømpriser i egen fil
# La strømstøtteordning ligge i selve appen
# Knapper som fjerner noen av grafene (make color an aes by adding a new column for that to the data and add it to tooltip) # 3

### Nytt oppsett
# Vis alle historiske strømpriser (så langt jeg har observert), men default view er to siste dager som nå
# vis strømstøtten for siste observerte dag
# dragslider under samt knapper med siste uke, måned, år og alt bør med
# Under der igjen vises slider som man kan dra på for å vise andre konfidensintervaller.
## TODO for nytt oppsett
## Lag 1 kombinert datasett som kan brukes til ALT. Skill på estimert og faktisk strømstøtte



# Hvis båndbredde blir et problem enten ved at server bruker for mye ram, eller at det tar tid så kan jeg lage meg ny fil som inneholder nyeste
# for inneværende måned (med alle kvantiler) + fullobserverte for alle tidligere måneder. Dersom jeg ønsker s¨å vise strømstøtten for andre dager,
# får jeg så laste inn en annen fil


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

#Sys.setlocale("LC_ALL", "en_US.UTF-8") # UTF-8 to get latin letters
#Sys.setlocale("LC_ALL", "en_US") # UTF-8 to get latin letters
#Sys.setlocale("LC_ALL", "en_US.UTF-8") # UTF-8 to get latin letters
Sys.setlocale(locale='no_NB.UTF-8')

library(data.table)

deployed <- TRUE

if(deployed){
  path <- "https://raw.githubusercontent.com/martinju/stromstotte/before_release/"
} else{
  path <- "../"
}

source("helper_funcs.R")

dt_nettleie <- fread(file.path(path,"data/database_nettleie_simple.csv"))
dt_nettleie_kl6 <- fread(file.path(path,"data/database_nettleie_simple_kl_6.csv"))
dt_hourly <- fread(file.path(path,"data/database_nordpool_hourly.csv"))
dt_comp <- fread(file.path(path,"data/historic_estimated_compensation.csv"))
dt_postnr_nettselskap_prisomraader_map <- fread(file.path(path,"data/simple_postnr_nettselskap_prisomraader_dt.csv"))

# Should do this in another file!
dt_postnr_nettselskap_prisomraader_map[,postnr:=as.character(postnr)]
dt_postnr_nettselskap_prisomraader_map[,nchar_postnr:=nchar(postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==1,postnr:=paste0("000",postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==2,postnr:=paste0("00",postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==3,postnr:=paste0("0",postnr)]
dt_postnr_nettselskap_prisomraader_map[,nchar_postnr:=NULL]

dt_postnr_nettselskap_prisomraader_map[,prisomraade:=gsub(" ","",prisomraade,fixed=T)]

dt_nettleie[,Energiledd:=Energiledd/100]


#####

# Colorblind friendly colors from here: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
mycols <- c(spotpris=cbbPalette[1],
            nettleie = cbbPalette[2],
            stotte = cbbPalette[4],
            totalpris = cbbPalette[3])
mylabels <- c(spotpris= "Spotpris",
            nettleie = "Nettleie",
            stotte = "Strømstøtte",
            totalpris = "Din strømpris")


######

rmdfiles <- c("stromstotte_shiny_description.Rmd")
sapply(rmdfiles, knitr::knit, quiet = T)


#######

today <- Sys.Date()


input_mapper <- copy(dt_postnr_nettselskap_prisomraader_map)

unique_postnr <- unique(input_mapper[,postnr])

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(scales)
library(knitr)
library(htmltools)
library(markdown)

## app.R ##

header <- dashboardHeader(title = "Din strømpris",titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    selectizeInput("postnr","Skriv inn postnummer",choices=NULL),
    selectInput("nettselskap","Velg Nettselskap",""),
    selectInput("prisomraade","Velg Prisområde",""),
    menuItem("Din strømpris nå", tabName = "strompris_naa", icon = icon("dashboard",verify_fa = FALSE)),
    menuItem("Detaljert historisk strømpris", tabName = "strompris_history", icon = icon("dashboard",verify_fa = FALSE)),
#    menuItem("Estimering av strømstøtte", tabName = "stromstotte", icon = icon("bolt",verify_fa = FALSE)),
#    menuItem("Fremtidig strømpris", tabName = "strompris", icon = icon("bolt",verify_fa = FALSE)),
#    menuItem("Historisk estimering", tabName = "historic", icon = icon("bolt",verify_fa = FALSE)),
    menuItem("Eksperimentering", tabName = "experimental", icon = icon("gear",verify_fa = FALSE)),
    menuItem("Om siden", tabName = "about", icon = icon("info",verify_fa = FALSE)),
    menuItem("Endringslogg", tabName = "changelog", icon = icon("info",verify_fa = FALSE)),
    tags$html(
      tags$h5(
        tags$em("Laget av ",
                tags$a(href="https://martinjullum.com", "Martin Jullum"),
                " Norsk Regnesentral"
        ),
        style = "text-align: center"
      )
    )
  )
)

body_strompris_naa <- tabItem(tabName = "strompris_naa",
                              fluidPage(
#                                tags$style(".topimg {
#                            margin-left:-30px;
##                            margin-right:-30px;
#                            margin-top:-15px;
#                          }")
                                #style='padding:-10px;padding:0 !important',
#                                tags$head(tags$style(HTML('
#.box {margin-top: 2px;margin-left: 0px; margin-right: 0px; margin-bottom:2px;padding:-10px}
#div {padding: 0 !important;}'
#                                ))),
                                tags$head(includeHTML("google_analytics.html")),
                                plotlyOutput("now_spotplot3",height ="300px"),
                                fluidRow(
                                  box(width = 8,
                                      h3("Hva ser du?"),
                                      #                                    p("Dagens strømprissytem med store svininger variabel og effektbasert nettleie")
                                      p("Statens strømstøtteordning har direkte påvirkning på din timespris på strøm."),
                                      p("Ved å taste inn ditt postnummer i margen til venstre viser grafen ovenfor din strømpris idag/imorgen for nettopp deg, angitt som:"),
                                      p(
                                        strong(
                                          tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                          "=",
                                          tags$span(style=paste0("color:",mycols['spotpris']),"spotpris"),
                                          "+",
                                          tags$span(style=paste0("color:",mycols['nettleie']),"nettleie"),
                                          "-",
                                          tags$span(style=paste0("color:",mycols['stotte']),"strømstøtte")
                                        )
                                      ),
                                      p("Grunnen til at din strømpris vises som et estimat (m/usikkerhet) er at strømstøtten er basert på gjennomsnittlig spotpris i inneværende måned, og dermed ikke er kjent før månedsslutt.",
                                        "Strømstøtten vist ovenfor derfor basert på simuleringer av fremtidige spotpriser ",
                                        tags$a(href="https://martinjullum.com/sideprojects/stromstotte/","(fra en statistisk modell)"),"."
                                      ),
                                      h4("Merk"),
                                      p("Faste og effektbasert månedsavgift fra nettleverandør kommer i tillegg på regningen fra nettleverandør."),
                                      p("Faste (typisk 0-50 kr/mnd) og forbruksbaserte (typisk 0-5 øre/kWh) kommer i tillegg på regningen fra din strømleverandør.")
                                  ),
                                  box(width = 4,
                                      title = "Oversikt",
                                      uiOutput("nettleie"),
                                      uiOutput("stromstotte")
                                  )
                                )
                              )
)
body_strompris_history <- tabItem(tabName = "strompris_history",
                                  h3("Detaljert/historisk strømpris"),
                                  fluidPage(
                                    plotlyOutput("history_spotplot"),
                                    fluidRow(
                                      box(width = 12,
                                          h3("Forklaring"),
                                          #                                    p("Dagens strømprissytem med store svininger variabel og effektbasert nettleie")
                                          p("Grafen ovenfor viser den forbruksbaserte prisen for spotpriskunder:"),
                                          p(
                                            strong(
                                              tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                              "=",
                                              tags$span(style=paste0("color:",mycols['spotpris']),"spotpris"),
                                              "+",
                                              tags$span(style=paste0("color:",mycols['nettleie']),"nettleie"),
                                              "-",
                                              tags$span(style=paste0("color:",mycols['stotte']),"strømstøtte")
                                            )
                                          ),
                                          p("Grunnen til at din pris vises som et estimat (m/usikkerhet) er at strømstøtten er ikke er kjent før månedsslutt,",
                                            "og strømstøtten derfor er estimert basert på en ",
                                            tags$a(href="https://martinjullum.com/sideprojects/stromstotte/","statistisk modell")
                                          ),
                                          h4("Merk"),
                                          p("Faste og effektbasert månedsavgift fra nettleverandør kommer i tillegg på regningen fra nettleverandør."),
                                          p("Faste (typisk 0-50 kr/mnd) og forbruksbaserte (typisk 0-5 øre/kWh) kommer i tillegg på regningen fra din strømleverandør.")
                                      ),
                                    )
                                  )
)


body_stromstotte <- tabItem(tabName = "stromstotte",
                            fluidPage(
                              withMathJax(includeMarkdown("stromstotte_shiny_description.md"))
                            )
)

body_strompris <- tabItem(tabName = "strompris",
        h2("Her kan jeg legge inn grafer med observerte strømpriser så langt + stier med predikerte strømpriser resten av måneden.")
)

body_historic <- tabItem(tabName = "historic",
        h2("Putt inn noe om historisk tilpasning her.")
)

body_experimental <- tabItem(tabName = "experimental",
                         h2("Eksperimentering"),
                         fluidPage(
                           box(
                             dateRangeInput("daterange_strompris_naa", "Datoer visning strømpris:",
                                            start = Sys.Date(),
                                            end = Sys.Date()+1,
                                            min = "2022-09-01",
                                            max = Sys.Date()+1,
                                            language = "no",
                                            weekstart = 1,
                                            format = "dd-mm-yyyy"),
                             sliderInput("hourstart_strompris_naa","Første time, visning strømpris",
                                         min = 0,
                                         max=23,
                                         value = 0, # TODO: Make floor if start data is current day
                                         step = 1),
                             sliderInput("hourend_strompris_naa","Siste time, visning strømpris",
                                         min = 0,
                                         max = 23,
                                         value = 23,
                                         step = 1)
                           ),
                           box(
                             dateInput("date_estimation", "Estimeringsdato strømstøtte:", # TODO: Make this update as visningsdato is changed.
                                       value = Sys.Date()-1,
                                       min = "2022-11-01", # TODO: Make generic
                                       max = Sys.Date(), # TODO: Make generic
                                       language = "no",
                                       weekstart = 1,
                                       format = "dd-mm-yyyy")
                           )
                         ),
                         box(width = 12,
                           plotlyOutput("spotplot")
                         )
)

body_about <- tabItem(tabName = "about",
                      box(
                        h2("Om siden"),
                        p("Hensikten med denne er å samle daglige spotpriser, nettleie og strømstøtte i en og samme løsning og presentere det i et brukervennlig format."),
                        p("Ved å taste inn postnummer, identifiseres både nettleverandør og spotprisområde og dagens (+ ev. morgendagens) priser visualiseres.",
                          "I enkelte tilfeller må brukeren velge blant 2 eller 3 nettleverandører/prisområder da postnummer overlapper med flere av disse."),
                        p("Grunnen til at din strømpris vises som et estimat (m/usikkerhet) er at strømstøtten er basert på gjennomsnittlig spotpris i inneværende måned, og dermed ikke er kjent før månedsslutt.",
                          "Strømstøtten er derfor basert på simuleringer av fremtidige spotpriser ",
                          tags$a(href="https://martinjullum.com/sideprojects/stromstotte/","(fra en statistisk modell)"),"."
                        ),
                        p("Det gjennomføres nye simuleringene daglig når morgendags spotpriser blir tilgjenglig  (ca kl. 13.30), som dermed endrer estimatet (og usikkerheten) til strømstøtten."),
                        p("Siden er laget av ",
                          tags$a(href="https://martinjullum.com/","Martin Jullum"),
                          "(",
                          tags$a(href="mailto:jullum@nr.no?subject=minstrompris.no","epost: jullum@nr.no"),
                          ") ",
                          "seniorforsker ",
                          tags$a(href="https://nr.no/","Norsk Regnesentral")
                          ),
                        p("Det er kun benyttet åpne datakilder (se til høyre)."),
                        p("All kildekode tilhørende dette prosjektet er tilgjenglig på ",
                          tags$a(href="https://github.com/martinju/stromstotte","github.com/martinju/stromstotte"),
                          "Fremtidige spotpriser simuleres automatisk ved hjelp av ",
                          tags$a(href="https://github.com/features/actions","GitHub actions"),".")
                      ),
                      box(
                        h2("Data"),
                        h4("Spotpriser"),
                        p("Spotpriser hentes daglig fra Nordpool sitt",
                          tags$a(href="https://www.nordpoolgroup.com/api/marketdata/page/23?currency=NOK","API")),

                        h4("Nettleie"),
                        p("Data med nettleie for nettselskapene i Norge er mottatt på epost 25.10.22 fra Roald Lien Glad, NVE. Samme informasjon er tilgjengelig gjennom NVEs ",
                          tags$a(href="https://biapi.nve.no/nettleietariffer/swagger/index.html", "API")),

                        h4("Postnummerområde"),
                        p("Data med geografisk område for postnumre er hentet i GeoJson format fra Kartverket via",
                          tags$a(href="https://kartkatalog.geonorge.no/metadata/postnummeromraader/462a5297-33ef-438a-82a5-07fff5799be3", "GeoNorge")),

                        h4("Nettkonsesjonsområder"),
                        p("Data med geografisk område for de ulike nettselskapene er hentet i GeoJson format fra NVEs",
                          tags$a(href="https://nedlasting.nve.no/gis/", "GIS tjeneste"), "(herunder 'Nettanlegg' -> 'Områdekonsesjonærer'"),

                        h4("Spotprisområder"),
                        p("Data med geografisk område for de fem spotprisområdene er 'manuelt' ekportet i GeoJson format fra NVEs",
                          tags$a(href="https://temakart.nve.no/tema/nettanlegg", "temakart for nettanlegg")),
                      ),
                      box(
                        h2("Innspill"),
                        p("Har du funnet feil, eller har forslag til forbedringer av tjenesten? Opprett gjerne et ",
                          tags$a(href="https://github.com/martinju/stromstotte/issues","'issue'"), "på sidens ",
                          tags$a(href="https://github.com/martinju/stromstotte/","GitHub repo"),".")
                      )
                      #                      verbatimTextOutput("datarange_strompris_naa")
)

body_changelog <- tabItem(tabName = "changelog",
                         htmltools::includeMarkdown("changelog.md")
)



body <-  dashboardBody(
  tags$script(HTML('
                                            $(document).ready(function() {
                                            $("header").find("nav").append(\'<div class="myClass"> &#8592; Velg postnummer </div>\');
                                            })
                                            ')),
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
  ),
#  tags$head(tags$style(HTML('
#.box {margin-top: 2px;margin-left: 0px; margin-right: 0px; margin-bottom:2px;padding:-10px}
#div {padding: 0 !important;}'
#  ))),
  tabItems(body_strompris_naa,
           body_strompris_history,
           body_stromstotte,
           body_strompris,
           body_historic,
           body_experimental,
           body_about,
           body_changelog)
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output,session) {

  # Server-side updating as you type with postnr
  updateSelectizeInput(session, 'postnr', choices = unique_postnr, server = TRUE)

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
     tmp <- dt_hourly[area ==input$prisomraade & date%in%seq(input$daterange_strompris_naa[1], input$daterange_strompris_naa[2],by="day"),]
     tmp[,keep:=TRUE]
     tmp[date==input$daterange_strompris_naa[1] & start_hour < input$hourstart_strompris_naa, keep:=FALSE]
     tmp[date==input$daterange_strompris_naa[2] & start_hour > input$hourend_strompris_naa, keep:=FALSE]
     ret <- tmp[keep==TRUE]
     ret[,keep:=NULL]
     ret
   })

   # Filter dt_comp based on input
   updated_dt_comp <- reactive({
     dt_comp[area == input$prisomraade & estimation_date==input$date_estimation,]
   })

   # Filter dt_comp based on input
   updated_dt_nettleie <- reactive({
     dt_nettleie[Nettselskap == input$nettselskap]
   })


   # TESTING
   output$data_spot <- renderTable(updated_dt_hourly())
   output$data_comp <- renderTable(updated_dt_comp())
   output$data_nettleie <- renderTable(updated_dt_nettleie())
   output$datarange_strompris_naa <- renderPrint(input$daterange_strompris_naa)

   output$nettleie <- renderUI({
     dagpris <- updated_dt_nettleie()[pristype=="Dag",Energiledd]
     nattpris <- updated_dt_nettleie()[pristype=="Natt",Energiledd]

     div(
       p("Nettselskap: ",strong(paste0(input$nettselskap))),
       p(paste0("Nettleie dag (06-22): ",twodigits(dagpris)," kr/kWh")),
       p(paste0("Nettleie natt (22-06): ",twodigits(nattpris)," kr/kWh"))
     )
   })

   output$stromstotte <- renderUI({
     med <- updated_dt_comp()[type=="median",compensation]
     upper <- updated_dt_comp()[type=="quantile_0.975",compensation]
     lower <- updated_dt_comp()[type=="quantile_0.025",compensation]

     div(
       p("Strømstøtte for prisområde: ",strong(input$prisomraade)),
       p("Estimat: ",twodigits(med)," kr/kWh"),
       p("95% konfidensintervall: (",twodigits(lower),",",twodigits(upper),") kr/kWh")
     )
   })

   plot_dt_final <- reactive({
     estimation_date0= dt_comp[,max(estimation_date)]

     updated_dt_nettleie0 <- dt_nettleie[Nettselskap == input$nettselskap]
     updated_dt_hourly0 <- dt_hourly[area ==input$prisomraade]
     updated_dt_comp0 <- dt_comp[area == input$prisomraade]

     #updated_dt_nettleie0 <- dt_nettleie[Nettselskap=="ELVIA AS"]
     #updated_dt_hourly0 <- dt_hourly[area=="NO1"]
     #updated_dt_comp0 <- dt_comp[area == "NO1"]

     updated_dt_hourly0[,computation_year:=year(date)]
     updated_dt_hourly0[,computation_month:=month(date)]



     updated_dt_comp0[,last_est_date_per_month:=max(estimation_date),by=.(computation_year,computation_month)]
     updated_dt_comp0_1 <- updated_dt_comp0[estimation_date==last_est_date_per_month & estimation_date!=estimation_date0 & type=="median"]
     updated_dt_comp0_2 <- updated_dt_comp0[estimation_date==estimation_date0 & type %in% c("median","quantile_0.025","quantile_0.975","lower_bound")]

     plot_strompris_naa_dt <- copy(updated_dt_hourly0)
     setnames(plot_strompris_naa_dt,"price","spotpris")

     dc1 <- dcast(updated_dt_comp0_1[,.(compensation,type,computation_year,computation_month)],formula= computation_year+computation_month~type,value.var="compensation")
     dc2 <- dcast(updated_dt_comp0_2[,.(compensation,type,computation_year,computation_month)],formula= computation_year+computation_month~type,value.var="compensation")

     setnames(dc1,"median","stotte")
     setnames(dc2,c("median","quantile_0.025","quantile_0.975","lower_bound"),c("stotte","stotte_lower_CI","stotte_upper_CI","stotte_lower_bound"))

     dc3 <- rbind(dc1,dc2,fill=TRUE)


     plot_strompris_naa_dt[start_hour %in% seq(6,21),nettleie:=updated_dt_nettleie0[pristype=="Dag",Energiledd]]
     plot_strompris_naa_dt[is.na(nettleie),nettleie:=updated_dt_nettleie0[pristype=="Natt",Energiledd]]

     plot_strompris_naa_dt <- merge(plot_strompris_naa_dt,dc3,by=c("computation_year", "computation_month"))

     plot_strompris_naa_dt[,totalpris:=spotpris+nettleie-stotte]
     plot_strompris_naa_dt[,totalpris_lower_CI:=spotpris+nettleie-stotte_upper_CI]
     plot_strompris_naa_dt[,totalpris_upper_CI:=spotpris+nettleie-stotte_lower_CI]


     plot_strompris_naa_dt[,datetime:=as.POSIXct(date)+start_hour*60*60]


     plot_strompris_naa_dt0 <- plot_strompris_naa_dt[,.(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI)]

     texthelper_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols))]
     texthelper_simple_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc_simple(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols))]
     texthelper_simple2_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc_simple3(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols,fontsize=9))]

     plot_strompris_naa_dt0_dup <- copy(plot_strompris_naa_dt0)
     plot_strompris_naa_dt0_dup[,datetime:=datetime+1*60*60-1]

     plot_strompris_naa_dt00 <- rbind(plot_strompris_naa_dt0,plot_strompris_naa_dt0_dup)
     setkey(plot_strompris_naa_dt00,datetime)


     plot_strompris_naa_dt_ints_totalpris <- plot_strompris_naa_dt00[,.(datetime,lower_CI=totalpris_lower_CI,upper_CI=totalpris_upper_CI)]
     plot_strompris_naa_dt_ints_totalpris[,type:="totalpris"]

     plot_strompris_naa_dt_ints_stotte <- plot_strompris_naa_dt00[,.(datetime,lower_CI=stotte_lower_CI,upper_CI=stotte_upper_CI)]
     plot_strompris_naa_dt_ints_stotte[,type:="stotte"]

     plot_strompris_naa_dt_ints <- rbind(plot_strompris_naa_dt_ints_totalpris,plot_strompris_naa_dt_ints_stotte)

     plot_strompris_naa_dt_melted <- melt(plot_strompris_naa_dt00[,.(datetime,spotpris,nettleie,totalpris,stotte)],
                                          id.vars = c("datetime"),variable.name = "type",value.name = "pris")

     plot_dt_final <- merge(plot_strompris_naa_dt_melted,plot_strompris_naa_dt_ints,by=c("datetime","type"),all = T)

     plot_dt_final[,linesize := "b"]
     plot_dt_final[type%in% c("totalpris","stotte"),linesize := "a"]

     setcolorder(plot_dt_final,c("datetime","type","pris","lower_CI","upper_CI","linesize"))
     plot_dt_final[,type:=factor(type,levels=c("spotpris","nettleie","stotte","totalpris"))]


     setkey(plot_dt_final,datetime)

     dt_list <-
     list(plot_dt_final=plot_dt_final,
          texthelper_dt=texthelper_dt,
          texthelper_simple_dt=texthelper_simple_dt,
          texthelper_simple2_dt=texthelper_simple2_dt,
          estimation_date0 = estimation_date0)

   })

   output$now_spotplot <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     dt_list <- plot_dt_final()

     max_dinstrompris <- dt_list$plot_dt_final[datetime>=dt_list$estimation_date0,max(upper_CI,na.rm = T)]
     now_hms =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())


     p_now <- ggplot(data=dt_list$plot_dt_final[datetime>=(now_hms-3*60*60)],mapping=aes(x=datetime,y=pris,col=type,fill=type))+
       geom_line(aes(size=linesize))+
       geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.3)+
       ggtitle("Din strømpris")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(15))+
       scale_x_datetime(name = "Tid/dato",
                        breaks=breaks_pretty(12),
                        minor_breaks = breaks_pretty(24))+
       #                 labels = label_date_short(format = c("%Y", "", "%d.%b\n", "%H\n"),sep=""))+
       scale_size_manual(values=c("a" = 1,"b"=0.5))+
       guides(size="none")+
       scale_color_manual(name="",values = mycols,labels = mylabels)+
       scale_fill_manual(name="",values = mycols,labels = mylabels)+
       #geom_vline(xintercept=Sys.time(),linetype=2,col="grey",inherit.aes=F)+
       geom_line(data=dt_list$texthelper_simple_dt[datetime>=(now_hms-3*60*60)],aes(x=datetime,y=max_dinstrompris,text=text),inherit.aes = F,size=0.00001)

     ggp_now <- ggplotly(p_now,dynamicTicks = TRUE,tooltip = "text")
     ggp_now <- layout(
       ggp_now,
       hovermode = "x unified",
       xaxis = list(fixedrange = TRUE),#,tickformat="%b<br>%Y"),
       yaxis = list(fixedrange = TRUE, tickformat = ".2f",nticks=20),
       legend = list(font = list(size=10))#,
#       legend = list(orientation = 'h')
     )
     ggp_now <- style(ggp_now,visible="legendonly",traces=c(1,2,3,7)) #trace=2 identified through plotly_json(ggp_now)
     ggp_now <- style(ggp_now,hoverinfo="none",traces=1:(length(ggp_now$x$data)-1))
     ggp_now <- style(ggp_now,name="strømstøtte",traces=3)
     ggp_now <- style(ggp_now,name="Din strømpris",traces=4)

     ggp_now <- config(ggp_now,locale="no")

     ggp_now

   })

   output$now_spotplot2 <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     dt_list <- plot_dt_final()

     max_dinstrompris <- dt_list$plot_dt_final[datetime>=dt_list$estimation_date0,max(upper_CI,na.rm = T)]
     now_hms =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())

     today_daterange <- as.POSIXct(range(dt_list$plot_dt_final[datetime>=dt_list$estimation_date & datetime<dt_list$estimation_date+1,datetime]))

#     p_now <- ggplot(data=dt_list$plot_dt_final[datetime>=(now_hms-3*60*60)],mapping=aes(x=datetime,y=pris,col=type,fill=type))+
     p_now <- ggplot(data=dt_list$plot_dt_final[datetime>=dt_list$estimation_date0],mapping=aes(x=datetime,y=pris,col=type,fill=type))+
       geom_line(aes(size=linesize))+
       geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.3)+
       ggtitle("Din strømpris")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(15))+
       scale_x_datetime(name = "Tid/dato",
                        breaks=breaks_pretty(12),
                        minor_breaks = breaks_pretty(24),
                        labels = label_date_short(format = c("%Y", "", "%d.%b\n", "%H:%M\n"),sep=""))+
       scale_size_manual(values=c("a" = 1,"b"=0.5))+
       guides(size="none")+
       scale_color_manual(name="",values = mycols,labels = mylabels)+
       scale_fill_manual(name="",values = mycols,labels = mylabels)+
       #geom_vline(xintercept=Sys.time(),linetype=2,col="grey",inherit.aes=F)+
     #  geom_line(data=dt_list$texthelper_simple_dt[datetime>=(now_hms-3*60*60)],aes(x=datetime,y=max_dinstrompris,text=text),inherit.aes = F,size=0.00001)
     geom_line(data=dt_list$texthelper_simple_dt[datetime>=dt_list$estimation_date0],aes(x=datetime,y=max_dinstrompris,text=text),inherit.aes = F,size=0.00001)

     ggp_now <- ggplotly(p_now,tooltip = "text",
                         dynamicTicks = "x")
                         #dynamicTicks = "y")
#                         dynamicTicks = TRUE)
     ggp_now <- layout(
       ggp_now,
       hovermode = "x unified",
       xaxis = list(ticks=12,
                    range = as.numeric(today_daterange),
                    rangeselector = list(
                      buttons = list(
                        list(
                          count = 1,
                          label = "idag",
                          step = "day",
                          stepmode = "todate"),
                        list(
                          count = 2,
                          label = "2 siste dager",
                          step = "day",
                          stepmode = "todate")
                      ))#,
#                    rangeslider = list(type = "date")
       ),
       yaxis = list(fixedrange = TRUE, tickformat = ".2f",nticks=20)
       )


     ggp_now <- style(ggp_now,visible="legendonly",traces=c(1,2,3,7)) #trace=2 identified through plotly_json(ggp_now)
     ggp_now <- style(ggp_now,hoverinfo="none",traces=1:(length(ggp_now$x$data)-1))
     ggp_now <- style(ggp_now,name="strømstøtte",traces=3)
     ggp_now <- style(ggp_now,name="Din strømpris",traces=4)

     ggp_now <- config(ggp_now,locale="no")

     ggp_now_true <- ggp_now
     ggp_now_y <- ggp_now

     all.equal(ggp_now_y,ggp_now_true)
     all.equal(ggp_now_true,ggp_now_y)

     ggp_now_y$x$layout$xaxis$type <- ggp_now_true$x$layout$xaxis$type
     ggp_now_y$x$layout$xaxis$autorange <- ggp_now_true$x$layout$xaxis$autorange
     ggp_now_y$x$layout$xaxis$autorange <- ggp_now_true$x$layout$xaxis$autorange

     # OK, looks like scaling and getting the axis right is conflicting. A workaround might be to work with
     # as.numeric(datetime) all the way from the start. However, not sure if I can add buttons to non-date data.

     ggp_now_y
   })

   output$now_spotplot3 <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     dt_list <- plot_dt_final()

     max_dinstrompris <- dt_list$plot_dt_final[datetime>=dt_list$estimation_date0,max(upper_CI,na.rm = T)]
     now_hms =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())

     dat <- dt_list$plot_dt_final[datetime>=(now_hms-3*60*60) & type=="totalpris"]
     dat <- dat[-1]
     #helper0 <- dt_list$texthelper_simple_dt[datetime>=(now_hms-3*60*60)]
     #setnames(helper0,"text","text0")
     #dat <-merge(dat,helper0,by="datetime",all=T)
     #dat[, text := text0[nafill(replace(.I, is.na(text0), NA), "locf")]]
     #dat[,text0:=NULL]

     plotrange <- c(min(dat$lower_CI),max(dat$upper_CI))
     plotrange2 <- plotrange
     plotrange2[2] <- plotrange2[2] + diff(plotrange)*0.05
     plotrange3 <- plotrange2
     plotrange3[1] <- plotrange2[1] - diff(plotrange)*0.01

     helper0 <- dt_list$texthelper_simple2_dt[datetime>=(now_hms-3*60*60)]
     helper0 <- merge(helper0,dat[,.(datetime,upper_CI)],by="datetime")
#     helper0[,plotval:=upper_CI+diff(plotrange)*0.1]
     helper0[,plotval:=max(upper_CI)+diff(plotrange)*0.05]

     helper0[,datetime2:=datetime+0.5*60*60]

     dt <- data.table(x=rep(Sys.time(),2),y=plotrange3)

     p_now <- ggplot(data=dat,mapping=aes(x=datetime,y=pris))+
       geom_line(col=mycols["totalpris"],size=1)+
       geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),fill=mycols["totalpris"], alpha = 0.3)+
       ggtitle("Din strømpris")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(15))+
       scale_x_datetime(name = "Tid/dato",
                        date_breaks="2 hours",
                        #minor_breaks = breaks_pretty(24),
                        labels = label_date_short(format = c("", "", "%d.%b\n", "%H\n"),sep=""))+
       geom_line(dt,mapping = aes(x=x,y=y),linetype=2,col="grey",size=0.75)+
       #annotate("text", x = dt[1,x]+20*60, y =  plotrange2[1], label = "NÅ")+
#       geom_vline(xintercept=Sys.time(),linetype=2,col="grey",inherit.aes=F)+
       geom_line(data=helper0,aes(x=datetime2,y=plotval,text=text),inherit.aes = F,size=0.00001)


#p_now

     #+
     #  #                 +
     #  scale_size_manual(values=c("a" = 1,"b"=0.5))+
    #   guides(size="none")+
    #   scale_color_manual(name="",values = mycols,labels = mylabels)+
    #   scale_fill_manual(name="",values = mycols,labels = mylabels)+

     ggp_now <- ggplotly(p_now,tooltip = "text")
     ggp_now <- layout(
       ggp_now,
       annotations=list(x=as.numeric(dt[1,x]),
                        y=plotrange3[1],
                        text="NÅ",
                        showarrow=FALSE,
                        font=list(size=10)),
       hovermode = "x unified",
       xaxis = list(constrain="domain",
                    fixedrange = TRUE,
                    nticks=24,
                    range=dat[,as.numeric(range(datetime))]
                    ),#,tickformat="%b<br>%Y"),
       yaxis = list(constrain="domain",
                    fixedrange = TRUE,
                    tickformat = ".2f",
                    nticks=20,
                    zeroline = TRUE,
                    range=plotrange3
                    ),
       legend = list(font = list(size=10)),
       hoverlabel = list(font=list(color = mycols["totalpris"],size=9))#,
       #       legend = list(orientation = 'h')
     )
#     ggp_now <- style(ggp_now,visible="legendonly",traces=c(1,2,3,7)) #trace=2 identified through plotly_json(ggp_now)
     ggp_now <- style(ggp_now,hoverinfo="none",traces=1:(length(ggp_now$x$data)-1))
 #    ggp_now <- style(ggp_now,name="strømstøtte",traces=3)
#     ggp_now <- style(ggp_now,name="Din strømpris",traces=4)

     ggp_now <- config(ggp_now,locale="no")

     ggp_now

   })

   output$now_spotplot4 <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     dt_list <- plot_dt_final()

     max_dinstrompris <- dt_list$plot_dt_final[datetime>=dt_list$estimation_date0,max(upper_CI,na.rm = T)]
     now_hms =as.POSIXct(as.IDate(Sys.Date()))#+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())

     dat <- dt_list$plot_dt_final[datetime>=(now_hms-24*60*60) & type=="totalpris"]
     dat <- dat[-1]
     #helper0 <- dt_list$texthelper_simple_dt[datetime>=(now_hms-3*60*60)]
     #setnames(helper0,"text","text0")
     #dat <-merge(dat,helper0,by="datetime",all=T)
     #dat[, text := text0[nafill(replace(.I, is.na(text0), NA), "locf")]]
     #dat[,text0:=NULL]

     plotrange <- c(min(dat$lower_CI),max(dat$upper_CI))
     plotrange2 <- plotrange
     plotrange2[2] <- plotrange2[2] + diff(plotrange)*0.05
     plotrange3 <- plotrange2
     plotrange3[1] <- plotrange2[1] - diff(plotrange)*0.01

     helper0 <- dt_list$texthelper_simple2_dt[datetime>=(now_hms-24*60*60)]
     helper0 <- merge(helper0,dat[,.(datetime,upper_CI)],by="datetime")
     #     helper0[,plotval:=upper_CI+diff(plotrange)*0.1]
     helper0[,plotval:=max(upper_CI)+diff(plotrange)*0.05]

     helper0[,datetime2:=datetime+0.5*60*60]

     dt <- data.table(x=rep(Sys.time(),2),y=plotrange3)

     p_now <- ggplot(data=dat,mapping=aes(x=datetime,y=pris))+
       geom_line(col=mycols["totalpris"],size=1)+
       geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),fill=mycols["totalpris"], alpha = 0.3)+
       ggtitle("Din strømpris")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(15))+
       scale_x_datetime(name = "Tid/dato",
                        date_breaks="2 hours",
                        #minor_breaks = breaks_pretty(24),
                        labels = label_date_short(format = c("", "", "%d.%b\n", "%H\n"),sep=""))+
       geom_line(dt,mapping = aes(x=x,y=y),linetype=2,col="grey",size=0.75)+
       #annotate("text", x = dt[1,x]+20*60, y =  plotrange2[1], label = "NÅ")+
       #       geom_vline(xintercept=Sys.time(),linetype=2,col="grey",inherit.aes=F)+
       geom_line(data=helper0,aes(x=datetime2,y=plotval,text=text),inherit.aes = F,size=0.00001)+
       theme_bw()


     #p_now

     #+
     #  #                 +
     #  scale_size_manual(values=c("a" = 1,"b"=0.5))+
     #   guides(size="none")+
     #   scale_color_manual(name="",values = mycols,labels = mylabels)+
     #   scale_fill_manual(name="",values = mycols,labels = mylabels)+

     ggp_now <- ggplotly(p_now,tooltip = "text")
     ggp_now <- layout(
       ggp_now,
       annotations=list(x=as.numeric(dt[1,x]),
                        y=plotrange3[1],
                        text="NÅ",
                        showarrow=FALSE,
                        font=list(size=10)),
       hovermode = "x unified",
       xaxis = list(constrain="domain",
                    fixedrange = TRUE,
                    nticks=24,
                    range=dat[,as.numeric(range(datetime))]
       ),#,tickformat="%b<br>%Y"),
       yaxis = list(constrain="domain",
                    fixedrange = TRUE,
                    tickformat = ".2f",
                    nticks=20,
                    zeroline = TRUE,
                    range=plotrange3
       ),
       legend = list(font = list(size=10)),
       hoverlabel = list(font=list(color = mycols["totalpris"],size=9))#,
       #       legend = list(orientation = 'h')
     )
     #     ggp_now <- style(ggp_now,visible="legendonly",traces=c(1,2,3,7)) #trace=2 identified through plotly_json(ggp_now)
     ggp_now <- style(ggp_now,hoverinfo="none",traces=1:(length(ggp_now$x$data)-1))
     #    ggp_now <- style(ggp_now,name="strømstøtte",traces=3)
     #     ggp_now <- style(ggp_now,name="Din strømpris",traces=4)

     ggp_now <- config(ggp_now,locale="no")

     ggp_now

   })


   output$history_spotplot <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     dt_list <- plot_dt_final()

     p_history <- ggplot(data=dt_list$plot_dt_final,mapping=aes(x=datetime,y=pris,col=type,fill=type))+
       geom_line(aes(size=linesize))+
       geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.3)+
#       ggtitle("Din strømpris")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(15))+
       scale_x_datetime(name = "Tid/dato",
                        breaks=breaks_pretty(12),
                        minor_breaks = breaks_pretty(24),
                        labels = label_date_short(format = c("%Y", "", "%d.%b\n", "%H:%M\n"),sep=""))+
       scale_size_manual(values=c("a" = 1,"b"=0.5))+
       scale_color_manual(name="",values = mycols)+
       scale_fill_manual(name="",values = mycols)+
       guides(size="none")+
       geom_line(data=dt_list$texthelper_dt,aes(x=datetime,y=0,text=text),inherit.aes = F,size=0.00001)

     ggp_history <- ggplotly(p_history,dynamicTicks = TRUE,tooltip = "text")
     ggp_history <- layout(
       ggp_history,
       hovermode = "x unified",
       xaxis = list(
         rangeselector = list(
           buttons = list(
             list(
               count = 1,
               label = "denne måned",
               step = "month",
               stepmode = "todate"),
             list(
               count = lubridate::wday(Sys.Date(),week_start=1),
               label = "denne uka",
               step = "day",
               stepmode = "todate"),
             list(
               count = 1,
               label = "idag",
               step = "day",
               stepmode = "todate"),
             list(
               count = 2,
               label = "2 siste dager",
               step = "day",
               stepmode = "todate")
           )),
         rangeslider = list(type = "date")
       ),
       yaxis = list(tickformat = ".2f",fixedrange = FALSE)
     )

     ggp_history <- style(ggp_history,visible="legendonly",traces=c(3,7)) #trace=2 identified through plotly_json(ggp_history)
     ggp_history <- style(ggp_history,hoverinfo="none",traces=1:8)

     ggp_history <- config(ggp_history,locale="no")

     ggp_history

   })

   ### OLD ###

   output$spotplot <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     updated_dt_nettleie0 <- updated_dt_nettleie()#dt_nettleie[Nettselskap=="ELVIA AS"]
     updated_dt_hourly0 <- updated_dt_hourly()#dt_hourly[area=="NO1" & date==today]
     updated_dt_comp0 <- updated_dt_comp()#dt_comp[area == "NO1" & estimation_date==today-1,]

     #updated_dt_nettleie0 <- dt_nettleie[Nettselskap=="ELVIA AS"]
     #updated_dt_hourly0 <- dt_hourly[area=="NO1" & date%in%(c(today-1,today))]
     #updated_dt_comp0 <- dt_comp[area == "NO1" & estimation_date==today-1,]

     #updated_dt_nettleie0 <- dt_nettleie[Nettselskap=="BARENTS NETT AS"]
     #updated_dt_hourly0 <- dt_hourly[area=="NO4" & date==today]
     #updated_dt_comp0 <- dt_comp[area == "NO4" & estimation_date==today-1,]

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
     plot_strompris_naa_dt[,datetime:=as.POSIXct(date)+start_hour*60*60]



     tmp_melting_dt <- plot_strompris_naa_dt[,.(datetime,nettleie,spotpris,totalpris_median,totalpris_lower_CI, totalpris_upper_CI, totalpris_upper_bound)]
     tmp_melting_dt0 <- tail(tmp_melting_dt,1)
     tmp_melting_dt0[,datetime:=datetime+1*60*60-1]
     tmp_melting_dt <- rbind(tmp_melting_dt,tmp_melting_dt0)
     setkey(tmp_melting_dt,datetime)
     plot_strompris_naa_dt_melted <- melt(tmp_melting_dt,id.vars = c("datetime"),variable.name = "type",value.name = "pris")

     plot_strompris_naa_dt_ints <- plot_strompris_naa_dt[,.(datetime,totalpris_lower_CI,totalpris_upper_CI)]
     plot_strompris_naa_dt_ints2 <- copy(plot_strompris_naa_dt_ints)
     plot_strompris_naa_dt_ints2[,datetime:=datetime+1*60*60-1]


     now_h =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60
     now_hms =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())

     tmp_now <- plot_strompris_naa_dt[datetime==now_h]
     tmp_now[,datetime:=now_hms]

     if(now_h<=plot_strompris_naa_dt[,max(datetime)]){
       plot_strompris_naa_dt <- rbind(plot_strompris_naa_dt,tmp_now)
       setkey(plot_strompris_naa_dt,datetime)
     }


     p <- ggplot(mapping=aes(x=datetime,y=pris))+
       geom_line(aes(y=nettleie,text=paste0("<span style='text-decoration:underline'><b>Priser (NOK/kWh) kl ",start_hour,"-",start_hour+1,": </b></span>\n",
                                            "<span style='color:#619CFF'>Spot: ",twodigits(spotpris),"</span>\n",
                                            "<span style='color:#00BA38'>Nettleie: ",twodigits(nettleie),"</span>\n\n",
                                            "<span style='color:#F8766D'><b>Din pris:</b>\n",
                                            "Estimat: ",twodigits(totalpris_median)," (",twodigits(totalpris_lower_CI),", ",twodigits(totalpris_upper_CI),")","\n",
                                            "Øvre grense: ",twodigits(totalpris_upper_bound),"</span>")),
                 size=0.0001,data=plot_strompris_naa_dt)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="spotpris"],direction = "hv",col=scales::hue_pal()(3)[3])+
       geom_step(data=plot_strompris_naa_dt_melted[type=="nettleie"],direction = "hv",col=scales::hue_pal()(3)[2])+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_upper_bound"],direction = "hv",col=scales::hue_pal()(3)[1],linetype=2,alpha=0.5)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_lower_CI"],direction = "hv",col=scales::hue_pal()(3)[1],alpha=0.5)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_upper_CI"],direction = "hv",col=scales::hue_pal()(3)[1],alpha=0.5)+
       geom_step(data=plot_strompris_naa_dt_melted[type=="totalpris_median"],direction = "hv",col=scales::hue_pal()(3)[1],size=1)

     if(now_h<=plot_strompris_naa_dt[,max(datetime)]){
       p <- p + geom_vline(xintercept=Sys.time(),type=2,col="grey") # TODO: Fix such that this is not displayed in the hoover
       }
#     for(i in seq_len(nrow(plot_strompris_naa_dt_ints))){
#       p <- p + geom_ribbon(data=rbind(plot_strompris_naa_dt_ints[i],
#                                       plot_strompris_naa_dt_ints2[i]),alpha=0.3,inherit.aes=FALSE, fill=scales::hue_pal()(3)[1],
#                   mapping=aes(ymin=totalpris_lower_CI,
#                               ymax=totalpris_upper_CI,
#                               x=datetime))
#     }


     plot_strompris_naa_dt_ints_comb <- rbind(plot_strompris_naa_dt_ints,
                                              plot_strompris_naa_dt_ints2)

     setkey(plot_strompris_naa_dt_ints_comb,datetime)
     p <- p + geom_ribbon(data=plot_strompris_naa_dt_ints_comb,alpha=0.3,inherit.aes=FALSE,
                          fill=scales::hue_pal()(3)[1],
                          mapping=aes(ymin=totalpris_lower_CI,
                                      ymax=totalpris_upper_CI,
                                      x=datetime))


      p <- p + expand_limits(y=0)+
       ggtitle("Din strømpris")+
        scale_y_continuous(name = "Pris (NOK/kWh)",labels=scaleFUN,breaks = breaks_extended(15))+
        scale_x_datetime(name = "Tid/dato",
                         breaks=breaks_pretty(12),
                         minor_breaks = breaks_pretty(24),
                         labels = label_date_short(format = c("%Y", "", "%d.%b\n", "%H:%M\n"),sep="")) # TODO: Get Norwegian months
     #p <- ggplot(data = updated_dt_hourly(),aes(x=start_hour,y=price))+
    #   geom_line()+
    #   geom_point(aes(y=price-1))


     height <- session$clientData$output_p_height
     width <- session$clientData$output_p_width
     ggplotly(p, height = height, width = width,tooltip = "text",dynamicTicks = "x") %>%
       layout(hovermode = "x unified",
#              yaxis=list(fixedrange=TRUE),
              xaxis = list(rangeslider = list(visible = FALSE,type="date"))) %>%
       style(p, hoverinfo = "none", traces = 2:(length(p$layers)))
    })

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
