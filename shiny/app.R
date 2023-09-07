# TODO:

# Last opp til server og se at ting funker der
#
# Spør Elin om jeg bør ta bort Spotpris og "din strømpris nå" fra Din strømpris består av.
#DONE # Nettleiernavn på tvers av datasett
#DONE# Sjekk bug med postnr 2863 + "SØR AURDAL ENERGI AS"
# SJekk tidspunkt for "NÅ" i grafen på NR-server
# oppdater "estimering av strømstøtte"/finpuss modellbeskrivelsen ref slider
# Skriv tekst til linkedin

#DONE # Vis kun nettleverandør + prisområde som selective hvis ikke unik
#DONE # vis postnummer i plotly-header
#DONE# Få interne linker til å fungere
#DONE # Legg til info om effekttariff
#DONE # vurder å bytte rundt på farger



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
#DONE# Prøv hvit bakgrunn
#DONE # Større NÅ
#DONE# færre tall på y-aksen
#DONE # bruk vindu nå + fremover
# ja til postummer i header på plott ( kanskje i en annen farge)
# tallgrunnlagboks: Din strømpris består av, og nederest i samme boks kan "din strømregning inkluderer også" der
#DONE# Endringslogg inn i Om siden

# Før release:
#Vis kun nettleverandør + prisområde som selective hvis ikke unik
# vis postnummer i plotly-header
# oppdater "estimering av strømstøtte"
# finpuss modellbeskrivelsen ref slider
# Få interne linker til å fungere
# Legg til info om effekttariff
# vurder å bytte rundt på farger

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
Sys.setlocale(locale='nb_NO.utf8')

library(data.table)

deployed <- FALSE

if(deployed){
  path <- "https://raw.githubusercontent.com/martinju/stromstotte/master"
  #path <- "https://raw.githubusercontent.com/martinju/stromstotte/before_release"
} else{
  path <- "../"
}

source("helper_funcs.R")

dt_nettleie <- fread(file.path(path,"data/database_nettleie_simple.csv"),encoding = "Latin-1")
dt_nettleie_kl6 <- fread(file.path(path,"data/database_nettleie_simple_kl_6.csv"))
dt_nettleie_kapasitetsledd <- fread(file.path(path,"data/database_nettleie_kapasitetsledd.csv"),encoding = "Latin-1")

dt_hourly <- fread(file.path(path,"data/database_nordpool_hourly.csv"))
dt_filtered_prices <- fread(file.path(path,"data/historic_filtered_prices_sept23_system.csv"))
dt_filtered_prices_org_comp_system <- fread(file.path(path,"data/historic_filtered_prices_original_system.csv"))



dt_comp <- fread(file.path(path,"data/historic_estimated_compensation.csv"))
dt_postnr_nettselskap_prisomraader_map <- fread(file.path(path,"data/simple_postnr_nettselskap_prisomraader_dt.csv"))

# Should do this in another file!
dt_postnr_nettselskap_prisomraader_map[,postnr:=as.character(postnr)]
dt_postnr_nettselskap_prisomraader_map[,nchar_postnr:=nchar(postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==1,postnr:=paste0("000",postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==2,postnr:=paste0("00",postnr)]
dt_postnr_nettselskap_prisomraader_map[nchar_postnr==3,postnr:=paste0("0",postnr)]
dt_postnr_nettselskap_prisomraader_map[,nchar_postnr:=NULL]

dt_postnr_nettselskap_prisomraader_map[nettselskap=="TROLLFJORD KRAFT AS",nettselskap:="TROLLFJORD NETT AS"]
dt_postnr_nettselskap_prisomraader_map[nettselskap=="HALLINGDAL KRAFTNETT A/S",nettselskap:="HALLINGDAL KRAFTNETT AS"]
dt_postnr_nettselskap_prisomraader_map[nettselskap=="NORE ENERGI AS",nettselskap:="FØIE AS"]

dt_postnr_nettselskap_prisomraader_map[,keep:=TRUE]
dt_postnr_nettselskap_prisomraader_map[nettselskap=="FORSVARSBYGG",keep:=FALSE]
dt_postnr_nettselskap_prisomraader_map[nettselskap=="HERØYA NETT AS",keep:=FALSE]
dt_postnr_nettselskap_prisomraader_map[nettselskap=="FIVEN NORGE AS",keep:=FALSE]
dt_postnr_nettselskap_prisomraader_map <- dt_postnr_nettselskap_prisomraader_map[keep==TRUE][,keep:=NULL]


dt_nettleie[,keep:=TRUE]
dt_nettleie[Nettselskap=="TINFOS AS",keep:=FALSE]
dt_nettleie[Nettselskap=="ARVA AS*",Nettselskap:="ARVA AS"]
dt_nettleie <- dt_nettleie[keep==TRUE][,keep:=NULL]


dt_postnr_nettselskap_prisomraader_map[,prisomraade:=gsub(" ","",prisomraade,fixed=T)]

dt_nettleie[,Energiledd:=Energiledd/100]

dt_nettleie_kapasitetsledd[,Forbruk:=paste0(`Kapasitetsledd fra kW`,"-",`Kapasitetsledd til kW`," kW")]
dt_nettleie_kapasitetsledd[,Kostnad:=paste0(twodigits(Kapasitetsledd)," kr/mnd")]
dt_nettleie_kapasitetsledd[,max:=max(Kapasitetsledd),by=Nettselskap]
dt_nettleie_kapasitetsledd[Kapasitetsledd==max & `Grunnlag effektrinn`!="Sikringsstørrelse",Forbruk:=paste0(">",`Kapasitetsledd fra kW`," kW")]

dt_nettleie_kapasitetsledd[`Grunnlag effektrinn`=="Topp 3 forbrukstopper innenfor 1 siste måneder", grunnlag_simple:="Gjennomsnitt av 3 høyeste timesforbruk siste måned"]
dt_nettleie_kapasitetsledd[`Grunnlag effektrinn`=="Sikringsstørrelse", grunnlag_simple:="sikringsstørrelse"]
dt_nettleie_kapasitetsledd[`Grunnlag effektrinn`=="Topp 1 forbrukstopper", grunnlag_simple:="høyeste timesforbruk siste måned"]
dt_nettleie_kapasitetsledd[`Grunnlag effektrinn`=="Topp 5 forbrukstopper innenfor 12 siste måneder", grunnlag_simple:="Gjennomsnitt av 5 høyeste timesforbruk siste år"]
dt_nettleie_kapasitetsledd[`Grunnlag effektrinn`=="Topp 2 forbrukstopper innenfor 1 siste måneder", grunnlag_simple:="Gjennomsnitt av 2 høyeste timesforbruk siste måned"]

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

# No longer need to compile this one
rmdfiles <- c("stromstotte_shiny_description.Rmd") # Vurdere å ikke hente tidspunkt for kompilering fra nettet inn i denne rmd-fila.
sapply(rmdfiles, knitr::knit, quiet = T)


#######

today <- Sys.Date()

is_weekend <- (weekdays(today) %in% c("lørdag","søndag"))

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
    id = "tabs",
    selectizeInput("postnr","Skriv inn postnummer",choices=NULL),
    tabsetPanel(
      id = "select_nettselskap",
      type = "hidden",
      tabPanel("multiple_nettselskap",
               selectInput("nettselskap","Velg Nettselskap","")
      ),
      tabPanel("single_nettselskap") # empty
    ),
    tabsetPanel(
      id = "select_prisomraade",
      type = "hidden",
      tabPanel("multiple_prisomraade",
               selectInput("prisomraade","Velg Prisområde","")
      ),
      tabPanel("single_prisomraade") # empty
    ),
    menuItem("Din strømpris nå", tabName = "strompris_naa", icon = icon("dashboard",verify_fa = FALSE),selected=T),
    menuItem("Din strømpris nå (detaljert)", tabName = "strompris_naa_detaljert", icon = icon("dashboard",verify_fa = FALSE)),
    menuItem("Historisk strømpris", tabName = "strompris_history", icon = icon("dashboard",verify_fa = FALSE)),
    menuItem("Estimering av strømstøtte", tabName = "stromstotte", icon = icon("bolt",verify_fa = FALSE)),
    #    menuItem("Fremtidig strømpris", tabName = "strompris", icon = icon("bolt",verify_fa = FALSE)),
    #    menuItem("Historisk estimering", tabName = "historic", icon = icon("bolt",verify_fa = FALSE)),
    #    menuItem("Eksperimentering", tabName = "experimental", icon = icon("gear",verify_fa = FALSE)),
    menuItem("Om siden", tabName = "about", icon = icon("info",verify_fa = FALSE)),
    #menuItem("Endringslogg", tabName = "changelog", icon = icon("info",verify_fa = FALSE)),
    tags$html(
      br(),
      p(actionLink("link_to_about", "dinstrompris.no v 0.1.5"),style = "text-align: center")
    ),
    tags$html(
      tags$h5(
        tags$em("Laget av ",
                tags$a(href="https://martinjullum.com", "Martin Jullum"),
                ",",
                tags$a(href="https://nr.no", "Norsk Regnesentral")
        ),
        style = "text-align: center"
      )
    )#,
#    div(class = "sticky_footer",p(" dinstrompris.no v0.1.3"))
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
                                  box(width = 7,
                                      h3("Hva viser grafen?"),
                                      #                                    p("Dagens strømprissytem med store svininger variabel og effektbasert nettleie")
                                      #                                      p("Statens strømstøtteordning har direkte påvirkning på din timespris på strøm."),
                                      p("Ved å taste inn ditt postnummer i margen til venstre viser grafen ovenfor ",
                                        tags$span(style=paste0("color:",mycols['totalpris']),"din strømpris"),
                                        " idag/imorgen for nettopp deg."),# angitt som:"),
                                      p(tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                        "hensyntar både nettleie og strømstøtte, og er definert som:"),
                                      strong(
                                        tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                        "=",
                                        tags$span(style=paste0("color:",mycols['spotpris']),"spotpris"),
                                        "+",
                                        tags$span(style=paste0("color:",mycols['nettleie']),"nettleie"),
                                        "-",
                                        tags$span(style=paste0("color:",mycols['stotte']),"strømstøtte\n")
                                      ),
                                      p(""),
                                      p("Etter at regjeringen endret strømstøtteordningen til timesbasert avregning fra 1.september 2023, er den faktiske strømstøtten kjent umiddelbart, og vises som et enkelt tall ovenfor. ",
                                        "Det månedsbaserte avregningssystemet som gjaldt før gjorde at strømstøtten ikke var kjent før månedsslutt. ",
                                        "Strømstøtten var da beregnet basert på simuleringer av fremtidige spotpriser fra en statistisk modell,",
                                      #                                        tags$a(href="https://martinjullum.com/sideprojects/stromstotte/","(fra en statistisk modell)"),
                                      actionLink("link_to_estimering_av_stromstotte2", "klikk her for detaljer"),
                                      ". Denne modellering er altså ikke nødvendig etter 1.september 2023."),
                                      p(actionLink("link_to_strompris_naa_detaljert", "Klikk her")," for å se en mer detaljert oppdeling av ",
                                        tags$span(style=paste0("color:",mycols['totalpris']),"din strømpris"),"."),
                                      p(actionLink("link_to_strompris_history", "Klikk her")," for å se ",
                                        tags$span(style=paste0("color:",mycols['totalpris']),"din strømpris"),
                                        " tilbake i tid.")
                                      #h4("Merk"),
                                      #                                      p("Faste og effektbasert månedsavgift fra nettleverandør kommer i tillegg på regningen fra nettleverandør."),
                                      #                                      p("Faste (typisk 0-50 kr/mnd) og forbruksbaserte (typisk 0-5 øre/kWh) kommer i tillegg på regningen fra din strømleverandør.")
                                  ),
                                  box(width = 5,
                                      #title = "Din strømpris består av",
                                      h3("Din strømpris består av"),
                                      uiOutput("strompris_for"),
                                      uiOutput("stromstotte"),
                                      uiOutput("nettleie"),
                                      uiOutput("spotpris"),
                                      uiOutput("dinstrompris"),
                                      uiOutput("kontrollert_nettleie")
                                  ),
#                                ),
                                  box(width = 7,
                                    h3("Øvrige tillegg på din strømregning"),
                                    uiOutput("nettleie_kapasitetsledd"),
                                    tableOutput("nettleie_kapasitetsledd_tabell"),
                                    p(strong("Kostnader til strømselskap")),
                                    p("Både faste (typisk 0-50 kr/mnd) og forbruksbaserte (typisk 0-5 øre/kWh) kostnader fra din din strømleverandør kommer også i tillegg.",
                                      br(),
                                      em("(I fremtiden vil det være mulig å inkludere disse kostnadene i grafen ovenfor.)")
                                      )
                                )
                                )
)
)

body_strompris_naa_detaljert <- tabItem(tabName = "strompris_naa_detaljert",
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
                                plotlyOutput("now_spotplot_detaljert",height ="300px"),
                                fluidRow(
                                  box(width = 7,
                                      h3("Hva viser grafen?"),
                                      #                                    p("Dagens strømprissytem med store svininger variabel og effektbasert nettleie")
                                      #                                      p("Statens strømstøtteordning har direkte påvirkning på din timespris på strøm."),
                                      p("Ved å taste inn ditt postnummer i margen til venstre viser grafen ovenfor ",
                                        tags$span(style=paste0("color:",mycols['totalpris']),"din strømpris"),
                                        " idag/imorgen for nettopp deg, sammen med ",
                                        tags$span(style=paste0("color:",mycols['spotpris']),"spotpris "),
                                        tags$span(style=paste0("color:",mycols['nettleie']),"nettleie")," og ",
                                        tags$span(style=paste0("color:",mycols['stotte']),"strømstøtte"),"(klikk i margen til høyre for figuren)."
                                        ),# angitt som:"),
                                      p(tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                        "hensyntar både nettleie og strømstøtte, og er definert som:"),
                                      strong(
                                        tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                        "=",
                                        tags$span(style=paste0("color:",mycols['spotpris']),"spotpris"),
                                        "+",
                                        tags$span(style=paste0("color:",mycols['nettleie']),"nettleie"),
                                        "-",
                                        tags$span(style=paste0("color:",mycols['stotte']),"strømstøtte\n")
                                      ),
                                      p(""),
                                      p("Etter at regjeringen endret strømstøtteordningen til timesbasert avregning fra 1.september 2023, er den faktiske strømstøtten kjent umiddelbart, og vises som et enkelt tall ovenfor. ",
                                        "Det månedsbaserte avregningssystemet som gjaldt før gjorde at strømstøtten ikke var kjent før månedsslutt. ",
                                        "Strømstøtten var da beregnet basert på simuleringer av fremtidige spotpriser fra en statistisk modell,",
                                        #                                        tags$a(href="https://martinjullum.com/sideprojects/stromstotte/","(fra en statistisk modell)"),
                                        actionLink("link_to_estimering_av_stromstotte2", "klikk her for detaljer"),
                                        ". Denne modellering er altså ikke nødvendig etter 1.september 2023."),
                                      p(actionLink("link_to_strompris_naa", "Klikk her")," for å se en mer enklere fremstilling av ",
                                        tags$span(style=paste0("color:",mycols['totalpris']),"din strømpris"),"."),
                                      p(actionLink("link_to_strompris_history2", "Klikk her")," for å se ",
                                        tags$span(style=paste0("color:",mycols['totalpris']),"din strømpris"),
                                        " tilbake i tid.")
                                      #h4("Merk"),
                                      #                                      p("Faste og effektbasert månedsavgift fra nettleverandør kommer i tillegg på regningen fra nettleverandør."),
                                      #                                      p("Faste (typisk 0-50 kr/mnd) og forbruksbaserte (typisk 0-5 øre/kWh) kommer i tillegg på regningen fra din strømleverandør.")
                                  ),
                                  box(width = 5,
                                      #title = "Din strømpris består av",
                                      h3("Din strømpris består av"),
                                      uiOutput("strompris_for2"),
                                      uiOutput("stromstotte2"),
                                      uiOutput("nettleie2"),
                                      uiOutput("spotpris2"),
                                      uiOutput("dinstrompris2"),
                                      uiOutput("kontrollert_nettleie2")
                                  ),
                                  #                                ),
                                  box(width = 7,
                                      h3("Øvrige tillegg på din strømregning"),
                                      uiOutput("nettleie_kapasitetsledd2"),
                                      tableOutput("nettleie_kapasitetsledd_tabell2"),
                                      p(strong("Kostnader til strømselskap")),
                                      p("Både faste (typisk 0-50 kr/mnd) og forbruksbaserte (typisk 0-5 øre/kWh) kostnader fra din din strømleverandør kommer også i tillegg.",
                                        br(),
                                        em("(I fremtiden vil det være mulig å inkludere disse kostnadene i grafen ovenfor.)")
                                      )
                                  )
                                )
                              )
)

body_strompris_history <- tabItem(tabName = "strompris_history",
                                  fluidPage(
                                    fluidRow(
                                      box(width = 12,
                                          h3("Detaljert/historisk strømpris")
                                      )
                                    ),
                                    #                                    box(
                                    plotlyOutput("history_spotplot"),
                                    #                                    )
                                    fluidRow(
                                      box(width = 7,
                                          h3("Hva viser grafen?"),
                                          #                                    p("Dagens strømprissytem med store svininger variabel og effektbasert nettleie")
                                          #                                      p("Statens strømstøtteordning har direkte påvirkning på din timespris på strøm."),
                                          p("Ved å taste inn ditt postnummer i margen til venstre viser grafen ovenfor ",
                                            tags$span(style=paste0("color:",mycols['totalpris']),"din strømpris"),
                                            " fra i morgen og tilbake til 1.oktober 2022, sammen med ",
                                            tags$span(style=paste0("color:",mycols['spotpris']),"spotpris "),
                                            tags$span(style=paste0("color:",mycols['nettleie']),"nettleie")," og ",
                                            tags$span(style=paste0("color:",mycols['stotte']),"strømstøtte"),"(klikk i margen til høyre for figuren)."
                                          ),# angitt som:"),
                                          p(tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                            "hensyntar både nettleie og strømstøtte, og er definert som:"),
                                          strong(
                                            tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
                                            "=",
                                            tags$span(style=paste0("color:",mycols['spotpris']),"spotpris"),
                                            "+",
                                            tags$span(style=paste0("color:",mycols['nettleie']),"nettleie"),
                                            "-",
                                            tags$span(style=paste0("color:",mycols['stotte']),"strømstøtte\n")
                                          ),
                                          p(""),
                                          p("Mellom 1.desember 2021 og 30.august 2023 var strømstøtten basert gjennomsnittlig spotpris for inneværeende måned. ",
                                            "Fra 1.september 2023 blir strømstøtten beregnet basert på spotprisen inneværende time."),
#                                          p("Mellom 1.desember 2021 og 30.august 2023 var strømstøtten basert gjennomsnittlig spotpris for inneværeende måned",
#                                            " og var dermed ikke kjent før månedsslutt. Strømstøtten var derfor basert på simuleringer av fremtidige spotpriser fra en statistisk modell,",
#                                          actionLink("link_to_estimering_av_stromstotte3", "klikk her for detaljer"),
#                                          ". Fra 1.september 2023 blir strømstøtten beregnet basert på spotprisen inneværende time, og er ikke lenger ukjent når strømmen forbrukes.",
#                                            tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris"),
#                                            " kan dermed umiddelbart beregnes presist, og det er ikke lenger behov for modellering av fremtidige spotpriser (med usikkerhet)."),
                                          p("Bruk knappene over plottet, slideren under plotter, eller dra med musepekere for å endre lengde på historikk og zoom.")                                          #h4("Merk"),
                                          #                                      p("Faste og effektbasert månedsavgift fra nettleverandør kommer i tillegg på regningen fra nettleverandør."),
                                          #                                      p("Faste (typisk 0-50 kr/mnd) og forbruksbaserte (typisk 0-5 øre/kWh) kommer i tillegg på regningen fra din strømleverandør.")
                                      ),
                                      box(width = 5,
                                          #title = "Din strømpris består av",
                                          h3("Din strømpris består av"),
                                          uiOutput("strompris_for3"),
                                          h4(tags$span(style=paste0("color:",mycols['stotte']),"Strømstøtte (per kWh)")),
                                          uiOutput("nettleie3"),
                                          uiOutput("kontrollert_nettleie3")
                                      )
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
                          tags$a(href="mailto:jullum@nr.no?subject=dinstrompris.no","epost: jullum@nr.no"),
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
                          tags$a(href="https://biapi.nve.no/nettleietariffer/swagger/index.html", "API"),
                          "Data er justert for redusert elavgift 01.01.23, og data for de største nettleverandørene er samtidig justert/kontrollert."),

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
                          tags$a(href="https://github.com/martinju/stromstotte/","GitHub repo"),"."),
                        p("Alternativt, send meg en epost: ",
                          tags$a(href="mailto:jullum@nr.no?subject=dinstrompris.no","jullum@nr.no"))
                      ),
                      box(width=6,
                        h2("Disclaimer"),
                        p("Dette er et hobbyprosjekt utført separat fra mitt virke som seniorforsker i Norsk Regnesentral.",
                        "Det tas intet ansvar for feil i informasjonen vist frem på denne siden, eller for konsekvenser av bruk av denne informasjonen.")
                      ),
                      box(width=6,
                      htmltools::includeMarkdown("changelog.md")
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
           body_strompris_naa_detaljert,
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

  observeEvent(input$link_to_about, {
    updateTabItems(session, "tabs", "about")
  })

  observeEvent(input$link_to_estimering_av_stromstotte, {
    updateTabItems(session, "tabs", "stromstotte")
  })

  observeEvent(input$link_to_estimering_av_stromstotte2, {
    updateTabItems(session, "tabs", "stromstotte")
  })

  observeEvent(input$link_to_estimering_av_stromstotte3, {
    updateTabItems(session, "tabs", "stromstotte")
  })

  observeEvent(input$link_to_strompris_naa_detaljert, {
    updateTabItems(session, "tabs", "strompris_naa_detaljert")
  })

  observeEvent(input$link_to_strompris_naa, {
    updateTabItems(session, "tabs", "strompris_naa")
  })

  observeEvent(input$link_to_strompris_history, {
    updateTabItems(session, "tabs", "strompris_history")
  })

  observeEvent(input$link_to_strompris_history2, {
    updateTabItems(session, "tabs", "strompris_history")
  })


  # Server-side updating as you type with postnr
  updateSelectizeInput(session, 'postnr', choices = unique_postnr, server = TRUE)

  # Update the nettselskap input
  updated_nettselskap <- reactive({
    input_mapper[postnr==input$postnr,unique(nettselskap)]
  })

  observe({
    these_nettselskap <- updated_nettselskap()
    if(length(these_nettselskap)>1){
      no_nettselskap <- "multiple_nettselskap"
    } else {
      no_nettselskap <- "single_nettselskap"
    }

    updateTabsetPanel(inputId = "select_nettselskap", selected = no_nettselskap)
    })

  observe({
    these_nettselskap <- updated_nettselskap()
    updateSelectInput(session, "nettselskap",choices = these_nettselskap)
  })


  # Update the prisomraade input
  updated_prisomraade <- reactive({
    input_mapper[postnr==input$postnr & nettselskap ==input$nettselskap,unique(prisomraade)]
  })

  observe({
    these_prisomraade <- updated_prisomraade()
    if(length(these_prisomraade)>1){
      no_prisomraade <- "multiple_prisomraade"
    } else {
      no_prisomraade <- "single_prisomraade"
    }

    updateTabsetPanel(inputId = "select_prisomraade", selected = no_prisomraade)
  })


  observe({
    these_prisomraade <- updated_prisomraade()

    updateSelectInput(session, "prisomraade",choices = these_prisomraade)
    })

   ## Filter dt_nettleie based on input
   #updated_dt_nettleie <- reactive({
  #   input_mapper[postnr==input$postnr & nettselskap ==input$nettselskap & prisomraade ==input$prisomraade,]
  # })

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

   # Filter dt_hourly based on input
   updated_dt_filtered_prices <- reactive({
     dt_filtered_prices[area ==input$prisomraade & date%in%seq(today-1, today+1,by="day"),]
   })


   # Filter dt_comp based on input
   updated_dt_comp <- reactive({
     dt_comp[area == input$prisomraade & estimation_date==input$date_estimation,]
   })

   # Filter dt_comp based on input
   updated_dt_nettleie <- reactive({
     dt_nettleie[Nettselskap == input$nettselskap]
   })


   updated_dt_nettleie_kapasitetsledd <- reactive({
     dt_nettleie_kapasitetsledd[Nettselskap == input$nettselskap]
   })


   # TESTING
   output$data_spot <- renderTable(updated_dt_hourly())
   output$data_comp <- renderTable(updated_dt_comp())
   output$data_nettleie <- renderTable(updated_dt_nettleie())
   output$datarange_strompris_naa <- renderPrint(input$daterange_strompris_naa)

   output$strompris_for <- output$strompris_for2 <- output$strompris_for3 <- renderUI({
     div(
       em("(Alle priser inkl. mva for ",
       "postnr: ",strong(paste0(input$postnr)),", ",
         "nettselskap: ",strong(paste0(input$nettselskap)),", ",
         "prisområde: ",strong(paste0(input$prisomraade)),")")
#       p("Postnummer: ",strong(paste0(input$postnr))),
#       p("Nettselskap: ",strong(paste0(input$nettselskap))),
#       p("Prisområde: ",strong(paste0(input$prisomraade)))
     )
   })


   output$nettleie <- output$nettleie2 <- output$nettleie3 <- renderUI({
     dagpris <- updated_dt_nettleie()[pristype=="Dag",Energiledd]
     nattpris <- updated_dt_nettleie()[pristype=="Natt",Energiledd]
     helgepris <- updated_dt_nettleie()[pristype=="Helg",Energiledd]

     div(
       h4(tags$span(style=paste0("color:",mycols['nettleie']),"Nettleie (per kWh)")),
       p(
         paste0(ifelse(length(helgepris)>0,"Hverdag: ",""),"Dag (06-22): ",twodigits(dagpris)," kr/kWh"),
         br(),
       paste0(ifelse(length(helgepris)>0,"Hverdag: ",""),"Natt (22-06): ",twodigits(nattpris)," kr/kWh"),
         br(),
         ifelse(length(helgepris)>0,
                paste0("Helg (hele døgnet): ",twodigits(helgepris)," kr/kWh"),
                "")
       )
     )
   })

   output$stromstotte <- output$stromstotte2 <- output$stromstotte3 <- renderUI({
     mean_comp <- updated_dt_filtered_prices()[,list(meancomp=mean(compensation)),by=date]

     meancomp_yesterday <- mean_comp[date==today-1,meancomp]
     meancomp_today <- mean_comp[date==today,meancomp]
     meancomp_tomorrow <- mean_comp[date==today+1,meancomp]

     this_hour <- hour(Sys.time())
     this_date <- as.IDate(Sys.time())

     comp_now <- updated_dt_filtered_prices()[date==this_date & start_hour==this_hour,compensation]


     div(
       h4(tags$span(style=paste0("color:",mycols['stotte']),"Strømstøtte (per kWh)")),

       p("Nå: (Kl.",paste0(this_hour,"-",this_hour+1),") ",twodigits(comp_now)," kr/kWh"),
       p(
         ifelse(
           length(meancomp_yesterday)==1,
           paste0("I går: ",twodigits(meancomp_yesterday)," kr/kWh"),
           ""
         ),
         br(),
         ifelse(
           length(meancomp_today)==1,
           paste0("I dag: ",twodigits(meancomp_today)," kr/kWh"),
           ""
         ),
         br(),
         ifelse(
           length(meancomp_tomorrow)==1,
           paste0("I morgen: ",twodigits(meancomp_tomorrow)," kr/kWh\n"),
           ""
         )
       )
     )
   })

   output$spotpris <- output$spotpris2 <- renderUI({
     this_hour <- hour(Sys.time())
     this_date <- as.IDate(Sys.time())

     spotpris <- updated_dt_hourly()[date==this_date & start_hour==this_hour,price]
#     spotpris <- updated_dt_hourly0[date==this_date & start_hour==this_hour,price]

     div(
       h4(tags$span(style=paste0("color:",mycols['spotpris']),"Spotpris (per kWh)")),
       p("Kl.",paste0(this_hour,"-",this_hour+1),twodigits(spotpris)," kr/kWh")
     )
   })




   output$nettleie_kapasitetsledd <- output$nettleie_kapasitetsledd2 <- renderUI({
     dt <- updated_dt_nettleie_kapasitetsledd()
     #dt <- dt_nettleie_kapasitetsledd[Nettselskap=="ELVIA AS"]

     grunnlag <- dt[,unique(`grunnlag_simple`)]

     div(
       p(strong("Nettleie (kapasitetsbasert)"),
         br(),
         "Månedsbeløp som er avhengig av hvor mye strøm du bruker samtidig.",
         br(),
         paste0("Maksforbruk = ",grunnlag,"."))
       # Sett inn tableoutput her (fra annen fil)
       # Til slutt angis grunnlag
       # Nevn også at tillegg fra strømleverandør kommer i tillegg.
     )
   })

   output$nettleie_kapasitetsledd_tabell <- output$nettleie_kapasitetsledd_tabell2 <- renderTable({
     dt <- updated_dt_nettleie_kapasitetsledd()
     #dt <- dt_nettleie_kapasitetsledd[Nettselskap=="ELVIA AS"]

     dt[,.(Maksforbruk=Forbruk,Kostnad)]
   })

   output$kontrollert_nettleie <- output$kontrollert_nettleie2 <- output$kontrollert_nettleie3 <- renderUI({
     pris_kontrollert <- updated_dt_nettleie()[,unique(kontrollert_pris)]

     div(
       tags$span(style=paste0("color:red"),ifelse(pris_kontrollert,
                "",
                "Merk: Det er oppdaget enkelte feil i nettleiedataene som hentes fra NVE.
                Data fra din nettleverandør er ikke manuelt kontrollert, og kan derfor inneholde feil (typisk < 10øre/kWh)."
                )
       )
     )
   })



   output$dinstrompris <- output$dinstrompris2 <- renderUI({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     this_hour <- hour(Sys.time())


     dt_list <- plot_dt_final()
     dt <- dt_list$plot_dt_final

     now_hms =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60

     res_vec <- unlist(dt[datetime==now_hms & type=="totalpris",.(pris)])

     div(
       h4(tags$span(style=paste0("color:",mycols['totalpris']),"Din strømpris nå (per kWh)")),
       p(paste0("Kl. ",this_hour,"-",this_hour+1),": ",twodigits(res_vec[1])," kr/kWh")
     )
   })


   plot_dt_final_old <- reactive({
     estimation_date0= dt_comp[,max(estimation_date)]

     updated_dt_nettleie0 <- dt_nettleie[Nettselskap == input$nettselskap]
     updated_dt_hourly0 <- dt_hourly[area ==input$prisomraade]
     updated_dt_comp0 <- dt_comp[area == input$prisomraade]

     #     updated_dt_nettleie0 <- dt_nettleie[Nettselskap=="ELVIA AS"]
     #     updated_dt_hourly0 <- dt_hourly[area=="NO1"]
     #     updated_dt_comp0 <- dt_comp[area == "NO1"]

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
     texthelper2_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc2(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols))]
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
            texthelper2_dt = texthelper2_dt,
            texthelper_simple_dt=texthelper_simple_dt,
            texthelper_simple2_dt=texthelper_simple2_dt,
            estimation_date0 = estimation_date0)

   })

   plot_dt_final <- reactive({
     estimation_date0= dt_filtered_prices[,max(date)]

     updated_dt_nettleie0 <- dt_nettleie[Nettselskap == input$nettselskap]
     updated_dt_filtered_prices0 <- dt_filtered_prices[area ==input$prisomraade]
     updated_dt_filtered_prices_org_comp_system0 <- dt_filtered_prices_org_comp_system[area ==input$prisomraade]

#     updated_dt_nettleie0 <- dt_nettleie[Nettselskap=="ELVIA AS"]
#     updated_dt_hourly0 <- dt_hourly[area=="NO1"]
#     updated_dt_comp0 <- dt_comp[area == "NO1"]

     plot_strompris_naa_dt <- rbind(updated_dt_filtered_prices_org_comp_system0,updated_dt_filtered_prices0)

     plot_strompris_naa_dt[,filtered_price:=NULL]
     setnames(plot_strompris_naa_dt,"price","spotpris")
     setnames(plot_strompris_naa_dt,"compensation","stotte")

     plot_strompris_naa_dt[start_hour %in% seq(6,21),nettleie:=updated_dt_nettleie0[pristype=="Dag",Energiledd]]
     plot_strompris_naa_dt[is.na(nettleie),nettleie:=updated_dt_nettleie0[pristype=="Natt",Energiledd]]

     plot_strompris_naa_dt[,totalpris:=spotpris+nettleie-stotte]
     plot_strompris_naa_dt[,datetime:=as.POSIXct(date)+start_hour*60*60]


     plot_strompris_naa_dt0 <- plot_strompris_naa_dt[,.(datetime,spotpris,nettleie,totalpris,stotte)]

#     texthelper_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols))]
     texthelper2_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc2_new(datetime,spotpris,nettleie,totalpris,stotte,mycols))]
#     texthelper_simple_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc_simple(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols))]
     texthelper_simple2_dt <- plot_strompris_naa_dt0[,.(datetime,text=textfunc_simple3_new(datetime,spotpris,nettleie,totalpris,mycols,fontsize=9))]

     plot_strompris_naa_dt0_dup <- copy(plot_strompris_naa_dt0)
     plot_strompris_naa_dt0_dup[,datetime:=datetime+1*60*60-1]

     plot_strompris_naa_dt00 <- rbind(plot_strompris_naa_dt0,plot_strompris_naa_dt0_dup)
     setkey(plot_strompris_naa_dt00,datetime)


#     plot_strompris_naa_dt_ints_totalpris <- plot_strompris_naa_dt00[,.(datetime,lower_CI=totalpris_lower_CI,upper_CI=totalpris_upper_CI)]
#     plot_strompris_naa_dt_ints_totalpris[,type:="totalpris"]

#     plot_strompris_naa_dt_ints_stotte <- plot_strompris_naa_dt00[,.(datetime,lower_CI=stotte_lower_CI,upper_CI=stotte_upper_CI)]
#     plot_strompris_naa_dt_ints_stotte[,type:="stotte"]

#     plot_strompris_naa_dt_ints <- rbind(plot_strompris_naa_dt_ints_totalpris,plot_strompris_naa_dt_ints_stotte)

     plot_strompris_naa_dt_melted <- melt(plot_strompris_naa_dt00[,.(datetime,spotpris,nettleie,totalpris,stotte)],
                                          id.vars = c("datetime"),variable.name = "type",value.name = "pris")

     plot_dt_final <- plot_strompris_naa_dt_melted

     plot_dt_final[,linesize := "b"]
     plot_dt_final[type%in% c("totalpris","stotte"),linesize := "a"]

     setcolorder(plot_dt_final,c("datetime","type","pris","linesize"))
     plot_dt_final[,type:=factor(type,levels=c("spotpris","nettleie","stotte","totalpris"))]


     setkey(plot_dt_final,datetime)

     dt_list <-
     list(plot_dt_final=plot_dt_final,
          #texthelper_dt=texthelper_dt,
          texthelper2_dt = texthelper2_dt,
          #texthelper_simple_dt=texthelper_simple_dt,
          texthelper_simple2_dt=texthelper_simple2_dt,
          estimation_date0 = estimation_date0)

   })



   output$now_spotplot3 <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     dt_list <- plot_dt_final()

     max_dinstrompris <- dt_list$plot_dt_final[datetime>=dt_list$estimation_date0,max(pris,na.rm = T)]
     now_hms =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())

     dat <- dt_list$plot_dt_final[datetime>=(now_hms-3*60*60) & type=="totalpris"]
     dat <- dat[-1]

     dat_all <- dt_list$plot_dt_final[datetime>=(now_hms-3*60*60)]
     dat_all <- dat_all[-(1:4)]
     #helper0 <- dt_list$texthelper_simple_dt[datetime>=(now_hms-3*60*60)]
     #setnames(helper0,"text","text0")
     #dat <-merge(dat,helper0,by="datetime",all=T)
     #dat[, text := text0[nafill(replace(.I, is.na(text0), NA), "locf")]]
     #dat[,text0:=NULL]

     plotrange <- c(min(c(0,dat$pris),na.rm = T),max(c(0,dat$pris),na.rm=T))
     plotrange2 <- plotrange
     plotrange2[2] <- plotrange2[2] + diff(plotrange)*0.05
     plotrange3 <- plotrange2
     plotrange3[1] <- plotrange2[1] - diff(plotrange)*0.01
     plotrange4 <- plotrange2
     plotrange4[1] <- plotrange2[1] - diff(plotrange)*0.005

     helper0 <- dt_list$texthelper_simple2_dt[datetime>=(now_hms-3*60*60)]
     helper0 <- merge(helper0,dat[,.(datetime)],by="datetime")
#     helper0[,plotval:=upper_CI+diff(plotrange)*0.1]
     helper0[,plotval:=max(plotrange)+diff(plotrange)*0.05]

     helper0[,datetime2:=datetime+0.5*60*60]

     x_range <- as.numeric(dat[,diff(range(datetime))])

     dt <- data.table(x=rep(Sys.time()+60*60,2),y=plotrange2)

     p_now <- ggplot(data=dat,mapping=aes(x=datetime,y=pris))+
       geom_line(col=mycols["totalpris"],linewidth=1)+
#       geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),fill=mycols["totalpris"], alpha = 0.3)+
       ggtitle("")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(8))+
       scale_x_datetime(name = "Tid/dato",
                        date_breaks=ifelse(x_range>24,"3 hours","2 hours"),
                        minor_breaks = "1 hours",
                        labels = label_date_short(format = c("", "", "%d.%b\n", "%H\n"),sep=""))+
       geom_line(dt,mapping = aes(x=x,y=y),linetype=3,col="grey",linewidth=0.75)+
       #annotate("text", x = dt[1,x]+20*60, y =  plotrange2[1], label = "NÅ")+
#       geom_vline(xintercept=Sys.time(),linetype=2,col="grey",inherit.aes=F)+
       geom_line(data=helper0,aes(x=datetime2,y=plotval,text=text),inherit.aes = F,linewidth=0.00001)+
      theme_minimal()


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
       title = list(text = paste0("Din strømpris <span style='color:grey; font-size:small'>(Postnr: ",input$postnr,")</span>")),
       margin=list(t=35),
       annotations=list(x=as.numeric(dt[1,x]),
                        y=plotrange3[1],
                        text="NÅ",
                        showarrow=FALSE,
                        font=list(size=11)),
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
       hoverlabel = list(font=list(size=9))#,
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
   #   req(input$postnr,input$nettselskap, input$prisomraade)
   #   if (identical(input$prisomraade, "")) return(NULL)
   #   if (identical(input$nettselskap, "")) return(NULL)
   #
   #   dt_list <- plot_dt_final()
   #
   #   max_dinstrompris <- dt_list$plot_dt_final[datetime>=dt_list$estimation_date0,max(upper_CI,na.rm = T)]
   #   now_hms =as.POSIXct(as.IDate(Sys.Date()))#+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())
   #
   #   dat <- dt_list$plot_dt_final[datetime>=(now_hms-24*60*60) & type=="totalpris"]
   #   dat <- dat[-1]
   #   #helper0 <- dt_list$texthelper_simple_dt[datetime>=(now_hms-3*60*60)]
   #   #setnames(helper0,"text","text0")
   #   #dat <-merge(dat,helper0,by="datetime",all=T)
   #   #dat[, text := text0[nafill(replace(.I, is.na(text0), NA), "locf")]]
   #   #dat[,text0:=NULL]
   #
   #   plotrange <- c(min(dat$lower_CI),max(dat$upper_CI))
   #   plotrange2 <- plotrange
   #   plotrange2[2] <- plotrange2[2] + diff(plotrange)*0.05
   #   plotrange3 <- plotrange2
   #   plotrange3[1] <- plotrange2[1] - diff(plotrange)*0.01
   #
   #   helper0 <- dt_list$texthelper_simple2_dt[datetime>=(now_hms-24*60*60)]
   #   helper0 <- merge(helper0,dat[,.(datetime,upper_CI)],by="datetime")
   #   #     helper0[,plotval:=upper_CI+diff(plotrange)*0.1]
   #   helper0[,plotval:=max(upper_CI)+diff(plotrange)*0.05]
   #
   #   helper0[,datetime2:=datetime+0.5*60*60]
   #
   #   dt <- data.table(x=rep(Sys.time(),2),y=plotrange3)
   #
   #   p_now <- ggplot(data=dat,mapping=aes(x=datetime,y=pris))+
   #     geom_line(col=mycols["totalpris"],size=1)+
   #     geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),fill=mycols["totalpris"], alpha = 0.3)+
   #     ggtitle("Din strømpris")+
   #     scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(15))+
   #     scale_x_datetime(name = "Tid/dato",
   #                      date_breaks="2 hours",
   #                      #minor_breaks = breaks_pretty(24),
   #                      labels = label_date_short(format = c("", "", "%d.%b\n", "%H\n"),sep=""))+
   #     geom_line(dt,mapping = aes(x=x,y=y),linetype=2,col="grey",size=0.75)+
   #     #annotate("text", x = dt[1,x]+20*60, y =  plotrange2[1], label = "NÅ")+
   #     #       geom_vline(xintercept=Sys.time(),linetype=2,col="grey",inherit.aes=F)+
   #     geom_line(data=helper0,aes(x=datetime2,y=plotval,text=text),inherit.aes = F,size=0.00001)+
   #     theme_bw()
   #
   #
   #   #p_now
   #
   #   #+
   #   #  #                 +
   #   #  scale_size_manual(values=c("a" = 1,"b"=0.5))+
   #   #   guides(size="none")+
   #   #   scale_color_manual(name="",values = mycols,labels = mylabels)+
   #   #   scale_fill_manual(name="",values = mycols,labels = mylabels)+
   #
   #   ggp_now <- ggplotly(p_now,tooltip = "text")
   #   ggp_now <- layout(
   #     ggp_now,
   #     annotations=list(x=as.numeric(dt[1,x]),
   #                      y=plotrange4[1],
   #                      text="NÅ",
   #                      showarrow=FALSE,
   #                      font=list(size=11)),
   #     hovermode = "x unified",
   #     xaxis = list(constrain="domain",
   #                  fixedrange = TRUE,
   #                  nticks=24,
   #                  range=dat[,as.numeric(range(datetime))]
   #     ),#,tickformat="%b<br>%Y"),
   #     yaxis = list(constrain="domain",
   #                  fixedrange = TRUE,
   #                  tickformat = ".2f",
   #                  nticks=20,
   #                  zeroline = TRUE,
   #                  range=plotrange3
   #     ),
   #     legend = list(font = list(size=10)),
   #     hoverlabel = list(font=list(color = mycols["totalpris"],size=9))#,
   #     #       legend = list(orientation = 'h')
   #   )
   #   #     ggp_now <- style(ggp_now,visible="legendonly",traces=c(1,2,3,7)) #trace=2 identified through plotly_json(ggp_now)
   #   ggp_now <- style(ggp_now,hoverinfo="none",traces=1:(length(ggp_now$x$data)-1))
   #   #    ggp_now <- style(ggp_now,name="strømstøtte",traces=3)
   #   #     ggp_now <- style(ggp_now,name="Din strømpris",traces=4)
   #
   #   ggp_now <- config(ggp_now,locale="no")
   #
   #   ggp_now
   #
    })

   output$now_spotplot_detaljert <- renderPlotly({
     req(input$postnr,input$nettselskap, input$prisomraade)
     if (identical(input$prisomraade, "")) return(NULL)
     if (identical(input$nettselskap, "")) return(NULL)

     dt_list <- plot_dt_final()

     max_dinstrompris <- dt_list$plot_dt_final[datetime>=dt_list$estimation_date0,max(pris,na.rm = T)]
     now_hms =as.POSIXct(as.IDate(Sys.Date()))+hour(Sys.time())*60*60+minute(Sys.time())*60+second(Sys.time())

     dat <- dt_list$plot_dt_final[datetime>=(now_hms-3*60*60)]# & type=="totalpris"]
     dat <- dat[datetime!=min(datetime)]
     #helper0 <- dt_list$texthelper_simple_dt[datetime>=(now_hms-3*60*60)]
     #setnames(helper0,"text","text0")
     #dat <-merge(dat,helper0,by="datetime",all=T)
     #dat[, text := text0[nafill(replace(.I, is.na(text0), NA), "locf")]]
     #dat[,text0:=NULL]

     plotrange <- c(min(c(0,dat$pris),na.rm = T),max(c(0,dat$pris),na.rm=T))
     plotrange2 <- plotrange
     plotrange2[2] <- plotrange2[2] + diff(plotrange)*0.08
     plotrange3 <- plotrange2
     plotrange3[1] <- plotrange2[1] - diff(plotrange)*0.01


     helper0 <- dt_list$texthelper2_dt[datetime>=(now_hms-3*60*60)]
     helper0[,plotval:=plotrange2[2]]

     helper0[,datetime2:=datetime+0.5*60*60]

     x_range <- as.numeric(dat[,diff(range(datetime))])

     dt <- data.table(x=rep(Sys.time()+60*60,2),y=plotrange2)

     p_now <- ggplot(data=dat,mapping=aes(x=datetime,y=pris,col=type,fill=type))+
       geom_line(aes(size=linesize))+
       #geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.3)+
       ggtitle("")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(8))+
       scale_x_datetime(name = "Tid/dato",
                        date_breaks=ifelse(x_range>24,"3 hours","2 hours"),
                        minor_breaks = "1 hours",
                        labels = label_date_short(format = c("", "", "%d.%b\n", "%H\n"),sep=""))+
       scale_size_manual(values=c("a" = 1,"b"=0.5))+
       guides(size="none")+
       scale_color_manual(name="",values = mycols,labels = mylabels)+
       scale_fill_manual(name="",values = mycols,labels = mylabels)+
       geom_line(dt,mapping = aes(x=x,y=y),linetype=2,col="grey",size=0.75,inherit.aes = F)+
       geom_line(data=helper0,aes(x=datetime2,y=plotval,text=text),inherit.aes = F,size=0.00001)+
       theme_minimal()


     p_now


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
       title = list(text = paste0("Din strømpris <span style='color:grey; font-size:small'>(Postnr: ",input$postnr,")</span>")),
       margin=list(t=35),
       annotations=list(x=as.numeric(dt[1,x]),
                        y=plotrange3[1],
                        text="NÅ",
                        showarrow=FALSE,
                        font=list(size=11)),
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
       #hoverlabel = list(font=list(color = mycols["totalpris"],size=9))#,
       hoverlabel = list(font=list(size=9))#,

       #       legend = list(orientation = 'h')
     )
#     ggp_now <- style(ggp_now,visible="legendonly",traces=c(1,2,3,7)) #trace=2 identified through plotly_json(ggp_now)
     ggp_now <- style(ggp_now,visible="legendonly",traces=c(3)) #trace=2 identified through plotly_json(ggp_now)
     ggp_now <- style(ggp_now,hoverinfo="none",traces=1:(length(ggp_now$x$data)-1))
     ggp_now <- style(ggp_now,name="strømstøtte",traces=3)
     ggp_now <- style(ggp_now,name="Din strømpris",traces=4)

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
       #geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.3)+
       ggtitle("")+
       scale_y_continuous(name = "NOK/kWh inkl. mva",labels=scaleFUN,breaks = breaks_extended(8))+
       scale_x_datetime(name = "Tid/dato",
                        breaks=breaks_pretty(15),
#                        minor_breaks = breaks_pretty(24),
                        labels = label_date_short(format = c("%Y", "", "%d.%b\n", "%H\n"),sep=""))+
       scale_size_manual(values=c("a" = 1,"b"=0.5))+
       scale_color_manual(name="",values = mycols,labels = mylabels)+
       scale_fill_manual(name="",values = mycols,labels = mylabels)+
       guides(size="none")+
#       geom_line(data=dt_list$texthelper_dt,aes(x=datetime,y=0,text=text),inherit.aes = F,size=0.00001)+
       theme_minimal()

     ggp_history <- ggplotly(p_history,dynamicTicks = TRUE,tooltip = "text")
     ggp_history <- layout(
       ggp_history,
       title = list(text = paste0("Din strømpris <span style='color:grey; font-size:small'>(Postnr: ",input$postnr,")</span>"),
                    y=0.99),
       margin=list(t=35+25),
#       hovermode = "x unified",
       xaxis = list(
         rangeselector = list(
           buttons = list(
             list(
               count = 1,
               label = "denne måned",
               step = "month",
               stepmode = "backward"),
             list(
               count = lubridate::wday(Sys.Date(),week_start=1),
               label = "denne uka",
               step = "day",
               stepmode = "backward"),
             list(
               count = 1,
               label = "idag",
               step = "day",
               stepmode = "backward"),
             list(
               count = 2,
               label = "2 siste dager",
               step = "day",
               stepmode = "backward")
           )),
         rangeslider = list(type = "date",bgcolor="#eef4f5")
       ),
       yaxis = list(tickformat = ".2f",fixedrange = FALSE)
     )

     ggp_history <- style(ggp_history,visible="legendonly",traces=c(3,7)) #trace=2 identified through plotly_json(ggp_history)
     ggp_history <- style(ggp_history,hoverinfo="none")
     ggp_history <- style(ggp_history,name="strømstøtte",traces=3)
     ggp_history <- style(ggp_history,name="Din strømpris",traces=4)

     ggp_history <- config(ggp_history,locale="no",
                           modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","hoverClosestCartesian","hoverCompareCartesian","autoScale2d"))

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
