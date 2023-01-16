


#ELVIA AS
# ORGNR: "980489698"
# FYLKE 34, 3, 30

# LEDE AS
#orgnr: "979422679"
# Fylke: 38, 30

# AGDER ENERGI NETT AS
#orgnr: "982974011"
# Fylke: 11, 38, 42

dato <- Sys.Date()

nettselskap <- "ELVIA AS"
orgnr <- 980489698
fylkenr <- 30
# (energileddEks +elavgift)*1.25 gir riktig nettleie

nettselskap <- "LEDE AS"
orgnr <- 979422679
fylkenr <- 38
# (energileddEks +elavgift+energifond)*1.25 gir riktig nettleie

nettselskap = "AGDER ENERGI NETT AS"
orgnr = 982974011
fylkenr <- 11



url <- paste0("https://biapi.nve.no/nettleietariffer/api/NettleiePerOmradePrTimeHusholdningFritidEffekttariffer?ValgtDato=",
              dato,
              "&Tariffgruppe=Husholdning&FylkeNr=",
              fylkenr,
              "&OrganisasjonsNr=",
              orgnr)

dat_json <- rjson::fromJSON(file=url)
dat_dt0 <- as.data.table(dat_json)
dat_dt00 <- dat_dt0[,lapply(.SD,unlist)]

dat_dt <- as.data.table(t(dat_dt00))
names(dat_dt) <- names(unlist(dat_json[[1]]))
dat_dt[,Nettselskap:=nettselskap]

dat_dt_effekttariff <- unique(dat_dt[,.(effekttrinnFraKw,effekttrinnTilKw,fastleddEks,fastleddInk)])
dat_dt_effekttariff[,effekttrinnFraKw:=as.numeric(effekttrinnFraKw)]
setkey(dat_dt_effekttariff,effekttrinnFraKw)
dat_dt_effekttariff

dat_dt[,fastleddEks :=NULL]
dat_dt[,fastleddInk  :=NULL]
dat_dt[,effekttrinnFraKw   :=NULL]
dat_dt[,effekttrinnTilKw   :=NULL]


dat_dt <- unique(dat_dt)

setkey(dat_dt,time)

dat_dt


nettleie <- fread("data/nettleie_nve_api.csv")

#ELVIA AS:
unique(nettleie[organisasjonsnr==980489698,.(organisasjonsnr,konsesjonar,fylkeNr,fylke,energileddEks,energileddInk)])
#organisasjonsnr konsesjonar fylkeNr     fylke energileddEks energileddInk
#1:       980489698    ELVIA AS      34 Innlandet         19.07         43.10
#2:       980489698    ELVIA AS      34 Innlandet         14.07         36.85
#3:       980489698    ELVIA AS      30     Viken         14.07         36.85
#4:       980489698    ELVIA AS      30     Viken         19.07         43.10

#LEDE
unique(nettleie[organisasjonsnr==979422679,.(organisasjonsnr,konsesjonar,fylkeNr,fylke,energileddEks,energileddInk)])
#organisasjonsnr konsesjonar fylkeNr                fylke energileddEks energileddInk
#1:       979422679     LEDE AS      38 Vestfold og Telemark            17         40.51
#2:       979422679     LEDE AS      30                Viken            17         40.51

### https://lede.no/priser/
### opplyser at deres nettleie er


# Glitre nett
unique(nettleie[organisasjonsnr==982974011,.(organisasjonsnr,konsesjonar,fylkeNr,fylke,energileddEks,energileddInk)])
#organisasjonsnr    konsesjonar fylkeNr                fylke energileddEks energileddInk
#1:       982974011 GLITRE NETT AS      11             Rogaland          26.6         52.51
#2:       982974011 GLITRE NETT AS      11             Rogaland          18.6         42.51
#3:       982974011 GLITRE NETT AS      38 Vestfold og Telemark          26.6         52.51
#4:       982974011 GLITRE NETT AS      38 Vestfold og Telemark          18.6         42.51
#5:       982974011 GLITRE NETT AS      42                Agder          18.6         42.51
#6:       982974011 GLITRE NETT AS      42                Agder          26.6         52.51

###
### https://www.glitreenergi-nett.no/kunde/alt-om-nettleiepriser/gebyrer-og-avgifter/
### Opplyser at deres nye nettavgifter er 38.65 dag, 26.65 natt


