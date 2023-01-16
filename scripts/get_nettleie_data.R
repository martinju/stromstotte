
library(data.table)
library(rjson)

Sys.setlocale(locale='en_US.UTF-8') # Also OK for reading Norwegian letters

nettleie <- fread("raw-data/innrapportert_nettleie_251022_utf8.csv",dec = ",")

setnames(nettleie,"Konsesjonær","Nettselskap")

orgnr_dt <- unique(nettleie[,.(Nettselskap,Organisasjonsnummer,FylkeNr)])

dato <- Sys.Date()

dat_dt_list <- list()
nettselskap_not_found <- NULL
for(i in seq_len(nrow(orgnr_dt))){

  nettselskap <- orgnr_dt[i,Nettselskap]

  orgnr <- orgnr_dt[i,Organisasjonsnummer]
  fylkenr <- orgnr_dt[i,FylkeNr]

  url <- paste0("https://biapi.nve.no/nettleietariffer/api/NettleiePerOmradePrTimeHusholdningFritidEffekttariffer?ValgtDato=",
                dato,
                "&Tariffgruppe=Husholdning&FylkeNr=",
                fylkenr,
                "&OrganisasjonsNr=",
                orgnr)

  dat_json <- rjson::fromJSON(file=url)
  if(length(dat_json)>0){
    dat_dt0 <- as.data.table(dat_json)
    dat_dt00 <- dat_dt0[,lapply(.SD,unlist)]

    dat_dt_list[[i]] <- as.data.table(t(dat_dt00))
    names(dat_dt_list[[i]]) <- names(unlist(dat_json[[1]]))
    dat_dt_list[[i]][,Nettselskap_excel:=nettselskap]
  } else {
    nettselskap_not_found <- c(nettselskap_not_found,nettselskap)
  }

  print(i)
}

dat_dt <- rbindlist(dat_dt_list)

fwrite(dat_dt,"data/nettleie_nve_api.csv")


#nettselskap_not_found
#orgnr_dt[Nettselskap=="SKIAKERNETT AS"]

#sort(unique(dat_dt$konsesjonar))

#https://nettalliansen.no/selskapene/skj%C3%A5k-energi-kf
# Ref #https://www.kartverket.no/til-lands/fakta-om-norge/norske-fylke-og-kommunar
#så er det Skjåk kommune i innlandet med fylkesnummer 34, men finner ikke denne

#url <- paste0("https://biapi.nve.no/nettleietariffer/api/NettleiePerOmradePrTimeHusholdningFritidEffekttariffer?ValgtDato=",
#              "2023-01-09",
#              "&Tariffgruppe=Husholdning&FylkeNr=",
#              34,
#              "&OrganisasjonsNr=",
#              920295975)

#Nettselskap Organisasjonsnummer FylkeNr
#1: SKIAKERNETT AS           920295975      34
#2: SKIAKERNETT AS           920295975      46


