
### Getting nettleie for nettselskap from api
library(data.table)
library(rjson)

orgnr_dt <- fread("data/orgnr_dt.csv")
orgnr_dt[,datoId:=NULL] # Ignore date
orgnr_dt[,konsesjonar:=NULL] # ignore navn, as might be duplicates, and does not extract it from there
orgnr_dt <- unique(orgnr_dt)


today <- Sys.Date()


#### Now, getting nettleiedata for each of these

nettleie_api_dt_list <- list()

for(i in seq_len(nrow(orgnr_dt))){
  fylkenr <- orgnr_dt[i,fylkeNr]
  orgnr <- orgnr_dt[i,organisasjonsnr]

  url <- paste0("https://biapi.nve.no/nettleietariffer/api/NettleiePerOmradePrTimeHusholdningFritidEffekttariffer?",
  "ValgtDato=",today,
  "&Tariffgruppe=Husholdning",
  "&FylkeNr=",fylkenr,
  "&OrganisasjonsNr=",orgnr)

  dat_json <- rjson::fromJSON(file=url)
  if(length(dat_json)>0){
    dat_dt0 <- as.data.table(dat_json)
    nettleie_api_dt_list[[i]] <- as.data.table(t(dat_dt0[,lapply(.SD,unlist)]))
    names(nettleie_api_dt_list[[i]]) <- names(dat_json[[1]])[-3]
  } else {
    print(paste0(paste0(orgnr_dt[i],collapse=", ")," NOT FOUND!"))
  }


  print(i)
}

nettleie_api_dt <- rbindlist(nettleie_api_dt_list)
fwrite(nettleie_api_dt,"raw-data/raw_database_nettleie_nve_api.csv",append=TRUE)

nettleie_api_dt[,time:=as.numeric(time)]
nettleie_api_dt[,energileddInk:=as.numeric(energileddInk)]
nettleie_api_dt[,effekttrinnFraKw:=as.numeric(effekttrinnFraKw)]



setnames(nettleie_api_dt,"konsesjonar","Nettselskap")


setkey(nettleie_api_dt,Nettselskap,effekttrinnFraKw,time)


nettselskap_fylke_dt <- unique(nettleie_api_dt[,.(Nettselskap,fylkeNr)])

nettleie_kapasitetsledd_api_dt_simple_list <- list()


for (i in seq_len(nrow(nettselskap_fylke_dt))){
  tmp <- nettleie_api_dt[time==0][Nettselskap==nettselskap_fylke_dt[i,Nettselskap] & fylkeNr==nettselskap_fylke_dt[i,fylkeNr]]
  nettleie_kapasitetsledd_api_dt_simple_list[[i]] <- unique(tmp[,.(Nettselskap,fylkeNr,fastleddEks,fastleddInk,effekttrinnFraKw,effekttrinnTilKw)])
}
nettleie_kapasitetsledd_api_dt_simple <- data.table(date=as.IDate(today),rbindlist(nettleie_kapasitetsledd_api_dt_simple_list))
setkey(nettleie_kapasitetsledd_api_dt_simple,Nettselskap,fylkeNr,effekttrinnFraKw)

fwrite(nettleie_kapasitetsledd_api_dt_simple,"data/nettleie_kapasitetsledd_api_dt_simple.csv",append=T)



####

#### Now, formatting nettleie dt

nettleie_api_dt[fylke=="Troms og Finnmark Romsa ja Finnmárku",fylke:="Troms og Finnmark"]

# Where does Energiledd jump?

nettleie_api_dt[,Energiledd_diff:=energileddInk-shift(energileddInk,type="lag"),by=.(Nettselskap,fylke)]

nettleie_api_dt_simple_list <- list()

for (i in seq_len(nrow(nettselskap_fylke_dt))){
  tmp <- nettleie_api_dt[effekttrinnFraKw==0][Nettselskap==nettselskap_fylke_dt[i,Nettselskap] & fylkeNr==nettselskap_fylke_dt[i,fylkeNr]]
  changes <- tmp[Energiledd_diff!=0,time]
  if(length(changes)>0){
    tmp_list <- list()
    for(j in seq_len(length(changes)-1)){
      tmp_list[[j]]<- data.table(Nettselskap=nettselskap_fylke_dt[i,Nettselskap],
                                 fylkeNr = nettselskap_fylke_dt[i,fylkeNr],
                                 energileddInk=tmp[time==changes[j],energileddInk],
                                 energileddEks=tmp[time==changes[j],energileddEks],
                                 time_fom=changes[j],
                                 time_tom=changes[j+1]-1)
    }
    tmp_list[[length(changes)]] <- data.table(Nettselskap=nettselskap_fylke_dt[i,Nettselskap],
                                              fylkeNr = nettselskap_fylke_dt[i,fylkeNr],
                                              energileddInk=tmp[time==length(changes),energileddInk],
                                              energileddEks=tmp[time==length(changes),energileddEks],
                                              time_fom=changes[length(changes)],
                                              time_tom=changes[1]-1)


    nettleie_api_dt_simple_list[[i]] <- rbindlist(tmp_list)

  } else {
    nettleie_api_dt_simple_list[[i]] <- data.table(Nettselskap=nettselskap_fylke_dt[i,Nettselskap],
                                                   fylkeNr = nettselskap_fylke_dt[i,fylkeNr],
                                                   energileddInk=tmp[1,energileddInk],
                                                   energileddEks=tmp[1,energileddEks],
                                                   time_fom=0,
                                                   time_tom=23)
  }
}
nettleie_api_dt_simple <- data.table(date=as.IDate(today),rbindlist(nettleie_api_dt_simple_list))

fwrite(nettleie_api_dt_simple,"data/database_nettleie_api_simple.csv",append=T)
#nettleie_api_dt_simple <- fread("data/database_nettleie_api_simple.csv")

#### CONTINUE THE CHECK HERE, TO THEN EMAIL NVE ####

nettleie_dt_simple <- fread("data/database_nettleie_simple.csv",encoding = "Latin-1")


nettleie_dt_simple[Nettselskap=="LEDE AS"]
nettleie_api_dt_simple[Nettselskap=="LEDE AS"] # mangler 1.25 (Avgift Energifondet)

nettleie_dt_simple[Nettselskap=="ELVIA AS"]
nettleie_api_dt_simple[Nettselskap=="ELVIA AS"]  # Mangler 1.35 øre på natta og 1.98 øre på dagen

nettleie_dt_simple[Nettselskap=="TENSIO TS AS"]
nettleie_api_dt_simple[Nettselskap=="TENSIO TS AS"]  # Korrekt

nettleie_dt_simple[Nettselskap=="TENSIO TN AS"]
nettleie_api_dt_simple[Nettselskap=="TENSIO TN AS"]  # Korrekt (fylke 50)

nettleie_dt_simple[Nettselskap=="BKK NETT AS"]
nettleie_api_dt_simple[Nettselskap=="BKK AS"]  # Korrekt

nettleie_dt_simple[Nettselskap=="LNETT AS"]
nettleie_api_dt_simple[Nettselskap=="LNETT AS"]  # Korrekt


nettleie_dt_simple[Nettselskap=="ARVA AS"]
nettleie_api_dt_simple[Nettselskap=="ARVA AS"]  # Korrekt

nettleie_dt_simple[Nettselskap=="NORGESNETT AS"]
nettleie_api_dt_simple[Nettselskap=="NORGESNETT AS"]  # Korrekt

nettleie_dt_simple[Nettselskap=="GLITRE ENERGI NETT AS"]
nettleie_api_dt_simple[Nettselskap=="GLITRE NETT AS"]  # Korrekt

nettleie_dt_simple[Nettselskap=="FAGNE AS"]
nettleie_api_dt_simple[Nettselskap=="FAGNE AS"]  # Korrekt

nettleie_dt_simple[Nettselskap=="JÆREN EVERK AS"]
nettleie_api_dt_simple[Nettselskap=="JÆREN EVERK AS"]  # Korrekt


nettleie_kapasitetsledd <- fread("data/database_nettleie_kapasitetsledd.csv",encoding = "Latin-1")



nettleie_kapasitetsledd[Nettselskap=="LEDE AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="LEDE AS"] # KORREKT

nettleie_kapasitetsledd[Nettselskap=="ELVIA AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="ELVIA AS"]  # Mangler 5 kr for trinn 1, 10 kr for trinn 2, 17.5 kr for trinn 3

nettleie_kapasitetsledd[Nettselskap=="TENSIO TS AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="TENSIO TS AS"]  # Korrekt

nettleie_kapasitetsledd[Nettselskap=="TENSIO TN AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="TENSIO TN AS"]  # Korrekt (fylke 50)

nettleie_kapasitetsledd[Nettselskap=="BKK NETT AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="BKK AS"]  # Korrekt

nettleie_kapasitetsledd[Nettselskap=="LNETT AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="LNETT AS"]  # Korrekt


nettleie_kapasitetsledd[Nettselskap=="ARVA AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="ARVA AS"]  # Korrekt

nettleie_kapasitetsledd[Nettselskap=="NORGESNETT AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="NORGESNETT AS"]  # Korrekt

nettleie_kapasitetsledd[Nettselskap=="GLITRE ENERGI NETT AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="GLITRE NETT AS"]  # Korrekt

nettleie_kapasitetsledd[Nettselskap=="FAGNE AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="FAGNE AS"]  # Korrekt

nettleie_kapasitetsledd[Nettselskap=="JÆREN EVERK AS"]
nettleie_kapasitetsledd_api_dt_simple[Nettselskap=="JÆREN EVERK AS"]  # Korrekt


