
### Getting nettleie for nettselskap from api
library(data.table)
library(rjson)

orgnr_dt <- fread("data/orgnr_dt.csv")
orgnr_dt[,datoId:=NULL] # Ignore date
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
#fwrite(nettleie_api_dt,"raw-data/raw_database_nettleie_nve_api.csv")

nettleie_api_dt[,time:=as.numeric(time)]
nettleie_api_dt[,energileddInk:=as.numeric(energileddInk)]

setkey(nettleie_api_dt,konsesjonar,effekttrinnFraKw,time)

setnames(nettleie_api_dt,"konsesjonar","Nettselskap")

nettselskap_fylke_dt <- unique(nettleie_api_dt[,.(Nettselskap,fylkeNr)])

#### Now, formatting nettleie dt

### TODO:
# 1. Convert to correct format
# 2. Check the values
# 3. Check for helg
# 4. Check for helligdag
# 5. Check for new nettselskap

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

fwrite(nettleie_api_dt_simple,"data/datebase_nettleie_api_simple.csv")

#### CONTINUE THE CHECK HERE, TO THEN EMAIL NVE ####

nettleie_dt_simple <- fread("data/database_nettleie_simple.csv")

# Alle mangler Avgift Energifondet 1.25

nettleie_dt_simple[Nettselskap=="LEDE AS"]
nettleie_api_dt_simple[Nettselskap=="LEDE AS"] # mangler Avgift Energifondet 1.25

nettleie_dt_simple[Nettselskap=="ELVIA AS"]
nettleie_api_dt_simple[Nettselskap=="ELVIA AS"]  # Mangler 10 øre på natta og 73 øre på dagen

nettleie_dt_simple[Nettselskap=="TENSIO TS AS"]
nettleie_api_dt_simple[Nettselskap=="TENSIO TS AS"]  # mangler ingenting (altså har energifondavgift inkludert)

nettleie_dt_simple[Nettselskap=="TENSIO TN AS"]
nettleie_api_dt_simple[Nettselskap=="TENSIO TN AS"]  # mangler ingenting for fylke 50 (altså har energifondavgift inkludert)

nettleie_dt_simple[Nettselskap=="BKK NETT AS"]
nettleie_api_dt_simple[Nettselskap=="BKK NETT AS"]  # mangler ingenting for fylke 50 (altså har energifondavgift inkludert)



#nettleie_kapasitetsledd <- fread("data/database_nettleie_kapasitetsledd.csv")

