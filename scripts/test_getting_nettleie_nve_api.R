
### Getting orgnr for nettselskap
library(data.table)
library(rjson)

today <- Sys.Date()

url <- paste0("https://biapi.nve.no/nettleietariffer/api/NettleiePerOmradePrManedHusholdningFritidEffekttariffer?FraDato=",
              today,
              "&Tariffgruppe=Husholdning&Kundegruppe=2")

dat_json <- rjson::fromJSON(file=url)

orgnr_dt <- matrix("",ncol=4,nrow=length(dat_json))

for (i in seq_along(dat_json)){
  orgnr_dt[i,] <- unlist(dat_json[[i]][c("datoId","konsesjonar","organisasjonsnr","fylkeNr")])
}

orgnr_dt <- as.data.table(orgnr_dt)
names(orgnr_dt) <- c("datoId","konsesjonar","organisasjonsnr","fylkeNr")

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
  dat_dt0 <- as.data.table(dat_json)
  nettleie_api_dt_list[[i]] <- as.data.table(t(dat_dt0[,lapply(.SD,unlist)]))
  names(nettleie_api_dt_list[[i]]) <- names(dat_json[[1]])[-3]


  print(i)
}

nettleie_api_dt <- rbindlist(nettleie_api_dt_list)
fwrite(nettleie_api_dt,"raw-data/raw_database_nettleie_nve_api.csv")

nettleie_api_dt[,time:=as.numeric(time)]
nettleie_api_dt[,energileddInk:=as.numeric(energileddInk)]

setkey(nettleie_api_dt,konsesjonar,effekttrinnFraKw,time)

setnames(nettleie_api_dt,"konsesjonar","Nettselskap")

nettselskap_vec <- nettleie_api_dt[,unique(Nettselskap)]

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

### TODO: Create a loop that runs through every nettselskap at effekttrinnFraKw==0, and creates one line in a new table whenever the nettleie changes


nettleie_api_dt[Energiledd_diff!=0&time==7] # FØIE AS differs from the others on day/night definition
nettleie_api_dt[effekttrinnFraKw==0 & Energiledd_diff!=0 & time!=6 & time!=22]

nettleie_api_dt[effekttrinnFraKw==0 & Nettselskap=="ENIDA AS"] # natt: 23-6
nettleie_api_dt[effekttrinnFraKw==0 & Nettselskap=="FØIE AS"] # natt: 22-7
nettleie_api_dt[effekttrinnFraKw==0 & Nettselskap=="GRIUG AS"] # natt: 22-7

GRIUG AS

nettleie_api_dt[,pristype:="Natt"]
nettleie_api_dt[time%in% seq(6,21),pristype:="Dag"]

nettleie_api_dt_simple <- unique(nettleie_api_dt[time %in% c(0,10),.(fylke,Nettselskap,energileddInk,pristype)])



nettleie_dt_simple <- fread("data/database_nettleie_simple.csv")
nettleie_kapasitetsledd <- fread("data/database_nettleie_kapasitetsledd.csv")

