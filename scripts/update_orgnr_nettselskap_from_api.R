### Getting orgnr for nettselskap
library(data.table)
library(rjson)

orgnr_dt_org <- fread("data/orgnr_dt.csv")


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
orgnr_dt[,datoId:=as.IDate(datoId)]

orgnr_dt_new <- rbind(orgnr_dt,orgnr_dt_org)

fwrite(orgnr_dt_new,"data/orgnr_dt.csv")
