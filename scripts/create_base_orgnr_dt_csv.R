

csv_nettleie_dt <- fread("raw-data/innrapportert_nettleie_251022.csv",dec = ",",encoding = "Latin-1")

orgnr_dt_csv <- unique(csv_nettleie_dt[,c("Dato_Id","Konsesjonær","Organisasjonsnummer","FylkeNr")])
names(orgnr_dt_csv) <- c("datoId","konsesjonar","organisasjonsnr","fylkeNr")
orgnr_dt_csv[,datoId:=as.Date(datoId,format="%d.%m.%Y")]

fwrite(orgnr_dt_csv,"data/orgnr_dt.csv")
