library(data.table)

nettleie <- fread("raw-data/innrapportert_nettleie_251022.csv",dec = ",",encoding = "Latin-1")

keep_cols <- c("Konsesjonær","Fylke", "Har MVA", "Energiledd (øre/kWh) eks. MVA","Effekttrinn fra KW","Effekttrinn til KW","Time")

nettleie_dt <- nettleie[,..keep_cols]
