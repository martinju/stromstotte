library(data.table)

nettleie <- fread("raw-data/innrapportert_nettleie_251022.csv",dec = ",",encoding = "Latin-1")

setnames(nettleie,"Konsesjonær","Nettselskap")
setnames(nettleie,"Energiledd (øre/kWh) eks. MVA","Energiledd")

nettleie[,length(unique(Energiledd)),by=.(Nettselskap,Time,Fylke)][,any(V1!=1)]

setnames(nettleie,"Har MVA","MVA")

keep_cols <- c("Nettselskap","Fylke", "MVA", "Energiledd","Time")

nettleie_dt <- unique(nettleie[,..keep_cols])

setkey(nettleie_dt,Fylke,Nettselskap,Time)

nettleie_dt[Fylke=="Troms og Finnmark Romsa ja Finnmárku",Fylke:="Troms og Finnmark"]

# Where does Energiledd jump?

nettleie_dt[,Energiledd_diff:=Energiledd-shift(Energiledd,type="lag"),by=.(Nettselskap,Fylke)]
nettleie_dt[Energiledd_diff!=0&Time==7]

nettleie_dt[,pristype:="Natt"]
nettleie_dt[Time%in% seq(6,21),pristype:="Dag"]

nettleie_dt_simple <- unique(nettleie_dt[Time %in% c(0,10),.(Fylke,Nettselskap,Energiledd,pristype,MVA)])

# Special object to handle FØIE AS that differs from the others in terms of definition of day/night (night include 6-7 as well)
nettleie_dt_simple_kl_6 <- unique(nettleie_dt[Time %in% 6,.(Fylke,Nettselskap,Energiledd,Time,pristype,MVA)])

setkey(nettleie_dt_simple,Fylke,Nettselskap,pristype)
setkey(nettleie_dt_simple_kl_6,Fylke,Nettselskap,pristype)

fwrite(nettleie_dt_simple,"data/database_nettleie_simple.csv")
fwrite(nettleie_dt_simple_kl_6,"data/database_nettleie_simple_kl_6.csv")


### Testing


nettleie_dt_simple[Nettselskap=="LEDE AS"]


nettleie_dt_simple[Nettselskap=="FØIE AS"]
nettleie_dt_simple_kl_6[Nettselskap=="FØIE AS"]

nettleie_dt[Nettselskap=="FØIE AS"]

nettleie_dt[Nettselskap=="NORGESNETT AS"]


nettleie_dt[,.N,by=.(Nettselskap)][N!=24]


nettleie_dt[Nettselskap=="GLITRE ENERGI NETT AS" & Time==0]

nettleie_dt[Nettselskap=="UVDAL KRAFTFORSYNING SA"]


nettleie_dt[,unique(Fylke)]

nettleie_dt[Fylke=="Vestfold og Telemark",unique(Nettselskap)]

nettleie_dt[,.N,by=c("Nettselskap","Effekttrinn fra KW")]

nettleie_dt[Nettselskap=="FJELLNETT AS" & Time==3 & `Effekttrinn fra KW` == 21]
