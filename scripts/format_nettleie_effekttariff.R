library(data.table)

Sys.setlocale(locale='en_US.UTF-8') # Also OK for reading Norwegian letters

nettleie <- fread("raw-data/innrapportert_nettleie_251022.csv",dec = ",",encoding = "Latin-1")

setnames(nettleie,"Konsesjonær","Nettselskap")
#setnames(nettleie,"Energiledd (øre/kWh) ink. MVA","Energiledd")
setnames(nettleie,"Fastledd (kr/måned) ink. MVA","Kapasitetsledd")
setnames(nettleie,"Effekttrinn fra KW","Kapasitetsledd fra kW")
setnames(nettleie,"Effekttrinn til KW","Kapasitetsledd til kW")
setnames(nettleie,"Har MVA","MVA")


#keep_cols <- c("Nettselskap","Fylke", "MVA", "Energiledd","Time")
keep_cols <- c("Nettselskap","Fylke","Kapasitetsledd","Kapasitetsledd fra kW","Kapasitetsledd til kW","Grunnlag effektrinn")

nettleie_dt <- unique(nettleie[,..keep_cols])

setkey(nettleie_dt,Fylke,Nettselskap,`Kapasitetsledd fra kW`)

nettleie_dt[Fylke=="Troms og Finnmark Romsa ja Finnmárku",Fylke:="Troms og Finnmark"]


# Checking if FYlke matters here
aa=nettleie_dt[,.SD,.SDcols=c("Nettselskap","Kapasitetsledd fra kW","Kapasitetsledd til kW", "Grunnlag effektrinn","Kapasitetsledd")]
(unique(aa)[,.N,by=c("Nettselskap","Kapasitetsledd fra kW","Kapasitetsledd til kW", "Grunnlag effektrinn")][N>1,unique(Nettselskap)])

nettleie_dt[,keep:=TRUE]
nettleie_dt[Nettselskap=="TINFOS AS" & Fylke=="Troms og Finnmark",keep:=FALSE] # Finnes ikke i data
nettleie_dt[Nettselskap=="TENSIO TN AS" & Fylke == "Nordland",keep:=FALSE] # Oppført da de deltar i kraftutredning https://tn.tensio.no/kraftsystemutredning-for-nord-trondelag-og-bindal
nettleie_dt <- nettleie_dt[keep==TRUE]
nettleie_dt[,keep:=NULL]

aa=nettleie_dt[,.SD,.SDcols=c("Nettselskap","Kapasitetsledd fra kW","Kapasitetsledd til kW", "Grunnlag effektrinn","Kapasitetsledd")]
(unique(aa)[,.N,by=c("Nettselskap","Kapasitetsledd fra kW","Kapasitetsledd til kW", "Grunnlag effektrinn")][N>1,unique(Nettselskap)])
# NONE

nettleie_dt[,Fylke:=NULL]
nettleie_dt <- unique(nettleie_dt)

nettleie_dt[Nettselskap=="AGDER ENERGI NETT AS",Kapasitetsledd:=c(135,170,290,600,780,980,1520,2400,3200,5200)]

nettleie_dt[Nettselskap=="LEDE AS"] #OK

nettleie_dt[Nettselskap=="ELVIA AS"] #OK

nettleie_dt[Nettselskap=="TENSIO TS AS", Kapasitetsledd:=c(83,147,252,371,490,610,1048,1645,2243,3239,4433,6424,8815,11204,13594)]
nettleie_dt[Nettselskap=="TENSIO TS AS", `Grunnlag effektrinn`:="Topp 3 forbrukstopper innenfor 1 siste måneder"]


nettleie_dt[Nettselskap=="TENSIO TN AS", Kapasitetsledd:=c(104,207,375,569,764,957,1668,2638,3608,5224,7165,10396,14276,18156,22034)]

nettleie_dt[Nettselskap=="BKK NETT AS"] #OK

nettleie_dt[Nettselskap=="LNETT AS"] #OK

nettleie_dt[Nettselskap=="ARVA AS",Kapasitetsledd:=c(73,173,342,511,680,850,1696,2542,3388,5112)]

nettleie_dt[Nettselskap=="NORGESNETT AS", Kapasitetsledd:=c(129.94,216.56,356.13,633.33,841.23,1043.35,1617,2531.38,3445.75)]

nettleie_dt[Nettselskap=="FAGNE AS"] OK


fwrite(nettleie_dt,"data/database_nettleie_kapasitetsledd.csv")


# ### Testing
#
#
# nettleie_dt_simple[Nettselskap=="LEDE AS"]
#
#
# nettleie_dt_simple[Nettselskap=="FØIE AS"]
# nettleie_dt_simple_kl_6[Nettselskap=="FØIE AS"]
#
# nettleie_dt[Nettselskap=="FØIE AS"]
#
# nettleie_dt[Nettselskap=="NORGESNETT AS"]
#
#
# nettleie_dt[,.N,by=.(Nettselskap)][N!=24]
#
#
# nettleie_dt[Nettselskap=="GLITRE ENERGI NETT AS" & Time==0]
#
# nettleie_dt[Nettselskap=="UVDAL KRAFTFORSYNING SA"]
#
#
# nettleie_dt[,unique(Fylke)]
#
# nettleie_dt[Fylke=="Vestfold og Telemark",unique(Nettselskap)]
#
# nettleie_dt[,.N,by=c("Nettselskap","Effekttrinn fra KW")]
#
# nettleie_dt[Nettselskap=="FJELLNETT AS" & Time==3 & `Effekttrinn fra KW` == 21]
