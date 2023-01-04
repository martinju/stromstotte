library(data.table)

Sys.setlocale(locale='en_US.UTF-8') # Also OK for reading Norwegian letters

nettleie <- fread("raw-data/innrapportert_nettleie_251022.csv",dec = ",",encoding = "Latin-1")

setnames(nettleie,"Konsesjonær","Nettselskap")
setnames(nettleie,"Energiledd (øre/kWh) ink. MVA","Energiledd")

nettleie[,length(unique(Energiledd)),by=.(Nettselskap,Time,Fylke)][,any(V1!=1)]

setnames(nettleie,"Har MVA","MVA")

#keep_cols <- c("Nettselskap","Fylke", "MVA", "Energiledd","Time")
keep_cols <- c("Nettselskap","Fylke", "Energiledd","Time")

nettleie_dt <- unique(nettleie[,..keep_cols])

setkey(nettleie_dt,Fylke,Nettselskap,Time)

nettleie_dt[Fylke=="Troms og Finnmark Romsa ja Finnmárku",Fylke:="Troms og Finnmark"]

# Where does Energiledd jump?

nettleie_dt[,Energiledd_diff:=Energiledd-shift(Energiledd,type="lag"),by=.(Nettselskap,Fylke)]
nettleie_dt[Energiledd_diff!=0&Time==7]

nettleie_dt[,pristype:="Natt"]
nettleie_dt[Time%in% seq(6,21),pristype:="Dag"]

nettleie_dt_simple <- unique(nettleie_dt[Time %in% c(0,10),.(Fylke,Nettselskap,Energiledd,pristype)])

# Special object to handle FØIE AS that differs from the others in terms of definition of day/night (night include 6-7 as well)
nettleie_dt_simple_kl_6 <- unique(nettleie_dt[Time %in% 6,.(Fylke,Nettselskap,Energiledd,Time,pristype)])

setkey(nettleie_dt_simple,Fylke,Nettselskap,pristype)
setkey(nettleie_dt_simple_kl_6,Fylke,Nettselskap,pristype)

### CHecking if Fylke matters at all

nettleie_dt_simple[,keep:=TRUE]
nettleie_dt_simple[Nettselskap=="TINFOS AS" & Fylke=="Troms og Finnmark",keep:=FALSE] # Finnes ikke i data
nettleie_dt_simple[Nettselskap=="TENSIO TN AS" & Fylke == "Nordland",keep:=FALSE] # Oppført da de deltar i kraftutredning https://tn.tensio.no/kraftsystemutredning-for-nord-trondelag-og-bindal
nettleie_dt_simple <- nettleie_dt_simple[keep==TRUE]
nettleie_dt_simple[,keep:=NULL]

nettleie_dt_simple_kl_6[,keep:=TRUE]
nettleie_dt_simple_kl_6[Nettselskap=="TINFOS AS" & Fylke=="Troms og Finnmark",keep:=FALSE] # Finnes ikke i data
nettleie_dt_simple_kl_6[Nettselskap=="TENSIO TN AS" & Fylke == "Nordland",keep:=FALSE] # Oppført da de deltar i kraftutredning https://tn.tensio.no/kraftsystemutredning-for-nord-trondelag-og-bindal
nettleie_dt_simple_kl_6 <- nettleie_dt_simple_kl_6[keep==TRUE]
nettleie_dt_simple_kl_6[,keep:=NULL]



(fylke_matters1 <- unique(nettleie_dt_simple[,.(Nettselskap,Energiledd,pristype)])[,.N,by=Nettselskap][,any(N!=2)]) # FALSE
(fylke_matters2 <- unique(nettleie_dt_simple_kl_6[,.(Nettselskap,Energiledd,pristype)])[,.N,by=Nettselskap][,any(N!=1)]) # FALSE

if(!(fylke_matters1 & fylke_matters1)){
  nettleie_dt_simple[,Fylke:=NULL]
  nettleie_dt_simple_kl_6[,Fylke:=NULL]

  nettleie_dt_simple <- unique(nettleie_dt_simple)
  nettleie_dt_simple_kl_6 <- unique(nettleie_dt_simple_kl_6)

}


nettleie_dt_simple[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Dag",Energiledd:=38.65]
nettleie_dt_simple[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Natt",Energiledd:=26.65]

nettleie_dt_simple_kl_6[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Dag",Energiledd:=38.65]


fwrite(nettleie_dt_simple,"data/database_nettleie_simple.csv")
fwrite(nettleie_dt_simple_kl_6,"data/database_nettleie_simple_kl_6.csv")


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
