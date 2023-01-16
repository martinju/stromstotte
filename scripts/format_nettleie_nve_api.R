
library(data.table)

Sys.setlocale(locale='en_US.UTF-8') # Also OK for reading Norwegian letters

nettleie <- fread("data/nettleie_nve_api.csv")




setnames(nettleie,"konsesjonar","Nettselskap")
setnames(nettleie,"energileddInk","Energiledd")
setnames(nettleie,"fylke","Fylke")
setnames(nettleie,"time","Time")
setnames(nettleie,"organisasjonsnr","Orgnr")


nettleie[,length(unique(Energiledd)),by=.(Nettselskap,Time,Fylke)][,any(V1!=1)]

#setnames(nettleie,"Har MVA","MVA")

#keep_cols <- c("Nettselskap","Fylke", "MVA", "Energiledd","Time")
keep_cols <- c("Nettselskap","Orgnr","Fylke", "Energiledd","Time")

nettleie_dt <- unique(nettleie[,..keep_cols])

setkey(nettleie_dt,Fylke,Nettselskap,Orgnr,Time)

nettleie_dt[Fylke=="Troms og Finnmark Romsa ja Finnmárku",Fylke:="Troms og Finnmark"]


unique(nettleie_dt[,.(Orgnr,Nettselskap)])[,.N,by=Nettselskap][,any(N!=1)]
unique(nettleie_dt[,.(Orgnr,Nettselskap)])[,.N,by=Orgnr][,any(N!=1)]

# Where does Energiledd jump?

nettleie_dt[,Energiledd_diff:=Energiledd-shift(Energiledd,type="lag"),by=.(Nettselskap,Orgnr,Fylke)]
nettleie_dt[Energiledd_diff!=0,.N,by=Time]
nettleie_dt[Energiledd_diff!=0&!(Time%in%c(6,22))]

nettleie_dt[,pristype:="Natt"]
nettleie_dt[Time%in% seq(6,21),pristype:="Dag"]

nettleie_dt_simple <- unique(nettleie_dt[Time %in% c(0,10),.(Fylke,Nettselskap,Orgnr,Energiledd,pristype)])

# Special object to handle FØIE AS that differs from the others in terms of definition of day/night (night include 6-7 as well)
nettleie_dt_simple_kl_6 <- unique(nettleie_dt[Time %in% 6,.(Fylke,Nettselskap,Orgnr,Energiledd,Time,pristype)])

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



(fylke_matters1 <- unique(nettleie_dt_simple[,.(Nettselskap,Orgnr,Energiledd,pristype)])[,.N,by=Nettselskap][,any(N!=2)]) # FALSE
(fylke_matters2 <- unique(nettleie_dt_simple_kl_6[,.(Nettselskap,Orgnr,Energiledd,pristype)])[,.N,by=Nettselskap][,any(N!=1)]) # FALSE

if(!(fylke_matters1 & fylke_matters1)){
  nettleie_dt_simple[,Fylke:=NULL]
  nettleie_dt_simple_kl_6[,Fylke:=NULL]

  nettleie_dt_simple <- unique(nettleie_dt_simple)
  nettleie_dt_simple_kl_6 <- unique(nettleie_dt_simple_kl_6)

}

nettleie_dt_simple[Nettselskap=="GLITRE ENERGI NETT AS"]
nettleie_dt_simple[Nettselskap=="GLITRE NETT AS"]

nettleie_dt_simple[Nettselskap=="ELVIA AS"]

unique(nettleie[Orgnr==980489698,.(Orgnr,Nettselskap,fylkeNr,energileddEks,Energiledd)])
# https://www.elvia.no/nettleie/alt-om-nettleiepriser/nettleiepriser-for-privatkunder/


nettleie_dt_simple[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Dag",Energiledd:=38.65]
nettleie_dt_simple[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Natt",Energiledd:=26.65]

nettleie_dt_simple_kl_6[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Dag",Energiledd:=38.65]





fwrite(nettleie_dt_simple,"data/database_nettleie_simple.csv")
fwrite(nettleie_dt_simple_kl_6,"data/database_nettleie_simple_kl_6.csv")

