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

nettleie_dt_simple[,helg:=FALSE]
nettleie_dt_simple[,kontrollert_pris:=FALSE]

#nettleie_dt_simple[,Energiledd:=round(Energiledd-6.25*1.25,2)] # Endring av energiledd fra oktober 2022 til jan-mars 2023


# https://www.aenett.no/nettleie/tariffer/
nettleie_dt_simple[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Dag",Energiledd:=50.00]
nettleie_dt_simple[Nettselskap=="AGDER ENERGI NETT AS" & pristype=="Natt",Energiledd:=38.00]
nettleie_dt_simple[Nettselskap=="AGDER ENERGI NETT AS",kontrollert_pris:=TRUE]

#https://lede.no/priser/
nettleie_dt_simple[Nettselskap=="LEDE AS" , Energiledd:=21.25+19.80+1.25]
nettleie_dt_simple[Nettselskap=="LEDE AS",kontrollert_pris:=TRUE]


#https://www.elvia.no/nettleie/alt-om-nettleiepriser/nettleiepriser-for-privatkunder/
nettleie_dt_simple[Nettselskap=="ELVIA AS" & pristype=="Dag",Energiledd:=47.50]
nettleie_dt_simple[Nettselskap=="ELVIA AS" & pristype=="Natt",Energiledd:=40.00]
tmp <- nettleie_dt_simple[Nettselskap=="ELVIA AS"& pristype=="Natt"] # Same as natt
tmp[,pristype:="Helg"]
tmp[,helg:=TRUE]
nettleie_dt_simple <- rbind(nettleie_dt_simple,tmp)
nettleie_dt_simple[Nettselskap=="ELVIA AS",kontrollert_pris:=TRUE]


# https://ts.tensio.no/kunde/nettleie-priser-og-avtaler
nettleie_dt_simple[Nettselskap=="TENSIO TS AS" & pristype=="Natt",Energiledd:=30.68]
nettleie_dt_simple[Nettselskap=="TENSIO TS AS" & pristype=="Dag",Energiledd:=40.30]
nettleie_dt_simple[Nettselskap=="TENSIO TS AS",kontrollert_pris:=TRUE]

# https://tn.tensio.no/nettleie-og-tilknytningsavtaler
nettleie_dt_simple[Nettselskap=="TENSIO TN AS" & pristype=="Dag",Energiledd:=48.55]
nettleie_dt_simple[Nettselskap=="TENSIO TN AS" & pristype=="Natt",Energiledd:=34.80]
nettleie_dt_simple[Nettselskap=="TENSIO TN AS",kontrollert_pris:=TRUE]

#https://nett.bkk.no/produktdetaljer?productId=49cedfc9-82b1-4d3b-be45-904704e3b9c7&divisionName=Nett
nettleie_dt_simple[Nettselskap=="BKK NETT AS" & pristype=="Dag",Energiledd:=55.73]
nettleie_dt_simple[Nettselskap=="BKK NETT AS" & pristype=="Natt",Energiledd:=43.93]
tmp <- nettleie_dt_simple[Nettselskap=="BKK NETT AS"& pristype=="Natt"] # Same as natt
tmp[,pristype:="Helg"]
tmp[,helg:=TRUE]
nettleie_dt_simple <- rbind(nettleie_dt_simple,tmp)
tmp[,pristype:="Helligdag"] ### Må sjekkes spesielt
nettleie_dt_simple <- rbind(nettleie_dt_simple,tmp)
nettleie_dt_simple[Nettselskap=="BKK NETT AS",kontrollert_pris:=TRUE]


#https://www.l-nett.no/nettleie/priser-og-vilkar-privat/
nettleie_dt_simple[Nettselskap=="LNETT AS" & pristype=="Dag",Energiledd:=30.59+19.80+1.25]
nettleie_dt_simple[Nettselskap=="LNETT AS" & pristype=="Natt",Energiledd:=22.59+19.80+1.25]
tmp <- nettleie_dt_simple[Nettselskap=="LNETT AS"& pristype=="Natt"] # Same as natt
tmp[,pristype:="Helg"]
tmp[,helg:=TRUE]
nettleie_dt_simple <- rbind(nettleie_dt_simple,tmp)
nettleie_dt_simple[Nettselskap=="LNETT AS",kontrollert_pris:=TRUE]



#https://arva.no/ny-nettleie/Priser
nettleie_dt_simple[Nettselskap=="ARVA AS" & pristype=="Natt",Energiledd:=9.9+1+19.80/1.25]
nettleie_dt_simple[Nettselskap=="ARVA AS" & pristype=="Dag",Energiledd:=19.9+1+19.80/1.25]
nettleie_dt_simple[Nettselskap=="ARVA AS",kontrollert_pris:=TRUE]

##### KOMMET HIT ################

#https://norgesnett.no/nettleie-privat/
nettleie_dt_simple[Nettselskap=="NORGESNETT AS" & pristype=="Natt",Energiledd:=30.56]
nettleie_dt_simple[Nettselskap=="NORGESNETT AS" & pristype=="Dag",Energiledd:=35.57]
nettleie_dt_simple[Nettselskap=="NORGESNETT AS", kontrollert_pris:=TRUE]

# https://www.aenett.no/nettleie/tariffer/
nettleie_dt_simple[Nettselskap=="GLITRE ENERGI NETT AS" & pristype=="Dag",Energiledd:=50.00]
nettleie_dt_simple[Nettselskap=="GLITRE ENERGI NETT AS" & pristype=="Natt",Energiledd:=38.00]
nettleie_dt_simple[Nettselskap=="GLITRE ENERGI NETT AS",kontrollert_pris:=TRUE]

# https://fagne.no/kunde-og-nettleie/nettleie-priser-og-vilkar/priser-privatkunder/
nettleie_dt_simple[Nettselskap=="FAGNE AS" & pristype=="Dag",Energiledd:=52.05]
nettleie_dt_simple[Nettselskap=="FAGNE AS" & pristype=="Natt",Energiledd:=42.05]
tmp <- nettleie_dt_simple[Nettselskap=="FAGNE AS"& pristype=="Natt"] # Same as natt
tmp[,pristype:="Helg"]
tmp[,helg:=TRUE]
nettleie_dt_simple <- rbind(nettleie_dt_simple,tmp)
nettleie_dt_simple[Nettselskap=="FAGNE AS",kontrollert_pris:=TRUE]

# https://jev.no/nettleie-for-kunder-med-forbruk-under-100-000-kwh-2-2-2
nettleie_dt_simple[Nettselskap=="JÆREN EVERK AS"] #OK
nettleie_dt_simple[Nettselskap=="JÆREN EVERK AS" & pristype=="Dag",Energiledd:=23.75+19.80+1.25]
nettleie_dt_simple[Nettselskap=="JÆREN EVERK AS" & pristype=="Natt",Energiledd:=16.25+19.80+1.25]
tmp <- nettleie_dt_simple[Nettselskap=="JÆREN EVERK AS"& pristype=="Natt"] # Same as natt
tmp[,pristype:="Helg"]
tmp[,helg:=TRUE]
nettleie_dt_simple <- rbind(nettleie_dt_simple,tmp)
nettleie_dt_simple[Nettselskap=="JÆREN EVERK AS",kontrollert_pris:=TRUE]



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
