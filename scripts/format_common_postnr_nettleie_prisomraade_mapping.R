

library(data.table)
library(stringr)
Sys.setlocale(locale='en_US.UTF-8') # Also OK for reading Norwegian letters


nettleie_dt_simple <- fread("data/database_nettleie_simple.csv",encoding = "Latin-1")
nettleie_dt_simple_kl_6 <- fread("data/database_nettleie_simple_kl_6.csv",encoding = "Latin-1")

setnames(nettleie_dt_simple,"Nettselskap","nettselskap")
setnames(nettleie_dt_simple_kl_6,"Nettselskap","nettselskap")

postnr_prisomraader_area_dt <- fread("data/postnr_prisomraader_area.csv")
postnr_nettselskap_area_dt <- fread("data/postnr_nettselskap_area.csv")
nettselskap_prisomraader_area_dt <- fread("data/nettselskap_prisomraader_area.csv")




postnr_nettselskap_area_melted_dt <- melt(postnr_nettselskap_area_dt,id.vars=1:3,variable.name = "nettselskap",value.name = "area_postnr_nettselskap")
postnr_nettselskap_area_melted_dt[,nettselskap_org := as.character(nettselskap)]
postnr_nettselskap_area_melted_dt[,nettselskap:=
                                   unlist(lapply(strsplit(nettselskap_org,split="_dup_",fixed=T),FUN = "[[",1))]
postnr_nettselskap_area_melted_dt[grepl("_dup_",nettselskap_org),nettselskap_dup:=
                                   unlist(lapply(strsplit(nettselskap_org,split="_dup_",fixed=T),FUN = "[[",2))]
postnr_nettselskap_area_melted_dt <- postnr_nettselskap_area_melted_dt[area_postnr_nettselskap>1]

postnr_nettselskap_area_melted_dt[,no_nettselskap_per_postnr:=.N,by=.(postnr)]
postnr_nettselskap_area_melted_dt[no_nettselskap_per_postnr==1,min(area_postnr_nettselskap)] # 3096.826
postnr_nettselskap_area_melted_dt <- postnr_nettselskap_area_melted_dt[area_postnr_nettselskap>10^3]

postnr_prisomraader_area_melted_dt <- melt(postnr_prisomraader_area_dt,id.vars=1:3,variable.name="prisomraade",value.name="area_postnr_prisomraade")
postnr_prisomraader_area_melted_dt <- postnr_prisomraader_area_melted_dt[area_postnr_prisomraade>1]
postnr_prisomraader_area_melted_dt[,no_prisomraade_per_postnr:=.N,by=postnr]
postnr_prisomraader_area_melted_dt[no_prisomraade_per_postnr==1,min(area_postnr_prisomraade)] # 3096.826
postnr_prisomraader_area_melted_dt <- postnr_prisomraader_area_melted_dt[area_postnr_prisomraade>10^3]

nettselskap_prisomraader_area_dt[,nettselskap_org := nettselskap]
nettselskap_prisomraader_area_dt[,nettselskap:=
                                   unlist(lapply(strsplit(nettselskap_org,split="_dup_",fixed=T),FUN = "[[",1))]
nettselskap_prisomraader_area_dt[grepl("_dup_",nettselskap_org),nettselskap_dup:=
                                   unlist(lapply(strsplit(nettselskap_org,split="_dup_",fixed=T),FUN = "[[",2))]
nettselskap_prisomraader_area_privat_dt <- nettselskap_prisomraader_area_dt[eierType=="EVERK"][,1:8]
nettselskap_prisomraader_area_privat_melted_dt <- melt(nettselskap_prisomraader_area_privat_dt,id.vars=c("nettselskap","konsesjonType","eierType"),variable.name="prisomraade",value.name="area_prisomraade_nettselskap")
nettselskap_prisomraader_area_privat_melted_dt <- nettselskap_prisomraader_area_privat_melted_dt[area_prisomraade_nettselskap>1]
nettselskap_prisomraader_area_privat_melted_dt[,no_prisomraade_per_nettselskap:=.N,by=nettselskap]
nettselskap_prisomraader_area_privat_melted_dt[no_prisomraade_per_nettselskap==1,min(area_prisomraade_nettselskap)] # 142972
nettselskap_prisomraader_area_privat_melted_dt <- nettselskap_prisomraader_area_privat_melted_dt[area_prisomraade_nettselskap>10^3]


postnr_prisomraader_nettselskap_dt <- merge(postnr_nettselskap_area_melted_dt[,1:5],postnr_prisomraader_area_melted_dt[,1:5],by=c("postnr","poststed","kommune"))

merge(postnr_prisomraader_nettselskap_dt,nettselskap_prisomraader_area_privat_melted_dt,by=c("nettselskap","prisomraade"),allow.cartesian = T)


postnr_prisomraader_nettselskap_dt[,N:=.N,by=postnr]

postnr_prisomraader_nettselskap_dt[,max(N)]
postnr_prisomraader_nettselskap_dt[N==12]

nettselskap_prisomraader_area_privat_melted_dt[nettselskap=="ROLLAG ELEKTRISITETSVERK AS"]

#### OLD



all_nettselskap_dt_simple <- sort(nettleie_dt_simple[,unique(nettselskap)])
all_nettselskap_prisomraader_area_privat_dt <- sort(nettselskap_prisomraader_area_privat_dt[,unique(nettselskap)])
all_nettselskap_postnr_privat_dt <- sort(postnr_nettselskap_area_melted_dt[,unique(nettselskap)])



all_nettselskap_dt_simple[!(all_nettselskap_dt_simple %in% all_nettselskap_prisomraader_area_privat_dt)]

all_nettselskap_prisomraader_area_privat_dt[!(all_nettselskap_prisomraader_area_privat_dt %in% all_nettselskap_dt_simple)]

replace_vec <- c("ARVA AS","HALLINGDAL KRAFTNETT A/S","TROLLFJORD KRAFT AS","FØIE AS")
names(replace_vec) <- c("ARVA AS*","HALLINGDAL KRAFTNETT AS","TROLLFJORD NETT AS","NORE ENERGI AS")
# Unmatched missing nettselskap: "TINFOS AS" "HERØYA NETT AS"

nettleie_dt_simple[,nettselskap:=str_replace_all(nettselskap,fixed(replace_vec))]
nettleie_dt_simple_kl_6[,nettselskap:=str_replace_all(nettselskap,fixed(replace_vec))]
nettselskap_prisomraader_area_privat_dt[,nettselskap:=str_replace_all(nettselskap,fixed(replace_vec))]



postnr_prisomraader_area_dt_id <- copy(postnr_prisomraader_area_dt)

postnr_prisomraader_area_dt_id[,max_area := do.call(pmax,.SD),.SDcols=paste0("NO ",1:5)]

postnr_prisomraader_area_dt_id[,paste0("NO ",1:5) := lapply(.SD,function(x)x>100*100 | (x==max_area & max_area>0)),.SDcols=paste0("NO ",1:5)]

postnr_prisomraader_dt_list <- list()
for(i in 1:5){
  col <- paste0("NO ",i)
  postnr_prisomraader_dt_list[[i]] <- postnr_prisomraader_area_dt_id[get(col)==TRUE,.(postnr,poststed,kommune,prisomraade=col)]
}
postnr_prisomraader_dt <- rbindlist(postnr_prisomraader_dt_list)
postnr_prisomraader_dt <- unique(postnr_prisomraader_dt)
setkey(postnr_prisomraader_dt,postnr)

postnr_prisomraader_dt[,no_prisomraade_per_postnr:=.N,by=postnr]

postnr_prisomraader_dt[no_prisomraade_per_postnr>1]
