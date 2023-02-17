
### Use spotpriserNorge2022 xlsx file to add data Nordpool data before october 2022

library(readxl)



dt_NO1 <- as.data.table(readxl::read_xlsx("raw-data/SpotpriserNorge2022.xlsx",sheet="NO1"))
dt_NO2 <- as.data.table(readxl::read_xlsx("raw-data/SpotpriserNorge2022.xlsx",sheet="NO2"))
dt_NO3 <- as.data.table(readxl::read_xlsx("raw-data/SpotpriserNorge2022.xlsx",sheet="NO3"))
dt_NO4 <- as.data.table(readxl::read_xlsx("raw-data/SpotpriserNorge2022.xlsx",sheet="NO4"))
dt_NO5 <- as.data.table(readxl::read_xlsx("raw-data/SpotpriserNorge2022.xlsx",sheet="NO5"))

dt_all <- data.table::rbindlist(list(dt_NO1,dt_NO2,dt_NO3,dt_NO4,dt_NO5),use.names = F,idcol = "area")
dt_all[,area:=paste0("NO",area)]
melted_dt_all <- melt(dt_all,id.vars = c("area","ØST"))
melted_dt_all[,start_hour:=as.numeric(substr(variable,1,2))]
melted_dt_all[,value:=value/100]
melted_dt_all[,variable:=NULL]
names(melted_dt_all)=c("area","date","price","start_hour")
setcolorder(melted_dt_all,c("area","date","start_hour","price"))
setkey(melted_dt_all,date,area)

melted_dt_all[,date:=as.IDate(date)]
melted_dt_all[,price:=price/1.25]

fwrite(melted_dt_all,"data/database_nordpool_hourly_2022_no_mva.csv")


#aa=melted_dt_all[date>="2022-10-04" & date<"2023-01-01"]
#bb <- dt_hourly[date>="2022-10-04" & date<"2023-01-01"]

#waldo::compare(aa[area!="NO4"],bb[area!="NO4"])

#aa[date=="2022-10-04" & area=="NO4"]
#bb[date=="2022-10-04" & area=="NO4"]

