#aa = fread("daily_prices_NO2_nordpool.csv",header = T)

library(data.table)

#dt <- fread("hourly_prices_NO2_nordpool.csv",header=T,dec=",")
dt <- fread("hourly_prices_NO2_nordpoolv2.csv",header=T,dec=",")




dt <- melt(dt,id.vars = "V1",variable.name = "date",value.name = "price")
setnames(dt,"V1","time")
dt[,area:="NO2"]
dt[,date:=as.IDate(stringr::str_replace_all(as.character(date),"[.]","-"),tryFormats = c("%d-%m-%Y"))]
dt[,start_hour:=as.numeric(stringr::str_sub(time,end=2L))]
dt[,time:=NULL]
setcolorder(dt,c("area","start_hour","date","price"))
setorderv(dt,cols=c("area","date","start_hour"))
dt[,price:=price*1.25/1000] # ADD MVA and convert to kWh

dt_fake <- copy(dt)
dt_fake[,area:="NO1"]
dt_fake[,price:=price+1]

dt <- rbind(dt,dt_fake)

fwrite(dt,"database_nordpool.csv")





tmp <- fread("daily_prices_NO2_nordpool.csv",header=T,dec=",")
tmp[,area:="NO2"]
tmp[,V1:=NULL]
setcolorder(tmp,rev(names(tmp)))
dt_daily <- melt(tmp,id.vars="area",value.name = "price")

first_date <- as.IDate("2021-11-01")
dt_daily <- dt_daily[!is.na(price)]
dt_daily[,date:=seq(from=first_date,by=1,length.out=.N)]
dt_daily[,variable:=NULL]

setcolorder(dt_daily,c("area","date"))

fwrite(dt_daily,"datebase_daily_nordpool.csv")


