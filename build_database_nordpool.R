#aa = fread("daily_prices_NO2_nordpool.csv",header = T)

library(data.table)

dt <- fread("hourly_prices_NO2_nordpool.csv",header=T,dec=",")


dt <- melt(dt,id.vars = "V1",variable.name = "date",value.name = "price")
setnames(dt,"V1","time")
dt[,area:="NO2"]
dt[,date:=as.IDate(stringr::str_replace_all(as.character(date),"[.]","-"),tryFormats = c("%d-%m-%Y"))]
dt[,start_hour:=as.numeric(stringr::str_sub(time,end=2L))]
dt[,time:=NULL]
setcolorder(dt,c("area","start_hour","date","price"))
setorderv(dt,cols=c("area","date","start_hour"))
dt[,price:=price/1000]

fwrite(dt,"datebase_nordpool.csv")
