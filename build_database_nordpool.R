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

setnafill(dt,"locf",cols="price") # Just filling in something when we adjust the clock

fwrite(dt,"database_nordpool.csv")







