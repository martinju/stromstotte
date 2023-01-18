
### Add first three october days to nordpool data

dt_oct <- fread("raw-data/extra_data_nordpool_daily.csv",dec=",")


dt_melted_oct <- melt(dt_oct,id.vars = c("date","start_hour"),variable.name = "area",value="price")
dt_melted_oct[,price:=as.numeric(sub(",", ".",x = sub(" ","",price,fixed=TRUE), fixed=TRUE))]
dt_melted_oct[,price:=price/1000]
dt_melted_oct[,date:=as.IDate(date,format="%d.%m.%Y")]
dt_melted_oct[,area:=as.character(area)]

setcolorder(dt_melted_oct,"area")
setorder(dt_melted_oct,date,area,start_hour)

dt_hourly <- fread("data/database_nordpool_hourly.csv")

dt_hourly2 <- rbind(dt_melted_oct,dt_hourly)

fwrite(dt_hourly2,"data/database_nordpool_hourly.csv")
