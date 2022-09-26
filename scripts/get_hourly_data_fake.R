

# 2 read daily data and append to existing HOURLY data (every day at 13.15)

library(data.table)
library(lubridate)

source("funcs.R")


playground = TRUE
key = "1e931976-1e0a-466d-811d-aec47e9ebe42"
areas <- c("NO1","NO2")#,"NO1")#c("NO1","NO2")
database_hourly_filename <- "database_hourly_fake.csv"
database_daily_filename <- "database_daily_nordpool.csv"

today <- as.IDate(Sys.time())#as.IDate("2022-09-23") #as.IDate(Sys.time())

tomorrow <- today-1


tomorrow_dt <- NULL
for(j in seq_along(areas)){
  if(playground){
    file = paste0("https://playground-norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=123")
  } else {
    file = paste0("https://norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=",key)
  }

  this_dt <- basic_ffail_to_dt(file,areas[j])
  tomorrow_dt <- rbind(tomorrow_dt,this_dt)

}

if(playground){
  tomorrow_dt[,date:=tomorrow]
}

daily_tomorrow_dt <- tomorrow_dt[,list(price=mean(price)),by=.(area,date)]

fwrite(daily_tomorrow_dt,database_daily_filename,append = T)

fwrite(tomorrow_dt,database_hourly_filename,append = T)

