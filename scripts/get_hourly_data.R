

# 2 read daily data and append to existing HOURLY data (every day at 13.15)

library(data.table)
library(lubridate)
library(rjson)

source("funcs.R")

KEY_FFAIL_DATA <- Sys.getenv("KEY_FFAIL_DATA")

playground = FALSE
areas <- c("NO1","NO2")#,"NO1")#c("NO1","NO2")
database_hourly_filename <- "database_hourly.csv"
database_daily_filename <- "database_daily_nordpool.csv"

today <- as.IDate(Sys.time())-1#as.IDate("2022-09-23") #as.IDate(Sys.time())

tomorrow <- today+1


tomorrow_dt <- NULL
for(j in seq_along(areas)){
  if(playground){
    file = paste0("https://playground-norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=123")
  } else {
    file = paste0("https://norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=",KEY_FFAIL_DATA)
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

