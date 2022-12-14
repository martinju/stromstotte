

# 2 read daily data and append to existing HOURLY data (every day at 13.15)

library(data.table)
library(lubridate)
library(rjson)

source("source/funcs.R")

KEY_FFAIL_DATA <- Sys.getenv("KEY_FFAIL_DATA")

playground = TRUE
areas <- c("NO1","NO2")
database_hourly_filename <- "data/database_hourly_fake.csv"
database_daily_filename <- "data/database_daily_fake.csv"

today <- as.IDate(Sys.time())

tomorrow <- today-1


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

