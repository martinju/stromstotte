

# 2 read daily data and append to existing HOURLY data (every day at 13.15)

library(data.table)
library(lubridate)
library(rjson)

source("source/funcs.R")

KEY_FFAIL_DATA <- Sys.getenv("KEY_FFAIL_DATA")
source = c("ffail","nordpool")

playground = FALSE
areas <- c("NO1","NO2")#c("NO3","NO4","NO5")

database_hourly_ffail_filename <- "data/database_ffail_hourly.csv"
database_daily_ffail_filename <- "data/database_ffail_daily.csv"

database_hourly_nordpool_filename <- "data/database_nordpool_hourly.csv"
database_daily_nordpool_filename <- "data/database_nordpool_daily.csv"



today <- as.IDate(Sys.time())

tomorrow <- today+1


if("ffail" %in% source){
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

  fwrite(daily_tomorrow_dt,database_daily_ffail_filename,append = T)

  fwrite(tomorrow_dt,database_hourly_ffail_filename,append = T)

}

if("nordpool" %in% source){
  tomorrow_dt <- basic_nordpoolAPI_to_dt(areas=areas) # always day-ahead!

  daily_tomorrow_dt <- tomorrow_dt[,list(price=mean(price)),by=.(area,date)]

  fwrite(daily_tomorrow_dt,database_daily_nordpool_filename,append = T)

  fwrite(tomorrow_dt,database_hourly_nordpool_filename,append = T)

}

