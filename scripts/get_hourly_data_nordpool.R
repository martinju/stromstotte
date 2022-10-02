

# 2 read daily data and append to existing HOURLY data (every day at 13.15)

library(data.table)
library(lubridate)
library(rjson)

source("source/funcs.R")

areas <- c("NO1","NO2")#c("NO3","NO4","NO5")

database_hourly_nordpool_filename <- "data/database_nordpool_hourly.csv"
database_daily_nordpool_filename <- "data/database_nordpool_daily.csv"



today <- as.IDate(Sys.time())

tomorrow <- today+1


tomorrow_dt <- basic_nordpoolAPI_to_dt(areas=areas) # always day-ahead!

daily_tomorrow_dt <- tomorrow_dt[,list(price=mean(price)),by=.(area,date)]

fwrite(daily_tomorrow_dt,database_daily_nordpool_filename,append = T)

fwrite(tomorrow_dt,database_hourly_nordpool_filename,append = T)

