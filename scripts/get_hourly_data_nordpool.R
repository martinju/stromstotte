

# 2 read daily data and append to existing HOURLY data (every day at 13.15)

library(data.table)
library(lubridate)
library(rjson)

source("source/funcs.R")

areas <- c("NO1","NO2","NO3","NO4","NO5")

database_hourly_nordpool_filename <- "data/database_nordpool_hourly.csv"
database_daily_nordpool_filename <- "data/database_nordpool_daily.csv"



today <- as.IDate(Sys.time())

tomorrow <- today+1


tomorrow_dt <- basic_nordpoolAPI_to_dt(areas=areas) # always day-ahead!

daily_tomorrow_dt <- tomorrow_dt[,list(price=mean(price)),by=.(area,date)]


# Writing to file
prev_datebase_hourly <- fread(database_hourly_nordpool_filename)
prev_datebase_daily <- fread(database_daily_nordpool_filename)
prev_dates <- prev_datebase_daily[,unique(date)]

if(tomorrow%in%prev_dates){
  warning("Data for current date already exists! Overwriting rather than appending data.")

  prev_datebase_daily <- prev_datebase_daily[date!=tomorrow]
  daily_tomorrow_dt <- rbind(prev_datebase_daily,daily_tomorrow_dt)
  setkey(daily_tomorrow_dt,date,area)
  fwrite(daily_tomorrow_dt,database_daily_nordpool_filename)

  prev_datebase_hourly <- prev_datebase_hourly[date!=tomorrow]
  tomorrow_dt <- rbind(prev_datebase_hourly,tomorrow_dt)
  setkey(tomorrow_dt,date,area)
  fwrite(tomorrow_dt,database_hourly_nordpool_filename)

} else {
  fwrite(daily_tomorrow_dt,database_daily_nordpool_filename,append = T)
  fwrite(tomorrow_dt,database_hourly_nordpool_filename,append = T)
}

