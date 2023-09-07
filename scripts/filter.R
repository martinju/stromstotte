# 3 NEW []Filter the prices based on the direct hourly compensation valid from sept 2023.

library(data.table)
library(forecast)


compensation_prop <- 0.90
compensation_threshold <- 0.70*1.25

today <- as.IDate(Sys.time())

database_filename <- "data/database_nordpool_hourly.csv"
historic_filtered_prices_filename <- "data/historic_filtered_prices_sept23_system.csv"
historic_filtered_prices_filename_json <- "data/historic_filtered_prices_sept23_system.json"

### Input

source("source/funcs.R")

database_hourly <- fread(database_filename)

setkey(database_hourly,"area","date")

tomorrow <- today+1

if(database_hourly[date==tomorrow,.N]==0){
  stop("Data for tomorrow not recorded!")
}



if(tomorrow>=as.Date("23-09-01")){
  filtered_dt <- database_hourly[date==tomorrow]

  filtered_dt[,compensation:=compensation_func(avgprice = price,
                                               compensation_threshold = compensation_threshold,
                                               compensation_prop = compensation_prop)]
  filtered_dt[,filtered_price:=price-compensation]

  ####
  prev_historic_filtered_prices <- fread(historic_filtered_prices_filename)
  prev_dates <- prev_historic_filtered_prices[,unique(date)]
  if(tomorrow%in%prev_dates){
    warning("Computation already done for current date! Updating rather than appending historic data.")

    prev_historic_filtered_prices <- prev_historic_filtered_prices[date!=tomorrow]

    filtered_dt <- rbind(prev_historic_filtered_prices,filtered_dt)

    setkey(filtered_dt,date,area)

    fwrite(filtered_dt,historic_filtered_prices_filename)

  } else {
    fwrite(filtered_dt,historic_filtered_prices_filename,append=T)
  }

  new_filtered_dt <- fread(historic_filtered_prices_filename)

  jsonlite::write_json(new_filtered_dt,historic_filtered_prices_filename_json)


} else {
  stop("The direct spot price compensation system is not in force for this date")
}





