

dt_hourly <- fread("data/database_nordpool_hourly.csv")
dt_filtered_prices <- fread("data/historic_filtered_prices_sept23_system.csv")

dt_comp <- fread("data/historic_estimated_compensation.csv")


dt_comp[,last_est_date_per_month:=max(estimation_date),by=.(computation_year,computation_month)]
dt_comp_subset <- dt_comp[estimation_date==last_est_date_per_month & type=="median" & estimation_date <="2023-09-01"][,.(area,compensation,computation_year,computation_month)]


dt_hourly[,computation_year:=year(date)]
dt_hourly[,computation_month:=month(date)]

dt_historic_filtered_prices_format <- merge(dt_comp_subset,dt_hourly,by=c("area","computation_year","computation_month"))

dt_historic_filtered_prices_format[,filtered_price:=price-compensation]
dt_historic_filtered_prices_format[,computation_month :=NULL]
dt_historic_filtered_prices_format[,computation_year :=NULL]

setcolorder(dt_historic_filtered_prices_format,c("area","date","start_hour","price","compensation","filtered_price"))

fwrite(dt_historic_filtered_prices_format,"data/historic_filtered_prices_original_system.csv")

