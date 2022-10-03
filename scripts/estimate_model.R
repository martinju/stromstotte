# 1. Estimate model

library(data.table)

areas <- c("NO1","NO2","NO3","NO4","NO5")#,"NO1")#c("NO1","NO2")
database_filename <- "data/database_nordpool_daily.csv"
seed = 12345
model_training_range <- c(as.IDate("2021-11-01"),as.IDate("2022-08-31"))
model_filename <- "models/model_list.RData"

source("source/funcs.R")


database <- data.table::fread(database_filename)
model_training_dt <- database[date>=model_training_range[1] & date<=model_training_range[2]]
model_training_dt[,wday:=date_to_wday_factor(date)]


mod_list <- list()
for(j in seq_along(areas)){

  this_model_training_dt <- model_training_dt[area==areas[j]]

  wday_numeric <- model.matrix(~wday,data=this_model_training_dt)

  mod_list[[j]] <- forecast::auto.arima(this_model_training_dt$price,xreg=wday_numeric[,-1],trace = T,approximation = F)

}

names(mod_list) <- areas

save(mod_list,file=model_filename)
