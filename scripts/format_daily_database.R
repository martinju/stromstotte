
areas <- c("NO2","NO1")

first_date <- as.IDate("2021-11-01")


dt_list <- list()
for(j in seq_along(areas)){
  tmp <- fread(paste0("daily_prices_",areas[j],"_nordpool.csv"),header=T,dec=",")
  tmp[,area:=areas[j]]
  tmp[,V1:=NULL]
  setcolorder(tmp,rev(names(tmp)))
  tmp_melt <-
  dt_list[[j]] <- melt(tmp,id.vars="area",value.name = "price")
  dt_list[[j]] <- dt_list[[j]][!is.na(price)]
  dt_list[[j]][,date:=seq(from=first_date,by=1,length.out=.N)]
  dt_list[[j]][,variable:=NULL]
}

dt_daily <- rbindlist(dt_list)

dt_daily[,price:=price*1.25/1000] # ADD MVA and convert to kWh


setcolorder(dt_daily,c("area","date"))

fwrite(dt_daily,"database_daily_nordpool.csv")

