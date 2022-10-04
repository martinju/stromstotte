
library(data.table)

areas <- c("NO1","NO2","NO3","NO4","NO5")

first_date <- as.IDate("2021-11-01")


dt_list <- list()
for(j in seq_along(areas)){
  tmp <- fread(paste0("raw-data/daily_prices_",areas[j],"_nordpool.csv"),header=T,dec=",")
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

tmp <- dt_daily[date=="2022-09-30"]
tmp[,date:=date+1]
tmp[,price:=c(907.64,	907.64,	552.24,	322.07,	921.6)]

tmp2<-copy(tmp)
tmp2[,date:=date+1]
tmp2[,price:=c(1775.57,	1775.57,	899.84,	455.14,	1775.57
)]

tmp3<-copy(tmp2)
tmp3[,date:=date+1]
tmp3[,price:=c(2338.86,	2338.86,	616.71,	504.17,	2338.86
)]

dt_daily <- rbind(dt_daily,tmp,tmp2,tmp3)

dt_daily[area!="NO4",price:=price*1.25] # Add mva (not for NO4 in Tromso)
dt_daily[,price:=price/1000] # Convert to kWh

setcolorder(dt_daily,c("area","date"))
setorder(dt_daily,date,area)

fwrite(dt_daily,"data/database_nordpool_daily.csv")
fwrite(dt_daily,"data/database_ffail_daily.csv")


