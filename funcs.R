basic_ffail_to_dt <- function(path,area){
  dat <- rjson::fromJSON(file = path)
  date <- as.IDate(lubridate::ymd_hms(dat[[10]]$valid_from)) # Avoid time zone issues
  prices <- sapply(dat,function(x)x$NOK_per_kWh,USE.NAMES = F)

  dt <- data.table(area=area,start_hour=0:23,date=date,price=prices)

  return(dt)
}

path_to_dt <- function(path){

  dat <- rjson::fromJSON(file = path)
  all_data <- NULL
  for(k in seq_along(dat)){
    all_data <- rbind(all_data,c(dat[[k]]$NOK_per_kWh,
                                 dat[[k]]$valid_from,
                                 dat[[k]]$valid_to,
                                 area = areas[j]))
  }
  all_data = data.table::as.data.table(all_data)
  all_data[,price:=as.numeric(V1)]

  all_data[,c("from_date","from_time"):= tstrsplit(V2,"T",fixed=T)]
  all_data[,from_time:=as.ITime(from_time)]
  all_data[,from_date:=as.POSIXct(from_date)]
  all_data[,from:=from_date+from_time]

  all_data[,c("to_date","to_time"):= tstrsplit(V3,"T",fixed=T)]
  all_data[,to_time:=as.ITime(to_time)]
  all_data[,to_date:=as.POSIXct(to_date)]
  all_data[,to:=to_date+to_time]

  all_data[,c("V1","V2","V3","from_date","from_time","to_date","to_time"):=NULL]

  return(all_data)
}



meanfunc <- function(x,prices,remaining_days){
  mean(sample(x = prices,size = remaining_days*24,replace=T))
}

compensation_func <- function(avgprice,compensation_threshold,compensation_prop){
  ret <- ifelse(avgprice<=compensation_threshold,
                yes = avgprice,
                no = (avgprice-compensation_threshold)*compensation_prop)
  as.data.table(ret,keep.rownames = T)
}
