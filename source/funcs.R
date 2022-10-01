basic_ffail_to_dt <- function(path,area){
  dat <- rjson::fromJSON(file = path)
  date <- as.IDate(lubridate::ymd_hms(dat[[10]]$valid_from)) # Avoid time zone issues
  prices <- sapply(dat,function(x)x$NOK_per_kWh,USE.NAMES = F)

  dt <- data.table(area=area,start_hour=0:23,date=date,price=prices)

  return(dt)
}

basic_nordpoolAPI_to_dt <- function(path="https://www.nordpoolgroup.com/api/marketdata/page/23?currency=NOK",areas){
  dat <- rjson::fromJSON(file = path)

  area_tab <- c("NO1","NO2","NO5","NO3","NO3","NO4")
  area_nums <- match(areas,area_tab)
  price_mat <- matrix(0,nrow=24,ncol=length(area_nums))
  colnames(price_mat) <- areas
  for(j in seq_len(24)){
    for (i in seq_along(area_nums)){
      ii <- area_nums[i]
      price_mat[j,i] <- as.numeric(gsub(" ","",gsub(",",".",dat$data$Rows[[j]]$Columns[[i]]$Value,fixed=T)))
    }
  }
  price_mat <- price_mat*1.25/1000
  price_dt <- as.data.table(price_mat)

  date0 <- as.IDate(lubridate::ymd_hms(dat$data$DataStartdate)) # Avoid time zone issues
  price_dt[,date:=date0]
  price_dt[,start_hour:=0:23]

  dt <- melt(price_dt,id.vars = c("date","start_hour"),variable.name = "area",value.name = "price")

  setcolorder(dt,c("area","date","start_hour"))

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



all_indep <- function(x,prices,remaining_days){
  mean(sample(x = prices,size = remaining_days*24,replace=T))
}

double_bootstrap <- function(non,dates,sampling_dt,remaining_days){
  samp_dates <- sample(x=dates,size=remaining_days,replace=T)
  mean(sapply(samp_dates,helper))
}
helper <- function(x){
  prices <- sampling_dt[date==x,price]
  mean(sample(prices,size=24,replace=T))
}

sim_forecast <- function(x,tomorrow_model,bootstrap,remaining_days){
  samps <- simulate(tomorrow_model,nsim=remaining_days,future=TRUE,bootstrap=bootstrap)
  mean(pmax(0,samps))
}



compensation_func <- function(avgprice,compensation_threshold,compensation_prop){
  ret <- ifelse(avgprice<=compensation_threshold,
                yes = avgprice,
                no = (avgprice-compensation_threshold)*compensation_prop)
  as.data.table(ret,keep.rownames = T)
}

get_density <- function(x,adjust){
  tmp <- density(x,adjust=adjust)
  data.table(x=tmp$x,y=tmp$y)
}

