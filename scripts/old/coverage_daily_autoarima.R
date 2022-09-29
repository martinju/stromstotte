

library(data.table)
library(lubridate)

source("funcs.R")

k_sim <- 10^3
areas <- c("NO2") # Just one area at a time
use_past_k_days <- 31
plot_CI_ints <- c(0.99,0.95,0.9,0.8,0.5)
plot_avg_measures <- c("mean","median")
database_filename <- "database_daily_nordpool.csv"
today_vec <- seq(as.IDate("2022-01-31"),as.IDate("2022-08-30"),by=1) #as.IDate(Sys.time())
seed = 12345



database <- data.table::fread(database_filename)
database <- database[area==areas] # Just one area at a time
mod <- forecast::auto.arima(database$price,trace = T,approximation = F)
plot(database$date,database$price,type="l")
points(database$date,database$price)
lines(database$date,mod$fitted,col=2)
points(database$date,mod$fitted,col=2)


mean((mod$fitted-mod$x)^2)

res_mat <- matrix(NA,ncol=length(plot_CI_ints)*2+length(plot_avg_measures),nrow=length(today_vec))
for(ii in seq_along(today_vec)){
  today <- today_vec[ii]
  tomorrow <- today+1

  sampling_dt <- database[date>tomorrow-use_past_k_days & date<=tomorrow]



  this_month <- data.table::month(tomorrow)
  this_year <- data.table::year(tomorrow)
  this_day <- data.table::mday(tomorrow)

  days_this_month <- lubridate::days_in_month(tomorrow)

  remaining_days <- days_this_month-this_day


  first_day_month <- as.IDate(paste0(this_year,"-",this_month,"-01"))
  last_day_month <- as.IDate(paste0(this_year,"-",this_month,"-",days_this_month))


  this_month_dt <- database[date>=first_day_month & date <=last_day_month]

  current_mean_dt <- this_month_dt[,list(mean_price = mean(price)),by=area]
  current_mean_vec <- current_mean_dt[,mean_price]
  names(current_mean_vec) <- current_mean_dt[,area]

  this_mod <- forecast::Arima(y=sampling_dt$price,model=mod)
  set.seed(seed)
#  plot(sampling_dt$date,sampling_dt$price,type="l",xlim=c(as.IDate("2022-01-06"),as.IDate("2022-02-28")),ylim=c(0,5))
  samp_price_mat <- matrix(0,ncol=length(areas),nrow=k_sim)

  if(remaining_days>0){
    for(i in 1:k_sim){
      samps <- simulate(this_mod,nsim=remaining_days,future=TRUE,bootstrap=TRUE)
      #   lines(tail(this_month_dt[,date],remaining_days),samps,col=2)
      samp_price_mat[i,] <- mean(pmax(0,samps))
      #    lines(tail(this_month_dt[,date],remaining_days),rep(meanval,remaining_days),col=3)
    }
  }


  estimated_meanprice <- NULL

  if(!is.null(plot_avg_measures)){
    if("mean" %in% plot_avg_measures){
      mean_samp_price <- colMeans(samp_price_mat)
      mean_estimated_meanprice <- current_mean_vec*(this_day/days_this_month)+mean_samp_price*(remaining_days/days_this_month)
      estimated_meanprice <- cbind(estimated_meanprice,mean=mean_estimated_meanprice) # Store this

    }

    if("mean" %in% plot_avg_measures){
      median_samp_price <- apply(samp_price_mat,MARGIN = 2,median)
      median_estimated_meanprice <- current_mean_vec*(this_day/days_this_month)+median_samp_price*(remaining_days/days_this_month)
      estimated_meanprice <- cbind(estimated_meanprice,median=median_estimated_meanprice) # Store this

    }
  }


  if(!is.null(plot_CI_ints)){
    for(i in seq_along(plot_CI_ints)){
      CI_int <- plot_CI_ints[i]
      probs <- c((1-CI_int)/2,CI_int+(1-CI_int)/2)

      CI_samp_price <- t(apply(samp_price_mat,MARGIN = 2,quantile,probs = probs))


      CI_estimated_meanprice <- current_mean_vec*(this_day/days_this_month)+CI_samp_price*(remaining_days/days_this_month)
      estimated_meanprice <- cbind(estimated_meanprice,CI_estimated_meanprice) # Store this



    }
  }







#####################
  res_mat[ii,] <- estimated_meanprice

  print(ii)

}



res_dt <- as.data.table(res_mat)
names(res_dt) <- colnames(estimated_meanprice)

res_dt[,date:=today_vec+1]
res_dt[,month:=month(date)]
res_dt[,year:=year(date)]
res_dt[,mday:=mday(date)]
res_dt[,days_in_month:=lubridate::days_in_month(date)]
res_dt[,remaining_days:=days_in_month-mday]
res_dt[remaining_days>=1 & remaining_days<=5,rem_days_group:="1-5"]
res_dt[remaining_days>=6 & remaining_days<=10,rem_days_group:="6-10"]
res_dt[remaining_days>=11 & remaining_days<=15,rem_days_group:="11-15"]
res_dt[remaining_days>=16 & remaining_days<=20,rem_days_group:="16-20"]
res_dt[remaining_days>=21 & remaining_days<=25,rem_days_group:="21-25"]
res_dt[remaining_days>=26, rem_days_group:=">26"]


datebase0 <- data.table::fread(database_filename)
datebase0 <- datebase0[area == areas] # Just one area at a time
datebase0[,month:=month(date)]
datebase0[,year:=year(date)]
actual_avg_dt <- datebase0[,list(actual_avgprice=mean(price)),by=.(month,year)]

res_dt <- merge(res_dt,actual_avg_dt,by=c("month","year"))
res_dt[,month:=NULL]
res_dt[,year:=NULL]

setcolorder(res_dt,c("date","actual_avgprice"))

coverage_dt_list <- list()
for(ii in seq_along(plot_CI_ints)){
  CI_int <- plot_CI_ints[ii]
  probs <- c((1-CI_int)/2,CI_int+(1-CI_int)/2)
  colnames <- paste0(probs*100,"%")
  actual_coverage0 <- res_dt[,list(actual_coverage=mean(get(colnames[1])<actual_avgprice & actual_avgprice<get(colnames[2]))),by=rem_days_group]
  actual_coverage0[,supposed_level:=CI_int]
  coverage_dt_list[[ii]] <- actual_coverage0
}
coverage_dt <- rbindlist(coverage_dt_list)

res_dt[,sqrt(mean((actual_avgprice-mean)^2)),by=rem_days_group]
res_dt[,mean(abs(actual_avgprice-mean)),by=rem_days_group]

coverage_dt[]
# bootstrap=FALSE
#supposed_level actual_coverage
#1:           0.99       0.9481132
#2:           0.95       0.9103774
#3:           0.90       0.8537736
#4:           0.80       0.8207547
#5:           0.50       0.7075472




