

####



library(data.table)
library(lubridate)

source("funcs.R")

# What I really should do here is to get data from the previous year (daily proces).
# Then do a basic autocorrelation plot of this
# Then, I can weight the daily observations according to this

database <- data.table::fread(database_filename)


tomorrow <- today+1


### currently just doing this for a single area
if(read_new_data){
  tomorrow_dt <- NULL
  for(j in seq_along(areas)){
    if(playground){
      file = paste0("https://playground-norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=123")
    } else {
      file = paste0("https://norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=",key)
    }

    this_dt <- basic_ffail_to_dt(file,areas[j])
    tomorrow_dt <- rbind(tomorrow_dt,this_dt)

  }

  if(playground){
    tomorrow_dt[,date:=date+1]
  }

  database <- rbind(database,tomorrow_dt)
  database <- unique(database[area%in%areas])
} else {
  database <- unique(database[area%in%areas])
  tomorrow_dt <- database[date==tomorrow]

}




this_month <- data.table::month(tomorrow)
this_year <- data.table::year(tomorrow)
this_day <- data.table::mday(tomorrow)

days_this_month <- lubridate::days_in_month(tomorrow)

remaining_days <- days_this_month-this_day


first_day_month <- as.IDate(paste0(this_year,"-",this_month,"-01"))
last_day_month <- as.IDate(paste0(this_year,"-",this_month,"-",days_this_month))







if(method %in% c("all_indep","double_bootstrap")){
  sampling_dt <- database[date>tomorrow-use_past_k_days & date<=tomorrow]

  this_month_dt0 <- data.table(area=rep(areas,each=days_this_month*24),
                               start_hour=rep(0:23,times=days_this_month*length(areas)),
                               date=rep(seq(first_day_month,last_day_month,1),each=24))
  this_month_dt <- merge(this_month_dt0,database,all.x=T,all.y=F,by=c("area","start_hour","date"))
}

if(method=="double_bootstrap"){
  daily_avg_prices_dt <- sampling_dt[,list(price=mean(price)),by=.(date,area)]
}

if(method=="daily_auto.arima"){
  database_daily <- database[,list(price=mean(price)),by=.(area,date)]

  model_training_dt <- database_daily[date>=model_training_range[1] & date<=model_training_range[2]]

  this_month_dt0 <- data.table(area=rep(areas,each=days_this_month),
                               date=rep(seq(first_day_month,last_day_month,1)))
  this_month_dt <- merge(this_month_dt0,database_daily,all.x=T,all.y=F,by=c("area","date"))


  sampling_dt <- database_daily[date>tomorrow-use_past_k_days & date<=tomorrow]

  model_training_dt[,wday:=as.factor(wday(date))]

  wday_numeric <- model.matrix(~wday,data=model_training_dt)

  mod_daily <- forecast::auto.arima(model_training_dt$price,xreg=wday_numeric[,-1],trace = T,approximation = F)

  tomorrow_mod <- forecast::Arima(y=sampling_dt$price,model=mod)

}

current_mean_dt <- this_month_dt[,list(mean_price = mean(price)),by=area]
current_mean_vec <- current_mean_dt[,mean_price]
names(current_mean_vec) <- current_mean_dt[,area]


set.seed(seed)
samp_price_mat <- matrix(0,ncol=length(areas),nrow=k_bootstrap)
colnames(samp_price_mat)<- areas

if(remaining_days>0){
  for(j in seq_along(areas)){

    if(method=="all_indep"){
      prices <- sampling_dt[area==areas[j],price]
      samp_price_mat[,j] <- sapply(seq_len(k_bootstrap),all_indep,prices=prices,remaining_days=remaining_days)
    }
    if(method=="double_bootstrap"){
      dates <- sampling_dt[,unique(date)]
      samp_price_mat[,j] <- sapply(seq_len(k_bootstrap),double_bootstrap,dates=dates,sampling_dt=sampling_dt,remaining_days=remaining_days)
    }


    if(method=="daily_auto.arima"){


      for(i in 1:k_sim){
        samps <- simulate(this_mod,nsim=remaining_days,future=TRUE,bootstrap=TRUE)
        #   lines(tail(this_month_dt[,date],remaining_days),samps,col=2)
        samp_price_mat[i,] <- mean(pmax(0,samps))
        #    lines(tail(this_month_dt[,date],remaining_days),rep(meanval,remaining_days),col=3)
      }


      samp_price_mat[,j] <- sapply(seq_len(k_bootstrap),sim_forecast,tomorrow_mod,bootstrap=bootstrap,remaining_days=remaining_days)

    }

  }
}

tomorrow_dt_plot <- copy(tomorrow_dt)
setnames(tomorrow_dt_plot,"price","spot_price")

estimated_meanprice <- NULL

if(!is.null(plot_avg_measures)){
  if("mean" %in% plot_avg_measures){
    mean_samp_price <- colMeans(samp_price_mat)
    mean_estimated_meanprice <- current_mean_vec*(this_day/days_this_month)+mean_samp_price*(remaining_days/days_this_month)
    estimated_meanprice <- cbind(estimated_meanprice,mean=mean_estimated_meanprice) # Store this

    mean_estimated_compensation <-compensation_func(mean_estimated_meanprice,
                                                    compensation_threshold = compensation_threshold,
                                                    compensation_prop = compensation_prop)

    colname <- "expected_price"


    for(j in seq_along(areas)){
      compensation <- unlist(mean_estimated_compensation[rn==areas[j],2])
      tomorrow_dt_plot[area==areas[j],(colname):=spot_price-compensation]
    }

  }

  if("mean" %in% plot_avg_measures){
    median_samp_price <- apply(samp_price_mat,MARGIN = 2,median)
    median_estimated_meanprice <- current_mean_vec*(this_day/days_this_month)+median_samp_price*(remaining_days/days_this_month)
    estimated_meanprice <- cbind(estimated_meanprice,median=median_estimated_meanprice) # Store this

    median_estimated_compensation <-compensation_func(median_estimated_meanprice,
                                                    compensation_threshold = compensation_threshold,
                                                    compensation_prop = compensation_prop)

    colname <- "median_price"


    for(j in seq_along(areas)){
      compensation <- unlist(median_estimated_compensation[rn==areas[j],2])
      tomorrow_dt_plot[area==areas[j],(colname):=spot_price-compensation]
    }

  }
}

if(!is.null(plot_CI_ints)){
  for(i in seq_along(plot_CI_ints)){
    CI_int <- plot_CI_ints[i]
    probs <- c((1-CI_int)/2,CI_int+(1-CI_int)/2)

    CI_samp_price <- t(apply(samp_price_mat,MARGIN = 2,quantile,probs = probs))


    CI_estimated_meanprice <- current_mean_vec*(this_day/days_this_month)+CI_samp_price*(remaining_days/days_this_month)
    estimated_meanprice <- cbind(estimated_meanprice,CI_estimated_meanprice) # Store this


    CI_estimated_compensation <-compensation_func(CI_estimated_meanprice,
                                                                compensation_threshold = compensation_threshold,
                                                                compensation_prop = compensation_prop)

    colname_lower <- paste0("CI_lower_",100*CI_int,"%_price")
    colname_upper <- paste0("CI_upper_",100*CI_int,"%_price")


    for(j in seq_along(areas)){
      compensation_lower <- unlist(CI_estimated_compensation[rn==areas[j],2])
      compensation_upper <- unlist(CI_estimated_compensation[rn==areas[j],3])

      tomorrow_dt_plot[area==areas[j],(colname_lower):=spot_price-compensation_lower]
      tomorrow_dt_plot[area==areas[j],(colname_upper):=spot_price-compensation_upper]

    }
  }
}






