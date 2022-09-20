
# Schedule this to run at 13.15 every day

library(data.table)
library(lubridate)

playground = TRUE
use_past_k_days <- 14
k_bootstrap <- 10^5
compensation_prop <- 0.90
compensation_threshold <- 0.70*1.25
areas <- c("NO1","NO2")


# What I really should do here is to get data from the previous year (daily proces).
# Then do a basic autocorrelation plot of this
# Then, I can weight the daily observations according to this

if(playground){
  database <- data.table::fread("database_fake.csv",tz = "") # SOME BUG HERE ON READING TIME ZONE ETC
}
database[,to:=as.POSIXct(to,tz="CEST")] #### FIX THIW !!!!!!!!!!!!!


today <- as.Date(Sys.time())
tomorrow <- today+1


### currently just doing this for a single area
tomorrow_dt <- NULL
for(j in seq_along(areas)){
  if(playground){
    file = paste0("https://playground-norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=123")
  } else {
    file = paste0("https://norway-power.ffail.win/?zone=",areas[j],"&date=",tomorrow,"&key=",key)
  }

  dat <- path_to_dt(file)
  tomorrow_dt <- rbind(tomorrow_dt,dat)

}

if(playground){
  tomorrow_dt[,from:=from+24*60*60]
  tomorrow_dt[,to:=to+24*60*60]
}

database <- rbind(database,tomorrow_dt)


tomorrow_POSIXct <- as.POSIXct(tomorrow,tz="UTC")
sampling_dt <- database[from>=tomorrow_POSIXct-(use_past_k_days-1)*24*60*60]



this_month <- lubridate::month(tomorrow)
this_year <- lubridate::year(tomorrow)
this_day <- lubridate::day(tomorrow)

days_this_month <- lubridate::days_in_month(tomorrow)

reamining_days <- days_this_month-this_day


first_day_month <- as.POSIXct(paste0(this_year,"-",this_month,"-01"))
last_day_month <- as.POSIXct(paste0(this_year,"-",this_month,"-",days_this_month))


this_month_dt <- database[from>=first_day_month & from <=last_day_month ]

current_mean_dt <- this_month_dt[,list(mean_price = mean(price)),by=area]
current_mean_vec <- current_mean_dt[,mean_price]
names(current_mean_vec) <- current_mean_dt[,area]


set.seed(123)
samp_price_mat <- matrix(NA,ncol=length(areas),nrow=k_bootstrap)
for(j in seq_along(areas)){

  prices <- sampling_dt[area==areas[j],price]

  ### CONTINUE FIXING BELOW HERE, calling lapply on the below function k_boostrap times
  samp_price_mat[,j] <- sapply(seq_len(k_bootstrap),meanfunc,prices=prices,remaining_days=remaining_days)

}
quants <- c(0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99)

quant_samp_price <- apply(samp_price_mat,MARGIN = 2,quantile,probs = quants)
mean_samp_price <- colMeans(samp_price_mat)
summary_forecated_price <- rbind(quant_samp_price,mean=mean_samp_price)

# Monthly mean price assuming price zero the rest of the month
min_month_mean_price <- current_mean_vec*(this_day/days_this_month)



# Quantiles
observed_contrib <- matrix(rep(current_mean_vec,each=nrow(summary_forecated_price)),ncol=2)*(this_day/days_this_month)
forecasted_contrib <- summary_forecated_price*(reamining_days/days_this_month)

estimated_month_mean_price <- observed_contrib+forecasted_contrib
colnames(estimated_month_mean_price) <- areas

estimated_compensation_dt <- as.data.table(compensation_func(estimated_month_mean_price,
                                                             compensation_threshold = compensation_threshold,
                                                             compensation_prop = compensation_prop),keep.rownames = T)
tomorrow_dt_plot <- copy(tomorrow_dt)
#tomorrow_dt_plot[,from_time:=as.ITime(from)]
#tomorrow_dt_plot[,from:=NULL]
#tomorrow_dt_plot[,to:=NULL]


plot_dt <- copy(tomorrow_dt_plot)
plot_dt[,type:="original"]
for(j in seq_along(areas)){
  for(i in seq_len(nrow(estimated_compensation_dt))){
    this_row <- estimated_compensation_dt[i,rn]
    compensation <- estimated_compensation_dt[i,get(areas[j])]
    tmp <- tomorrow_dt_plot[area==areas[j]][,price:=price-compensation]
    tmp[,type:=this_row]

    plot_dt <- rbind(plot_dt,tmp)
  }
}

plot_dt_CI80 <- copy(plot_dt[type=="90%"])
plot_dt_CI80[,price_upper:=price]
plot_dt_CI80[,price_lower:=plot_dt[type=="10%",price]]
plot_dt_CI80[,price:=NULL]


library(ggplot2)
library(scales)
plot_list <- list()
for(j in seq_along(areas)){

  tmp <- ggplot(mapping=aes(x=from))+
    geom_step(data=plot_dt[area==areas[j]&type=="original"],mapping=aes(y=price))+
    geom_step(data=plot_dt[area==areas[j]&type=="mean"],col=2,linetype=2,mapping=aes(y=price))+
    geom_ribbon(data=plot_dt_CI80[area==areas[j]],mapping=aes(ymin=price_lower,ymax=price_upper))+
    scale_x_datetime(labels = date_format("%H:%M",tz="Europe/Berlin"))

  plot_list[[j]] <- tmp
}


