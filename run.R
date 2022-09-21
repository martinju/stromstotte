
# Schedule this to run at 13.15 every day

library(data.table)
library(lubridate)

playground = TRUE
use_past_k_days <- 31
k_bootstrap <- 10^5
compensation_prop <- 0.80
compensation_threshold <- 0.70*1.25
areas <- c("NO2","NO1")#c("NO1","NO2")
plot_CI_ints <- c(0.999,0.5)
plot_avg_measures <- c("mean","median")


# What I really should do here is to get data from the previous year (daily proces).
# Then do a basic autocorrelation plot of this
# Then, I can weight the daily observations according to this

database <- data.table::fread("datebase_nordpool.csv")


today <- as.IDate(Sys.time())-14
tomorrow <- today+1


### currently just doing this for a single area
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


set.seed(123)
samp_price_mat <- matrix(0,ncol=length(areas),nrow=k_bootstrap)
colnames(samp_price_mat)<- areas

if(remaining_days>0){
  for(j in seq_along(areas)){

    prices <- sampling_dt[area==areas[j],price]

    samp_price_mat[,j] <- sapply(seq_len(k_bootstrap),meanfunc,prices=prices,remaining_days=remaining_days)

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


# Duplicating the last hour to make plotting nicer
tomorrow_dt_plot <- rbind(tomorrow_dt_plot,tomorrow_dt_plot[start_hour==23][,start_hour:=24])

library(ggplot2)
library(scales)
plot_list <- list()
for(j in seq_along(areas)){

  gg <- ggplot(data=tomorrow_dt_plot[area==areas[j]],mapping=aes(x=start_hour))+
    geom_step(aes(y=spot_price))
  if("mean" %in% plot_avg_measures){
    gg <- gg+    geom_step(aes(y=expected_price),col=2)
  }
  if("median" %in% plot_avg_measures){
    gg <- gg+    geom_step(aes(y=median_price),col=3)
  }

  if(!is.null(plot_CI_ints)){
    for(i in seq_along(plot_CI_ints)){
      CI_int <- plot_CI_ints[i]
      ymin_col <- paste0("CI_lower_",100*CI_int,"%_price")
      ymax_col <- paste0("CI_upper_",100*CI_int,"%_price")

      gg2 <- gg + pammtools::geom_stepribbon(aes(ymin=get(ymin_col),
                                                ymax=get(ymax_col)),fill="red",alpha=0.1)


    }
  }

    geom_step(aes(y=spot_price))+
    geom_step(aes(y=spot_price))+


    geom_step(data=plot_dt[area==areas[j]&type=="mean"],col=2,linetype=2,mapping=aes(y=price))+
    geom_ribbon(data=plot_dt_CI80[area==areas[j]],mapping=aes(ymin=price_lower,ymax=price_upper))+
    scale_x_datetime(labels = date_format("%H:%M",tz="Europe/Berlin"))

  plot_list[[j]] <- tmp
}





quants <- c(0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99)

quant_samp_price <- apply(samp_price_mat,MARGIN = 2,quantile,probs = quants)
mean_samp_price <- colMeans(samp_price_mat)
summary_forecated_price <- rbind(quant_samp_price,mean=mean_samp_price)

# Monthly mean price assuming price zero the rest of the month
min_month_mean_price <- current_mean_vec*(this_day/days_this_month)



# Quantiles
observed_contrib <- matrix(rep(current_mean_vec,each=nrow(summary_forecated_price)),ncol=2)*(this_day/days_this_month)
forecasted_contrib <- summary_forecated_price*(remaining_days/days_this_month)

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



