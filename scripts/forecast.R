# 3 []load model and daily data and use these to forecast the rest of the month

library(data.table)
library(forecast)

predict_based_on_past_k_days <- 31
k_sim <- 10^4
bootstrap_residuals <- FALSE

compensation_prop <- 0.90
compensation_threshold <- 0.70*1.25

areas <- c("NO1","NO2","NO3","NO4","NO5")
CI_ints <- c(0.99,0.95,0.9,0.8,0.5)
today <- as.IDate(Sys.time())
seed = 12345

database_filename <- "data/database_nordpool_daily.csv"
model_filename <- "models/model_list.RData"

current_density_compensation_filename <- "data/current_estimated_compensation_density.csv"
current_compensation_filename <- "data/current_estimated_compensation.csv"
historic_compensation_filename <- "data/historic_estimated_compensation.csv"

### Input

source("source/funcs.R")

load(model_filename)

database_daily <- fread(database_filename)

setkey(database_daily,"area","date")

tomorrow <- today+1

if(database_daily[date==tomorrow,.N]==0){
  stop("Data for tomorrow not recorded!")
}


this_month <- month(tomorrow)
this_year <- year(tomorrow)
this_day <- mday(tomorrow)

days_this_month <- lubridate::days_in_month(tomorrow)

remaining_days <- days_this_month-this_day

first_day_month <- as.IDate(paste0(this_year,"-",this_month,"-01"))
last_day_month <- as.IDate(paste0(this_year,"-",this_month,"-",days_this_month))




####

this_month_dt0 <- data.table(area=rep(areas,each=days_this_month),
                             date=rep(seq(first_day_month,last_day_month,1)))
this_month_dt <- merge(this_month_dt0,database_daily[date<=tomorrow],all.x=T,all.y=F,by=c("area","date"))

this_month_dt[,wday:=date_to_wday_factor(date)]



wday_numeric_future <- model.matrix(~wday,data=this_month_dt[is.na(price)])

prediction_dt <- database_daily[date>tomorrow-predict_based_on_past_k_days & date<=tomorrow]


prediction_dt[,wday:=date_to_wday_factor(date)]

wday_numeric_list = list()
prediction_dt_list <- list()
pred_mod_list <- list()
for(j in seq_along(areas)){
  this_mod <- mod_list[[which(names(mod_list)==areas[j])]]

  prediction_dt_list[[j]] <- prediction_dt[area==areas[j]]

  wday_numeric_list[[j]] <- model.matrix(~wday,data=prediction_dt_list[[j]])

  pred_mod_list[[j]] <- forecast::Arima(y=prediction_dt_list[[j]]$price,model=this_mod,xreg = wday_numeric_list[[j]][,-1])

}


current_mean_dt <- this_month_dt[,list(mean_price = mean(price,na.rm=T)),by=area]
current_mean_vec <- current_mean_dt[,mean_price]
names(current_mean_vec) <- current_mean_dt[,area]


samp_price_mat <- matrix(0,ncol=length(areas),nrow=k_sim)
colnames(samp_price_mat)<- areas


set.seed(seed)
if(remaining_days>0){
  for(j in seq_along(areas)){
    for(i in 1:k_sim){


      samps <- simulate(pred_mod_list[[j]],nsim=remaining_days,future=TRUE,bootstrap=bootstrap_residuals,xreg=wday_numeric_future[,-1])
      samp_price_mat[i,j] <- mean(pmax(0,samps))
    }
  }
}



estimated_meanprice_mat <- t(current_mean_vec*(this_day/days_this_month)+t(samp_price_mat*(remaining_days/days_this_month)))
colnames(estimated_meanprice_mat) <- areas

estimation_dt <- melt(as.data.table(estimated_meanprice_mat),measure.vars = areas,variable.name = "area",value.name="mean_price")

estimation_dt[,compensation := compensation_func(avgprice = mean_price,
                                                 compensation_threshold = compensation_threshold,
                                                 compensation_prop = compensation_prop)]


pos_comp <- function(x){mean(x>0)}
pos_comp_dt <- estimation_dt[,lapply(.SD,pos_comp),.SDcols=c("mean_price","compensation"),by=area]
pos_comp_dt[,type:="pos_comp"]



means_dt <- estimation_dt[,lapply(.SD,mean),.SDcols=c("mean_price","compensation"),by=area]
means_dt[,type:="mean"]

medians_dt <- estimation_dt[,lapply(.SD,median),.SDcols=c("mean_price","compensation"),by=area]
medians_dt[,type:="median"]

quants <- sort(c((1-CI_ints)/2,1-(1-CI_ints)/2))
quants_dt <- estimation_dt[,lapply(.SD,quantile,probs = quants),.SDcols=c("mean_price","compensation"),by=area]
quants_dt[,type:=rep(paste0("quantile_",quants),times=length(areas))]


lower_bound_dt <- as.data.table(current_mean_vec*(this_day/days_this_month),keep.rownames = T)
names(lower_bound_dt) <- c("area","mean_price")
lower_bound_dt[,compensation := compensation_func(avgprice = mean_price,
                                                  compensation_threshold = compensation_threshold,
                                                  compensation_prop = compensation_prop)]
lower_bound_dt[,type:="lower_bound"]

current_monthly_mean_dt <- as.data.table(current_mean_vec,keep.rownames = T)
names(current_monthly_mean_dt) <- c("area","mean_price")
current_monthly_mean_dt[,compensation := compensation_func(avgprice = mean_price,
                                                           compensation_threshold = compensation_threshold,
                                                           compensation_prop = compensation_prop)]
current_monthly_mean_dt[,type:="current_mean"]


res_dt <- rbind(pos_comp_dt,means_dt,medians_dt,quants_dt,lower_bound_dt,current_monthly_mean_dt)
res_dt[,estimation_date:=today]
res_dt[,computation_year:=data.table::year(tomorrow)]
res_dt[,computation_month:=data.table::month(tomorrow)]


density_mean_price_dt <- estimation_dt[,get_density(mean_price,adjust=2),by=area]
density_mean_price_dt[,type:="mean_price"]

density_compensation_dt <- estimation_dt[,get_density(compensation,adjust=2),by=area]
density_compensation_dt[,type:="compensation"]

density_dt <- rbind(density_mean_price_dt,density_compensation_dt)
density_dt[,estimation_date:=today]
density_dt[,computation_year:=data.table::year(tomorrow)]
density_dt[,computation_month:=data.table::month(tomorrow)]

setkey(res_dt,estimation_date,area)

fwrite(density_dt,current_density_compensation_filename)
fwrite(res_dt,current_compensation_filename)


prev_historic_compensation <- fread(historic_compensation_filename)
prev_estimation_dates <- prev_historic_compensation[,unique(estimation_date)]
if(today%in%prev_estimation_dates){
  warning("Computation already done for current date! Updating rather than appending historic data.")

  prev_historic_compensation <- prev_historic_compensation[estimation_date!=today]

  res_dt <- rbind(prev_historic_compensation,res_dt)

  setkey(res_dt,estimation_date,area)

  fwrite(res_dt,historic_compensation_filename)

} else {
  fwrite(res_dt,historic_compensation_filename,append=T)
}


