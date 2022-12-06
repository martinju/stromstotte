
library(data.table)

areas <- c("NO1","NO2","NO3","NO4","NO5")#,"NO1")#c("NO1","NO2")
database_filename <- "data/database_nordpool_daily.csv"

database <- data.table::fread(database_filename)

par(mfrow=c(2,1))
database[area=="NO1",acf(price)]
database[area=="NO1",pacf(price)]








############# forecast plot ##############


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
today <- as.IDate(Sys.time())-1
seed = 12345

database_filename <- "data/database_nordpool_daily.csv"
model_filename <- "models/model_list.RData"

current_density_compensation_filename <- "data/current_estimated_compensation_density.csv"
current_compensation_filename <- "data/current_estimated_compensation.csv"
historic_compensation_filename <- "data/historic_estimated_compensation.csv"

current_compensation_filename_json <- "data/current_estimated_compensation.json"
historic_compensation_filename_json <- "data/historic_estimated_compensation.json"


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



wday_numeric_future <- model.matrix(~wday,data=this_month_dt[area==areas[1] & is.na(price)])

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

samp_price_list <- list()


set.seed(seed)
if(remaining_days>0){
  for(j in seq_along(areas)){
    samp_price_list[[j]] <- matrix(0,ncol=remaining_days,nrow=k_sim)
    for(i in 1:k_sim){


      samps <- simulate(pred_mod_list[[j]],nsim=remaining_days,future=TRUE,bootstrap=bootstrap_residuals,xreg=wday_numeric_future[,-1,drop=FALSE])
      samp_price_list[[j]][i,] <- pmax(0,samps)
    }
  }
}


this_month_dt[area=="NO1",price][1:6]



plot_mat <- cbind(matrix(rep(this_month_dt[area=="NO1",price][1:6],k_sim),ncol=6,byrow = T),samp_price_list[[1]])

#matplot(t(head(plot_mat,1000)),type = "l")

colnames(plot_mat) <- as.character(seq(as.Date("2022-12-01"),as.Date("2022-12-31"),by=1))
plot_mat_dt <- as.data.table(plot_mat)

plot_mat_dt[,id:=.I]
plot_dt <- melt(plot_mat_dt,id.vars = "id")
plot_dt[,variable:=as.Date(variable)]
plot_dt[,id:=as.factor(id)]


setkey(plot_dt,id)

median_plot_dt <- plot_dt[,.(value=median(value)),by=variable]
quant_l_plot_dt <- plot_dt[,.(value=quantile(value,probs=0.05)),by=variable]
quant_u_plot_dt <- plot_dt[,.(value=quantile(value,probs=0.95)),by=variable]

scaleFUN <- function(x) sprintf("%.2f", x)

library(ggplot2)
ggplot(plot_dt[id%in%1:100],aes(x=variable,y=value))+geom_line(aes(group=id),alpha=0.1)+
  geom_line(data=quant_l_plot_dt,col=3,size=1)+
  geom_line(data=quant_u_plot_dt,col=3,size=1)+
  geom_line(data=median_plot_dt,col=2,size=1)+
  scale_x_date(name = "Dag",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b")+
  scale_y_continuous(name = "Spotpris (NOK/kWh inkl. mva)",labels=scaleFUN)+theme_minimal()

plot_dt[,meanpred:=mean(value),by=id]
plot_dt[,comp:=(meanpred-0.7*1.25)*0.9]
plot_dt[,value_compensated:=value-comp]

c_median_plot_dt <- plot_dt[,.(value_compensated=median(value_compensated)),by=variable]
c_quant_l_plot_dt <- plot_dt[,.(value_compensated=quantile(value_compensated,probs=0.05)),by=variable]
c_quant_u_plot_dt <- plot_dt[,.(value_compensated=quantile(value_compensated,probs=0.95)),by=variable]


library(ggplot2)
ggplot(plot_dt[id%in%1:100],aes(x=variable,y=value_compensated))+geom_line(aes(group=id),alpha=0.1)+
  geom_line(data=c_quant_l_plot_dt,col=3,size=1)+
  geom_line(data=c_quant_u_plot_dt,col=3,size=1)+
  geom_line(data=c_median_plot_dt,col=2,size=1)+
  scale_x_date(name = "Dag",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b")+
  scale_y_continuous(name = "Spotpris (NOK/kWh inkl. mva)",labels=scaleFUN)



hist((rowMeans(plot_mat)-0.7*1.25)*0.9,main = "Estimated compensation",xlab = "NOK/kWh")







