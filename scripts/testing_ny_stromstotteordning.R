
area <- "NO1"

org_comp_thres= 0.7*1.25
org_comp_prop = 0.90

dt_hourly <- fread("data/database_nordpool_hourly.csv")
dt_comp <- fread("data/historic_estimated_compensation.csv")

dt_comp[,max_estimation_date:=max(estimation_date),by=.(computation_year,computation_month)]
meanprice_per_area <- dt_comp[estimation_date==max_estimation_date & type=="mean" & computation_year=="2022", .(area,mean_price,year=computation_year,month=computation_month)]

compfunc <- function(avgprice,compensation_threshold,compensation_prop){
  ifelse(avgprice<=compensation_threshold,
         yes = avgprice*0,
         no = (avgprice-compensation_threshold)*compensation_prop)

}

dt_hourly[,year:=year(date)]
dt_hourly[,month:=month(date)]

dt_hourly <- dt_hourly[meanprice_per_area,on=c("area","year","month")]

dt_hourly <- dt_hourly[year=="2022" & month>=10]
dt_hourly[,org_comp:=compfunc(mean_price,org_comp_thres,org_comp_prop)]
dt_hourly[,org_real_price:=price-org_comp]

new_comp_thres1 <- 0.70*1.25
new_comp_prop1 <- 0.90

dt_hourly[,new_comp1:=compfunc(price,org_comp_thres,org_comp_prop)]
dt_hourly[,new_real_price1:=price-new_comp1]


dt_hourly[,.(tot_org_comp=sum(org_comp),
             sd_org_comp=sd(org_real_price),
             tot_new_comp1=sum(new_comp1),
             sd_new_comp1=sd(new_real_price1)),by=.(year,month)]

dt_hourly[,tp:=as.POSIXct(date)+start_hour*60*60]

dt_plot <- melt(dt_hourly[,.(area,
                        tp,
                        price,
                        month,
                        org_real_price,
                        new_real_price1)], id.vars = c("area","month","tp"))

library(ggplot2)

ggplot(dt_plot,aes(x=tp,y=value,col=variable))+geom_line()+
  facet_wrap(vars(area))


ggplot(dt_plot[area=="NO1"],aes(x=tp,y=value,col=variable))+geom_line()+
facet_wrap(vars(month),scales = "free_x",ncol = 1)


