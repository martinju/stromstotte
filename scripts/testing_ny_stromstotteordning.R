
library(data.table)
library(ggplot2)


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

(prop_of_price_compensated <- dt_hourly[area=="NO1",mean(price>=org_comp_thres)])
#0.7955077

dt_hourly[,org_comp:=compfunc(mean_price,org_comp_thres,org_comp_prop)]
dt_hourly[,org_real_price:=price-org_comp]

new_comp_thres1 <- 0.70*1.25
new_comp_prop1 <- 0.90

dt_hourly[,new_comp1:=compfunc(price,new_comp_thres1,new_comp_prop1)]
dt_hourly[,new_real_price1:=price-new_comp1]

new_comp_thres2 <- 0.70*1.25
new_comp_prop2 <- 0.90*0.894451 # kompensasjonsgrad 0.80 istedet for 0.90

dt_hourly[,new_comp2:=compfunc(price,new_comp_thres2,new_comp_prop2)]
dt_hourly[,new_real_price2:=price-new_comp2]

new_comp_thres3 <- 0.70*1.2424317*1.25 # Innslag på 87 ør før moms (109 øre etter moms)
new_comp_prop3 <- 0.90

dt_hourly[,new_comp3:=compfunc(price,new_comp_thres3,new_comp_prop3)]
dt_hourly[,new_real_price3:=price-new_comp3]




dt_hourly[area=="NO1",.(tot_org_comp=sum(org_comp),
             sd_org_comp=sd(org_real_price),
             tot_new_comp1=sum(new_comp1),
             sd_new_comp1=sd(new_real_price1),
             tot_new_comp2=sum(new_comp2),
             sd_new_comp2=sd(new_real_price2),
             tot_new_comp3=sum(new_comp3),
             sd_new_comp3=sd(new_real_price3))]

dt_hourly[,tp:=as.POSIXct(date)+start_hour*60*60]

dt_plot <- melt(dt_hourly[,.(area,
                        tp,
                        price,
                        month,
                        org_real_price,
                        new_real_price1,
                        new_real_price2,
                        new_real_price3)], id.vars = c("area","month","tp"))


ggplot(dt_plot,aes(x=tp,y=value,col=variable))+geom_line()+
  facet_wrap(vars(area))


ggplot(dt_plot[area=="NO1"],aes(x=tp,y=value,col=variable))+geom_line()+
facet_wrap(vars(month),scales = "free_x",ncol = 1)



dt_timesforbruk <- fread("../../Div/Debattinnlegg/timesforbruksdata_csv.csv",dec = ",")
dt_dagsforbruk <- fread("../../Div/Debattinnlegg/Daglig-forbruk-husholdning.csv")
dt_dagsforbruk <- dt_dagsforbruk[Gruppe=="Husholdning"]

names(dt_dagsforbruk)[1] <- "Antall_maalepunkter"
dt_dagsforbruk[,avg_forbruk_kWh:=`Volum (MWh)`/`Antall_maalepunkter`*1000]
dt_dagsforbruk[,dato:=as.IDate(Bruksdogn,format="%d.%m.%Y")]
dt_dagsforbruk[,is_weekend:=(wday(dato) %in% c(1,7))]

dt_timesforbruk_long <- rbind(dt_timesforbruk[,.(Time,andel_per_time=Prosentfordeling_helg,is_weekend=TRUE)],
                              dt_timesforbruk[,.(Time,andel_per_time=Prosentfordeling_ukedag,is_weekend=FALSE)])

dt_forbruk <- merge(dt_dagsforbruk[dato>="2022-10-01",.(Prisomraade,dato,avg_forbruk_kWh,is_weekend)],dt_timesforbruk_long,by="is_weekend",allow.cartesian = T)

setcolorder(dt_forbruk,c("Prisomraade","Time","dato"))
dt_forbruk[,forbruk_per_time:=avg_forbruk_kWh*andel_per_time]
dt_forbruk[,tp:=as.POSIXct(dato)+(Time-1)*60*60]


ggplot(dt_forbruk[tp>"2022-12-10"],aes(x=tp,y=forbruk_per_time))+geom_line()+facet_wrap(vars(Prisomraade))


dt_time_egen1 <- fread("../../Div/Debattinnlegg/meteringvalues-mp-707057500039947698-consumption-20230118T1015.csv",dec=",")
dt_time_egen2 <- fread("../../Div/Debattinnlegg/meteringvalues-mp-707057500039947698-consumption-20230118T1016(1).csv",dec=",")
dt_time_egen3 <- fread("../../Div/Debattinnlegg/meteringvalues-mp-707057500039947698-consumption-20230118T1016.csv",dec=",")

dt_time_egen <- rbind(dt_time_egen1,
                      dt_time_egen2,
                      dt_time_egen3)
dt_time_egen[,dato:=as.IDate(Fra,format="%d.%m.%Y")]
dt_time_egen[,time:=hour(as.ITime(Fra,format="%d.%m.%Y %H:%M"))]
dt_time_egen[,forbruk := as.numeric(sub(",", ".",x = `KWH 60 Forbruk`, fixed=TRUE))]

dt_time_egen[,tp:=as.POSIXct(dato) + time*60*60]

dt_avg_time_egen <- dt_time_egen[,sum(forbruk)/dt_time_egen[,sum(forbruk)],by=time]
dt_avg_time_egen2 <- dt_time_egen[,mean(forbruk),by=time]


ggplot(dt_avg_time_egen,aes(x=time,y=V1))+geom_line()
ggplot(dt_avg_time_egen2,aes(x=time,y=V1))+geom_line()+ylim(c(0,4.2))


ggplot(dt_time_egen[tp>"2022-12-10"],aes(x=tp,y=forbruk))+geom_line()


#### Computing cost with different compensation methods:

dt_cost <- merge(dt_hourly[,.(area,tp,org_comp,org_real_price,
                              new_comp1,new_real_price1,
                              new_comp2,new_real_price2,
                              new_comp3,new_real_price3)],
                 dt_forbruk[,.(area=Prisomraade,tp,forbruk_per_time)],
                 by=c("area","tp"))
dt_cost[,consumer_cost_org := org_real_price*forbruk_per_time]
dt_cost[,consumer_cost_new1 := new_real_price1*forbruk_per_time]
dt_cost[,consumer_cost_new2 := new_real_price2*forbruk_per_time]
dt_cost[,consumer_cost_new3 := new_real_price3*forbruk_per_time]

dt_cost[,tot_comp_org := org_comp*forbruk_per_time]
dt_cost[,tot_comp_new1 := new_comp1*forbruk_per_time]
dt_cost[,tot_comp_new2 := new_comp2*forbruk_per_time]
dt_cost[,tot_comp_new3 := new_comp3*forbruk_per_time]


dt_cost[,lapply(.SD,sum),.SDcols=c("consumer_cost_org","consumer_cost_new1","consumer_cost_new2","consumer_cost_new3",
                                   "tot_comp_org","tot_comp_new1","tot_comp_new2","tot_comp_new3"),by=.(month(tp),area)]
dt_cost[area=="NO1",lapply(.SD,sum),.SDcols=c("consumer_cost_org","consumer_cost_new1","consumer_cost_new2","consumer_cost_new3",
                                              "tot_comp_org","tot_comp_new1","tot_comp_new2","tot_comp_new3")]

(compensation_reduction_factor_org_new1 <- dt_cost[area=="NO1",sum(tot_comp_org)]/dt_cost[area=="NO1",sum(tot_comp_new1)])
# 0.894451


dt_cost_melt <-  melt(dt_cost[,.(area,
                             tp,
                             consumer_cost_org,
                             consumer_cost_new1,
                             consumer_cost_new2,
                             consumer_cost_new3)], id.vars = c("area","tp"))


ggplot(dt_cost_melt,aes(x=tp,y=value,col=variable))+geom_line()+
  facet_wrap(vars(area))


ggplot(dt_cost_melt[area=="NO1"],aes(x=tp,y=value,col=variable))+geom_line()+
  facet_wrap(vars(month(tp)),scales = "free_x",ncol = 1)




