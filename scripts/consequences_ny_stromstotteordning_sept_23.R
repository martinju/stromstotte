
library(data.table)
library(ggplot2)
Sys.setlocale(locale='nb_NO.utf8')

# Doing everything before mva, and adding that in the end.

current_comp_thres= 0.7
current_comp_prop = 0.90

actual_comp_thres = 0.7
actual_comp_prop_per_month = c(rep(0.8,8),rep(0.9,4))


dt_hourly <- fread("data/database_nordpool_hourly_2022_no_mva.csv")

compfunc <- function(avgprice,compensation_threshold,compensation_prop){
  ifelse(avgprice<=compensation_threshold,
         yes = avgprice*0,
         no = (avgprice-compensation_threshold)*compensation_prop)

}

dt_hourly[is.na(price),price:=0]

dt_hourly[,year:=year(date)]
dt_hourly[,month:=month(date)]
dt_hourly[,mean_price:=mean(price),by=.(area,month)]


dt_hourly[,current_comp:=compfunc(mean_price,current_comp_thres,current_comp_prop)]
dt_hourly[,current_real_price:=price-current_comp]

new_comp_thres1 <- 0.70
new_comp_prop1 <- 0.90

dt_hourly[,new_comp1:=compfunc(price,new_comp_thres1,new_comp_prop1)]
dt_hourly[,new_real_price1:=price-new_comp1]

new_comp_thres2 <- 0.70
new_comp_prop2 <- 0.90*0.8935035 # kompensasjonsgrad 0.80 istedet for 0.90

dt_hourly[,new_comp2:=compfunc(price,new_comp_thres2,new_comp_prop2)]
dt_hourly[,new_real_price2:=price-new_comp2]

new_comp_thres3 <- 0.70*1.2432324 # Innslag på 87 ør før moms (109 øre etter moms)
new_comp_prop3 <- 0.90

dt_hourly[,new_comp3:=compfunc(price,new_comp_thres3,new_comp_prop3)]
dt_hourly[,new_real_price3:=price-new_comp3]

for(i in 1:12){
  dt_hourly[month==i,actual_comp:=compfunc(mean_price,actual_comp_thres,actual_comp_prop_per_month[i])]
  dt_hourly[,actual_price:=price-actual_comp]
}

cols <- names(dt_hourly)[-c(1:3,5:6)]
dt_hourly[area!="NO4",(cols):=lapply(.SD,function(x) x*1.25),.SDcols=cols]

dt_hourly[,tp:=as.POSIXct(date)+start_hour*60*60]

dt_actual_comp_prop_per_month <- data.table(month=1:12,actual_comp2=actual_comp_prop_per_month)

#dt_hourly <- merge(dt_hourly,dt_actual_comp_prop_per_month,by="month")

dt_plotprice <- dt_hourly[,.(tp,area,date,start_hour,year,month,
                            price,mean_price,current_real_price,new_real_price1,actual_price)]
setnames(dt_plotprice,
         c("price","mean_price","current_real_price","new_real_price1","actual_price"),
         c("spotpris",
           "månedlig_gjennomsnittlig_spotpris",
           "pris_nåværende_ordning",
           "pris_ny_ordning",
           "pris_daværende_ordning"))


melt_dt_plotprice <- melt(dt_plotprice,
                          id.vars = c("tp","area","date","start_hour","year","month"))
melt_dt_plotprice[,variable:=factor(variable,levels=c("månedlig_gjennomsnittlig_spotpris",
                                                      "spotpris",
                                                      "pris_daværende_ordning",
                                                      "pris_nåværende_ordning",
                                                      "pris_ny_ordning"))]


ggplot(melt_dt_plotprice[month%in%1:12],aes(x=tp,y=value,col=variable))+
  geom_line()+
  facet_wrap(vars(area),scales="free",ncol=1)+
  theme(legend.position = "bottom")+
  ylab("NOK/kWh")+
  xlab("Tidspunkt")

gg_pris_list <- list()
for(i in 1:5){
  this <- paste0("NO",i)
  gg_pris_list[[i]] <- ggplot(melt_dt_plotprice[area==this],aes(x=tp,y=value,col=variable))+
    geom_line()+
    facet_wrap(vars(month),scales="free",labeller = label_both)+
    theme(legend.position = "bottom")+
    ylab("NOK/kWh")+
    xlab("Tidspunkt")+
    ggtitle(paste0("Priser per måned prisområde ",this))

}

pdf("DN/timespris_ulike_ordninger.pdf",width = 12,height = 8)
for(i in 1:5){
  print(gg_pris_list[[i]])
}
dev.off()

dt_timesforbruk <- fread("../../Div/Debattinnlegg/timesforbruksdata_csv.csv",dec = ",")
dt_dagsforbruk <- fread("../../Div/Debattinnlegg/Daglig-forbruk-husholdning.csv")
dt_dagsforbruk <- dt_dagsforbruk[Gruppe=="Husholdning"]

names(dt_dagsforbruk)[1] <- "Antall_maalepunkter"
dt_dagsforbruk[,avg_forbruk_kWh:=`Volum (MWh)`/`Antall_maalepunkter`*1000]
dt_dagsforbruk[,dato:=as.IDate(Bruksdogn,format="%d.%m.%Y")]
dt_dagsforbruk[,is_weekend:=(wday(dato) %in% c(1,7))]

dt_timesforbruk_long <- rbind(dt_timesforbruk[,.(Time,andel_per_time=Prosentfordeling_helg,is_weekend=TRUE)],
                              dt_timesforbruk[,.(Time,andel_per_time=Prosentfordeling_ukedag,is_weekend=FALSE)])

dt_forbruk <- merge(dt_dagsforbruk[dato>="2022-01-01",.(Prisomraade,dato,avg_forbruk_kWh,is_weekend)],dt_timesforbruk_long,by="is_weekend",allow.cartesian = T)

setcolorder(dt_forbruk,c("Prisomraade","Time","dato"))
dt_forbruk[,forbruk_per_time:=avg_forbruk_kWh*andel_per_time]
dt_forbruk[,tp:=as.POSIXct(dato)+(Time-1)*60*60]


#### Computing cost with different compensation methods:



dt_cost <- merge(dt_hourly[,.(area,tp,
                              current_comp,current_real_price,
                              new_comp1,new_real_price1,
                              new_comp2,new_real_price2,
                              new_comp3,new_real_price3,
                              actual_comp,actual_price)],
                 dt_forbruk[,.(area=Prisomraade,tp,forbruk_per_time)],
                 by=c("area","tp"))

dt_cost[,consumer_cost_current := current_real_price*forbruk_per_time]
dt_cost[,consumer_cost_new1 := new_real_price1*forbruk_per_time]
dt_cost[,consumer_cost_new2 := new_real_price2*forbruk_per_time]
dt_cost[,consumer_cost_new3 := new_real_price3*forbruk_per_time]
dt_cost[,consumer_cost_actual := actual_price*forbruk_per_time]

dt_cost[,tot_comp_current := current_comp*forbruk_per_time]
dt_cost[,tot_comp_new1 := new_comp1*forbruk_per_time]
dt_cost[,tot_comp_new2 := new_comp2*forbruk_per_time]
dt_cost[,tot_comp_new3 := new_comp3*forbruk_per_time]
dt_cost[,tot_comp_actual := actual_comp*forbruk_per_time]
dt_cost[,month:=month(tp)]

dt_cost_month <- dt_cost[,lapply(.SD,sum),.SDcols=c("consumer_cost_current","consumer_cost_new1","consumer_cost_new2","consumer_cost_new3","consumer_cost_actual",
                                                    "tot_comp_current","tot_comp_new1","tot_comp_new2","tot_comp_new3","tot_comp_actual"),
                         by=.(month,area)]

dt_plotcost <- dt_cost_month[,.(area,month,
                                consumer_cost_current,consumer_cost_new1,consumer_cost_actual)]

setnames(dt_plotcost,
         c("consumer_cost_current","consumer_cost_new1","consumer_cost_actual"),
         c("nåværende_ordning",
           "ny_ordning",
           "daværende_ordning"))


melt_dt_plotcost <- melt(dt_plotcost,
                          id.vars = c("area","month"))
melt_dt_plotcost[,variable:=factor(variable,levels=c("daværende_ordning",
                                                      "nåværende_ordning",
                                                      "ny_ordning"))]

ggplot(melt_dt_plotcost,aes(x=month,y=value,col=variable))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(area),scales="free")+
  theme(legend.position = "bottom")+
  ylab("NOK")+
  xlab("Måned")+
  ggtitle("Husholdningskostnad per måned for gjennomsnittshusholdning")

ggsave("DN/Husholdningskostnad_per_måned.pdf",width = 12,height=8)


##### KOMPENSASJON ####

dt_plotcost2 <- dt_cost_month[,.(area,month,
                                tot_comp_current,tot_comp_new1,tot_comp_actual)]

setnames(dt_plotcost2,
         c("tot_comp_current","tot_comp_new1","tot_comp_actual"),
         c("nåværende_ordning",
           "ny_ordning",
           "daværende_ordning"))


melt_dt_plotcost2 <- melt(dt_plotcost2,
                         id.vars = c("area","month"))
melt_dt_plotcost2[,variable:=factor(variable,levels=c("daværende_ordning",
                                                     "nåværende_ordning",
                                                     "ny_ordning"))]

ggplot(melt_dt_plotcost2,aes(x=month,y=value,col=variable))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(area),scales="free")+
  theme(legend.position = "bottom")+
  ylab("NOK")+
  xlab("Måned")+
  ggtitle("Kompensasjon per måned for gjennomsnittshusholdning")

ggsave("DN/Kompensasjon_per_måned.pdf",width = 12,height=8)

#### Besparelse med ny ordning (kontra daværende og nåværende)

dt_cost_month[,besparelse_ny_vs_daværende := tot_comp_new1-tot_comp_actual]
dt_cost_month[,besparelse_ny_vs_nåværende := tot_comp_new1-tot_comp_current]

dt_plotcost3 <- dt_cost_month[,.(area,month,
                                 besparelse_ny_vs_daværende,besparelse_ny_vs_nåværende)]

setnames(dt_plotcost3,
         c("besparelse_ny_vs_daværende","besparelse_ny_vs_nåværende"),
         c("ny_vs_daværende",
           "ny_vs_nåværende"))


melt_dt_plotcost3 <- melt(dt_plotcost3,
                          id.vars = c("area","month"))
melt_dt_plotcost3[,variable:=factor(variable,levels=c("ny_vs_daværende",
                                                      "ny_vs_nåværende"))]

ggplot(melt_dt_plotcost3,aes(x=month,y=value,col=variable))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(area),scales="free")+
  theme(legend.position = "bottom")+
  ylab("NOK")+
  xlab("Måned")+
  ggtitle("Besparelse per måned for gjennomsnittshusholdning")

ggsave("DN/Besparelse_per_måned.pdf",width = 12,height=8)

### Legg også inn tabell med summen over året for kostand, kompensasjon og besparelse per område

dt_cost_year <- dt_cost_month[,lapply(.SD,sum),.SDcols=c("tot_comp_current","tot_comp_new1","tot_comp_actual",
                                                         "consumer_cost_current","consumer_cost_new1","consumer_cost_actual",
                                                         "besparelse_ny_vs_daværende","besparelse_ny_vs_nåværende"),
                              by=.(area)]
setnames(dt_cost_year,
         c("tot_comp_current","tot_comp_new1","tot_comp_actual"),
         c("kompensasjon_nåværende_ordning",
           "kompensasjon_ny_ordning",
           "kompensasjon_daværende_ordning"))

setnames(dt_cost_year,
         c("consumer_cost_current","consumer_cost_new1","consumer_cost_actual"),
         c("husholdningskostnad_nåværende_ordning",
           "husholdningskostnad_ny_ordning",
           "husholdningskostnad_daværende_ordning"))

library(gridExtra)
pdf("DN/årstotal_kompensasjon.pdf", width=10,height=4)
grid.table(dt_cost_year[,.(area,kompensasjon_nåværende_ordning, kompensasjon_ny_ordning, kompensasjon_daværende_ordning)])
dev.off()

library(gridExtra)
pdf("DN/årstotal_husholdningskostnad.pdf", width=12,height=4)
grid.table(dt_cost_year[,.(area,husholdningskostnad_nåværende_ordning, husholdningskostnad_ny_ordning, husholdningskostnad_daværende_ordning )])
dev.off()

library(gridExtra)
pdf("DN/årstotal_besparelse.pdf", width=10,height=4)
grid.table(dt_cost_year[,.(area,besparelse_ny_vs_daværende, besparelse_ny_vs_nåværende )])
dev.off()



#fwrite(dt_cost_year,file = "DN/årsoversikt_per_prisomårde.csv",)


#### besparelse vs svingning i spotpris per måned per omr

dt_vol <- dt_hourly[,list(volatilitet=sd(price)),by=.(area,month)]

dt_plot_vol_besparelse <- merge(dt_cost_month[,.(area,month,besparelse_ny_vs_daværende, besparelse_ny_vs_nåværende)],dt_vol,by=c("area","month"))

ggplot(dt_plot_vol_besparelse,aes(x=volatilitet,y=besparelse_ny_vs_daværende))+
  geom_point()+
  geom_smooth()+
  ggtitle("Månedlig besparelse (kontra daværende ordning) vs volatilitet i spotpris")

ggsave("DN/besparelse_vs_volatilitet.pdf",width=12,height=8)
