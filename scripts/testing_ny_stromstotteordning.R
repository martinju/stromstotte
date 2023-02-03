
library(data.table)
library(ggplot2)
Sys.setlocale(locale='nb_NO.utf8')


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
new_comp_prop2 <- 0.90*0.8935035 # kompensasjonsgrad 0.80 istedet for 0.90

dt_hourly[,new_comp2:=compfunc(price,new_comp_thres2,new_comp_prop2)]
dt_hourly[,new_real_price2:=price-new_comp2]

new_comp_thres3 <- 0.70*1.2432324*1.25 # Innslag på 87 ør før moms (109 øre etter moms)
new_comp_prop3 <- 0.90

dt_hourly[,new_comp3:=compfunc(price,new_comp_thres3,new_comp_prop3)]
dt_hourly[,new_real_price3:=price-new_comp3]




dt_hourly[,.(tot_org_comp=sum(org_comp),
             sd_org_comp=sd(org_real_price),
             range_org_comp=range(org_real_price),
             tot_new_comp1=sum(new_comp1),
             sd_new_comp1=sd(new_real_price1),
             range_new_comp1=range(new_real_price1),
             tot_new_comp2=sum(new_comp2),
             sd_new_comp2=sd(new_real_price2),
             range_new_comp2=range(new_real_price2),
             tot_new_comp3=sum(new_comp3),
             sd_new_comp3=sd(new_real_price3),
             range_new_comp3=range(new_real_price3)),by=.(area,month)]

dt_hourly[area=="NO1",.(tot_org_comp=sum(org_comp),
             sd_org_comp=sd(org_real_price),
             range_org_comp=range(org_real_price),
             tot_new_comp1=sum(new_comp1),
             sd_new_comp1=sd(new_real_price1),
             range_new_comp1=range(new_real_price1),
             tot_new_comp2=sum(new_comp2),
             sd_new_comp2=sd(new_real_price2),
             range_new_comp2=range(new_real_price2),
             tot_new_comp3=sum(new_comp3),
             sd_new_comp3=sd(new_real_price3),
             range_new_comp3=range(new_real_price3))]



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

ggplot(dt_plot[area=="NO1"& variable %in% c("price","org_real_price","new_real_price2","new_real_price3")],aes(x=tp,y=value,col=variable))+geom_line()+
  facet_wrap(vars(month),scales = "free",ncol = 1)

ggplot(dt_plot[area=="NO1"& variable %in% c("new_real_price2","new_real_price3")],aes(x=tp,y=value,col=variable))+geom_line()+
  facet_wrap(vars(month),scales = "free",ncol = 1)

ggplot(dt_plot[area=="NO1"& variable %in% c("org_real_price","new_real_price3")],aes(x=tp,y=value,col=variable))+geom_line()+
  facet_wrap(vars(month),scales = "free",ncol = 1)

ggplot(dt_plot[tp >= "2022-11-27" & tp < "2022-12-01" & area=="NO1"& variable %in% c("price","org_real_price","new_real_price3")],aes(x=tp,y=value,col=variable))+
  geom_line()

ggplot(dt_plot[tp >= "2022-12-24" & area=="NO1"& variable %in% c("price","org_real_price","new_real_price3")],aes(x=tp,y=value,col=variable))+
  geom_line()





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



dt_cost_egen <- merge(dt_hourly[area=="NO2",.(tp,price,
                                              org_comp,org_real_price,
                              new_comp1,new_real_price1,
                              new_comp2,new_real_price2,
                              new_comp3,new_real_price3)],
                      dt_time_egen[,.(tp,forbruk)],
                 by=c("tp"))
dt_cost_egen[,variabel_nettleie:=0.4176]
dt_cost_egen[tp > "2022-12-25",]
dt_cost_egen[tp > "2022-12-25",sum((org_real_price+variabel_nettleie)*forbruk)]
dt_cost_egen[tp > "2022-12-25",sum((org_comp)*forbruk)]
dt_cost_egen[tp > "2022-12-25",sum(price*forbruk)]
dt_cost_egen[tp > "2022-12-25",sum(variabel_nettleie*forbruk)]
dt_cost_egen[tp > "2022-12-25",sum(forbruk)]


dt_cost_egen[tp > "2022-11-29" & tp <= "2022-12-01",sum((org_real_price +variabel_nettleie)*forbruk)]
dt_cost_egen[tp > "2022-11-29" & tp <= "2022-12-01",sum(forbruk)]
dt_cost_egen[tp > "2022-11-29" & tp <= "2022-12-01",sum(org_comp*forbruk)]


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
# 0.8935035


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


dt_cost[area=="NO1" & tp >="2022-12-25", sum((org_real_price+0.40)*forbruk_per_time)]


#### Plott til innlegg:

dt_plot_innlegg <- dt_plot[area=="NO1" & variable %in% c("price","new_real_price3","org_real_price")]
dt_plot_innlegg[month==10,month2:="Oktober"]
dt_plot_innlegg[month==11,month2:="November"]
dt_plot_innlegg[month==12,month2:="Desember"]
dt_plot_innlegg[,month2:=factor(month2,levels=c("Oktober","November","Desember"))]


dt_plot_innlegg[variable=="price",variable:="spotpris"]
dt_plot_innlegg[variable=="org_real_price",variable:="din strømpris (dagens støtteordning)"]
dt_plot_innlegg[variable=="new_real_price3",variable:="din strømpris (foreslått støtteordning)"]
dt_plot_innlegg[,variable:=factor(variable,levels=c("spotpris",
                                                    "din strømpris (dagens støtteordning)",
                                                    "din strømpris (foreslått støtteordning)"))]
library(gtable)
library(cowplot)
library(grid)

shift_legend <- function(p){

  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }

  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }

  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")

  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")

  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")

  return(gp)
}

scaleFUN <- function(x) sprintf("%.2f", x)

rects <- data.frame(start=as.POSIXct(c("2022-11-29","2022-12-24"),tz="UTC"),
                    end=as.POSIXct(c("2022-12-01","2023-01-01"),tz="UTC"),
                    group=factor(c("29.-30. november","Romjula"),levels=c("29.-30. november","Romjula")),
                    month2 = as.factor(c("November","Desember")))
rects1 <- rects[1,]
rects2 <- rects[2,]

p <- ggplot(dt_plot_innlegg,aes(x=tp,y=value,col=variable))+
  geom_hline(yintercept=0,linetype=2)+
  geom_line(linewidth=1)+
  facet_wrap(vars(month2),scales = "free_x",ncol = 2)+
  scale_color_manual(values=c(spotpris="gray50",
                              `din strømpris (foreslått støtteordning)`="red",
                              `din strømpris (dagens støtteordning)`="blue"))+
  scale_fill_manual(values=c(`29.-30. november`="orange",
                             `Romjula`="green"))+
  theme_bw(base_size=14)+
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size=18),
        legend.spacing.y = unit(-0.3, "cm"))+
  labs(x="dag",y="NOK/kWh (inkl. mva.)",fill="",color="Strømpriser NO1 (Oslo) okt-des 2022\n")+
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2))+
  geom_rect(data=rects1, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(dt_plot_innlegg$value),
                                               ymax=max(dt_plot_innlegg$value), fill=group), color="transparent", alpha=0.2)+
  geom_rect(data=rects2, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(dt_plot_innlegg$value),
                                                ymax=max(dt_plot_innlegg$value), fill=group), color="transparent", alpha=0.2)+
  scale_y_continuous(labels=scaleFUN,n.breaks = 8)+
#  scale_x_datetime(timezone="Europe/London",
#                   breaks=seq(as.POSIXct("2022-10-01"), as.POSIXct("2022-12-31"), by="2 days"))
#  scale_x_datetime(timezone = "Europe/London",
#                   date_labels="%d",
#                   date_breaks = "4 days",
#                   minor_breaks = "1 day",
#                   expand = expansion(mult=0.02))
scale_x_datetime(timezone = "UTC",
                 date_labels="%d",
                 breaks = c(seq(as.POSIXct("2022-10-01",tz="UTC"), as.POSIXct("2022-10-31",tz = "UTC"), by="2 days"),
                            seq(as.POSIXct("2022-11-02",tz="UTC"), as.POSIXct("2022-11-30",tz = "UTC"), by="2 days"),
                            seq(as.POSIXct("2022-12-02",tz="UTC"), as.POSIXct("2022-12-31",tz = "UTC"), by="2 days")),
#                 minor_breaks = "1 day",
                 expand = expansion(mult=0.02))


dev.off()
pdf("blottplot2.pdf",width = 12,height=8)
grid.draw(shift_legend(p))
dev.off()

ggplot(dt_plot[tp >= "2022-11-29" & tp < "2022-12-01" & area=="NO1"& variable %in% c("price","org_real_price","new_real_price3")],aes(x=tp,y=value,col=variable))+
  geom_line()+
  scale_y_continuous(n.breaks=40)

ggplot(dt_plot[tp >= "2022-12-24" & area=="NO1"& variable %in% c("price","org_real_price","new_real_price3")],aes(x=tp,y=value,col=variable))+
  geom_line()










dt_dagsforbruk

