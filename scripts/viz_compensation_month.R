library(data.table)
library(ggplot2)

Sys.setlocale("LC_ALL", "en_US.UTF-8") # UTF-8 to get latin letters
#Sys.setlocale("LC_ALL", "no_NO.UTF-8") # UTF-8 with Norwegian months
source("source/funcs.R")

areas <- c("NO1","NO2","NO3","NO4","NO5")
plot_CI_int <- 0.95
plot_k_days_prev_month = 7

historic_compensation_filename <- "data/historic_estimated_compensation.csv"

database_daily_filename <- "data/database_nordpool_daily.csv"

daily_dt <- fread(database_daily_filename)

res_dt <- fread(historic_compensation_filename)

res_dt[,computation_date:=estimation_date+1]

res_dt <- res_dt[area%in% areas]

this_date <- res_dt[,max(computation_date)]
#this_date <- as.IDate("2022-09-30")

this_month <- data.table::month(this_date)
this_year <- data.table::year(this_date)
this_day <- data.table::mday(this_date)

days_this_month <- lubridate::days_in_month(this_date)

remaining_days <- days_this_month-this_day

first_day_month <- as.IDate(paste0(this_year,"-",this_month,"-01"))
last_day_month <- as.IDate(paste0(this_year,"-",this_month,"-",days_this_month))


res_dt <- res_dt[computation_month==this_month & computation_year==this_year]

these_quants <- paste0("quantile_",round(c((1-plot_CI_int)/2,1-(1-plot_CI_int)/2),6))

plot_lines0 <- res_dt[type%in%c("current_mean","lower_bound","median")]
plot_lines <- melt(plot_lines0,measure.vars = c("mean_price","compensation"),value.name = "pris")

plot_lines[type=="median",type:="a_median"]
plot_lines[type=="current_mean",type:="b_current_mean"]
plot_lines[type=="lower_bound",type:="c_lower_bound"]
plot_lines[,area_long:=get_long_area_vec(area)]



plot_ints0 <- res_dt[type%in%these_quants]

plot_ints01 <-dcast(plot_ints0,area+computation_date+computation_year+computation_month~type, value.var ="mean_price")
plot_ints02 <- dcast(plot_ints0,area+computation_date+computation_year+computation_month~type, value.var ="compensation")

plot_ints <- rbind(plot_ints01[,variable:="mean_price"],
                 plot_ints02[,variable:="compensation"])
plot_ints[,type:="median"]


plot_ints[type=="median",type:="a_median"]
plot_ints[type=="current_mean",type:="b_current_mean"]
plot_ints[type=="lower_bound",type:="c_lower_bound"]
plot_ints[,area_long:=get_long_area_vec(area)]


plot_text <- rbind(plot_ints[computation_date==max(computation_date),.(area,variable,computation_date,y=quantile_0.025,what="CI")],
                   plot_ints[computation_date==max(computation_date),.(area,variable,computation_date,y=quantile_0.975,what="CI")],
                   plot_lines[computation_date==max(computation_date) & type=="a_median",.(area,variable,computation_date,y=pris,what="est")])
plot_text[,type:="a_median"]
plot_text[,y_format:=format(round(y,2),nsmall=2)]
plot_text[,font:=ifelse(what=="CI","plain","bold")]
plot_text[,size:=ifelse(what=="CI",1,2)]
plot_text[,area_long:=get_long_area_vec(area)]


setorder(plot_text,-what)

plot_observed_prices <- daily_dt[date>=first_day_month-plot_k_days_prev_month & date<=last_day_month & area %in% areas]
setnames(plot_observed_prices,"price","pris")
setnames(plot_observed_prices,"date","computation_date")
plot_observed_prices[,obscol:="Daglig spotpris"]
plot_observed_prices[,area_long:=get_long_area_vec(area)]
plot_observed_prices[,variable:="mean_price"]

scaleFUN <- function(x) sprintf("%.2f", x)

computation_month_NO <- get_NO_month(month(this_date))
computation_year <- year(this_date)

title_compensation <- paste("Estimert strømstøtte for",computation_month_NO,computation_year)
title_mean_price <- paste("Estimert gjennomsnittlig månedlig spotpris for",computation_month_NO,computation_year)

subtitle <- paste("Per dag",first_day_month,"til",this_date)


gg_compensation <- ggplot(mapping = aes(x=computation_date,col=type))+
  geom_ribbon(data=plot_ints[variable=="compensation"],alpha=0.3,
              mapping=aes(ymin=get(these_quants[1]),
                          ymax=get(these_quants[2]),
                          fill=""))+
  geom_line(data=plot_lines[variable=="compensation"],mapping = aes(y=pris,linetype=type),size=1)+
#              aes(ymin=as.character(these_quants[1]),
#                  ymax=as.character(these_quants[2])))+
  facet_wrap(vars(Prisområde=area_long),ncol=1,labeller = label_both,scales = "free_y")+
#  expand_limits(x = )+
  scale_x_date(name = "Siste prisoppdatering",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b",limits=c(first_day_month,last_day_month))+
  scale_y_continuous(name = "Pris (NOK/kWh inkl. mva, eks. nettleie/påslag)",labels=scaleFUN)+
  scale_fill_manual(name = "95% Konfidensintervall",values=scales::hue_pal()(3)[1])+
  scale_color_discrete(labels = c("Estimat m/ 95% konfidensintervall","Så langt denne måned", "Absolutt nedre grense","Observert dagspris"))+
  guides(color=guide_legend("",override.aes = list(fill=NA)),fill="none",linetype="none")+
  theme(legend.position = "bottom")+
  ggtitle(title_compensation,subtitle = subtitle)+
  geom_text(data=plot_text[variable=="compensation"],
            mapping=aes(y=y,label=y_format,fontface=font),
            hjust="left",check_overlap=TRUE,show.legend = F)


gg_mean_price <- ggplot(mapping = aes(x=computation_date,col=type))+
  geom_ribbon(data=plot_ints[variable=="mean_price"],alpha=0.3,
              mapping=aes(ymin=get(these_quants[1]),
                          ymax=get(these_quants[2]),
                          fill=""))+
  geom_line(data=plot_lines[variable=="mean_price"],mapping = aes(y=pris,linetype=type),size=1)+
  #              aes(ymin=as.character(these_quants[1]),
  #                  ymax=as.character(these_quants[2])))+
  geom_vline(xintercept=first_day_month)+
  geom_point(plot_observed_prices,mapping=aes(y=pris,shape=obscol),color="black")+
  facet_wrap(vars(Prisområde=area_long),ncol=1,labeller = label_both,scales = "free_y")+
  #  expand_limits(x = )+
  scale_x_date(name = "Siste prisoppdatering",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b",limits=c(first_day_month-plot_k_days_prev_month,last_day_month))+
  scale_y_continuous(name = "Pris (NOK/kWh inkl. mva, eks. nettleie/påslag)",labels=scaleFUN)+
  scale_fill_manual(name = "95% Konfidensintervall",values=scales::hue_pal()(3)[1])+
  scale_color_discrete(labels = c("Estimat m/ 95% konfidensintervall","Så langt denne måned", "Absolutt nedre grense","Observert dagspris"))+
  guides(color=guide_legend("",override.aes = list(fill=NA),order=1),shape=guide_legend(""),fill="none",linetype="none")+
  theme(legend.position = "bottom")+
  ggtitle(title_mean_price,subtitle = subtitle)+
  geom_text(data=plot_text[variable=="mean_price"],
            mapping=aes(y=y,label=y_format,fontface=font),
            hjust="left",check_overlap=TRUE,show.legend = F)

#gg_compensation
#gg_mean_price


ggsave("output/current_estimated_compensation.png",plot = gg_compensation,width = 10,height=3*length(areas)+2,scale=0.75)
ggsave("output/current_estimated_mean_price.png",plot = gg_mean_price,width = 10,height=3*length(areas)+2,scale=0.75)

ggsave(paste0("output/historic/estimated_compensation_",this_date,".png"),plot = gg_compensation,width = 10,height=3*length(areas)+2,scale=0.75)
ggsave(paste0("output/historic/estimated_mean_price_",this_date,".png"),plot = gg_mean_price,width = 10,height=3*length(areas)+2,scale=0.75)


gg_NO1_compensation <- ggplot(mapping = aes(x=computation_date,col=type))+
  geom_ribbon(data=plot_ints[variable=="compensation"& area=="NO1"],alpha=0.3,
              mapping=aes(ymin=get(these_quants[1]),
                          ymax=get(these_quants[2]),
                          fill=""))+
  geom_line(data=plot_lines[variable=="compensation"& area=="NO1"],mapping = aes(y=pris,linetype=type),size=1)+
  #              aes(ymin=as.character(these_quants[1]),
  #                  ymax=as.character(these_quants[2])))+
  #  expand_limits(x = )+
  scale_x_date(name = "Siste prisoppdatering",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b",limits=c(first_day_month,this_date+4))+
  scale_y_continuous(name = "",labels=scaleFUN)+
  scale_fill_manual(name = "95% Konfidensintervall",values=scales::hue_pal()(3)[1])+
  scale_color_discrete(labels = c("Estimat m/ 95% konfidensintervall","Så langt denne måned", "Absolutt nedre grense","Observert dagspris"))+
  guides(color=guide_legend("",override.aes = list(fill=NA)),fill="none",linetype="none")+
  theme(legend.position = "bottom")+
  ggtitle("Strømstøtte")+
  geom_text(data=plot_text[variable=="compensation" & area=="NO1"],
            mapping=aes(y=y,label=y_format,fontface=font),
            hjust="left",check_overlap=TRUE,show.legend = F)
gg_NO1_compensation


gg_NO1_mean_price <- ggplot(mapping = aes(x=computation_date,col=type))+
  geom_ribbon(data=plot_ints[variable=="mean_price"& area=="NO1"],alpha=0.3,
              mapping=aes(ymin=get(these_quants[1]),
                          ymax=get(these_quants[2]),
                          fill=""))+
  geom_line(data=plot_lines[variable=="mean_price"& area=="NO1"],mapping = aes(y=pris,linetype=type),size=1)+
  #              aes(ymin=as.character(these_quants[1]),
  #                  ymax=as.character(these_quants[2])))+
  geom_vline(xintercept=first_day_month)+
  geom_point(plot_observed_prices[area=="NO1"],mapping=aes(y=pris,shape=obscol),color="black")+
  scale_x_date(name = "Siste prisoppdatering",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b",limits=c(first_day_month-plot_k_days_prev_month,this_date+4))+
  scale_y_continuous(name = "Pris (NOK/kWh inkl. mva, eks. nettleie/påslag)",labels=scaleFUN)+
  scale_fill_manual(name = "95% Konfidensintervall",values=scales::hue_pal()(3)[1])+
  scale_color_discrete(labels = c("Estimat m/ 95% konfidensintervall","Så langt denne måned", "Absolutt nedre grense","Observert dagspris"))+
  guides(color=guide_legend("",override.aes = list(fill=NA),order=1),shape=guide_legend(""),fill="none",linetype="none")+
  theme(legend.position = "bottom")+
  ggtitle("Månedlig spotpris")+
  geom_text(data=plot_text[variable=="mean_price"& area=="NO1"],
            mapping=aes(y=y,label=y_format,fontface=font),
            hjust="left",check_overlap=TRUE,show.legend = F)
gg_NO1_mean_price

library(patchwork)

title_common_NO1 <- paste("Estimert månedlig spotpris og strømstøtte for prisområde NO1 (Østlandet) ",computation_month_NO,computation_year)
subtitle_common_NO1 <- paste("Per dag",first_day_month,"til",this_date)


gg_NO_both <- (gg_NO1_mean_price+gg_NO1_compensation)+ plot_annotation(title = title_common_NO1,subtitle = subtitle_common_NO1)+
  plot_layout(guides = 'collect')& theme(legend.position='bottom')

ggsave("output/current_estimation_both_NO1.png",plot = gg_NO_both,width = 10,height=5,scale=1)


update_text <- paste0(format(Sys.time(),'%d.%m.%Y, kl. %H.%M')," med spotpriser for ",format(this_date,'%d.%m.%Y'))
writeLines(update_text,"output/update_time.txt")


