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

plot_lines0 <- res_dt[type%in%c("current_mean","lower_bound","mean")]
plot_lines <- melt(plot_lines0,measure.vars = c("mean_price","compensation"),value.name = "pris")

plot_lines[type=="mean",type:="a_mean"]
plot_lines[type=="current_mean",type:="b_current_mean"]
plot_lines[type=="lower_bound",type:="c_lower_bound"]



plot_ints0 <- res_dt[type%in%these_quants]

plot_ints01 <-dcast(plot_ints0,area+computation_date+computation_year+computation_month~type, value.var ="mean_price")
plot_ints02 <- dcast(plot_ints0,area+computation_date+computation_year+computation_month~type, value.var ="compensation")

plot_ints <- rbind(plot_ints01[,variable:="mean_price"],
                 plot_ints02[,variable:="compensation"])
plot_ints[,type:="mean"]


plot_ints[type=="mean",type:="a_mean"]
plot_ints[type=="current_mean",type:="b_current_mean"]
plot_ints[type=="lower_bound",type:="c_lower_bound"]


plot_observed_prices <- daily_dt[date>=first_day_month-plot_k_days_prev_month & date<=last_day_month & area %in% areas]
setnames(plot_observed_prices,"price","pris")
setnames(plot_observed_prices,"date","computation_date")
plot_observed_prices[,obscol:="Daglig spotpris"]


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
  geom_line(data=plot_lines[variable=="compensation"],mapping = aes(y=pris),size=1)+
#              aes(ymin=as.character(these_quants[1]),
#                  ymax=as.character(these_quants[2])))+
  facet_wrap(vars(Prisområde=area),ncol=1,labeller = label_both,scales = "free_y")+
#  expand_limits(x = )+
  scale_x_date(name = "Siste prisoppdatering",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b",limits=c(first_day_month,last_day_month))+
  scale_y_continuous(name = "Pris (NOK/kWh)",labels=scaleFUN)+
  scale_fill_manual(name = "95% Konfidensintervall",values=scales::hue_pal()(3)[1])+
  scale_color_discrete(labels = c("Estimat m/ 95% konfidensintervall","Så langt denne måned", "Absolutt nedre grense","Observert dagspris"))+
  guides(color=guide_legend("",override.aes = list(fill=NA)),fill="none")+
  theme(legend.position = "bottom")+
  ggtitle(title_compensation,subtitle = subtitle)


gg_mean_price <- ggplot(mapping = aes(x=computation_date,col=type))+
  geom_ribbon(data=plot_ints[variable=="mean_price"],alpha=0.3,
              mapping=aes(ymin=get(these_quants[1]),
                          ymax=get(these_quants[2]),
                          fill=""))+
  geom_line(data=plot_lines[variable=="mean_price"],mapping = aes(y=pris),size=1)+
  #              aes(ymin=as.character(these_quants[1]),
  #                  ymax=as.character(these_quants[2])))+
  geom_vline(xintercept=first_day_month)+
  geom_point(plot_observed_prices,mapping=aes(y=pris,shape=obscol),color="black")+
  facet_wrap(vars(Prisområde=area),ncol=1,labeller = label_both,scales = "free_y")+
  #  expand_limits(x = )+
  scale_x_date(name = "Siste prisoppdatering",date_minor_breaks = "1 day",date_breaks = "3 days",date_labels="%d. %b",limits=c(first_day_month-plot_k_days_prev_month,last_day_month))+
  scale_y_continuous(name = "Pris (NOK/kWh inkl. mva, eks. nettleie/påslag)",labels=scaleFUN)+
  scale_fill_manual(name = "95% Konfidensintervall",values=scales::hue_pal()(3)[1])+
  scale_color_discrete(labels = c("Estimat m/ 95% konfidensintervall","Så langt denne måned", "Absolutt nedre grense","Observert dagspris"))+
  guides(color=guide_legend("",override.aes = list(fill=NA),order=1),shape=guide_legend(""),fill="none")+
  theme(legend.position = "bottom")+
  ggtitle(title_mean_price,subtitle = subtitle)

#gg_compensation
#gg_mean_price


ggsave("output/current_estimated_compensation.png",plot = gg_compensation,width = 10,height=3*length(areas)+2,scale=0.75)
ggsave("output/current_estimated_mean_price.png",plot = gg_mean_price,width = 10,height=3*length(areas)+2,scale=0.75)

ggsave(paste0("output/historic/estimated_compensation_",this_date,".png"),plot = gg_compensation,width = 10,height=3*length(areas)+2,scale=0.75)
ggsave(paste0("output/historic/estimated_mean_price_",this_date,".png"),plot = gg_mean_price,width = 10,height=3*length(areas)+2,scale=0.75)


