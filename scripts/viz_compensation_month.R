library(data.table)
library(ggplot2)

Sys.setlocale("LC_ALL", "en_US.UTF-8") # UTF-8 to get latin letters

source("source/funcs.R")

areas <- c("NO1","NO2")
plot_CI_int <- 0.95
plot_k_days_prev_month = 7

historic_compensation_filename <- "data/historic_estimated_compensation.csv"

database_daily_filename <- "data/database_nordpool_daily.csv"

daily_dt <- fread(database_daily_filename)

res_dt <- fread(historic_compensation_filename)

res_dt[,computation_date:=estimation_date+1]


this_date <- res_dt[,max(computation_date)]

this_month <- data.table::month(this_date)
this_year <- data.table::year(this_date)
this_day <- data.table::mday(this_date)

days_this_month <- lubridate::days_in_month(this_date)

remaining_days <- days_this_month-this_day

first_day_month <- as.IDate(paste0(this_year,"-",this_month,"-01"))
last_day_month <- as.IDate(paste0(this_year,"-",this_month,"-",days_this_month))


#get_quant <- function(x){
#  round(as.numeric(sub("%","",sapply(strsplit(x, "_"), "[", 2))),6)
#}

#res_dt[type0=="quantile",quant:=unlist(lapply(type,get_quant))]

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


plot_observed_prices <- daily_dt[date>=first_day_month-plot_k_days_prev_month & area %in% areas]
setnames(plot_observed_prices,"price","pris")
setnames(plot_observed_prices,"date","computation_date")
plot_observed_prices[,type:="d_observed"]


scaleFUN <- function(x) sprintf("%.2f", x)

library(ggplot2)

computation_date0 <- res_dt[,max(computation_date)]
computation_month_NO <- get_NO_month(month(estimation_date))
computation_year <- year(estimation_date)

title <- paste("Estimert strømstøtte for",computation_month_NO,computation_year,"per",computation_date0)


ggplot(mapping = aes(x=computation_date,col=type))+
  geom_ribbon(data=plot_ints[variable=="compensation"],alpha=0.3,
              mapping=aes(ymin=get(these_quants[1]),
                          ymax=get(these_quants[2]),
                          fill=""))+
  geom_line(data=plot_lines[variable=="compensation"],mapping = aes(y=pris),size=1)+
#              aes(ymin=as.character(these_quants[1]),
#                  ymax=as.character(these_quants[2])))+
  geom_vline(xintercept=first_day_month)+
  geom_point(plot_observed_prices,mapping=aes(y=pris))+
  facet_wrap(vars(Prisområde=area),nrow=2,labeller = label_both)+
#  expand_limits(x = )+
  scale_x_date(name = "Siste prisoppdatering",date_minor_breaks = "1 day",date_breaks = "1 weeks",date_labels="%d-%m",limits=c(first_day_month-plot_k_days_prev_month,last_day_month))+
  scale_y_continuous(name = "Pris (NOK/kWh)",labels=scaleFUN)+
  scale_fill_manual(name = "95% Konfidensintervall",values=scales::hue_pal()(3)[1])+
  scale_color_discrete(labels = c("Estimat m/ 95% konfidensintervall","Så langt denne måned", "Absoltt nedre grense","Observert dagspris"))+
  guides(color=guide_legend("",override.aes = list(fill=NA)),fill="none")+
  theme(legend.position = "bottom")+
  ggtitle(title)

