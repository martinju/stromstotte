estimation_date0=today#today-1

updated_dt_nettleie0 <- dt_nettleie[Nettselskap=="ELVIA AS"]
updated_dt_hourly0 <- dt_hourly[area=="NO1"]
updated_dt_comp0 <- dt_comp[area == "NO1"]

updated_dt_hourly0[,computation_year:=year(date)]
updated_dt_hourly0[,computation_month:=month(date)]



updated_dt_comp0[,last_est_date_per_month:=max(estimation_date),by=.(computation_year,computation_month)]
updated_dt_comp0_1 <- updated_dt_comp0[estimation_date==last_est_date_per_month & estimation_date!=estimation_date0 & type=="median"]
updated_dt_comp0_2 <- updated_dt_comp0[estimation_date==estimation_date0 & type %in% c("median","quantile_0.025","quantile_0.975","lower_bound")]

plot_strompris_naa_dt <- copy(updated_dt_hourly0)
setnames(plot_strompris_naa_dt,"price","spotpris")

dc1 <- dcast(updated_dt_comp0_1[,.(compensation,type,computation_year,computation_month)],formula= computation_year+computation_month~type,value.var="compensation")
dc2 <- dcast(updated_dt_comp0_2[,.(compensation,type,computation_year,computation_month)],formula= computation_year+computation_month~type,value.var="compensation")

setnames(dc1,"median","stotte")
setnames(dc2,c("median","quantile_0.025","quantile_0.975","lower_bound"),c("stotte","stotte_lower_CI","stotte_upper_CI","stotte_lower_bound"))

dc3 <- rbind(dc1,dc2,fill=TRUE)


plot_strompris_naa_dt[start_hour %in% seq(6,21),nettleie:=updated_dt_nettleie0[pristype=="Dag",Energiledd]]
plot_strompris_naa_dt[is.na(nettleie),nettleie:=updated_dt_nettleie0[pristype=="Natt",Energiledd]]

plot_strompris_naa_dt <- merge(plot_strompris_naa_dt,dc3,by=c("computation_year", "computation_month"))

plot_strompris_naa_dt[,totalpris:=spotpris+nettleie-stotte]
plot_strompris_naa_dt[,totalpris_lower_CI:=spotpris+nettleie-stotte_lower_CI]
plot_strompris_naa_dt[,totalpris_upper_CI:=spotpris+nettleie-stotte_upper_CI]


plot_strompris_naa_dt[,datetime:=as.POSIXct(date)+start_hour*60*60]


plot_strompris_naa_dt0 <- plot_strompris_naa_dt[,.(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI)]

plot_strompris_naa_dt0_dup <- copy(plot_strompris_naa_dt0)
plot_strompris_naa_dt0_dup[,datetime:=datetime+1*60*60-1]

plot_strompris_naa_dt00 <- rbind(plot_strompris_naa_dt0,plot_strompris_naa_dt0_dup)
setkey(plot_strompris_naa_dt00,datetime)

plot_strompris_naa_dt_ints_totalpris <- plot_strompris_naa_dt00[,.(datetime,lower_CI=totalpris_lower_CI,upper_CI=totalpris_upper_CI)]
plot_strompris_naa_dt_ints_totalpris[,type:="totalpris"]

plot_strompris_naa_dt_ints_stotte <- plot_strompris_naa_dt00[,.(datetime,lower_CI=stotte_lower_CI,upper_CI=stotte_upper_CI)]
plot_strompris_naa_dt_ints_stotte[,type:="stotte"]

plot_strompris_naa_dt_ints <- rbind(plot_strompris_naa_dt_ints_totalpris,plot_strompris_naa_dt_ints_stotte)

plot_strompris_naa_dt_melted <- melt(plot_strompris_naa_dt00[,.(datetime,spotpris,nettleie,totalpris,stotte)],
                                     id.vars = c("datetime"),variable.name = "type",value.name = "pris")

plot_dt_final <- merge(plot_strompris_naa_dt_melted,plot_strompris_naa_dt_ints,by=c("datetime","type"),all = T)

plot_dt_final[,linesize := "b"]
plot_dt_final[type%in% c("totalpris","stotte"),linesize := "a"]

setkey(plot_dt_final,datetime)

p_history <- ggplot(data=plot_dt_final,mapping=aes(x=datetime,y=pris,col=type,fill=type))+
  geom_line(aes(size=linesize))+
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.5)+
  ggtitle("Estimert reell strømpris")+
  scale_y_continuous(name = "Pris (NOK/kWh)",labels=scaleFUN,breaks = breaks_extended(15))+
  scale_x_datetime(name = "Tid/dato",
                   breaks=breaks_pretty(12),
                   minor_breaks = breaks_pretty(24),
                   labels = label_date_short(format = c("%Y", "", "%d.%b\n", "%H:%M\n"),sep=""))+ # TODO: Get Norwegian months
  scale_size_manual(values=c("a" = 1,"b"=0.5))

p_history

ggp_history <- ggplotly(p_history,dynamicTicks = TRUE)
ggp_history <- layout(
  ggp_history,
  hovermode = "x unified",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 1,
          label = "denne måned",
          step = "month",
          stepmode = "todate"),
        list(
          count = lubridate::wday(Sys.Date(),week_start=1),
          label = "denne uka",
          step = "day",
          stepmode = "todate"),
        list(
          count = 1,
          label = "idag",
          step = "day",
          stepmode = "todate"),
        list(
          count = 2,
          label = "2 siste dager",
          step = "day",
          stepmode = "todate")
      )),
    rangeslider = list(type = "date")
  )
)

ggp_history <- style(ggp_history,visible="legendonly",traces=c(3,7)) #trace=2 identified through plotly_json(ggp_history)
ggp_history

p_now <- ggplot(data=plot_dt_final[datetime>=today-1],mapping=aes(x=datetime,y=pris,col=type,fill=type))+
  geom_line(aes(size=linesize))+
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.5)+
  ggtitle("Estimert reell strømpris")+
  scale_y_continuous(name = "Pris (NOK/kWh)",labels=scaleFUN,breaks = breaks_extended(15))+
  scale_x_datetime(name = "Tid/dato",
                   breaks=breaks_pretty(12),
                   minor_breaks = breaks_pretty(24),
                   labels = label_date_short(format = c("%Y", "", "%d.%b\n", "%H:%M\n"),sep=""))+ # TODO: Get Norwegian months
  scale_size_manual(values=c("a" = 1,"b"=0.5))+
  guides(size="none")

p_now

ggp_now <- ggplotly(p_now,dynamicTicks = TRUE)
ggp_now <- layout(
  ggp_now,
  hovermode = "x unified"
  )
ggp_now <- style(ggp_now,visible="legendonly",traces=c(3,7)) #trace=2 identified through plotly_json(ggp_history)
ggp_now



style(ggplotly(p),visible="legendonly", traces = 2)


# TODO:

# Consider trying to mimick clicking on one of the buttons in shiny
# plotly::event_register and plotly::event_data might be something to look at


