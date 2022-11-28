
library(data.table)

historic_compensation_filename <- "data/historic_estimated_compensation_full.csv"


dat <- fread(historic_compensation_filename)



dat[,max_date:=max(estimation_date),by=computation_month]

true_mean_price_dt <- dat[estimation_date==max_date & type=="mean",.(area,mean_price,computation_month)]
setnames(true_mean_price_dt,"mean_price","actual_mean")


dcast_dat <- dcast(data = dat[,.(area,estimation_date,mean_price,computation_month,type)],area+estimation_date+computation_month~type,value.var="mean_price")

dcast_dat <- merge(dcast_dat,true_mean_price_dt,by=c("area","computation_month"))

dcast_dat[,inside_99:=FALSE]
dcast_dat[,inside_95:=FALSE]
dcast_dat[,inside_90:=FALSE]
dcast_dat[,inside_80:=FALSE]
dcast_dat[,inside_50:=FALSE]

dcast_dat[actual_mean>quantile_0.005 & actual_mean<quantile_0.995,inside_99:=TRUE]
dcast_dat[actual_mean>quantile_0.025 & actual_mean<quantile_0.975,inside_95:=TRUE]
dcast_dat[actual_mean>quantile_0.05 & actual_mean<quantile_0.95,inside_90:=TRUE]
dcast_dat[actual_mean>quantile_0.1 & actual_mean<quantile_0.9,inside_80:=TRUE]
dcast_dat[actual_mean>quantile_0.25 & actual_mean<quantile_0.75,inside_50:=TRUE]

dcast_dat[,mean(inside_99),by=area]
dcast_dat[,mean(inside_95),by=area]
dcast_dat[,mean(inside_90),by=area]
dcast_dat[,mean(inside_80),by=area]
dcast_dat[,mean(inside_50),by=area]

dcast_dat[,mean(abs(actual_mean-median)),by=area]
dcast_dat[,mean(abs(actual_mean-mean)),by=area]
dcast_dat[,.(current_mean =mean(abs(actual_mean-current_mean)),
             median_pred = mean(abs(actual_mean-median))),by=.(area,computation_month)]
