
# Schedule this to run at 13.15 every day

library(data.table)

read_new_data <- FALSE
playground = TRUE
use_past_k_days <- 31
k_bootstrap <- 5*10^2
compensation_prop <- 0.80
compensation_threshold <- 0.70*1.25
areas <- c("NO2") # Just one area at a time
plot_CI_ints <- c(0.99,0.95,0.9,0.8,0.5)
plot_avg_measures <- c("mean","median")
database_filename <- "database_nordpool.csv"
today_vec <- seq(as.IDate("2022-01-31"),as.IDate("2022-08-30"),by=1) #as.IDate(Sys.time())
seed = 12345
method = "double_bootstrap"#"all_indep"


res_mat <- matrix(NA,ncol=length(plot_CI_ints)*2+length(plot_avg_measures),nrow=length(today_vec))
for(ii in seq_along(today_vec)){
  today <- today_vec[ii]
  source("run.R")

  res_mat[ii,] <- estimated_meanprice

  print(ii)

}

res_dt <- as.data.table(res_mat)
names(res_dt) <- colnames(estimated_meanprice)

res_dt[,date:=today_vec+1]
res_dt[,month:=month(date)]
res_dt[,year:=year(date)]

datebase0 <- data.table::fread(database_filename)
datebase0 <- datebase0[area == areas] # Just one area at a time
datebase0[,month:=month(date)]
datebase0[,year:=year(date)]
actual_avg_dt <- datebase0[,list(actual_avgprice=mean(price)),by=.(month,year)]

res_dt <- merge(res_dt,actual_avg_dt,by=c("month","year"))
res_dt[,month:=NULL]
res_dt[,year:=NULL]

setcolorder(res_dt,c("date","actual_avgprice"))

coverage_dt <- data.table(supposed_level=plot_CI_ints,actual_coverage=0)
for(ii in seq_along(plot_CI_ints)){
  CI_int <- plot_CI_ints[ii]
  probs <- c((1-CI_int)/2,CI_int+(1-CI_int)/2)
  colnames <- paste0(probs*100,"%")
  actual_coverage0 <- res_dt[,mean(get(colnames[1])<actual_avgprice & actual_avgprice<get(colnames[2]))]
  coverage_dt[supposed_level==CI_int,actual_coverage:=actual_coverage0]
}

coverage_dt[]
