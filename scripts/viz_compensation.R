
library(data.table)
library(ggplot2)

areas <- c("NO1","NO2")#,"NO1")#c("NO1","NO2")
plot_CI_ints <- c(0.99,0.95,0.9,0.8,0.5)
plot_avg_measures <- c("mean","median")

database_hourly_filename <- "database_hourly.csv"
current_compensation_filename <- "current_estimated_compensation.csv"
current_density_compensation_filename <- "current_estimated_compensation_density.csv"

density_dt <- fread(current_density_compensation_filename)

res_dt <- fread(current_compensation_filename)

tomorrow_dt <- fread(database_hourly_filename)

quantiles_dt <- res_dt[grepl("quantile",type)]

get_quant <- function(x){
  as.numeric(sub("%","",sapply(strsplit(x, "_"), "[", 2)))
}

quantiles_dt[,quant:=lapply(type,get_quant)]



#### Plotting compensation

ggplot(density_dt[type=="compensation"],aes(x=x,y=y))+geom_line()+facet_wrap(vars(area))
