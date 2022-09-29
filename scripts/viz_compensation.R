
library(data.table)
library(ggplot2)

areas <- c("NO1","NO2")
plot_CI_ints <- c(0.99,0.95,0.9,0.8,0.5)
plot_avg_measures <- c("mean","median")

database_hourly_filename <- "data/database_hourly.csv"
current_compensation_filename <- "data/current_estimated_compensation.csv"
current_density_compensation_filename <- "data/current_estimated_compensation_density.csv"

density_dt <- fread(current_density_compensation_filename)

res_dt <- fread(current_compensation_filename)

tomorrow_dt <- fread(database_hourly_filename)

quantiles_dt <- res_dt[grepl("quantile",type)]

get_quant <- function(x){
  as.numeric(sub("%","",sapply(strsplit(x, "_"), "[", 2)))
}

quantiles_dt[,quant:=lapply(type,get_quant)]



#### Plotting compensation

#ggplot(density_dt[type=="compensation"],aes(x=x,y=y))+geom_line()+facet_wrap(vars(area))

tab=NULL
for(j in seq_along(areas)){
  mp_m <- format(round(res_dt[area==areas[j] & type=="mean",mean_price],2),nsmall=2)
  mp_CI_L <- format(round(res_dt[area==areas[j] & type=="quantile_0.025",mean_price],2),nsmall=2)
  mp_CI_U <- format(round(res_dt[area==areas[j] & type=="quantile_0.975",mean_price],2),nsmall=2)

  mp_lb <- format(round(res_dt[area==areas[j] & type=="lower_bound",mean_price],2),nsmall=2)
  mp_cm <- format(round(res_dt[area==areas[j] & type=="current_mean",mean_price],2),nsmall=2)


  c_m <- format(round(res_dt[area==areas[j] & type=="mean",compensation],2),nsmall=2)
  c_CI_L <- format(round(res_dt[area==areas[j] & type=="quantile_0.025",compensation],2),nsmall=2)
  c_CI_U <- format(round(res_dt[area==areas[j] & type=="quantile_0.975",compensation],2),nsmall=2)

  c_lb <- format(round(res_dt[area==areas[j] & type=="lower_bound",compensation],2),nsmall=2)
  c_cm <- format(round(res_dt[area==areas[j] & type=="current_mean",compensation],2),nsmall=2)

  tab0 <- cbind(c("Gjennomsnittspris",mp_m,
                  paste0("(",mp_CI_L,",",mp_CI_U,")"),
                  mp_lb,
                  mp_cm),
                c("Kompensasjon",c_m,
                  paste0("(",c_CI_L,",",c_CI_U,")"),
                  c_lb,
                  c_cm))

  tab <- cbind(tab,tab0)
}

colnames(tab) <- rep(areas,each=2)
rownames(tab) <- c("","Estimat","95% konfidens","Absolutt nedre grense","Saa langt denne maned")

