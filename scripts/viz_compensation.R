library(data.table)
library(flextable)
library(ggplot2)

Sys.setlocale("LC_ALL", "en_US.UTF-8") # UTF-8 to get latin letters

source("source/funcs.R")

areas <- c("NO1","NO2","NO3","NO4","NO5")
plot_CI_ints <- c(0.99,0.95,0.9,0.8,0.5)
plot_avg_measures <- c("mean","median")

database_hourly_filename <- "data/database_nordpool_hourly.csv"
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
  mp_m <- format(round(res_dt[area==areas[j] & type=="median",mean_price],2),nsmall=2)
  mp_CI_L <- format(round(res_dt[area==areas[j] & type=="quantile_0.025",mean_price],2),nsmall=2)
  mp_CI_U <- format(round(res_dt[area==areas[j] & type=="quantile_0.975",mean_price],2),nsmall=2)

  mp_lb <- format(round(res_dt[area==areas[j] & type=="lower_bound",mean_price],2),nsmall=2)
  mp_cm <- format(round(res_dt[area==areas[j] & type=="current_mean",mean_price],2),nsmall=2)


  c_m <- format(round(res_dt[area==areas[j] & type=="median",compensation],2),nsmall=2)
  c_CI_L <- format(round(res_dt[area==areas[j] & type=="quantile_0.025",compensation],2),nsmall=2)
  c_CI_U <- format(round(res_dt[area==areas[j] & type=="quantile_0.975",compensation],2),nsmall=2)

  c_lb <- format(round(res_dt[area==areas[j] & type=="lower_bound",compensation],2),nsmall=2)
  c_cm <- format(round(res_dt[area==areas[j] & type=="current_mean",compensation],2),nsmall=2)

  tab0 <- cbind(c(mp_m,
                  paste0("(",mp_CI_L,",",mp_CI_U,")"),
                  mp_lb,
                  mp_cm),
                c(c_m,
                  paste0("(",c_CI_L,",",c_CI_U,")"),
                  c_lb,
                  c_cm))

  tab <- cbind(tab,tab0)
}



tab_header <- c("",rep(c("Spotpris\nmåned","Strømstøtte"),times=length(areas)))
names(tab_header) <- paste0("V",seq_len(ncol(tab)+1)-1)


tab_dt <- as.data.table(tab)
tab_dt <- cbind(V0=c("Estimat","95% konfidensintervall","Absolutt nedre grense","Så langt denne måned"),tab_dt)

set_flextable_defaults(background.color = "white")
ft <- flextable(tab_dt)
ft <- set_header_labels(ft,values=as.list(tab_header))
ft <- add_header_row(ft,values=c("",areas),colwidths = c(1,rep(2,length(areas))))

if(length(areas)>1){
  ft <- vline(ft,j=paste0("V",c(0,2*seq_len(length(areas)-1))),border=officer::fp_border(style="dashed"),part = "all")
}
ft <- align(ft,part="header",align="center")
ft <- align(ft,part="body",align="right")

computation_date <- res_dt[1,estimation_date]+1
computation_month_NO <- get_NO_month(month(computation_date))
computation_year <- year(computation_date)

caption_text <- paste("Estimert strømstøtte for",computation_month_NO,computation_year,"per",computation_date)
ft <- set_caption(ft, caption = caption_text)
#ft <- add_footer_lines(ft,"Estimering Martin Jullum, Norsk Regnesentral")
ft <- fontsize(ft, size = 9, part = "footer")
ft <- footnote(ft,i=2,j=seq(2*length(areas))+1,value=as_paragraph("Alle priser i NOK/kWh, inkl. mva, eksl. nettleie og øvrige påslag fra strømleverandør."),part="header",ref_symbols = "1")
#ft <- align(ft,align="right",part="footer",i = 1)
ft <- align(ft,align="left",part="footer",i = 1)
ft <- autofit(ft)

webshot::install_phantomjs() # Should get rid of this one if I can
save_as_image(ft,"output/current_estimate_tab.png")
