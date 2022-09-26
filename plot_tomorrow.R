library(ggplot2)
library(scales)


# Duplicating the last hour to make plotting nicer
tomorrow_dt_plot <- rbind(tomorrow_dt_plot,tomorrow_dt_plot[start_hour==23][,start_hour:=24])

plot_list <- list()

for(j in seq_along(areas)){

  gg <- ggplot(data=tomorrow_dt_plot[area==areas[j]],mapping=aes(x=start_hour))+
    geom_step(aes(y=spot_price))
  if("mean" %in% plot_avg_measures){
    gg <- gg+    geom_step(aes(y=expected_price),col=2)
  }
  if("median" %in% plot_avg_measures){
    gg <- gg+    geom_step(aes(y=median_price),col=3)
  }

  if(!is.null(plot_CI_ints)){
    for(i in seq_along(plot_CI_ints)){
      CI_int <- plot_CI_ints[i]
      ymin_col <- paste0("CI_lower_",100*CI_int,"%_price")
      ymax_col <- paste0("CI_upper_",100*CI_int,"%_price")

      gg2 <- gg + pammtools::geom_stepribbon(aes(ymin=get(ymin_col),
                                                 ymax=get(ymax_col)),fill="red",alpha=0.1)


    }
  }

  geom_step(aes(y=spot_price))+
    geom_step(aes(y=spot_price))+


    geom_step(data=plot_dt[area==areas[j]&type=="mean"],col=2,linetype=2,mapping=aes(y=price))+
    geom_ribbon(data=plot_dt_CI80[area==areas[j]],mapping=aes(ymin=price_lower,ymax=price_upper))+
    scale_x_datetime(labels = date_format("%H:%M",tz="Europe/Berlin"))

  plot_list[[j]] <- tmp
}
