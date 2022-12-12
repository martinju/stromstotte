

library(data.table)
library(ggplot2)
library(plotly)

dt <- data.table(x=Sys.Date()-(1:10),y=1:10)

p <- ggplot(dt,aes(x=x,y=y))+geom_point()

p_pl <- ggplotly(p,dynamicTicks = TRUE)

p_pl2 <- layout(p_pl,xaxis = list(
  rangeselector = list(
    buttons = list(
      list(
        count = 5,
        label = "last 5",
        step = "day",
        stepmode = "todate"
      ),
      list(
        step = "all",
        visible=FALSE
      )
    )
  ),
  rangeslider = list(type="date"),
#  range = as.character(c(Sys.Date()-5,Sys.Date()-1))
range = c( plotly:::to_milliseconds(Sys.Date()-5),plotly:::to_milliseconds(Sys.Date()-1))
#    range = c(10^3,10^5)
)
)

p_pl2$x$layout$xaxis

p_pl2%>%rangeslider(start=as.character(Sys.Date()-5),end=as.character(Sys.Date()-1))




#### from https://stackoverflow.com/questions/39664378/plotly-range-slider-on-x-axis-date-with-custom-start-end-date

library(plotly)

df <- data.frame(Date = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by="days"),
                 Value = sample(100:200, size = 244, replace = T))

p0 <- ggplot(df,aes(x=Date,y=Value))+geom_line()

p <- ggplotly(p0) %>%
  layout(xaxis = list(range = c( as.numeric(max(df$Date)-30) *86400000,
                                 as.numeric(max(df$Date)) * 86400000   ),
                      rangeslider = list(type = "date")  ))
p











###########


plot_ly(x = time(USAccDeaths), y = USAccDeaths) %>%
  add_lines() %>%
  rangeslider()

d <- tibble::tibble(
  time = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by = "days"),
  y = rnorm(seq_along(time))
)

plot_ly(d, x = ~time, y = ~y) %>%
  add_lines() %>%
  rangeslider(d$time[5], d$time[50])


a <- ggplot(d,aes(x=time,y=y))+geom_line()
b <- a +rangeslider(a,d$time[5], d$time[50])
b




dt <- data.table(x=rep(Sys.Date()-(1:5),2),y=1:10,type=as.character(c(rep(1,5),rep(2,5))))

p <- ggplot(dt,aes(x=x,y=y,col=type))+geom_line()

p_pl <- ggplotly(p,dynamicTicks = TRUE)

p_pl <- style(p_pl,visible="legendonly",traces=1) #trace=2 identified through plotly_json(p_pl)

p_pl



