dt_daily <- data.table::fread("datebase_daily_nordpool.csv")

acf(dt_daily$price,lag.max = 60)

mod <- forecast::auto.arima(dt_daily$price)
plot(dt_daily$date,dt_daily$price,type="l")
points(dt_daily$date,dt_daily$price)
lines(dt_daily$date,mod$fitted,col=2)
points(dt_daily$date,mod$fitted,col=2)

