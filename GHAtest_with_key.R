

library(data.table)
library(ggplot2)

KEY_FFAIL_DATA <- Sys.getenv("KEY_FFAIL_DATA")

dt <- data.table(aa=1:10,bb=rnorm(10))
dt[,systime:=Sys.time()]
dt[,my_key:=KEY_FFAIL_DATA]

ggplot(dt,aes(x=aa,y=bb))+geom_line()

fwrite(dt,file = "GHA_testing_with_creds.csv")

