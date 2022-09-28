

library(data.table)
library(ggplot2)

dt <- data.table(aa=1:10,bb=rnorm(10))
dt[,systime:=Sys.time()]

ggplot(dt,aes(x=aa,y=bb))+geom_line()

fwrite(dt,file = "GHA_testing.csv")

