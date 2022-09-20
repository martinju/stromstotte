
#install.packages("rjson")

library(rjson)
library(data.table)

source("funcs.R")

# Building database

date_from = as.Date("2022-09-15")
date_to = as.Date("2022-09-21")#as.Date(Sys.time())
areas = c("NO2")#,"NO2")
playground = FALSE
key = "1e931976-1e0a-466d-811d-aec47e9ebe42"

dates = seq(date_from,date_to,1)

database <- NULL
for(i in seq_along(dates)){
  for(j in seq_along(areas)){
    if(playground){
      file = paste0("https://playground-norway-power.ffail.win/?zone=",areas[j],"&date=",dates[i],"&key=123")
    } else {
      file = paste0("https://norway-power.ffail.win/?zone=",areas[j],"&date=",dates[i],"&key=",key)
    }

    dat <- path_to_dt(file)
    database <- rbind(database,dat)

  }
  print(dates[i])
}

if(playground){
  #database0 <- copy(database)
  while(uniqueN(database,by=c("to","from","area"))<database[,.N]){
    database[,dups:=duplicated(.SD,fromLast=TRUE),.SDcols=c("area","from","to")]
    database[dups==TRUE,from:=from-24*60*60]
    database[dups==TRUE,to:=to-24*60*60]

  }
  database[,dups:=NULL]
  data.table::fwrite(database,"database_fake.csv")
} else {
  data.table::fwrite(database,"database.csv",append = T)
}

