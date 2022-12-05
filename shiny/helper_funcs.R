twodigits <- function(x){
  format(round(x,2),nsmall=2)
}

scaleFUN <- function(x) sprintf("%.2f", x)

textfunc <- function(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text_prev_month <-   paste0("<span style='text-decoration:underline'><b>",format(date,'%d.%m.%y')," kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1)," </b></span>\n",
                              "<span style='color:",mycols['spotpris'],"'>Spot: ",twodigits(spotpris),"</span>\n",
                              "<span style='color:",mycols['nettleie'],"'>Nettleie: ",twodigits(nettleie),"</span>\n",
                              "<span style='color:",mycols['stotte'],"'>Strømstøtte: ",twodigits(stotte),"</span>\n\n",
                              "<span style='color:",mycols['totalpris'],"'><b>Reell pris:</b>",twodigits(totalpris),"</span>")

  text_this_month <-   paste0("<span style='text-decoration:underline'><b>",format(date,'%d.%m.%y')," kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1)," </b></span>\n",
                              "<span style='color:",mycols['spotpris'],"'>Spot: ",twodigits(spotpris),"</span>\n",
                              "<span style='color:",mycols['nettleie'],"'>Nettleie: ",twodigits(nettleie),"</span>\n",
                              "<span style='color:",mycols['stotte'],"'>Estimert støtte: ",twodigits(stotte)," (",twodigits(stotte_lower_CI),", ",twodigits(stotte_upper_CI),")","</span>\n\n",
                              "<span style='color:",mycols['totalpris'],"'><b>Reell pris:</b>\n",
                              "Estimat: ",twodigits(totalpris)," (",twodigits(totalpris_lower_CI),", ",twodigits(totalpris_upper_CI),")</span>")

  ret <- text_prev_month
  ret[!is.na(stotte_lower_CI)] <- text_this_month[!is.na(stotte_lower_CI)]

  return(ret)
}
