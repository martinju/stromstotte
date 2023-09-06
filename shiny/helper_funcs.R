twodigits <- function(x){
  format(round(x,2),nsmall=2)
}

scaleFUN <- function(x) sprintf("%.2f", x)

textfunc <- function(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text_prev_month <-   paste0("<span style='text-decoration:underline'><b>",format(date,'%d.%m.%y')," kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1)," </b></span>\n",
                              "<span style='color:",mycols['spotpris'],"'>Spotpris: ",twodigits(spotpris),"</span>\n",
                              "<span style='color:",mycols['nettleie'],"'>Nettleie: ",twodigits(nettleie),"</span>\n",
                              "<span style='color:",mycols['stotte'],"'>Strømstøtte: ",twodigits(stotte),"</span>\n\n",
                              "<span style='color:",mycols['totalpris'],"'><b>Din strømpris:</b>",twodigits(totalpris),"</span>")

  text_this_month <-   paste0("<span style='text-decoration:underline'><b>",format(date,'%d.%m.%y')," kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1)," </b></span>\n",
                              "<span style='color:",mycols['spotpris'],"'>Spot: ",twodigits(spotpris),"</span>\n",
                              "<span style='color:",mycols['nettleie'],"'>Nettleie: ",twodigits(nettleie),"</span>\n",
                              "<span style='color:",mycols['stotte'],"'>Estimert støtte: ",twodigits(stotte)," (",twodigits(stotte_lower_CI),", ",twodigits(stotte_upper_CI),")","</span>\n\n",
                              "<span style='color:",mycols['totalpris'],"'><b>Din strømpris:</b>\n",
                              "Estimat: ",twodigits(totalpris)," (",twodigits(totalpris_lower_CI),", ",twodigits(totalpris_upper_CI),")</span>")

  ret <- text_prev_month
  ret[!is.na(stotte_lower_CI)] <- text_this_month[!is.na(stotte_lower_CI)]

  return(ret)
}

textfunc2 <- function(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text_prev_month <-   paste0("<span style='color:#000000; text-decoration:underline'><b>",format(date,'%d.%m.%y')," kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1)," </b></span>\n",
                              "<span style='color:",mycols['spotpris'],"'>Spotpris: ",twodigits(spotpris),"</span>",
                              "<span style='color:",mycols['nettleie'],"'>      Nettleie: ",twodigits(nettleie),"</span>\n",
                              "<span style='color:",mycols['stotte'],"'>Strømstøtte: ",twodigits(stotte),"</span>\n\n",
                              "<span style='color:",mycols['totalpris'],"'><b>Din strømpris: </b>",twodigits(totalpris),"</span>")

  text_this_month <-   paste0("<span style='color:#000000; text-decoration:underline'><b>",format(date,'%d.%m.%y')," kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1)," </b></span>\n",
                              "<span style='color:",mycols['spotpris'],"'>Spot: ",twodigits(spotpris),"</span>",
                              "<span style='color:",mycols['nettleie'],"'>      Nettleie: ",twodigits(nettleie),"</span>\n",
                              "<span style='color:",mycols['stotte'],"'>Estimert støtte: ",twodigits(stotte)," (",twodigits(stotte_lower_CI),", ",twodigits(stotte_upper_CI),")","</span>\n\n",
                              "<span style='color:",mycols['totalpris'],"'><b>Din strømpris: </b>",
                              twodigits(totalpris)," (",twodigits(totalpris_lower_CI),", ",twodigits(totalpris_upper_CI),")</span>")

  ret <- text_prev_month
  ret[!is.na(stotte_lower_CI)] <- text_this_month[!is.na(stotte_lower_CI)]

  return(ret)
}

textfunc2_new <- function(datetime,spotpris,nettleie,totalpris,stotte,mycols){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text <-   paste0("<span style='color:#000000; text-decoration:underline'><b>",format(date,'%d.%m.%y')," kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1)," </b></span>\n",
                              "<span style='color:",mycols['spotpris'],"'>Spotpris: ",twodigits(spotpris),"</span>\n",
                              "<span style='color:",mycols['nettleie'],"'>Nettleie: ",twodigits(nettleie),"</span>\n",
                              "<span style='color:",mycols['stotte'],"'>Strømstøtte: ",twodigits(stotte),"</span>\n\n",
                              "<span style='color:",mycols['totalpris'],"'><b>Din strømpris: </b>",twodigits(totalpris),"</span>")


  ret <- text
  return(ret)
}



textfunc_simple <- function(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text_this_month <-   paste0("<span style='color:",mycols['totalpris'],"'><b>Din strømpris kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1),": </b>\n",
                              twodigits(totalpris)," (",twodigits(totalpris_lower_CI),", ",twodigits(totalpris_upper_CI),")</span>")

  ret <- text_this_month
  ret

  return(ret)
}

textfunc_simple2 <- function(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols,fontsize){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text_this_month <-   paste0("<span style='color:#000000; font-size:",fontsize,".0pt'><b>Kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1),": </b>",
                              twodigits(totalpris)," (",twodigits(totalpris_lower_CI),", ",twodigits(totalpris_upper_CI),")</span>")

  ret <- text_this_month
  ret

  return(ret)
}

textfunc_simple3 <- function(datetime,spotpris,nettleie,totalpris,totalpris_lower_CI,totalpris_upper_CI,stotte,stotte_lower_CI,stotte_upper_CI,mycols,fontsize){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text_this_month <-   paste0("<span style='color:#000000><b>Kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1),": </b>",
                              twodigits(totalpris)," (",twodigits(totalpris_lower_CI),", ",twodigits(totalpris_upper_CI),")</span>")

  ret <- text_this_month
  ret

  return(ret)
}

textfunc_simple3_new <- function(datetime,spotpris,nettleie,totalpris,mycols,fontsize){
  date <- as.Date(datetime)
  start_hour <- lubridate::hour(datetime)

  text_this_month <-   paste0("<span style='color:#000000><b>Kl. ",sprintf("%02d", start_hour),"-",sprintf("%02d", start_hour+1),": </b>",
                              twodigits(totalpris),"</span>")

  ret <- text_this_month
  ret

  return(ret)
}

