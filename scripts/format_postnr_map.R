
library(geojsonsf)
library(sf)

crs_EUREF89_UTM_sone_33 <- st_crs(25833)

postnr <- geojsonsf::geojson_sf("raw-data/Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON_EKS_postnummeromradegrense.geojson",
                                input = crs_EUREF89_UTM_sone_33$input,wkt = crs_EUREF89_UTM_sone_33$wkt)

prisomraader0 <- geojsonsf::geojson_sf("raw-data/ELSpot_omraade.geojson") # Default WGS84, longlat
nettselskap <- geojsonsf::geojson_sf("raw-data/NettKonsesjonsomrade.geojson",
                                     input = crs_EUREF89_UTM_sone_33$input,wkt = crs_EUREF89_UTM_sone_33$wkt)

prisomraader=st_transform(prisomraader0,crs = crs_EUREF89_UTM_sone_33)

postnr_prisomraader_area_mat <- matrix(NA,ncol=nrow(prisomraader),nrow=nrow(postnr))

for(i in seq_len(nrow(postnr))){
  for(j in seq_len(nrow(prisomraader))){
    area_intersection <- as.numeric(st_area(st_intersection(postnr[i,],prisomraader[j,])))
    if(length(area_intersection)>0){
      postnr_prisomraader_area_mat[i,j] <- area_intersection
    } else {
      postnr_prisomraader_area_mat[i,j] <- 0
    }
    print(c(i,j))
  }
}

postnr_nettselskap_area_mat <- matrix(NA,ncol=nrow(nettselskap),nrow=nrow(postnr))

for(i in seq_len(nrow(postnr))){
  for(j in seq_len(nrow(nettselskap))){
    area_intersection <- as.numeric(st_area(st_intersection(postnr[i,],nettselskap[j,])))
    if(length(area_intersection)>0){
      postnr_nettselskap_area_mat[i,j] <- area_intersection
    } else {
      postnr_nettselskap_area_mat[i,j] <- 0
    }
    print(c(i,j))
  }
}




