
library(geojsonsf)
library(sf)
library(future.apply)
library(data.table)

Sys.setlocale(locale='en_US.UTF-8') # Also OK for reading Norwegian letters

crs_EUREF89_UTM_sone_33 <- st_crs(25833)

postnr <- geojsonsf::geojson_sf("raw-data/Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON_EKS_postnummeromradegrense.geojson",
                                input = crs_EUREF89_UTM_sone_33$input,wkt = crs_EUREF89_UTM_sone_33$wkt)

prisomraader0 <- geojsonsf::geojson_sf("raw-data/ELSpot_omraade.geojson") # Default WGS84, longlat
nettselskap <- geojsonsf::geojson_sf("raw-data/NettKonsesjonsomrade.geojson",
                                     input = crs_EUREF89_UTM_sone_33$input,wkt = crs_EUREF89_UTM_sone_33$wkt)

prisomraader=st_transform(prisomraader0,crs = crs_EUREF89_UTM_sone_33)


postnr_vec <- postnr$postnummer
nettselskap_vec <- make.unique(nettselskap$eierNavn,sep="_dup_")
prisomraader_vec <- prisomraader$ElSpotOmr

konsesjonType_vec <- nettselskap$konsesjonType
eierType_vec <- nettselskap$eierType
poststed_vec <- postnr$poststed
kommune_vec <- postnr$kommune

# Getting the area of the intersections between the polygons

plan(multisession)

intersection_area_func <- function(index_vec,poly1,poly2){
  area_intersection <- as.numeric(st_area(st_intersection(st_make_valid(poly1[index_vec[1],]),st_make_valid(poly2[index_vec[2],]))))
  if(length(area_intersection)>0){
    return(area_intersection)
  } else {
    return(0)
  }
}

# Nettselskap vs prisomraader
nettselskap_prisomraader_index_mat <- expand.grid(seq_len(nrow(nettselskap)),seq_len(nrow(prisomraader)))
nettselskap_prisomraader_area_vec <- future_apply(nettselskap_prisomraader_index_mat,MARGIN = 1,FUN=intersection_area_func,poly1=nettselskap,poly2=prisomraader)

nettselskap_prisomraader_area_mat <- matrix(nettselskap_prisomraader_area_vec,nrow=nrow(nettselskap),ncol=nrow(prisomraader))
rownames(nettselskap_prisomraader_area_mat) <- nettselskap_vec
colnames(nettselskap_prisomraader_area_mat) <- prisomraader_vec

nettselskap_prisomraader_area_dt <- as.data.table(nettselskap_prisomraader_area_mat,keep.rownames = "nettselskap")
nettselskap_prisomraader_area_dt[,konsesjonType:=konsesjonType_vec]
nettselskap_prisomraader_area_dt[,eierType:=eierType_vec]

setcolorder(nettselskap_prisomraader_area_dt,c("nettselskap","konsesjonType","eierType"))
setkey(nettselskap_prisomraader_area_dt,nettselskap)

fwrite(nettselskap_prisomraader_area_dt,"data/nettselskap_prisomraader_area.csv")



# Postnr vs prisomraader
postnr_prisomraader_index_mat <- expand.grid(seq_len(nrow(postnr)),seq_len(nrow(prisomraader)))
postnr_prisomraader_area_vec <- future_apply(postnr_prisomraader_index_mat,MARGIN = 1,FUN=intersection_area_func,poly1=postnr,poly2=prisomraader)

postnr_prisomraader_area_mat <- matrix(postnr_prisomraader_area_vec,nrow=nrow(postnr),ncol=nrow(prisomraader))
rownames(postnr_prisomraader_area_mat) <- postnr_vec
colnames(postnr_prisomraader_area_mat) <- prisomraader_vec

postnr_prisomraader_area_dt <- as.data.table(postnr_prisomraader_area_mat,keep.rownames = "postnr")
postnr_prisomraader_area_dt[,poststed:=poststed_vec]
postnr_prisomraader_area_dt[,kommune:=kommune_vec]

setcolorder(postnr_prisomraader_area_dt,c("postnr","poststed","kommune"))
setkey(postnr_prisomraader_area_dt,postnr)

fwrite(postnr_prisomraader_area_dt,"data/postnr_prisomraader_area.csv")




# Postnr vs Nettselskap
postnr_nettselskap_index_mat <- expand.grid(seq_len(nrow(postnr)),seq_len(nrow(nettselskap)))
postnr_nettselskap_area_vec <- future_apply(postnr_nettselskap_index_mat,MARGIN = 1,FUN=intersection_area_func,poly1=postnr,poly2=nettselskap)

postnr_nettselskap_area_mat <- matrix(postnr_nettselskap_area_vec,nrow=nrow(postnr),ncol=nrow(nettselskap))
rownames(postnr_nettselskap_area_mat) <- postnr_vec
colnames(postnr_nettselskap_area_mat) <- nettselskap_vec

postnr_nettselskap_area_dt <- as.data.table(postnr_nettselskap_area_mat,keep.rownames = "postnr")
postnr_nettselskap_area_dt[,poststed:=poststed_vec]
postnr_nettselskap_area_dt[,kommune:=kommune_vec]

setcolorder(postnr_nettselskap_area_dt,c("postnr","poststed","kommune"))
setkey(postnr_nettselskap_area_dt,postnr)


fwrite(postnr_nettselskap_area_dt,"data/postnr_nettselskap_area.csv")





