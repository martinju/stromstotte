
# Here I should first check postnr vs prisområde (and save the interactions) +
# + prisområde vs netteier and save the interactions when they are nonzero.
# Then for each postnr/prisområde combination, check the interactions for the netteier
# that are in this area.

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

nettselskap <- nettselskap[nettselskap$eierType=="EVERK",] # Only care about EVERK



postnr_vec <- postnr$postnummer
nettselskap_vec <- nettselskap$eierNavn
prisomraader_vec <- prisomraader$ElSpotOmr

konsesjonType_vec <- nettselskap$konsesjonType
eierType_vec <- nettselskap$eierType

poststed_vec <- postnr$poststed
kommune_vec <- postnr$kommune

# Getting the intersections between the polygons

plan(multisession)

intersection_area_func <- function(index_vec,poly1,poly2){
  area_intersection <- as.numeric(st_area(st_intersection(st_make_valid(poly1[index_vec[1],]),st_make_valid(poly2[index_vec[2],]))))
  if(length(area_intersection)>0){
    return(area_intersection)
  } else {
    return(0)
  }
}

intersection_poly_func <- function(index_vec,poly1,poly2){
  intersection <- st_intersection(st_make_valid(poly1[index_vec[1],]),st_make_valid(poly2[index_vec[2],]))
  return(intersection)
}

intersection_polylist_func <- function(index_vec,polylist1,polylist2){
  intersection <- st_intersection(st_make_valid(polylist1[[index_vec[1]]]),st_make_valid(polylist2[[index_vec[2]]]))
  return(intersection)
}


area_poly_func <- function(poly){
  area <- as.numeric(st_area(poly))
  if(length(area)>0){
    return(area)
  } else {
    return(0)
  }
}

# Nettselskap vs prisomraader
nettselskap_prisomraader_index_mat <- expand.grid(seq_len(nrow(nettselskap)),seq_len(nrow(prisomraader)))
nettselskap_prisomraader_int_poly <- future_apply(nettselskap_prisomraader_index_mat,MARGIN = 1,FUN=intersection_poly_func,poly1=nettselskap,poly2=prisomraader,future.seed=TRUE)
nettselskap_prisomraader_int_area <- future_lapply(nettselskap_prisomraader_int_poly,area_poly_func,future.seed=TRUE)

nettselskap_prisomraader_index_dt <- data.table(nettselskap =nettselskap_vec[nettselskap_prisomraader_index_mat[,1]],
                                                konsesjonType = konsesjonType_vec[nettselskap_prisomraader_index_mat[,1]],
                                                eierType = eierType_vec[nettselskap_prisomraader_index_mat[,1]],
                                           prisomraade = prisomraader_vec[nettselskap_prisomraader_index_mat[,2]],
                                           area_nettselskap_prisomraader = unlist(nettselskap_prisomraader_int_area))
nettselskap_prisomraader_index_dt[,id_nettselskap_prisomraader:=.I]
nettselskap_prisomraader_index_dt <- nettselskap_prisomraader_index_dt[area_nettselskap_prisomraader>100]

#fwrite(nettselskap_prisomraader_area_dt,"data/nettselskap_prisomraader_area.csv")



# Postnr vs prisomraader
postnr_prisomraader_index_mat <- expand.grid(seq_len(nrow(postnr)),seq_len(nrow(prisomraader)))
#postnr_prisomraader_index_mat <- head(postnr_prisomraader_index_mat,100) # For testing to complete inn less time
postnr_prisomraader_int_poly <- future_apply(postnr_prisomraader_index_mat,MARGIN = 1,FUN=intersection_poly_func,poly1=postnr,poly2=prisomraader,future.seed=TRUE)
postnr_prisomraader_int_area <- future_lapply(postnr_prisomraader_int_poly,area_poly_func,future.seed=TRUE)

postnr_prisomraader_index_dt <- data.table(postnr =postnr_vec[postnr_prisomraader_index_mat[,1]],
                                           poststed = poststed_vec[postnr_prisomraader_index_mat[,1]],
                                           kommune = kommune_vec[postnr_prisomraader_index_mat[,1]],
                                           prisomraade = prisomraader_vec[postnr_prisomraader_index_mat[,2]],
                                           area_postnr_prisomraader = unlist(postnr_prisomraader_int_area))
postnr_prisomraader_index_dt[,id_postnr_prisomraader:=.I]
postnr_prisomraader_index_dt <- postnr_prisomraader_index_dt[area_postnr_prisomraader>100]

# merging to get full table to check intersection on
postnr_nettselskap_prisomraader_index_dt <- merge(nettselskap_prisomraader_index_dt,postnr_prisomraader_index_dt,by = "prisomraade",all=F,allow.cartesian = TRUE)

postnr_nettselskap_prisomraader_index_mat <- as.matrix(postnr_nettselskap_prisomraader_index_dt[,.(id_nettselskap_prisomraader,id_postnr_prisomraader)])

postnr_nettselskap_prisomraader_int_poly <- future_apply(postnr_nettselskap_prisomraader_index_mat,MARGIN = 1,
                                                         FUN=intersection_polylist_func,
                                                         polylist1=nettselskap_prisomraader_int_poly,
                                                         polylist2=postnr_prisomraader_int_poly,future.seed=TRUE)
postnr_nettselskap_prisomraader_int_area <- future_lapply(postnr_nettselskap_prisomraader_int_poly,area_poly_func,future.seed=TRUE)

postnr_nettselskap_prisomraader_index_dt[,area_postnr_nettselskap_prisomraader:=unlist(postnr_nettselskap_prisomraader_int_area)]
postnr_nettselskap_prisomraader_index_dt[,id_postnr_nettselskap_prisomraader:=.I]
postnr_nettselskap_prisomraader_index_dt <- postnr_nettselskap_prisomraader_index_dt[area_postnr_nettselskap_prisomraader>100]

simple_postnr_nettselskap_prisomraader_dt <- unique(postnr_nettselskap_prisomraader_index_dt[,.(postnr,nettselskap,prisomraade,intersection_area =area_postnr_nettselskap_prisomraader)])
setorder(simple_postnr_nettselskap_prisomraader_dt,postnr,-intersection_area,nettselskap)

fwrite(simple_postnr_nettselskap_prisomraader_dt,"data/simple_postnr_nettselskap_prisomraader_dt.csv")
fwrite(postnr_nettselskap_prisomraader_index_dt,"data/complete_postnr_nettselskap_prisomraader_dt.csv")


nettselskap_prisomraader_int_id_vec <- which(nettselskap_prisomraader_int_area>0)
nettselskap_prisomraader_int_poly0 <- nettselskap_prisomraader_int_poly[nettselskap_prisomraader_int_id_vec]

postnr_prisomraader_int_id_vec <- which(postnr_prisomraader_int_area>0)
postnr_prisomraader_int_poly0 <- postnr_prisomraader_int_poly[postnr_prisomraader_int_id_vec]

postnr_nettselskap_prisomraader_int_id_vec <- which(postnr_nettselskap_prisomraader_int_area>0)
postnr_nettselskap_prisomraader_int_poly0 <- postnr_nettselskap_prisomraader_int_poly[postnr_nettselskap_prisomraader_int_id_vec]


save(postnr_nettselskap_prisomraader_int_poly0,
     postnr_prisomraader_int_poly0,
     nettselskap_prisomraader_int_poly0,
     nettselskap_prisomraader_int_id_vec,
     postnr_prisomraader_int_id_vec,
     postnr_nettselskap_prisomraader_int_id_vec,
     file = "data/postnr_nettselskap_prisomraader_intersection_polygon_lists.RData")



