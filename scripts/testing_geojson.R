
#install.packages("geojsonio")
library(rgdal)
# Data er EUREF89 UTM sone 33, 2d.
postnr_path <- "C:\\Users\\jullum\\Downloads\\Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON\\Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON.geojson"
postnr_path_NH <- "C:\\Users\\jullum\\Downloads\\Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON\\Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON_NO_HEADER.geojson"

postnr_path_test <- "C:\\Users\\jullum\\Downloads\\Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON\\MJtest.geojson.json"
postnr_path_test2 <- "C:\\Users\\jullum\\Downloads\\Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON\\MJtest.geojson2.json"

test = rgdal::readOGR(postnr_path_test2)
nycounties <- rgdal::readOGR("https://rstudio.github.io/leaflet/json/nycounties.geojson")

test2 <- geojsonio::geojson_read(postnr_path_test2,what = "sp")

test3 <- geojsonsf::geojson_sf(postnr_path_test2)

full <- geojsonsf::geojson_sf(postnr_path_NH)
#full2 <- geojsonsf::geojson_sf(postnr_path,expand_geometries = TRUE)



nett <- geojsonsf::geojson_sf("C:\\Users\\jullum\\Downloads\\NVE_60751B14_1667513019194_12948\\NVEData\\NettKonsesjonsomrade.geojson")

prisomrader <- geojsonsf::geojson_sf("C:\\Users\\jullum\\Downloads\\file.geojson")

prisomrader2=st_transform(prisomrader,crs = st_crs(25833))
st_crs(nett) <- 25833
st_crs(full) <- 25833

par(xaxs="r")
plot(prisomrader2["ElSpotOmr"],reset=FALSE,xlim=c(10000,10000))

axis(2)
plot(st_geometry(nett["eierNavn"]),add=T)
plot(st_geometry(full["postnummer"]),add=T)

library(ggplot2)
ggplot()+
  geom_sf(data=prisomrader2["ElSpotOmr"],size=1)+
  geom_sf(data=nett["eierNavn"],col=2,fill=NA)+
  geom_sf(data=full["postnummer"],col=3,fill=NA,lty=2)

ggplot()+
  geom_sf(data=prisomrader2["ElSpotOmr"],size=1.5)+
  geom_sf(data=nett["eierNavn"],col=2,fill=NA,size=1.5,lty=3)+
  geom_sf(data=full["postnummer"],col=3,fill=NA,lty=2)+
  coord_sf(default_crs = sf::st_crs(4326),xlim=c(10,15),ylim = c(64, 65.5), expand = FALSE)

ggplot()+
  geom_sf(data=prisomrader2["ElSpotOmr"],size=1.5)+
  geom_sf(data=nett["eierNavn"],col=2,fill=NA,size=1.5,lty=3)+
  geom_sf(data=full["postnummer"],col=3,fill=NA,lty=2)+
  coord_sf(default_crs = sf::st_crs(4326),xlim=c(11.8,12.3),ylim = c(64.9, 65.1), expand = FALSE)



#st_crs(full)==
st_crs(nett)==st_crs(prisomrader)

plot(nett["eierNavn"])
plot(prisomrader["ElSpotOmr"],reset=FALSE,alpha=0.5)
plot(nett["eierNavn"],add=T)

plot(nett["eierNavn"],reset=FALSE)
plot(full["postnummer"],add=T)

aa=st_crs(prisomrader["ElSpotOmr"])
st_as_sf

prisomrader["ElSpotOmr"]

st_crs(25833) # ESPG:25833 corresponding to "EUREF89 UTM sone 33, 2d" as both postnummer and nett are given in ref https://register.geonorge.no/epsg-koder?page=2

aaa=st_transform(prisomrader,crs = st_crs(25833))


st_crs(nett) <- 25833
st_crs(full) <- 25833

plot(nett["eierNavn"],reset=FALSE)
plot(full["postnummer"],add=T)
plot(aaa,add=T)
