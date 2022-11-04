

innrapportert_nettleie_251022.csv
  Nettleiedata fått på epost fra Roald Lien Glad 25.10.22

Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON_EKS_postnummeromradegrense.geojson:
  Postnummerområder fra Kartverket hentet via GeoNorge (https://kartkatalog.geonorge.no/metadata/postnummeromraader/462a5297-33ef-438a-82a5-07fff5799be3).
  Valg nedlastbar fil Geografisk område = Hele landet, Projeksjon = EUREF89 UTM sone 33, 2d,  format = GeoJSON.
  Pga innlesingsproblemer er ytterste lag i fila fjernet og kun postnummeromrader.postnummeromrade i lag 2 tatt med.
  Dette er gjort ved å fjerne rad 1 + 2 (frem til "{" ), og så komma i linje 1171608 samt alt nedenfor (postnummeromrader.postnummeromradegrense er altså ekskludert).
  MERK AT data feilaktig er oppført med koordinatsystem WGS84, longlat når den leses inn med geojsonsf::geojson_sf om man ikke velger noe annet selv.

NettKonsesjonsomrade.geojson
  Nettselskapsområder fra NVE hentet via https://nedlasting.nve.no/gis/.
  Valgt Nettanlegg -> Områdekonsesjonærer. Kartformat GeoJSON v1.0, Koordinatsystem: EUREF89 (UTM), UTOM-sone 33, utvalgsmetode: Overlapper, Dekningsområde: Landsdekkende (inkludert Svalbard). Oppgir epostadresse -> laster ned fra url i epost, og filen ligger under NVEData
  MERK AT data feilaktiv er oppført i koordinatsystem WGS84, longlat når den leses inn med geojsonsf::geojson_sf, om man ikke velger noe annet selv.

ELSpot_omraade.geojson
  Elspotområder fra NVE hentet "manuelt" fra karttjenesten til NVE: https://temakart.nve.no/tema/nettanlegg
  Under NVE Elspot områder trykker man på "Vis tabell" under ElSpot_omraade. Så huker man av alle de 5 områdene og trykker eksporter i tabben som dukker opp,
  og velger GeoJSON som format. Filen blir kalt file.geojson
  Disse data er i WGS84, longlat
