


nettleie <- fread("data/nettleie_nve_api.csv")

#ELVIA AS:
unique(nettleie[organisasjonsnr==980489698,.(organisasjonsnr,konsesjonar,fylkeNr,fylke,energileddEks,energileddInk)])
#organisasjonsnr konsesjonar fylkeNr     fylke energileddEks energileddInk
#1:       980489698    ELVIA AS      34 Innlandet         19.07         43.10
#2:       980489698    ELVIA AS      34 Innlandet         14.07         36.85
#3:       980489698    ELVIA AS      30     Viken         14.07         36.85
#4:       980489698    ELVIA AS      30     Viken         19.07         43.10

#LEDE
unique(nettleie[organisasjonsnr==979422679,.(organisasjonsnr,konsesjonar,fylkeNr,fylke,energileddEks,energileddInk)])
#organisasjonsnr konsesjonar fylkeNr                fylke energileddEks energileddInk
#1:       979422679     LEDE AS      38 Vestfold og Telemark            17         40.51
#2:       979422679     LEDE AS      30                Viken            17         40.51

### https://lede.no/priser/
### opplyser at deres nettleie er


# Glitre nett
unique(nettleie[organisasjonsnr==982974011,.(organisasjonsnr,konsesjonar,fylkeNr,fylke,energileddEks,energileddInk)])
#organisasjonsnr    konsesjonar fylkeNr                fylke energileddEks energileddInk
#1:       982974011 GLITRE NETT AS      11             Rogaland          26.6         52.51
#2:       982974011 GLITRE NETT AS      11             Rogaland          18.6         42.51
#3:       982974011 GLITRE NETT AS      38 Vestfold og Telemark          26.6         52.51
#4:       982974011 GLITRE NETT AS      38 Vestfold og Telemark          18.6         42.51
#5:       982974011 GLITRE NETT AS      42                Agder          18.6         42.51
#6:       982974011 GLITRE NETT AS      42                Agder          26.6         52.51

###
### https://www.glitreenergi-nett.no/kunde/alt-om-nettleiepriser/gebyrer-og-avgifter/
### Opplyser at deres nye nettavgifter er 38.65 dag, 26.65 natt


