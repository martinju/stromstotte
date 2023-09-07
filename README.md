Estimering av strømstøtte
================

*Av [Martin Jullum, Norsk
Regnesentral](https://nr.no/ansatte/martin-jullum/).*

# [dinstrompris.no](https://dinstrompris.no)

På [dinstrompris.no](https://dinstrompris.no) kan du ved å taste inn
ditt postnummer, få opp din strømpris akkurat nå og ut morgendagen.
Prisen er medregnet både spotpris, nettleie og estimert strømstøtte.

Dette repoet inneholder all kode bak nettsiden, kode for
prognosemodeller, samt daglig oppdaterte spotdata og prognoser for
strømstøtten.

**MERK: Etter at regjeringen endret strømstøtteordningen til timesbasert
avregning fra 1.september 2023, er den faktiske strømstøtten kjent
umiddelbart. Det er dermed ikke lenger behov for å estimere
strømstøtten, som var hovedformålet med dette prosjektet.
Prognosemodellene ble derfor stoppet 30.08.2023. Dokumentasjonen av
hvordan modelleringen ble utført er imidlertid beholdt i sin helhet da
den kan være av metodisk interesse.**

Daglige oppdaterte spotpriser, både med og uten justering for
strømstøtten etter 1.september 2023 kan hentes fra følgende daglig
oppdaterte json/csv-fil:  
<https://raw.githubusercontent.com/martinju/stromstotte/master/data/historic_filtered_prices_sept23_system.json>
<https://raw.githubusercontent.com/martinju/stromstotte/master/data/historic_filtered_prices_sept23_system.csv>

Tilsvarende historiske spotpriser med og uten endelig justering for
strømstøtte før 1.september 2023 kan hentes her:
<https://raw.githubusercontent.com/martinju/stromstotte/master/data/historic_filtered_prices_original_system.json>
<https://raw.githubusercontent.com/martinju/stromstotte/master/data/historic_filtered_prices_original_system.csv>

Data/prognoser er sist oppdatert 07.09.2023, kl. 12.22 med spotpriser
for 08.09.2023.

Litt bakgrunn om strømstøtteestimering som ble benyttet for
strømstrøtteordningn som lå til grunn før 1.september 2023, og etter
hvert ledet frem til [dinstrompris.no](https://dinstrompris.no), finnes
[her](https://martinjullum.com/sideprojects/stromstotte/).

------------------------------------------------------------------------

All modellering [Martin Jullum](https://martinjullum.com)
(<jullum@nr.no>), [Seniorforsker Norsk
Regnesentral](https://nr.no/ansatte/martin-jullum/)
