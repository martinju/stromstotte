## Endringslogg dinstrompris.no

### v 0.2.3 [26.10.24]
- Nordpool endret sitt API fra 15.oktober, slik at denne siden gikk ned. Har nå gått over til å hente fra en annen kilde. Data 15.10-25.10 ikke tilgjengelig ennå.

### v 0.2.2 [05.01.24]
- Oppdatert terskel for når strømstøtten slår inn fra 0.70 NOK/kWh til 0.73 NOK/kWh fra 01.01.24, ref [regjeringen.no](https://www.regjeringen.no/no/aktuelt/str%C3%B8mst%C3%B8tten-oker-til-73-ore-kwh/id2898439/)

### v 0.2.1 [03.11.23]
- Oppdatert variabel nettleie for største nettselskap (ikke effekttariff).
- Fikset korrekt håndtering av egen helgesats

### v 0.2.0 [07.09.23]
- Oppdatert systemet til ny strømstøtteordning (underliggende system, visualiseringer og forklaringstekst)
- Stoppet prognosemodeller og automatisk generering av relaterte figurer da de ikke lenger er i bruk med ny strømstøtteordning
- Transformert historiske strømstøtte data til nytt dataformat

### v 0.1.5 [16.01.23]
- Justert nettleie for redusert elavgift
- Manuelt kontrollert/justert de 10 største nettselskapene i Norge og lagt inn kommentar om mulig feil for øvrige.
- Oppdatert GitHub repo readme

### v 0.1.4 [21.12.22]
- La til disclaimer om intet ansvar om feil
- Henter data lokaltr istedet for direkte fra github ved hver sesjon

### v 0.1.3 [19.12.22]
- Oppsummerte strømpriskomponentene i "din strømpris består av"
- La inn info om kapasitetsledd under "øvrige tilleg gpå din strømregning"
- Linket til øvrige tabs fra forside

### v 0.1.2 [18.12.22]
- La til valg av nettselskap/prisområde kun når det er nødvendig (flere enn en mulighet)
- La til postnummer i tittel på plotly

### v 0.1.1 [16.12.22]
- Delte opp i 3 plott (nå (enkel), nå (detaljert), historisk)
- Diverse visuelle forbedringer av nå (enkel) for å tilpasse noe bedre til mobilskjerm
- Ny bakgrunnsfarge og diverse visuelle oppdateringer i alle plott
- Flyttet endringslogg til om siden

### v 0.1.0 [06.12.22]
- Bugfix: konfidensintervall feil vei for strømstøtte
- La til endringslogg
- La til forklaring av strømstøtteestimering
- La til midlertidig tracking fra Google analytics





