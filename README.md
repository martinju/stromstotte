Estimering av strømstøtte
================
Martin Jullum, Seniorforsker Norsk Regnesentral

## Bakgrunn

Statens strømstøtteordning gir en støtte til husholdninger per kWh som
fastsettes etter månedsslutt basert på månedens gjennsomnittlige
spotpris. Støtteordningen tar altså ikke hensyn til spotprisen når man
bruker den, og siden dagens spotpris fastsettes kun én dag før, vet man
heller ikek hvor stor støtten blir før nest siste dag i måneden. Med
andre ord vet man ikke hvor mye man i praksis må betale for strømmen når
man bruker den! [VG](https://www.vg.no/spesial/2022/stromprisene/)
presenterer gjennomsnittlig spotpris (og dermed strømstøtte under
antagelsen om at resten av måneden har samme gjennomsnittlige spotpris).
Et slikt regnestykke tar imideltid ikke hensyn til hvilke ukedager som
gjenstår i måneden (spotprisen er typisk lavere i helgene), og hvordan
spotprisen har utviklet seg de siste dagene. Og kanskje viktigst av alt:
Det sier heller ingenting om usikkerheten. Etter hvert som måneden går
observerer man jo en større og større andel av den månedens spotpriser,
og blir dermed sikrere og sikrere på hva strømstøtten lander på –
uansett hva slags tallmateriale man legger til grunn!

Kan vi si noe mer fornuftig om hva vi til en hver tid tror månedens
strømstøtte ender på? Og hvor sikre er vi på det vi tror?

I et forsøk på å svare på disse spørsmålene har jeg estimert den
gjennomsnittlige spotprisen for måneden gjennom å modellere og så
predikere daglig spotpris for de gjenværende dagene i måneden. Dette
gjøres separat for hver av de 5 prisområdene i Norge. Kombinert med den
gjennomsnittlige spotprisen så langt i måneden gir dette et estimat på
månedens strømstøtten med usikkerhetsintervaller. Estimatene oppdateres
automatisk ca kl. 13.30 når neste dags spotpris blir offentliggjort.

## Estimater

Tabellen nedenfor viser estimat + 95% konfidensintervall for månedlig
strømpris og dertilhørende strømstøtte for hver av de fem prisområdene
med siste tilgjengelige spotpriser. Konfidensintervallet viser hvilket
intervall den faktiske månedelige spotprisen/strømstøtten vil ligge
innenfor i 95% av gangene dette gjentas. I tillegg til estimatene vises
“Absolutt nedre grense” svarende til at strømprisen blir 0 NOK/kWh
resten av måneden, mens “Så langt denne måneden” angir hva månedelig
spotpris/strømstøtte om gjennomsnittet resten av måneden blir helt likt
slik det har vært så langt denne måneden. Tallene inkluderer moms, men
ikke verken fast eller variabel nettleie, eller andre påslag fra
nettleverandør.

<img src="output/current_estimate_tab.png" alt="Siste estimerte priser per område" width="100%" />

Figuren nedenfor gir en visuell fremstilling av utviklingen av
strømstøttetallene fra tabellen overfor slik de har endret seg fra dag
til dag så langt denne måneden.

<img src="output/current_estimated_compensation.png" alt="Estimert kompensasjon per område per dag" width="100%" />

Figuren nedenfor gir en visuell fremstilling av utviklingen av tallene
knyttet til månedlig spotpris fra tabellen overfor slik de har endret
seg fra dag til dag så langt denne måneden. Daglig spotpris er markert
med prikker for å forklare hvorfor utviklingen er som den er.

<img src="output/current_estimated_mean_price.png" alt="Estimerte gjennomsnittspriser per område per dag" width="100%" />

## Metode (for spesielt interesserte)

Estimering av fremtidige strømpriser er med dagens kraftmarket og
internasjonale kraftledninger ingen enkel oppgave. Hvor mye det regner
på vestlandet, blåser i Tyskland og forbrukes i Sverige har betydning
for strømprisen i Oslo. Modellen som er lagt til grunn her er en
relativt enkel modell bygget på historisk spotpriser mellom 1.november
2021 og 31.august 2022. Modellen modellerer daglige spotpriser separat
for hvert prisområde og hensyntar at morgendagens spotpris empirisk
avhenger av spotprisen de siste dagene og hvilken ukedag det er.

#### Modelltilpasning

Teknisk er modellene som brukes av typen
[ARIMA](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average)
med ukedag som ekstra kategorisk kovariat. Den presise
modellspesifikasjoen for hvert område bestemmes med å tilpasse mange
varianter av denne modelltypen, og så bruke modellvalgskriteriet
[AIC](https://en.wikipedia.org/wiki/Akaike_information_criterion) til å
velge hvilken modell som passer best til dataene i hvert område.
Parameterne i modellene estimeres ved hjelp av [maximum
likelihood](https://en.wikipedia.org/wiki/Maximum_likelihood_estimation).
At det tilpasses individuelle modeller til hver prisområde er også
grunnen til at estimatene for prisområde NO1, NO2 og NO3 ikke er like
selv om spotprisen har vært lik i disse områdene den siste tiden.

#### Prediksjon av spotpriser og estimering av strømstøtte

Estimerte månedelige spotpriser beregnes ved å for hvert område simulere
spotpriser for de resterende dagene i måneden fra de tilpassede
modellene mange (10000) ganger. Sammen med observert spotpris så langt i
måneden får man da 10000 rekker med simulerte daglige spotpriser, som
man kan ta gjennomsnittet av for å få 10000 simulerte månedelig
spotpriser. Ved å beregne strømstøtten (90% over 70 øre/kWh eks. MVA)
for hver simulerte månedlig spotpris får man også 10000 simulerte
strømstøtter. Estimatetene for strømstøtten er medianen i disse
simulerte strømstøttene, mens 95% konfidensintervallet er basert på
2.5%- og 97.5%-kvantilene. Tilsvarende gjelder estimater for måndelige
spotpriser.

Bakgrunnen for å benytte simulering for å bestemme disse estimatene er
at avhengigheten predikert spotpris $p$ dager frem og $q$ dager frem,
gjør det krevende å regne seg frem til analytiske uttrykk for
usikkerheten til gjennomsnittet av dem.

#### Utvidelser og antagelser

Denne relativt enkle modellen tar ikke hensyn til værprognoser. Den tar
heller ikke hensyn til årstid annet enn gjennom de siste dagers
observerte spotpriser. Dette, samt priser fra fremtidskontrakter vil
muligens kunne øke presisjonen til estimatene noe. Modellen antar også
at avhengigheten mellom de daglige spotprisene er lik nå som de var i
hele estimeringsperioden (november 2021 til august 2022). Det er sikkert
ikke 100% korrekt, men er neppe totalt feil og gir tilstrekkelig med
data til å få god presisjon på parameterestimatene. Tidsperioden er
valgt med omhu da strømprisene og de daglige svingningene i disse for
alvor skjøt fart på senhøsten 2021.

Usikkerhetsestimatene antar implisitt at estimert modell er sann. Det
vil si at parameterusikkerhet og modellvalgsusikkerhet ikke er
hensyntatt. Feilleddet i modellene (som antas uavhengig normalfordelte)
inkluderes imidlertid i modelleringen, sammen med tidsavhengigheten i
fremtidige spotpriser. Ekskluderingen av usikkerheten knyttet til
parameterestimering og modellvalg vil kunne øke bredden på
usikkerhetsestimatene til strømstøtten noe, men ikke betydelig.
Validering tilbake i tid viser at dekningsgraden på
konfidensintervallene treffer nært nominelt nivå.

<span style="float:right">All modellering av Martin Jullum
(<jullum@nr.no>), Seniorforsker Norsk Regnesentral</span>
