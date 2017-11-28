library(Rcmdr)

####################
# Analyza fytocenologickeho snimku metodou PCA
# Herben, Munzbergova (2003): Zpracovani fytocenologickych dat v prikladech. Cast I: Data o druhovem slozeni. 118 s.
####################

# Nacteme usporadana data o druhovem slozeni (pokryvnosti) 5 fytocenologickych snimku (druhy - sloupecky, snimky - radky)
snimek <- readXL("F:/vyuka/Ekologie/R/vzor_data.xlsx", rownames=TRUE, header=TRUE, na="", sheet="snimek_doR", stringsAsFactors=TRUE)

library(vegan) # R-package, ktery umoznuje ordinacni analyzy dat (skupina statistickych analyz, kam spada i PCA)
ord <- rda(snimek)
summary(ord)
	# Importance of components - podil variability vysvetleny jednotlivymi osami
	# Species scores - vektory definujici polohu jednotlivych environmentalnich promennych (zde druhu) v prostoru
	# Site scores - body definujici polohu jednotlivych zaznamu (zde fytocenologickych snimku) v prostoru

# Vysledky PCA se standardne zobrazuji ve forme PCA biplotu
	# vodorovna osa - nejcasteji prvni osa PCA (ta, ktera vysvetluje nejvice variability), svisla osa - nejcasteji druha osa PCA
	# sipky - pokryvnost druhu
	# body - fytocenologicke snimky

par(mfrow=c(1,2))
biplot (ord, choices=c(1,2), display = c('species','sites'), type = c('text', 'text'), scaling=0) # Bez skalovani druhovych vektoru - delka vektoru~dominance druhu (malo dominantni druh jsou necitelne)
biplot (ord, choices=c(1,2), display = c('species','sites'), type = c('text', 'text'), scaling=-1) # Skalovani druhovych vektoru - citelne i malo zastoupene druhy, ale delka sipky neodpovida dominanci druhu

# Interpretace - PC1 je linearni kombinaci vstupnich promennych, ktera vysvetluje nejvetsi podil variability mezi vstupnimi promennymi
# PC2 je linearni kombinaci vstupnich promennych, ktera vysvetluje 2. nejvetsi mnozstvi variability, a nekoreluje s PC1
# Pokud jdou dva vektory stejnym smerem - lze predpokladat kladnou korelaci promennych (tj. druhy maji k sobe v ramci fytocenologickych snimku podobne rozsireni/naroky)
# Kolmy prumet bodu na vektor - odhad hodnoty vektoru (druhu) na dane plose (tj. jake druhy dominuji na danem fytocenologickem snimku) 


################## 
# Priklad PCA na jinych nez fytocenologickych datech
# Datovy soubor popisujici formy poskozeni smrkovych prostu v MS Beskydech
##################
poskozeni <- readXL("F:/vyuka/Ekologie/R/vzor_data.xlsx", rownames=TRUE, header=TRUE, na="", sheet="poskozeni", stringsAsFactors=TRUE)

# Otazka je, jak otestovat, jak spolu jednotliva poskozeni souvisi ?

# 1. moznost - korelacni koeficient a test jeho statistickeho vyznamu
cor.test(poskozeni$R_DRYTREETOP, poskozeni$R_PEELING, alternative="two.sided", method="spearman")
	# Nevyhoda - mozne testovat jenom dvojici parametru

# 2. moznost - korelacni matice
rcorr.adjust(poskozeni[,c("R_BREAK","R_DRYTREETOP","R_PEELING","R_REDUCEDINCREMENT","R_RESIN")], type="spearman", use="complete")
	# Nevyhoda - neprehlednost

# 3. moznost - analyza hlavnich komponent
library(vegan)
ord <- rda(poskozeni, scaled=T)
summary(ord)
biplot (ord, choices=c(1,2), display = c('species','sites'), type = c('text', 'points'), scaling=0)
plot(ord)

#######################################
# Prostorova autokorelace, Moranovo I
#######################################

prostorovaA <- readXL("F:/vyuka/Ekologie/R/vzor_data.xlsx", rownames=TRUE, header=TRUE, na="", sheet="MoranI", stringsAsFactors=TRUE)
# Data o produktivite a strukture plantazi akatu, ktery se pouziva k rekultivaci post-tezebnich lokalit (Carl et al. (in prep): iForest)
# Do jake miry ovlivnuje vek a hustota stromu (nezavisle promenne) v plantazi produktivitu biomasy (zavisla promenna)?
# nezavislych promennych je vice a jsou spojite -> pouzijeme regresni model

lm.model <- lm(prostorovaA$Biomass ~ prostorovaA$Age + prostorovaA$NumOfPlants)
summary(lm.model)
# Dokazali jsme, ze produktivita biomasy v prumeru roste s vekem i hustotou plantaze
	# 1 strom navic na ha ~ 0.01 t.ha/rok
	# 1 rok stari porostu ~ 0.04 t.ha/rok
# Kvalita modelu je pomerne vysoka (R2=50 %)

### Je to ale spravne ?
#
#
#
### ... pouze, pokud jsou splneny vsechny vstupni predpoklady linearni regrese !!!!
# Napr. nezavislost promennych - hodnota produkce biomasy v jednom bode neovlivnuje produkci biomasy ve svem okoli.
# Tzn.v nasem souboru dat se nesmi v prostoru vyskytovat shluky s vysokymi a nizkymi hodnotami v prostoru (tzv. prosotorova autokorelace), ale hodnoty produkce by mely byt rozmisteny zcela nahodne 

# Jak to overit? - Moranovo I (obdoba korelacniho koeficientu pro hodnoceni sily prostorove autokorelace)
library(ape); library(spdep)

# 1] Nutne vytvorit inverzni matici vzdalenosti - nepouzivat souradny system WGS, ale UTM nebo SJTSK !!!!!!!!! 
dists <- as.matrix(dist(prostorovaA[c("LonD2","LatD2")]))
dists.inv <- 1/dists
diag(dists.inv) <- 0
lw1 <- mat2listw(dists.inv, style="W") # Z inverzni matice vzdalenosti vytvorim list

# 2] Vlastni vypocet Moranova I
Moran.I(prostorovaA$Biomass, dists.inv)
# p-hodnota << 0.05   -> data jsou zatizena silnou prostorovou autokorelaci, a proto pro jejich analyzu NEMUZEME POUZIT OBYCEJNY REGRESNI MODEL !!!

### Reseni - pouziti regresniho modelu zohlednujiciho prostorovou autokorelaci
# Spatial Lag Regression
# zavisla promenna = zavisla promenna*vektor vzdalenosti(rho) + nezavisle promenne...

slr.model <- lagsarlm(Biomass ~ Age + NumOfPlants, prostorovaA, lw1)
summary(slr.model)
