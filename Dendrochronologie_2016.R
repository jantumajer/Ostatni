###########################################################################
### Zakladni operace s dendrochronologickymi daty v prostredi R
###########################################################################

library(dplR)

### Nejprve vyuzijeme balik dplR pro nacteni letokruhovych krivek ...
serie <- read.rwl('D:/tumajer/vyuka/trw_komplet.rwl', format=c("auto"))

# Jakou strukturu ma soubor letokruhovych krivek v prostredi R ?
View(serie)
  # Radky = roky, Sloupecky = vyvrty

# Graficke moznosti, jak si serie zobrazit
spag.plot(serie)
seg.plot(serie)

# Zakladni popisne statistiky
rwl.stats(serie)
  # Sens (sensitivita) = prumerna zmena v sirce dvou po sobe nasledujicich letokruhu
  # AR1 (autokorelace 1. radu) = korelace sirky letokruhu k sirce predchazejiciho letokruhu

#########################################
### Odstraneni vekoveho trendu ze serii
#########################################

spline <- detrend(serie, method="Spline", nyrs=50)
exponenciala <- detrend(serie, method="ModNegExp") # Negativni exponenciala nebo linearni trend (pokud se exponenciala nenafituje)
  # TRWmod = a*e^(b*rok)+c
prumer <- detrend(serie, method="Mean")
AR <- detrend(serie, method="Ar") # Residualni varianta splinu

### Porovnani zakladnich statistik hrubych dat 
statistiky.1 <- 
  rbind(colMeans((rwl.stats(serie))[,c(2:12)]),
      colMeans((rwl.stats(spline))[,c(2:12)]),
      colMeans((rwl.stats(exponenciala))[,c(2:12)]),
      colMeans((rwl.stats(prumer))[,c(2:12)]),
      colMeans((rwl.stats(AR))[,c(2:12)]))
rownames(statistiky.1) <- c("hruba data", "spline", "exponenciala", "prumer", "AR")
View(statistiky.1)
  # Srovnej predevsim sloupecky mean a stdev (hruba vs. detrendovana) a ar1 (standardni vs. residualni detrendovaci metody)

# Zakladni statistiky DETRENDOVANYCH dat
statistiky.2 <-   
  rbind(rwi.stats(spline),
        rwi.stats(exponenciala),
        rwi.stats(prumer),
        rwi.stats(AR))
rownames(statistiky.2) <- c("spline", "exponenciala", "prumer", "AR")
View(statistiky.2)
  # Rbar = prumerna korelace mezi vsemi moznymi dvojicemi serii
  # EPS = Rbar vazeny poctem serii (do jake miry dany soubor serii charakterizuje idealni chronologii tvorenou nekonecnym 
  # poctem vrtanych stromu)
    # Kdyz EPS>0.85 -> dostatecne robustni chronologie
        

########################################################
### Tvorba letokruhove chronologie
########################################################


chronologie <- chron(exponenciala, biweight=TRUE)
View(chronologie)
crn.plot(chronologie)

##################
### Ukazka vypoctu korelaci mezi klimatickymi promennymi a letokruhovou chronologii
### pomoci baliku treeclim
### Zang, Biondi (2015): treeclim: an R package for the numerical calibration of proxy-climate relationships. Ecography, 38(4), 431-436.
### https://cran.r-project.org/web/packages/treeclim/treeclim.pdf
##################

library(treeclim)


###########################################################################
# Popis vztahu mezi klimatickymi podminkami a sirkou letokruhu (growth-climate interaction)
# je nejcasteji zalozen na vypoctu Pearsonovych korelacnich koeficientu mezi 1] chronologii a 
# 2] casovou radou klimatickych dat v mesicnim kroku.
# Jako klimaticka data se nejcasteji pouzivaji prumerne mesicni teploty, suma mesicnich
# srazek nebo prumerne mesicni indexy sucha (ve Stredomori spise SPEI a SPI, ve vyssich zemepisnych sirkach spise scPDSI).

# Dendrochronologicky software pro vypocet korelaci: DendroClim 2002, treeclim (bootRes - starsi verze baliku treeclim)
#########################

# Nejprve je nutne ke chronologii nacist relevantni klimaticka data
teplota <- read.table("D:/tumajer/vyuka/temp.txt", header=FALSE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
scPDSI <- read.table("D:/tumajer/vyuka/spei.txt", header=FALSE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
srazky <- read.table("D:/tumajer/vyuka/prec.txt", header=FALSE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

klima <- list(teplota=teplota, srazky=srazky, scPDSI=scPDSI)

# Vlastni vypocet korelaci se spousti jednim jednoduchym prikazem
korelace <- dcc(chronologie, klima, selection=-6:9, method="correlation")
plot(korelace)

#################################
# Neco malo navic:
# Takto se analyza vztahu klimatu a rustu provadi nejcasteji, ale ma urcita metodicka omezeni.
# 1] pro kazdy klimaticky parametr dostanete za cele obdobi jedno cislo, ktere popisuje
# vliv daneho parametru na rust. Ten se ale mohl v case menit (napr. vlivem rostouci
# teploty nemusi byt teplota v soucasnosti tak dulezita pro rust -> pokles korelaci)

klouzave.korelace <- dcc(chronologie, teplota, selection = -6:9, method = "correlation", dynamic = "moving", win_size = 35, win_offset = 1)
plot(klouzave.korelace)

# 2] jednotlive mesicni hodnoty klimatickych dat mezi sebou koreluji, coz muze
# zkreslit vysledny obrazek o growth-climate vztazích.

response.function <- dcc(chronologie, klima, selection=-6:9, method="response")
plot(response.function)

#################################
# Az budete nekdy pouzivat treeclim samostatne na svych datech, nenechte se prekvapit:
# Treeclim urcuje hodnotu a statistickou vyznamnost Pearsonova korelacnich koeficientu
# pomoci bootstrapingu. Z toho plynou 2 veci, na ktere si je potreba dat pozor:
# 1] pokud treeclim pustite se stejnymi daty 2x po sobe, dostanete pokazde jine hodnoty
# korelacnich koeficientu (i kdyz zmeny byvaji minimalni).
# 2] muze nastat situace, kdy vyssi korelacni koeficient je nesignifikantni a nizsi
# korelacni koeficient je signifikantni (i kdyz se to stava jenom zridka)
#################################


###########################################################
### Analyza extremnich roku
# Extremni roky mohou vypovidat o disturbancich ruznych meritek
# Event years, Pointer years, Signatury
# Vyuziti baliku pointRes, blize viz van der Maaten-Theunissen et al. (2015): pointRes: An R package to analyze pointer years 
# and components of resilience. Dendrochronologia 35, 34-38. 
# https://cran.r-project.org/web/packages/pointRes/pointRes.pdf
###########################################################

library(pointRes)

pointer.years <- pointer.norm(serie, window=7, method.thresh="Neuwirth", N.thresh1=1, N.thresh2=1.28, N.thresh3=1.645, series.thresh=75)
  # Princip - uvnitr urciteho (zde 7leteho) okna provede Z-standardizaci a nasledne urci o kolik SD se hodnota stredoveho letokruhu lisi od prumeru
  # Pokud je odchylka vyssi nez N.thresh -> event year pro dany strom
  # Pokud je podil stromu s identifikovanym event year vyssi nez series.threshold -> pointer year pro cely soubor serii 

View(pointer.years$EYvalues) # Informace o event years - u kterych stromu se v kterem roce vyskytuje dana intenzita event years
View(pointer.years$out) # Informace o pointer years

# Graficke vystupy
event.plot(pointer.years)
pointer.plot(list(A=pointer.years))

##########################################################
# SEA - superposed epoch analysis
# Resi otazky, jake podminky panovaly nekolik let pred vynikem extremniho letokruhu a jak dlouho nasledky extremnich
# udalosti pretrvavaji v letokruhovem zaznamu
##########################################################

sea <- sea(chronologie, c(1942), lag=7)
View(sea)
  # se = standardizovany index chronologie v zadanych letech (lag=0), letech predchazejicich (lag<0) a nasledujicih (lag>0) 
  # p = signifikance ochylky od prumerneho indexu v ramci vstupni chronologie
foo <- sea$se.unscaled
names(foo) <- sea$lag
barplot(foo, col = ifelse(sea$p < 0.05, "grey30", "grey75"), ylab = "RWI", xlab = "Superposed Epoch")


#############################################################
### Hodnoceni resistence a resilience
# Resistence = schopnost odolavat pusobeni vnejsich rusivych vlivu
# Resilience = pri pusobeni rusiveho vliva silna odezva, ale rychly navrat do puvodniho stavu
# Zakladem je vymezeni pointeru, v jejichz casovem "okoli" se nasledne pocitaji specialni indexy
#############################################################

res <- res.comp(serie, nb.yrs=5, res.thresh.neg=25, series.thresh=50)
res.plot(res)

# Resistance = sirka letokruhu/prumerna sirka letokruhu za predchozi obdobi
# Resilience = prumerna sirka letokruhu za predchozi obdobi/prumerna sirka letokruhu za nasledujici obdobi
# Recovery = prumerna sirka letokruhu za nasledujici obdobi/sirka letokruhu
# Relative resilience = (prumerna sirka letokruhu za nasledujici obdobi - sirka letokruhu)/prumerna sirka letokruhu za predchozi obdobi





################################################
# Procvicovani - postup
################################################

procv.series <- read.rwl("C:/Users/Jan Tumajer/Desktop/skola/vyuka/Dendrochronologie/Dendro_R/Dendro_procvicovani/germ035.rwl")
# 21 stromu

max((rwl.stats(procv.series))["ar1"])
# PGN08A


sirka <- (rwl.stats(procv.series))[c(4,5)]
sirka$vahy <- sirka$year*sirka$mean
sum(sirka$vahy)/sum(sirka$year)
# 2.06

det <- detrend(procv.series, method="Spline", nyrs=50)

chronologie <- chron(det, biweight=TRUE)
crn.plot(chronologie)

pointer.years <- pointer.norm(procv.series, window=7, method.thresh="Neuwirth", N.thresh1=1, N.thresh2=1.28, N.thresh3=1.645, series.thresh=75)
View(pointer.years$out)


