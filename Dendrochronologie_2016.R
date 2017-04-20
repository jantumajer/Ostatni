##################
### Ukazka vypoctu korelaci mezi klimatickymi promennymi a letokruhovou chronologii
### pomoci baliku treeclim
### Zang, Biondi (2015): treeclim: an R package for the numerical calibration of proxy-climate relationships. Ecography, 38(4), 431-436.
### https://cran.r-project.org/web/packages/treeclim/treeclim.pdf
##################

library(dplR)
library(treeclim)

### Nejprve vyuzijeme balik dplR pro nacteni letokruhovych krivek ...
serie <- read.rwl('F:/upor_n/TRW/trw_komplet.rwl', format=c("auto"))

# ... odstraneni vekoveho trendu ze serii ...
detrendovane.serie <- detrend(serie, method="ModNegExp")

# ... a zprumerovani do letokruhove chronologie.
chronologie <- chron(detrendovane.serie, biweight=TRUE, prewhitten=TRUE)
crn.plot(chronologie)

###########################################################################
# Popis vztahu mezi klimatickymi podminkami a sirkou letokruhu (growth-climate interaction)
# je nejcasteji zalozen na vypoctu Pearsonovych korelacnich koeficientu mezi 1] chronologii a 
# 2] casovou radou klimatickych dat v mesicnim kroku.
# Jako klimaticka data se nejcasteji pouzivaji prumerne mesicni teploty, suma mesicnich
# srazek nebo prumerne mesicni indexy sucha (ve Stredomori spise SPEI a SPI, ve vyssich zemepisnych sirkach spise scPDSI).

# Dendrochronologicky software pro vypocet korelaci: DendroClim 2002, treeclim (bootRes - starsi verze baliku treeclim)
#########################

# Nejprve je nutne ke chronologii nacist relevantni klimaticka data
teplota <- read.table("f:/upor_n/dendroclim_vypocet/temp.txt", header=FALSE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
scPDSI <- read.table("f:/upor_n/dendroclim_vypocet/spei.txt", header=FALSE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
srazky <- read.table("f:/upor_n/dendroclim_vypocet/prec.txt", header=FALSE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

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
