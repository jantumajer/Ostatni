###################################################
### Linearni smiseny model
###################################################

library(lme4)
library(piecewiseSEM) 

### Prikladova data
# Casove rady o prumernem rustu smrku ztepileho a prislusne klimaticke a environmentalni parametry
# Growth - prumerna sirka letokruhu 40leteho smrku
# Temp - prumerna jarni teplota (brezen-kveten) [°C]
# N - depozice dusiku modelovana podle Oulehle et al. (2016) [kg.ha-1.rok-1] 
# Pasmo - nadmorska vyska, za kterou byly statistiky zprumerovany

Dataset <- readXL("C:/Users/Jan Tumajer/Desktop/skola/vyuka/LMM/Database_1961to2013.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Data", stringsAsFactors=TRUE) # Cesta na Applu
#View(Dataset)
head(Dataset)


### Otazka: jak jarni teploty ovlivnuji sirku letokruhu?
#	-> scatterplot + regresni analyza

attach(Dataset)
plot(Temp, Growth, cex=1.25, pch=16)
# Na prvni pohled se zda, ze s rostouci jarni teplotou sirka letokruhu klesa

lin.model <- lm(Growth ~ Temp)
summary(lin.model)
# Linearni model ukazuje na signifikantni vazbu mezi teplotami a rustem (p<<0.05) a potvrzuje jeji zaporne znamenko

abline(lin.model)

### Zaver: S rostouci teplotou klesa v CR prirust smrku, nejspise vlivem sucha (to by si ale zaslouzilo nejakou dalsi analyzu)

# ...
# ...
# Zkusme si jeste pro zajimavost znazornit, ktere body ve scatterplotu reprezentuji ruzne vyskove stupne

plot(Temp, Growth, cex=1.25, pch=16, , col=Pasmo)
legend(2.8, 3.25, c("500-700m","do500m", "nad700m"), col=1:length(Pasmo), pch=16)

# Tento obrazek ukazuje, ze predchozi zaver byl nespravny, protoze linearni model ignoroval vnitrni strukturu dat
# Analyzovana data ale maji vnitrni strukturu (prislusnost k urcitemu vyskovemu pasu), kterou je potreba zohlednit
# reseni -> smisene regresni modely (mixed-effects models)
	# Proc se jim rika smisene? Vstupuji do nich dva druhy prediktoru:
		# 1] fixni - spojite promenne, funguji skoro stejne jako promenne v obycejne linearni regresi
		# 2] nahodne - kategoricka promenna, ktera predstavuje odchylku casti pozorovani od globalniho prumeru

mixed.lin.model <- lmer(Growth ~ 1 + Temp + (1|Pasmo), data=Dataset) 
	# Sirka letokruhu je zavisla na teplote (fixni f.), avsak sila tohoto vztahu se lisi mezi jednotlivymi vyskovymi pasy (nahodny f.)
summary(mixed.lin.model)
	# Globalni model sirky letokruhu je 1.06+0.119*/Temp
coef(mixed.lin.model)
	# Existuji 3 lokalni modely pro jednotlive nadmorske vysky, viz ...

abline(1.0640469, 0.119886, col="black")
abline(0.2677848, 0.119886, col="red")
abline(1.8646510, 0.119886, col="green")


### Otazka: jak depozice dusiku ovlivnuji sirku letokruhu?

plot(N, Growth, cex=1.25, pch=16, col=Pasmo)
	# Graf vypada podobne, jako v pripade zavislosti prirustu na jarnich teplotach
	# Navic se ale zda, ze depozice N ma jinak silny ucinek v ruznych nadmorskych vyskach - slabe pozitivni v nizkych; negativni ve vysokych

mixed.lin.model.N <- lmer(Growth ~ 1 + N + (1 + N|Pasmo), data=Dataset) 
	# Ted ocekavame, ze jednotlive vyskove stupne nemaji rozdilny jenom intercept, ale i smernici zavislosti vuci N depozicim
summary(mixed.lin.model.N)
coef(mixed.lin.model.N)

abline( 1.998568,  -0.005484228, col="black")
abline( 1.093418,  0.005131948, col="red")
abline( 2.914956, -0.016232209, col="green")


#################################################################################
#################################################################################

# Hlavni prakticke rozdily mezi LMM a LM.
# 1] Chybi p-hodnoty = s tim se neda nic delat
	# Proto se malokdy LMM pouzivaji pro testovani, zda existuje nejaka statisticka zavislost mezi prediktory a zavislou promennou
	# Casteji je existence zavislosti vstupnim predpokladem a LMM slouzi jenom jako test jeji sily	
# 2] Chybi R2 = lze pouzit alternativni pseudoR2
sem.model.fits(mixed.lin.model)
	# marginal R2 = podil variability vysvetleny pouze fixnimi faktory
	# conditional R2 = podil variability vysvetleny fixnimi + nahodnymi faktory