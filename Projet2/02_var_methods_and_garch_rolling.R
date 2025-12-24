
# PROJET 2 - Korea Aerospace Industries (047810.KS)
# Estimation et backtesting de la VaR : application aux rendements de l’action 
# Korea Aerospace Industries
  
# Importation des données
rm(list = ls(all = TRUE))
library(yfR)
library(xts)

## KAI 047810.KS 
my_ticker  <- "047810.KS"
first_date <- "2012-01-01"
last_date  <- "2025-12-19"

df_yf <- yf_get(tickers = my_ticker, 
                first_date = first_date, last_date = last_date,
                freq_data   = "daily", type_return = "log")


## séries sur toute la période
pt <- df_yf$price_adjusted
dpt <- diff(pt)
dates <- df_yf$ref_date[-1]           
rt <- xts(df_yf$ret_adjusted_prices[-1], order.by=dates)


## RTE : 2012-01-01 à 2020-12-30
df_rte <- subset(df_yf,
                 ref_date >= "2012-01-01" &
                   ref_date <= "2020-12-30")

rte <- df_rte$ret_adjusted_prices[-1]
dates_rte <- df_rte$ref_date[-1] # dates associées à rte


## RTT : 2021-01-04 à 2025-11-07
df_rtt <- subset(df_yf,
                 ref_date >= "2021-01-04" &
                   ref_date <= "2025-12-19")

rtt <- df_rtt$ret_adjusted_prices[-1]
dates_rtt <- df_rtt$ref_date[-1] # dates associées à rtt


###############################################################################.
# I - VaR Paramétrique ----
###############################################################################.

#_______________________________________________________________________________
## I.1 - Choix de la distribution ----
#_______________________________________________________________________________

# Étant donné les résultats obtenus dans le projet 1, nous ne retiendrons que des 
# distributions asymétriques, car l’analyse de rte a mis en évidence une asymétrie 
# marquée vers la gauche
library(ghyp)


### a) QQ-plot -----------------------------------------------------------------
qqnorm(rte)
qqline(rte, col = 2)
# L’analyse du QQ-plot met clairement en évidence que les rendements s’écartent 
# fortement de la loi normale. Les deux queues de la distribution sont nettement 
# plus épaisses que celles d’une normale, ce qui traduit une probabilité accrue 
# d’événements extrêmes. 
# De plus, la queue gauche (celle des valeurs négatives) apparaît sensiblement 
# plus lourde que la queue droite : la distance entre la courbe et la droite est
# plus importante pour les valeurs fortement négatives de rte que pour les
# valeurs fortement positives de rte


### b) Avec ghyp : distribution sstd, NIG, hyp et ghyp -------------------------
# Estimation student asymétrique (sstd)
fitstu<-fit.tuv(rte,silent=T)
summary(fitstu) # Converged : TRUE

# Gaussienne inverse asymétrique (NIG)
fitnig<-fit.NIGuv(data=rte,silent=T)
summary(fitnig) # Converged : TRUE

# Hyperbolique asymétrique (hyp)
fithyp<-fit.hypuv(rte,silent=T)
summary(fithyp) # Converged : TRUE

# Hyperbolique généralisée asymétrique (ghyp)
fitghypuv<-fit.ghypuv(rte,silent=T)
summary(fitghypuv) # Converged : FALSE 

# Comparaison
plot(density(rte),
     main = "Estimateur par noyau de la densité \n des rendements et distributions ajustées",
     ylab = "Densité", 
     lwd = 2)

lines(fitstu, col = "brown4", lwd = 2) # student       
lines(fitnig, col = "steelblue", lwd = 2) # estimateur de la densité de rte
lines(fithyp, col = "darkseagreen3", lwd = 2) 

legend("topleft",
       legend = c("rte","student asymétrique","nig","hyp"),
       col = c("black", "brown4", "steelblue", "darkseagreen3"),
       lwd = 2, bty = "n", cex = 0.95)
# La distribution NIG semble être la meilleure



### c) Distribution ghst -------------------------------------------------------
library(SkewHyperbolic)
# ghstfit<-skewhypFit(rte, print = FALSE, plot =FALSE, hessian = TRUE)
# summary(ghstfit)

# L’ajustement de la distribution ghst n’a pas abouti, la procédure générant des 
# valeurs non numériques (NaN). Nous écartons donc la distribution GHST et ne la 
# retenons pas pour la suite de l’analyse.


#___________________________________
# L’examen de l’estimateur par noyau de la densité des rendements, comparé aux 
# distributions ajustées, montre que le modèle NIG est celui qui reproduit le plus 
# fidèlement la forme empirique de rte, ce qui en fait le choix le plus cohérent 
# pour la suite de l’analyse paramétrique.

# Cependant ce résultat doit être confirmé statistiquement. Nous poursuivons donc 
# l’analyse à l’aide des tests fournis par rugarch.


  

### d) Choix avec rugarch ------------------------------------------------------
# Pour évaluer la pertinence des distributions candidates, nous nous appuyons
# sur deux éléments
# (1) La significativité de skew (paramètres d’asymétrie et shape (de forme) , dont 
# les p-values doivent elles aussi être inférieures à 5 %. Ces paramètres doivent 
# en effet capturer l’asymétrie et la leptokurticité mises en évidence lors de 
# l’analyse préalable des rendements. Une distribution dont ces coefficients ne 
# sont pas significatifs ne serait pas cohérente avec les caractéristiques empiriques 
# observées.
# (2) L’Adjusted Pearson Goodness-of-Fit Test, qui permet de tester l’adéquation 
# entre la distribution supposée dans la spécification et la distribution empirique 
# des résidus standardisés. L’hypothèse nulle correspond à une bonne adéquation, 
# nous l’acceptons lorsque les quatre p-values du test sont strictement supérieur à 5 %.

library(rugarch)

# distribution nig
spec6.1 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1)),
                     distribution.model="nig")
fit6.1 = ugarchfit(spec=spec6.1, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6.1)

# distribution sstd
spec6.2 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1)),
                     distribution.model="sstd")
fit6.2 = ugarchfit(spec=spec6.2, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6.2)

# distribution sGED
spec6.3 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1)),
                     distribution.model="sged")
fit6.3 = ugarchfit(spec=spec6.3, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6.3)

# distribution jsu
spec6.4 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1)),
                     distribution.model="jsu")
fit6.4 = ugarchfit(spec=spec6.4, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6.4)


# | Distribution | p-value Pearson (1) | p-value Pearson (2) | p-value Pearson (3) | p-value Pearson (4) | p-value skew  | p-value shape  |
# | NIG      | 0.02                | 0.01                | 0.01                | 0.07            | 0.644     | 0.000      |
# | sSTD     | 0.04                | 0.01                | 0.03                | 0.03            | 0.000     | 0.000      |
# | sGED     | 0.00                | 0.02                | 0.00                | 0.15            | 0.000     | 0.000      |
# | JSU      | 0.03                | 0.01                | 0.04                | 0.03            | 0.398     | 0.000      |
 
  
# Au regard des résultats des tests d’adéquation, aucune des distributions évaluées 
# ne satisfait parfaitement l’ensemble des critères retenus. 
# 
# Toutefois, la distribution sGED se distingue comme la plus cohérente avec les 
# caractéristiques empiriques des rendements. 
# 
# * D’une part, skew (paramètres d’asymétrie et shape (de forme) y sont 
# significatifs au seuil de 5 %, conformément à l’asymétrie marquée et à la 
# leptokurticité observées dans l’analyse descriptive des rendements. 
# * D’autre part, ses p-values issues de l’Adjusted Pearson Goodness-of-Fit Test 
# figurent parmi les plus acceptables, ce qui en fait la distribution la moins 
# éloignée de la distribution empirique des résidus standardisés. 
# 
# Ainsi, même si l’ajustement n’est pas parfait, la distribution sGED apparaît 
# comme la meilleure approximation disponible parmi les modèles testés et sera 
# retenue pour la suite de l’estimation de la VaR paramétrique.
# 
# Par ailleurs, nous testerons également la distribution sSTD, dans la mesure où 
# elle satisfait le critère de significativité des paramètres skew et shape. 
# Il est en effet possible que, pour une autre modèle que modèle apARCH, la sstd 
# fournisse une meilleure adéquation.

  
#_______________________________________________________________________________
## I.2 - Modèles ARCH,GARCH asymétriques ----
#_______________________________________________________________________________
library(rugarch)

### a) Modèle apARCH -----------------------------------------------------------
#### a.1) Modèle apARCH(1,1) - ARMA(1,1) - $\varepsilon$ ~ sged ----
spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sged")
fit6 = ugarchfit(spec=spec6, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6)
# Comme delta (=1.2) est proche de 1, nous réestimons le modèle en fixant delta = 1



spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sged", fixed.pars=list(delta=1))
fit6 = ugarchfit(spec=spec6, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6)
# Tous les coefficients sont significatifs dont notamment omega

# Test de Ljung-Box pondérée sur les résidus standardisés : 
# Toutes le p-value sont >5% donc on accepte H0, pas d'autocorrélation dans les 
# aléas du ARMA(1,1). Le modèle ARMA(1,1) a ainsi réussi à prendre en compte 
# toute l'autocorrélation présente.

# Test d'Engle appliquée aux résidus standardisés et ce pour différents retards : 
# Toutes les p-value sont >5% on accepte ainsi H0, l'hypothèse d'abscence de 
# cluster de volatilité. Les aléas sont ainsi conditionnelement homoscédastiques
# donc le modèle apARCH(1,1) a réussi à prendre en compte tous les clusters de 
# volatilité 

# Test de stabilité de Nyblom : 
# La valeur calculée de la statistique jointe (1.43) est inférieur à la valeur tabulée 
# jointe (2.32) ainsi on accepte H0, tous les coefficients sont stable dans le temps. 

# Test du signe du biais : 
# * La p-value de la statistique de l'effet joint (0,000) est inférieur à 5% 
#   donc on rejette H0 ainsi présence d’au moins un effet d’asymétrie (signe ou taille)
# * La p-value de la statistique du signe du biais (0,5) est supérieur à 5% 
#   donc on accepte H0 : abscence d'effet signe i.e. un choc positif a le même impact 
#   sur la volatilité du rendemment qu'un choc négatif
# * La p-value de la statistique liees aux effets taille d'un choc négatif (0,2)
#   est supérieur à 5% donc on accepte H0 : il n’y a pas d’effet taille d’un choc négatif
# * La p-value de la statistique liees aux effets taille d'un choc positif (0,000) 
#   est inférieure à 5% donc on rejette H0 ainsi présence d’un effet taille d'un choc 
#   positif, i.e. les grands chocs positif ont un impact plus fort sur la volatilité
#   que les petits chocs positifs

# Test d'adéquation entre la distribution supposé et la distribution empirique 
# des résidus standardisés : 
# Les p-values étant globalement < 5 %, on rejette H0 l’adéquation entre la 
# distribution supposée et la distribution empirique.


sgedaparch=newsimpact(z=NULL, fit6)

plot(sgedaparch$zx,sgedaparch$zy, xlab=sgedaparch$xexpr,ylab=sgedaparch$yexpr ,
     type="l", main = "Courbe des impacts des nouvelles dans le apARCH")
# D’une part, les chocs positifs (bonnes nouvelles) et négatifs (mauvaises
# ne semblent pas avoir un impact de même intensité : les chocs négatifs paraissent
# avoir un effet plus fort.
# D’autre part, l’amplitude du choc joue un rôle : plus la nouvelle est extrême
# (très bonne ou très mauvaise), plus l’impact est important.




#### a.2) Modèle apARCH(1,1) - ARMA(1,1) - $\varepsilon$ ~ sstd ----
spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sstd")
fit6 = ugarchfit(spec=spec6, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6)
# Comme delta (=1.1) est proche de 1, nous réestimons le modèle en fixant delta = 1

spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sstd", fixed.pars=list(delta=1))
fit6 = ugarchfit(spec=spec6, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6)
# mu n’étant pas significatif (p-value = 0.71>5%), nous le retirons du modèle

spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1), include.mean=FALSE),
                   distribution.model="sstd", fixed.pars=list(delta=1))
fit6 = ugarchfit(spec=spec6, data = rt, out.sample=length(rtt), solver="hybrid")
show(fit6)
# Tous les coefficients sont significatifs dont notamment omega

# Test de Ljung-Box pondérée sur les résidus standardisés : 
# Toutes le p-value sont >5% donc on accepte H0, pas d'autocorrélation dans les 
# aléas du ARMA(1,1). Le modèle ARMA(1,1) a ainsi réussi à prendre en compte 
# toute l'autocorrélation présente.

# Test d'Engle appliquée aux résidus standardisés et ce pour différents retards : 
# Toutes les p-value sont >5% on accepte ainsi H0, l'hypothèse d'abscence de 
# cluster de volatilité. Les aléas sont ainsi conditionnelement homoscédastiques
# donc le modèle apARCH(1,1) a réussi à prendre en compte tous les clusters de 
# volatilité 

# Test de stabilité de Nyblom : 
# La valeur calculée de la statistique jointe (1.52) est inférieur à la valeur tabulée 
# jointe (2.11) ainsi on accepte H0, tous les coefficients sont stable dans le temps. 

# Test du signe du biais : 
# * La p-value de la statistique de l'effet joint (0,000) est inférieur à 5% 
#   donc on rejette H0 ainsi présence d’au moins un effet d’asymétrie (signe ou taille)
# * La p-value de la statistique du signe du biais (0,4) est supérieur à 5% 
#   donc on accepte H0 : abscence d'effet signe i.e. un choc positif a le même impact 
#   sur la volatilité du rendemment qu'un choc négatif
# * La p-value de la statistique liees aux effets taille d'un choc négatif (0,2)
#   est supérieur à 5% donc on accepte H0 : il n’y a pas d’effet taille d’un choc négatif
# * La p-value de la statistique liees aux effets taille d'un choc positif (0,000) 
#   est inférieure à 5% donc on rejette H0 ainsi présence d’un effet taille d'un choc 
#   positif, i.e. les grands chocs positif ont un impact plus fort sur la volatilité
#   que les petits chocs positifs

# Test d'adéquation entre la distribution supposé et la distribution empirique 
# des résidus standardisés : 
# Les p-values étant globalement < 5 %, on rejette H0 l’adéquation entre la 
# distribution supposée et la distribution empirique.



### b) Modèle GJR-GARCH --------------------------------------------------------
#### b.1) Modèle GJR-GARCH(1,1) - ARMA(1,1) - $\varepsilon$ ~ sged ----
spec5 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sged")
fit5= ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)
# mu n’étant pas significatif (p-value = 0.9>5%), nous le retirons du modèle

spec5 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1), include.mean=FALSE),
                   distribution.model="sged")
fit5= ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)
# alpha1 (p-value = 0.1>5%) et gamma1 (p-value = 0.1>5%) non significatifs
# Or, dans un GJR-GARCH, on impose alpha1 >= 0 et la présence d’effet de levier 
# implique que gamma1 soit >0
# Donc comme alpha1 doit être >= 0, nous pouvons le fixer à 0

spec5 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1), include.mean=FALSE),
                   distribution.model="sged", fixed.pars=list(alpha1=0))
fit5= ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)
# Ne converge plus donc nous rejetons ce modèle




#### b.2) Modèle GJR-GARCH(1,1) - ARMA(1,1) - $\varepsilon$ ~ sstd ----
spec5 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sstd")
fit5= ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)
# mu n’étant pas significatif (p-value = 0.93>5%), nous le retirons du modèle

spec5 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,0), include.mean=FALSE),
                   distribution.model="sstd")
fit5= ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)
# omega (p-value = 0.12>5%) et gamma1 (p-value = 0.13>5%) non significatifs
# Or, dans un GJR-GARCH, on impose omega>0  pour satisfaire la condition de positivité 
# de la variance conditionnelle et gamma1>0 la prise en compte de l'effet taille
# Nous rejetons donc ce modèle



### c) Modèle EGARCH -----------------------------------------------------------
#### c.1) Modèle EGARCH(1,1) - ARMA(1,1) - $\varepsilon$ ~ sged ----
spec4 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sged")
fit4= ugarchfit(spec = spec4, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit4)
# alpha1 (p-value = 0.15>5%) et gamma1 (p-value = 0.17>5%) non significatifs
# Or, dans un EGARCH, alpha1<0 pour la prise en compte de l'effet de levier 
# et gamma1>0 pour la prise en compte de l'effet taille
# Donc ce modèle n'est pas le bon 



#### c.2) Modèle EGARCH(1,1) - ARMA(1,1) - $\varepsilon$ ~ sstd ----
spec4 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sstd")
fit4= ugarchfit(spec = spec4, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit4)
# mu n’étant pas significatif (p-value = 0.85>5%), nous le retirons du modèle

spec4 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1), include.mean=FALSE),
                   distribution.model="sstd")
fit4= ugarchfit(spec = spec4, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit4)
# alpha1 (p-value = 0.06>5%) et gamma1 (p-value = 0.35>5%) 
# non significatifs
# Or, dans un EGARCH, alpha1<0 pour la prise en compte de l'effet de levier 
# et gamma1>0 pour la prise en compte de l'effet taille
# Donc ce modèle n'est pas le bon 




#_______________________________________________________________________________
## I.3 - Modèles ARCH,GARCH symétriques ----
#_______________________________________________________________________________
library(rugarch)

### a) Modèle IGARCH -----------------------------------------------------------
spec3 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),
                   distribution.model="sged")
fit3 = ugarchfit(spec = spec3, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit3)
# Tous les coefficients sont significatifs

# Test de Ljung-Box pondérée sur les résidus standardisés : 
# Toutes le p-value sont >5% donc on accepte H0, pas d'autocorrélation dans les 
# aléas du ARMA(1,1). 

# Test d'Engle appliquée aux résidus standardisés et ce pour différents retards : 
# Toutes les p-value sont >5% on accepte ainsi H0 : l'hypothèse d'abscence de 
# cluster de volatilité.

# Test de stabilité de Nyblom : 
# La valeur calculée de la statistique jointe (2.0) est supérieur à la valeur  
# tabulée jointe (1.9) donc on rejette H0 ainsi tous les coefficients ne sont pas 
# stable dans le temps. Cela indique qu'il y a au moins un coefficient qui n'est 
# pas stable dans le temps. On voit que c'est omega qui ne l'est pas car la valeur
# de sa statitique individuelle (0.6) est supérieur à la valeur tabulée 
# individuellle (0.47)

# Test du signe du biais : 
# * La p-value de la statistique de l'effet joint (0.03) < 5% 
#   donc on rejette H0 ainsi présence d’au moins un effet d’asymétrie (signe ou taille)
# * La p-value de la statistique du signe du biais (0.5) > 5% 
#   donc on accepte H0 : abscence d'effet signe 
# * La p-value de la statistique liees aux effets taille d'un choc négatif (0.3) > 5%
#   donc on accepte H0 : pas d’effet taille d’un choc négatif
# * La p-value de la statistique liees aux effets taille d'un choc positif (0.006) < 5%
#   donc on rejette H0 ainsi présence d’un effet taille d'un choc positif

# Test d'adéquation entre la distribution supposé et la distribution empirique 
# des résidus standardisés : 
# Les p-values étant globalement < 5 %, on rejette H0 l’adéquation entre la 
# distribution supposée et la distribution empirique.


sgedigarch=newsimpact(z=NULL, fit3)

# plot(sgedigarch$zx,sgedigarch$zy, xlab=sgedigarch$xexpr,ylab=sgedigarch$yexpr ,
#      type="l", main = "Courbe des impacts des nouvelles dans le iGARCH")
# Warning :aucun argument trouvé pour min ; Inf est renvoyé
# Warning :aucun argument pour max ; -Inf est renvoyé





### b) Modèle GARCH-M ----------------------------------------------------------
spec2 = ugarchspec(mean.model=list(armaOrder=c(1,1),archm=TRUE),
                   distribution.model="sged")
fit2 = ugarchfit(spec = spec2,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit2)
# Tous les coefficients sont significatifs notamment c, la prime de risque, et 
# alpha1 et beta1 (avec bien [alpha1+beta1]<1)

# Test de Ljung-Box pondérée sur les résidus standardisés : 
# Toutes le p-value sont >5% donc on accepte H0, pas d'autocorrélation dans les 
# aléas du ARMA(1,1).

# Test d'Engle appliquée aux résidus standardisés et ce pour différents retards : 
# Toutes les p-value sont >5% on accepte ainsi H0 : l'hypothèse d'abscence de 
# cluster de volatilité.

# Test de stabilité de Nyblom : 
# La valeur calculée de la statistique jointe (1.5) est inférieur à la valeur  
# tabulée jointe (2.32) ainsi on accepte H0 : tous les coefficients sont 
# stable dans le temps. 

# Test du signe du biais : 
# * La p-value de la statistique de l'effet joint (0.003) < 5% 
#   donc on rejette H0 ainsi présence d’au moins un effet d’asymétrie (signe ou taille)
# * La p-value de la statistique du signe du biais (0.3) > 5% 
#   donc on accepte H0 : abscence d'effet signe 
# * La p-value de la statistique liees aux effets taille d'un choc négatif (0.2) > 5%
#   donc on accepte H0 : pas d’effet taille d’un choc négatif
# * La p-value de la statistique liees aux effets taille d'un choc positif (0.000) < 5%
#   donc on rejette H0 ainsi présence d’un effet taille d'un choc positif

# Test d'adéquation entre la distribution supposé et la distribution empirique 
# des résidus standardisés : 
# Les p-values étant globalement < 5 %, on rejette H0 l’adéquation entre la 
# distribution supposée et la distribution empirique.



sgedgarchm=newsimpact(z=NULL, fit2)

plot(sgedgarchm$zx,sgedgarchm$zy, xlab=sgedgarchm$xexpr,ylab=sgedgarchm$yexpr ,
     type="l", main = "Courbe des impacts des nouvelles dans le GARCH-M")
# D’une part, les chocs positifs (bonnes nouvelles) et négatifs (mauvaises 
# nouvelles) semblent avoir un impact de même intensité.
# D’autre part, l’amplitude du choc joue un rôle : plus la nouvelle est extrême 
# (très bonne ou très mauvaise), plus l’impact est important.




### c) Modèle GARCH ------------------------------------------------------------
spec1 = ugarchspec(distribution.model="sged")
fit1 = ugarchfit(spec = spec1, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit1)
# Tous les coefficients sont significatifs dont omega et alpha1 et beta1 
# (avec bien [alpha1+beta1]<1)


# Test de Ljung-Box pondérée sur les résidus standardisés : 
# Toutes le p-value sont >5% donc on accepte H0, pas d'autocorrélation dans les 
# aléas du ARMA(1,1).

# Test d'Engle appliquée aux résidus standardisés et ce pour différents retards : 
# Toutes les p-value sont >5% on accepte ainsi H0 : l'hypothèse d'abscence de 
# cluster de volatilité.

# Test de stabilité de Nyblom : 
# La valeur calculée de la statistique jointe (1.5) est inférieur à la valeur  
# tabulée jointe (2.11) donc on accepte H0 : tous les coefficients sont 
# stable dans le temps.

# Test du signe du biais : 
# * La p-value de la statistique de l'effet joint (0.003) < 5% 
#   donc on rejette H0 ainsi présence d’au moins un effet d’asymétrie (signe ou taille)
# * La p-value de la statistique du signe du biais (0.2) > 5% 
#   donc on accepte H0 : abscence d'effet signe 
# * La p-value de la statistique liees aux effets taille d'un choc négatif (0.2) > 5%
#   donc on accepte H0 : pas d’effet taille d’un choc négatif
# * La p-value de la statistique liees aux effets taille d'un choc positif (0.00) < 5%
#   donc on rejette H0 ainsi présence d’un effet taille d'un choc positif

# Test d'adéquation entre la distribution supposé et la distribution empirique 
# des résidus standardisés : 
# Les p-values étant globalement < 5 %, donc on rejette H0 ainsi non adéquation entre la 
# distribution supposée et la distribution empirique.



sgedgarch=newsimpact(z=NULL, fit2)

plot(sgedgarch$zx,sgedgarch$zy, xlab=sgedgarch$xexpr,ylab=sgedgarch$yexpr ,
     type="l", main = "Courbe des impacts des nouvelles dans le GARCH")
# Ce qu'on observe est identique à GARCH-M 
# D’une part, les chocs positifs (bonnes nouvelles) et négatifs (mauvaises 
# nouvelles) semblent avoir un impact de même intensité.
# D’autre part, l’amplitude du choc joue un rôle : plus la nouvelle est extrême 
# (très bonne ou très mauvaise), plus l’impact est important.




library(rugarch)
library(xts)
library(zoo)
library(parallel)

#_______________________________________________________________________________
## I.4 - Modèle choisi ----
#_______________________________________________________________________________

### a) Choix du modèle ---------------------------------------------------------
# Le choix final du modèle se fait parmi les spécifications asymétriques valides, 
# car ce sont elles qui permettent de prendre en compte un éventuel effet de levier. 
# Deux modèles candidats ressortent : 
# * apARCH(1,1)–ARMA(1,1) avec $\varepsilon$ ~ sGED
# * apARCH(1,1)–ARMA(1,1) avec $\varepsilon$ ~ sSTD 
# 
# Les deux spécifications sont pratiquement identiques ; la seule différence provient 
# de la distribution des innovations. Dans les deux cas, le test de Pearson ajusté 
# présente une seule p-value supérieure à 5%, ce qui indique un ajustement imparfait. 
# 
# Dès lors, la sélection s’appuie principalement sur un critère d’information : le 
# BIC est légèrement plus faible (donc meilleur) pour la spécification sSTD 
# (-4,8657) que pour la sGED (-4,8478). Nous retenons donc le modèle : 
# apARCH(1,1)–ARMA(1,1) avec $\varepsilon$ ~ sSTD 


### b) Ajout de la saisonnalité ------------------------------------------------
# Maintenant que le modèle de base est retenu, nous l’enrichissons en intégrant 
# des effets de saisonnalité afin de capter d’éventuelles régularités calendaires : 
# une saisonnalité dans la moyenne (effet mercredi) ainsi qu’une saisonnalité 
# dans la variance (effets vendredi et janvier).

T <- length(rte)
jour=format(dates_rte, format = "%A")
mois=format(dates_rte, format = "%B")
moisrte=mois[1:T] 
janvier = as.integer(moisrte=="janvier")
jourrte=jour[1:T] 
vendredi=as.integer(jourrte=="vendredi")
mercredi=as.integer(jourrte=="mercredi")


spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1), include.mean=FALSE,
                                   external.regressors=as.matrix(mercredi)),
                   distribution.model="sstd", fixed.pars=list(delta=1))
fit6 = ugarchfit(spec=spec6, data = rte, solver="hybrid")
show(fit6)
# Comme la p-value (0.75) du coefficients associées à la variable mxreg1 (liée à effet 
# janvier dans la moyenne) est supérieur à 5%, ce coefficient n'est pas significatif 
# donc on ne le garde pas


spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1),
                                       external.regressors=as.matrix(vendredi)),
                   mean.model=list(armaOrder=c(1,1), include.mean=FALSE),
                   distribution.model="sstd", fixed.pars=list(delta=1))
fit6 = ugarchfit(spec=spec6, data = rte, solver="hybrid")
show(fit6)
# Comme la p-value (0.99) du coefficients associées à la variable vxreg1 (liée à effet 
# vendredi dans la variance) est supérieur à 5%, ce coefficient n'est pas significatif 
# donc on ne le garde pas


spec6 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1),
                                       external.regressors=as.matrix(janvier)),
                   mean.model=list(armaOrder=c(1,1), include.mean=FALSE),
                   distribution.model="sstd", fixed.pars=list(delta=1))
fit6 = ugarchfit(spec=spec6, data = rte, solver="hybrid")
show(fit6)
# Comme la p-value (0.51) du coefficients associées à la variable vxreg1 (liée à effet 
# janvier dans la variance) est supérieur à 5%, ce coefficient n'est pas significatif 
# donc on ne le garde pas


# Comme les coefficients associés à ces nouvelles variables de saisonnalité ne sont 
# pas significatifs, nous en concluons que la saisonnalité n’est pas pertinente 
# dans ce cadre et le modèle final ne l’inclura pas.


#_______________________________________________________________________________ 
## I.5 - Prévision de la VaR ----
#_______________________________________________________________________________ 

# Pour prévoir la VaR hors échantillon, nous utilisons une estimation par fenêtre 
# glissante (*rolling estimation*). Le principe est d’estimer le modèle sur les (T) 
# premières observations, puis de produire une prévision pour la date (T+1) afin 
# d’en déduire la VaR. Ensuite, la fenêtre est décalée d’une observation : le modèle 
# est ré-estimé sur les dates (2) à (T+1), une prévision est réalisée pour (T+2), 
# et ainsi de suite jusqu’à la fin de l’échantillon.


# En raison d’une erreur lors de la production du graphique, j’ai dû modifier la procédure :

# Sécurisation des données
stopifnot(inherits(rt, "xts")) # rt doit être xts avec un index Date
stopifnot(is.numeric(coredata(rt))) # rendements numériques

if (!inherits(rtt, "xts")) {
  h <- length(rtt) # h = taille de la période test
  rtt <- tail(rt, h) # rtt devient un xts aligné sur les dernières dates
}

forecast_len <- NROW(rtt) # longueur de prévision hors-échantillon


# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(1,1), include.mean=FALSE),
                  distribution.model="sstd", fixed.pars=list(delta=1))
roll=ugarchroll(spec, data=rt,n.ahead=1,forecast.length=length(rtt),refit.every=1,
                refit.window="moving",solver = "hybrid", cluster=cl,fit.control = list(),
                calculate.VaR=TRUE,VaR.alpha=0.05,keep.coef = TRUE)
stopCluster(cl)

# valueatrisk<-zoo(roll@forecast$VaR[,1])
# reelles<-zoo(roll@forecast$VaR[,2])#=rtt
# index<-rownames(roll@forecast$VaR)

nV <- nrow(roll@forecast$VaR) # nb de points de VaR produits par ugarchroll
dates_var <- tail(index(rtt), nV) # dates correspondantes (fin de période test)

valueatrisk <- xts(roll@forecast$VaR[, 1], order.by = dates_var)  # VaR
reelles <- xts(roll@forecast$VaR[, 2], order.by = dates_var)  # rtt



# N <- length(rendement)
# plot(dates[2214:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
# lines(dates[2214:N],valueatrisk,type='l',col="red")
# legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))
# Erreur dans xy.coords(x, y, xlabel, ylabel, log) : 
#  les longueurs de 'x' et 'y' diffèrent


library(ggplot2)

df_plot <- data.frame(Date = as.Date(index(reelles)),
                      rt   = as.numeric(reelles),
                      VaR  = as.numeric(valueatrisk))

ggplot(df_plot, aes(x = Date)) +
  geom_line(aes(y = rt), color = "grey30", linewidth = 0.2) +
  geom_point(aes(y = rt), shape = 1, color = "grey10", size = 0.8, stroke = 0.3) +
  geom_line(aes(y = VaR), color = "#2683C6", linewidth = 0.5) +
  labs(title = "VaR avec la méthode de fenêtre glissante de l’action \n Korea Aerospace Industries",
       x = "Dates", y = "Rendements et VaR") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.92),
        legend.title = element_blank()) +
  scale_color_manual(values = c("rtt" = "grey30",
                                "VaR" = "#2683C6")) +
  geom_line(aes(y = rt, color = "rtt"), linewidth = 0.2) +
  geom_line(aes(y = VaR, color = "VaR"), linewidth = 0.5) +
  guides(color = guide_legend(override.aes = list(linewidth = c(0.7, 0.7))))

# La courbe bleue représente la VaR au seuil de 95%. Les observations situées
# en dessous de cette courbe correspondent aux violations : ce sont des jours où le rendement
# réalisé est inférieur à la VaR, donc où la perte est plus importante que celle anticipée.



# Afin de donner une interprétation économique à ces dépassements, nous avons extrait
# les dates de violations et classé les plus sévères (gap = rtt - VaR, plus gap est
# négatif, plus la violation est importante). 

# Ajout des bonnes dates (2021-01-04 > ...)
reelles_xts <- xts(as.numeric(reelles), order.by = as.Date(dates_rtt))
VaR_xts <- xts(as.numeric(valueatrisk), order.by = as.Date(dates_rtt))

data_bt <- merge.xts(reelles_xts, VaR_xts, join = "inner")
colnames(data_bt) <- c("rtt", "VaR")

# Top 10 des violations 
viol <- data_bt$rtt < data_bt$VaR
dates_viol <- index(data_bt)[as.logical(viol)]

df_viol <- data.frame(Date = as.Date(index(data_bt)),
                      rtt  = as.numeric(data_bt$rtt),
                      VaR  = as.numeric(data_bt$VaR))

df_viol <- df_viol[df_viol$rtt < df_viol$VaR, ]
df_viol$gap <- df_viol$rtt - df_viol$VaR
df_viol <- df_viol[order(df_viol$gap), ]

head(df_viol, 10)

# Le tableau "top 10" met en évidence des chocs journaliers extrêmes (allant jusqu'à -11%) 
# nettement au-delà du seuil de VaR.
#
# La violation la plus sévère se situe fin octobre 2023. Elle est cohérente avec
# un choc davantage "firm-specific" : sur 3Q23, KAI a publié un résultat opérationnel
# inférieur aux attentes, expliqué par des charges exceptionnelles, des marges faibles sur les
# premières livraisons du FA-50 à la Pologne, et une contribution plus faible de l’activité
# pièces aéronautiques. Une telle déception par rapport au consensus peut déclencher une
# correction brutale et donc une violation importante de VaR.
#
# On observe aussi une forte concentration de violations en 2022 (juillet, août, et
# surtout octobre 2022). Cette période correspond à un environnement de marché très tendu
# en Corée, marqué par un stress de financement et une hausse brutale de l’aversion au risque.
# L’épisode dit “Legoland” (défaut lié au financement d’un projet) a notamment alimenté des
# craintes de credit crunch et a fait grimper les taux/spreads, ce qui a accru la volatilité
# des marchés coréens.
#
# Enfin, la violation de mars 2025 peut s’inscrire dans un contexte de nervosité,
# ce jour-là, des problèmes techniques ont entraîné une interruption
# temporaire des échanges sur le KOSPI, ce qui a pu accentuer l’incertitude, dégrader la liquidité
# et amplifier les mouvements intrajournaliers.




#_______________________________________________________________________________
## I.6 - Backtesting -----
#_______________________________________________________________________________

report(roll, type="VaR", VaR.alpha=0.05, conf.level=0.95)
# Commençons par le test de Kupiec avec H0 : le taux de violation
# théorique et le taux de violation empirique sont statistiquement identiques.
# Comme notre p-value (0.15) est supérieur au seuil de 5%, nous acceptons H0.

# Cependant, il est possibles que les autocorrélations soient autocorélées, 
# On lui préférera donc le test de Christoffersen avec H0 : le taux de 
# violation théorique et le taux de violation empirique sont statistiquement 
# identiques ET les violations sont indépendantes les unes des autres.
# Comme notre p-value (0.35) est supérieur au seuil de 5%, nous acceptons donc bien H0.

# Ainsi, notre VaR estime bien le risque.



###############################################################################.
# II - VaR Normale, VaR Historique et VaR Cornish-Fisher ----
###############################################################################.
library(PerformanceAnalytics)
library(rugarch)

#_______________________________________________________________________________
## II.1 - Prévision de la VaR ----
#_______________________________________________________________________________

### a) VaR Normale -------------------------------------------------------------

VaR(rte, p=.95, method="gaussian") #-0.04090
# Il y a 5 jours sur 100 jours de bourse, soit environs 12.6 jours dans l'année 
# (252 jours de bourse/an), où le rendement du lendemain sera inférieur à −4,09%, 
# c’est-à-dire où la perte sera plus importante que le seuil donné par la VaR à 95%.


V <- tail(df_yf$price_adjusted, 1)   # valeur d'1 action (dernier prix ajusté)
V # 108 800 Korean won

# Convertir la VaR en monnaie par action
VaR <- -0.04090
(1 - exp(VaR)) * V
# Avec un niveau de confiance de 95%, la perte journalière par action ne dépassera 
# pas environ  4 360.147 KRW. Équivalemment, il y a 5% de chances (environ 5 jours sur 100 
# jours de bourse) que la perte soit supérieure à  4 360.147 KRW




### b) VaR Historique ----------------------------------------------------------

VaR(rte, p=.95, method="historical") # -0.03468
# Il y a 5 jours sur 100 jours de bourse, soit environs 12.6 jours dans l'année 
# (252 jours de bourse/an), où le rendement du lendemain sera inférieur à −3,46%, 
# c’est-à-dire où la perte sera plus importante que le seuil donné par la VaR à 95%.


# Convertir la VaR en monnaie par action
VaR <- -0.03468
(1 - exp(VaR)) * V
# Avec un niveau de confiance de 95%, la perte journalière par action ne dépassera 
# pas 3 708.507 KRW. Équivalemment, il y a 5% de chances (environ 5 jours sur 100 
# jours de bourse) que la perte soit supérieure à 3 708.507 KRW




### c) VaR de Cornish-Fisher ---------------------------------------------------

VaR(rte, p=.95, method="modified") # -0.03674
# Il y a 5 jours sur 100 jours de bourse, soit environs 12.6 jours dans l'année 
# (252 jours de bourse/an), où le rendement du lendemain sera inférieur à −3,67%, 
# c’est-à-dire où la perte sera plus importante que le seuil donné par la VaR à 95%.


# Convertir la VaR en monnaie par action
VaR <- -0.03674
(1 - exp(VaR)) * V
# Avec un niveau de confiance de 95%, la perte journalière par action ne dépassera 
# pas  3 924.772 KRW. Équivalemment, il y a 5% de chances (environ 5 jours sur 100 
# jours de bourse) que la perte soit supérieure à   3 924.772KRW


  
#_______________________________________________________________________________
## II.2 - Backtesting ----
#_______________________________________________________________________________

Ne=length(rte)
Nt=length(rtt)
alpha=0.95 # VaR à 95%

backTestVaR <- function(x, p = alpha) {
  normal.VaR = as.numeric(VaR(x, p=p, method="gaussian"))
  historical.VaR = as.numeric(VaR(x, p=p, method="historical"))
  modified.VaR = as.numeric(VaR(x, p=p, method="modified"))
  ans = c(normal.VaR, historical.VaR, modified.VaR)
  names(ans) = c("Normal", "HS", "Modified")
  return(ans)
}

# rolling 1-step ahead estimates of VaR
VaR.results = rollapply(as.zoo(rt), width=Ne, 
                        FUN = backTestVaR, p=alpha, by.column = FALSE,
                        align = "right")

violations.mat = matrix(0, 3, 5)
rownames(violations.mat) = c("Normal", "HS", "Modified")
colnames(violations.mat) = c("En1", "n1", "1-alpha", "Percent", "VR")
violations.mat[, "En1"] = (1-alpha)*Nt
violations.mat[, "1-alpha"] = 1 - alpha

# Show Normal VaR violations
normalVaR.violations = as.numeric(as.zoo(
  rt[index(VaR.results)])) < VaR.results[, "Normal"]
violation.dates = index(normalVaR.violations[which(normalVaR.violations)])


for(i in colnames(VaR.results)) {
  VaR.violations = as.numeric(as.zoo(rt[index(VaR.results)])) < VaR.results[, i]
  violations.mat[i, "n1"] = sum(VaR.violations)
  violations.mat[i, "Percent"] = sum(VaR.violations)/Nt
  violations.mat[i, "VR"] = violations.mat[i, "n1"]/violations.mat[i, "En1"]
}
violations.mat
# Cela nous permet d'obtenir les taux de violation empiriques, que nous comparerons 
# ensuite au taux de violation théorique afin de vérifier, à l’aide des tests ci-dessous, 
# s’ils sont statistiquement égaux.


resultats<-data.frame(matrix(NA,ncol=4,nrow=3))
colnames(resultats)<-c("expected.exceed","actual.exceed","Kupiecpv","Christoffersenpv")
rownames(resultats)<-c("Normale","HS","CF")

# normale
VaR.test1 = VaRTest(1-alpha,actual=coredata(rt[index(VaR.results)]), VaR=coredata(VaR.results[,"Normal"]))
resultats[1,1]=VaR.test1$expected.exceed
resultats[1,2]=VaR.test1$actual.exceed
resultats[1,3]=VaR.test1$uc.LRp
resultats[1,4]=VaR.test1$cc.LRp

# historique
VaR.test2 = VaRTest(1-alpha,actual=coredata(rt[index(VaR.results)]), VaR=coredata(VaR.results[,"HS"]))
resultats[2,1]=VaR.test2$expected.exceed
resultats[2,2]=VaR.test2$actual.exceed
resultats[2,3]=VaR.test2$uc.LRp
resultats[2,4]=VaR.test2$cc.LRp

# modifie
VaR.test3 = VaRTest(1-alpha, actual=coredata(rt[index(VaR.results)]), VaR=coredata(VaR.results[,"Modified"]))
resultats[3,1]=VaR.test3$expected.exceed
resultats[3,2]=VaR.test3$actual.exceed
resultats[3,3]=VaR.test3$uc.LRp
resultats[3,4]=VaR.test3$cc.LRp

resultats

# VaR Normale : 
  # Commençons par le test de Kupiec avec H0 : le taux de variation théorique et 
  # le taux de variation empirique sont statistiquement identique. Comme notre 
  # p-value (0.18) est supérieur au seuil de 5%, nous acceptons H0. Cependant, il 
  # est possibles que les autocorrélations soient autocorélées, on lui préfèrera 
  # donc le test de Christoffersen avec H0 : le taux de variation théorique et le 
  # taux de variation empirique sont statistiquement identique & les violations 
  # sont indépendantes les unes des autres. Comme notre p-value (0.27) est supérieur 
  # au seuil de 5%, nous acceptons H0. La VaR Normale estime donc bien le risque. 

# VaR Historique : 
  # Commençons par le test de Kupiec avec H0 : comme notre p-value (0.04) est inférieur 
  # au seuil de 5%, nous rejetons H0. Ainsi, le taux de violation empirique n'est pas 
  # statistiquement égal au taux de violation théorique. Comme taux violation théorique 
  # (5%) < (5.185%) taux de violation empirique, on a prit trop de risque. Ainsi la VaR 
  # sous-estimerait le risque. 
  # Cependant, il est possibles que les autocorrélations soient autocorélées, on lui 
  # préfèrera donc le test de Christoffersen, comme notre p-value (0.07) est supérieur 
  # au seuil de 5%, nous acceptons H0. La VaR Historique estime donc bien le risque. 

# VaR Cornish-Fisher : 
  # Commençons par le test de Kupiec avec H0 : comme notre p-value (0.77) est inférieur 
  # au seuil de 5%, nous rejetons H0. Cependant, il est possibles que les autocorrélations 
  # soient autocorélées, on lui préfèrera donc le test de Christoffersen, comme notre 
  # p-value (0.88) est supérieur au seuil de 5%, nous acceptons H0. La VaR Historique 
  # estime donc bien le risque. 


