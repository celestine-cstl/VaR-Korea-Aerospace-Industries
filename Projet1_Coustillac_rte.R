
# PROJET 1 - Korea Aerospace Industries (047810.KS)
# COUSTILLAC Cêlestine

# Importation des données ------------------------------------------------------
rm(list = ls(all = TRUE))
library(yfR)

## KAI 047810.KS 
my_ticker  <- "047810.KS"
first_date <- "2012-01-01"
last_date  <- "2025-11-07"

df_yf <- yf_get(tickers = my_ticker, 
                first_date = first_date, last_date = last_date,
                freq_data   = "daily", type_return = "log")


## séries sur toute la période
pt <- df_yf$price_adjusted
dpt <- diff(pt)
dates <- df_yf$ref_date[-1]           
rt <- df_yf$ret_adjusted_prices[-1]


## RTE : 2012-01-01 à 2020-12-30
df_rte <- subset(df_yf,
                 ref_date >= "2012-01-01" &
                   ref_date <= "2020-12-30")

rte <- df_rte$ret_adjusted_prices[-1]
dates_rte <- df_rte$ref_date[-1] # dates associées à rte


## RTT : 2021-01-04 à 2025-11-07
df_rtt <- subset(df_yf,
                 ref_date >= "2021-01-04" &
                   ref_date <= "2025-11-07")

rtt <- df_rtt$ret_adjusted_prices[-1]
dates_rtt <- df_rtt$ref_date[-1] # dates associées à rtt



#___________________________________________________________________________________________
# Introduction : Analyse de toute la période -----------------------------------
#___________________________________________________________________________________________

op <- par(mfrow = c(3,1))

plot(df_yf$ref_date, df_yf$price_adjusted, type = 'l', col = "darkseagreen3",
     ylab = "Cours ajusté", xlab = "",
     main = "Évolution du cours ajusté de l’action KAI")

plot(dates, dpt, type = 'l', col = "brown4",
     ylab = "Variation du prix", xlab = "",
     main = "Variations du prix ajusté de l’action KAI")

plot(dates, rt, type = 'l',ylab = "Rendement", xlab = "",
     main = "Rendements de l’action KAI")
par(op)

# le cours ajusté de l’action KAI présente une tendance globale croissante
# variations de prix et rendements centrés autour de zéro
# les fluctuations autour de 0 change clairement au cours du temps, particulièrement pour dpt
# il semble y avoir des paquets de volatilité en 2016, en 2018 et en 2025.






#___________________________________________________________________________________________
# I - Analyse des 8 propriétés statistiques du rendement logarithmique de la série rte ----
#___________________________________________________________________________________________
s=sd(rte) # écart type estimé
rbar<-mean(rte) # moyenne empirique
rbar
 
# Est-ce que cette moyenne empirique proche de 0 est statistiquement nulle ? 
# H0 : E[rte] = mu = 0 VS Ha : E[rte] = mu =/= 0 
t.test(rte) 
 # Comme p-value = 0.758 > 0.05, alors on accepte H0. 
# Ainsi, l'espérance du PGD qui a généré  rte est nulle.





## PROPRIETE 1 - Asymétrie perte/gain ------------------------------------------
library(moments)

agostino.test(rte)
 
# p-value < 2.2e-16 < 5 %, la skewness est donc statistiquement différente de zéro, 
# ce qui signifie que la distribution des rendements n’est pas symétrique. 

# Ainsi, nous pouvons interpréter sa valeur calculé : skew = -1.2156 < 0, cela 
# traduit une asymétrie orientée vers la gauche : on observe généralement de 
# nombreux rendements positifs mais de faible valeurs, tandis que les rendements 
# négatifs, plus rares, sont de forte valeurs. Autrement dit, les petites hausses 
# sont fréquentes mais les baisses, lorsqu’elles surviennent, sont plus importantes, 
# ce qui caractérise une distribution avec une queue gauche plus lourde.




## PROPRIETE 2 : Queues de distribution épaisses -------------------------------
anscombe.test(rte)
 
# p-value < 2.2e-16 < 5 %, nous rejetons l’hypothèse nulle selon laquelle la kurtosis 
# serait égale à 3. 

# Ainsi, nous pouvons interpréter sa valeur calculé : kurt = 27.073 >> 3, cela 
# indique une distribution fortement leptokurtique, i.e. une distribution dont la 
# cloche est plus pointue que celle de la loi gaussienne, avec des queues plus 
# importante.


# Comparaison de rte avec une loi normale : 
mu <- mean(rte, na.rm = TRUE)
sigma <- sd(rte, na.rm = TRUE)

hist(rte, breaks = 70, freq = FALSE, col = "grey",
     main = "Comparaison de la distribution des rendements \n logarithmiques avec la loi normale",
     xlab = "Rendements",
     ylab = "Densité",
     border = "white")

lines(density(rte), col = "brown", lwd = 2)

curve(dnorm(x, mean = mu, sd = sigma),
      from = min(rte), to = max(rte),
      add = TRUE, col = "steelblue", lwd = 2)
# On observe bien la distribution leptokurtique


## PROPRIETE 3 : Autocorrélations des carrés des rendements fortes et faibles pour les rendements ----
library(FinTS)
library(TSA)
library(lmtest)
library(forecast)

# analyse de la présence éventuelle d’autocorrélation dans les rendements et dans les rendements au carré

### Corrélogramme (ACF) ----
op<-par(mfrow=c(2,1))
Acf(rte,main='ACF du rendement logarithmique')
Acf(rte^2,main='ACF du rendement logarithmique au carrée')
par(op)

op<-par(mfrow=c(2,1))
Pacf(rte,main='ACF du rendement logarithmique')
Pacf(rte^2,main='ACF du rendement logarithmique au carrée')
par(op)

# L’analyse des ACF et PACF montre que, pour les rendements logarithmiques, seul 
# rho(32) dépasse légèrement l’intervalle de confiance, ce qui indique une 
# autocorrélation significative mais faible en raison de sa petite valeur (< 0.06). 
# En revanche, pour les rendements au carré, aucune autocorrélation significative 
# n’apparaît dans les ACF comme dans les PACF.


### Test de Ljung-Box
# Rendement logarithmique au carrée
Box.test(rte^2,lag=1,type="Ljung-Box") 
# p-value = 1.134e-06 < 5%
# On rejette donc l’hypothèse nulle d’absence d’autocorrélation

# Rendement logarithmique
pvaluesrt =rep(0,40)
pvaluesrt <- numeric(40) 
for (i in 1:40) {
  pvaluesrt[i] = Box.test(rte,lag=i,type="Ljung-Box")$p.value
}
pvaluesrt
# p-value appliqué à lag 40 = 0.013 < 5%, on rejette l’hypothèse nulle d’abscence d’autocorrélation pour  rte. 
# Pour modéliser cette caractéristique nous utiliserons un modèle ARMA(p,q) 


### EACF ----
# Dans un premier temps, nous devons déterminer la valeur de p et q du ARMA(p,q) via l’eacf. 
eacf(rte)
# p = 2 et q = 2


### Arima ----
#### ARMA(2,2) ----
reg<-Arima(rte, order=c(2,0,2))
coeftest(reg)
# Le modèle ne converge pas donc non valide. 



#### MA(32) ----
# Comme on a observé de l'autocorrélation à l'ordre 32, nous testons un MA(32) 
 
reg<-Arima(rte, order=c(0,0,32))
coeftest(reg)

# HO : theta(k) = 0 VS Ha : theta(k) =/= 0 
# Les coefficients ne sont pas tous significatifs. 
# On va fixer à 0 des coefficients qui ne sont pas significatifs un à un.

# Nous commence par fixer à 0 theta(9) car sa p-value = 0.991 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)
 
# Fixons à 0 theta(24) car sa p-value = 0.952 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)
 
# Fixons à 0 theta(18) car sa p-value = 0.923 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, NA, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(14) car sa p-value = 0.893 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)
 
# Fixons à 0 theta(15) car sa p-value = 0.870  > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)
 
# Fixons à 0 theta(2) car sa p-value = 0.777  > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, NA, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(19) car sa p-value = 0.755  > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, NA, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, 0, 0, NA, NA, 0, 0, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)
 
# Fixons à 0 theta(3) car sa p-value = 0.730  > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, 0, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, 0, 0, NA, NA, 0, 0, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(20) car sa p-value = 0.700  > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, 0, NA, NA, NA, NA, NA, 0, NA,
                                           NA, NA, NA, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)
 
# Fixons à 0 theta(11) car sa p-value = 0.668  > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, 0, NA, NA, NA, NA, NA, 0, NA,
                                           0, NA, NA, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)
 
# Fixons à 0 theta(13) car sa p-value = 0.666  > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, 0, NA, NA, NA, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(5) car sa p-value = 0.609 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(28) car sa p-value = 0.605 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(6) car sa p-value = 0.530 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(NA, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(1) car sa p-value = 0.486 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(22) car sa p-value = 0.476 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, 0, NA, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(23) car sa p-value = 0.358 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, 0, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(17) car sa p-value = 0.375 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           NA, 0, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(25) car sa p-value = 0.335 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           NA, 0, 0, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(21) car sa p-value = 0.259 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(29) car sa p-value = 0.262 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, 0, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(4) car sa p-value = 0.243 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, 0, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(16) car sa p-value = 0.192 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, 0, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(27) car sa p-value = 0.188 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, 0, 0, 0, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(7) car sa p-value = 0.151 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, 0, NA, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, 0, 0, 0, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(30) car sa p-value = 0.113 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, 0, NA, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, 0, 0, 0, 0,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(10) car sa p-value = 0.130 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, 0, NA, 0, 0,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, 0, 0, 0, 0,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(8) car sa p-value = 0.094 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, 0, 0, 0, 0,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(31) car sa p-value = 0.108 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(26) car sa p-value = 0.055 > 0,05.
reg<-Arima(rte, order=c(0,0,32), fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, NA, NA))
coeftest(reg)

# Enlevons la constante car elle n'est pas significatif (p-value = 0.777 > 0,05).
reg1<-Arima(rte, order=c(0,0,32), 
           include.mean = F,
           fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, NA),)
coeftest(reg1)

# Maintenant que nous avons un modele avec tous les coefficients qui sont significatif
# Celui-ci n'est valide que si les aléas sont des Bruits Blancs i.e.
# E[epsilon] = 0 et epsilon ne sont pas autocorrélés

# Test de l'espérance nulle des erreurs
# H0 : E[epsilon] = 0 VS Ha : E[epsilon] =/= 0
residu<-reg1$res
t.test(residu)
# p-value = 0.7829 > 0.05 donc on accepte H0 
# L'espérance des aléas sont nulles 



## Test d'abscence d'autocorrélation dans les aléas 
library(tseries)
residuv=(residu-mean(residu))/sd(residu)

Acf(residuv) # On observe aucune autocorrélation

tmp<-rep(0,40)
for(i in 1:40){
  tmp[i]<-Box.test(residuv,lag=i,type="Ljung-Box")$p.value
}
tmp
# p-values > 5 %. 
# On accepte H0, absence d’autocorrélation des aléas

# Notre modèle est donc valide. 
 

#### AR(32) ----
# Pour la même raison que nous avons tester le MA(32), nous testons un AR(32) 

reg<-Arima(rte, order=c(32,0,0))
coeftest(reg)

# HO : phi_k = 0 VS Ha : phi_k =/= 0 
# Les coefficients ne sont pas tous significatifs. 
# On va fixer à 0 des coefficients qui ne sont pas significatifs un à un.

# Nous commence par fixer à 0 phi(24) car sa p-value = 0.986 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(13) car sa p-value = 0.937 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, 0, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(3) car sa p-value = 0.913 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, NA, 0, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, 0, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(14) car sa p-value = 0.902 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, NA, 0, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(2) car sa p-value = 0.792 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(18) car sa p-value = 0.685 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(20) car sa p-value = 0.728 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, 0,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(5) car sa p-value = 0.723 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, 0,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(19) car sa p-value = 0.653 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, 0, 0, 0,
                                           NA, NA, NA, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(23) car sa p-value = 0.570 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, 0, 0, 0,
                                           NA, NA, 0, 0, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(28) car sa p-value = 0.537 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, NA, NA,
                                           NA, NA, 0, 0, NA, NA, NA, 0, 0, 0,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(9) car sa p-value = 0.509 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, 0, NA,
                                           NA, NA, 0, 0, NA, NA, NA, 0, 0, 0,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(11) car sa p-value = 0.496 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(NA, 0, 0, NA, 0, NA, NA, NA, 0, NA,
                                           0, NA, 0, 0, NA, NA, NA, 0, 0, 0,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(1) car sa p-value = 0.424 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, NA, NA, NA, 0, NA,
                                           0, NA, 0, 0, NA, NA, NA, 0, 0, 0,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(15) car sa p-value = 0.412 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, NA, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(6) car sa p-value = 0.386 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, NA, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(22) car sa p-value = 0.392 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, 0, 0, 0, NA, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(25) car sa p-value = 0.357 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, NA, 0, 0, 0,
                                           NA, 0, 0, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(17) car sa p-value = 0.376 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           NA, 0, 0, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(21) car sa p-value = 0.217 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, NA,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(10) car sa p-value = 0.209 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, NA, 0, 0, NA, NA, 0, 0,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(4) car sa p-value = 0.200 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, 0,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, NA, NA,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(30) car sa p-value = 0.178 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, 0,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, NA, 0,
                                           NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(31) car sa p-value = 0.159 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, 0,
                                           0, NA, 0, 0, 0, NA, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, NA, 0,
                                           0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(16) car sa p-value = 0.161 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, 0,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, NA, 0, NA, 0,
                                           0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(27) car sa p-value = 0.158 > 0,05.
reg<-Arima(rte, order=c(32,0,0), fixed = c(0, 0, 0, 0, 0, 0, NA, NA, 0, 0,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, NA, 0, 0, NA, 0,
                                           0, NA, NA))
coeftest(reg)

# Le modèle ne converge pas donc non valide. 



#### ARMA(1,1) ----
reg<-Arima(rte, order=c(1,0,1))
coeftest(reg)
# Le modèle ne converge pas donc non valide. 





# >>>>> Nous allons donc conserver notre MA(32)



## PROPRIETE 4 : Clusters de volatilité ----------------------------------------
# Nous analysons maintenant l'existence de clusters de volatilité. 
# Ce phénomène, remet en cause l’hypothèse d’homoscédasticité conditionnelle. 
# Afin de vérifier formellement si la variance dépend des chocs passés, nous 
# appliquons le test ARCH d’Engle (1982) sur la série  rte.
library(FinTS)
 
LM1<-ArchTest(as.numeric(rte),lag=1)
LM1 # p-value < 2.2e-16 < 5%

# 0n rejette l’hypothèse nulle d'homoscédasticité conditionnelle donc présence de 
# clusters de volatilités 



## PROPRIETE 5 : Queues épaisses conditionnelles -------------------------------
library(tseries)
library(fGarch)
library(FinTS)
library(moments)

# Après avoir estimé l’équation de la moyenne avec un modèle MA(25) et vérifié que 
# ses résidus ne présentent plus d’autocorrélation, le test d’Engle met en évidence 
# la présence de volatilité conditionnelle. Il est donc nécessaire de modéliser la 
# variance avec un modèle GARCH, en particulier le GARCH(1,1).

volat<-garch(residuv,order=c(1,1))
# Ne converge pas donc faire garchFit

volat<-garchFit(~garch(1,1),data=residuv,trace=F,include.mean = FALSE)
summary(volat)
# H0 : alpha(k) = 0 VS Ha : alpha(k) =/= 0
# On a nos coefficients qui sont significatifs (p-value < 5%).

# On souhaite maintenant savoir si avec notre modèle on a réussi à prendre en compte
# toutes l'hétéroscédasticité conditionnelle 

resvolat=volat@residuals/volat@sigma.t
ArchTest(resvolat, lag = 1)
# p-value 0.06309 > 5% donc pas de cluster de volatilité

ArchTest(resvolat, lag = 20)
# p-value 0.9441 > 5% donc pas de cluster de volatilité jusqu'à l'ordre 20

# Les tests ARCH appliqués aux résidus du modèle GARCH(1,1) donnent des p-values 
# supérieures à 5 % (pour les lags 1 et 20), ce qui conduit à accepter 
# l’hypothèse d’absence d’effets ARCH. 
 
anscombe.test(resvolat)
# p-value < 2.2e-16 < 5 %, nous rejetons l’hypothèse nulle selon laquelle la kurtosis 
# serait égale à 3. 
# Ainsi, nous pouvons interpréter sa valeur calculé : kurt = 11.657 >> 3, cela 
# indique une distribution fortement leptokurtique


# Même après la prise en compte de la volatilité conditionnelle, les résidus 
# présentent donc encore des queues épaisses, confirmant la présence de leptokurtose conditionnelle.





## PROPRIETE 6 : Effet de levier -----------------------------------------------
# Dans cette partie, nous cherchons à mettre en évidence l’effet de levier, 
# c’est-à-dire l’asymétrie entre l’impact des baisses et des du rendement 
# logarithmique sur la volatilité du titre.
T <- length(rte)

sig <- rep(NA, T)
for (t in 22:T) {
  sig[t] <- sd(rte[(t-21):t])
}

sigma_full <- sig
sigma <- sigma_full[24:T] * 100

summary(log(pt[24:length(rte)]))
summary(sigma)

dates_sub <- dates[24:length(rte)]

par(mar = c(5, 4, 4, 4) + 0.1)

plot(dates_sub, log(pt[24:length(rte)]),
     type = "l", col = 2, axes = FALSE, xlab = "", ylab = "", lwd = 3)

axis(2, at = seq(9.5, 11.5, by = 0.25))   # axe de gauche

years <- unique(format(dates_sub, "%Y"))
ticks <- as.Date(paste0(years, "-01-01"))
ticks <- ticks[ticks >= min(dates_sub) & ticks <= max(dates_sub)]
axis(1, at = ticks, labels = format(ticks, "%Y"))

par(new = TRUE)

plot(dates_sub, sigma,
     type = "l", col = "grey", axes = FALSE, xlab = "", ylab = "")

axis(4, at = seq(0, 7.5, by = 0.25))      # axe de droite 

legend("topleft",legend = c("log(pt)", "sigma"),
       col = c(2, "grey"),lty = c(1, 1),lwd = c(3, 1))



# Sigma + forte hausse/+ forte baisse
idx_test <- which(dates %in% dates_rte)

sigma_test_pct <- sigma_full[idx_test] * 100
dpt_test <- dpt[idx_test]

i_min <- which.min(dpt_test)
i_max <- which.max(dpt_test)

date_min_dpt_test <- dates_rte[i_min]
date_max_dpt_test <- dates_rte[i_max]

sigma_at_min_dpt <- sigma_test_pct[i_min]
sigma_at_max_dpt <- sigma_test_pct[i_max]

res_test <- data.frame(Type = c("Plus forte baisse", "Plus forte hausse"),
                       Date = c(date_min_dpt_test, date_max_dpt_test),
                       Rendement = c(dpt_test[i_min], dpt_test[i_max]),
                       Sigma = c(sigma_at_min_dpt, sigma_at_max_dpt))
print(res_test, row.names = FALSE)
# sigma(Plus forte baisse) > sigma(Plus forte hausse) = effet de levier 
# La date du 28/09/2018 correspond à l’annonce de la perte du contrat T-X de l’US 
# Air Force, qui a provoqué un effondrement du cours de KAI, alors que celle du 
# 19/10/2017 coïncide avec une phase de reprise de confiance lors du salon ADEX 2017.


## PROPRIETE 7 : La saisonnalité -----------------------------------------------
library(moments)

### Journalier - Effet week-end ----
# Dans la littérature, plusieurs travaux montrent que l’accumulation d’informations 
# pendant les périodes de fermeture augmente la volatilité, en particulier en 
# début de semaine (French & Roll, 1986) ou à partir du mercredi (Baillie & Bollerslev, 1989). 

# On crée un vecteur de dates aligné avec rte
dates_rte <- tail(dates, length(rte)) # mêmes longueurs
jour <- format(dates_rte, format = "%A") # 2213 jours, comme rte

tableaures <- data.frame(matrix(NA,ncol=5,nrow=4))
colnames(tableaures) <- c("lundi","mardi","mercredi","jeudi","vendredi")
rownames(tableaures) <- c("moyenne en %","écart-type annuel en %","skewness","kurtosis")

rtmar <- as.numeric(rte[jour=="mardi"])
mardi <- mean(rtmar) 
tableaures[1,2] <- mardi*100
tableaures[2,2] <- sd(rtmar)*100*sqrt(252)
tableaures[3,2] <- skewness(rtmar)
tableaures[4,2] <- kurtosis(rtmar)

rtmer <- as.numeric(rte[jour=="mercredi"])
mer <- mean(rtmer)
tableaures[1,3] <- mer*100
tableaures[2,3] <- sd(rtmer)*100*sqrt(252)
tableaures[3,3] <- skewness(rtmer)
tableaures[4,3] <- kurtosis(rtmer)

rtjeu <- as.numeric(rte[jour=="jeudi"])
jeudi <- mean(rtjeu)
tableaures[1,4] <- jeudi*100
tableaures[2,4] <- sd(rtjeu)*100*sqrt(252)
tableaures[3,4] <- skewness(rtjeu)
tableaures[4,4] <- kurtosis(rtjeu)

rtven <- as.numeric(rte[jour=="vendredi"])
ven <- mean(rtven)
tableaures[1,5] <- ven*100
tableaures[2,5] <- sd(rtven)*100*sqrt(252)
tableaures[3,5] <- skewness(rtven)
tableaures[4,5] <- kurtosis(rtven)

rtlun <- as.numeric(rte[jour=="lundi"])
lundi <- mean(rtlun)
tableaures[1,1] <- lundi*100
tableaures[2,1] <- sd(rtlun)*100*sqrt(252)
tableaures[3,1] <- skewness(rtlun)
tableaures[4,1] <- kurtosis(rtlun)

tableaures

# Dans nos données, on retrouve partiellement cet effet : la moyenne de lundi ne 
# se distingue pas des autres, cependant on a les volatilités qui deviennent 
# plus élevées à partir du mercredi et atteignent même un maximum le vendredi, ce 
# qui correspond davantage aux observations de Baillie & Bollerslev (1989).
# On observe donc bien un effet week-end 




### Mensuelle - Effet janvier ----
 
monthplot(rte, ylab="rendement", main="", cex.main=1, 
          col.base= 2, lwd.base=3, 
          col='grey', ,lwd=0.01)
 
# Nous avons la volatilité des rendements en janvier qui est beaucoup plus 
# importante que les autres mois. 
# Concernant les rendements moyens, les plus élevés apparaissent en juillet et 
# en septembre, tandis que les mois les plus faibles paraissent être en janvier 
# et décembre. Cependant, aucune moyenne ne se distingue réellement 



## PROPRIETE 8 : Stationnarité -------------------------------------------------

### Test de Dickey-Fuller (DF) ----
library(urca) 
 
summary(ur.df(rte,type= "trend",lags=0))
# H0 : B1 = 0 vs Ha : B1 =/= 0
# p-value(B1) = 0.505 > 0.05 le coefficient n'est pas significatif 
# Donc on estime drift 


summary(ur.df(rte,type= "drift",lags=0)) 
# H0 : B0 = 0 vs Ha : B0 =/= 0
# p-value(B0) = 0.755 > 0.05 B0 n'est pas significatif 
# Donc on estime none 

summary(ur.df(rte,type= "none",lags=0))
# H0 : rho-1 = 0 vs Ha : rho-1 =/= 0
# On a t.calculé = -47.7589 < -1.95 donc on rejette H0

# Ainsi on peut conclure à partir du test de DF que le PGD est stationnaire. 
# valide que si les aléas de la régression de Dickey et Fuller ne sont pas 
# auto-corrélés. 


plot(ur.df(rte,lag=0,type="none"))
# La figure nous indique que les aléas sont auto-corrélés aux ordres 32 et donc 
# notre conclusion concernant l’absence de RU n’est pas valide. 
# Nous devons effectuer un test de RU dans le cadre de la régression de Dickey Fuller Augmenté.





### Test de Dickey Fuller Augmenté (ADF) ----
library(CADFtest)
T = length(rte)
Schwert<-as.integer(12*(T/100)^(0.25)) # = 26

# Nous vérifions que B0 est bien non significatif 
summary(CADFtest(rte, criterion="MAIC",type="drift",max.lag.y=Schwert)) 
# Le MAIC nous donne un lag de 6 
summary(ur.df(rte,type= "drift",lags=6))
# p-value(B0) = 0.772 > 0.05 B0 n'est pas significatif 


# Donc on estime bien none 
summary(CADFtest(rte, criterion="MAIC",type="none",max.lag.y=Schwert)) 
# Le MAIC nous donne un lag de 6 

summary(ur.df(rte,type= "none",lags=6))
# On valide la spécification lorsque le dernier gamma est significatif.
# t.stat = |-1.771| > 1.64 donc dernier gamma significatif 

# On peut donc passer au test de racine unitaire : 
# H0 : rho-1 = 0 vs Ha : rho-1 =/= 0
# On a t.calculé = -17.0962  < -1.95 donc on rejette H0

# Ainsi on peut conclure à partir du test de DFA que le PGD est stationnaire. 


### Test de Zivot et Andrews (ZA) ----
# Cependant, ces résultat non sont valide que si pendant toute la période d'étude, 
# il n'y a pas de choc structurelle u conjoncturelle 
# H0 : DS sans date de rupture 
# Ha : TS avec une date de rupture 

# On commence avec comme choix du model "both" 
summary(ur.za(rte, model = "both", lag = Schwert))
# Dernier gamma non significatif car sa statistique t associée (1.032) est inférieure en valeur absolue au seuil de 1,64. 

summary(ur.za(rte, model = "both", lag = 25))
# Dernier gamma significatif car sa statistique t associée (2.144) est supérieure en valeur absolue au seuil de 1,64. 

# On observe que delta 1 et delta 2 sont significatif. 
# En effet, leurs p-values, respectivement 0.0001 et 0.0086, sont bien 
# inférieur à notre seuil de 5%
# De même pour beta(1) avec sa p-value de 0.0012 < 5%.

# On a donc le bon modèle, on peut donc faire le test de racine unitaire : 
# H0 : rho-1 = 0 
# t.stat = -10.9818 < -5.08 = t.critique ainsi on rejette H0 
# Le PGD est donc TS avec date de rupture. 

break_point = 890 
dates_rte[break_point] # 2015-08-10

# Cependant, comme le rendement n'a pas de tendance alors il ne peut pas être TS,
# le PGD est donc stationnaire avec une date de rupture autour du 10 août 2015. 
# Cette date correspond à la période où le cours de l’action KAI atteignait son 
# pic historique (≈ 100 000 ₩ le 7 août 2015), après avoir été multiplié par trois 
# en trois ans.



### Test de Lee et Strazicich (LS) ----
# Cependant, nous pouvons rejeter/accepter H0 dans ZA alors que possibilité 
# d'un PGD DS avec une date de rupture 
# On va donc faire un test LS 

# Comme dans ZA notre model est both alors dans LS notre model est break 

# sourcer LeeStrazicichUnitRootTest.R
myLS_test <- ur.ls(y = rte, model = "break", breaks = 1,
                   lags = Schwert , method = "GTOS", pn = 0.1, print.results = "print")

# t.stat = -13.99145 < -4.5  = t.critique donc on rejette H0 
# Ainsi, le PGD qui a généré les données est TS avec une date de rupture.

# Nous avons suffisamment d'observation pour ne pas faire bootstrap (2213) 

break_point = 834 
dates_rte[break_point] # 2020-12-30

# Cette date coïncide avec la signature d’un contrat majeur de production de 
# KUH-1 Surion avec l’agence d’armement DAPA (environ 1 000 milliards de wons).

# Cependant, comme le rendement n'a pas de tendance alors il ne peut pas être TS,
# le PGD est donc stationnaire avec une date de rupture autour du 30 décembre 2020. 


