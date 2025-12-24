
# PROJET 1 - Korea Aerospace Industries (047810.KS)

# Importation des données ------------------------------------------------------
rm(list = ls(all = TRUE))
library(yfR)

## KAI 047810.KS
my_ticker  <- "047810.KS"
first_date <- "2012-01-01"
last_date  <- "2025-11-07"

df_yf <- yf_get(tickers = my_ticker, first_date = first_date, last_date   = last_date,
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




# II - Analyse des 8 propriétés statistiques du rendement logarithmique de la série rtt ----
s=sd(rtt) # écart type estimé
rbar<-mean(rtt) # moyenne empirique
rbar # = 0.00110

# Est-ce que cette moyenne empirique proche de 0 est statistiquement nulle ? 
# H0 : E[rtt] = mu = 0 VS Ha : E[rtt] = mu =/= 0 
t.test(rtt) 
# Comme p-value = 0.1448 > 0.05, alors on accepte H0. 
# Ainsi, l'espérance du PGD qui a généré  rtt est nulle.





## PROPRIETE 1 - Asymétrie perte/gain ------------------------------------------
library(moments)

agostino.test(rtt)

# p-value = 0.1428 > 5 %, on accepte donc H0 : la skewness n’est pas 
# significativement différente de zéro, ce qui indique l’absence d’asymétrie.




## PROPRIETE 2 : Queues de distribution épaisses -------------------------------
anscombe.test(rtt)

# p-value < 2.2e-16 < 5 %, nous rejetons l’hypothèse nulle selon laquelle la kurtosis 
# serait égale à 3. 

# Ainsi, nous pouvons interpréter sa valeur calculée : kurt = 5.8089 > 3, cela 
# indique une distribution fortement leptokurtique, i.e. une distribution dont la 
# cloche est plus pointue que celle de la loi gaussienne, avec des queues plus 
# importante.


# Comparaison de rtt avec une loi normale : 
mu <- mean(rtt, na.rm = TRUE)
sigma <- sd(rtt, na.rm = TRUE)

hist(rtt, breaks = 70, freq = FALSE, col = "grey",
     main = "Comparaison de la distribution des rendements \n logarithmiques avec la loi normale",
     xlab = "Rendements",
     ylab = "Densité",
     border = "white")

lines(density(rtt), col = "brown", lwd = 2)

curve(dnorm(x, mean = mu, sd = sigma),
      from = min(rtt), to = max(rtt),
      add = TRUE, col = "steelblue", lwd = 2)
# On observe bien la distribution leptokurtique



## PROPRIETE 3 : Autocorrélations des carrés des rendements forts et faibles pour les rendements ----
library(FinTS)
library(TSA)
library(lmtest)
library(forecast)

# analyse de la présence éventuelle d’autocorrélation dans les rendements et dans les rendements au carré

### Corrélogramme (ACF) ----
op<-par(mfrow=c(2,1))
Acf(rtt,main='ACF du rendement logarithmique')

Acf(rtt^2,main='ACF du rendement logarithmique au carrée')
par(op)
# rho(1), rho(10), rho(11), rho(12), rho(22), rho(23) significatif 
# # mais les rho prennent des petite valeur (max 0.07) 
# autocorrélation dans les rendements 

# autocorrélation aussi dans les rendements au carrées (+ fort) 


### Test de Ljung-Box ----

# Rendement logarithmique au carrée
Box.test(rtt^2,lag=1,type="Ljung-Box") 
# p-value = 6.629e-05 < 5%
# On rejette donc l’hypothèse nulle d’absence d’autocorrélation

# Rendement logarithmique
Box.test(rtt,lag=1,type="Ljung-Box") 
# p-value = 0.00815 < 5%
# On rejette donc l’hypothèse nulle d’absence d’autocorrélation

# Pour modéliser cette caractéristique nous utiliserons un modèle ARMA(p,q) 


### EACF ----
# Dans un premier temps, nous devons déterminer la valeur de p et q du ARMA(p,q) via l’eacf. 
eacf(rtt)
# p = 4 et q = 4


### Arima ----
#### ARMA(4,4) ----
reg<-Arima(rtt, order=c(4,0,4))
coeftest(reg)

# HO : theta(k) = 0 VS Ha : theta(k) =/= 0 
# HO : phi(k) = 0 VS Ha : phi(k) =/= 0 

# Les coefficients ne sont pas tous significatifs. 
# On va fixer à 0 des coefficients qui ne sont pas significatifs un à un.

# Nous commence par fixer à 0 phi(2) car sa p-value = 0.8381 > 0,05.
reg<-Arima(rtt, order=c(4,0,4), fixed = c(NA, 0, NA, NA, 
                                          NA, NA, NA, NA,
                                          NA))
coeftest(reg)
# le modèle ne converge pas, il n'est donc pas valide. 


#### ARMA(1,1) ----
reg<-Arima(rtt, order=c(1,0,1))
coeftest(reg)

# Fixons à 0 theta(1) car sa p-value = 0.2878 > 0,05.
# Cela revient à faire un AR(1)

#### AR(1) ----
reg<-Arima(rtt, order=c(1,0,0))
coeftest(reg)

# Enlevons la constante car elle n'est pas significatif (p-value = 0.115 > 0,05).
reg1 <-Arima(rtt, order=c(1,0,0), include.mean = F)
coeftest(reg1)

# Miantenant que nous avons un modele avec tous les coefficients qui sont significatif
# Celui-ci n'est valide que si les aléas sont des Bruits Blancs i.e.
# E[epsilon] = 0 et epsilon ne sont pas autocorrélés

# Test de l'espérance nulle des erreurs
# H0 : E[epsilon] = 0 VS Ha : E[epsilon] =/= 0
residu<-reg1$res
t.test(residu)
# p-value = 0.1157 > 0.05 donc on accepte H0 
# L'espérance des aléas sont nulles 


## Test d'abscence d'autocorrélation dans les aléas 
library(tseries)
residuv=(residu-mean(residu))/sd(residu)

Acf(residuv) # On observe encore de l'autocorrélation dans nos résidus 
# rho(10), rho(12) et rho(27) significatif

# Notre modèle n'est donc pas valide.

# Il faut donc un modèle avec P ou Q plus grand. 




#### MA(23) ----
# Comme on a observé de l'autocorrélation à l'ordre 23 dans l'ACF du rendement 
# logarithmique, nous testons un MA(23)

reg<-Arima(rtt, order=c(0,0,23))
coeftest(reg)

# HO : theta(k) = 0 VS Ha : theta(k) =/= 0 
# Les coefficients ne sont pas tous significatifs. 
# On va fixer à 0 des coefficients qui ne sont pas significatifs un à un.

# Nous commence par fixer à 0 tous les theta ayant une p-value très élevée :
# theta(3), theta(5), theta(6), theta(7), theta(8), theta(13), theta(14), 
# theta(15), theta(16), theta(20) 
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, NA, 0, NA, 0, 0, 0, 0, NA, NA,
                                           NA, NA, 0, 0, 0, 0, NA, NA, NA, 0,
                                           NA, NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(21) car sa p-value = 0.23672 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, NA, 0, NA, 0, 0, 0, 0, NA, NA,
                                           NA, NA, 0, 0, 0, 0, NA, NA, NA, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(14) car sa p-value = 0.90165 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, NA, 0, NA, 0, 0, 0, 0, NA, NA,
                                           NA, NA, 0, 0, 0, 0, NA, NA, NA, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(9) car sa p-value = 0.27212 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, NA, 0, NA, 0, 0, 0, 0, 0, NA,
                                           NA, NA, 0, 0, 0, 0, NA, NA, NA, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(2) car sa p-value = 0.16902 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           NA, NA, 0, 0, 0, 0, NA, NA, NA, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(11) car sa p-value = 0.12096 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, NA, NA, NA, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(17) car sa p-value = 0.11624  > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, NA, NA, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 theta(22) car sa p-value = 0.11600 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, NA, NA, 0,
                                           0, 0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(19) car sa p-value = 0.08165 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, NA, 0, 0,
                                           0, 0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(18) car sa p-value = 0.08373 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(4) car sa p-value = 0.05128 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(10) car sa p-value = 0.05783 > 0,05.
reg<-Arima(rtt, order=c(0,0,23), fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, NA, NA))
coeftest(reg)

# Enlevons la constante car elle n'est pas significatif (p-value = 0.777 > 0,05).
reg<-Arima(rtt, order=c(0,0,23), 
           include.mean = F,
           fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, NA))
coeftest(reg)


# Fixons à 0 theta(23) car sa p-value = 0.05783 > 0,05 donc cela revient à faire un 
# MA(12)

#### MA(12) ----
# en lien avec notre rho(12) qui sortait dans l'ACf des rendements logarithmique
reg1 <-Arima(rtt, order=c(0,0,12), 
           include.mean = F,
           fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, NA))
coeftest(reg1)


# Maintenant que nous avons un modele avec tous les coefficients qui sont significatif
# Celui-ci n'est valide que si les aléas sont des Bruits Blancs i.e.
# E[epsilon] = 0 et epsilon ne sont pas autocorrélés

# Test de l'espérance nulle des erreurs
# H0 : E[epsilon] = 0 VS Ha : E[epsilon] =/= 0
residu<-reg1$res
t.test(residu)
# p-value = 0.09023 > 0.05 donc on accepte H0 
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




#### AR(23) ----
# Pour la même raison que nous avons tester le MA(23), nous testons un AR(23) 

reg<-Arima(rtt, order=c(23,0,0))
coeftest(reg)

# HO : phi_k = 0 VS Ha : phi_k =/= 0 
# Les coefficients ne sont pas tous significatifs. 

# Nous commence par fixer à 0 tous les phi ayant une p-value très élevée :
# phi(3), phi(5), phi(6), phi(7), phi(8), phi(13), phi(14), phi(15)
reg<-Arima(rtt, order=c(23,0,0), fixed = c(NA, NA, 0, NA, 0, 0, 0, 0, NA, NA,
                                           NA, NA, 0, 0, 0, NA, NA, NA, NA, NA,
                                           NA, NA, NA, NA))
coeftest(reg)

# De même pour : phi(2), phi(9), phi(11), phi(16), phi(17), phi(20), phi(21)
reg<-Arima(rtt, order=c(23,0,0), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, NA, NA, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 phi(19) car sa p-value = 0.13537 > 0,05.
reg<-Arima(rtt, order=c(23,0,0), fixed = c(NA, 0, 0, NA, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, NA, 0, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 phi(4) car sa p-value = 0.10417 > 0,05.
reg<-Arima(rtt, order=c(23,0,0), fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, NA, 0, 0,
                                           0, NA, NA, NA))
coeftest(reg)

# Fixons à 0 phi(22) car sa p-value = 0.11160 > 0,05.
reg<-Arima(rtt, order=c(23,0,0), fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, NA, 0, 0,
                                           0, 0, NA, NA))
coeftest(reg)

# Fixons à 0 phi(18) car sa p-value = 0.11054 > 0,05.
reg<-Arima(rtt, order=c(23,0,0), fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                                           0, NA, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, NA, NA))
coeftest(reg)

# Fixons à 0 theta(23) car sa p-value = 0.08396 > 0,05 donc cela revient à faire un 
# AR(12)

#### AR(12) ----
reg<-Arima(rtt, order=c(12,0,0), fixed = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                                           0, NA, NA))
coeftest(reg)

# le modèle ne converge pas, il n'est donc pas valide. 




# >> On choisi notre MA(12)


## PROPRIETE 4 : Clusters de volatilité ----------------------------------------
# Nous analysons maintenant l'existence de clusters de volatilité. 
# Ce phénomène, remet en cause l’hypothèse d’homoscédasticité conditionnelle. 
# Afin de vérifier formellement si la variance dépend des chocs passés, nous 
# appliquons le test ARCH d’Engle (1982) sur la série  rtt.
library(FinTS)

LM1<-ArchTest(as.numeric(rtt),lag=1)
LM1 #  p-value = 6.813e-05 < 5%

# 0n rejette l’hypothèse nulle d'homoscédasticité conditionnelle donc présence de 
# clusters de volatilités 





## PROPRIETE 5 : Queues épaisses conditionnelles -------------------------------
library(tseries)
library(fGarch)
library(FinTS)
library(moments)

# Après avoir estimé l’équation de la moyenne avec un modèle MA(12) et vérifié que 
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
# p-value 0.9013 > 5% donc pas de cluster de volatilité

ArchTest(resvolat, lag = 20)
# p-value 0.6513 > 5% donc pas de cluster de volatilité jusqu'à l'ordre 20

# Les tests ARCH appliqués aux résidus du modèle GARCH(1,1) donnent des p-values 
# supérieures à 5 % (pour les lags 1 et 20), ce qui conduit à accepter 
# l’hypothèse d’absence d’effets ARCH. 

anscombe.test(resvolat)
# p-value = 3.845e-16 < 5 %, nous rejetons l’hypothèse nulle selon laquelle la kurtosis 
# serait égale à 3. 
# Ainsi, nous pouvons interpréter sa valeur calculée : kurt = 5.2520 >> 3, cela 
# indique une distribution fortement leptokurtique


# Même après la prise en compte de la volatilité conditionnelle, les résidus 
# présentent donc encore des queues épaisses, confirmant la présence de leptokurtose conditionnelle.






## PROPRIETE 6 : Effet de levier -----------------------------------------------
T <- length(rt) 

sigma_full <- rep(NA, T)
for (t in 22:T) {
  sigma_full[t] <- sd(rt[(t-21):t])
}

idx_test <- which(dates %in% dates_rtt)

dates_sub <- dates[idx_test]
pt_sub    <- pt[idx_test]
sigma_sub <- sigma_full[idx_test] * 100

par(mar = c(5, 4, 4, 4) + 0.1)

plot(dates_sub, log(pt_sub),
     type = "l", col = 2, axes = FALSE, xlab = "", ylab = "", lwd = 3)

axis(2, at = seq(9.5, 11.5, by = 0.25))

years <- unique(format(dates_sub, "%Y"))
ticks <- as.Date(paste0(years, "-01-01"))
ticks <- ticks[ticks >= min(dates_sub) & ticks <= max(dates_sub)]
axis(1, at = ticks, labels = format(ticks, "%Y"))

par(new = TRUE)

plot(dates_sub, sigma_sub,
     type = "l", col = "grey", axes = FALSE, xlab = "", ylab = "")

axis(4, at = seq(0, 7.5, by = 0.25))

legend("topleft",
       legend = c("log(pt)", "sigma"),
       col = c(2, "grey"),
       lty = c(1, 1),
       lwd = c(3, 1))
# semble y avoir effet de levier en fin 2022

# Sigma + plus forte hausse / plus forte baisse
dpt_test <- dpt[idx_test] 
idx_valid <- idx_test[!is.na(sigma_full[idx_test])]

sigma_valid_pct <- sigma_full[idx_valid] * 100
dpt_valid       <- dpt[idx_valid]

i_min <- which.min(dpt_valid)
i_max <- which.max(dpt_valid)

date_min_dpt <- dates[idx_valid][i_min]
date_max_dpt <- dates[idx_valid][i_max]

sigma_at_min_dpt <- sigma_valid_pct[i_min]
sigma_at_max_dpt <- sigma_valid_pct[i_max]

res_test <- data.frame(Type = c("Plus forte baisse", "Plus forte hausse"),
                       Date = c(date_min_dpt, date_max_dpt),       
                       Rendement = c(dpt_valid[i_min], dpt_valid[i_max]), 
                       Sigma = c(sigma_at_min_dpt, sigma_at_max_dpt))

print(res_test, row.names = FALSE)
# sigma(Plus forte baisse) > sigma(Plus forte hausse) = effet de levier 
# Cette chute intervient juste après l’annonce qu’un hélicoptère Surion de KAI a 
# été totalement détruit lors d’une collision avec un drone Heron, ce qui a pu 
# alimenter des craintes sur les risques opérationnels et provoquer une correction 
# du titre.
# Cette forte hausse correspond au jour où KAI dévoile le design de son nouvel 
# avion de guerre électronique lors du 2025 Electromagnetic Warfare Workshop à 
# Séoul, annonce perçue très positivement par le marché



## PROPRIETE 7 : La saisonnalité -----------------------------------------------
library(moments)

### Journalier - Effet week-end ----
# Dans la littérature, plusieurs travaux montrent que l’accumulation d’informations 
# pendant les périodes de fermeture augmente la volatilité, en particulier en 
# début de semaine (French & Roll, 1986) ou à partir du mercredi (Baillie & Bollerslev, 1989). 

# On crée un vecteur de dates aligné avec rtt
dates_rtt <- tail(dates, length(rtt)) 
jour <- format(dates_rtt, format = "%A") 

tableaures <- data.frame(matrix(NA,ncol=5,nrow=4))
colnames(tableaures) <- c("lundi","mardi","mercredi","jeudi","vendredi")
rownames(tableaures) <- c("moyenne en %","écart-type annuel en %","skewness","kurtosis")

rtmar <- as.numeric(rtt[jour=="mardi"])
mardi <- mean(rtmar) 
tableaures[1,2] <- mardi*100
tableaures[2,2] <- sd(rtmar)*100*sqrt(252)
tableaures[3,2] <- skewness(rtmar)
tableaures[4,2] <- kurtosis(rtmar)

rtmer <- as.numeric(rtt[jour=="mercredi"])
mer <- mean(rtmer)
tableaures[1,3] <- mer*100
tableaures[2,3] <- sd(rtmer)*100*sqrt(252)
tableaures[3,3] <- skewness(rtmer)
tableaures[4,3] <- kurtosis(rtmer)

rtjeu <- as.numeric(rtt[jour=="jeudi"])
jeudi <- mean(rtjeu)
tableaures[1,4] <- jeudi*100
tableaures[2,4] <- sd(rtjeu)*100*sqrt(252)
tableaures[3,4] <- skewness(rtjeu)
tableaures[4,4] <- kurtosis(rtjeu)

rtven <- as.numeric(rtt[jour=="vendredi"])
ven <- mean(rtven)
tableaures[1,5] <- ven*100
tableaures[2,5] <- sd(rtven)*100*sqrt(252)
tableaures[3,5] <- skewness(rtven)
tableaures[4,5] <- kurtosis(rtven)

rtlun <- as.numeric(rtt[jour=="lundi"])
lundi <- mean(rtlun)
tableaures[1,1] <- lundi*100
tableaures[2,1] <- sd(rtlun)*100*sqrt(252)
tableaures[3,1] <- skewness(rtlun)
tableaures[4,1] <- kurtosis(rtlun)

tableaures

# On observe bien un effet week-end
# En effet, lundi est le jour avec la moyenne la plus importante ainsi que 
# l'écart-type annuel le plus important. 



### Mensuelle - Effet janvier ----

monthplot(rtt, ylab="rendement", main="", cex.main=1, 
          col.base= 2, lwd.base=3, 
          col='grey', ,lwd=0.01)


# Nous observons un effet janvier dans nos données.
# Les rendements moyens les plus élevés apparaissent en janvier suivi d'octobre,
# tandis que les mois les plus faibles paraissent être en juin et décembre. 

# La volatilité la plus importante est en février.



## PROPRIETE 8 : Stationnarité -------------------------------------------------

### Test de Dickey-Fuller (DF) ----
library(urca) 

summary(ur.df(rtt,type= "trend",lags=0))
# H0 : B1 = 0 vs Ha : B1 =/= 0
# p-value(B1) = 0.848 > 0.05 le coefficient n'est pas significatif 
# Donc on estime drift 


summary(ur.df(rtt,type= "drift",lags=0)) 
# H0 : B0 = 0 vs Ha : B0 =/= 0
# p-value(B0) = 0.124 > 0.05 le coefficient n'est pas significatif 
# Donc on estime none 

summary(ur.df(rtt,type= "none",lags=0)) 
# H0 : rho-1 = 0 vs Ha : rho-1 =/= 0
# On a t.calculé = -37.0923 < -1.95 donc on rejette H0

# Ainsi on peut conclure à partir du test de DF que le PGD est stationnaire. 
# valide que si les aléas de la régression de Dickey et Fuller ne sont pas 
# auto-corrélés. 


plot(ur.df(rtt,lag=0,type="none"))
# La figure nous indique que les aléas sont auto-corrélés aux ordres 12 et 27 et donc 
# notre conclusion concernant l’absence de RU n’est pas valide. 
# Nous devons effectuer un test de RU dans le cadre de la régression de Dickey Fuller Augmenté.





### Test de Dickey Fuller Augmenté (ADF) ----
library(CADFtest)
T = length(rtt)
Schwert<-as.integer(12*(T/100)^(0.25)) # = 22

# Nous vérifions que B0 est bien non significatif 
summary(CADFtest(rtt, criterion="MAIC",type="drift",max.lag.y=Schwert)) 
# Le MAIC nous donne un lag de 2
summary(ur.df(rte,type= "drift",lags=2))
# p-value(B0) = 0.762 > 0.05 B0 n'est pas significatif 


# Donc on estime bien none 
summary(CADFtest(rtt, criterion="MAIC",type="none",max.lag.y=Schwert)) 
# Le MAIC nous donne un lag de 8

# Nous vérifions que B0 est bien non significatif 
summary(ur.df(rtt,type= "drift",lags=8))
# p-value(B0) = 0.144 > 0.05 B0 n'est pas significatif 
# Donc on estime none 

summary(ur.df(rtt,type= "none",lags=8)) 
# On valide la spécification lorsque le dernier gamma est significatif.
# t.stat = |-1.594| < 1.64 donc dernier gamma non significatif 

summary(ur.df(rtt,type= "none",lags=7)) 
# t.stat = |0.742| < 1.64 donc dernier gamma non significatif 

summary(ur.df(rtt,type= "none",lags=6)) 
# t.stat = |-0.051| < 1.64 donc dernier gamma non significatif 

summary(ur.df(rtt,type= "none",lags=5)) 
# t.stat = |-0.441| < 1.64 donc dernier gamma non significatif 

summary(ur.df(rtt,type= "none",lags=4)) 
# t.stat = |-0.069| < 1.64 donc dernier gamma non significatif 

summary(ur.df(rtt,type= "none",lags=3)) 
# t.stat = |1.359| < 1.64 donc dernier gamma non significatif 

summary(ur.df(rtt,type= "none",lags=2)) 
# t.stat = |-0.500| < 1.64 donc dernier gamma non significatif 

summary(ur.df(rtt,type= "none",lags=1)) 
# t.stat = |-1.309| < 1.64 donc dernier gamma non significatif 

# Donc cela revient à DF : le PGD est stationnaire. 


### Test de Zivot et Andrews (ZA) ----
# Cependant, ces résultat non sont valide que si pendant toute la période d'étude, 
# il n'y a pas de choc structurelle u conjoncturelle 
# H0 : DS sans date de rupture 
# Ha : TS avec une date de rupture 

# On commence avec comme choix du model "both" 
summary(ur.za(rtt, model = "both", lag = Schwert))
# gamma(22) significatif car sa statistique t associée (1.762) est supérieure en valeur absolue au seuil de 1,64. 

# On observe que delta 1 et delta 2 sont significatif. 
# En effet, leurs p-values, respectivement 0.0005 et 0.0042, sont bien 
# inférieur à notre seuil de 5%, on garde donc le modele "both"
# mais beta(1) non significatif avec sa p-value de 0.7442 < 5%.

# On a donc le bon modèle, on peut donc faire le test de racine unitaire : 
# H0 : rho-1 = 0 
# t.stat = -9.1123  < -5.08 = t.critique ainsi on rejette H0 
# Le PGD est donc TS avec date de rupture.  

break_point = 1005 
dates_rtt[break_point] # 2021-02-05

# Cette date correspond à une phase de forte reprise des valeurs industrielles 
# et aérospatiales en Corée du Sud, après le creux de 2020 lié à la pandémie.
# Pour Korea Aerospace Industries, cette période marque également le retour des 
# commandes militaires et exportations (notamment le programme FA-50), ce qui a 
# pu modifier la dynamique de la série de rendements.

# Cependant, comme le rendement n'a pas de tendance alors il ne peut pas être TS,
# le PGD est donc stationnaire avec une date de rupture le 5 février 2021.



### Test de Lee et Strazicich (LS) ----
# Cependant, nous pouvons rejeter H0 dans ZA alors que possibilité 
# d'un PGD DS avec une date de rupture 
# On va donc faire un test LS 
# H0 : DS avec une date de rupture 
# Ha : TS avec une date de rupture 

# Comme dans ZA notre model est both alors dans LS notre model est break 

# sourcer LeeStrazicichUnitRootTest.R
myLS_test <- ur.ls(y = rtt, model = "break", breaks = 1,
                   lags = 3, method = "GTOS", pn = 0.1, print.results = "print")

# t.stat = -37.09324 < -4.45  = t.critique donc on rejette H0 
# Ainsi, le PGD qui a généré les données est TS avec une date de rupture.

# Nous avons suffisamment d'observation pour ne pas faire bootstrap (2213) 

break_point = 404
dates_rtt[break_point] # 2022-08-24

# Cette date coïncide avec la période de négociation et de finalisation du contrat 
# d’exportation de 48 FA-50 vers la Pologne, annoncé fin juillet 2022 et signé en 
# septembre 2022.

# Cependant, comme le rendement n'a pas de tendance alors il ne peut pas être TS,
# le PGD est donc stationnaire. 


