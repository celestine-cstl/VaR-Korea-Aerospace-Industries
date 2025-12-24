# Value at Risk (VaR) de l'action Korea Aerospace Industries (047810.KS)

**Projet en 2 parties : (1) propriétés stylisées des rendements & comparaison temporelle, (2) estimation de VaR 1-jour (95%) avec plusieurs méthodes + backtesting (Kupiec / Christoffersen) en rolling window, avec modélisation de la volatilité (apARCH).**

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=rstudio&logoColor=white)
![xts](https://img.shields.io/badge/xts-444444?style=for-the-badge&logo=r&logoColor=white)
![forecast](https://img.shields.io/badge/forecast-444444?style=for-the-badge&logo=r&logoColor=white)
![rugarch](https://img.shields.io/badge/rugarch-444444?style=for-the-badge&logo=r&logoColor=white)
![Word](https://img.shields.io/badge/Word-2B579A?style=for-the-badge&logo=microsoft-word&logoColor=white)

> Travail réalisé majoritairement sous **R** : récupération et préparation des rendements (Yahoo Finance), analyse statistique (moments, tests), visualisations, estimation de modèles de volatilité (**(ap)ARCH/GARCH**) et calcul de **VaR** (paramétrique / non-paramétrique), puis **backtesting** (Kupiec & Christoffersen) avec une procédure **rolling window**.

<br>

## 🎯 Objectifs

### Partie 1 - Propriétés stylisées
- Étudier les **8 caractéristiques statistiques** classiques des rendements financiers (normalité, queues épaisses, asymétrie, autocorrélations, clustering de volatilité, effets ARCH, etc.)
- Comparer **période d’estimation** vs **période plus récente** pour discuter la stabilité des faits stylisés

### Partie 2 - VaR & Backtesting
- Estimer la **VaR à 1 jour (95%)** via différentes approches
- Évaluer la qualité des modèles via **backtesting** :
  - **Kupiec** 
  - **Christoffersen** 
- Mettre en place une **rolling window** pour simuler un usage “en production”

<br>

## 🛠️ Compétences mobilisées

- **R / Time series** : manipulation de séries temporelles (log-returns), cleaning, fenêtres glissantes, stabilité
- **Stylized facts** : skewness/kurtosis, tests de normalité, ACF/PACF, ARCH effects, stationnarité
- **VaR (1-jour, 95%)**
  - Paramétrique : **VaR Normale**
  - Paramétrique améliorée : **Cornish–Fisher**
  - Non-paramétrique : **Historical Simulation**
  - Conditionnelle : **(ap)ARCH/GARCH** + innovations (ex. sSTD)
- **Backtesting** : Kupiec, Christoffersen, analyse des violations (clusters, indépendance)
## 📁 Structure conseillée du dépôt

