#########################################
#       set du working directory        #
#à modifier selon l'emplacement souhaité#
#########################################
setwd("C:/Users/Utilisateur/OneDrive - Université de Poitiers/SAE/s2/saeStats")

# Importation des données
train      <- read.csv2("train.csv")
to_predict <- read.csv2("to_predict.csv")

# Conversion des colonnes numériques
train$Surface.reelle.bati <- as.numeric(gsub(",", ".", train$Surface.reelle.bati))
train$Valeur.fonciere     <- as.numeric(gsub(",", ".", train$Valeur.fonciere))

to_predict$Surface.reelle.bati <- as.numeric(gsub(",", ".", to_predict$Surface.reelle.bati))

# Suppression des lignes avec valeurs manquantes ou aberrantes
train <- train[!is.na(train$Surface.reelle.bati) & train$Surface.reelle.bati > 0, ]
train <- train[!is.na(train$Valeur.fonciere)     & train$Valeur.fonciere > 0,     ]

to_predict <- to_predict[!is.na(to_predict$Surface.reelle.bati) &
                           to_predict$Surface.reelle.bati > 0, ]

# Mise en place des saisons 
saison_immobiliere <- function(date) {
  date <- as.Date(date, format = "%d/%m/%Y")
  mois <- as.integer(format(date, "%m"))
  
  if (mois %in% c(12, 1, 2)) {
    return("hiver")
  } else if (mois %in% c(3, 4, 5)) {
    return("printemps")
  } else if (mois %in% c(6, 7, 8)) {
    return("ete")
  } else if (mois %in% c(9, 10, 11)) {
    return("automne")
  }
}

train$saison      <- sapply(train$Date.mutation,      saison_immobiliere)
to_predict$saison <- sapply(to_predict$Date.mutation, saison_immobiliere)

# Coefficients a et b
calcul_ab <- function(x, y) {
  xBarre<- mean(x)
  yBarre <- mean(y)
  a <- sum((x - xBarre) * (y - yBarre)) / sum((x - xBarre)^2)
  b <- yBarre - a * xBarre
  return(c(a = a, b = b))
}

# Coefficient de détermination R²
calcul_r2 <- function(yi, y_pred) {
  scr <- sum((yi - y_pred)^2)
  sct <- sum((yi - mean(yi))^2)
  return(1 - scr / sct)
}

# Application du modèle
appliquer_modele <- function(coefs, surface) {
  return(coefs["a"] * surface + coefs["b"])
}

#########################
# Découpage en segments #
#########################

# Appartements printemps + été
seg_appart_pe <- train[
  train$Type.local == "Appartement" &
    train$saison %in% c("printemps", "ete"), ]

# Appartements automne + hiver
seg_appart_ah <- train[
  train$Type.local == "Appartement" &
    train$saison %in% c("automne", "hiver"), ]

# Maisons à Niort en automne seulement
seg_maison_niort_aut <- train[
  train$Type.local == "Maison" &
    train$Commune    == "NIORT"  &
    train$saison     == "automne", ]

# Maisons à Niort : été, hiver, printemps
seg_maison_niort_rest <- train[
  train$Type.local == "Maison" &
    train$Commune    == "NIORT"  &
    train$saison %in% c("ete", "hiver", "printemps"), ]

# Maisons hors Niort (toutes saisons)
seg_maison_autres <- train[
  train$Type.local == "Maison" &
    train$Commune    != "NIORT", ]


# Calcul des coefficients pour chaque segment
coef_appart_pe         <- calcul_ab(seg_appart_pe$Surface.reelle.bati,
                                     seg_appart_pe$Valeur.fonciere)

coef_appart_ah         <- calcul_ab(seg_appart_ah$Surface.reelle.bati,
                                     seg_appart_ah$Valeur.fonciere)

coef_maison_niort_aut  <- calcul_ab(seg_maison_niort_aut$Surface.reelle.bati,
                                     seg_maison_niort_aut$Valeur.fonciere)

coef_maison_niort_rest <- calcul_ab(seg_maison_niort_rest$Surface.reelle.bati,
                                     seg_maison_niort_rest$Valeur.fonciere)

coef_maison_autres     <- calcul_ab(seg_maison_autres$Surface.reelle.bati,
                                     seg_maison_autres$Valeur.fonciere)

# Calcul et affichage des R²
r2_appart_pe <- calcul_r2(
  seg_appart_pe$Valeur.fonciere,
  appliquer_modele(coef_appart_pe, seg_appart_pe$Surface.reelle.bati))

r2_appart_ah <- calcul_r2(
  seg_appart_ah$Valeur.fonciere,
  appliquer_modele(coef_appart_ah, seg_appart_ah$Surface.reelle.bati))

r2_maison_niort_aut <- calcul_r2(
  seg_maison_niort_aut$Valeur.fonciere,
  appliquer_modele(coef_maison_niort_aut, seg_maison_niort_aut$Surface.reelle.bati))

r2_maison_niort_rest <- calcul_r2(
  seg_maison_niort_rest$Valeur.fonciere,
  appliquer_modele(coef_maison_niort_rest, seg_maison_niort_rest$Surface.reelle.bati))

r2_maison_autres <- calcul_r2(
  seg_maison_autres$Valeur.fonciere,
  appliquer_modele(coef_maison_autres, seg_maison_autres$Surface.reelle.bati))

cat("--- R2 par segment ---\n")
cat(sprintf("Appartements printemps/ete      : R2 = %.2f  (n = %d)\n",
            r2_appart_pe,         nrow(seg_appart_pe)))
cat(sprintf("Appartements automne/hiver      : R2 = %.2f  (n = %d)\n",
            r2_appart_ah,         nrow(seg_appart_ah)))
cat(sprintf("Maisons Niort automne           : R2 = %.2f  (n = %d)\n",
            r2_maison_niort_aut,  nrow(seg_maison_niort_aut)))
cat(sprintf("Maisons Niort ete/hiver/print.  : R2 = %.2f  (n = %d)\n",
            r2_maison_niort_rest, nrow(seg_maison_niort_rest)))
cat(sprintf("Maisons hors Niort              : R2 = %.2f  (n = %d)\n",
            r2_maison_autres,     nrow(seg_maison_autres)))

#######################################################
# Graphiques : nuage de points + droite de régression #
#######################################################

# Affiche les points observés, la droite, les résidus et les stats SCR/SCT/R²
tracer_graphique <- function(x, y, coefs, r2_val, titre) {

  y_pred <- coefs["a"] * x + coefs["b"]
  scr    <- sum((y - y_pred)^2)
  sct    <- sum((y - mean(y))^2)

  par(mar = c(5, 5, 4, 2))

  # Nuage de points observés (rouge)
  plot(x, y,
       col  = rgb(0.85, 0.2, 0.2, 0.4),
       pch  = 16,
       cex  = 0.6,
       xlab = "Surface reelle batie (m2)",
       ylab = "Valeur fonciere (euros)",
       main = titre,
       xlim = range(x),
       ylim = range(c(y, y_pred)))

  # Résidus : barres verticales
  segments(x, y, x, y_pred,
           col = rgb(0.85, 0.2, 0.2, 0.3),
           lwd = 0.8)

  # Droite de régression (bleue)
  x_seq <- seq(min(x), max(x), length.out = 300)
  lines(x_seq, coefs["a"] * x_seq + coefs["b"],
        col = "#2255AA",
        lwd = 2.5)

  # Points prédits (bleu)
  points(x, y_pred,
         col = "#2255AA",
         pch = 16,
         cex = 0.4)

  # Légende
  legend("topright",
         legend = c("Valeurs observees", "Valeurs predites", "Residus"),
         col    = c(rgb(0.85, 0.2, 0.2, 0.7), "#2255AA", rgb(0.85, 0.2, 0.2, 0.5)),
         pch    = c(16, 16, NA),
         lty    = c(NA, 1, 1),
         lwd    = c(NA, 2.5, 0.8),
         cex    = 0.8,
         bty    = "n")

  # Stats en haut
  mtext(sprintf("SCR = %.0f", scr),  side = 3, adj = 0, line = 2, cex = 0.75)
  mtext(sprintf("SCT = %.0f", sct),  side = 3, adj = 0, line = 1, cex = 0.75)
  mtext(sprintf("Coefficient de determination : R2 = %.2f", r2_val),
        side = 3, adj = 0, line = 0, cex = 0.75, font = 2)
}

par(mfrow = c(1, 1))

tracer_graphique(seg_appart_pe$Surface.reelle.bati,
                 seg_appart_pe$Valeur.fonciere,
                 coef_appart_pe, r2_appart_pe,
                 "Appartements - Printemps / Ete")

tracer_graphique(seg_appart_ah$Surface.reelle.bati,
                 seg_appart_ah$Valeur.fonciere,
                 coef_appart_ah, r2_appart_ah,
                 "Appartements - Automne / Hiver")

tracer_graphique(seg_maison_niort_aut$Surface.reelle.bati,
                 seg_maison_niort_aut$Valeur.fonciere,
                 coef_maison_niort_aut, r2_maison_niort_aut,
                 "Maisons a Niort - Automne")

tracer_graphique(seg_maison_niort_rest$Surface.reelle.bati,
                 seg_maison_niort_rest$Valeur.fonciere,
                 coef_maison_niort_rest, r2_maison_niort_rest,
                 "Maisons a Niort - Ete / Hiver / Printemps")

tracer_graphique(seg_maison_autres$Surface.reelle.bati,
                 seg_maison_autres$Valeur.fonciere,
                 coef_maison_autres, r2_maison_autres,
                 "Maisons hors Niort - Toutes saisons")

##################################
# Prédictions sur to_predict.csv # 
##################################

to_predict$Valeur.fonciere <- NA

# Appartements printemps + été
idx <- to_predict$Type.local == "Appartement" &
       to_predict$saison %in% c("printemps", "ete")
to_predict$Valeur.fonciere[idx] <- appliquer_modele(
  coef_appart_pe, to_predict$Surface.reelle.bati[idx])

# Appartements automne + hiver
idx <- to_predict$Type.local == "Appartement" &
       to_predict$saison %in% c("automne", "hiver")
to_predict$Valeur.fonciere[idx] <- appliquer_modele(
  coef_appart_ah, to_predict$Surface.reelle.bati[idx])

# Maisons Niort automne
idx <- to_predict$Type.local == "Maison" &
       to_predict$Commune    == "NIORT"  &
       to_predict$saison     == "automne"
to_predict$Valeur.fonciere[idx] <- appliquer_modele(
  coef_maison_niort_aut, to_predict$Surface.reelle.bati[idx])

# Maisons Niort été + hiver + printemps
idx <- to_predict$Type.local == "Maison" &
       to_predict$Commune    == "NIORT"  &
       to_predict$saison %in% c("ete", "hiver", "printemps")
to_predict$Valeur.fonciere[idx] <- appliquer_modele(
  coef_maison_niort_rest, to_predict$Surface.reelle.bati[idx])

# Maisons hors Niort
idx <- to_predict$Type.local == "Maison" &
       to_predict$Commune    != "NIORT"
to_predict$Valeur.fonciere[idx] <- appliquer_modele(
  coef_maison_autres, to_predict$Surface.reelle.bati[idx])

#########################
# Export prediction.csv #
#########################

prediction <- data.frame(
  id              = to_predict$id,
  Valeur.fonciere = to_predict$Valeur.fonciere
)

write.csv2(prediction, file = "prediction.csv", row.names = FALSE)

cat("\nFichier prediction.csv exporte avec", nrow(prediction), "lignes.\n")