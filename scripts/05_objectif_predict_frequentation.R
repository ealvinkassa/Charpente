################################################################################
#                                                                              #
#                OBJECTIF 1 : PRÉDICTION DE LA FRÉQUENTATION                   #
#                   Premier Semestre 2026 (Janvier - Juin)                     #
#                         VERSION FINALE CORRIGÉE                              #
#                                                                              #
################################################################################

# =============================================================================
# PARTIE 1 : CONFIGURATION ET CHARGEMENT DES PACKAGES
# =============================================================================

# if (!require("pacman")) install.packages("pacman")
# 
# pacman::p_load(
#   tidyverse, lubridate, data.table,
#   ggplot2, plotly, patchwork, scales,
#   forecast, prophet, tsibble,
#   caret, randomForest, xgboost,
#   zoo, gridExtra, purrr
# )
# 
# # Gestion des conflits de namespace
# library(conflicted)
# conflict_prefer("month", "lubridate")
# conflict_prefer("year", "lubridate")
# conflict_prefer("week", "lubridate")
# conflict_prefer("wday", "lubridate")
# conflict_prefer("yday", "lubridate")
# conflict_prefer("mday", "lubridate")
# conflict_prefer("quarter", "lubridate")
# conflict_prefer("day", "lubridate")
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")


# Configuration
options(scipen = 999, digits = 4)
set.seed(2025)

theme_set(theme_minimal(base_size = 12) +
            theme(
              plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5),
              legend.position = "bottom"
            ))

# cat("✓ Packages chargés avec succès\n\n")

# =============================================================================
# PARTIE 2 : CHARGEMENT DES DONNÉES
# =============================================================================

cat("📂 Chargement des données...\n")

# REMPLACEZ PAR VOS FICHIERS RÉELS

data_frequentation <- readRDS("data/processed/data_frequentation.rds")

cat("✓ Données chargées\n")
cat(paste("  - data_frequentation :", nrow(data_frequentation), "visites\n\n"))

# =============================================================================
# PARTIE 3 : AGRÉGATION PAR JOUR
# =============================================================================

cat("📊 Agrégation des données par jour...\n")

data_daily <- data_frequentation %>%
  mutate(visit_date = as.Date(visit_date)) %>%
  group_by(visit_date) %>%
  summarise(
    nb_visiteurs = n_distinct(phone),
    nb_visites = n(),
    pct_femmes = mean(sex == "Feminin", na.rm = TRUE) * 100,
    age_moyen = mean(age, na.rm = TRUE),
    duree_moyenne_minutes = mean(duration_minutes, na.rm = TRUE),
    duree_mediane_minutes = median(duration_minutes, na.rm = TRUE),
    nb_espaces_actifs = n_distinct(visited_space),
    nb_nouveaux_usagers = sum(seniority_days == 0, na.rm = TRUE),
    heure_arrivee_moyenne = mean(as.numeric(arrival_time) / 3600, na.rm = TRUE),
    nb_arrivees_matin = sum(as.numeric(arrival_time) < 12*3600, na.rm = TRUE),
    nb_arrivees_aprem = sum(as.numeric(arrival_time) >= 12*3600, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(visit_date)

# Compléter les dates manquantes
date_range <- seq(min(data_daily$visit_date), max(data_daily$visit_date), by = "day")
data_daily <- data_daily %>%
  complete(visit_date = date_range) %>%
  mutate(
    nb_visiteurs = replace_na(nb_visiteurs, 0),
    nb_visites = replace_na(nb_visites, 0),
    nb_espaces_actifs = replace_na(nb_espaces_actifs, 0),
    nb_nouveaux_usagers = replace_na(nb_nouveaux_usagers, 0)
  )

cat("✓ Agrégation terminée\n\n")

# =============================================================================
# PARTIE 4 : FEATURE ENGINEERING - VARIABLES TEMPORELLES
# =============================================================================

cat("🔧 Création des features temporelles...\n")

data_daily <- data_daily %>%
  mutate(
    annee = lubridate::year(visit_date),
    mois = lubridate::month(visit_date),
    mois_label = lubridate::month(visit_date, label = TRUE, abbr = FALSE),
    jour_mois = lubridate::day(visit_date),
    jour_annee = lubridate::yday(visit_date),
    semaine = lubridate::week(visit_date),
    trimestre = lubridate::quarter(visit_date),
    jour_semaine = lubridate::wday(visit_date, label = TRUE, abbr = FALSE),
    jour_semaine_num = lubridate::wday(visit_date),
    est_weekend = jour_semaine_num %in% c(1, 7),
    est_lundi = jour_semaine_num == 2,
    est_vendredi = jour_semaine_num == 6,
    est_debut_mois = jour_mois <= 5,
    est_fin_mois = jour_mois >= 25,
    est_milieu_mois = jour_mois >= 10 & jour_mois <= 20,
    mois_sin = sin(2 * pi * mois / 12),
    mois_cos = cos(2 * pi * mois / 12),
    jour_semaine_sin = sin(2 * pi * jour_semaine_num / 7),
    jour_semaine_cos = cos(2 * pi * jour_semaine_num / 7),
    jour_annee_sin = sin(2 * pi * jour_annee / 365),
    jour_annee_cos = cos(2 * pi * jour_annee / 365),
    tendance = as.numeric(visit_date - min(visit_date))
  )

# Jours fériés du Bénin
jours_feries_2025 <- as.Date(c(
  "2025-01-01", "2025-01-10", "2025-04-21", "2025-05-01",
  "2025-05-29", "2025-06-09", "2025-08-01", "2025-08-15",
  "2025-10-26", "2025-11-01", "2025-11-30", "2025-12-25"
))

calcul_jours_depuis_ferie <- function(date, feries) {
  feries_passes <- feries[feries < date]
  if (length(feries_passes) > 0) {
    return(as.numeric(date - max(feries_passes)))
  } else {
    return(NA_real_)
  }
}

calcul_jours_avant_ferie <- function(date, feries) {
  feries_futurs <- feries[feries > date]
  if (length(feries_futurs) > 0) {
    return(as.numeric(min(feries_futurs) - date))
  } else {
    return(NA_real_)
  }
}

data_daily <- data_daily %>%
  mutate(
    est_jour_ferie = visit_date %in% jours_feries_2025,
    jours_depuis_ferie = purrr::map_dbl(visit_date, calcul_jours_depuis_ferie, feries = jours_feries_2025),
    jours_avant_ferie = purrr::map_dbl(visit_date, calcul_jours_avant_ferie, feries = jours_feries_2025)
  )

# Vacances scolaires
periodes_vacances <- data.frame(
  debut = as.Date(c("2025-07-01", "2025-12-20")),
  fin = as.Date(c("2025-08-31", "2026-01-05"))
)

data_daily <- data_daily %>% mutate(est_vacances = FALSE)
for (i in 1:nrow(periodes_vacances)) {
  data_daily <- data_daily %>%
    mutate(est_vacances = ifelse(visit_date >= periodes_vacances$debut[i] & 
                                   visit_date <= periodes_vacances$fin[i], TRUE, est_vacances))
}

cat("✓ Features temporelles créées\n\n")

# =============================================================================
# PARTIE 5 : LAGS ET MOYENNES MOBILES
# =============================================================================

cat("🔧 Création des lags et moyennes mobiles...\n")

data_daily <- data_daily %>%
  arrange(visit_date) %>%
  mutate(
    nb_visiteurs_lag1 = dplyr::lag(nb_visiteurs, 1),
    nb_visiteurs_lag7 = dplyr::lag(nb_visiteurs, 7),
    nb_visiteurs_lag14 = dplyr::lag(nb_visiteurs, 14),
    nb_visiteurs_lag30 = dplyr::lag(nb_visiteurs, 30),
    nb_visiteurs_ma7 = zoo::rollmean(nb_visiteurs, k = 7, fill = NA, align = "right"),
    nb_visiteurs_ma14 = zoo::rollmean(nb_visiteurs, k = 14, fill = NA, align = "right"),
    nb_visiteurs_ma30 = zoo::rollmean(nb_visiteurs, k = 30, fill = NA, align = "right"),
    nb_visiteurs_sd7 = zoo::rollapply(nb_visiteurs, width = 7, FUN = sd, fill = NA, align = "right", partial = TRUE),
    croissance_vs_lag7 = (nb_visiteurs - nb_visiteurs_lag7) / (nb_visiteurs_lag7 + 1) * 100,
    ecart_vs_ma7 = nb_visiteurs - nb_visiteurs_ma7
  )

cat("✓ Lags créés\n\n")

# =============================================================================
# PARTIE 6 : VISUALISATION EXPLORATOIRE
# =============================================================================

cat("📊 Visualisations...\n")

p45 <- ggplot(data_daily, aes(x = visit_date, y = nb_visiteurs)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Évolution de la Fréquentation", x = "Date", y = "Visiteurs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/45_evolution_de_la_frequentation.png", p45, width = 15, height = 6, dpi = 300)


p46 <- ggplot(data_daily %>% filter(nb_visiteurs > 0), 
             aes(x = jour_semaine, y = nb_visiteurs, fill = jour_semaine)) +
  geom_boxplot() +
  labs(title = "Fréquentation par Jour", x = "", y = "Visiteurs") +
  theme(legend.position = "none")

ggsave("outputs/figures/46_frequentation_par_jour.png", p46, width = 15, height = 6, dpi = 300)


print(p45)
print(p46)

cat("✓ Visualisations créées\n\n")


# =============================================================================
# PARTIE 7 : PRÉPARATION MODÉLISATION
# =============================================================================

cat("🎯 Préparation modélisation...\n")

data_model <- data_daily %>%
  filter(!is.na(nb_visiteurs_lag7)) %>%
  filter(!is.na(nb_visiteurs_ma7))

cat(paste("  - Moyenne:", round(mean(data_model$nb_visiteurs), 2), "visiteurs/jour\n"))

# Division train/test
cutoff_date <- data_model$visit_date[floor(nrow(data_model) * 0.8)]
train_data <- data_model %>% filter(visit_date <= cutoff_date)
test_data <- data_model %>% filter(visit_date > cutoff_date)

cat(paste("  - Train:", nrow(train_data), "jours\n"))
cat(paste("  - Test:", nrow(test_data), "jours\n\n"))


# =============================================================================
# PARTIE 8 : MODÈLE ARIMA
# =============================================================================

cat("🤖 MODÈLE 1 : ARIMA\n")
cat("════════════════════════════════════════════════════════════\n")

ts_data <- ts(train_data$nb_visiteurs, frequency = 7)
arima_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, trace = FALSE)

cat("✓ ARIMA entraîné\n")

arima_pred <- forecast(arima_model, h = nrow(test_data))
test_data$pred_arima <- as.numeric(arima_pred$mean)

rmse_arima <- sqrt(mean((test_data$nb_visiteurs - test_data$pred_arima)^2))
mae_arima <- mean(abs(test_data$nb_visiteurs - test_data$pred_arima))
mape_arima <- mean(abs((test_data$nb_visiteurs - test_data$pred_arima) / (test_data$nb_visiteurs + 1))) * 100

cat(paste("  - RMSE:", round(rmse_arima, 2), "\n"))
cat(paste("  - MAE:", round(mae_arima, 2), "\n"))
cat(paste("  - MAPE:", round(mape_arima, 2), "%\n\n"))

# =============================================================================
# PARTIE 9 : MODÈLE PROPHET
# =============================================================================

cat("🤖 MODÈLE 2 : PROPHET\n")
cat("════════════════════════════════════════════════════════════\n")

prophet_train <- train_data %>% select(ds = visit_date, y = nb_visiteurs)

jours_feries_benin <- data.frame(
  holiday = c("Nouvel An", "Vodoun", "Pâques", "Travail", "Ascension", "Pentecôte", 
              "Nationale", "Assomption", "Armées", "Toussaint", "Nationale2", "Noël"),
  ds = as.Date(c("2025-01-01", "2025-01-10", "2025-04-21", "2025-05-01",
                 "2025-05-29", "2025-06-09", "2025-08-01", "2025-08-15",
                 "2025-10-26", "2025-11-01", "2025-11-30", "2025-12-25")),
  lower_window = 0,
  upper_window = 1
)

prophet_model <- prophet(
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "multiplicative",
  holidays = jours_feries_benin
)

prophet_model <- fit.prophet(prophet_model, prophet_train)
cat("✓ Prophet entraîné\n")

future_prophet <- make_future_dataframe(prophet_model, periods = nrow(test_data))
prophet_forecast <- predict(prophet_model, future_prophet)
test_data$pred_prophet <- prophet_forecast %>% tail(nrow(test_data)) %>% pull(yhat)

rmse_prophet <- sqrt(mean((test_data$nb_visiteurs - test_data$pred_prophet)^2))
mae_prophet <- mean(abs(test_data$nb_visiteurs - test_data$pred_prophet))
mape_prophet <- mean(abs((test_data$nb_visiteurs - test_data$pred_prophet) / (test_data$nb_visiteurs + 1))) * 100

cat(paste("  - RMSE:", round(rmse_prophet, 2), "\n"))
cat(paste("  - MAE:", round(mae_prophet, 2), "\n"))
cat(paste("  - MAPE:", round(mape_prophet, 2), "%\n\n"))

# =============================================================================
# PARTIE 10 : MODÈLE RANDOM FOREST
# =============================================================================

cat("🤖 MODÈLE 3 : RANDOM FOREST\n")
cat("════════════════════════════════════════════════════════════\n")

features_rf <- c(
  "jour_semaine_num", "mois", "jour_mois", "trimestre",
  "est_weekend", "est_debut_mois", "est_fin_mois", "tendance",
  "nb_visiteurs_lag1", "nb_visiteurs_lag7", "nb_visiteurs_lag14",
  "nb_visiteurs_ma7", "nb_visiteurs_ma14", "nb_visiteurs_ma30",
  "nb_visiteurs_sd7", "croissance_vs_lag7", "ecart_vs_ma7"
)

train_rf <- train_data %>% select(all_of(c("nb_visiteurs", features_rf)))
test_rf <- test_data %>% select(all_of(c("nb_visiteurs", features_rf)))

# ⭐ CORRECTION : Imputation synchronisée train ET test
for (col in features_rf) {
  # Calculer médiane sur TRAIN uniquement
  median_val <- median(train_rf[[col]], na.rm = TRUE)
  
  # Si médiane est NA, utiliser 0
  if (is.na(median_val)) median_val <- 0
  
  # Remplacer NA dans train ET test avec la même valeur
  train_rf[[col]][is.na(train_rf[[col]])] <- median_val
  test_rf[[col]][is.na(test_rf[[col]])] <- median_val
}

# Vérifications de sécurité
cat(paste("✓ Train:", nrow(train_rf), "lignes,", ncol(train_rf), "colonnes, NA:", sum(is.na(train_rf)), "\n"))
cat(paste("✓ Test:", nrow(test_rf), "lignes,", ncol(test_rf), "colonnes, NA:", sum(is.na(test_rf)), "\n"))

# Vérifier que les colonnes sont identiques
if (!identical(names(train_rf), names(test_rf))) {
  cat("⚠️  ATTENTION : Colonnes différentes entre train et test !\n")
  cat("   Train:", paste(names(train_rf), collapse = ", "), "\n")
  cat("   Test:", paste(names(test_rf), collapse = ", "), "\n")
  stop("Les datasets train et test doivent avoir les mêmes colonnes")
}

# Vérifier que nb_visiteurs est bien présent
if (!"nb_visiteurs" %in% names(train_rf)) {
  stop("Colonne 'nb_visiteurs' manquante dans train_rf")
}

cat("✓ Colonnes identiques entre train et test\n")

rf_model <- randomForest(
  nb_visiteurs ~ .,
  data = train_rf,
  ntree = 500,
  mtry = floor(sqrt(length(features_rf))),
  importance = TRUE
)

cat("✓ Random Forest entraîné\n")

test_rf$pred_rf <- predict(rf_model, newdata = test_rf)

rmse_rf <- sqrt(mean((test_rf$nb_visiteurs - test_rf$pred_rf)^2))
mae_rf <- mean(abs(test_rf$nb_visiteurs - test_rf$pred_rf))
mape_rf <- mean(abs((test_rf$nb_visiteurs - test_rf$pred_rf) / (test_rf$nb_visiteurs + 1))) * 100

cat(paste("  - RMSE:", round(rmse_rf, 2), "\n"))
cat(paste("  - MAE:", round(mae_rf, 2), "\n"))
cat(paste("  - MAPE:", round(mape_rf, 2), "%\n\n"))

# =============================================================================
# PARTIE 11 : MODÈLE XGBOOST
# =============================================================================

cat("🤖 MODÈLE 4 : XGBOOST\n")
cat("════════════════════════════════════════════════════════════\n")

# ⭐ VÉRIFICATION FINALE avant création des matrices
cat("\n🔍 VÉRIFICATION DES DONNÉES:\n")
cat(paste("   Train dimensions:", paste(dim(train_rf), collapse = " x "), "\n"))
cat(paste("   Test dimensions:", paste(dim(test_rf), collapse = " x "), "\n"))
cat(paste("   Train NA:", sum(is.na(train_rf)), "\n"))
cat(paste("   Test NA:", sum(is.na(test_rf)), "\n"))
cat(paste("   Colonnes train:", ncol(train_rf), "\n"))
cat(paste("   Colonnes test:", ncol(test_rf), "\n"))

# Afficher les noms des colonnes pour debug
cat("\n   Colonnes train:", paste(names(train_rf), collapse = ", "), "\n")
cat("   Colonnes test:", paste(names(test_rf), collapse = ", "), "\n\n")

# S'assurer que les colonnes sont dans le même ordre
test_rf <- test_rf[, names(train_rf)]

# ⭐ CRÉATION DES MATRICES avec vérification
train_features <- train_rf %>% select(-nb_visiteurs)
test_features <- test_rf %>% select(-nb_visiteurs)

cat(paste("✓ Features train:", ncol(train_features), "colonnes\n"))
cat(paste("✓ Features test:", ncol(test_features), "colonnes\n"))

# Vérifier qu'il n'y a AUCUN NA avant de créer les matrices
if (any(is.na(train_features))) {
  cat("⚠️  Remplacement des NA restants dans train par 0\n")
  train_features[is.na(train_features)] <- 0
}

if (any(is.na(test_features))) {
  cat("⚠️  Remplacement des NA restants dans test par 0\n")
  test_features[is.na(test_features)] <- 0
}

train_matrix <- xgb.DMatrix(
  data = as.matrix(train_features),
  label = train_rf$nb_visiteurs
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(test_features),
  label = test_rf$nb_visiteurs
)

cat(paste("✓ Matrix train:", dim(train_features)[1], "lignes,", dim(train_features)[2], "features\n"))
cat(paste("✓ Matrix test:", dim(test_features)[1], "lignes,", dim(test_features)[2], "features\n\n"))

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# ⭐ ENTRAÎNEMENT avec gestion d'erreur
cat("🔄 Entraînement XGBoost...\n")

tryCatch({
  xgb_model <- xgb.train(
    params = params,
    data = train_matrix,
    nrounds = 1000,
    evals = list(train = train_matrix, test = test_matrix),
    early_stopping_rounds = 50,
    verbose = 0
  )
  
  cat("✓ XGBoost entraîné avec succès\n")
  
}, error = function(e) {
  cat("❌ ERREUR lors de l'entraînement XGBoost:\n")
  cat(paste("   Message:", e$message, "\n"))
  cat("\n🔍 DIAGNOSTIC:\n")
  cat(paste("   Train matrix - lignes:", nrow(as.matrix(train_features)), 
            "colonnes:", ncol(as.matrix(train_features)), "\n"))
  cat(paste("   Test matrix - lignes:", nrow(as.matrix(test_features)), 
            "colonnes:", ncol(as.matrix(test_features)), "\n"))
  stop("Impossible de continuer - vérifiez les données")
})

test_rf$pred_xgb <- predict(xgb_model, test_matrix)

rmse_xgb <- sqrt(mean((test_rf$nb_visiteurs - test_rf$pred_xgb)^2))
mae_xgb <- mean(abs(test_rf$nb_visiteurs - test_rf$pred_xgb))
mape_xgb <- mean(abs((test_rf$nb_visiteurs - test_rf$pred_xgb) / (test_rf$nb_visiteurs + 1))) * 100

cat(paste("  - RMSE:", round(rmse_xgb, 2), "\n"))
cat(paste("  - MAE:", round(mae_xgb, 2), "\n"))
cat(paste("  - MAPE:", round(mape_xgb, 2), "%\n\n"))

# =============================================================================
# PARTIE 12 : COMPARAISON DES MODÈLES
# =============================================================================

cat("📊 COMPARAISON DES MODÈLES\n")
cat("════════════════════════════════════════════════════════════\n")

resultats <- data.frame(
  Modele = c("ARIMA", "Prophet", "Random Forest", "XGBoost"),
  RMSE = c(rmse_arima, rmse_prophet, rmse_rf, rmse_xgb),
  MAE = c(mae_arima, mae_prophet, mae_rf, mae_xgb),
  MAPE = c(mape_arima, mape_prophet, mape_rf, mape_xgb)
) %>% arrange(RMSE)

print(resultats)

meilleur_modele <- resultats$Modele[1]
cat(paste("\n🏆 MEILLEUR MODÈLE:", meilleur_modele, "\n\n"))

# =============================================================================
# PARTIE 13 : PRÉDICTIONS S1 2026 (VERSION CORRIGÉE)
# =============================================================================

cat("🔮 PRÉDICTIONS POUR LE S1 2026\n")
cat(paste("   Utilisation du modèle:", meilleur_modele, "\n"))
cat("════════════════════════════════════════════════════════════\n")

dates_futures <- seq(as.Date("2026-01-01"), as.Date("2026-06-30"), by = "day")

data_futur <- data.frame(visit_date = dates_futures) %>%
  mutate(
    annee = lubridate::year(visit_date),
    mois = lubridate::month(visit_date),
    mois_label = lubridate::month(visit_date, label = TRUE),
    jour_mois = lubridate::day(visit_date),
    jour_annee = lubridate::yday(visit_date),
    semaine = lubridate::week(visit_date),
    trimestre = lubridate::quarter(visit_date),
    jour_semaine = lubridate::wday(visit_date, label = TRUE),
    jour_semaine_num = lubridate::wday(visit_date),
    est_weekend = jour_semaine_num %in% c(1, 7),
    est_debut_mois = jour_mois <= 5,
    est_fin_mois = jour_mois >= 25,
    tendance = as.numeric(visit_date - min(data_daily$visit_date))
  )

# Prédiction selon le meilleur modèle
predictions_futures <- numeric(nrow(data_futur))
historique_complet <- data_model$nb_visiteurs

if (meilleur_modele == "ARIMA") {
  
  cat("📊 Utilisation de ARIMA pour les prédictions...\n")
  arima_forecast_futur <- forecast(arima_model, h = nrow(data_futur))
  predictions_futures <- as.numeric(arima_forecast_futur$mean)
  
} else if (meilleur_modele == "Prophet") {
  
  cat("📊 Utilisation de Prophet pour les prédictions...\n")
  future_prophet <- data_futur %>% select(ds = visit_date)
  prophet_forecast_futur <- predict(prophet_model, future_prophet)
  predictions_futures <- prophet_forecast_futur$yhat
  
} else if (meilleur_modele == "Random Forest") {
  
  cat("📊 Utilisation de Random Forest pour les prédictions...\n")
  
  for (i in 1:nrow(data_futur)) {
    nb_visiteurs_lag1 <- tail(historique_complet, 1)
    nb_visiteurs_lag7 <- tail(historique_complet, 7)[1]
    nb_visiteurs_lag14 <- tail(historique_complet, 14)[1]
    nb_visiteurs_ma7 <- mean(tail(historique_complet, 7))
    nb_visiteurs_ma14 <- mean(tail(historique_complet, 14))
    nb_visiteurs_ma30 <- mean(tail(historique_complet, min(30, length(historique_complet))))
    nb_visiteurs_sd7 <- sd(tail(historique_complet, 7))
    croissance_vs_lag7 <- (nb_visiteurs_lag1 - nb_visiteurs_lag7) / (nb_visiteurs_lag7 + 1) * 100
    ecart_vs_ma7 <- nb_visiteurs_lag1 - nb_visiteurs_ma7
    
    features_pred <- data.frame(
      jour_semaine_num = data_futur$jour_semaine_num[i],
      mois = data_futur$mois[i],
      jour_mois = data_futur$jour_mois[i],
      trimestre = data_futur$trimestre[i],
      est_weekend = data_futur$est_weekend[i],
      est_debut_mois = data_futur$est_debut_mois[i],
      est_fin_mois = data_futur$est_fin_mois[i],
      tendance = data_futur$tendance[i],
      nb_visiteurs_lag1 = nb_visiteurs_lag1,
      nb_visiteurs_lag7 = nb_visiteurs_lag7,
      nb_visiteurs_lag14 = nb_visiteurs_lag14,
      nb_visiteurs_ma7 = nb_visiteurs_ma7,
      nb_visiteurs_ma14 = nb_visiteurs_ma14,
      nb_visiteurs_ma30 = nb_visiteurs_ma30,
      nb_visiteurs_sd7 = nb_visiteurs_sd7,
      croissance_vs_lag7 = croissance_vs_lag7,
      ecart_vs_ma7 = ecart_vs_ma7
    )
    
    pred <- predict(rf_model, newdata = features_pred)
    pred <- max(0, pred)
    
    predictions_futures[i] <- pred
    historique_complet <- c(historique_complet, pred)
  }
  
} else if (meilleur_modele == "XGBoost") {
  
  cat("📊 Utilisation de XGBoost pour les prédictions...\n")
  
  for (i in 1:nrow(data_futur)) {
    nb_visiteurs_lag1 <- tail(historique_complet, 1)
    nb_visiteurs_lag7 <- tail(historique_complet, 7)[1]
    nb_visiteurs_lag14 <- tail(historique_complet, 14)[1]
    nb_visiteurs_ma7 <- mean(tail(historique_complet, 7))
    nb_visiteurs_ma14 <- mean(tail(historique_complet, 14))
    nb_visiteurs_ma30 <- mean(tail(historique_complet, min(30, length(historique_complet))))
    nb_visiteurs_sd7 <- sd(tail(historique_complet, 7))
    croissance_vs_lag7 <- (nb_visiteurs_lag1 - nb_visiteurs_lag7) / (nb_visiteurs_lag7 + 1) * 100
    ecart_vs_ma7 <- nb_visiteurs_lag1 - nb_visiteurs_ma7
    
    features_pred <- data.frame(
      jour_semaine_num = data_futur$jour_semaine_num[i],
      mois = data_futur$mois[i],
      jour_mois = data_futur$jour_mois[i],
      trimestre = data_futur$trimestre[i],
      est_weekend = data_futur$est_weekend[i],
      est_debut_mois = data_futur$est_debut_mois[i],
      est_fin_mois = data_futur$est_fin_mois[i],
      tendance = data_futur$tendance[i],
      nb_visiteurs_lag1 = nb_visiteurs_lag1,
      nb_visiteurs_lag7 = nb_visiteurs_lag7,
      nb_visiteurs_lag14 = nb_visiteurs_lag14,
      nb_visiteurs_ma7 = nb_visiteurs_ma7,
      nb_visiteurs_ma14 = nb_visiteurs_ma14,
      nb_visiteurs_ma30 = nb_visiteurs_ma30,
      nb_visiteurs_sd7 = nb_visiteurs_sd7,
      croissance_vs_lag7 = croissance_vs_lag7,
      ecart_vs_ma7 = ecart_vs_ma7
    )
    
    pred_matrix <- xgb.DMatrix(data = as.matrix(features_pred))
    pred <- predict(xgb_model, pred_matrix)
    pred <- max(0, pred)
    
    predictions_futures[i] <- pred
    historique_complet <- c(historique_complet, pred)
  }
}

data_futur$nb_visiteurs_predit <- round(predictions_futures)
data_futur <- data_futur %>%
  mutate(
    prediction_lower = pmax(0, nb_visiteurs_predit - 1.96 * min(resultats$RMSE)),
    prediction_upper = nb_visiteurs_predit + 1.96 * min(resultats$RMSE)
  )

cat("✓ Prédictions générées\n\n")



# Statistiques
cat("📊 STATISTIQUES PRÉDICTIONS:\n")
cat(paste("  - Moyenne:", round(mean(data_futur$nb_visiteurs_predit), 2), "visiteurs/jour\n"))
cat(paste("  - Total:", sum(data_futur$nb_visiteurs_predit), "visiteurs sur 6 mois\n\n"))

# Par mois
predictions_mois <- data_futur %>%
  group_by(mois_label) %>%
  summarise(
    total_visiteurs = sum(nb_visiteurs_predit),
    moyenne_jour = mean(nb_visiteurs_predit),
    .groups = "drop"
  )

cat("📅 PAR MOIS:\n")
print(predictions_mois)

# =============================================================================
# PARTIE 14 : VISUALISATIONS PRÉDICTIONS
# =============================================================================

cat("\n📊 Visualisations prédictions...\n")

historique_viz <- data_model %>%
  filter(visit_date >= max(visit_date) - 90) %>%
  select(visit_date, nb_visiteurs) %>%
  mutate(type = "Historique")

predictions_viz <- data_futur %>%
  select(visit_date, nb_visiteurs = nb_visiteurs_predit) %>%
  mutate(type = "Prédiction")

combined_data <- bind_rows(historique_viz, predictions_viz)

p_global <- ggplot() +
  geom_line(data = combined_data, aes(x = visit_date, y = nb_visiteurs, color = type), linewidth = 1) +
  geom_ribbon(data = data_futur, aes(x = visit_date, ymin = prediction_lower, ymax = prediction_upper),
              alpha = 0.2, fill = "blue") +
  geom_vline(xintercept = as.numeric(max(data_model$visit_date)), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Historique" = "steelblue", "Prédiction" = "darkgreen")) +
  labs(title = "Prédiction Fréquentation S1 2026", x = "Date", y = "Visiteurs", color = "") +
  theme(legend.position = "bottom")

ggsave("outputs/figures/47_prediction_frequentation_journaliere_S1_2026.png", p_global, width = 15, height = 6, dpi = 300)

p_mois <- ggplot(predictions_mois, aes(x = mois_label, y = total_visiteurs, fill = mois_label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_visiteurs), vjust = -0.5) +
  labs(title = "Prédictions Mensuelles S1 2026", x = "", y = "Total Visiteurs") +
  theme(legend.position = "none")

ggsave("outputs/figures/48_prediction_frequentation_mensuelle_S1_2026.png", p_mois, width = 15, height = 6, dpi = 300)

cat("✓ Visualisations créées\n\n")

# =============================================================================
# PARTIE 15 : EXPORT DES RÉSULTATS
# =============================================================================

cat("💾 Export des résultats...\n")

write.csv(data_model, "outputs/tables/data_frequentation_daily_engineered.csv", row.names = FALSE)
write.csv(data_futur, "outputs/tables/predictions_S1_2026.csv", row.names = FALSE)
saveRDS(xgb_model, "outputs/models/modele_xgboost.rds")
saveRDS(prophet_model, "outputs/models/modele_prophet.rds")
saveRDS(rf_model, "outputs/models/modele_randomforest.rds")

cat("✓ Fichiers sauvegardés\n\n")

# =============================================================================
# RAPPORT FINAL
# =============================================================================

cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                    RAPPORT DE PRÉDICTION FINAL                  ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n\n")

cat(paste("📅 Date:", format(Sys.Date(), "%d/%m/%Y"), "\n\n"))
cat("1. MEILLEUR MODÈLE:", meilleur_modele, "\n")
cat(paste("   RMSE:", round(min(resultats$RMSE), 2), "\n"))
cat(paste("   MAE:", round(min(resultats$MAE), 2), "\n\n"))

cat("2. DONNÉES HISTORIQUES\n")
cat(paste("   Période:", min(data_model$visit_date), "au", max(data_model$visit_date), "\n"))
cat(paste("   Nombre de jours:", nrow(data_model), "\n"))
cat(paste("   Moyenne:", round(mean(data_model$nb_visiteurs), 2), "visiteurs/jour\n\n"))

cat("3. PRÉDICTIONS S1 2026\n")
cat(paste("   Total prédit:", sum(data_futur$nb_visiteurs_predit), "visiteurs\n"))
cat(paste("   Moyenne:", round(mean(data_futur$nb_visiteurs_predit), 2), "visiteurs/jour\n"))
cat(paste("   Évolution:", round((mean(data_futur$nb_visiteurs_predit) / mean(data_model$nb_visiteurs) - 1) * 100, 1), "%\n\n"))

cat("4. MOIS LE PLUS FORT:", as.character(predictions_mois$mois_label[which.max(predictions_mois$total_visiteurs)]), "\n")
cat("   Total:", max(predictions_mois$total_visiteurs), "visiteurs\n\n")

cat("5. MOIS LE PLUS FAIBLE:", as.character(predictions_mois$mois_label[which.min(predictions_mois$total_visiteurs)]), "\n")
cat("   Total:", min(predictions_mois$total_visiteurs), "visiteurs\n\n")

cat("6. FICHIERS GÉNÉRÉS:\n")
cat("   ✓ data_frequentation_daily_engineered.csv\n")
cat("   ✓ predictions_S1_2026.csv\n")
cat("   ✓ modele_xgboost.rds\n")
cat("   ✓ modele_prophet.rds\n")
cat("   ✓ modele_randomforest.rds\n\n")

cat("7. RECOMMANDATIONS STRATÉGIQUES:\n")
cat("   📈 OPTIMISATION TEMPORELLE\n")
cat("      • Renforcer l'offre pendant les pics prédits\n")
cat("      • Promotions ciblées pendant les creux\n")
cat("      • Adapter le personnel selon les prévisions\n\n")

cat("   💡 ALLOCATION DES RESSOURCES\n")
cat("      • Planifier maintenance pendant périodes creuses\n")
cat("      • Anticiper besoins en personnel\n")
cat("      • Optimiser horaires d'ouverture\n\n")

cat("   🎯 ACTIONS MARKETING\n")
cat("      • Campagne fidélisation avant mois faibles\n")
cat("      • Événements spéciaux jours à faible affluence\n")
cat("      • Communication anticipée pour pics\n\n")

cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                         ✅ OBJECTIF 1 TERMINÉ !                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n\n")

cat("📌 PROCHAINES ÉTAPES:\n")
cat("   → Objectif 2 : Identifier les facteurs d'influence\n")
cat("   → Objectif 3 : Prédire le comportement des usagers\n")
cat("   → Objectif 4 : Optimiser l'allocation des ressources\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("                    🎉 SCRIPT TERMINÉ AVEC SUCCÈS ! 🎉           \n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")


# =============================================================================
# FONCTION RÉUTILISABLE : PRÉDIRE N'IMPORTE QUELLE PÉRIODE FUTURE
# =============================================================================

predire_frequentation <- function(date_debut, 
                                  date_fin, 
                                  modele_type = meilleur_modele,  # ✅ Utilise le meilleur par défaut
                                  modele_objet = NULL,             # Pour spécifier un modèle manuellement
                                  historique = data_model$nb_visiteurs,
                                  afficher_progression = TRUE) {
  
  # Validation des dates
  date_debut <- as.Date(date_debut)
  date_fin <- as.Date(date_fin)
  
  if (date_fin < date_debut) {
    stop("❌ Erreur : date_fin doit être postérieure à date_debut")
  }
  
  dates <- seq(date_debut, date_fin, by = "day")
  nb_jours <- length(dates)
  
  if (afficher_progression) {
    cat("🔮 PRÉDICTION EN COURS\n")
    cat(paste("   Période:", date_debut, "au", date_fin, "\n"))
    cat(paste("   Nombre de jours:", nb_jours, "\n"))
    cat(paste("   Modèle utilisé:", modele_type, "\n\n"))
  }
  
  predictions <- numeric(nb_jours)
  
  # ════════════════════════════════════════════════════════════════════════
  # MÉTHODE 1 : ARIMA
  # ════════════════════════════════════════════════════════════════════════
  if (modele_type == "ARIMA") {
    
    if (is.null(modele_objet)) {
      if (!exists("arima_model")) {
        stop("❌ Modèle ARIMA non trouvé. Entraînez d'abord le modèle.")
      }
      modele_objet <- arima_model
    }
    
    arima_forecast <- forecast(modele_objet, h = nb_jours)
    predictions <- as.numeric(arima_forecast$mean)
    predictions <- pmax(0, predictions)  # Pas de valeurs négatives
    
  }
  
  # ════════════════════════════════════════════════════════════════════════
  # MÉTHODE 2 : PROPHET
  # ════════════════════════════════════════════════════════════════════════
  else if (modele_type == "Prophet") {
    
    if (is.null(modele_objet)) {
      if (!exists("prophet_model")) {
        stop("❌ Modèle Prophet non trouvé. Entraînez d'abord le modèle.")
      }
      modele_objet <- prophet_model
    }
    
    future_df <- data.frame(ds = dates)
    prophet_forecast <- predict(modele_objet, future_df)
    predictions <- prophet_forecast$yhat
    predictions <- pmax(0, predictions)  # Pas de valeurs négatives
    
  }
  
  # ════════════════════════════════════════════════════════════════════════
  # MÉTHODE 3 : RANDOM FOREST (prédiction itérative)
  # ════════════════════════════════════════════════════════════════════════
  else if (modele_type == "Random Forest") {
    
    if (is.null(modele_objet)) {
      if (!exists("rf_model")) {
        stop("❌ Modèle Random Forest non trouvé. Entraînez d'abord le modèle.")
      }
      modele_objet <- rf_model
    }
    
    hist_complet <- historique
    
    for (i in 1:nb_jours) {
      date_actuelle <- dates[i]
      
      # Calculer les features
      features <- data.frame(
        jour_semaine_num = lubridate::wday(date_actuelle),
        mois = lubridate::month(date_actuelle),
        jour_mois = lubridate::day(date_actuelle),
        trimestre = lubridate::quarter(date_actuelle),
        est_weekend = lubridate::wday(date_actuelle) %in% c(1, 7),
        est_debut_mois = lubridate::day(date_actuelle) <= 5,
        est_fin_mois = lubridate::day(date_actuelle) >= 25,
        tendance = as.numeric(date_actuelle - min(data_daily$visit_date)),
        nb_visiteurs_lag1 = tail(hist_complet, 1),
        nb_visiteurs_lag7 = tail(hist_complet, 7)[1],
        nb_visiteurs_lag14 = tail(hist_complet, 14)[1],
        nb_visiteurs_ma7 = mean(tail(hist_complet, 7)),
        nb_visiteurs_ma14 = mean(tail(hist_complet, 14)),
        nb_visiteurs_ma30 = mean(tail(hist_complet, min(30, length(hist_complet)))),
        nb_visiteurs_sd7 = sd(tail(hist_complet, 7)),
        croissance_vs_lag7 = (tail(hist_complet, 1) - tail(hist_complet, 7)[1]) / 
          (tail(hist_complet, 7)[1] + 1) * 100,
        ecart_vs_ma7 = tail(hist_complet, 1) - mean(tail(hist_complet, 7))
      )
      
      pred <- predict(modele_objet, newdata = features)
      pred <- max(0, pred)
      
      predictions[i] <- pred
      hist_complet <- c(hist_complet, pred)
      
      # Afficher progression tous les 30 jours
      if (afficher_progression && i %% 30 == 0) {
        cat(paste("   Progression:", i, "/", nb_jours, "jours traités\n"))
      }
    }
    
  }
  
  # ════════════════════════════════════════════════════════════════════════
  # MÉTHODE 4 : XGBOOST (prédiction itérative)
  # ════════════════════════════════════════════════════════════════════════
  else if (modele_type == "XGBoost") {
    
    if (is.null(modele_objet)) {
      if (!exists("xgb_model")) {
        stop("❌ Modèle XGBoost non trouvé. Entraînez d'abord le modèle.")
      }
      modele_objet <- xgb_model
    }
    
    hist_complet <- historique
    
    for (i in 1:nb_jours) {
      date_actuelle <- dates[i]
      
      # Calculer les features
      features <- data.frame(
        jour_semaine_num = lubridate::wday(date_actuelle),
        mois = lubridate::month(date_actuelle),
        jour_mois = lubridate::day(date_actuelle),
        trimestre = lubridate::quarter(date_actuelle),
        est_weekend = lubridate::wday(date_actuelle) %in% c(1, 7),
        est_debut_mois = lubridate::day(date_actuelle) <= 5,
        est_fin_mois = lubridate::day(date_actuelle) >= 25,
        tendance = as.numeric(date_actuelle - min(data_daily$visit_date)),
        nb_visiteurs_lag1 = tail(hist_complet, 1),
        nb_visiteurs_lag7 = tail(hist_complet, 7)[1],
        nb_visiteurs_lag14 = tail(hist_complet, 14)[1],
        nb_visiteurs_ma7 = mean(tail(hist_complet, 7)),
        nb_visiteurs_ma14 = mean(tail(hist_complet, 14)),
        nb_visiteurs_ma30 = mean(tail(hist_complet, min(30, length(hist_complet)))),
        nb_visiteurs_sd7 = sd(tail(hist_complet, 7)),
        croissance_vs_lag7 = (tail(hist_complet, 1) - tail(hist_complet, 7)[1]) / 
          (tail(hist_complet, 7)[1] + 1) * 100,
        ecart_vs_ma7 = tail(hist_complet, 1) - mean(tail(hist_complet, 7))
      )
      
      pred_matrix <- xgb.DMatrix(data = as.matrix(features))
      pred <- predict(modele_objet, pred_matrix)
      pred <- max(0, pred)
      
      predictions[i] <- pred
      hist_complet <- c(hist_complet, pred)
      
      # Afficher progression tous les 30 jours
      if (afficher_progression && i %% 30 == 0) {
        cat(paste("   Progression:", i, "/", nb_jours, "jours traités\n"))
      }
    }
    
  } else {
    stop(paste("❌ Modèle inconnu:", modele_type))
  }
  
  # ════════════════════════════════════════════════════════════════════════
  # RÉSULTATS
  # ════════════════════════════════════════════════════════════════════════
  
  resultats <- data.frame(
    date = dates,
    nb_visiteurs_predit = round(predictions),
    jour_semaine = lubridate::wday(dates, label = TRUE),
    mois = lubridate::month(dates, label = TRUE)
  )
  
  if (afficher_progression) {
    cat("✓ Prédictions terminées\n")
    cat(paste("   Total prédit:", sum(resultats$nb_visiteurs_predit), "visiteurs\n"))
    cat(paste("   Moyenne/jour:", round(mean(resultats$nb_visiteurs_predit), 2), "visiteurs\n\n"))
  }
  
  return(resultats)
}

# =============================================================================
# EXEMPLES D'UTILISATION
# =============================================================================

# Exemple 1 : Utiliser le meilleur modèle automatiquement (par défaut)
# predictions_janvier <- predire_frequentation("2026-01-01", "2026-01-31")

# Exemple 2 : Forcer l'utilisation d'un modèle spécifique
# predictions_prophet <- predire_frequentation("2026-01-01", "2026-01-31", 
#                                               modele_type = "Prophet")

# Exemple 3 : Prédire les 30 prochains jours
# predictions_30j <- predire_frequentation(Sys.Date() + 1, Sys.Date() + 30)

# Exemple 4 : Sans affichage de progression
# predictions_silent <- predire_frequentation("2026-02-01", "2026-02-28", 
#                                             afficher_progression = FALSE)

cat("💡 FONCTION predire_frequentation() prête à l'emploi\n")
cat("   Utilise automatiquement le meilleur modèle :", meilleur_modele, "\n\n")

cat("💡 BONUS : Fonction predire_frequentation() disponible pour de nouvelles dates\n")
cat("   Exemple : predire_frequentation('2026-07-01', '2026-07-31')\n\n")

# =============================================================================
# INFORMATIONS DE SESSION
# =============================================================================

cat("📋 INFORMATIONS DE SESSION:\n")
cat(paste("   R version:", R.version.string, "\n"))
cat(paste("   Système:", Sys.info()["sysname"], Sys.info()["release"], "\n"))
cat(paste("   Date exécution:", Sys.time(), "\n\n"))

cat("📦 VERSIONS DES PACKAGES PRINCIPAUX:\n")
cat(paste("   tidyverse:", packageVersion("tidyverse"), "\n"))
cat(paste("   lubridate:", packageVersion("lubridate"), "\n"))
cat(paste("   forecast:", packageVersion("forecast"), "\n"))
cat(paste("   prophet:", packageVersion("prophet"), "\n"))
cat(paste("   randomForest:", packageVersion("randomForest"), "\n"))
cat(paste("   xgboost:", packageVersion("xgboost"), "\n\n"))


# =============================================================================
# FONCTION D'ÉVALUATION CONTINUE
# =============================================================================

# Fonction pour évaluer les prédictions vs réalité
evaluer_predictions <- function(predictions_df, realisations_df) {
  
  merged <- predictions_df %>%
    inner_join(realisations_df, by = "date", suffix = c("_pred", "_reel"))
  
  rmse <- sqrt(mean((merged$nb_visiteurs_reel - merged$nb_visiteurs_pred)^2))
  mae <- mean(abs(merged$nb_visiteurs_reel - merged$nb_visiteurs_pred))
  mape <- mean(abs((merged$nb_visiteurs_reel - merged$nb_visiteurs_pred) / 
                     (merged$nb_visiteurs_reel + 1))) * 100
  
  erreur_moyenne <- mean(merged$nb_visiteurs_pred - merged$nb_visiteurs_reel)
  
  cat("📊 ÉVALUATION DES PRÉDICTIONS:\n")
  cat(paste("   RMSE:", round(rmse, 2), "\n"))
  cat(paste("   MAE:", round(mae, 2), "\n"))
  cat(paste("   MAPE:", round(mape, 2), "%\n"))
  cat(paste("   Biais:", round(erreur_moyenne, 2), 
            ifelse(erreur_moyenne > 0, "(surestimation)", "(sous-estimation)"), "\n\n"))
  
  # Graphique comparatif
  p <- ggplot(merged, aes(x = date)) +
    geom_line(aes(y = nb_visiteurs_reel, color = "Réel"), linewidth = 1) +
    geom_line(aes(y = nb_visiteurs_pred, color = "Prédit"), linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("Réel" = "blue", "Prédit" = "red")) +
    labs(title = "Prédictions vs Réalité", x = "Date", y = "Nombre de Visiteurs", color = "") +
    theme_minimal()
  
  print(p)
  
  return(list(
    rmse = rmse,
    mae = mae,
    mape = mape,
    biais = erreur_moyenne,
    donnees = merged
  ))
}

# =============================================================================
# CHECKLIST FINALE
# =============================================================================

cat("✅ CHECKLIST FINALE:\n\n")

checklist <- data.frame(
  Etape = c(
    "Données chargées",
    "Features créées",
    "Modèles entraînés",
    "Prédictions générées",
    "Visualisations créées",
    "Fichiers exportés",
    "Rapport généré"
  ),
  Statut = rep("✓", 7)
)

print(checklist, row.names = FALSE)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("                         FIN DU SCRIPT                                  \n")
cat("═══════════════════════════════════════════════════════════════════════\n")
