################################################################################
#                                                                              #
#       OBJECTIF 3 : PRÉDICTION DU COMPORTEMENT DES USAGERS                    #
#         Identification des Futurs Fidèles et Risques de Churn                #
#                                                                              #
################################################################################

# =============================================================================
# PARTIE 1 : CONFIGURATION ET CHARGEMENT
# =============================================================================

cat("🎯 OBJECTIF 3 : PRÉDICTION DU COMPORTEMENT DES USAGERS\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# # Packages nécessaires
# if (!require("pacman")) install.packages("pacman")
# 
# # Installer survival séparément si besoin
# if(!require("survival")) install.packages("survival")
# if(!require("survminer")) install.packages("survminer")
# 
# pacman::p_load(
#   tidyverse, lubridate, data.table,
#   ggplot2, plotly, patchwork, scales,
#   caret, randomForest, xgboost, e1071,
#   pROC,           # Courbes ROC
#   # DMwR retiré - obsolète pour R 4.5
#   ggalluvial,     # Diagrammes de flux
#   gridExtra
# )
# 
# # Résoudre les conflits
# conflicts_prefer(caret::cluster)
# conflicts_prefer(caret::confusionMatrix)  # NOUVEAU
# conflicts_prefer(stats::filter)
# conflicts_prefer(dplyr::select)

options(scipen = 999, digits = 4)
set.seed(2025)

cat("✓ Configuration terminée\n")
cat("✓ Package DMwR ignoré (obsolète pour cette version de R)\n\n")


# =============================================================================
# PARTIE 2 : PRÉPARATION DES DONNÉES USAGERS
# =============================================================================

cat("📊 Préparation des données usagers...\n")

# Vérifier si data_usagers_comportement existe
if(!exists("data_usagers_comportement")) {
  stop("⚠️ data_usagers_comportement non trouvé. Veuillez charger ce dataset.")
}

# Enrichir le dataset usagers
data_usagers <- data_usagers_comportement %>%
  mutate(
    # Variables temporelles
    anciennete_semaines = anciennete_jours / 7,
    anciennete_mois = anciennete_jours / 30,
    
    # Fréquence de visite
    frequence_hebdo = nb_visites / pmax(anciennete_semaines, 1, na.rm = TRUE),
    frequence_mensuelle = nb_visites / pmax(anciennete_mois, 1, na.rm = TRUE),
    
    # Intensité d'utilisation
    intensite_globale = duree_totale_minutes / pmax(anciennete_jours, 1, na.rm = TRUE),
    
    # Diversité
    diversite_spatiale = nb_espaces_differents / pmax(nb_visites, 1, na.rm = TRUE),
    est_mono_espace = nb_espaces_differents == 1,
    
    # Régularité (coefficient de variation)
    cv_regularite = case_when(
      !is.na(regularite_jours) & nb_visites > 1 & anciennete_jours > 0 ~ 
        regularite_jours / (anciennete_jours / nb_visites),
      TRUE ~ NA_real_
    ),
    
    # Engagement weekend
    propension_weekend = pct_visites_weekend / 100,
    
    # Inactivité récente
    jours_depuis_derniere_visite = as.numeric(Sys.Date() - derniere_visite),
    est_inactif_30j = jours_depuis_derniere_visite > 30,
    est_inactif_60j = jours_depuis_derniere_visite > 60,
    
    # Vitesse d'adoption
    jours_entre_inscr_et_1ere_visite = as.numeric(premiere_visite - registration_date),
    adoption_rapide = !is.na(jours_entre_inscr_et_1ere_visite) & jours_entre_inscr_et_1ere_visite <= 7,
    
    # Catégorisation actuelle (base pour labels)
    categorie_actuelle = case_when(
      nb_visites == 1 ~ "Occasionnel",
      nb_visites >= 2 & nb_visites <= 5 ~ "Explorateur",
      nb_visites >= 6 & nb_visites <= 10 ~ "Régulier",
      nb_visites > 10 ~ "Fidèle",
      TRUE ~ NA_character_
    ),
    categorie_actuelle = factor(categorie_actuelle, 
                                levels = c("Occasionnel", "Explorateur", "Régulier", "Fidèle"))
  )

cat("✓ Dataset usagers enrichi\n")
cat(paste("  - Nombre d'usagers :", nrow(data_usagers), "\n"))
cat(paste("  - Features créées  :", ncol(data_usagers), "\n\n"))


# =============================================================================
# PARTIE 3 : DÉFINITION DES CIBLES DE PRÉDICTION
# =============================================================================

cat("🎯 Définition des cibles de prédiction...\n\n")

# CIBLE 1 : Potentiel de progression (Occasionnel → Fidèle)
data_usagers <- data_usagers %>%
  mutate(
    # Pour les occasionnels : critères pour identifier ceux qui VONT progresser
    # Critères positifs : adoption rapide, longue durée, pas inactif
    criteres_positifs = 
      ifelse(!is.na(adoption_rapide) & adoption_rapide == TRUE, 1, 0) + 
      ifelse(!is.na(duree_moyenne_visite) & duree_moyenne_visite > median(duree_moyenne_visite, na.rm = TRUE), 1, 0) + 
      ifelse(!is.na(est_inactif_30j) & est_inactif_30j == FALSE, 1, 0),
    
    potentiel_progression = case_when(
      categorie_actuelle == "Occasionnel" & criteres_positifs >= 2 ~ "Élevé",
      categorie_actuelle == "Occasionnel" & criteres_positifs == 1 ~ "Moyen",
      categorie_actuelle == "Occasionnel" & criteres_positifs == 0 ~ "Faible",
      categorie_actuelle == "Explorateur" ~ "Moyen",
      TRUE ~ NA_character_
    ),
    
    # Binaire pour modélisation (Élevé = 1, autres = 0)
    va_progresser = case_when(
      categorie_actuelle == "Occasionnel" & !is.na(potentiel_progression) & potentiel_progression == "Élevé" ~ 1,
      categorie_actuelle == "Occasionnel" & !is.na(potentiel_progression) & potentiel_progression != "Élevé" ~ 0,
      TRUE ~ NA_real_
    )
  )

# CIBLE 2 : Risque de churn (pour usagers actifs)
data_usagers <- data_usagers %>%
  mutate(
    # Calculer des signaux de churn (gestion des NA)
    signaux_churn = 
      ifelse(!is.na(est_inactif_60j) & est_inactif_60j == TRUE, 1, 0) + 
      ifelse(!is.na(frequence_hebdo) & frequence_hebdo < median(frequence_hebdo, na.rm = TRUE), 1, 0) +
      ifelse(!is.na(score_engagement) & score_engagement < 50, 1, 0),
    
    # Définir le churn
    risque_churn = case_when(
      nb_visites >= 3 & signaux_churn >= 2 ~ "Élevé",
      nb_visites >= 3 & signaux_churn == 1 ~ "Moyen",
      nb_visites >= 3 & signaux_churn == 0 ~ "Faible",
      TRUE ~ NA_character_
    ),
    
    # Binaire
    va_churner = case_when(
      nb_visites >= 3 & !is.na(risque_churn) & risque_churn == "Élevé" ~ 1,
      nb_visites >= 3 & !is.na(risque_churn) & risque_churn != "Élevé" ~ 0,
      TRUE ~ NA_real_
    )
  )

# CIBLE 3 : Potentiel multi-espaces (pour mono-espaces)
data_usagers <- data_usagers %>%
  mutate(
    # Signaux de potentiel de diversification (gestion des NA)
    signaux_diversif = 
      ifelse(!is.na(nb_visites) & nb_visites >= 3, 1, 0) + 
      ifelse(!is.na(duree_moyenne_visite) & duree_moyenne_visite > median(duree_moyenne_visite, na.rm = TRUE), 1, 0) +
      ifelse(!is.na(frequence_hebdo) & frequence_hebdo > median(frequence_hebdo, na.rm = TRUE), 1, 0),
    
    # Pour les mono-espaces : vont-ils explorer d'autres espaces ?
    potentiel_multi_espaces = case_when(
      est_mono_espace & signaux_diversif >= 2 ~ "Élevé",
      est_mono_espace & signaux_diversif == 1 ~ "Moyen",
      est_mono_espace & signaux_diversif == 0 ~ "Faible",
      TRUE ~ NA_character_
    ),
    
    # Binaire
    va_multi_espaces = case_when(
      est_mono_espace & !is.na(potentiel_multi_espaces) & potentiel_multi_espaces == "Élevé" ~ 1,
      est_mono_espace & !is.na(potentiel_multi_espaces) & potentiel_multi_espaces != "Élevé" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Statistiques des cibles
cat("📊 DISTRIBUTION DES CIBLES:\n\n")

cat("1️⃣ POTENTIEL DE PROGRESSION (Occasionnels uniquement):\n")
table_progression <- table(data_usagers$potentiel_progression, useNA = "ifany")
print(table_progression)
cat(paste("   Total occasionnels :", sum(!is.na(data_usagers$va_progresser)), "\n"))

# Vérification de la variable binaire
if("va_progresser" %in% names(data_usagers)) {
  cat("   Variable binaire va_progresser :\n")
  print(table(data_usagers$va_progresser, useNA = "ifany"))
} else {
  cat("   ⚠️ ERREUR : va_progresser n'a pas été créée !\n")
}
cat("\n")

cat("2️⃣ RISQUE DE CHURN (Usagers actifs 3+ visites):\n")
table_churn <- table(data_usagers$risque_churn, useNA = "ifany")
print(table_churn)
cat(paste("   Total actifs :", sum(!is.na(data_usagers$va_churner)), "\n"))

# Vérification
if("va_churner" %in% names(data_usagers)) {
  cat("   Variable binaire va_churner :\n")
  print(table(data_usagers$va_churner, useNA = "ifany"))
} else {
  cat("   ⚠️ ERREUR : va_churner n'a pas été créée !\n")
}
cat("\n")

cat("3️⃣ POTENTIEL MULTI-ESPACES (Mono-espaces uniquement):\n")
table_multi <- table(data_usagers$potentiel_multi_espaces, useNA = "ifany")
print(table_multi)
cat(paste("   Total mono-espaces :", sum(!is.na(data_usagers$va_multi_espaces)), "\n"))

# Vérification
if("va_multi_espaces" %in% names(data_usagers)) {
  cat("   Variable binaire va_multi_espaces :\n")
  print(table(data_usagers$va_multi_espaces, useNA = "ifany"))
} else {
  cat("   ⚠️ ERREUR : va_multi_espaces n'a pas été créée !\n")
}
cat("\n")


# =============================================================================
# PARTIE 4 : MODÈLE 1 - PRÉDICTION DE PROGRESSION (OCCASIONNELS → RÉGULIERS)
# =============================================================================

cat("🤖 MODÈLE 1 : PRÉDICTION DE PROGRESSION\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# Vérifier que va_progresser existe
if(!"va_progresser" %in% names(data_usagers)) {
  stop("⚠️ ERREUR : La variable 'va_progresser' n'existe pas dans data_usagers")
}

# Filtrer les occasionnels avec label - IMPORTANT : ne pas utiliser select() qui peut perdre la variable
data_progression <- data_usagers %>%
  dplyr::filter(!is.na(va_progresser))

# Vérifier que va_progresser est toujours là
if(!"va_progresser" %in% names(data_progression)) {
  stop("⚠️ ERREUR : va_progresser perdue après filter")
}

# Maintenant sélectionner les colonnes
data_progression <- data_progression %>%
  dplyr::select(
    va_progresser,  # IMPORTANT : d'abord la cible
    age, sex, city,
    duree_moyenne_visite, duree_totale_minutes,
    jours_depuis_derniere_visite,
    heure_arrivee_moyenne, nb_visites_weekend,
    nb_espaces_differents, anciennete_jours
  ) %>%
  mutate(
    va_progresser = factor(va_progresser, levels = c(0, 1), labels = c("Non", "Oui")),
    age = as.numeric(age),
    sex = as.factor(sex),
    city = as.factor(city)
  ) %>%
  na.omit()

cat(paste("\n📊 Dataset progression : ", nrow(data_progression), "occasionnels\n"))

# Vérifier qu'on a des données
if(nrow(data_progression) == 0) {
  cat("⚠️ ERREUR : Aucune donnée après filtrage\n")
  skip_model_prog <- TRUE
} else {
  cat("   Distribution :\n")
  print(table(data_progression$va_progresser))
  skip_model_prog <- FALSE
}

# Gérer le déséquilibre des classes si nécessaire
table_prog <- table(data_progression$va_progresser)
if(length(table_prog) == 2 && min(table_prog) / max(table_prog) < 0.3) {
  cat("\n⚠️ Classes déséquilibrées - Application de sur-échantillonnage...\n")
  
  # Méthode alternative : sur-échantillonner la classe minoritaire
  classe_min <- names(which.min(table_prog))
  classe_maj <- names(which.max(table_prog))
  
  data_min <- data_progression %>% dplyr::filter(va_progresser == classe_min)
  data_maj <- data_progression %>% dplyr::filter(va_progresser == classe_maj)
  
  # Sur-échantillonner jusqu'à 50% de la classe majoritaire
  n_samples <- min(nrow(data_maj) / 2, nrow(data_min) * 3)
  data_min_oversampled <- data_min[sample(nrow(data_min), n_samples, replace = TRUE), ]
  
  data_progression <- bind_rows(data_maj, data_min_oversampled)
  
  cat("   Nouvelle distribution :\n")
  print(table(data_progression$va_progresser))
} else if(length(table_prog) == 1) {
  cat("\n⚠️ Une seule classe présente - Modèle non entraînable\n")
  cat("   Passage à l'analyse descriptive uniquement\n\n")
  skip_model_prog <- TRUE
} else {
  skip_model_prog <- FALSE
}

# Split train/test
set.seed(2025)
split_idx_prog <- createDataPartition(data_progression$va_progresser, p = 0.75, list = FALSE)
train_prog <- data_progression[split_idx_prog, ]
test_prog <- data_progression[-split_idx_prog, ]

# Vérifier si on peut entraîner le modèle
if(!exists("skip_model_prog")) skip_model_prog <- FALSE

if(!skip_model_prog && length(table(train_prog$va_progresser)) == 2) {
  
  # Validation croisée
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  # Modèle Random Forest
  cat("\nEntraînement Random Forest...\n")
  model_prog_rf <- train(
    va_progresser ~ .,
    data = train_prog,
    method = "rf",
    trControl = ctrl,
    metric = "ROC",
    ntree = 300,
    importance = TRUE
  )
  
  # Prédictions
  pred_prog_rf <- predict(model_prog_rf, newdata = test_prog, type = "prob")
  pred_prog_class <- predict(model_prog_rf, newdata = test_prog)
  
  # Évaluation
  conf_matrix_prog <- confusionMatrix(pred_prog_class, test_prog$va_progresser, positive = "Oui")
  roc_prog <- roc(test_prog$va_progresser, pred_prog_rf$Oui)
  
  cat("\n📊 PERFORMANCE - PRÉDICTION DE PROGRESSION:\n")
  print(conf_matrix_prog)
  cat(paste("\n   AUC-ROC:", round(auc(roc_prog), 3), "\n"))
  
  # Courbe ROC
  plot(roc_prog, main = "Courbe ROC - Prédiction de Progression",
       col = "blue", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Importance des variables
  importance_prog <- varImp(model_prog_rf)
  
  # Vérifier la structure de l'objet importance
  if(is.list(importance_prog) && "importance" %in% names(importance_prog)) {
    importance_df <- importance_prog$importance %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Variable")
    
    # Trouver la colonne d'importance (peut varier selon le modèle)
    col_importance <- names(importance_df)[2]  # Généralement la 2ème colonne
    importance_df <- importance_df %>%
      arrange(desc(.data[[col_importance]]))
    
    cat("\n🔍 TOP 10 FACTEURS DE PROGRESSION:\n")
    print(head(importance_df, 10))
    
    plot(varImp(model_prog_rf), top = 10, main = "Facteurs de Progression")
  } else {
    cat("\n⚠️ Impossible d'extraire l'importance des variables\n")
  }
  
} else {
  cat("\n⚠️ Modèle de progression non entraînable (données insuffisantes)\n")
  cat("   Utilisation d'heuristiques simples à la place\n\n")
  model_prog_rf <- NULL
}


# =============================================================================
# PARTIE 5 : MODÈLE 2 - PRÉDICTION DE CHURN
# =============================================================================

cat("\n🤖 MODÈLE 2 : PRÉDICTION DE CHURN\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# Filtrer les usagers actifs avec label
data_churn <- data_usagers %>%
  dplyr::filter(!is.na(va_churner))

# Sélectionner les colonnes
data_churn <- data_churn %>%
  dplyr::select(
    va_churner,
    age, sex, city,
    nb_visites, anciennete_jours, anciennete_semaines,
    frequence_hebdo, frequence_mensuelle,
    duree_moyenne_visite, duree_totale_minutes,
    regularite_jours, cv_regularite,
    nb_espaces_differents, diversite_spatiale,
    pct_visites_weekend,
    heure_arrivee_moyenne,
    jours_depuis_derniere_visite,
    score_engagement
  ) %>%
  mutate(
    va_churner = factor(va_churner, levels = c(0, 1), labels = c("Non", "Oui")),
    age = as.numeric(age),
    sex = as.factor(sex),
    city = as.factor(city)
  ) %>%
  na.omit()

cat(paste("\n📊 Dataset churn : ", nrow(data_churn), "usagers actifs\n"))
cat("   Distribution :\n")
print(table(data_churn$va_churner))

# Gérer le déséquilibre
table_churn <- table(data_churn$va_churner)
if(length(table_churn) == 2 && min(table_churn) / max(table_churn) < 0.3) {
  cat("\n⚠️ Classes déséquilibrées - Application de sur-échantillonnage...\n")
  
  classe_min <- names(which.min(table_churn))
  classe_maj <- names(which.max(table_churn))
  
  data_min <- data_churn %>% dplyr::filter(va_churner == classe_min)
  data_maj <- data_churn %>% dplyr::filter(va_churner == classe_maj)
  
  n_samples <- min(nrow(data_maj) / 2, nrow(data_min) * 3)
  data_min_oversampled <- data_min[sample(nrow(data_min), n_samples, replace = TRUE), ]
  
  data_churn <- bind_rows(data_maj, data_min_oversampled)
  
  cat("   Nouvelle distribution :\n")
  print(table(data_churn$va_churner))
} else if(length(table_churn) == 1) {
  cat("\n⚠️ Une seule classe présente - Modèle non entraînable\n")
  skip_model_churn <- TRUE
} else {
  skip_model_churn <- FALSE
}

# Split train/test
split_idx_churn <- createDataPartition(data_churn$va_churner, p = 0.75, list = FALSE)
train_churn <- data_churn[split_idx_churn, ]
test_churn <- data_churn[-split_idx_churn, ]

if(!skip_model_churn && length(table(train_churn$va_churner)) == 2) {
  # Modèle Random Forest au lieu de XGBoost (plus stable)
  cat("\nEntraînement Random Forest...\n")
  model_churn_rf <- train(
    va_churner ~ .,
    data = train_churn,
    method = "rf",
    trControl = ctrl,
    metric = "ROC",
    ntree = 300,
    importance = TRUE
  )
  
  # Prédictions
  pred_churn_rf <- predict(model_churn_rf, newdata = test_churn, type = "prob")
  pred_churn_class <- predict(model_churn_rf, newdata = test_churn)
  
  # Évaluation
  conf_matrix_churn <- confusionMatrix(pred_churn_class, test_churn$va_churner, positive = "Oui")
  roc_churn <- roc(test_churn$va_churner, pred_churn_rf$Oui)
  
  cat("\n📊 PERFORMANCE - PRÉDICTION DE CHURN:\n")
  print(conf_matrix_churn)
  cat(paste("\n   AUC-ROC:", round(auc(roc_churn), 3), "\n"))
  
  # Courbe ROC
  plot(roc_churn, main = "Courbe ROC - Prédiction de Churn",
       col = "red", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Importance des variables
  importance_churn <- varImp(model_churn_rf)
  
  if(is.list(importance_churn) && "importance" %in% names(importance_churn)) {
    importance_df <- importance_churn$importance %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Variable")
    
    col_importance <- names(importance_df)[2]
    importance_df <- importance_df %>%
      arrange(desc(.data[[col_importance]]))
    
    cat("\n🔍 TOP 10 SIGNAUX D'ALERTE DE CHURN:\n")
    print(head(importance_df, 10))
    
    plot(varImp(model_churn_rf), top = 10, main = "Signaux de Churn")
  } else {
    cat("\n⚠️ Impossible d'extraire l'importance des variables\n")
  }
} else {
  cat("\n⚠️ Modèle de churn non entraînable\n")
  model_churn_rf <- NULL
}


# =============================================================================
# PARTIE 6 : MODÈLE 3 - PRÉDICTION MULTI-ESPACES
# =============================================================================

cat("\n🤖 MODÈLE 3 : PRÉDICTION POTENTIEL MULTI-ESPACES\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# Filtrer les mono-espaces
data_multi <- data_usagers %>%
  dplyr::filter(!is.na(va_multi_espaces))

# Sélectionner les colonnes
data_multi <- data_multi %>%
  dplyr::select(
    va_multi_espaces,
    age, sex, city,
    nb_visites, anciennete_jours,
    duree_moyenne_visite, duree_totale_minutes,
    frequence_hebdo,
    heure_arrivee_moyenne,
    pct_visites_weekend,
    jours_depuis_derniere_visite,
    adoption_rapide,
    score_engagement
  ) %>%
  mutate(
    va_multi_espaces = factor(va_multi_espaces, levels = c(0, 1), labels = c("Non", "Oui")),
    age = as.numeric(age),
    sex = as.factor(sex),
    city = as.factor(city),
    adoption_rapide = as.numeric(adoption_rapide)
  ) %>%
  na.omit()

cat(paste("\n📊 Dataset multi-espaces : ", nrow(data_multi), "mono-espaces\n"))

# Vérifier qu'on a des données
if(nrow(data_multi) == 0) {
  cat("⚠️ ERREUR : Aucune donnée après filtrage\n")
  skip_model_multi <- TRUE
} else {
  cat("   Distribution :\n")
  print(table(data_multi$va_multi_espaces))
  skip_model_multi <- FALSE  # Initialiser par défaut
}

# Gérer le déséquilibre
if(!skip_model_multi) {
  table_multi <- table(data_multi$va_multi_espaces)
  if(length(table_multi) == 2 && min(table_multi) / max(table_multi) < 0.3) {
    cat("\n⚠️ Classes déséquilibrées - Application de sur-échantillonnage...\n")
    
    # Utiliser dplyr explicitement
    classe_min <- names(which.min(table_multi))
    classe_maj <- names(which.max(table_multi))
    
    data_min <- data_multi %>% dplyr::filter(va_multi_espaces == classe_min)
    data_maj <- data_multi %>% dplyr::filter(va_multi_espaces == classe_maj)
    
    n_samples <- min(nrow(data_maj) / 2, nrow(data_min) * 3)
    data_min_oversampled <- data_min[sample(nrow(data_min), n_samples, replace = TRUE), ]
    
    data_multi <- bind_rows(data_maj, data_min_oversampled)
    
    cat("   Nouvelle distribution :\n")
    print(table(data_multi$va_multi_espaces))
  } else if(length(table_multi) == 1) {
    cat("\n⚠️ Une seule classe présente - Modèle non entraînable\n")
    skip_model_multi <- TRUE
  }
}

# Split train/test
split_idx_multi <- createDataPartition(data_multi$va_multi_espaces, p = 0.75, list = FALSE)
train_multi <- data_multi[split_idx_multi, ]
test_multi <- data_multi[-split_idx_multi, ]

if(!skip_model_multi && length(table(train_multi$va_multi_espaces)) == 2) {
  # Modèle Random Forest
  cat("\nEntraînement Random Forest...\n")
  model_multi_rf <- train(
    va_multi_espaces ~ .,
    data = train_multi,
    method = "rf",
    trControl = ctrl,
    metric = "ROC",
    ntree = 300,
    importance = TRUE
  )
  
  # Prédictions
  pred_multi_rf <- predict(model_multi_rf, newdata = test_multi, type = "prob")
  pred_multi_class <- predict(model_multi_rf, newdata = test_multi)
  
  # Évaluation
  conf_matrix_multi <- confusionMatrix(pred_multi_class, test_multi$va_multi_espaces, positive = "Oui")
  roc_multi <- roc(test_multi$va_multi_espaces, pred_multi_rf$Oui)
  
  cat("\n📊 PERFORMANCE - PRÉDICTION MULTI-ESPACES:\n")
  print(conf_matrix_multi)
  cat(paste("\n   AUC-ROC:", round(auc(roc_multi), 3), "\n"))
  
  # Courbe ROC
  plot(roc_multi, main = "Courbe ROC - Prédiction Multi-Espaces",
       col = "green", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Importance des variables
  importance_multi <- varImp(model_multi_rf)
  
  if(is.list(importance_multi) && "importance" %in% names(importance_multi)) {
    importance_df <- importance_multi$importance %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Variable")
    
    col_importance <- names(importance_df)[2]
    importance_df <- importance_df %>%
      arrange(desc(.data[[col_importance]]))
    
    cat("\n🔍 TOP 10 FACTEURS DE DIVERSIFICATION:\n")
    print(head(importance_df, 10))
    
    plot(varImp(model_multi_rf), top = 10, main = "Facteurs de Diversification")
  } else {
    cat("\n⚠️ Impossible d'extraire l'importance des variables\n")
  }
} else {
  cat("\n⚠️ Modèle multi-espaces non entraînable\n")
  model_multi_rf <- NULL
}


# =============================================================================
# PARTIE 7 : SCORING DE TOUS LES USAGERS
# =============================================================================

cat("\n📊 SCORING DE TOUS LES USAGERS\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# Créer une copie pour scoring
data_scoring <- data_usagers

# Score de progression (pour occasionnels uniquement)
if(!is.null(model_prog_rf)) {
  occasionnels_idx <- which(data_scoring$categorie_actuelle == "Occasionnel")
  if(length(occasionnels_idx) > 0) {
    data_occasionnels <- data_scoring[occasionnels_idx, ] %>%
      select(all_of(names(train_prog)[-1])) %>%
      na.omit()
    
    if(nrow(data_occasionnels) > 0) {
      pred_score_prog <- predict(model_prog_rf, newdata = data_occasionnels, type = "prob")
      data_scoring$score_progression[occasionnels_idx[1:nrow(data_occasionnels)]] <- pred_score_prog$Oui * 100
    }
  }
} else {
  # Heuristique simple si pas de modèle
  data_scoring <- data_scoring %>%
    mutate(score_progression = ifelse(
      categorie_actuelle == "Occasionnel" & adoption_rapide & duree_moyenne_visite > 120,
      80, 
      ifelse(categorie_actuelle == "Occasionnel", 40, NA)
    ))
}

# Score de churn (pour actifs uniquement)
if(!is.null(model_churn_rf)) {
  actifs_idx <- which(data_scoring$nb_visites >= 3)
  if(length(actifs_idx) > 0) {
    data_actifs <- data_scoring[actifs_idx, ] %>%
      select(all_of(names(train_churn)[-1])) %>%
      na.omit()
    
    if(nrow(data_actifs) > 0) {
      pred_score_churn <- predict(model_churn_rf, newdata = data_actifs, type = "prob")
      data_scoring$score_churn[actifs_idx[1:nrow(data_actifs)]] <- pred_score_churn$Oui * 100
    }
  }
} else {
  # Heuristique simple
  data_scoring <- data_scoring %>%
    mutate(score_churn = ifelse(
      nb_visites >= 3 & jours_depuis_derniere_visite > 60,
      90,
      ifelse(nb_visites >= 3 & jours_depuis_derniere_visite > 30, 60, 
             ifelse(nb_visites >= 3, 20, NA))
    ))
}

# Score multi-espaces (pour mono-espaces uniquement)
if(!is.null(model_multi_rf)) {
  mono_idx <- which(data_scoring$est_mono_espace)
  if(length(mono_idx) > 0) {
    data_mono <- data_scoring[mono_idx, ] %>%
      select(all_of(names(train_multi)[-1])) %>%
      na.omit()
    
    if(nrow(data_mono) > 0) {
      pred_score_multi <- predict(model_multi_rf, newdata = data_mono, type = "prob")
      data_scoring$score_multi_espaces[mono_idx[1:nrow(data_mono)]] <- pred_score_multi$Oui * 100
    }
  }
} else {
  # Heuristique simple
  data_scoring <- data_scoring %>%
    mutate(score_multi_espaces = ifelse(
      est_mono_espace & nb_visites >= 3 & duree_moyenne_visite > 150,
      75,
      ifelse(est_mono_espace & nb_visites >= 2, 40, 
             ifelse(est_mono_espace, 20, NA))
    ))
}

cat("\n✓ Scoring terminé\n")
cat(paste("  - Occasionnels scorés  :", sum(!is.na(data_scoring$score_progression)), "\n"))
cat(paste("  - Actifs scorés (churn):", sum(!is.na(data_scoring$score_churn)), "\n"))
cat(paste("  - Mono-espaces scorés  :", sum(!is.na(data_scoring$score_multi_espaces)), "\n\n"))


# =============================================================================
# PARTIE 8 : SEGMENTATION ET PRIORISATION DES ACTIONS
# =============================================================================

cat("🎯 SEGMENTATION ET PRIORISATION\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# Segment 1 : Occasionnels à fort potentiel
occasionnels_potentiel <- data_scoring %>%
  filter(categorie_actuelle == "Occasionnel", !is.na(score_progression)) %>%
  mutate(
    priorite = case_when(
      score_progression >= 70 ~ "Très Haute",
      score_progression >= 50 ~ "Haute",
      score_progression >= 30 ~ "Moyenne",
      TRUE ~ "Faible"
    )
  ) %>%
  arrange(desc(score_progression))

cat("\n1️⃣ OCCASIONNELS À FORT POTENTIEL:\n")
cat(paste("   Total                  :", nrow(occasionnels_potentiel), "\n"))
cat("   Répartition par priorité:\n")
print(table(occasionnels_potentiel$priorite))
cat(paste("\n   🎯 Cible prioritaire   :", 
          sum(occasionnels_potentiel$score_progression >= 70), 
          "usagers (score ≥70)\n"))

# Top 10
cat("\n   TOP 10 OCCASIONNELS À CONVERTIR:\n")
top10_prog <- occasionnels_potentiel %>%
  select(phone, first_name, last_name, score_progression, duree_moyenne_visite, 
         jours_depuis_derniere_visite) %>%
  head(10)
print(top10_prog)


# Segment 2 : Actifs à risque de churn
actifs_risque <- data_scoring %>%
  filter(nb_visites >= 3, !is.na(score_churn)) %>%
  mutate(
    niveau_risque = case_when(
      score_churn >= 70 ~ "Critique",
      score_churn >= 50 ~ "Élevé",
      score_churn >= 30 ~ "Modéré",
      TRUE ~ "Faible"
    )
  ) %>%
  arrange(desc(score_churn))

cat("\n\n2️⃣ ACTIFS À RISQUE DE CHURN:\n")
cat(paste("   Total                  :", nrow(actifs_risque), "\n"))
cat("   Répartition par niveau de risque:\n")
print(table(actifs_risque$niveau_risque))
cat(paste("\n   ⚠️ Risque critique     :", 
          sum(actifs_risque$score_churn >= 70), 
          "usagers (score ≥70)\n"))

# Top 10
cat("\n   TOP 10 USAGERS À RISQUE:\n")
top10_churn <- actifs_risque %>%
  select(phone, first_name, last_name, score_churn, nb_visites, 
         jours_depuis_derniere_visite, categorie_actuelle) %>%
  head(10)
print(top10_churn)


# Segment 3 : Mono-espaces à potentiel de diversification
mono_potentiel <- data_scoring %>%
  filter(est_mono_espace, !is.na(score_multi_espaces)) %>%
  mutate(
    potentiel_diversif = case_when(
      score_multi_espaces >= 70 ~ "Très Élevé",
      score_multi_espaces >= 50 ~ "Élevé",
      score_multi_espaces >= 30 ~ "Moyen",
      TRUE ~ "Faible"
    )
  ) %>%
  arrange(desc(score_multi_espaces))

cat("\n\n3️⃣ MONO-ESPACES À DIVERSIFIER:\n")
cat(paste("   Total                  :", nrow(mono_potentiel), "\n"))
cat("   Répartition par potentiel:\n")
print(table(mono_potentiel$potentiel_diversif))
cat(paste("\n   🎯 Potentiel élevé     :", 
          sum(mono_potentiel$score_multi_espaces >= 70), 
          "usagers (score ≥70)\n"))

# Top 10
cat("\n   TOP 10 USAGERS À DIVERSIFIER:\n")
top10_multi <- mono_potentiel %>%
  select(phone, first_name, last_name, score_multi_espaces, nb_visites, 
         espace_prefere, duree_moyenne_visite) %>%
  head(10)
print(top10_multi)


# =============================================================================
# PARTIE 9 : VISUALISATIONS DES SEGMENTS
# =============================================================================

cat("\n\n📊 VISUALISATIONS DES SEGMENTS\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# 1. Distribution des scores de progression
p1 <- ggplot(occasionnels_potentiel, aes(x = score_progression)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = c(30, 50, 70), linetype = "dashed", color = "red") +
  labs(
    title = "Distribution des Scores de Progression (Occasionnels)",
    x = "Score de Progression (%)",
    y = "Nombre d'Usagers"
  ) +
  theme_minimal()

print(p1)

# 2. Distribution des scores de churn
p2 <- ggplot(actifs_risque, aes(x = score_churn)) +
  geom_histogram(bins = 20, fill = "coral", alpha = 0.7) +
  geom_vline(xintercept = c(30, 50, 70), linetype = "dashed", color = "darkred") +
  labs(
    title = "Distribution des Scores de Churn (Actifs)",
    x = "Score de Churn (%)",
    y = "Nombre d'Usagers"
  ) +
  theme_minimal()

print(p2)

# 3. Distribution des scores multi-espaces
p3 <- ggplot(mono_potentiel, aes(x = score_multi_espaces)) +
  geom_histogram(bins = 20, fill = "lightgreen", alpha = 0.7) +
  geom_vline(xintercept = c(30, 50, 70), linetype = "dashed", color = "darkgreen") +
  labs(
    title = "Distribution des Scores Multi-Espaces (Mono-espaces)",
    x = "Score Potentiel Multi-Espaces (%)",
    y = "Nombre d'Usagers"
  ) +
  theme_minimal()

print(p3)

# 4. Matrice de segmentation (catégorie actuelle vs scores)
data_viz <- data_scoring %>%
  filter(!is.na(categorie_actuelle)) %>%
  mutate(
    score_global = case_when(
      !is.na(score_progression) ~ score_progression,
      !is.na(score_churn) ~ 100 - score_churn,  # Inverser le churn
      !is.na(score_multi_espaces) ~ score_multi_espaces,
      TRUE ~ 50
    )
  )

p4 <- ggplot(data_viz, aes(x = categorie_actuelle, y = score_global, 
                           fill = categorie_actuelle)) +
  geom_boxplot() +
  labs(
    title = "Scores par Catégorie d'Usagers",
    x = "Catégorie",
    y = "Score Comportemental"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p4)

# 5. Analyse du parcours (flux entre catégories)
# Simuler les transitions probables
transitions <- data.frame(
  from = c(rep("Occasionnel", 3), rep("Explorateur", 3), rep("Régulier", 2)),
  to = c("Churn", "Explorateur", "Reste Occasionnel",
         "Churn", "Régulier", "Reste Explorateur",
         "Fidèle", "Reste Régulier"),
  freq = c(
    sum(occasionnels_potentiel$priorite == "Faible"),
    sum(occasionnels_potentiel$priorite %in% c("Haute", "Très Haute")),
    sum(occasionnels_potentiel$priorite == "Moyenne"),
    nrow(actifs_risque %>% filter(categorie_actuelle == "Explorateur", niveau_risque %in% c("Critique", "Élevé"))),
    sum(data_scoring$categorie_actuelle == "Explorateur" & data_scoring$nb_visites >= 4) * 0.3,
    sum(data_scoring$categorie_actuelle == "Explorateur") * 0.6,
    sum(data_scoring$categorie_actuelle == "Régulier") * 0.4,
    sum(data_scoring$categorie_actuelle == "Régulier") * 0.6
  )
)

# Diagramme de flux (Sankey simplifié avec barres)
p5 <- ggplot(transitions, aes(x = from, y = freq, fill = to)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Trajectoires Prédites des Usagers",
    x = "Catégorie Actuelle",
    y = "Nombre d'Usagers",
    fill = "Trajectoire"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)


# =============================================================================
# PARTIE 10 : ANALYSE DE SURVIE (DURÉE AVANT CHURN)
# =============================================================================

cat("\n📈 ANALYSE DE SURVIE\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# Préparer les données de survie
data_survie <- data_usagers %>%
  filter(nb_visites >= 2) %>%
  mutate(
    temps_survie = anciennete_jours,
    event = ifelse(est_inactif_60j, 1, 0)  # 1 = churné, 0 = censuré (actif)
  )

head(data_survie)


# Diagnostic des catégories

# Diagnostic complet
cat("=== DIAGNOSTIC DES CATÉGORIES ===\n\n")

# 1. Catégories dans les données filtrées
cat("1. Catégories dans data_survie (nb_visites >= 2):\n")
print(table(data_survie$categorie_actuelle))

# 2. Catégories dans le modèle
cat("\n2. Strates du modèle Kaplan-Meier:\n")
print(names(km_fit$strata))

# 3. Nombre de niveaux
cat("\n3. Nombre de niveaux du facteur:\n")
print(nlevels(data_survie$categorie_actuelle))

# 4. Tous les niveaux (même vides)
cat("\n4. Tous les niveaux définis:\n")
print(levels(data_survie$categorie_actuelle))

# Modèle de survie de Kaplan-Meier
km_fit <- survfit(Surv(temps_survie, event) ~ categorie_actuelle, 
                  data = data_survie)

cat("\nModèle de Kaplan-Meier estimé\n")
print(summary(km_fit))


# Temps médian de survie
cat("\n⏱️ TEMPS MÉDIAN DE SURVIE (50% encore actifs):\n")
medians <- summary(km_fit)$table[, "median"]
for(i in 1:length(medians)) {
  cat(paste("  ", names(medians)[i], ":", round(medians[i]), "jours\n"))
}


# ======================================================================
# ANALYSE DE SURVIE COMPLÈTE
# ======================================================================

# 1. Préparation des données
data_survie_complete <- data_usagers %>%
  mutate(
    temps_survie = anciennete_jours,
    event = ifelse(est_inactif_60j, 1, 0),
    # S'assurer que categorie_actuelle est un facteur ordonné
    categorie_actuelle = factor(categorie_actuelle, 
                                levels = c("Occasionnel", "Explorateur", 
                                           "Régulier", "Fidèle"),
                                ordered = TRUE)
  )

# 2. Statistiques descriptives
cat("=== STATISTIQUES DESCRIPTIVES ===\n")
cat("\nDistribution des catégories:\n")
print(table(data_survie_complete$categorie_actuelle))

cat("\nTaux de churn par catégorie:\n")
print(data_survie_complete %>%
        group_by(categorie_actuelle) %>%
        summarise(
          n = n(),
          n_churn = sum(event),
          taux_churn = round(mean(event) * 100, 1),
          anciennete_med = round(median(temps_survie), 1)
        ))

# 3. Modèle de Kaplan-Meier
km_fit <- survfit(Surv(temps_survie, event) ~ categorie_actuelle, 
                  data = data_survie_complete)

# 4. Temps médian de survie
cat("\n=== TEMPS MÉDIAN DE SURVIE ===\n")
medians <- surv_median(km_fit)
print(medians)

# 5. Visualisation principale
p1 <- ggsurvplot(
  km_fit,
  data = data_survie_complete,
  
  # Tests statistiques
  pval = TRUE,
  pval.method = TRUE,
  log.rank.weights = "1",  # Test log-rank standard
  
  # Intervalles de confiance
  conf.int = TRUE,
  conf.int.alpha = 0.1,  # Transparence
  
  # Table de risque
  risk.table = TRUE,
  risk.table.height = 0.3,
  risk.table.title = "Nombre d'usagers à risque",
  tables.theme = theme_cleantable(),
  
  # Titre et axes
  title = "Courbes de Survie par Catégorie d'Usagers du SCOP",
  subtitle = "Probabilité de rester actif au fil du temps",
  xlab = "Jours depuis l'inscription",
  ylab = "Probabilité de rester actif (%)",
  
  # Légende
  legend.title = "Catégorie d'usager",
  legend.labs = c("Occasionnel (1 visite)", 
                  "Explorateur (2-5 visites)", 
                  "Régulier (6-10 visites)", 
                  "Fidèle (11+ visites)"),
  legend = "right",
  
  # Couleurs cohérentes avec votre rapport
  palette = c("#95a5a6", "#3498db", "#f39c12", "#e74c3c"),
  
  # Lignes médianes
  surv.median.line = "hv",
  
  # Axes
  break.time.by = 30,
  xlim = c(0, max(data_survie_complete$temps_survie, na.rm = TRUE)),
  ylim = c(0, 1),
  
  # Style
  ggtheme = theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      legend.position = "right"
    ),
  
  # Annotations
  font.main = c(14, "bold"),
  font.x = c(12, "plain"),
  font.y = c(12, "plain"),
  font.legend = c(10, "plain")
)

# 6. Affichage
print(p1)

# 7. Test de log-rank (comparaison formelle)
cat("\n=== TEST DE LOG-RANK ===\n")
surv_diff <- survdiff(Surv(temps_survie, event) ~ categorie_actuelle,
                      data = data_survie_complete)
print(surv_diff)

# 8. Interprétation automatique
cat("\n=== INTERPRÉTATION ===\n")
if(surv_diff$pvalue < 0.001) {
  cat("✓ Les courbes de survie diffèrent TRÈS significativement (p < 0.001)\n")
} else if(surv_diff$pvalue < 0.05) {
  cat("✓ Les courbes de survie diffèrent significativement (p < 0.05)\n")
} else {
  cat("✗ Pas de différence significative entre les catégories\n")
}

cat("\nTemps médian de survie:\n")
for(i in 1:nrow(medians)) {
  cat(sprintf("  %s: %d jours (IC 95%%: %d-%d)\n",
              medians$strata[i], 
              medians$median[i],
              medians$lower[i],
              medians$upper[i]))
}

# 9. Sauvegarde (optionnel)
ggsave("outputs/figures/courbe_survie_scop.png", plot = p1$plot, 
        width = 12, height = 8, dpi = 300, bg = "white")



# =============================================================================
# PARTIE 11 : PLANS D'ACTION PERSONNALISÉS
# =============================================================================

cat("\n\n📋 PLANS D'ACTION PERSONNALISÉS\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# Action 1 : Onboarding renforcé pour occasionnels à potentiel
action1_cible <- occasionnels_potentiel %>%
  filter(priorite %in% c("Très Haute", "Haute"))

cat("\n🎯 ACTION 1 : ONBOARDING RENFORCÉ\n")
cat(paste("   Cible                  :", nrow(action1_cible), "occasionnels\n"))
cat("   Critères               : Score progression ≥50\n")
cat("   Actions recommandées   :\n")
cat("      • Email J+2 : Guide découverte des espaces\n")
cat("      • SMS J+7 : Invitation événement adapté\n")
cat("      • Offre spéciale 2ème visite (réduction 50%)\n")
cat("      • Appel personnalisé J+14 si pas de retour\n\n")


# Action 2 : Réactivation des actifs à risque
action2_cible <- actifs_risque %>%
  filter(niveau_risque %in% c("Critique", "Élevé"))

cat("⚠️ ACTION 2 : RÉACTIVATION URGENTE\n")
cat(paste("   Cible                  :", nrow(action2_cible), "actifs à risque\n"))
cat("   Critères               : Score churn ≥50\n")
cat("   Actions recommandées   :\n")
cat("      • Email immédiat : \"Vous nous manquez\"\n")
cat("      • Offre exclusive retour (1 mois gratuit)\n")
cat("      • Enquête de satisfaction (pourquoi l'absence ?)\n")
cat("      • Invitation événement VIP personnalisé\n\n")


# Action 3 : Parcours découverte pour mono-espaces
action3_cible <- mono_potentiel %>%
  filter(potentiel_diversif %in% c("Très Élevé", "Élevé"))

cat("🗺️ ACTION 3 : PARCOURS DÉCOUVERTE\n")
cat(paste("   Cible                  :", nrow(action3_cible), "mono-espaces\n"))
cat("   Critères               : Score multi-espaces ≥50\n")
cat("   Actions recommandées   :\n")
cat("      • Visite guidée gratuite des autres espaces\n")
cat("      • Challenge \"Explorateur\" (visiter 3 espaces = cadeau)\n")
cat("      • Recommandations personnalisées d'espaces\n")
cat("      • Session découverte en groupe\n\n")


# Action 4 : Programme de fidélisation pour explorateurs
action4_cible <- data_scoring %>%
  filter(categorie_actuelle == "Explorateur", nb_visites >= 3)

cat("🏆 ACTION 4 : PROGRAMME DE FIDÉLISATION\n")
cat(paste("   Cible                  :", nrow(action4_cible), "explorateurs\n"))
cat("   Critères               : 3-5 visites\n")
cat("   Actions recommandées   :\n")
cat("      • Carte de fidélité (6ème visite gratuite)\n")
cat("      • Accès prioritaire aux événements\n")
cat("      • Newsletter mensuelle personnalisée\n")
cat("      • Programme parrainage (réduction)\n\n")


# =============================================================================
# PARTIE 12 : TABLEAU DE BORD DE SUIVI
# =============================================================================

cat("📊 INDICATEURS DE SUIVI\n")
cat("════════════════════════════════════════════════════════════════════════\n")

# KPI par segment
kpi_segments <- data.frame(
  Segment = c("Occasionnels à convertir", "Actifs à risque", "Mono-espaces à diversifier", "Explorateurs à fidéliser"),
  Effectif = c(
    nrow(action1_cible),
    nrow(action2_cible),
    nrow(action3_cible),
    nrow(action4_cible)
  ),
  Priorite = c("Haute", "Critique", "Moyenne", "Moyenne"),
  Impact_Potentiel = c(
    nrow(action1_cible) * 4,  # Occasionnels → 4 visites supplémentaires
    nrow(action2_cible) * 6,  # Rétention actifs
    nrow(action3_cible) * 2,  # Diversification
    nrow(action4_cible) * 5   # Fidélisation
  ),
  Cout_Action = c("Faible", "Moyen", "Faible", "Moyen")
)

cat("\n🎯 TABLEAU DE BORD DES SEGMENTS:\n")
print(kpi_segments)

# ROI estimé
roi_total <- sum(kpi_segments$Impact_Potentiel)
cat(paste("\n💰 IMPACT TOTAL ESTIMÉ:", format(roi_total, big.mark = " "), "visites supplémentaires\n"))
cat(paste("   Soit +", round(roi_total / sum(data_usagers$nb_visites) * 100, 1), "% vs actuel\n\n"))


# =============================================================================
# PARTIE 13 : EXPORT DES RÉSULTATS
# =============================================================================

cat("💾 EXPORT DES RÉSULTATS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# 1. Dataset avec scores
write.csv(data_scoring, "usagers_avec_scores.csv", row.names = FALSE)
cat("✓ Dataset complet : usagers_avec_scores.csv\n")

# 2. Listes d'actions
write.csv(action1_cible, "action1_occasionnels_convertir.csv", row.names = FALSE)
write.csv(action2_cible, "action2_actifs_reactiver.csv", row.names = FALSE)
write.csv(action3_cible, "action3_mono_diversifier.csv", row.names = FALSE)
write.csv(action4_cible, "action4_explorateurs_fideliser.csv", row.names = FALSE)
cat("✓ Listes d'actions : action*.csv\n")

# 3. Sauvegarder les modèles
saveRDS(model_prog_rf, "modele_progression.rds")
saveRDS(model_churn_rf, "modele_churn.rds")
saveRDS(model_multi_rf, "modele_multi_espaces.rds")
cat("✓ Modèles sauvegardés\n")

# 4. Rapport synthétique
rapport_comportement <- list(
  date_generation = Sys.Date(),
  
  # Progression
  nb_occasionnels = nrow(occasionnels_potentiel),
  nb_occasionnels_potentiel = nrow(action1_cible),
  auc_progression = ifelse(!is.null(model_prog_rf) && exists("roc_prog"), 
                           round(auc(roc_prog), 3), NA),
  
  # Churn
  nb_actifs = nrow(actifs_risque),
  nb_actifs_risque = nrow(action2_cible),
  auc_churn = ifelse(!is.null(model_churn_rf) && exists("roc_churn"), 
                     round(auc(roc_churn), 3), NA),
  
  # Multi-espaces
  nb_mono_espaces = nrow(mono_potentiel),
  nb_mono_potentiel = nrow(action3_cible),
  auc_multi = ifelse(!is.null(model_multi_rf) && exists("roc_multi"), 
                     round(auc(roc_multi), 3), NA),
  
  # Impact
  impact_total_visites = roi_total,
  pct_gain = round(roi_total / sum(data_usagers$nb_visites) * 100, 1)
)

saveRDS(rapport_comportement, "rapport_comportement_usagers.rds")
cat("✓ Rapport synthétique : rapport_comportement_usagers.rds\n\n")


# =============================================================================
# PARTIE 14 : RAPPORT FINAL FORMATÉ
# =============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                       ║\n")
cat("║         RAPPORT DE PRÉDICTION DU COMPORTEMENT DES USAGERS            ║\n")
cat("║                 Identification et Actions Ciblées                     ║\n")
cat("║                                                                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("📅 DATE DE GÉNÉRATION:", format(Sys.Date(), "%d/%m/%Y"), "\n\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 1. PERFORMANCE DES MODÈLES PRÉDICTIFS                                │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
cat(paste("   🎯 Modèle Progression   : AUC-ROC =", rapport_comportement$auc_progression, "\n"))
cat(paste("   ⚠️ Modèle Churn         : AUC-ROC =", rapport_comportement$auc_churn, "\n"))
cat(paste("   🗺️ Modèle Multi-Espaces : AUC-ROC =", rapport_comportement$auc_multi, "\n"))
cat("\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 2. SEGMENTS IDENTIFIÉS                                               │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n\n")

cat("   1️⃣ OCCASIONNELS À FORT POTENTIEL\n")
cat(paste("      • Total                :", rapport_comportement$nb_occasionnels, "\n"))
cat(paste("      • Priorité haute/très haute:", rapport_comportement$nb_occasionnels_potentiel, "\n"))
cat("      • Action               : Onboarding renforcé\n\n")

cat("   2️⃣ ACTIFS À RISQUE DE CHURN\n")
cat(paste("      • Total                :", rapport_comportement$nb_actifs, "\n"))
cat(paste("      • Risque critique/élevé:", rapport_comportement$nb_actifs_risque, "\n"))
cat("      • Action               : Réactivation urgente\n\n")

cat("   3️⃣ MONO-ESPACES À DIVERSIFIER\n")
cat(paste("      • Total                :", rapport_comportement$nb_mono_espaces, "\n"))
cat(paste("      • Potentiel élevé      :", rapport_comportement$nb_mono_potentiel, "\n"))
cat("      • Action               : Parcours découverte\n\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 3. IMPACT POTENTIEL DES ACTIONS                                      │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
print(kpi_segments)
cat("\n")
cat(paste("💎 GAIN TOTAL POTENTIEL  : +", format(rapport_comportement$impact_total_visites, big.mark = " "), 
          "visites\n"))
cat(paste("   Soit +", rapport_comportement$pct_gain, "% vs situation actuelle\n"))
cat("\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 4. ACTIONS PRIORITAIRES                                              │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n\n")

cat("   🥇 PRIORITÉ 1 : RÉACTIVATION ACTIFS À RISQUE (Impact immédiat)\n")
cat(paste("      → Cible:", nrow(action2_cible), "usagers\n"))
cat("      → Délai: Immédiat (< 7 jours)\n")
cat("      → Coût: Moyen | ROI: Très élevé\n\n")

cat("   🥈 PRIORITÉ 2 : CONVERSION OCCASIONNELS (Croissance)\n")
cat(paste("      → Cible:", nrow(action1_cible), "usagers\n"))
cat("      → Délai: Court terme (< 1 mois)\n")
cat("      → Coût: Faible | ROI: Élevé\n\n")

cat("   🥉 PRIORITÉ 3 : DIVERSIFICATION MONO-ESPACES (Engagement)\n")
cat(paste("      → Cible:", nrow(action3_cible), "usagers\n"))
cat("      → Délai: Moyen terme (1-3 mois)\n")
cat("      → Coût: Faible | ROI: Moyen\n\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 5. FICHIERS GÉNÉRÉS                                                  │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
cat("   ✓ usagers_avec_scores.csv\n")
cat("   ✓ action1_occasionnels_convertir.csv\n")
cat("   ✓ action2_actifs_reactiver.csv\n")
cat("   ✓ action3_mono_diversifier.csv\n")
cat("   ✓ action4_explorateurs_fideliser.csv\n")
cat("   ✓ modele_progression.rds\n")
cat("   ✓ modele_churn.rds\n")
cat("   ✓ modele_multi_espaces.rds\n")
cat("   ✓ rapport_comportement_usagers.rds\n")
cat("\n")

cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                         FIN DU RAPPORT                                ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n")

cat("\n✅ OBJECTIF 3 TERMINÉ AVEC SUCCÈS !\n\n")
cat("📌 PROCHAINE ÉTAPE:\n")
cat("   → Objectif 4 : Optimiser l'allocation des ressources\n\n")