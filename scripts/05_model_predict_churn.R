################################################################################
# MODÈLE 2 : PRÉDICTION DU CHURN (RISQUE D'ABANDON)
################################################################################
#
# Objectif : Détecter les signaux d'alerte précoces d'abandon chez les usagers 
#            actifs (ayant effectué au moins 3 visites)
#
# Variables utilisées :
# - Régularité des visites (écart-type des intervalles)
# - Évolution de la durée des visites (tendance)
# - Ancienneté et jours depuis dernière visite
# - Diversité des espaces utilisés
# - Changements de comportement récents
#
# Définition du churn : Usager avec ≥3 visites qui n'est pas revenu depuis 30+ jours
#
################################################################################

# Chargement des packages
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(gbm)
library(pROC)
library(ggplot2)
library(gridExtra)
library(ROSE)  # Pour gérer le déséquilibre des classes

set.seed(42)
options(scipen = 999)

cat("================================================================================\n")
cat("  MODÈLE 2 : PRÉDICTION DU CHURN (RISQUE D'ABANDON)\n")
cat("================================================================================\n\n")

################################################################################
# 1. CALCUL DES FEATURES DE CHURN
################################################################################

calculate_churn_features <- function(df, observation_date = NULL) {
  cat("1. Calcul des features de churn...\n")
  
  if (is.null(observation_date)) {
    observation_date <- max(df$date_arrivee)
  }
  
  # Calcul de la durée en minutes si pas déjà fait
  if (!"duree_minutes" %in% names(df)) {
    df <- df %>%
      mutate(duree_minutes = as.numeric(difftime(date_sortie, date_arrivee, 
                                                 units = "mins")))
  }
  
  # Calculer les features par usager
  churn_features <- df %>%
    arrange(id_usager, date_arrivee) %>%
    group_by(id_usager) %>%
    filter(n() >= 3) %>%  # Seulement usagers avec 3+ visites
    summarise(
      # FEATURES TEMPORELLES
      anciennete_jours = as.numeric(difftime(observation_date, min(date_arrivee), 
                                             units = "days")),
      jours_depuis_derniere = as.numeric(difftime(observation_date, max(date_arrivee), 
                                                  units = "days")),
      nb_visites = n(),
      
      # FRÉQUENCE
      frequence_mensuelle = if_else(anciennete_jours > 0,
                                    (nb_visites / anciennete_jours) * 30,
                                    0),
      
      # RÉGULARITÉ (écart-type des intervalles entre visites)
      regularite = if_else(n() > 1,
                           sd(as.numeric(diff(date_arrivee)), na.rm = TRUE),
                           0),
      
      # DURÉE
      duree_moyenne = mean(duree_minutes, na.rm = TRUE),
      duree_std = sd(duree_minutes, na.rm = TRUE),
      duree_max = max(duree_minutes, na.rm = TRUE),
      
      # TENDANCE DE LA DURÉE (pente de régression)
      tendance_duree = if_else(n() >= 3,
                               {
                                 x <- 1:n()
                                 y <- duree_minutes
                                 if (sum(!is.na(y)) >= 2) {
                                   coef(lm(y ~ x, na.action = na.exclude))[2]
                                 } else {
                                   0
                                 }
                               },
                               0),
      
      # DIVERSITÉ DES ESPACES
      nb_espaces_distincts = n_distinct(espace),
      espace_principal_pct = (max(table(espace)) / n()) * 100,
      
      # CHANGEMENT DE COMPORTEMENT RÉCENT
      changement_frequence = if_else(n() >= 6,
                                     {
                                       recent <- tail(date_arrivee, 3)
                                       previous <- head(tail(date_arrivee, 6), 3)
                                       
                                       recent_days <- as.numeric(difftime(max(recent), 
                                                                          min(recent), 
                                                                          units = "days"))
                                       previous_days <- as.numeric(difftime(max(previous), 
                                                                            min(previous), 
                                                                            units = "days"))
                                       
                                       recent_freq <- if_else(recent_days > 0, 
                                                              3 / recent_days, 3)
                                       previous_freq <- if_else(previous_days > 0, 
                                                                3 / previous_days, 3)
                                       
                                       recent_freq - previous_freq
                                     },
                                     0),
      
      # RATIO DERNIÈRE VISITE
      derniere_duree = last(duree_minutes),
      ratio_derniere_duree = if_else(duree_moyenne > 0,
                                     derniere_duree / duree_moyenne,
                                     1),
      
      # DÉMOGRAPHIQUES
      age = first(age),
      genre = first(genre),
      ville = first(ville),
      
      # VARIABLE CIBLE : EST EN CHURN
      est_churn = as.integer(jours_depuis_derniere >= 30 & nb_visites >= 3),
      
      .groups = 'drop'
    )
  
  cat(sprintf("   - %d usagers analysés (avec ≥3 visites)\n", nrow(churn_features)))
  cat(sprintf("   - Taux de churn : %.1f%%\n", 
              mean(churn_features$est_churn) * 100))
  
  return(churn_features)
}

prepare_churn_data <- function(df_features) {
  cat("\n2. Préparation des données...\n")
  
  # Gestion des valeurs manquantes
  df_features <- df_features %>%
    mutate(
      age = ifelse(is.na(age), median(age, na.rm = TRUE), age),
      duree_moyenne = ifelse(is.na(duree_moyenne), 
                             median(duree_moyenne, na.rm = TRUE), 
                             duree_moyenne),
      duree_std = ifelse(is.na(duree_std), 0, duree_std),
      regularite = ifelse(is.na(regularite), 0, regularite),
      tendance_duree = ifelse(is.na(tendance_duree), 0, tendance_duree)
    )
  
  # Encodage du genre
  df_features <- df_features %>%
    mutate(
      genre_encoded = case_when(
        genre == "H" ~ 1,
        genre == "F" ~ 0,
        TRUE ~ -1
      )
    )
  
  # Sélection des features numériques
  feature_columns <- c(
    "anciennete_jours", "jours_depuis_derniere", "nb_visites",
    "frequence_mensuelle", "regularite", "tendance_duree",
    "duree_moyenne", "duree_std", "nb_espaces_distincts", 
    "espace_principal_pct", "changement_frequence", 
    "ratio_derniere_duree", "age", "genre_encoded"
  )
  
  X <- df_features %>% select(all_of(feature_columns))
  y <- factor(df_features$est_churn, levels = c(0, 1), 
              labels = c("Actif", "Churn"))
  
  cat(sprintf("   - %d features créées\n", ncol(X)))
  cat(sprintf("   - Distribution classes : Actif=%d, Churn=%d\n",
              sum(y == "Actif"), sum(y == "Churn")))
  
  return(list(
    X = X,
    y = y,
    feature_names = feature_columns,
    data = df_features
  ))
}

################################################################################
# 2. ENTRAÎNEMENT DES MODÈLES
################################################################################

train_churn_models <- function(X, y) {
  cat("\n3. Entraînement des modèles de churn...\n")
  
  # Split train/test stratifié
  train_index <- createDataPartition(y, p = 0.75, list = FALSE)
  
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  cat(sprintf("   - Train : %d observations\n", length(y_train)))
  cat(sprintf("   - Test : %d observations\n", length(y_test)))
  
  # Configuration avec gestion du déséquilibre
  train_control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"  # SMOTE pour gérer le déséquilibre
  )
  
  results <- list()
  
  # ============================================================================
  # MODÈLE 1 : RÉGRESSION LOGISTIQUE AVEC PÉNALITÉ
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("1. RÉGRESSION LOGISTIQUE (avec class_weight)\n")
  cat(rep("=", 60), "\n", sep = "")
  
  # Poids pour gérer le déséquilibre
  weights <- ifelse(y_train == "Churn", 
                    sum(y_train == "Actif") / sum(y_train == "Churn"), 
                    1)
  
  model_glm <- train(
    x = X_train,
    y = y_train,
    method = "glm",
    family = "binomial",
    trControl = train_control,
    metric = "ROC",
    preProcess = c("center", "scale"),
    weights = weights
  )
  
  pred_glm <- predict(model_glm, X_test, type = "prob")
  pred_glm_class <- predict(model_glm, X_test)
  
  cm_glm <- confusionMatrix(pred_glm_class, y_test, positive = "Churn")
  roc_glm <- roc(y_test, pred_glm$Churn)
  
  cat(sprintf("\nAUC-ROC : %.4f\n", auc(roc_glm)))
  cat(sprintf("Sensibilité (détection churn) : %.2f%%\n", 
              cm_glm$byClass["Sensitivity"] * 100))
  cat(sprintf("Spécificité : %.2f%%\n", cm_glm$byClass["Specificity"] * 100))
  print(cm_glm$table)
  
  results$glm <- list(
    model = model_glm,
    predictions = pred_glm,
    pred_class = pred_glm_class,
    confusion_matrix = cm_glm,
    roc = roc_glm,
    auc = auc(roc_glm)
  )
  
  # ============================================================================
  # MODÈLE 2 : RANDOM FOREST
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("2. RANDOM FOREST\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_rf <- train(
    x = X_train,
    y = y_train,
    method = "rf",
    trControl = train_control,
    metric = "ROC",
    ntree = 200,
    tuneGrid = expand.grid(mtry = c(3, 5, 7)),
    importance = TRUE
  )
  
  pred_rf <- predict(model_rf, X_test, type = "prob")
  pred_rf_class <- predict(model_rf, X_test)
  
  cm_rf <- confusionMatrix(pred_rf_class, y_test, positive = "Churn")
  roc_rf <- roc(y_test, pred_rf$Churn)
  
  cat(sprintf("\nAUC-ROC : %.4f\n", auc(roc_rf)))
  cat(sprintf("Sensibilité (détection churn) : %.2f%%\n", 
              cm_rf$byClass["Sensitivity"] * 100))
  cat(sprintf("Spécificité : %.2f%%\n", cm_rf$byClass["Specificity"] * 100))
  print(cm_rf$table)
  
  results$rf <- list(
    model = model_rf,
    predictions = pred_rf,
    pred_class = pred_rf_class,
    confusion_matrix = cm_rf,
    roc = roc_rf,
    auc = auc(roc_rf)
  )
  
  # ============================================================================
  # MODÈLE 3 : GRADIENT BOOSTING
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("3. GRADIENT BOOSTING MACHINE (GBM)\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_gbm <- train(
    x = X_train,
    y = y_train,
    method = "gbm",
    trControl = train_control,
    metric = "ROC",
    verbose = FALSE,
    tuneGrid = expand.grid(
      n.trees = c(100, 150),
      interaction.depth = c(3, 5),
      shrinkage = 0.05,
      n.minobsinnode = 10
    )
  )
  
  pred_gbm <- predict(model_gbm, X_test, type = "prob")
  pred_gbm_class <- predict(model_gbm, X_test)
  
  cm_gbm <- confusionMatrix(pred_gbm_class, y_test, positive = "Churn")
  roc_gbm <- roc(y_test, pred_gbm$Churn)
  
  cat(sprintf("\nAUC-ROC : %.4f\n", auc(roc_gbm)))
  cat(sprintf("Sensibilité (détection churn) : %.2f%%\n", 
              cm_gbm$byClass["Sensitivity"] * 100))
  cat(sprintf("Spécificité : %.2f%%\n", cm_gbm$byClass["Specificity"] * 100))
  print(cm_gbm$table)
  
  results$gbm <- list(
    model = model_gbm,
    predictions = pred_gbm,
    pred_class = pred_gbm_class,
    confusion_matrix = cm_gbm,
    roc = roc_gbm,
    auc = auc(roc_gbm)
  )
  
  # Sélection du meilleur modèle
  best_model_name <- names(which.max(sapply(results, function(x) x$auc)))
  cat("\n", rep("=", 60), "\n", sep = "")
  cat(sprintf("✅ MEILLEUR MODÈLE : %s (AUC = %.4f)\n", 
              toupper(best_model_name), 
              results[[best_model_name]]$auc))
  cat(rep("=", 60), "\n", sep = "")
  
  results$best_model <- best_model_name
  results$y_test <- y_test
  
  return(results)
}

################################################################################
# 3. ANALYSE DES FEATURES IMPORTANTES
################################################################################

analyze_feature_importance <- function(results, feature_names) {
  cat("\n4. Analyse de l'importance des variables...\n")
  
  best_model_name <- results$best_model
  best_model <- results[[best_model_name]]$model
  
  if (best_model_name == "rf") {
    importance_df <- as.data.frame(importance(best_model$finalModel))
    importance_df$feature <- rownames(importance_df)
    importance_df <- importance_df %>%
      arrange(desc(MeanDecreaseGini)) %>%
      head(10)
    
    cat("\nTop 10 variables prédictives du churn :\n")
    print(importance_df %>% select(feature, MeanDecreaseGini))
    
    p <- ggplot(importance_df, aes(x = reorder(feature, MeanDecreaseGini), 
                                   y = MeanDecreaseGini)) +
      geom_bar(stat = "identity", fill = "#E74C3C") +
      coord_flip() +
      labs(title = "Variables les Plus Prédictives du Churn - Random Forest",
           x = "Variable", y = "Importance") +
      theme_minimal(base_size = 12)
    
  } else if (best_model_name == "gbm") {
    importance_df <- summary(best_model$finalModel, plotit = FALSE)
    importance_df <- importance_df %>% head(10)
    
    cat("\nTop 10 variables prédictives du churn :\n")
    print(importance_df)
    
    p <- ggplot(importance_df, aes(x = reorder(var, rel.inf), y = rel.inf)) +
      geom_bar(stat = "identity", fill = "#E67E22") +
      coord_flip() +
      labs(title = "Variables les Plus Prédictives du Churn - GBM",
           x = "Variable", y = "Influence Relative") +
      theme_minimal(base_size = 12)
    
  } else {
    coef_df <- data.frame(
      feature = names(coef(best_model$finalModel))[-1],
      coefficient = abs(coef(best_model$finalModel)[-1])
    ) %>%
      arrange(desc(coefficient)) %>%
      head(10)
    
    cat("\nTop 10 variables prédictives du churn :\n")
    print(coef_df)
    
    p <- ggplot(coef_df, aes(x = reorder(feature, coefficient), y = coefficient)) +
      geom_bar(stat = "identity", fill = "#9B59B6") +
      coord_flip() +
      labs(title = "Variables les Plus Prédictives du Churn - GLM",
           x = "Variable", y = "Coefficient (valeur absolue)") +
      theme_minimal(base_size = 12)
  }
  
  print(p)
  
  return(importance_df)
}

################################################################################
# 4. VISUALISATIONS COMPLÈTES
################################################################################

plot_comprehensive_results <- function(results) {
  cat("\n5. Génération des visualisations...\n")
  
  # 1. Courbes ROC comparatives
  roc_data <- bind_rows(
    data.frame(
      FPR = 1 - results$glm$roc$specificities,
      TPR = results$glm$roc$sensitivities,
      model = sprintf("GLM (AUC=%.3f)", results$glm$auc)
    ),
    data.frame(
      FPR = 1 - results$rf$roc$specificities,
      TPR = results$rf$roc$sensitivities,
      model = sprintf("RF (AUC=%.3f)", results$rf$auc)
    ),
    data.frame(
      FPR = 1 - results$gbm$roc$specificities,
      TPR = results$gbm$roc$sensitivities,
      model = sprintf("GBM (AUC=%.3f)", results$gbm$auc)
    )
  )
  
  p_roc <- ggplot(roc_data, aes(x = FPR, y = TPR, color = model)) +
    geom_line(size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = "Courbes ROC - Détection du Churn",
         x = "Taux de Faux Positifs",
         y = "Taux de Vrais Positifs",
         color = "Modèle") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p_roc)
  
  # 2. Distribution des scores de churn
  best_model_name <- results$best_model
  pred_df <- data.frame(
    churn_score = results[[best_model_name]]$predictions$Churn * 100,
    actual = results$y_test
  )
  
  p_dist <- ggplot(pred_df, aes(x = churn_score, fill = actual)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "red", size = 1) +
    scale_fill_manual(values = c("Actif" = "#27AE60", "Churn" = "#E74C3C")) +
    labs(title = sprintf("Distribution des Scores de Risque de Churn - %s", 
                         toupper(best_model_name)),
         x = "Score de Risque de Churn (%)", 
         y = "Nombre d'usagers",
         fill = "Statut Réel") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
  
  print(p_dist)
  
  # 3. Matrices de confusion comparatives
  cm_plots <- list()
  
  for (model_name in c("glm", "rf", "gbm")) {
    cm <- results[[model_name]]$confusion_matrix$table
    cm_df <- as.data.frame(cm)
    
    p <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 6, fontface = "bold") +
      scale_fill_gradient(low = "#FEF5E7", high = "#E74C3C") +
      labs(title = sprintf("%s (AUC=%.3f)", 
                           toupper(model_name), 
                           results[[model_name]]$auc),
           x = "Réel", y = "Prédit") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))
    
    cm_plots[[model_name]] <- p
  }
  
  grid.arrange(grobs = cm_plots, ncol = 3,
               top = "Matrices de Confusion - Détection du Churn")
}

################################################################################
# 5. SCORING ET SEGMENTATION
################################################################################

score_and_segment <- function(prepared_data, results) {
  cat("\n6. Scoring et segmentation des usagers...\n")
  
  best_model_name <- results$best_model
  best_model <- results[[best_model_name]]$model
  
  # Prédictions sur tous les usagers
  all_predictions <- predict(best_model, prepared_data$X, type = "prob")
  
  scored_users <- prepared_data$data %>%
    mutate(
      churn_score = all_predictions$Churn * 100,
      churn_risk = cut(churn_score,
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c("Faible", "Moyen", "Élevé", "Très Élevé"),
                       include.lowest = TRUE),
      action_recommandee = case_when(
        churn_score >= 75 ~ "URGENCE - Intervention immédiate",
        churn_score >= 50 & jours_depuis_derniere >= 14 ~ "ALERTE - Suivi rapproché",
        churn_score >= 25 & nb_visites >= 5 ~ "SURVEILLANCE - Monitoring",
        TRUE ~ "STABLE - Maintien engagement"
      )
    )
  
  cat("\nDistribution des risques de churn :\n")
  print(table(scored_users$churn_risk))
  
  cat("\nActions recommandées :\n")
  print(table(scored_users$action_recommandee))
  
  # Statistiques par segment
  cat("\nStatistiques par segment de risque :\n")
  stats <- scored_users %>%
    group_by(churn_risk) %>%
    summarise(
      n = n(),
      score_moyen = mean(churn_score),
      jours_inactif_moyen = mean(jours_depuis_derniere),
      taux_churn_reel = mean(est_churn) * 100,
      .groups = 'drop'
    )
  print(stats)
  
  return(scored_users)
}

################################################################################
# 6. RECOMMANDATIONS STRATÉGIQUES
################################################################################

generate_recommendations <- function(scored_users) {
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("RECOMMANDATIONS ACTIONNABLES\n")
  cat(rep("=", 80), "\n", sep = "")
  
  urgence <- scored_users %>% 
    filter(action_recommandee == "URGENCE - Intervention immédiate")
  alerte <- scored_users %>% 
    filter(action_recommandee == "ALERTE - Suivi rapproché")
  surveillance <- scored_users %>% 
    filter(action_recommandee == "SURVEILLANCE - Monitoring")
  
  cat(sprintf("\n🚨 USAGERS EN URGENCE : %d (%.1f%%)\n", 
              nrow(urgence), nrow(urgence)/nrow(scored_users)*100))
  cat("   Actions IMMÉDIATES :\n")
  cat("   1. Appel téléphonique personnalisé dans les 24h\n")
  cat("   2. Offre exclusive de réengagement (réduction 20%%)\n")
  cat("   3. Invitation à événement VIP\n")
  cat("   4. Questionnaire de satisfaction avec incentive\n")
  
  cat(sprintf("\n⚠️  USAGERS EN ALERTE : %d (%.1f%%)\n", 
              nrow(alerte), nrow(alerte)/nrow(scored_users)*100))
  cat("   Actions PRÉVENTIVES :\n")
  cat("   1. Email personnalisé de suivi\n")
  cat("   2. Proposition de nouveaux services/espaces\n")
  cat("   3. Rappel des bénéfices et événements à venir\n")
  cat("   4. Invitation à partager feedback\n")
  
  cat(sprintf("\n📊 USAGERS EN SURVEILLANCE : %d (%.1f%%)\n", 
              nrow(surveillance), nrow(surveillance)/nrow(scored_users)*100))
  cat("   Actions de MONITORING :\n")
  cat("   1. Newsletter régulière\n")
  cat("   2. Tracking hebdomadaire de l'engagement\n")
  cat("   3. Alertes automatiques si dégradation\n")
  
  # Impact business
  cat("\n💰 IMPACT BUSINESS POTENTIEL\n")
  if (nrow(urgence) > 0) {
    cat(sprintf("   - Si on sauve 30%% des usagers en urgence : +%d usagers actifs\n",
                round(nrow(urgence) * 0.3)))
  }
  if (nrow(alerte) > 0) {
    cat(sprintf("   - Si on sauve 50%% des usagers en alerte : +%d usagers actifs\n",
                round(nrow(alerte) * 0.5)))
  }
  
  cat("\n✅ MODÈLE OPÉRATIONNEL\n")
  cat("   Fréquence recommandée : Scoring hebdomadaire\n")
  cat("   Priorisation : Urgence > Alerte > Surveillance\n")
}

################################################################################
# 7. EXÉCUTION PRINCIPALE
################################################################################

main <- function() {
  
  # Charger les données (remplacer par votre fichier)
  # df <- read.csv("votre_fichier.csv", stringsAsFactors = FALSE)
  # df$date_arrivee <- as.POSIXct(df$date_arrivee)
  
  # Pour l'exemple, données synthétiques
  set.seed(42)
  n_users <- 3540
  
  user_visits <- map_dfr(1:n_users, function(user_id) {
    n_visits <- sample(3:20, 1, prob = c(rep(0.15, 3), rep(0.05, 15)))
    
    data.frame(
      id_usager = user_id,
      visit_num = 1:n_visits
    )
  })
  
  df <- user_visits %>%
    group_by(id_usager) %>%
    mutate(
      date_arrivee = as.POSIXct("2024-01-01") + 
        hours(cumsum(sample(12:240, n(), replace = TRUE))),
      duree_minutes = pmax(30, rnorm(n(), 280, 100)),
      espace = sample(c("Rooftop", "Salle A", "Salle B", "Studio"), 
                      n(), replace = TRUE),
      age = round(rnorm(n(), 25, 8)),
      genre = sample(c("H", "F"), n(), replace = TRUE, prob = c(0.7, 0.3)),
      ville = sample(c("Cotonou", "Porto-Novo", "Parakou"), 
                     n(), replace = TRUE, prob = c(0.6, 0.25, 0.15))
    ) %>%
    ungroup()
  
  # 1. Calculer les features de churn
  churn_features <- calculate_churn_features(df)
  
  # 2. Préparer les données
  prepared_data <- prepare_churn_data(churn_features)
  
  # 3. Entraîner les modèles
  results <- train_churn_models(prepared_data$X, prepared_data$y)
  
  # 4. Analyser l'importance des features
  importance <- analyze_feature_importance(results, prepared_data$feature_names)
  
  # 5. Visualisations complètes
  plot_comprehensive_results(results)
  
  # 6. Scorer et segmenter tous les usagers
  scored_users <- score_and_segment(prepared_data, results)
  
  # 7. Générer les recommandations
  generate_recommendations(scored_users)
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("✅ MODÈLE 2 TERMINÉ AVEC SUCCÈS !\n")
  cat(rep("=", 80), "\n", sep = "")
  
  return(list(
    data = df,
    churn_features = churn_features,
    prepared_data = prepared_data,
    results = results,
    scored_users = scored_users
  ))
}

# EXÉCUTION
output <- main()