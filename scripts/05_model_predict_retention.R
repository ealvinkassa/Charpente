################################################################################
# MODÈLE 1 : PRÉDICTION DE LA RÉTENTION (RETOUR APRÈS 1ÈRE VISITE)
################################################################################
# 
# Objectif : Prédire si un usager reviendra après sa première visite
# 
# Variables utilisées :
# - Durée de la première visite
# - Espace visité lors de la première visite
# - Heure d'arrivée
# - Jour de la semaine
# - Profil démographique (âge, genre)
# - Ville d'origine
#
################################################################################

# Chargement des packages
library(tidyverse)      # Manipulation de données
library(lubridate)      # Gestion des dates
library(caret)          # Machine learning
library(randomForest)   # Random Forest
library(gbm)            # Gradient Boosting
library(pROC)           # Courbes ROC
library(ggplot2)        # Visualisations
library(scales)         # Formatage
library(gridExtra)      # Arrangement des graphiques

# Configuration
set.seed(42)
options(scipen = 999)

cat("================================================================================\n")
cat("  MODÈLE 1 : PRÉDICTION DE LA RÉTENTION APRÈS PREMIÈRE VISITE\n")
cat("================================================================================\n\n")

################################################################################
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
################################################################################

load_and_prepare_data <- function(file_path) {
  cat("1. Chargement des données...\n")
  
  # Charger vos données
  # df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Pour l'exemple, création de données synthétiques
  set.seed(42)
  n_users <- 3540
  
  # Génération des usagers avec nombre de visites variable
  visits_per_user <- sample(1:15, n_users, replace = TRUE, 
                            prob = c(0.559, rep(0.441/14, 14)))
  
  df <- data.frame(
    id_usager = rep(1:n_users, visits_per_user)
  ) %>%
    arrange(id_usager) %>%
    group_by(id_usager) %>%
    mutate(
      date_arrivee = as.POSIXct("2024-01-01") + hours(row_number() * sample(24:168, 1)),
      duree_minutes = rnorm(n(), mean = 280, sd = 100),
      duree_minutes = pmax(30, duree_minutes),  # Min 30 minutes
      espace = sample(c("Rooftop", "Salle A", "Salle B", "Cafétéria", "Studio"), 
                      n(), replace = TRUE, prob = c(0.35, 0.2, 0.15, 0.15, 0.15)),
      age = round(rnorm(n(), mean = 25, sd = 8)),
      age = pmax(18, pmin(60, age)),
      genre = sample(c("H", "F"), n(), replace = TRUE, prob = c(0.7, 0.3)),
      ville = sample(c("Cotonou", "Porto-Novo", "Parakou", "Abomey", "Autre"), 
                     n(), replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.05, 0.05))
    ) %>%
    ungroup()
  
  cat(sprintf("   - %d visites chargées\n", nrow(df)))
  cat(sprintf("   - %d usagers uniques\n", n_distinct(df$id_usager)))
  
  return(df)
}

create_retention_dataset <- function(df) {
  cat("\n2. Création du dataset de rétention...\n")
  
  # Identifier la première visite de chaque usager
  first_visits <- df %>%
    arrange(id_usager, date_arrivee) %>%
    group_by(id_usager) %>%
    slice(1) %>%
    ungroup()
  
  # Compter le nombre total de visites par usager
  visit_counts <- df %>%
    group_by(id_usager) %>%
    summarise(nombre_visites = n(), .groups = 'drop')
  
  # Créer la variable cible : a_revisite (1 si >1 visite, 0 sinon)
  retention_data <- first_visits %>%
    left_join(visit_counts, by = "id_usager") %>%
    mutate(
      a_revisite = ifelse(nombre_visites > 1, 1, 0),
      heure_arrivee = hour(date_arrivee),
      jour_semaine = wday(date_arrivee, week_start = 1),  # 1=Lundi, 7=Dimanche
      est_weekend = ifelse(jour_semaine %in% c(6, 7), 1, 0),
      mois = month(date_arrivee)
    )
  
  cat(sprintf("   - %d usagers analysés\n", nrow(retention_data)))
  cat(sprintf("   - Taux de rétention : %.1f%%\n", 
              mean(retention_data$a_revisite) * 100))
  cat(sprintf("   - Occasionnels : %d (%.1f%%)\n", 
              sum(retention_data$a_revisite == 0),
              mean(retention_data$a_revisite == 0) * 100))
  
  return(retention_data)
}

prepare_features <- function(retention_data) {
  cat("\n3. Préparation des features...\n")
  
  # Gestion des valeurs manquantes
  retention_data <- retention_data %>%
    mutate(
      duree_minutes = ifelse(is.na(duree_minutes), 
                             median(duree_minutes, na.rm = TRUE), 
                             duree_minutes),
      age = ifelse(is.na(age), median(age, na.rm = TRUE), age)
    )
  
  # Encodage des variables catégorielles
  retention_data <- retention_data %>%
    mutate(
      genre_encoded = case_when(
        genre == "H" ~ 1,
        genre == "F" ~ 0,
        TRUE ~ -1
      ),
      ville_top = ifelse(ville %in% c("Cotonou", "Porto-Novo", "Parakou"), 
                         ville, "Autre"),
      espace_top = ifelse(espace %in% c("Rooftop", "Salle A", "Salle B"), 
                          espace, "Autre")
    )
  
  # Créer des variables dummy pour ville et espace
  ville_dummies <- model.matrix(~ ville_top - 1, data = retention_data)
  espace_dummies <- model.matrix(~ espace_top - 1, data = retention_data)
  
  # Combiner toutes les features
  feature_matrix <- cbind(
    retention_data %>% 
      select(duree_minutes, heure_arrivee, jour_semaine, est_weekend, 
             mois, age, genre_encoded),
    ville_dummies,
    espace_dummies
  )
  
  cat(sprintf("   - %d features créées\n", ncol(feature_matrix)))
  
  return(list(
    X = as.data.frame(feature_matrix),
    y = as.factor(retention_data$a_revisite),
    data = retention_data
  ))
}

################################################################################
# 2. ENTRAÎNEMENT DES MODÈLES
################################################################################

train_models <- function(X, y) {
  cat("\n4. Séparation Train/Test et standardisation...\n")
  
  # Split train/test (80/20)
  train_index <- createDataPartition(y, p = 0.8, list = FALSE)
  
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  cat(sprintf("   - Train : %d observations\n", length(y_train)))
  cat(sprintf("   - Test : %d observations\n", length(y_test)))
  
  # Configuration du contrôle d'entraînement
  train_control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  # Renommer les niveaux pour caret (exige des noms valides sans chiffres)
  levels(y_train) <- c("Non", "Oui")
  levels(y_test) <- c("Non", "Oui")
  
  results <- list()
  
  # ============================================================================
  # MODÈLE 1 : RÉGRESSION LOGISTIQUE
  # ============================================================================
  cat("\n" ,rep("=", 60), "\n", sep = "")
  cat("MODÈLE 1 : RÉGRESSION LOGISTIQUE\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_glm <- train(
    x = X_train,
    y = y_train,
    method = "glm",
    family = "binomial",
    trControl = train_control,
    metric = "ROC",
    preProcess = c("center", "scale")
  )
  
  pred_glm <- predict(model_glm, X_test, type = "prob")
  pred_glm_class <- predict(model_glm, X_test)
  
  # Métriques
  cm_glm <- confusionMatrix(pred_glm_class, y_test)
  
  # Extraire la table de confusion de manière robuste
  if (is.list(cm_glm)) {
    cm_table_glm <- cm_glm$table
  } else if (is.table(cm_glm)) {
    cm_table_glm <- cm_glm
  } else {
    cm_table_glm <- table(Prediction = pred_glm_class, Reference = y_test)
  }
  
  roc_glm <- roc(as.numeric(y_test) - 1, pred_glm$Oui)
  
  cat(sprintf("\nAUC-ROC : %.4f\n", auc(roc_glm)))
  print(cm_table_glm)
  
  results$glm <- list(
    model = model_glm,
    predictions = pred_glm,
    pred_class = pred_glm_class,
    confusion_matrix = cm_table_glm,
    roc = roc_glm,
    auc = auc(roc_glm)
  )
  
  # ============================================================================
  # MODÈLE 2 : RANDOM FOREST
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("MODÈLE 2 : RANDOM FOREST\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_rf <- train(
    x = X_train,
    y = y_train,
    method = "rf",
    trControl = train_control,
    metric = "ROC",
    ntree = 100,
    importance = TRUE
  )
  
  pred_rf <- predict(model_rf, X_test, type = "prob")
  pred_rf_class <- predict(model_rf, X_test)
  
  cm_rf <- confusionMatrix(pred_rf_class, y_test)
  
  # Extraire la table
  if (is.list(cm_rf)) {
    cm_table_rf <- cm_rf$table
  } else if (is.table(cm_rf)) {
    cm_table_rf <- cm_rf
  } else {
    cm_table_rf <- table(Prediction = pred_rf_class, Reference = y_test)
  }
  
  roc_rf <- roc(as.numeric(y_test) - 1, pred_rf$Oui)
  
  cat(sprintf("\nAUC-ROC : %.4f\n", auc(roc_rf)))
  print(cm_table_rf)
  
  results$rf <- list(
    model = model_rf,
    predictions = pred_rf,
    pred_class = pred_rf_class,
    confusion_matrix = cm_table_rf,
    roc = roc_rf,
    auc = auc(roc_rf)
  )
  
  # ============================================================================
  # MODÈLE 3 : GRADIENT BOOSTING
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("MODÈLE 3 : GRADIENT BOOSTING MACHINE (GBM)\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_gbm <- train(
    x = X_train,
    y = y_train,
    method = "gbm",
    trControl = train_control,
    metric = "ROC",
    verbose = FALSE
  )
  
  pred_gbm <- predict(model_gbm, X_test, type = "prob")
  pred_gbm_class <- predict(model_gbm, X_test)
  
  cm_gbm <- confusionMatrix(pred_gbm_class, y_test)
  
  # Extraire la table
  if (is.list(cm_gbm)) {
    cm_table_gbm <- cm_gbm$table
  } else if (is.table(cm_gbm)) {
    cm_table_gbm <- cm_gbm
  } else {
    cm_table_gbm <- table(Prediction = pred_gbm_class, Reference = y_test)
  }
  
  roc_gbm <- roc(as.numeric(y_test) - 1, pred_gbm$Oui)
  
  cat(sprintf("\nAUC-ROC : %.4f\n", auc(roc_gbm)))
  print(cm_table_gbm)
  
  results$gbm <- list(
    model = model_gbm,
    predictions = pred_gbm,
    pred_class = pred_gbm_class,
    confusion_matrix = cm_table_gbm,
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

analyze_feature_importance <- function(results, X) {
  cat("\n5. Analyse de l'importance des variables...\n")
  
  best_model_name <- results$best_model
  best_model <- results[[best_model_name]]$model
  
  if (best_model_name == "rf") {
    # Pour Random Forest
    importance_df <- as.data.frame(importance(best_model$finalModel))
    importance_df$feature <- rownames(importance_df)
    importance_df <- importance_df %>%
      arrange(desc(MeanDecreaseGini)) %>%
      head(10)
    
    p <- ggplot(importance_df, aes(x = reorder(feature, MeanDecreaseGini), 
                                   y = MeanDecreaseGini)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Variables Importantes - Random Forest",
           x = "Variable", y = "Importance (Mean Decrease Gini)") +
      theme_minimal(base_size = 12)
    
  } else if (best_model_name == "gbm") {
    # Pour GBM
    importance_df <- summary(best_model$finalModel, plotit = FALSE)
    importance_df <- importance_df %>% head(10)
    
    p <- ggplot(importance_df, aes(x = reorder(var, rel.inf), y = rel.inf)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(title = "Top 10 Variables Importantes - GBM",
           x = "Variable", y = "Influence Relative") +
      theme_minimal(base_size = 12)
    
  } else {
    # Pour GLM
    coef_df <- data.frame(
      feature = names(coef(best_model$finalModel))[-1],
      coefficient = abs(coef(best_model$finalModel)[-1])
    ) %>%
      arrange(desc(coefficient)) %>%
      head(10)
    
    p <- ggplot(coef_df, aes(x = reorder(feature, coefficient), y = coefficient)) +
      geom_bar(stat = "identity", fill = "coral") +
      coord_flip() +
      labs(title = "Top 10 Variables Importantes - Régression Logistique",
           x = "Variable", y = "Coefficient (valeur absolue)") +
      theme_minimal(base_size = 12)
  }
  
  print(p)
  
  return(importance_df)
}

################################################################################
# 4. VISUALISATIONS
################################################################################

plot_results <- function(results) {
  cat("\n6. Génération des visualisations...\n")
  
  # 1. Matrices de confusion
  cm_plots <- list()
  
  for (model_name in c("glm", "rf", "gbm")) {
    cm_obj <- results[[model_name]]$confusion_matrix
    # La table est déjà extraite et stockée directement
    cm <- if (is.table(cm_obj)) cm_obj else as.table(cm_obj)
    cm_df <- as.data.frame(cm)
    
    p <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 6) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = sprintf("%s (AUC=%.3f)", 
                           toupper(model_name), 
                           results[[model_name]]$auc)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    cm_plots[[model_name]] <- p
  }
  
  grid.arrange(grobs = cm_plots, ncol = 3, 
               top = "Matrices de Confusion")
  
  # 2. Courbes ROC
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
  
  roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR, color = model)) +
    geom_line(size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(title = "Courbes ROC - Comparaison des Modèles",
         x = "Taux de Faux Positifs (1 - Spécificité)",
         y = "Taux de Vrais Positifs (Sensibilité)",
         color = "Modèle") +
    theme_minimal(base_size = 12)
  
  print(roc_plot)
  
  # 3. Distribution des scores prédits
  best_model_name <- results$best_model
  pred_df <- data.frame(
    score = results[[best_model_name]]$predictions$Oui,
    actual = results$y_test
  )
  
  dist_plot <- ggplot(pred_df, aes(x = score, fill = actual)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    labs(title = sprintf("Distribution des Scores de Rétention - %s", 
                         toupper(best_model_name)),
         x = "Probabilité de Revenir", y = "Nombre d'usagers",
         fill = "Réalité") +
    theme_minimal(base_size = 12)
  
  print(dist_plot)
}

################################################################################
# 5. SCORING DES USAGERS
################################################################################

score_users <- function(prepared_data, results) {
  cat("\n7. Scoring de tous les usagers...\n")
  
  best_model_name <- results$best_model
  best_model <- results[[best_model_name]]$model
  
  # Prédictions sur tous les usagers
  all_predictions <- predict(best_model, prepared_data$X, type = "prob")
  
  scored_users <- prepared_data$data %>%
    mutate(
      score_retention = all_predictions$Oui * 100,
      segment_risque = cut(score_retention,
                           breaks = c(0, 25, 50, 75, 100),
                           labels = c("Très faible", "Faible", "Moyen", "Fort"),
                           include.lowest = TRUE)
    )
  
  cat("\nDistribution des segments de risque :\n")
  print(table(scored_users$segment_risque))
  
  cat("\nStatistiques par segment :\n")
  stats <- scored_users %>%
    group_by(segment_risque) %>%
    summarise(
      n = n(),
      score_moyen = mean(score_retention),
      taux_retour_reel = mean(a_revisite) * 100,
      .groups = 'drop'
    )
  print(stats)
  
  return(scored_users)
}

################################################################################
# 6. RECOMMANDATIONS
################################################################################

generate_recommendations <- function(scored_users) {
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("RECOMMANDATIONS ACTIONNABLES\n")
  cat(rep("=", 80), "\n", sep = "")
  
  # Usagers à risque
  low_risk <- scored_users %>%
    filter(segment_risque %in% c("Très faible", "Faible"))
  
  cat(sprintf("\n🎯 USAGERS À RISQUE ÉLEVÉ : %d (%.1f%%)\n", 
              nrow(low_risk),
              nrow(low_risk) / nrow(scored_users) * 100))
  cat("   Actions recommandées :\n")
  cat("   - Programme d'onboarding renforcé\n")
  cat("   - Suivi personnalisé dans les 7 jours\n")
  cat("   - Incentives de retour (réductions, événements)\n")
  cat("   - Enquête de satisfaction post-première visite\n")
  
  # Potentiel de conversion
  cat(sprintf("\n💡 POTENTIEL DE CONVERSION\n"))
  cat(sprintf("   - Si on augmente le taux de rétention de 10%%\n"))
  cat(sprintf("   - Impact : +%d usagers réguliers potentiels\n", 
              round(nrow(low_risk) * 0.1)))
  
  cat("\n✅ MODÈLE OPÉRATIONNEL ET PRÊT À L'EMPLOI\n")
}

################################################################################
# 7. EXÉCUTION PRINCIPALE
################################################################################

main <- function() {
  
  # 1. Charger et préparer
  df <- load_and_prepare_data("votre_fichier.csv")
  
  # 2. Créer le dataset de rétention
  retention_data <- create_retention_dataset(df)
  
  # 3. Préparer les features
  prepared_data <- prepare_features(retention_data)
  
  # 4. Entraîner les modèles
  results <- train_models(prepared_data$X, prepared_data$y)
  
  # 5. Analyser l'importance des features
  importance <- analyze_feature_importance(results, prepared_data$X)
  
  # 6. Visualisations
  plot_results(results)
  
  # 7. Scorer tous les usagers
  scored_users <- score_users(prepared_data, results)
  
  # 8. Recommandations
  generate_recommendations(scored_users)
  
  # Retourner tous les résultats
  return(list(
    data = df,
    retention_data = retention_data,
    prepared_data = prepared_data,
    results = results,
    scored_users = scored_users
  ))
}

# EXÉCUTION
output <- main()

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ MODÈLE 1 TERMINÉ AVEC SUCCÈS !\n")
cat(rep("=", 80), "\n", sep = "")