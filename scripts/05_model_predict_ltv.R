################################################################################
# MODÈLE 3 : PRÉDICTION DE LA VALEUR LONG TERME (LTV - LIFETIME VALUE)
################################################################################
#
# Objectif : Identifier les usagers à fort potentiel dès leurs premières visites
#            pour prioriser les efforts d'acquisition et de fidélisation
#
# Variables utilisées :
# - Comportement lors des premières visites (durée, fréquence, espaces)
# - Diversité d'usage précoce
# - Engagement initial
# - Profil démographique
# - Patterns temporels
#
# LTV = Nombre total de visites sur toute la période d'observation
#
################################################################################

# Chargement des packages
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(gbm)
library(glmnet)      # Régression Ridge et Lasso
library(ggplot2)
library(gridExtra)
library(corrplot)    # Matrice de corrélation

set.seed(42)
options(scipen = 999)

cat("================================================================================\n")
cat("  MODÈLE 3 : PRÉDICTION DE LA VALEUR LONG TERME (LTV)\n")
cat("================================================================================\n\n")

################################################################################
# 1. CALCUL DE LA LTV ET DES FEATURES PRÉDICTIVES
################################################################################

calculate_ltv_features <- function(df, n_early_visits = 3) {
  cat("1. Calcul des features LTV basées sur les premières visites...\n")
  
  # Calcul de la durée si nécessaire
  if (!"duree_minutes" %in% names(df)) {
    df <- df %>%
      mutate(duree_minutes = as.numeric(difftime(date_sortie, date_arrivee, 
                                                 units = "mins")))
  }
  
  ltv_data <- df %>%
    arrange(id_usager, date_arrivee) %>%
    group_by(id_usager) %>%
    filter(n() >= n_early_visits) %>%  # Minimum de visites requis
    summarise(
      # === VARIABLE CIBLE : LTV (nombre total de visites) ===
      ltv_total_visits = n(),
      
      # === FEATURES BASÉES SUR LES N PREMIÈRES VISITES ===
      # Durée
      duree_moyenne_early = mean(head(duree_minutes, n_early_visits), na.rm = TRUE),
      duree_std_early = sd(head(duree_minutes, n_early_visits), na.rm = TRUE),
      duree_max_early = max(head(duree_minutes, n_early_visits), na.rm = TRUE),
      duree_min_early = min(head(duree_minutes, n_early_visits), na.rm = TRUE),
      
      # Fréquence initiale
      frequence_initiale = {
        early_dates <- head(date_arrivee, n_early_visits)
        jours <- as.numeric(difftime(max(early_dates), min(early_dates), 
                                     units = "days"))
        if (jours > 0) n_early_visits / jours else n_early_visits
      },
      
      # Diversité des espaces
      nb_espaces_early = n_distinct(head(espace, n_early_visits)),
      diversite_early = nb_espaces_early / n_early_visits,
      concentration_espace_early = {
        early_espaces <- head(espace, n_early_visits)
        max(table(early_espaces)) / n_early_visits
      },
      
      # Patterns temporels
      heure_moyenne_early = mean(hour(head(date_arrivee, n_early_visits))),
      variabilite_horaire_early = sd(hour(head(date_arrivee, n_early_visits))),
      pct_weekend_early = {
        early_days <- wday(head(date_arrivee, n_early_visits))
        sum(early_days %in% c(1, 7)) / n_early_visits
      },
      pct_matin_early = {
        early_hours <- hour(head(date_arrivee, n_early_visits))
        sum(early_hours >= 6 & early_hours < 12) / n_early_visits
      },
      
      # Tendance de la durée
      tendance_duree_early = {
        early_durations <- head(duree_minutes, n_early_visits)
        if (sum(!is.na(early_durations)) >= 2) {
          x <- 1:length(early_durations)
          coef(lm(early_durations ~ x, na.action = na.exclude))[2]
        } else {
          0
        }
      },
      
      # Régularité initiale
      regularite_early = {
        early_dates <- head(date_arrivee, n_early_visits)
        if (length(early_dates) > 1) {
          intervals <- as.numeric(diff(early_dates))
          sd(intervals, na.rm = TRUE)
        } else {
          0
        }
      },
      
      # Vitesse d'adoption (jours entre visite 1 et 3)
      vitesse_adoption = {
        if (n() >= n_early_visits) {
          as.numeric(difftime(date_arrivee[n_early_visits], 
                              date_arrivee[1], 
                              units = "days"))
        } else {
          NA
        }
      },
      
      # Engagement score initial (composite)
      engagement_score_early = {
        duree_norm <- (duree_moyenne_early / 300)  # Normalisation à 300 min
        freq_norm <- min(frequence_initiale * 10, 1)  # Cap à 1
        div_norm <- diversite_early
        
        (duree_norm * 0.4 + freq_norm * 0.3 + div_norm * 0.3)
      },
      
      # Croissance post-early period
      croissance_post_early = {
        if (n() > n_early_visits) {
          remaining <- n() - n_early_visits
          early_dates <- head(date_arrivee, n_early_visits)
          remaining_dates <- tail(date_arrivee, remaining)
          
          jours_post <- as.numeric(difftime(max(remaining_dates), 
                                            max(early_dates), 
                                            units = "days"))
          if (jours_post > 0) {
            remaining / jours_post  # Visites par jour post-early
          } else {
            0
          }
        } else {
          0
        }
      },
      
      # Démographiques
      age = first(age),
      genre = first(genre),
      ville = first(ville),
      
      # Ancienneté totale
      anciennete_jours = as.numeric(difftime(max(date_arrivee), 
                                             min(date_arrivee), 
                                             units = "days")),
      
      .groups = 'drop'
    )
  
  cat(sprintf("   - %d usagers analysés (avec ≥%d visites)\n", 
              nrow(ltv_data), n_early_visits))
  cat(sprintf("   - LTV moyenne : %.1f visites\n", mean(ltv_data$ltv_total_visits)))
  cat(sprintf("   - LTV médiane : %.0f visites\n", median(ltv_data$ltv_total_visits)))
  cat(sprintf("   - LTV max : %d visites\n", max(ltv_data$ltv_total_visits)))
  
  return(ltv_data)
}

prepare_ltv_data <- function(df_ltv) {
  cat("\n2. Préparation des données...\n")
  
  # Gestion des valeurs manquantes
  df_ltv <- df_ltv %>%
    mutate(
      across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))
    )
  
  # Encodage des variables catégorielles
  df_ltv <- df_ltv %>%
    mutate(
      genre_encoded = case_when(
        genre == "H" ~ 1,
        genre == "F" ~ 0,
        TRUE ~ -1
      )
    )
  
  # Top villes (regrouper les moins fréquentes)
  top_villes <- df_ltv %>%
    count(ville, sort = TRUE) %>%
    head(5) %>%
    pull(ville)
  
  df_ltv <- df_ltv %>%
    mutate(ville_top = ifelse(ville %in% top_villes, ville, "Autre"))
  
  # Variables dummy pour les villes
  ville_dummies <- model.matrix(~ ville_top - 1, data = df_ltv)
  
  # Features finales
  numeric_features <- c(
    "duree_moyenne_early", "duree_std_early", "duree_max_early", "duree_min_early",
    "frequence_initiale", "nb_espaces_early", "diversite_early",
    "concentration_espace_early", "heure_moyenne_early", "variabilite_horaire_early",
    "pct_weekend_early", "pct_matin_early", "tendance_duree_early",
    "regularite_early", "vitesse_adoption", "engagement_score_early",
    "croissance_post_early", "age", "genre_encoded", "anciennete_jours"
  )
  
  X <- cbind(
    df_ltv %>% select(all_of(numeric_features)),
    ville_dummies
  )
  
  y <- df_ltv$ltv_total_visits
  
  cat(sprintf("   - %d features créées\n", ncol(X)))
  cat(sprintf("   - Distribution LTV : Min=%d, Q1=%.0f, Médiane=%.0f, Q3=%.0f, Max=%d\n",
              min(y), quantile(y, 0.25), median(y), quantile(y, 0.75), max(y)))
  
  return(list(
    X = as.data.frame(X),
    y = y,
    feature_names = colnames(X),
    data = df_ltv
  ))
}

################################################################################
# 2. ENTRAÎNEMENT DES MODÈLES DE RÉGRESSION
################################################################################

train_ltv_models <- function(X, y) {
  cat("\n3. Entraînement des modèles de régression...\n")
  
  # Split train/test
  train_index <- createDataPartition(y, p = 0.75, list = FALSE)
  
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  cat(sprintf("   - Train : %d observations\n", length(y_train)))
  cat(sprintf("   - Test : %d observations\n", length(y_test)))
  
  # Configuration du contrôle
  train_control <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = FALSE
  )
  
  results <- list()
  
  # ============================================================================
  # MODÈLE 1 : RÉGRESSION LINÉAIRE MULTIPLE
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("1. RÉGRESSION LINÉAIRE MULTIPLE\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_lm <- train(
    x = X_train,
    y = y_train,
    method = "lm",
    trControl = train_control,
    preProcess = c("center", "scale")
  )
  
  pred_lm <- predict(model_lm, X_test)
  
  # Métriques
  rmse_lm <- sqrt(mean((y_test - pred_lm)^2))
  mae_lm <- mean(abs(y_test - pred_lm))
  r2_lm <- cor(y_test, pred_lm)^2
  
  cat(sprintf("\nRMSE : %.2f visites\n", rmse_lm))
  cat(sprintf("MAE  : %.2f visites\n", mae_lm))
  cat(sprintf("R²   : %.4f\n", r2_lm))
  
  results$lm <- list(
    model = model_lm,
    predictions = pred_lm,
    rmse = rmse_lm,
    mae = mae_lm,
    r2 = r2_lm
  )
  
  # ============================================================================
  # MODÈLE 2 : RÉGRESSION RIDGE
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("2. RÉGRESSION RIDGE (Régularisation L2)\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_ridge <- train(
    x = X_train,
    y = y_train,
    method = "ridge",
    trControl = train_control,
    preProcess = c("center", "scale"),
    tuneGrid = expand.grid(lambda = seq(0.001, 1, length.out = 20))
  )
  
  pred_ridge <- predict(model_ridge, X_test)
  
  rmse_ridge <- sqrt(mean((y_test - pred_ridge)^2))
  mae_ridge <- mean(abs(y_test - pred_ridge))
  r2_ridge <- cor(y_test, pred_ridge)^2
  
  cat(sprintf("\nRMSE : %.2f visites\n", rmse_ridge))
  cat(sprintf("MAE  : %.2f visites\n", mae_ridge))
  cat(sprintf("R²   : %.4f\n", r2_ridge))
  cat(sprintf("Lambda optimal : %.4f\n", model_ridge$bestTune$lambda))
  
  results$ridge <- list(
    model = model_ridge,
    predictions = pred_ridge,
    rmse = rmse_ridge,
    mae = mae_ridge,
    r2 = r2_ridge
  )
  
  # ============================================================================
  # MODÈLE 3 : RANDOM FOREST
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("3. RANDOM FOREST\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_rf <- train(
    x = X_train,
    y = y_train,
    method = "rf",
    trControl = train_control,
    ntree = 200,
    tuneGrid = expand.grid(mtry = c(5, 10, 15)),
    importance = TRUE
  )
  
  pred_rf <- predict(model_rf, X_test)
  
  rmse_rf <- sqrt(mean((y_test - pred_rf)^2))
  mae_rf <- mean(abs(y_test - pred_rf))
  r2_rf <- cor(y_test, pred_rf)^2
  
  cat(sprintf("\nRMSE : %.2f visites\n", rmse_rf))
  cat(sprintf("MAE  : %.2f visites\n", mae_rf))
  cat(sprintf("R²   : %.4f\n", r2_rf))
  
  results$rf <- list(
    model = model_rf,
    predictions = pred_rf,
    rmse = rmse_rf,
    mae = mae_rf,
    r2 = r2_rf
  )
  
  # ============================================================================
  # MODÈLE 4 : GRADIENT BOOSTING
  # ============================================================================
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("4. GRADIENT BOOSTING MACHINE (GBM)\n")
  cat(rep("=", 60), "\n", sep = "")
  
  model_gbm <- train(
    x = X_train,
    y = y_train,
    method = "gbm",
    trControl = train_control,
    verbose = FALSE,
    tuneGrid = expand.grid(
      n.trees = c(100, 150, 200),
      interaction.depth = c(3, 5),
      shrinkage = c(0.01, 0.05),
      n.minobsinnode = 10
    )
  )
  
  pred_gbm <- predict(model_gbm, X_test)
  
  rmse_gbm <- sqrt(mean((y_test - pred_gbm)^2))
  mae_gbm <- mean(abs(y_test - pred_gbm))
  r2_gbm <- cor(y_test, pred_gbm)^2
  
  cat(sprintf("\nRMSE : %.2f visites\n", rmse_gbm))
  cat(sprintf("MAE  : %.2f visites\n", mae_gbm))
  cat(sprintf("R²   : %.4f\n", r2_gbm))
  
  results$gbm <- list(
    model = model_gbm,
    predictions = pred_gbm,
    rmse = rmse_gbm,
    mae = mae_gbm,
    r2 = r2_gbm
  )
  
  # Sélection du meilleur modèle (R² le plus élevé)
  best_model_name <- names(which.max(sapply(results, function(x) x$r2)))
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat(sprintf("✅ MEILLEUR MODÈLE : %s\n", toupper(best_model_name)))
  cat(sprintf("   R² = %.4f | RMSE = %.2f | MAE = %.2f\n",
              results[[best_model_name]]$r2,
              results[[best_model_name]]$rmse,
              results[[best_model_name]]$mae))
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
      arrange(desc(`%IncMSE`)) %>%
      head(15)
    
    cat("\nTop 15 variables prédictives de la LTV :\n")
    print(importance_df %>% select(feature, `%IncMSE`, IncNodePurity))
    
    p <- ggplot(importance_df, aes(x = reorder(feature, `%IncMSE`), 
                                   y = `%IncMSE`)) +
      geom_bar(stat = "identity", fill = "#3498DB") +
      coord_flip() +
      labs(title = "Variables les Plus Prédictives de la LTV - Random Forest",
           x = "Variable", y = "% Augmentation MSE") +
      theme_minimal(base_size = 12)
    
  } else if (best_model_name == "gbm") {
    importance_df <- summary(best_model$finalModel, plotit = FALSE)
    importance_df <- importance_df %>% head(15)
    
    cat("\nTop 15 variables prédictives de la LTV :\n")
    print(importance_df)
    
    p <- ggplot(importance_df, aes(x = reorder(var, rel.inf), y = rel.inf)) +
      geom_bar(stat = "identity", fill = "#2ECC71") +
      coord_flip() +
      labs(title = "Variables les Plus Prédictives de la LTV - GBM",
           x = "Variable", y = "Influence Relative") +
      theme_minimal(base_size = 12)
    
  } else {
    # Pour modèles linéaires
    coef_df <- data.frame(
      feature = names(coef(best_model$finalModel))[-1],
      coefficient = abs(coef(best_model$finalModel)[-1])
    ) %>%
      arrange(desc(coefficient)) %>%
      head(15)
    
    cat("\nTop 15 variables prédictives de la LTV :\n")
    print(coef_df)
    
    p <- ggplot(coef_df, aes(x = reorder(feature, coefficient), y = coefficient)) +
      geom_bar(stat = "identity", fill = "#9B59B6") +
      coord_flip() +
      labs(title = sprintf("Variables les Plus Prédictives de la LTV - %s", 
                           toupper(best_model_name)),
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
  
  # 1. Comparaison des modèles
  comparison_df <- data.frame(
    model = c("LM", "Ridge", "RF", "GBM"),
    RMSE = c(results$lm$rmse, results$ridge$rmse, 
             results$rf$rmse, results$gbm$rmse),
    MAE = c(results$lm$mae, results$ridge$mae, 
            results$rf$mae, results$gbm$mae),
    R2 = c(results$lm$r2, results$ridge$r2, 
           results$rf$r2, results$gbm$r2)
  )
  
  p1 <- ggplot(comparison_df, aes(x = reorder(model, -R2), y = R2, fill = model)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.3f", R2)), vjust = -0.5) +
    labs(title = "Comparaison des Modèles - R²",
         x = "Modèle", y = "R² (Coefficient de Détermination)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none") +
    ylim(0, max(comparison_df$R2) * 1.1)
  
  p2 <- ggplot(comparison_df %>% 
                 pivot_longer(cols = c(RMSE, MAE), 
                              names_to = "metric", 
                              values_to = "value"),
               aes(x = reorder(model, value), y = value, fill = metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Comparaison des Erreurs - RMSE et MAE",
         x = "Modèle", y = "Erreur (nombre de visites)",
         fill = "Métrique") +
    theme_minimal(base_size = 12) +
    coord_flip()
  
  grid.arrange(p1, p2, ncol = 2,
               top = "Performance des Modèles de Prédiction LTV")
  
  # 2. Prédictions vs Réel
  best_model_name <- results$best_model
  
  pred_df <- data.frame(
    actual = results$y_test,
    predicted = results[[best_model_name]]$predictions
  )
  
  p_scatter <- ggplot(pred_df, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.5, color = "#3498DB") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
    labs(title = sprintf("LTV Prédite vs Réelle - %s", toupper(best_model_name)),
         x = "LTV Réelle (nombre de visites)",
         y = "LTV Prédite (nombre de visites)") +
    theme_minimal(base_size = 12) +
    annotate("text", x = max(pred_df$actual) * 0.7, y = max(pred_df$predicted) * 0.2,
             label = sprintf("R² = %.3f\nRMSE = %.2f", 
                             results[[best_model_name]]$r2,
                             results[[best_model_name]]$rmse),
             hjust = 0, size = 4)
  
  print(p_scatter)
  
  # 3. Distribution des erreurs
  pred_df$error <- pred_df$predicted - pred_df$actual
  
  p_error <- ggplot(pred_df, aes(x = error)) +
    geom_histogram(bins = 30, fill = "#E74C3C", alpha = 0.7) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    labs(title = "Distribution des Erreurs de Prédiction",
         x = "Erreur (Prédite - Réelle)",
         y = "Fréquence") +
    theme_minimal(base_size = 12)
  
  print(p_error)
}

################################################################################
# 5. SCORING ET SEGMENTATION
################################################################################

score_and_segment_ltv <- function(prepared_data, results) {
  cat("\n6. Scoring LTV de tous les usagers...\n")
  
  best_model_name <- results$best_model
  best_model <- results[[best_model_name]]$model
  
  # Prédictions sur tous les usagers
  ltv_predictions <- predict(best_model, prepared_data$X)
  
  scored_users <- prepared_data$data %>%
    mutate(
      ltv_predicted = ltv_predictions,
      ltv_segment = cut(ltv_predicted,
                        breaks = c(0, 5, 10, 20, Inf),
                        labels = c("Faible", "Moyen", "Élevé", "Très Élevé"),
                        include.lowest = TRUE),
      potentiel = case_when(
        ltv_predicted >= 20 ~ "VIP - Fort potentiel",
        ltv_predicted >= 10 ~ "Prometteur - Bon potentiel",
        ltv_predicted >= 5 ~ "Standard - Potentiel moyen",
        TRUE ~ "À risque - Faible potentiel"
      ),
      ecart_ltv = ltv_predicted - ltv_total_visits,
      sur_performeur = ecart_ltv < -3,  # Fait mieux que prévu
      sous_performeur = ecart_ltv > 3   # Fait moins bien que prévu
    )
  
  cat("\nDistribution des segments LTV :\n")
  print(table(scored_users$ltv_segment))
  
  cat("\nDistribution par potentiel :\n")
  print(table(scored_users$potentiel))
  
  # Statistiques par segment
  cat("\nStatistiques par segment :\n")
  stats <- scored_users %>%
    group_by(ltv_segment) %>%
    summarise(
      n = n(),
      ltv_pred_moyenne = mean(ltv_predicted),
      ltv_reelle_moyenne = mean(ltv_total_visits),
      ecart_moyen = mean(ecart_ltv),
      .groups = 'drop'
    )
  print(stats)
  
  return(scored_users)
}

################################################################################
# 6. RECOMMANDATIONS STRATÉGIQUES
################################################################################

generate_ltv_recommendations <- function(scored_users) {
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("RECOMMANDATIONS STRATÉGIQUES BASÉES SUR LA LTV\n")
  cat(rep("=", 80), "\n", sep = "")
  
  vip <- scored_users %>% filter(potentiel == "VIP - Fort potentiel")
  prometteur <- scored_users %>% filter(potentiel == "Prometteur - Bon potentiel")
  a_risque <- scored_users %>% filter(potentiel == "À risque - Faible potentiel")
  sur_perf <- scored_users %>% filter(sur_performeur)
  sous_perf <- scored_users %>% filter(sous_performeur)
  
  cat(sprintf("\n🌟 USAGERS VIP (LTV prédite ≥20) : %d (%.1f%%)\n",
              nrow(vip), nrow(vip)/nrow(scored_users)*100))
  cat("   Stratégie PREMIUM :\n")
  cat("   1. Programme de fidélité VIP avec avantages exclusifs\n")
  cat("   2. Account manager dédié\n")
  cat("   3. Événements exclusifs et networking\n")
  cat("   4. Early access aux nouveaux services\n")
  cat("   5. Tarifs préférentiels et packages personnalisés\n")
  
  cat(sprintf("\n💎 USAGERS PROMETTEURS (LTV 10-20) : %d (%.1f%%)\n",
              nrow(prometteur), nrow(prometteur)/nrow(scored_users)*100))
  cat("   Stratégie de MONTÉE EN GAMME :\n")
  cat("   1. Programme de découverte des services premium\n")
  cat("   2. Incentives pour augmenter la fréquence\n")
  cat("   3. Cross-selling ciblé\n")
  cat("   4. Invitations à des événements sélectionnés\n")
  
  cat(sprintf("\n⚠️  USAGERS À RISQUE (LTV <5) : %d (%.1f%%)\n",
              nrow(a_risque), nrow(a_risque)/nrow(scored_users)*100))
  cat("   Stratégie de CONVERSION :\n")
  cat("   1. Programme d'onboarding renforcé\n")
  cat("   2. Identification des barrières à l'usage\n")
  cat("   3. Offres découverte et essais gratuits\n")
  cat("   4. Suivi personnalisé intensif\n")
  
  if (nrow(sur_perf) > 0) {
    cat(sprintf("\n🚀 SUR-PERFORMEURS (dépassent les attentes) : %d\n", nrow(sur_perf)))
    cat("   Actions :\n")
    cat("   1. Analyser les facteurs de succès\n")
    cat("   2. Créer des cas d'usage inspirants\n")
    cat("   3. Programme ambassadeurs/référencement\n")
  }
  
  if (nrow(sous_perf) > 0) {
    cat(sprintf("\n📉 SOUS-PERFORMEURS (n'atteignent pas le potentiel) : %d\n", 
                nrow(sous_perf)))
    cat("   Actions :\n")
    cat("   1. Enquête qualitative pour identifier les freins\n")
    cat("   2. Plan d'action correctif personnalisé\n")
    cat("   3. Réengagement ciblé\n")
  }
  
  # Impact business
  cat("\n💰 IMPACT BUSINESS\n")
  ltv_totale_predite <- sum(scored_users$ltv_predicted)
  ltv_totale_actuelle <- sum(scored_users$ltv_total_visits)
  potentiel_croissance <- ltv_totale_predite - ltv_totale_actuelle
  
  cat(sprintf("   - LTV totale actuelle : %.0f visites\n", ltv_totale_actuelle))
  cat(sprintf("   - LTV totale prédite : %.0f visites\n", ltv_totale_predite))
  
  if (potentiel_croissance > 0) {
    cat(sprintf("   - Potentiel de croissance : +%.0f visites (%.1f%%)\n",
                potentiel_croissance,
                (potentiel_croissance / ltv_totale_actuelle) * 100))
  }
  
  # Priorités d'investissement
  cat("\n🎯 PRIORISATION DES INVESTISSEMENTS\n")
  cat("   1. FORTE PRIORITÉ : VIP (ROI maximum)\n")
  cat("   2. MOYENNE PRIORITÉ : Prometteurs (potentiel de conversion)\n")
  cat("   3. ANALYSE COÛT/BÉNÉFICE : À risque (évaluer la rentabilité)\n")
  
  # Stratégie d'acquisition
  cat("\n📊 STRATÉGIE D'ACQUISITION\n")
  cat("   Profil idéal à cibler (basé sur les VIP actuels) :\n")
  
  vip_profile <- scored_users %>%
    filter(potentiel == "VIP - Fort potentiel") %>%
    summarise(
      duree_moyenne = mean(duree_moyenne_early, na.rm = TRUE),
      freq_moyenne = mean(frequence_initiale, na.rm = TRUE),
      diversite_moyenne = mean(diversite_early, na.rm = TRUE),
      engagement_moyen = mean(engagement_score_early, na.rm = TRUE)
    )
  
  if (nrow(vip_profile) > 0) {
    cat(sprintf("   - Durée 1ères visites : %.0f minutes\n", 
                vip_profile$duree_moyenne))
    cat(sprintf("   - Fréquence initiale : %.2f visites/jour\n", 
                vip_profile$freq_moyenne))
    cat(sprintf("   - Diversité espaces : %.1f%%\n", 
                vip_profile$diversite_moyenne * 100))
    cat(sprintf("   - Score engagement : %.2f\n", 
                vip_profile$engagement_moyen))
  }
  
  cat("\n✅ MODÈLE OPÉRATIONNEL\n")
  cat("   Utilisation recommandée :\n")
  cat("   - Scoring des nouveaux usagers après 3 visites\n")
  cat("   - Réévaluation trimestrielle des segments\n")
  cat("   - Adaptation des stratégies par segment\n")
  cat("   - Suivi des KPIs : conversion, rétention par segment\n")
}

################################################################################
# 7. ANALYSE COMPLÉMENTAIRE : EARLY INDICATORS
################################################################################

analyze_early_indicators <- function(scored_users) {
  cat("\n7. Analyse des indicateurs précoces de succès...\n")
  
  # Corrélations entre features early et LTV
  correlation_data <- scored_users %>%
    select(ltv_total_visits, ltv_predicted,
           duree_moyenne_early, frequence_initiale, 
           diversite_early, engagement_score_early,
           regularite_early, tendance_duree_early) %>%
    na.omit()
  
  if (nrow(correlation_data) > 0) {
    cor_matrix <- cor(correlation_data)
    
    # Visualisation de la matrice de corrélation
    corrplot(cor_matrix, method = "color", type = "upper",
             tl.col = "black", tl.srt = 45,
             title = "Corrélations entre Features Early et LTV",
             mar = c(0, 0, 2, 0))
    
    # Top corrélations avec LTV
    ltv_correlations <- data.frame(
      feature = rownames(cor_matrix),
      correlation = cor_matrix[, "ltv_total_visits"]
    ) %>%
      filter(feature != "ltv_total_visits" & feature != "ltv_predicted") %>%
      arrange(desc(abs(correlation)))
    
    cat("\nCorrélations avec la LTV réelle :\n")
    print(ltv_correlations)
    
    # Segmentation par quartiles de features importantes
    top_feature <- ltv_correlations$feature[1]
    
    quartile_analysis <- scored_users %>%
      mutate(quartile = ntile(!!sym(top_feature), 4)) %>%
      group_by(quartile) %>%
      summarise(
        n = n(),
        ltv_moyenne = mean(ltv_total_visits),
        ltv_pred_moyenne = mean(ltv_predicted),
        .groups = 'drop'
      )
    
    cat(sprintf("\nAnalyse par quartiles de '%s' :\n", top_feature))
    print(quartile_analysis)
    
    # Visualisation
    p <- ggplot(quartile_analysis, aes(x = factor(quartile), y = ltv_moyenne)) +
      geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.7) +
      geom_line(aes(y = ltv_pred_moyenne, group = 1), 
                color = "#E74C3C", size = 1.5) +
      geom_point(aes(y = ltv_pred_moyenne), color = "#E74C3C", size = 3) +
      labs(title = sprintf("Impact de '%s' sur la LTV", top_feature),
           x = "Quartile (1=Faible, 4=Élevé)",
           y = "LTV Moyenne (nombre de visites)") +
      theme_minimal(base_size = 12) +
      annotate("text", x = 3.5, y = max(quartile_analysis$ltv_moyenne) * 0.9,
               label = "Barres = LTV Réelle\nLigne = LTV Prédite",
               color = "darkgray", hjust = 1)
    
    print(p)
  }
}

################################################################################
# 8. EXÉCUTION PRINCIPALE
################################################################################

main <- function() {
  
  # Charger les données (remplacer par votre fichier)
  # df <- read.csv("votre_fichier.csv", stringsAsFactors = FALSE)
  # df$date_arrivee <- as.POSIXct(df$date_arrivee)
  # df$date_sortie <- as.POSIXct(df$date_sortie)
  
  # Pour l'exemple, données synthétiques
  set.seed(42)
  n_users <- 3540
  
  # Générer des visites avec distribution réaliste
  user_visits <- map_dfr(1:n_users, function(user_id) {
    # LTV suit une distribution log-normale (quelques VIP, beaucoup de standard)
    ltv <- round(rlnorm(1, meanlog = 1.5, sdlog = 0.8))
    ltv <- max(3, min(ltv, 50))  # Entre 3 et 50 visites
    
    data.frame(
      id_usager = user_id,
      visit_num = 1:ltv
    )
  })
  
  df <- user_visits %>%
    group_by(id_usager) %>%
    mutate(
      # Les premiers usagers sont plus "engagés" (durées plus longues)
      engagement_factor = if_else(visit_num <= 3, 1.2, 1.0),
      
      date_arrivee = as.POSIXct("2024-01-01") + 
        hours(cumsum(sample(24:168, n(), replace = TRUE))),
      duree_minutes = pmax(30, rnorm(n(), 280 * engagement_factor, 80)),
      espace = sample(c("Rooftop", "Salle A", "Salle B", "Studio", "Cafétéria"), 
                      n(), replace = TRUE),
      age = round(rnorm(n(), 25, 7)),
      age = pmax(18, pmin(60, age)),
      genre = sample(c("H", "F"), n(), replace = TRUE, prob = c(0.7, 0.3)),
      ville = sample(c("Cotonou", "Porto-Novo", "Parakou", "Abomey"), 
                     n(), replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1))
    ) %>%
    select(-engagement_factor) %>%
    ungroup()
  
  # 1. Calculer les features LTV
  ltv_features <- calculate_ltv_features(df, n_early_visits = 3)
  
  # 2. Préparer les données
  prepared_data <- prepare_ltv_data(ltv_features)
  
  # 3. Entraîner les modèles
  results <- train_ltv_models(prepared_data$X, prepared_data$y)
  
  # 4. Analyser l'importance des features
  importance <- analyze_feature_importance(results, prepared_data$feature_names)
  
  # 5. Visualisations
  plot_comprehensive_results(results)
  
  # 6. Scorer et segmenter
  scored_users <- score_and_segment_ltv(prepared_data, results)
  
  # 7. Analyse des indicateurs précoces
  analyze_early_indicators(scored_users)
  
  # 8. Recommandations
  generate_ltv_recommendations(scored_users)
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("✅ MODÈLE 3 TERMINÉ AVEC SUCCÈS !\n")
  cat(rep("=", 80), "\n", sep = "")
  
  # Résumé final
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("RÉSUMÉ DES 3 MODÈLES DE PRÉDICTION\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n📊 MODÈLE 1 : RÉTENTION\n")
  cat("   → Identifie les usagers qui reviendront après leur 1ère visite\n")
  cat("   → Action : Programme d'onboarding ciblé\n")
  cat("\n🚨 MODÈLE 2 : CHURN\n")
  cat("   → Détecte les signaux d'abandon précoces\n")
  cat("   → Action : Interventions de réengagement\n")
  cat("\n💎 MODÈLE 3 : LTV (LIFETIME VALUE)\n")
  cat("   → Prédit la valeur long terme dès les premières visites\n")
  cat("   → Action : Priorisation des investissements\n")
  cat("\n✅ UTILISATION CONJOINTE RECOMMANDÉE\n")
  cat("   1. Scorer les nouveaux usagers avec MODÈLE 1 (rétention)\n")
  cat("   2. Après 3 visites : MODÈLE 3 (LTV) pour segmenter\n")
  cat("   3. Monitoring continu avec MODÈLE 2 (churn)\n")
  cat("   4. Adapter les stratégies selon les segments\n")
  
  cat("\n", rep("=", 80), "\n", sep = "")
  
  return(list(
    data = df,
    ltv_features = ltv_features,
    prepared_data = prepared_data,
    results = results,
    scored_users = scored_users,
    importance = importance
  ))
}

# EXÉCUTION
output <- main()

cat("\n🎉 TOUS LES MODÈLES SONT OPÉRATIONNELS !\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n")