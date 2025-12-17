################################################################################
#                                                                              #
#           OBJECTIF 2 : IDENTIFICATION DES FACTEURS D'INFLUENCE               #
#          Analyse des Leviers d'Amélioration de la Fréquentation              #
#                                                                              #
################################################################################

# =============================================================================
# PARTIE 1 : CONFIGURATION ET CHARGEMENT
# =============================================================================

cat("🎯 OBJECTIF 2 : IDENTIFICATION DES FACTEURS D'INFLUENCE\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# # Packages nécessaires
# if (!require("pacman")) install.packages("pacman")
# 
# pacman::p_load(
#   tidyverse, lubridate, data.table,
#   ggplot2, plotly, patchwork, scales, corrplot,
#   caret, randomForest, xgboost,
#   pdp,        # Partial Dependence Plots
#   iml,        # Interpretable Machine Learning
#   DALEX,      # Model explainability
#   gridExtra, ggridges
# )
# 
# 
# # Résoudre le conflit de noms
# conflicts_prefer(pdp::partial)

options(scipen = 999, digits = 4)
set.seed(2025)


# Charger le dataset enrichi de l'objectif 1
# Si vous venez de terminer l'objectif 1, data_model existe déjà

# data_model <- read.csv("outputs/tables/data_frequentation_daily_engineered.csv")

cat("✓ Configuration terminée\n\n")


# =============================================================================
# PARTIE 2 : PRÉPARATION DES DONNÉES DÉTAILLÉES PAR VISITE
# =============================================================================

cat("📊 Préparation des données détaillées par visite...\n")

# Enrichir data_frequentation avec variables temporelles et comportementales
data_visites_enrichi <- data_frequentation %>%
  mutate(
    visit_date = as.Date(visit_date),
    
    # Variables temporelles
    annee = year(visit_date),
    mois = month(visit_date),
    mois_label = month(visit_date, label = TRUE),
    jour_semaine = wday(visit_date, label = TRUE),
    jour_semaine_num = wday(visit_date),
    est_weekend = jour_semaine_num %in% c(1, 7),
    semaine = week(visit_date),
    trimestre = quarter(visit_date),
    
    # Variables horaires
    heure_arrivee = as.numeric(arrival_time) / 3600,
    heure_depart = as.numeric(departure_time) / 3600,
    tranche_horaire = case_when(
      heure_arrivee < 9 ~ "Avant 9h",
      heure_arrivee >= 9 & heure_arrivee < 12 ~ "9h-12h",
      heure_arrivee >= 12 & heure_arrivee < 14 ~ "12h-14h",
      heure_arrivee >= 14 & heure_arrivee < 18 ~ "14h-18h",
      heure_arrivee >= 18 ~ "Après 18h"
    ),
    
    # Variables de durée
    duree_categorie = case_when(
      duration_minutes < 60 ~ "Courte (<1h)",
      duration_minutes >= 60 & duration_minutes < 180 ~ "Moyenne (1-3h)",
      duration_minutes >= 180 & duration_minutes < 360 ~ "Longue (3-6h)",
      duration_minutes >= 360 ~ "Très longue (6h+)"
    ),
    
    # Variables démographiques
    age_categorie = case_when(
      age < 20 ~ "Moins de 20 ans",
      age >= 20 & age < 25 ~ "20-24 ans",
      age >= 25 & age < 30 ~ "25-29 ans",
      age >= 30 & age < 40 ~ "30-39 ans",
      age >= 40 ~ "40 ans et plus"
    ),
    
    # Séniorité
    est_nouveau = seniority_days == 0,
    anciennete_categorie = case_when(
      seniority_days == 0 ~ "Nouveau",
      seniority_days <= 7 ~ "Récent (1 semaine)",
      seniority_days <= 30 ~ "Moyen (1 mois)",
      seniority_days > 30 ~ "Ancien (1+ mois)"
    )
  )

cat("✓ Données enrichies\n")
cat(paste("  - Nombre de visites :", nrow(data_visites_enrichi), "\n\n"))


# =============================================================================
# PARTIE 3 : ANALYSE IMPACT DES FACTEURS TEMPORELS
# =============================================================================

cat("📅 ANALYSE DES FACTEURS TEMPORELS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# 3.1 Impact du jour de la semaine sur la fréquentation (nombre de visite et durée des visites)
impact_jour_semaine <- data_visites_enrichi %>%
  group_by(jour_semaine) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_visites = nb_visites / sum(nb_visites) * 100,
    indice_vs_moyenne = nb_visites / mean(nb_visites) * 100
  ) %>%
  arrange(desc(nb_visites))

cat("\n🔍 IMPACT DU JOUR DE LA SEMAINE:\n")
print(impact_jour_semaine)


# Visualisation
p_jour_semaine <- ggplot(impact_jour_semaine, 
                         aes(x = reorder(jour_semaine, nb_visites), 
                             y = nb_visites, fill = jour_semaine)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(nb_visites, "\n(", round(pct_visites, 1), "%)")), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Impact du Jour de la Semaine sur la Fréquentation",
    subtitle = paste("Écart min-max :", 
                     round((max(impact_jour_semaine$nb_visites) - 
                              min(impact_jour_semaine$nb_visites)) / 
                             mean(impact_jour_semaine$nb_visites) * 100, 1), "%"),
    x = "Jour de la Semaine",
    y = "Nombre de Visites"
  ) +
  coord_flip() +
  theme(legend.position = "none")

ggsave("outputs/figures/49_impact_jour_semaine_frequentation.png", p_jour_semaine, width = 15, height = 6, dpi = 300)


# Test statistique (ANOVA)
# La durée des visites varie-t-elle vraiment selon le jour de la semaine, ou est-ce juste le hasard ?
anova_jour <- aov(duration_minutes ~ jour_semaine, data = data_visites_enrichi)
cat("\nTest ANOVA - Durée de la visite vs Jour de semaine:\n")
print(summary(anova_jour))


# 3.2 Impact de la tranche horaire sur le nombre de visite
impact_horaire <- data_visites_enrichi %>%
  group_by(tranche_horaire) %>%
  summarise(
    nb_visites = n(),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100) %>%
  arrange(desc(nb_visites))

cat("\n🕐 IMPACT DE LA TRANCHE HORAIRE:\n")
print(impact_horaire)

p_horaire <- ggplot(data_visites_enrichi, aes(x = heure_arrivee)) +
  geom_histogram(bins = 24, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = c(9, 12, 14, 18), linetype = "dashed", color = "red") +
  labs(
    title = "Distribution des Heures d'Arrivée",
    x = "Heure d'Arrivée",
    y = "Nombre de Visites"
  ) +
  scale_x_continuous(breaks = seq(0, 24, 2))

ggsave("outputs/figures/50_impact_tranche_horaire_frequentation.png", p_horaire, width = 15, height = 6, dpi = 300)

print(p_horaire)


# 3.3 Impact du mois sur le nombre de visite
impact_mois <- data_visites_enrichi %>%
  group_by(mois_label) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100)

cat("\n📆 IMPACT DU MOIS:\n")
print(impact_mois)


p_mois <- ggplot(impact_mois, aes(x = mois_label, y = nb_visites, fill = mois_label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = nb_visites), vjust = -0.5) +
  labs(
    title = "Saisonnalité Mensuelle de la Fréquentation",
    x = "Mois",
    y = "Nombre de Visites"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/51_impact_mois_frequentation.png", p_mois, width = 15, height = 6, dpi = 300)



# 3.4 Impact weekend vs semaine
impact_weekend <- data_visites_enrichi %>%
  mutate(periode = ifelse(est_weekend, "Weekend", "Semaine")) %>%
  group_by(periode) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100)

cat("\n🏖️ IMPACT WEEKEND VS SEMAINE:\n")
print(impact_weekend)

# Calcul de l'écart
ecart_weekend <- (impact_weekend$nb_visites[impact_weekend$periode == "Weekend"] / 
                    impact_weekend$nb_visites[impact_weekend$periode == "Semaine"] - 1) * 100

cat(paste("\n⚡ La semaine génère", round(abs(ecart_weekend), 1), 
          "% de visites de plus que le weekend\n\n"))


# =============================================================================
# PARTIE 4 : ANALYSE D'IMPACT DES ESPACES
# =============================================================================

cat("🏢 ANALYSE DES ESPACES VISITÉS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Impact par espace
impact_espaces <- data_visites_enrichi %>%
  group_by(visited_space) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    age_moyen = mean(age, na.rm = TRUE),
    pct_femmes = mean(sex == "Feminin", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    pct_visites = nb_visites / sum(nb_visites) * 100,
    taux_fidelite = nb_visites / nb_visiteurs_uniques
  ) %>%
  arrange(desc(nb_visites))

cat("\n🏆 TOP 10 ESPACES LES PLUS VISITÉS:\n")
print(head(impact_espaces, 10))

# Visualisation Top 15
p_espaces <- ggplot(head(impact_espaces, 15), 
                    aes(x = reorder(visited_space, nb_visites), 
                        y = nb_visites, fill = nb_visites)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = nb_visites), hjust = -0.2, size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Top 15 des Espaces les Plus Fréquentés",
    x = "Espace",
    y = "Nombre de Visites"
  ) +
  coord_flip() +
  theme(legend.position = "none")

ggsave("outputs/figures/51_top_15_espaces_frequentes.png", p_espaces, width = 15, height = 6, dpi = 300)


# Analyse des espaces sous-utilisés
espaces_sous_utilises <- impact_espaces %>%
  filter(nb_visites < 50) %>%
  arrange(nb_visites)

cat("\n⚠️ ESPACES SOUS-UTILISÉS (<50 visites):\n")
print(espaces_sous_utilises)

cat(paste("\n💡 Opportunité : ", nrow(espaces_sous_utilises), 
          "espaces à promouvoir ou réaffecter\n\n"))


# =============================================================================
# PARTIE 5 : ANALYSE D'IMPACT DES MOTIFS DE VISITE
# =============================================================================

cat("💼 ANALYSE DES MOTIFS DE VISITE\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Impact par motif
impact_motifs <- data_visites_enrichi %>%
  group_by(visit_reason) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100) %>%
  arrange(desc(nb_visites))

cat("\n🎯 TOP 15 MOTIFS DE VISITE:\n")
print(head(impact_motifs, 15))

# Visualisation
p_motifs <- ggplot(head(impact_motifs, 10), 
                   aes(x = reorder(visit_reason, nb_visites), 
                       y = nb_visites)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = paste0(nb_visites, "\n(", round(pct_visites, 1), "%)")), 
            hjust = -0.2, size = 3) +
  labs(
    title = "Top 10 des Motifs de Visite",
    x = "Motif",
    y = "Nombre de Visites"
  ) +
  coord_flip()

ggsave("outputs/figures/52_impact_motif_frequentation.png", p_motifs, width = 15, height = 6, dpi = 300)



# =============================================================================
# PARTIE 6 : ANALYSE D'IMPACT DES CARACTÉRISTIQUES DÉMOGRAPHIQUES
# =============================================================================

cat("👥 ANALYSE DES FACTEURS DÉMOGRAPHIQUES\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# 6.1 Impact de l'âge
impact_age <- data_visites_enrichi %>%
  group_by(age_categorie) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100) %>%
  arrange(desc(nb_visites))

cat("\n👶 IMPACT DE L'ÂGE:\n")
print(impact_age)

p_age <- ggplot(impact_age, aes(x = age_categorie, y = nb_visites, fill = age_categorie)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct_visites, 1), "%")), vjust = -0.5) +
  labs(
    title = "Répartition des Visites par Tranche d'Âge",
    x = "Tranche d'Âge",
    y = "Nombre de Visites"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/52_impact_age_frequentation.png", p_age, width = 15, height = 6, dpi = 300)


# 6.2 Impact du sexe
impact_sexe <- data_visites_enrichi %>%
  group_by(sex) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100)

cat("\n⚥ IMPACT DU SEXE:\n")
print(impact_sexe)


# Test statistique (test t de Student )
t_test_sexe <- t.test(duration_minutes ~ sex, data = data_visites_enrichi)
cat("\nTest t - Différence de durée selon le sexe:\n")
cat(paste("  p-value:", round(t_test_sexe$p.value, 4), "\n"))
if(t_test_sexe$p.value < 0.05) {
  cat("  ✓ Différence significative\n")
} else {
  cat("  ✗ Pas de différence significative\n")
}

print(t_test_sexe)

# 6.3 Impact de la ville
impact_ville <- data_visites_enrichi %>%
  group_by(city) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100) %>%
  arrange(desc(nb_visites))

cat("\n🌍 TOP 10 VILLES D'ORIGINE:\n")
print(head(impact_ville, 10))


# Concentration géographique
concentration_top5 <- sum(head(impact_ville, 5)$pct_visites)
cat(paste("\n📍 Les 5 premières villes représentent", 
          round(concentration_top5, 1), "% des visites\n\n"))


# =============================================================================
# PARTIE 7 : ANALYSE D'IMPACT DE L'ANCIENNETÉ
# =============================================================================

cat("⏳ ANALYSE DE L'ANCIENNETÉ\n")
cat("────────────────────────────────────────────────────────────────────────\n")

impact_anciennete <- data_visites_enrichi %>%
  group_by(anciennete_categorie) %>%
  summarise(
    nb_visites = n(),
    nb_visiteurs_uniques = n_distinct(phone),
    duree_moyenne = mean(duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_visites = nb_visites / sum(nb_visites) * 100) %>%
  arrange(match(anciennete_categorie, c("Nouveau", "Récent (1 semaine)", 
                                        "Moyen (1 mois)", "Ancien (1+ mois)")))

cat("\n🆕 IMPACT DE L'ANCIENNETÉ:\n")
print(impact_anciennete)

# Part des nouveaux
pct_nouveaux <- impact_anciennete$pct_visites[impact_anciennete$anciennete_categorie == "Nouveau"]
cat(paste("\n💡", round(pct_nouveaux, 1), 
          "% des visites sont effectuées par des nouveaux usagers\n\n"))



# Ultra importance capitale

# =============================================================================
# PARTIE 8 : MODÉLISATION DE L'IMPORTANCE DES VARIABLES (ML)
# =============================================================================


cat("🤖 MODÉLISATION - IMPORTANCE DES VARIABLES\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Agrégation quotidienne pour la modélisation
if(!exists("data_model")) {
  # Si data_model n'existe pas depuis l'objectif 1, on le recrée
  data_model <- data_daily %>%
    filter(!is.na(nb_visiteurs_lag7))
}

# Sélection des features pour l'analyse d'importance
features_importance <- c(
  "jour_semaine_num", "mois", "est_weekend", "est_debut_mois", "est_fin_mois",
  "nb_visiteurs_lag1", "nb_visiteurs_lag7", "nb_visiteurs_ma7", 
  "nb_visiteurs_ma14", "nb_visiteurs_sd7",
  "pct_femmes", "age_moyen", "duree_moyenne_minutes",
  "nb_espaces_actifs", "nb_nouveaux_usagers", "heure_arrivee_moyenne"
)

# Filtrer les colonnes disponibles
features_disponibles <- features_importance[features_importance %in% names(data_model)]

data_ml <- data_model %>%
  select(all_of(c("nb_visiteurs", features_disponibles))) %>%
  na.omit()

cat(paste("\n📊 Features utilisées:", length(features_disponibles), "\n"))
cat(paste("📊 Observations:", nrow(data_ml), "\n\n"))

# Random Forest pour l'importance
cat("Entraînement Random Forest...\n")
rf_importance <- randomForest(
  nb_visiteurs ~ .,
  data = data_ml,
  ntree = 500,
  importance = TRUE,
  na.action = na.omit
)

# Extraire l'importance
importance_rf <- importance(rf_importance) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  arrange(desc(`%IncMSE`)) %>%
  mutate(
    Importance_Relative = `%IncMSE` / sum(`%IncMSE`) * 100,
    Categorie = case_when(
      str_detect(Variable, "jour|weekend|mois|debut|fin") ~ "Temporel",
      str_detect(Variable, "lag|ma|sd") ~ "Historique",
      str_detect(Variable, "femmes|age") ~ "Démographique",
      str_detect(Variable, "espace|nouveau|heure|duree") ~ "Comportemental",
      TRUE ~ "Autre"
    )
  )

cat("\n🏆 TOP 15 VARIABLES LES PLUS IMPORTANTES:\n")
print(head(importance_rf, 15))

# Visualisation
p_importance <- ggplot(head(importance_rf, 15), 
                       aes(x = reorder(Variable, `%IncMSE`), 
                           y = `%IncMSE`, fill = Categorie)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Importance des Variables dans la Prédiction de Fréquentation",
    subtitle = "Basé sur Random Forest - %IncMSE",
    x = "Variable",
    y = "Importance (%IncMSE)"
  ) +
  coord_flip() +
  theme(legend.position = "bottom")

ggsave("outputs/figures/53_top_15_variables_dimportance_frequentation.png", p_importance, width = 15, height = 6, dpi = 300)


# Importance par catégorie
importance_categorie <- importance_rf %>%
  group_by(Categorie) %>%
  summarise(
    Importance_Totale = sum(`%IncMSE`),
    Nb_Variables = n(),
    .groups = "drop"
  ) %>%
  mutate(Pct = Importance_Totale / sum(Importance_Totale) * 100) %>%
  arrange(desc(Importance_Totale))

cat("\n📊 IMPORTANCE PAR CATÉGORIE DE FACTEURS:\n")
print(importance_categorie)

p_categorie <- ggplot(importance_categorie, 
                      aes(x = reorder(Categorie, Pct), y = Pct, fill = Categorie)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Pct, 1), "%")), hjust = -0.2) +
  labs(
    title = "Contribution par Catégorie de Facteurs",
    x = "Catégorie",
    y = "Contribution (%)"
  ) +
  coord_flip() +
  theme(legend.position = "none")

ggsave("outputs/figures/54_contribution_par_categorie_de_facteur.png", p_categorie, width = 15, height = 6, dpi = 300)


# =============================================================================
# PARTIE 9 : PARTIAL DEPENDENCE PLOTS (PDP) - VERSION AMÉLIORÉE
# =============================================================================
cat("\n📈 PARTIAL DEPENDENCE PLOTS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Créer dossier si nécessaire
if(!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}

# Sélectionner top 4 variables
top_vars <- head(importance_rf$Variable, 4)
cat(paste("Variables analysées:", paste(top_vars, collapse = ", "), "\n\n"))

# Générer les PDP
pdp_plots <- list()

for(i in 1:length(top_vars)) {
  var <- top_vars[i]
  
  cat(paste("   Génération PDP pour:", var, "..."))
  
  if(var %in% names(data_ml)) {
    # Calculer PDP
    pdp_data <- partial(rf_importance, pred.var = var, train = data_ml)
    
    # Créer graphique
    pdp_plots[[i]] <- autoplot(pdp_data) +
      labs(
        title = paste("Impact de", var),
        y = "Prédiction Marginale (minutes)",
        x = var
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
      )
    
    # Sauvegarder individuellement
    ggsave(
      filename = paste0("outputs/figures/pdp_", gsub("[^[:alnum:]]", "_", var), ".png"),
      plot = pdp_plots[[i]],
      width = 15,
      height = 6,
      dpi = 300
    )
    
    cat(" ✓\n")
  } else {
    cat(" ⚠️  Variable non trouvée\n")
  }
}

# Afficher et sauvegarder la grille complète
if(length(pdp_plots) > 0) {
  cat("\n📊 Affichage de la grille PDP...\n")
  
  # Afficher
  grid.arrange(grobs = pdp_plots, ncol = 2)
  
  # Sauvegarder grille
  g <- arrangeGrob(grobs = pdp_plots, ncol = 2)
  ggsave(
    filename = "outputs/figures/pdp_grid_top4.png",
    plot = g,
    width = 14,
    height = 12,
    dpi = 300
  )
  
  cat("✓ Fichiers sauvegardés:\n")
  cat("   - outputs/figures/pdp_grid_top4.png (grille complète)\n")
  for(var in top_vars) {
    cat(paste0("   - outputs/figures/pdp_", gsub("[^[:alnum:]]", "_", var), ".png\n"))
  }
  cat("\n")
}


# =============================================================================
# PARTIE 10 : ANALYSE DES INTERACTIONS
# =============================================================================

cat("\n🔗 ANALYSE DES INTERACTIONS ENTRE FACTEURS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Interaction Jour x Weekend
interaction_jour_weekend <- data_visites_enrichi %>%
  mutate(periode = ifelse(est_weekend, "Weekend", "Semaine")) %>%
  group_by(jour_semaine, periode) %>%
  summarise(
    nb_visites = n(),
    .groups = "drop"
  )

p_interaction1 <- ggplot(interaction_jour_weekend, 
                         aes(x = jour_semaine, y = nb_visites, fill = periode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Interaction : Jour de la Semaine x Période",
    x = "Jour",
    y = "Nombre de Visites"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/55_interaction_jour_weekend_frequentation.png", p_interaction1, width = 15, height = 6, dpi = 300)


# Interaction Tranche Horaire x Jour Semaine
interaction_heure_jour <- data_visites_enrichi %>%
  filter(!is.na(tranche_horaire)) %>%
  group_by(tranche_horaire, jour_semaine) %>%
  summarise(nb_visites = n(), .groups = "drop")

p_interaction2 <- ggplot(interaction_heure_jour, 
                         aes(x = jour_semaine, y = tranche_horaire, fill = nb_visites)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(
    title = "Heatmap : Tranche Horaire x Jour de la Semaine",
    x = "Jour",
    y = "Tranche Horaire",
    fill = "Nb Visites"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/56_heatmap_tranche_horaire_jour_frequentation.png", p_interaction2, width = 15, height = 6, dpi = 300)



# Interaction Espace x Jour
top_espaces <- head(impact_espaces$visited_space, 5)
interaction_espace_jour <- data_visites_enrichi %>%
  filter(visited_space %in% top_espaces) %>%
  group_by(visited_space, jour_semaine) %>%
  summarise(nb_visites = n(), .groups = "drop")

p_interaction3 <- ggplot(interaction_espace_jour, 
                         aes(x = jour_semaine, y = nb_visites, 
                             color = visited_space, group = visited_space)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Patterns Hebdomadaires des Top 5 Espaces",
    x = "Jour de la Semaine",
    y = "Nombre de Visites",
    color = "Espace"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

ggsave("outputs/figures/57_interaction_espace_jour_frequentation.png", p_interaction3, width = 15, height = 6, dpi = 300)


print(p_interaction3)



# =============================================================================
# PARTIE 11 : MATRICE DE CORRÉLATIONS
# =============================================================================

cat("\n🔢 MATRICE DE CORRÉLATIONS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Sélectionner les variables numériques
vars_numeriques <- data_ml %>%
  select(where(is.numeric)) %>%
  select(-nb_visiteurs)  # Exclure la cible

# Calculer corrélations avec la cible
correlations_cible <- cor(vars_numeriques, data_ml$nb_visiteurs, use = "complete.obs")
correlations_df <- data.frame(
  Variable = rownames(correlations_cible),
  Correlation = correlations_cible[,1]
) %>%
  arrange(desc(abs(Correlation)))

cat("\n📊 TOP 10 CORRÉLATIONS AVEC LA FRÉQUENTATION:\n")
print(head(correlations_df, 10))

# Visualisation
p_corr <- ggplot(head(correlations_df, 15), 
                 aes(x = reorder(Variable, abs(Correlation)), 
                     y = Correlation, fill = Correlation > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "green"), labels = c("Négative", "Positive")) +
  labs(
    title = "Corrélations avec la Fréquentation",
    x = "Variable",
    y = "Coefficient de Corrélation",
    fill = "Type"
  ) +
  coord_flip() +
  theme(legend.position = "bottom")

ggsave("outputs/figures/58_matrice_corr_variables_numeriques_frequentation.png", p_corr, width = 15, height = 6, dpi = 300)



# =============================================================================
# PARTIE 12 : ANALYSE DE SENSIBILITÉ
# =============================================================================

cat("\n⚡ ANALYSE DE SENSIBILITÉ\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Calculer l'élasticité de chaque facteur
cat("Calcul des élasticités...\n\n")

# Élasticité du jour de la semaine
elasticite_jour <- impact_jour_semaine %>%
  mutate(
    elasticite = (nb_visites - mean(nb_visites)) / mean(nb_visites) * 100
  ) %>%
  select(jour_semaine, nb_visites, elasticite) %>%
  arrange(desc(elasticite))

cat("📊 ÉLASTICITÉ PAR JOUR DE LA SEMAINE:\n")
print(elasticite_jour)

# Meilleur et pire jour
meilleur_jour <- elasticite_jour$jour_semaine[1]
pire_jour <- elasticite_jour$jour_semaine[nrow(elasticite_jour)]
gain_potentiel <- elasticite_jour$elasticite[1] - elasticite_jour$elasticite[nrow(elasticite_jour)]

cat(paste("\n💡 INSIGHT : Porter le", pire_jour, "au niveau du", meilleur_jour, 
          "représente un gain potentiel de", round(gain_potentiel, 1), "%\n\n"))


# Élasticité des tranches horaires
elasticite_horaire <- impact_horaire %>%
  mutate(
    elasticite = (nb_visites - mean(nb_visites)) / mean(nb_visites) * 100
  ) %>%
  arrange(desc(elasticite))

cat("🕐 ÉLASTICITÉ PAR TRANCHE HORAIRE:\n")
print(elasticite_horaire)

# Créneaux sous-exploités
creneaux_faibles <- elasticite_horaire %>%
  filter(elasticite < -20) %>%
  pull(tranche_horaire)

if(length(creneaux_faibles) > 0) {
  cat(paste("\n⚠️ CRÉNEAUX SOUS-EXPLOITÉS:", paste(creneaux_faibles, collapse = ", "), "\n"))
  cat("💡 ACTION : Événements ciblés sur ces créneaux\n\n")
}


# =============================================================================
# PARTIE 13 : CLASSEMENT DES LEVIERS D'ACTION
# =============================================================================

cat("\n🎯 CLASSEMENT DES LEVIERS D'ACTION\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Créer un scoring des leviers
leviers <- data.frame(
  Levier = c(
    "Jour de la semaine",
    "Tranche horaire",
    "Espace visité",
    "Motif de visite",
    "Ancienneté usager",
    "Âge",
    "Sexe",
    "Ville d'origine",
    "Durée de visite"
  ),
  Impact = c(
    max(elasticite_jour$elasticite) - min(elasticite_jour$elasticite),
    max(elasticite_horaire$elasticite) - min(elasticite_horaire$elasticite),
    (max(impact_espaces$nb_visites) - min(impact_espaces$nb_visites)) / mean(impact_espaces$nb_visites) * 100,
    (max(impact_motifs$nb_visites) - min(impact_motifs$nb_visites)) / mean(impact_motifs$nb_visites) * 100,
    (max(impact_anciennete$nb_visites) - min(impact_anciennete$nb_visites)) / mean(impact_anciennete$nb_visites) * 100,
    (max(impact_age$nb_visites) - min(impact_age$nb_visites)) / mean(impact_age$nb_visites) * 100,
    (max(impact_sexe$nb_visites) - min(impact_sexe$nb_visites)) / mean(impact_sexe$nb_visites) * 100,
    (max(impact_ville$nb_visites) - min(impact_ville$nb_visites)) / mean(impact_ville$nb_visites) * 100,
    50  # Estimation
  ),
  Controlabilite = c(
    60,   # Jour : partiellement contrôlable (promotions)
    80,   # Horaire : très contrôlable (événements ciblés)
    90,   # Espace : très contrôlable (aménagement, promotion)
    70,   # Motif : contrôlable (offre de services)
    85,   # Ancienneté : contrôlable (fidélisation)
    20,   # Âge : peu contrôlable (ciblage marketing limité)
    30,   # Sexe : peu contrôlable
    40,   # Ville : partiellement contrôlable (marketing géociblé)
    60    # Durée : contrôlable (expérience, services)
  ),
  Cout_Mise_en_Oeuvre = c(
    "Moyen",
    "Faible",
    "Moyen",
    "Moyen",
    "Faible",
    "Élevé",
    "Élevé",
    "Élevé",
    "Moyen"
  )
) %>%
  mutate(
    Score_Priorite = Impact * (Controlabilite / 100),
    Rang = rank(-Score_Priorite)
  ) %>%
  arrange(Rang)

cat("🏆 LEVIERS CLASSÉS PAR PRIORITÉ D'ACTION:\n\n")
print(leviers)

# Visualisation matrice Impact x Contrôlabilité
p_leviers <- ggplot(leviers, aes(x = Controlabilite, y = Impact, 
                                 size = Score_Priorite, label = Levier)) +
  geom_point(aes(color = Cout_Mise_en_Oeuvre), alpha = 0.7) +
  geom_text(hjust = 0, vjust = 0, size = 3, nudge_x = 2) +
  geom_vline(xintercept = 50, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = median(leviers$Impact), linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 75, y = max(leviers$Impact), label = "Zone Prioritaire", 
           color = "darkgreen", fontface = "bold") +
  scale_color_manual(values = c("Faible" = "green", "Moyen" = "orange", "Élevé" = "red")) +
  labs(
    title = "Matrice Impact x Contrôlabilité des Leviers",
    subtitle = "Taille des bulles = Score de priorité",
    x = "Contrôlabilité (%)",
    y = "Impact Potentiel (%)",
    color = "Coût",
    size = "Score"
  ) +
  xlim(0, 100) +
  theme_minimal()

ggsave("outputs/figures/59_matrice_impact_controlabilite_leviers.png", p_leviers, width = 15, height = 6, dpi = 300)


# =============================================================================
# PARTIE 14 : RECOMMANDATIONS DÉTAILLÉES PAR LEVIER
# =============================================================================

cat("\n\n📋 RECOMMANDATIONS DÉTAILLÉES PAR LEVIER\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Top 3 leviers
top_leviers <- head(leviers, 3)

for(i in 1:nrow(top_leviers)) {
  cat(paste0("\n", i, ". ", toupper(top_leviers$Levier[i]), "\n"))
  cat(paste(rep("─", 70), collapse = ""), "\n")
  cat(paste("   Impact potentiel    :", round(top_leviers$Impact[i], 1), "%\n"))
  cat(paste("   Contrôlabilité      :", top_leviers$Controlabilite[i], "%\n"))
  cat(paste("   Coût                :", top_leviers$Cout_Mise_en_Oeuvre[i], "\n"))
  cat(paste("   Score de priorité   :", round(top_leviers$Score_Priorite[i], 1), "\n\n"))
  
  # Recommandations spécifiques
  if(top_leviers$Levier[i] == "Tranche horaire") {
    cat("   📌 ACTIONS RECOMMANDÉES:\n")
    cat("      • Organiser des événements matinaux (avant 9h)\n")
    cat("      • Ateliers en soirée (après 18h) pour attirer un public différent\n")
    cat("      • Happy hours ou tarifs préférentiels aux heures creuses\n")
    cat("      • Communication ciblée par SMS/email la veille\n\n")
    
  } else if(top_leviers$Levier[i] == "Espace visité") {
    cat("   📌 ACTIONS RECOMMANDÉES:\n")
    cat(paste0("      • Promouvoir les ", nrow(espaces_sous_utilises), " espaces sous-utilisés\n"))
    cat("      • Créer des parcours découverte multi-espaces\n")
    cat("      • Réaffecter ou fermer les espaces à très faible fréquentation\n")
    cat("      • Communiquer sur la diversité des espaces disponibles\n\n")
    
  } else if(top_leviers$Levier[i] == "Ancienneté usager") {
    cat("   📌 ACTIONS RECOMMANDÉES:\n")
    cat("      • Programme d'onboarding renforcé pour nouveaux usagers\n")
    cat("      • Suivi personnalisé après la 1ère visite\n")
    cat("      • Incitations à la 2ème et 3ème visite (offres limitées)\n")
    cat("      • Parcours de découverte guidé pour primo-visiteurs\n\n")
    
  } else if(top_leviers$Levier[i] == "Motif de visite") {
    cat("   📌 ACTIONS RECOMMANDÉES:\n")
    cat("      • Diversifier l'offre de services/événements\n")
    cat("      • Promouvoir les motifs à forte durée de visite\n")
    cat("      • Créer des packages thématiques\n")
    cat("      • Faciliter la réservation pour motifs spécifiques\n\n")
    
  } else if(top_leviers$Levier[i] == "Jour de la semaine") {
    cat("   📌 ACTIONS RECOMMANDÉES:\n")
    cat(paste0("      • Promotions ciblées le ", pire_jour, " (-", 
               round(abs(min(elasticite_jour$elasticite)), 1), "% vs moyenne)\n"))
    cat("      • Événements récurrents les jours faibles\n")
    cat("      • Tarification dynamique (réduction en semaine)\n")
    cat("      • Communication différenciée par jour\n\n")
  }
}


# =============================================================================
# PARTIE 15 : QUANTIFICATION DES OPPORTUNITÉS
# =============================================================================

cat("\n💰 QUANTIFICATION DES OPPORTUNITÉS\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Calculer les gains potentiels
moyenne_actuelle <- mean(data_daily$nb_visiteurs, na.rm = TRUE)
total_jours_historique <- nrow(data_daily)

# Opportunité 1 : Optimisation des jours faibles
jours_faibles <- elasticite_jour %>% filter(elasticite < 0)
nb_jours_faibles_an <- nrow(jours_faibles) * 52  # Par an

gain_jour_faible <- mean(impact_jour_semaine$nb_visites) - 
  min(impact_jour_semaine$nb_visites)
gain_potentiel_jours <- gain_jour_faible * nb_jours_faibles_an

cat("🎯 OPPORTUNITÉ 1 : OPTIMISATION DES JOURS FAIBLES\n")
cat(paste("   • Jours concernés        :", paste(jours_faibles$jour_semaine, collapse = ", "), "\n"))
cat(paste("   • Occurrences annuelles  :", nb_jours_faibles_an, "jours\n"))
cat(paste("   • Gain unitaire          : +", round(gain_jour_faible, 0), "visiteurs/jour\n"))
cat(paste("   • GAIN POTENTIEL ANNUEL  : +", format(round(gain_potentiel_jours), big.mark = " "), 
          "visiteurs (+", round(gain_potentiel_jours / (moyenne_actuelle * 365) * 100, 1), "%)\n\n"))


# Opportunité 2 : Exploitation des créneaux horaires faibles
if(length(creneaux_faibles) > 0) {
  pct_creneaux_faibles <- sum(impact_horaire$pct_visites[impact_horaire$tranche_horaire %in% creneaux_faibles])
  gain_potentiel_horaires <- (20 / 100) * sum(data_daily$nb_visiteurs, na.rm = TRUE)  # Objectif +20%
  
  cat("🎯 OPPORTUNITÉ 2 : EXPLOITATION DES CRÉNEAUX FAIBLES\n")
  cat(paste("   • Créneaux concernés     :", paste(creneaux_faibles, collapse = ", "), "\n"))
  cat(paste("   • Part actuelle          :", round(pct_creneaux_faibles, 1), "%\n"))
  cat(paste("   • Objectif visé          : +20% sur ces créneaux\n"))
  cat(paste("   • GAIN POTENTIEL ANNUEL  : +", format(round(gain_potentiel_horaires), big.mark = " "), 
            "visiteurs\n\n"))
}


# Opportunité 3 : Valorisation des espaces sous-utilisés
gain_espaces <- mean(impact_espaces$nb_visites[1:5]) - mean(espaces_sous_utilises$nb_visites)
gain_potentiel_espaces <- gain_espaces * nrow(espaces_sous_utilises)

cat("🎯 OPPORTUNITÉ 3 : VALORISATION DES ESPACES SOUS-UTILISÉS\n")
cat(paste("   • Espaces concernés      :", nrow(espaces_sous_utilises), "\n"))
cat(paste("   • Fréquentation moyenne  :", round(mean(espaces_sous_utilises$nb_visites)), "visites\n"))
cat(paste("   • Objectif visé          : Atteindre moyenne des top 5\n"))
cat(paste("   • GAIN POTENTIEL         : +", format(round(gain_potentiel_espaces), big.mark = " "), 
          "visites\n\n"))


# Opportunité 4 : Conversion des occasionnels
nb_occasionnels <- sum(data_usagers_comportement$nb_visites == 1)
taux_conversion_cible <- 0.30  # 30% des occasionnels deviennent réguliers
visites_moyennes_reguliers <- mean(data_usagers_comportement$nb_visites[data_usagers_comportement$nb_visites >= 6])

gain_conversion <- nb_occasionnels * taux_conversion_cible * (visites_moyennes_reguliers - 1)

cat("🎯 OPPORTUNITÉ 4 : CONVERSION DES OCCASIONNELS\n")
cat(paste("   • Occasionnels           :", format(nb_occasionnels, big.mark = " "), "usagers\n"))
cat(paste("   • Taux conversion cible  :", taux_conversion_cible * 100, "%\n"))
cat(paste("   • Visites/régulier       :", round(visites_moyennes_reguliers, 1), "\n"))
cat(paste("   • GAIN POTENTIEL         : +", format(round(gain_conversion), big.mark = " "), 
          "visites\n\n"))


# Synthèse des gains
gain_total <- gain_potentiel_jours + gain_potentiel_espaces + gain_conversion
if(exists("gain_potentiel_horaires")) {
  gain_total <- gain_total + gain_potentiel_horaires
}

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("💎 GAIN TOTAL POTENTIEL (scénario optimiste)\n")
cat(paste("   ", format(round(gain_total), big.mark = " "), "visiteurs supplémentaires\n"))
cat(paste("   soit +", round(gain_total / sum(data_daily$nb_visiteurs, na.rm = TRUE) * 100, 1), 
          "% vs situation actuelle\n"))
cat("═══════════════════════════════════════════════════════════════════════\n\n")


# =============================================================================
# PARTIE 16 : EXPORT DES RÉSULTATS
# =============================================================================

cat("\n💾 EXPORT DES RÉSULTATS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# 1. Sauvegarder l'analyse d'importance
write.csv(importance_rf, "outputs/tables/importance_variables.csv", row.names = FALSE)
cat("✓ Importance des variables : importance_variables.csv\n")

# 2. Sauvegarder les leviers
write.csv(leviers, "outputs/tables/leviers_action_priorites.csv", row.names = FALSE)
cat("✓ Leviers d'action : leviers_action_priorites.csv\n")

# 3. Sauvegarder les impacts par facteur
write.csv(impact_jour_semaine, "outputs/tables/impact_jour_semaine.csv", row.names = FALSE)
write.csv(impact_espaces, "outputs/tables/impact_espaces.csv", row.names = FALSE)
write.csv(impact_motifs, "outputs/tables/impact_motifs.csv", row.names = FALSE)
write.csv(impact_horaire, "outputs/tables/impact_horaire.csv", row.names = FALSE)
cat("✓ Impacts détaillés : impact_*.csv\n")

# 4. Rapport synthétique
rapport_facteurs <- list(
  date_generation = Sys.Date(),
  
  top_3_leviers = top_leviers$Levier,
  
  meilleur_jour = meilleur_jour,
  pire_jour = pire_jour,
  gain_jours = round(gain_potentiel_jours),
  
  creneaux_faibles = creneaux_faibles,
  
  nb_espaces_sous_utilises = nrow(espaces_sous_utilises),
  gain_espaces = round(gain_potentiel_espaces),
  
  nb_occasionnels = nb_occasionnels,
  gain_conversion = round(gain_conversion),
  
  gain_total_potentiel = round(gain_total),
  pct_gain = round(gain_total / sum(data_daily$nb_visiteurs, na.rm = TRUE) * 100, 1)
)

saveRDS(rapport_facteurs, "outputs/reports/rapport_facteurs_influence.rds")
cat("✓ Rapport synthétique : rapport_facteurs_influence.rds\n\n")


# =============================================================================
# PARTIE 17 : RAPPORT FINAL FORMATÉ
# =============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                 ║\n")
cat("║              RAPPORT D'ANALYSE DES FACTEURS D'INFLUENCE         ║\n")
cat("║              Leviers d'Amélioration de la Fréquentation         ║\n")
cat("║                                                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("📅 DATE DE GÉNÉRATION:", format(Sys.Date(), "%d/%m/%Y"), "\n\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 1. TOP 3 LEVIERS PRIORITAIRES                                   │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
for(i in 1:3) {
  cat(paste0("   ", i, ". ", top_leviers$Levier[i], 
             " (Score: ", round(top_leviers$Score_Priorite[i], 1), ")\n"))
}
cat("\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 2. FACTEURS TEMPORELS                                           │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
cat(paste("   🏆 Jour le plus fort    :", meilleur_jour, 
          "(", max(impact_jour_semaine$nb_visites), "visites)\n"))
cat(paste("   📉 Jour le plus faible  :", pire_jour, 
          "(", min(impact_jour_semaine$nb_visites), "visites)\n"))
cat(paste("   📊 Écart                :", round(gain_potentiel, 1), "%\n"))
cat(paste("   ⏰ Heure de pointe      :", round(mean(data_visites_enrichi$heure_arrivee, na.rm = TRUE)), "h\n"))
cat("\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 3. FACTEURS SPATIAUX                                            │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
cat(paste("   🏢 Espaces actifs       :", nrow(impact_espaces), "\n"))
cat(paste("   🏆 Espace le plus visité:", impact_espaces$visited_space[1], 
          "(", impact_espaces$nb_visites[1], "visites)\n"))
cat(paste("   ⚠️  Espaces sous-utilisés:", nrow(espaces_sous_utilises), "\n"))
cat("\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 4. FACTEURS DÉMOGRAPHIQUES                                      │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
cat(paste("   👥 Âge moyen            :", round(mean(data_visites_enrichi$age, na.rm = TRUE), 1), "ans\n"))
cat(paste("   👥 Tranche majoritaire  :", impact_age$age_categorie[1], 
          "(", round(impact_age$pct_visites[1], 1), "%)\n"))
cat(paste("   ⚥  Sexe majoritaire     :", impact_sexe$sex[1], 
          "(", round(impact_sexe$pct_visites[1], 1), "%)\n"))
cat(paste("   🌍 Ville principale     :", impact_ville$city[1], 
          "(", round(impact_ville$pct_visites[1], 1), "%)\n"))
cat("\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 5. GAINS POTENTIELS QUANTIFIÉS                                  │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
cat(paste("   📈 Optimisation jours      : +", format(round(gain_potentiel_jours), big.mark = " "), "visiteurs/an\n"))
cat(paste("   📈 Espaces sous-utilisés   : +", format(round(gain_potentiel_espaces), big.mark = " "), "visites\n"))
cat(paste("   📈 Conversion occasionnels : +", format(round(gain_conversion), big.mark = " "), "visites\n"))
cat(paste("   ─────────────────────────────────────────────────────────────\n"))
cat(paste("   💎 TOTAL POTENTIEL         : +", format(round(gain_total), big.mark = " "), 
          "visiteurs (", rapport_facteurs$pct_gain, "%)\n"))
cat("\n")

cat("┌───────────────────────────────────────────────────────────────────────┐\n")
cat("│ 6. ACTIONS PRIORITAIRES RECOMMANDÉES                            │\n")
cat("└───────────────────────────────────────────────────────────────────────┘\n")
cat("\n")
cat("   🎯 ACTION 1 : OPTIMISATION HORAIRE (Impact élevé, Coût faible)\n")
cat("      → Événements ciblés sur créneaux faibles\n")
cat("      → Communication proactive J-1\n\n")

cat("   🎯 ACTION 2 : VALORISATION SPATIALE (Impact élevé, Coût moyen)\n")
cat("      → Parcours découverte multi-espaces\n")
cat(paste0("      → Promotion des ", nrow(espaces_sous_utilises), " espaces sous-utilisés\n\n"))

cat("   🎯 ACTION 3 : FIDÉLISATION (Impact élevé, Coût faible)\n")
cat(paste0("      → Programme onboarding pour ", nb_occasionnels, " occasionnels\n"))
cat("      → Incitations à la 2ème visite\n\n")

cat("   🎯 ACTION 4 : RÉGULARISATION HEBDOMADAIRE (Impact moyen, Coût moyen)\n")
cat(paste0("      → Promotions ciblées le ", pire_jour, "\n"))
cat("      → Événements récurrents jours faibles\n\n")

cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                         FIN DU RAPPORT                          ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n")

cat("\n✅ OBJECTIF 2 TERMINÉ AVEC SUCCÈS !\n\n")
cat("📌 FICHIERS GÉNÉRÉS:\n")
cat("   • importance_variables.csv\n")
cat("   • leviers_action_priorites.csv\n")
cat("   • impact_jour_semaine.csv\n")
cat("   • impact_espaces.csv\n")
cat("   • impact_motifs.csv\n")
cat("   • impact_horaire.csv\n")
cat("   • rapport_facteurs_influence.rds\n\n")

cat("📌 PROCHAINES ÉTAPES:\n")
cat("   → Objectif 3 : Prédire le comportement des usagers\n")
cat("   → Objectif 4 : Optimiser l'allocation des ressources\n\n")