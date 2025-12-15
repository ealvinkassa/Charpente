# ============================================================================
# ANALYSES AVANCÉES ET INSIGHTS STRATÉGIQUES
# ============================================================================

# library(tidyverse)
# library(lubridate)
# library(scales)
# library(patchwork)

# Charger les données
data_frequentation <- readRDS("data/processed/data_frequentation.rds")
data_usagers_comportement <- readRDS("data/processed/data_usagers_comportement.rds")

cat("\n", rep("=", 80), "\n")
cat("ANALYSES AVANCÉES ET INSIGHTS STRATÉGIQUES\n")
cat(rep("=", 80), "\n\n")

# ============================================================================
# ANALYSE 1 : SEGMENTATION COMPORTEMENTALE AVANCÉE
# ============================================================================

cat("ANALYSE 1 : SEGMENTATION COMPORTEMENTALE AVANCÉE\n")
cat(rep("-", 80), "\n\n")

# 1.1 Profils d'usagers multidimensionnels
profils_usagers <- data_usagers_comportement %>%
  mutate(
    profil_frequence = case_when(
      nb_visites == 1 ~ "Occasionnel",
      nb_visites <= 5 ~ "Explorateur",
      nb_visites <= 10 ~ "Régulier",
      TRUE ~ "Fidèle"
    ),
    profil_duree = case_when(
      duree_moyenne_visite < 60 ~ "Rapide",
      duree_moyenne_visite < 180 ~ "Moyen",
      duree_moyenne_visite < 360 ~ "Long",
      TRUE ~ "Très long"
    ),
    profil_diversite = case_when(
      nb_espaces_differents == 1 ~ "Spécialisé",
      nb_espaces_differents <= 3 ~ "Sélectif",
      TRUE ~ "Explorateur spatial"
    ),
    profil_horaire = case_when(
      heure_arrivee_moyenne < 10 ~ "Matinal",
      heure_arrivee_moyenne < 14 ~ "Mi-journée",
      heure_arrivee_moyenne < 18 ~ "Après-midi",
      TRUE ~ "Soirée"
    )
  )

# Matrice de profils combinés
cat("1.1 PROFILS COMBINÉS FRÉQUENCE × DURÉE\n")
profils_freq_duree <- profils_usagers %>%
  count(profil_frequence, profil_duree) %>%
  group_by(profil_frequence) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(profil_frequence, desc(n))

print(profils_freq_duree)
cat("\n")

# Profils à forte valeur (Fidèles avec longues durées)
cat("1.2 USAGERS À FORTE VALEUR\n")
forte_valeur <- profils_usagers %>%
  filter(profil_frequence %in% c("Fidèle", "Régulier"),
         profil_duree %in% c("Long", "Très long")) %>%
  summarise(
    nb_usagers = n(),
    pct_total = n() / nrow(data_usagers_comportement) * 100,
    nb_total_visites = sum(nb_visites),
    duree_totale_heures = sum(duree_totale_minutes) / 60,
    score_engagement_moyen = mean(score_engagement, na.rm = TRUE)
  )

cat("Nombre d'usagers à forte valeur :", forte_valeur$nb_usagers, 
    "(", round(forte_valeur$pct_total, 1), "% du total)\n")
cat("Total de visites générées      :", forte_valeur$nb_total_visites, "\n")
cat("Total d'heures cumulées        :", round(forte_valeur$duree_totale_heures), "h\n")
cat("Score d'engagement moyen       :", round(forte_valeur$score_engagement_moyen, 1), "\n\n")

# ============================================================================
# ANALYSE 2 : ANALYSE DE COHORTES
# ============================================================================

cat(rep("=", 80), "\n")
cat("ANALYSE 2 : ANALYSE DE COHORTES\n")
cat(rep("-", 80), "\n\n")

# 2.1 Cohortes mensuelles d'inscription
cohortes <- data_usagers_comportement %>%
  mutate(cohorte = floor_date(registration_date, "month")) %>%
  group_by(cohorte) %>%
  summarise(
    nb_usagers = n(),
    nb_revenus = sum(nb_visites > 1),
    taux_retention = nb_revenus / nb_usagers * 100,
    nb_moy_visites = mean(nb_visites),
    duree_moy_totale = mean(duree_totale_minutes) / 60,
    score_engagement_moyen = mean(score_engagement, na.rm = TRUE)
  ) %>%
  arrange(cohorte)

cat("2.1 PERFORMANCE PAR COHORTE D'INSCRIPTION\n")
print(cohortes)
cat("\n")

# Identifier la meilleure et la pire cohorte
meilleure_cohorte <- cohortes %>% 
  filter(nb_usagers >= 50) %>%
  arrange(desc(taux_retention)) %>% 
  slice(1)

pire_cohorte <- cohortes %>% 
  filter(nb_usagers >= 50) %>%
  arrange(taux_retention) %>% 
  slice(1)

cat("MEILLEURE COHORTE (≥50 usagers):\n")
cat("Mois                 :", format(meilleure_cohorte$cohorte, "%B %Y"), "\n")
cat("Taux de rétention    :", round(meilleure_cohorte$taux_retention, 1), "%\n")
cat("Score engagement     :", round(meilleure_cohorte$score_engagement_moyen, 1), "\n\n")

cat("COHORTE À AMÉLIORER (≥50 usagers):\n")
cat("Mois                 :", format(pire_cohorte$cohorte, "%B %Y"), "\n")
cat("Taux de rétention    :", round(pire_cohorte$taux_retention, 1), "%\n")
cat("Score engagement     :", round(pire_cohorte$score_engagement_moyen, 1), "\n\n")

# ============================================================================
# ANALYSE 3 : PATTERNS TEMPORELS
# ============================================================================

cat(rep("=", 80), "\n")
cat("ANALYSE 3 : PATTERNS TEMPORELS ET SAISONNALITÉ\n")
cat(rep("-", 80), "\n\n")

# 3.1 Performance par jour de la semaine
cat("3.1 PERFORMANCE PAR JOUR DE LA SEMAINE\n")
perf_jour <- data_frequentation %>%
  mutate(
    jour_semaine = wday(visit_date, label = TRUE, abbr = FALSE, week_start = 1),
    est_weekend = wday(visit_date) %in% c(1, 7)
  ) %>%
  group_by(jour_semaine, est_weekend) %>%
  summarise(
    nb_visites = n(),
    duree_moy = mean(duration_minutes, na.rm = TRUE),
    nb_usagers_uniques = n_distinct(phone),
    .groups = "drop"
  ) %>%
  arrange(desc(nb_visites))

print(perf_jour)
cat("\n")

# Meilleur et pire jour
meilleur_jour <- perf_jour %>% slice(1)
pire_jour <- perf_jour %>% slice(n())

cat("MEILLEUR JOUR       :", as.character(meilleur_jour$jour_semaine), "\n")
cat("  - Visites         :", meilleur_jour$nb_visites, "\n")
cat("  - Durée moyenne   :", round(meilleur_jour$duree_moy), "min\n\n")

cat("JOUR LE MOINS ACTIF :", as.character(pire_jour$jour_semaine), "\n")
cat("  - Visites         :", pire_jour$nb_visites, "\n")
cat("  - Durée moyenne   :", round(pire_jour$duree_moy), "min\n\n")

# 3.2 Heures de pointe
cat("3.2 HEURES DE POINTE\n")
heures_pointe <- data_frequentation %>%
  mutate(heure = hour(arrival_time)) %>%
  count(heure, sort = TRUE) %>%
  head(5)

cat("Top 5 des heures d'arrivée:\n")
print(heures_pointe)
cat("\n")

# 3.3 Creux d'activité
heures_creuses <- data_frequentation %>%
  mutate(heure = hour(arrival_time)) %>%
  count(heure) %>%
  arrange(n) %>%
  head(5)

cat("Top 5 des heures les moins actives:\n")
print(heures_creuses)
cat("\n")

# ============================================================================
# ANALYSE 4 : ANALYSE SPATIALE AVANCÉE
# ============================================================================

cat(rep("=", 80), "\n")
cat("ANALYSE 4 : ANALYSE SPATIALE AVANCÉE\n")
cat(rep("-", 80), "\n\n")

# 4.1 Performance par espace
cat("4.1 PERFORMANCE DES ESPACES (TOP 10)\n")
perf_espaces <- data_frequentation %>%
  group_by(visited_space) %>%
  summarise(
    nb_visites = n(),
    nb_usagers_uniques = n_distinct(phone),
    duree_moy = mean(duration_minutes, na.rm = TRUE),
    duree_totale_heures = sum(duration_minutes, na.rm = TRUE) / 60,
    taux_occupation = nb_visites / n_distinct(data_frequentation$visit_date)
  ) %>%
  arrange(desc(nb_visites)) %>%
  head(10)

print(perf_espaces)
cat("\n")

# Espaces à fort engagement
cat("4.2 ESPACES À FORT ENGAGEMENT (Durée moyenne > 3h)\n")
espaces_engagement <- data_frequentation %>%
  group_by(visited_space) %>%
  summarise(
    nb_visites = n(),
    duree_moy = mean(duration_minutes, na.rm = TRUE)
  ) %>%
  filter(nb_visites >= 50, duree_moy > 180) %>%
  arrange(desc(duree_moy))

print(espaces_engagement)
cat("\n")

# 4.3 Espaces sous-utilisés
cat("4.3 ESPACES SOUS-UTILISÉS (Moins de 50 visites)\n")
espaces_sous_utilises <- data_frequentation %>%
  count(visited_space, sort = TRUE) %>%
  filter(n < 50) %>%
  summarise(
    nb_espaces = n(),
    total_visites = sum(n),
    pct_visites = sum(n) / nrow(data_frequentation) * 100
  )

cat("Nombre d'espaces sous-utilisés :", espaces_sous_utilises$nb_espaces, "\n")
cat("Total visites (cumulé)         :", espaces_sous_utilises$total_visites, "\n")
cat("% du total des visites         :", round(espaces_sous_utilises$pct_visites, 1), "%\n\n")

# ============================================================================
# ANALYSE 5 : ANALYSE DU CHURN ET DE LA RÉTENTION
# ============================================================================

cat(rep("=", 80), "\n")
cat("ANALYSE 5 : ANALYSE DU CHURN ET DE LA RÉTENTION\n")
cat(rep("-", 80), "\n\n")

# 5.1 Définir le churn (pas de visite depuis 30 jours)
date_reference <- max(data_usagers_comportement$derniere_visite, na.rm = TRUE)

analyse_churn <- data_usagers_comportement %>%
  mutate(
    jours_depuis_derniere = as.numeric(difftime(date_reference, derniere_visite, units = "days")),
    statut_churn = case_when(
      jours_depuis_derniere <= 7 ~ "Actif récent",
      jours_depuis_derniere <= 30 ~ "Actif",
      jours_depuis_derniere <= 90 ~ "À risque",
      TRUE ~ "Inactif"
    ),
    categorie_engagement = case_when(
      nb_visites == 1 ~ "Occasionnel",
      nb_visites <= 5 ~ "Explorateur",
      nb_visites <= 10 ~ "Régulier",
      TRUE ~ "Fidèle"
    )
  )

cat("5.1 STATUT D'ACTIVITÉ AU", format(date_reference, "%d/%m/%Y"), "\n")
statut_activite <- analyse_churn %>%
  count(statut_churn) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(match(statut_churn, c("Actif récent", "Actif", "À risque", "Inactif")))

print(statut_activite)
cat("\n")

# 5.2 Taux de churn par catégorie d'engagement
cat("5.2 TAUX D'INACTIVITÉ PAR CATÉGORIE\n")
churn_par_categorie <- analyse_churn %>%
  group_by(categorie_engagement) %>%
  summarise(
    nb_usagers = n(),
    nb_inactifs = sum(statut_churn == "Inactif"),
    taux_inactivite = nb_inactifs / nb_usagers * 100,
    nb_a_risque = sum(statut_churn == "À risque"),
    taux_risque = nb_a_risque / nb_usagers * 100
  ) %>%
  arrange(match(categorie_engagement, c("Occasionnel", "Explorateur", "Régulier", "Fidèle")))

print(churn_par_categorie)
cat("\n")

# 5.3 Usagers à risque de haute valeur
cat("5.3 USAGERS À RISQUE DE HAUTE VALEUR (À reconquérir)\n")
usagers_risque_valeur <- analyse_churn %>%
  filter(statut_churn == "À risque",
         nb_visites >= 6,
         score_engagement >= 70) %>%
  summarise(
    nb_usagers = n(),
    nb_total_visites = sum(nb_visites),
    duree_totale_heures = sum(duree_totale_minutes) / 60
  )

cat("Nombre d'usagers              :", usagers_risque_valeur$nb_usagers, "\n")
cat("Total visites historiques     :", usagers_risque_valeur$nb_total_visites, "\n")
cat("Total heures cumulées         :", round(usagers_risque_valeur$duree_totale_heures), "h\n\n")

# ============================================================================
# ANALYSE 6 : PARCOURS ET TRANSITIONS
# ============================================================================

cat(rep("=", 80), "\n")
cat("ANALYSE 6 : PARCOURS D'ENGAGEMENT\n")
cat(rep("-", 80), "\n\n")

# 6.1 Évolution du premier au dernier mois
cat("6.1 ÉVOLUTION DE L'ENGAGEMENT DANS LE TEMPS\n")
evolution_usagers <- data_frequentation %>%
  group_by(phone) %>%
  arrange(visit_date) %>%
  mutate(
    num_visite = row_number(),
    semaine_visite = floor(as.numeric(difftime(visit_date, min(visit_date), units = "weeks")))
  ) %>%
  ungroup()

# Durée moyenne des 3 premières vs 3 dernières visites (pour ceux avec 6+ visites)
evolution_duree <- evolution_usagers %>%
  filter(phone %in% (data_usagers_comportement %>% 
                       filter(nb_visites >= 6) %>% 
                       pull(phone))) %>%
  group_by(phone) %>%
  mutate(
    rang_debut = row_number(),
    rang_fin = n() - row_number() + 1
  ) %>%
  summarise(
    duree_3_premieres = mean(duration_minutes[rang_debut <= 3], na.rm = TRUE),
    duree_3_dernieres = mean(duration_minutes[rang_fin <= 3], na.rm = TRUE)
  ) %>%
  summarise(
    duree_moy_debut = mean(duree_3_premieres, na.rm = TRUE),
    duree_moy_fin = mean(duree_3_dernieres, na.rm = TRUE),
    evolution_pct = (duree_moy_fin - duree_moy_debut) / duree_moy_debut * 100
  )

cat("Durée moyenne 3 premières visites :", round(evolution_duree$duree_moy_debut), "min\n")
cat("Durée moyenne 3 dernières visites :", round(evolution_duree$duree_moy_fin), "min\n")
cat("Évolution                          :", 
    ifelse(evolution_duree$evolution_pct > 0, "+", ""),
    round(evolution_duree$evolution_pct, 1), "%\n\n")

# ============================================================================
# ANALYSE 7 : ANALYSE PAR GENRE
# ============================================================================

cat(rep("=", 80), "\n")
cat("ANALYSE 7 : ANALYSE COMPARATIVE PAR GENRE\n")
cat(rep("-", 80), "\n\n")

cat("7.1 COMPARAISON DES MÉTRIQUES CLÉS\n")
comp_genre <- data_usagers_comportement %>%
  filter(!is.na(sex)) %>%
  group_by(sex) %>%
  summarise(
    nb_usagers = n(),
    nb_moy_visites = mean(nb_visites),
    duree_moy_visite = mean(duree_moyenne_visite),
    duree_totale_moy = mean(duree_totale_minutes) / 60,
    nb_moy_espaces = mean(nb_espaces_differents),
    score_engagement_moy = mean(score_engagement, na.rm = TRUE),
    taux_retention = sum(nb_visites > 1) / n() * 100,
    pct_weekend = mean(pct_visites_weekend, na.rm = TRUE)
  )

print(comp_genre)
cat("\n")

# Tests statistiques simples
cat("7.2 DIFFÉRENCES SIGNIFICATIVES\n")

# Nombre de visites
test_visites <- wilcox.test(
  nb_visites ~ sex, 
  data = data_usagers_comportement %>% filter(!is.na(sex))
)

cat("Test Wilcoxon - Nombre de visites:\n")
cat("  p-value:", format.pval(test_visites$p.value, digits = 3), "\n")
cat("  Différence:", ifelse(test_visites$p.value < 0.05, "SIGNIFICATIVE", "Non significative"), "\n\n")

# Score d'engagement
test_engagement <- wilcox.test(
  score_engagement ~ sex, 
  data = data_usagers_comportement %>% filter(!is.na(sex), !is.na(score_engagement))
)

cat("Test Wilcoxon - Score d'engagement:\n")
cat("  p-value:", format.pval(test_engagement$p.value, digits = 3), "\n")
cat("  Différence:", ifelse(test_engagement$p.value < 0.05, "SIGNIFICATIVE", "Non significative"), "\n\n")

# ============================================================================
# ANALYSE 8 : OPPORTUNITÉS ET RECOMMANDATIONS
# ============================================================================

cat(rep("=", 80), "\n")
cat("ANALYSE 8 : OPPORTUNITÉS IDENTIFIÉES\n")
cat(rep("-", 80), "\n\n")

# 8.1 Segments à potentiel
cat("8.1 SEGMENTS À POTENTIEL DE CROISSANCE\n\n")

# Explorateurs avec bon engagement
explorateurs_potentiel <- data_usagers_comportement %>%
  filter(categorie_nb_visites == "Explorateur (2-5)",
         score_engagement >= 70) %>%
  nrow()

cat("A. Explorateurs à fort engagement (2-5 visites, score ≥70):\n")
cat("   Nombre                      :", explorateurs_potentiel, "usagers\n")
cat("   Action recommandée          : Programme de fidélisation ciblé\n\n")

# Usagers mono-espace
mono_espace <- data_usagers_comportement %>%
  filter(nb_espaces_differents == 1,
         nb_visites >= 3) %>%
  nrow()

cat("B. Usagers mono-espace réguliers (1 espace, 3+ visites):\n")
cat("   Nombre                      :", mono_espace, "usagers\n")
cat("   Action recommandée          : Découverte guidée d'autres espaces\n\n")

# Occasionnels récents
occasionnels_recents <- analyse_churn %>%
  filter(categorie_engagement == "Occasionnel",
         jours_depuis_derniere <= 14) %>%
  nrow()

cat("C. Occasionnels récents (1 visite, <14 jours):\n")
cat("   Nombre                      :", occasionnels_recents, "usagers\n")
cat("   Action recommandée          : Email de réengagement immédiat\n\n")

# 8.2 Optimisation temporelle
cat("8.2 OPPORTUNITÉS D'OPTIMISATION TEMPORELLE\n\n")

# Identifier les plages horaires sous-utilisées
plages_creuses <- data_frequentation %>%
  mutate(
    plage_horaire = cut(hour(arrival_time),
                        breaks = c(0, 9, 12, 14, 18, 24),
                        labels = c("Avant 9h", "9h-12h", "12h-14h", "14h-18h", "Après 18h"))
  ) %>%
  count(plage_horaire) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(n)

cat("Plages horaires à optimiser:\n")
print(head(plages_creuses, 2))
cat("\n")

cat(rep("=", 80), "\n")
cat("FIN DES ANALYSES AVANCÉES\n")
cat(rep("=", 80), "\n\n")