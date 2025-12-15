# ============================================================================
# ANALYSE EXPLORATOIRE DÉTAILLÉE - COMPORTEMENT DES USAGERS
# ============================================================================

# library(tidyverse)
# library(scales)

# Charger les données
data_usagers_comportement <- readRDS("data/processed/data_usagers_comportement.rds")

cat("\n", rep("=", 80), "\n")
cat("SECTION 7 : ANALYSE COMPORTEMENTALE DES USAGERS\n")
cat(rep("=", 80), "\n\n")

# 7.1 Distribution du nombre de visites
cat("7.1 DISTRIBUTION DU NOMBRE DE VISITES PAR USAGER\n")
cat(rep("-", 80), "\n")

visites_stats <- data_usagers_comportement %>%
  summarise(
    nb_usagers = n(),
    visites_min = min(nb_visites),
    visites_q1 = quantile(nb_visites, 0.25),
    visites_median = median(nb_visites),
    visites_mean = mean(nb_visites),
    visites_q3 = quantile(nb_visites, 0.75),
    visites_max = max(nb_visites),
    visites_sd = sd(nb_visites),
    pct_1_visite = sum(nb_visites == 1) / n() * 100,
    pct_2_5_visites = sum(nb_visites >= 2 & nb_visites <= 5) / n() * 100,
    pct_6_10_visites = sum(nb_visites >= 6 & nb_visites <= 10) / n() * 100,
    pct_11plus_visites = sum(nb_visites >= 11) / n() * 100
  )

cat("Nombre d'usagers      :", format(visites_stats$nb_usagers, big.mark = " "), "\n")
cat("Visites min          :", visites_stats$visites_min, "\n")
cat("1er quartile         :", visites_stats$visites_q1, "\n")
cat("Médiane              :", visites_stats$visites_median, "\n")
cat("Moyenne              :", round(visites_stats$visites_mean, 0), "\n")
cat("3ème quartile        :", visites_stats$visites_q3, "\n")
cat("Visites max          :", visites_stats$visites_max, "\n")
cat("Écart-type           :", round(visites_stats$visites_sd, 0), "\n\n")

cat("RÉPARTITION PAR CATÉGORIE:\n")
cat("1 visite (Occasionnel)     :", format(sum(data_usagers_comportement$nb_visites == 1), big.mark = " "),
    "usagers (", round(visites_stats$pct_1_visite, 1), "%)\n")
cat("2-5 visites (Explorateur)  :", format(sum(data_usagers_comportement$nb_visites >= 2 & 
                                                 data_usagers_comportement$nb_visites <= 5), big.mark = " "),
    "usagers (", round(visites_stats$pct_2_5_visites, 1), "%)\n")
cat("6-10 visites (Régulier)    :", format(sum(data_usagers_comportement$nb_visites >= 6 & 
                                                 data_usagers_comportement$nb_visites <= 10), big.mark = " "),
    "usagers (", round(visites_stats$pct_6_10_visites, 1), "%)\n")
cat("11+ visites (Fidèle)       :", format(sum(data_usagers_comportement$nb_visites >= 11), big.mark = " "),
    "usagers (", round(visites_stats$pct_11plus_visites, 1), "%)\n\n")

# Distribution détaillée
dist_visites <- data_usagers_comportement %>%
  count(nb_visites, sort = TRUE) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct_cumul = cumsum(pct)
  ) %>%
  head(20)

cat("TOP 20 FRÉQUENCES DE VISITES:\n")
print(dist_visites)
cat("\n")

# 7.2 Analyse de l'engagement
cat("7.2 SCORE D'ENGAGEMENT\n")
cat(rep("-", 80), "\n")

engagement_stats <- data_usagers_comportement %>%
  summarise(
    score_min = min(score_engagement, na.rm = TRUE),
    score_q1 = quantile(score_engagement, 0.25, na.rm = TRUE),
    score_median = median(score_engagement, na.rm = TRUE),
    score_mean = mean(score_engagement, na.rm = TRUE),
    score_q3 = quantile(score_engagement, 0.75, na.rm = TRUE),
    score_max = max(score_engagement, na.rm = TRUE),
    pct_engagement_faible = sum(score_engagement < 33, na.rm = TRUE) / n() * 100,
    pct_engagement_moyen = sum(score_engagement >= 33 & score_engagement < 66, na.rm = TRUE) / n() * 100,
    pct_engagement_fort = sum(score_engagement >= 66, na.rm = TRUE) / n() * 100
  )

cat("Score minimum        :", round(engagement_stats$score_min, 1), "\n")
cat("1er quartile         :", round(engagement_stats$score_q1, 1), "\n")
cat("Score médian         :", round(engagement_stats$score_median, 1), "\n")
cat("Score moyen          :", round(engagement_stats$score_mean, 1), "\n")
cat("3ème quartile        :", round(engagement_stats$score_q3, 1), "\n")
cat("Score maximum        :", round(engagement_stats$score_max, 1), "\n\n")

cat("NIVEAUX D'ENGAGEMENT:\n")
cat("Faible (<33)         :", round(engagement_stats$pct_engagement_faible, 1), "%\n")
cat("Moyen (33-66)        :", round(engagement_stats$pct_engagement_moyen, 1), "%\n")
cat("Fort (>66)           :", round(engagement_stats$pct_engagement_fort, 1), "%\n\n")

# 7.3 Durées de visite
cat("7.3 DURÉES DE VISITE (AGRÉGÉES PAR USAGER)\n")
cat(rep("-", 80), "\n")

duree_usagers_stats <- data_usagers_comportement %>%
  summarise(
    duree_totale_min = min(duree_totale_minutes, na.rm = TRUE),
    duree_totale_q1 = quantile(duree_totale_minutes, 0.25, na.rm = TRUE),
    duree_totale_median = median(duree_totale_minutes, na.rm = TRUE),
    duree_totale_mean = mean(duree_totale_minutes, na.rm = TRUE),
    duree_totale_q3 = quantile(duree_totale_minutes, 0.75, na.rm = TRUE),
    duree_totale_max = max(duree_totale_minutes, na.rm = TRUE),
    
    duree_moy_min = min(duree_moyenne_visite, na.rm = TRUE),
    duree_moy_median = median(duree_moyenne_visite, na.rm = TRUE),
    duree_moy_mean = mean(duree_moyenne_visite, na.rm = TRUE),
    duree_moy_max = max(duree_moyenne_visite, na.rm = TRUE)
  )

cat("DURÉE TOTALE CUMULÉE PAR USAGER:\n")
cat("Minimum              :", round(duree_usagers_stats$duree_totale_min), "min\n")
cat("1er quartile         :", round(duree_usagers_stats$duree_totale_q1), "min (", 
    round(duree_usagers_stats$duree_totale_q1/60, 1), "h)\n")
cat("Médiane              :", round(duree_usagers_stats$duree_totale_median), "min (", 
    round(duree_usagers_stats$duree_totale_median/60, 1), "h)\n")
cat("Moyenne              :", round(duree_usagers_stats$duree_totale_mean), "min (", 
    round(duree_usagers_stats$duree_totale_mean/60, 1), "h)\n")
cat("3ème quartile        :", round(duree_usagers_stats$duree_totale_q3), "min (", 
    round(duree_usagers_stats$duree_totale_q3/60, 1), "h)\n")
cat("Maximum              :", round(duree_usagers_stats$duree_totale_max), "min (", 
    round(duree_usagers_stats$duree_totale_max/60, 1), "h)\n\n")

cat("DURÉE MOYENNE PAR VISITE (PAR USAGER):\n")
cat("Minimum              :", round(duree_usagers_stats$duree_moy_min), "min\n")
cat("Médiane              :", round(duree_usagers_stats$duree_moy_median), "min\n")
cat("Moyenne              :", round(duree_usagers_stats$duree_moy_mean), "min\n")
cat("Maximum              :", round(duree_usagers_stats$duree_moy_max), "min\n\n")

# Distribution par catégories
cat("CATÉGORIES DE DURÉE MOYENNE:\n")
duree_categories <- data_usagers_comportement %>%
  count(categorie_duree) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n))

print(duree_categories)
cat("\n")

# 7.4 Ancienneté et fidélisation
cat("7.4 ANCIENNETÉ ET FIDÉLISATION\n")
cat(rep("-", 80), "\n")

anciennete_usagers <- data_usagers_comportement %>%
  summarise(
    anc_min = min(anciennete_jours, na.rm = TRUE),
    anc_q1 = quantile(anciennete_jours, 0.25, na.rm = TRUE),
    anc_median = median(anciennete_jours, na.rm = TRUE),
    anc_mean = mean(anciennete_jours, na.rm = TRUE),
    anc_q3 = quantile(anciennete_jours, 0.75, na.rm = TRUE),
    anc_max = max(anciennete_jours, na.rm = TRUE),
    
    pct_0_jours = sum(anciennete_jours == 0, na.rm = TRUE) / n() * 100,
    pct_moins_7j = sum(anciennete_jours <= 7, na.rm = TRUE) / n() * 100,
    pct_moins_30j = sum(anciennete_jours <= 30, na.rm = TRUE) / n() * 100,
    pct_plus_90j = sum(anciennete_jours > 90, na.rm = TRUE) / n() * 100
  )

cat("ANCIENNETÉ (PREMIÈRE À DERNIÈRE VISITE):\n")
cat("Minimum              :", anciennete_usagers$anc_min, "jours\n")
cat("1er quartile         :", round(anciennete_usagers$anc_q1), "jours\n")
cat("Médiane              :", round(anciennete_usagers$anc_median), "jours\n")
cat("Moyenne              :", round(anciennete_usagers$anc_mean, 1), "jours\n")
cat("3ème quartile        :", round(anciennete_usagers$anc_q3), "jours\n")
cat("Maximum              :", anciennete_usagers$anc_max, "jours\n\n")

cat("RÉPARTITION:\n")
cat("Usagers 0 jour (1 visite)   :", round(anciennete_usagers$pct_0_jours, 1), "%\n")
cat("Usagers ≤ 7 jours           :", round(anciennete_usagers$pct_moins_7j, 1), "%\n")
cat("Usagers ≤ 30 jours          :", round(anciennete_usagers$pct_moins_30j, 1), "%\n")
cat("Usagers > 90 jours          :", round(anciennete_usagers$pct_plus_90j, 1), "%\n\n")

# 7.5 Régularité
cat("7.5 RÉGULARITÉ DES VISITES\n")
cat(rep("-", 80), "\n")

regularite_stats <- data_usagers_comportement %>%
  filter(!is.na(regularite_jours) & regularite_jours > 0) %>%
  summarise(
    n_usagers_avec_reg = n(),
    reg_min = min(regularite_jours),
    reg_q1 = quantile(regularite_jours, 0.25),
    reg_median = median(regularite_jours),
    reg_mean = mean(regularite_jours),
    reg_q3 = quantile(regularite_jours, 0.75),
    reg_max = max(regularite_jours),
    
    # Très réguliers : ≤ 7 jours entre visites
    pct_tres_regulier = sum(regularite_jours <= 7) / n() * 100,
    # Réguliers : 7-14 jours
    pct_regulier = sum(regularite_jours > 7 & regularite_jours <= 14) / n() * 100,
    # Irréguliers : > 14 jours
    pct_irregulier = sum(regularite_jours > 14) / n() * 100
  )

cat("Usagers avec 2+ visites      :", format(regularite_stats$n_usagers_avec_reg, big.mark = " "), "\n")
cat("Écart moyen min              :", round(regularite_stats$reg_min, 1), "jours\n")
cat("1er quartile                 :", round(regularite_stats$reg_q1, 1), "jours\n")
cat("Médiane                      :", round(regularite_stats$reg_median, 1), "jours\n")
cat("Moyenne                      :", round(regularite_stats$reg_mean, 1), "jours\n")
cat("3ème quartile                :", round(regularite_stats$reg_q3, 1), "jours\n")
cat("Écart moyen max              :", round(regularite_stats$reg_max, 1), "jours\n\n")

cat("PROFILS DE RÉGULARITÉ:\n")
cat("Très régulier (≤7j)          :", round(regularite_stats$pct_tres_regulier, 1), "%\n")
cat("Régulier (7-14j)             :", round(regularite_stats$pct_regulier, 1), "%\n")
cat("Irrégulier (>14j)            :", round(regularite_stats$pct_irregulier, 1), "%\n\n")

# 7.6 Diversité spatiale
cat("7.6 DIVERSITÉ SPATIALE\n")
cat(rep("-", 80), "\n")

espaces_diversite <- data_usagers_comportement %>%
  summarise(
    espaces_min = min(nb_espaces_differents, na.rm = TRUE),
    espaces_median = median(nb_espaces_differents, na.rm = TRUE),
    espaces_mean = mean(nb_espaces_differents, na.rm = TRUE),
    espaces_max = max(nb_espaces_differents, na.rm = TRUE),
    
    pct_1_espace = sum(nb_espaces_differents == 1) / n() * 100,
    pct_2_3_espaces = sum(nb_espaces_differents >= 2 & nb_espaces_differents <= 3) / n() * 100,
    pct_4plus_espaces = sum(nb_espaces_differents >= 4) / n() * 100
  )

cat("Nombre d'espaces min         :", espaces_diversite$espaces_min, "\n")
cat("Médiane                      :", espaces_diversite$espaces_median, "\n")
cat("Moyenne                      :", round(espaces_diversite$espaces_mean, 2), "\n")
cat("Maximum                      :", espaces_diversite$espaces_max, "\n\n")

cat("DIVERSITÉ:\n")
cat("1 seul espace                :", round(espaces_diversite$pct_1_espace, 1), "%\n")
cat("2-3 espaces                  :", round(espaces_diversite$pct_2_3_espaces, 1), "%\n")
cat("4+ espaces                   :", round(espaces_diversite$pct_4plus_espaces, 1), "%\n\n")

# Top espaces préférés
cat("TOP 15 ESPACES PRÉFÉRÉS:\n")
top_espaces_pref <- data_usagers_comportement %>%
  count(espace_prefere, sort = TRUE) %>%
  head(15) %>%
  mutate(pct = n / sum(data_usagers_comportement$espace_prefere != "", na.rm = TRUE) * 100)

print(top_espaces_pref)
cat("\n")

# 7.7 Motifs principaux
cat("7.7 MOTIFS PRINCIPAUX DE VISITE\n")
cat(rep("-", 80), "\n")

top_motifs_principaux <- data_usagers_comportement %>%
  count(motif_principal, sort = TRUE) %>%
  head(15) %>%
  mutate(pct = n / sum(data_usagers_comportement$motif_principal != "", na.rm = TRUE) * 100)

print(top_motifs_principaux)
cat("\n")

# 7.8 Comportement horaire
cat("7.8 COMPORTEMENT HORAIRE\n")
cat(rep("-", 80), "\n")

horaire_stats <- data_usagers_comportement %>%
  summarise(
    heure_moy_min = min(heure_arrivee_moyenne, na.rm = TRUE),
    heure_moy_q1 = quantile(heure_arrivee_moyenne, 0.25, na.rm = TRUE),
    heure_moy_median = median(heure_arrivee_moyenne, na.rm = TRUE),
    heure_moy_mean = mean(heure_arrivee_moyenne, na.rm = TRUE),
    heure_moy_q3 = quantile(heure_arrivee_moyenne, 0.75, na.rm = TRUE),
    heure_moy_max = max(heure_arrivee_moyenne, na.rm = TRUE)
  )

cat("Heure moyenne d'arrivée:\n")
cat("Minimum                      :", round(horaire_stats$heure_moy_min, 1), "h\n")
cat("1er quartile                 :", round(horaire_stats$heure_moy_q1, 1), "h\n")
cat("Médiane                      :", round(horaire_stats$heure_moy_median, 1), "h\n")
cat("Moyenne                      :", round(horaire_stats$heure_moy_mean, 1), "h\n")
cat("3ème quartile                :", round(horaire_stats$heure_moy_q3, 1), "h\n")
cat("Maximum                      :", round(horaire_stats$heure_moy_max, 1), "h\n\n")

# Profils horaires
profils_horaires <- data_usagers_comportement %>%
  mutate(
    profil_horaire = case_when(
      heure_arrivee_moyenne < 10 ~ "Lève-tôt (avant 10h)",
      heure_arrivee_moyenne < 14 ~ "Matin/Midi (10h-14h)",
      heure_arrivee_moyenne < 18 ~ "Après-midi (14h-18h)",
      TRUE ~ "Soirée (après 18h)"
    )
  ) %>%
  count(profil_horaire) %>%
  mutate(pct = n / sum(n) * 100)

cat("PROFILS HORAIRES:\n")
print(profils_horaires)
cat("\n")

# 7.9 Visites weekend
cat("7.9 COMPORTEMENT WEEKEND\n")
cat(rep("-", 80), "\n")

weekend_stats <- data_usagers_comportement %>%
  summarise(
    total_visites_weekend = sum(nb_visites_weekend, na.rm = TRUE),
    pct_usagers_weekend = sum(nb_visites_weekend > 0, na.rm = TRUE) / n() * 100,
    pct_moyen_weekend = mean(pct_visites_weekend, na.rm = TRUE),
    nb_usagers_uniquement_weekend = sum(pct_visites_weekend == 100, na.rm = TRUE),
    nb_usagers_jamais_weekend = sum(nb_visites_weekend == 0, na.rm = TRUE)
  )

cat("Total visites weekend        :", format(weekend_stats$total_visites_weekend, big.mark = " "), "\n")
cat("% usagers venus en weekend   :", round(weekend_stats$pct_usagers_weekend, 1), "%\n")
cat("% moyen visites weekend      :", round(weekend_stats$pct_moyen_weekend, 1), "%\n")
cat("Usagers uniquement weekend   :", weekend_stats$nb_usagers_uniquement_weekend, "\n")
cat("Usagers jamais weekend       :", format(weekend_stats$nb_usagers_jamais_weekend, big.mark = " "), "\n\n")

# 7.10 Statut usager vs visiteur
cat("7.10 STATUT FINAL\n")
cat(rep("-", 80), "\n")

statut_final <- data_usagers_comportement %>%
  count(statut_final) %>%
  mutate(pct = n / sum(n) * 100)

print(statut_final)
cat("\n")

# 7.11 Analyse croisée démographique
cat("7.11 ANALYSE DÉMOGRAPHIQUE DES USAGERS\n")
cat(rep("-", 80), "\n")

demo_stats <- data_usagers_comportement %>%
  summarise(
    age_min = min(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_max = max(age, na.rm = TRUE)
  )

cat("Âge minimum                  :", demo_stats$age_min, "ans\n")
cat("Âge médian                   :", demo_stats$age_median, "ans\n")
cat("Âge moyen                    :", round(demo_stats$age_mean, 0), "ans\n")
cat("Âge maximum                  :", demo_stats$age_max, "ans\n\n")

# Genre
genre_usagers <- data_usagers_comportement %>%
  count(sex) %>%
  mutate(pct = n / sum(n) * 100)

cat("RÉPARTITION PAR GENRE:\n")
print(genre_usagers)
cat("\n")

cat(rep("=", 80), "\n")
cat("FIN SECTION 2 - ANALYSE COMPORTEMENT USAGERS\n")
cat(rep("=", 80), "\n\n")