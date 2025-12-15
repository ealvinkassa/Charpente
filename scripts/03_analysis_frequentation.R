# ============================================================================
# ANALYSE EXPLORATOIRE DÉTAILLÉE - FRÉQUENTATION DU LIEU
# ============================================================================

# Packages nécessaires
# library(tidyverse)
# library(lubridate)
# library(scales)
# library(patchwork)
# library(ggridges)
# library(viridis)

# Charger les données
data_frequentation <- readRDS("data/processed/data_frequentation.rds")
data_usagers_comportement <- readRDS("data/processed/data_usagers_comportement.rds")

# ============================================================================
# SECTION 1 : STATISTIQUES DESCRIPTIVES GÉNÉRALES
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("SECTION 1 : STATISTIQUES DESCRIPTIVES GÉNÉRALES\n")
cat(rep("=", 80), "\n\n")

# 1.1 Vue d'ensemble temporelle
cat("1.1 PÉRIODE D'OBSERVATION\n")
cat(rep("-", 80), "\n")
periode <- data_frequentation %>%
  summarise(
    date_debut = min(visit_date, na.rm = TRUE),
    date_fin = max(visit_date, na.rm = TRUE),
    nb_jours_total = as.numeric(difftime(date_fin, date_debut, units = "days")) + 1,
    nb_jours_activite = n_distinct(visit_date)
  )

cat("Date de début       :", format(periode$date_debut, "%d/%m/%Y"), "\n")
cat("Date de fin         :", format(periode$date_fin, "%d/%m/%Y"), "\n")
cat("Durée totale        :", periode$nb_jours_total, "jours\n")
cat("Jours d'activité    :", periode$nb_jours_activite, "jours\n")
cat("Taux d'ouverture    :", round(periode$nb_jours_activite / periode$nb_jours_total * 100, 0), "%\n\n")

# 1.2 Volumes globaux
cat("1.2 VOLUMES GLOBAUX\n")
cat(rep("-", 80), "\n")
volumes <- data_frequentation %>%
  summarise(
    nb_visites_total = n(),
    nb_usagers_uniques = n_distinct(phone),
    nb_espaces_utilises = n_distinct(visited_space),
    nb_motifs_differents = n_distinct(visit_reason),
    visites_par_jour = n() / n_distinct(visit_date),
    visites_par_usager = n() / n_distinct(phone)
  )

cat("Nombre total de visites       :", format(volumes$nb_visites_total, big.mark = " "), "\n")
cat("Nombre d'usagers uniques      :", format(volumes$nb_usagers_uniques, big.mark = " "), "\n")
cat("Nombre d'espaces utilisés     :", volumes$nb_espaces_utilises, "\n")
cat("Nombre de motifs différents   :", volumes$nb_motifs_differents, "\n")
cat("Moyenne visites/jour          :", round(volumes$visites_par_jour, 0), "\n")
cat("Moyenne visites/usager        :", round(volumes$visites_par_usager, 0), "\n\n")

# 1.3 Répartition par statut (usager vs visiteur)
cat("1.3 RÉPARTITION USAGERS / VISITEURS\n")
cat(rep("-", 80), "\n")
statut_repartition <- data_frequentation %>%
  count(visitor_status) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct_label = paste0(round(pct, 1), "%")
  )

print(statut_repartition)
cat("\n")

# ============================================================================
# SECTION 2 : ANALYSE DÉMOGRAPHIQUE
# ============================================================================

cat(rep("=", 80), "\n")
cat("SECTION 2 : ANALYSE DÉMOGRAPHIQUE\n")
cat(rep("=", 80), "\n\n")

# 2.1 Répartition par genre
cat("2.1 RÉPARTITION PAR GENRE\n")
cat(rep("-", 80), "\n")
genre_stats <- data_frequentation %>%
  count(sex) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct_label = paste0(round(pct, 1), "%")
  )

print(genre_stats)

# Ratio hommes/femmes
ratio_hf <- genre_stats$n[genre_stats$sex == "Masculin"] / 
  genre_stats$n[genre_stats$sex == "Feminin"]
cat("\nRatio Hommes/Femmes :", round(ratio_hf, 2), ":1\n\n")

# 2.2 Distribution des âges
cat("2.2 DISTRIBUTION DES ÂGES\n")
cat(rep("-", 80), "\n")
age_stats <- data_frequentation %>%
  filter(!is.na(age)) %>%
  summarise(
    age_min = min(age),
    age_q1 = quantile(age, 0.25),
    age_median = median(age),
    age_mean = mean(age),
    age_q3 = quantile(age, 0.75),
    age_max = max(age),
    age_sd = sd(age),
    age_cv = sd(age) / mean(age) * 100
  )

cat("Âge minimum       :", age_stats$age_min, "ans\n")
cat("1er quartile      :", age_stats$age_q1, "ans\n")
cat("Âge médian        :", age_stats$age_median, "ans\n")
cat("Âge moyen         :", round(age_stats$age_mean, 0), "ans\n")
cat("3ème quartile     :", age_stats$age_q3, "ans\n")
cat("Âge maximum       :", age_stats$age_max, "ans\n")
cat("Écart-type        :", round(age_stats$age_sd, 0), "ans\n")
cat("Coef. variation   :", round(age_stats$age_cv, 0), "%\n\n")

# Tranches d'âge
age_tranches <- data_frequentation %>%
  filter(!is.na(age)) %>%
  mutate(
    tranche_age = cut(age, 
                      breaks = c(0, 18, 25, 35, 45, 100),
                      labels = c("Moins de 18 ans", "18-25 ans", "26-35 ans", 
                                 "36-45 ans", "Plus de 45 ans"),
                      include.lowest = TRUE)
  ) %>%
  count(tranche_age) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct_cumul = cumsum(pct)
  )

cat("RÉPARTITION PAR TRANCHES D'ÂGE:\n")
print(age_tranches)
cat("\n")

# 2.3 Personnes en situation de handicap
cat("2.3 PERSONNES EN SITUATION DE HANDICAP\n")
cat(rep("-", 80), "\n")
handicap_stats <- data_frequentation %>%
  count(disability) %>%
  mutate(pct = n / sum(n) * 100)

print(handicap_stats)
cat("\n")

# ============================================================================
# SECTION 3 : ANALYSE GÉOGRAPHIQUE
# ============================================================================

cat(rep("=", 80), "\n")
cat("SECTION 3 : ANALYSE GÉOGRAPHIQUE\n")
cat(rep("=", 80), "\n\n")

# 3.1 Top villes
cat("3.1 TOP 15 VILLES D'ORIGINE\n")
cat(rep("-", 80), "\n")
top_villes <- data_frequentation %>%
  filter(!is.na(city)) %>%
  count(city, sort = TRUE) %>%
  head(15) %>%
  mutate(
    pct = n / sum(data_frequentation$city != "" & !is.na(data_frequentation$city)) * 100,
    pct_cumul = cumsum(pct)
  )

print(top_villes)
cat("\nConcentration: Les 5 premières villes représentent", 
    round(top_villes$pct_cumul[5], 1), "% des visites\n\n")

# 3.2 Diversité géographique
cat("3.2 DIVERSITÉ GÉOGRAPHIQUE\n")
cat(rep("-", 80), "\n")
geo_diversity <- data_frequentation %>%
  filter(!is.na(city)) %>%
  summarise(
    nb_villes_total = n_distinct(city),
    nb_villes_1_visite = sum(table(city) == 1),
    nb_villes_10plus = sum(table(city) >= 10),
    indice_herfindahl = sum((table(city) / n())^2)
  )

cat("Nombre total de villes        :", geo_diversity$nb_villes_total, "\n")
cat("Villes avec 1 seule visite    :", geo_diversity$nb_villes_1_visite, "\n")
cat("Villes avec 10+ visites       :", geo_diversity$nb_villes_10plus, "\n")
cat("Indice de concentration (HHI) :", round(geo_diversity$indice_herfindahl, 4), "\n\n") # Concentration des visites sur Cotonou

# ============================================================================
# SECTION 4 : ANALYSE ACADÉMIQUE
# ============================================================================

cat(rep("=", 80), "\n")
cat("SECTION 4 : ANALYSE ACADÉMIQUE\n")
cat(rep("=", 80), "\n\n")

# 4.1 Universités
cat("4.1 TOP 15 UNIVERSITÉS\n")
cat(rep("-", 80), "\n")
top_univ <- data_frequentation %>%
  filter(!is.na(university)) %>%
  count(university, sort = TRUE) %>%
  head(15) %>%
  mutate(
    pct = n / sum(!is.na(data_frequentation$university)) * 100
  )

print(top_univ)
cat("\nTaux de renseignement université:", 
    round(sum(!is.na(data_frequentation$university)) / nrow(data_frequentation) * 100, 1), 
    "%\n\n")

# 4.2 Domaines d'étude
cat("4.2 TOP 15 DOMAINES D'ÉTUDE\n")
cat(rep("-", 80), "\n")
top_domaines <- data_frequentation %>%
  filter(!is.na(field_of_study)) %>%
  count(field_of_study, sort = TRUE) %>%
  head(15) %>%
  mutate(
    pct = n / sum(!is.na(data_frequentation$field_of_study)) * 100
  )

print(top_domaines)
cat("\nTaux de renseignement domaine:", 
    round(sum(!is.na(data_frequentation$field_of_study)) / nrow(data_frequentation) * 100, 1), 
    "%\n\n")

# 4.3 Profils
cat("4.3 TOP 20 PROFILS\n")
cat(rep("-", 80), "\n")
top_profils <- data_frequentation %>%
  filter(!is.na(profile)) %>%
  count(profile, sort = TRUE) %>%
  head(20) %>%
  mutate(
    pct = n / sum(!is.na(data_frequentation$profile)) * 100
  )

print(top_profils)
cat("\n")

# ============================================================================
# SECTION 5 : ANALYSE SPATIALE (ESPACES)
# ============================================================================

cat(rep("=", 80), "\n")
cat("SECTION 5 : ANALYSE SPATIALE (ESPACES)\n")
cat(rep("=", 80), "\n\n")

# 5.1 Fréquentation par espace
cat("5.1 FRÉQUENTATION PAR ESPACE\n")
cat(rep("-", 80), "\n")
espaces_stats <- data_frequentation %>%
  count(visited_space, sort = TRUE) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct_cumul = cumsum(pct)
  )

print(espaces_stats)

cat("\nConcentration: Les 3 premiers espaces représentent", 
    round(espaces_stats$pct_cumul[3], 1), "% des visites\n\n")

# 5.2 Motifs de visite
cat("5.2 TOP 20 MOTIFS DE VISITE\n")
cat(rep("-", 80), "\n")
top_motifs <- data_frequentation %>%
  count(visit_reason, sort = TRUE) %>%
  head(20) %>%
  mutate(
    pct = n / sum(data_frequentation$visit_reason != "") * 100
  )

print(top_motifs)
cat("\n")

# ============================================================================
# SECTION 6 : ANALYSE TEMPORELLE
# ============================================================================

cat(rep("=", 80), "\n")
cat("SECTION 6 : ANALYSE TEMPORELLE\n")
cat(rep("=", 80), "\n\n")

# 6.1 Distribution par mois
cat("6.1 FRÉQUENTATION PAR MOIS\n")
cat(rep("-", 80), "\n")
freq_mois <- data_frequentation %>%
  mutate(mois = floor_date(visit_date, "month")) %>%
  count(mois) %>%
  mutate(pct = n / sum(n) * 100)

print(freq_mois)
cat("\n")

# 6.2 Distribution par jour de la semaine
cat("6.2 FRÉQUENTATION PAR JOUR DE LA SEMAINE\n")
cat(rep("-", 80), "\n")
freq_jour_semaine <- data_frequentation %>%
  mutate(
    jour_semaine = wday(visit_date, label = TRUE, abbr = FALSE, week_start = 1)
  ) %>%
  count(jour_semaine) %>%
  mutate(
    pct = n / sum(n) * 100,
    type_jour = ifelse(jour_semaine %in% c("samedi", "dimanche"), "Weekend", "Semaine")
  )

print(freq_jour_semaine)

cat("\nWeekend vs Semaine:\n")
weekend_vs_semaine <- data_frequentation %>%
  mutate(
    est_weekend = wday(visit_date) %in% c(1, 7)
  ) %>%
  count(est_weekend) %>%
  mutate(
    label = ifelse(est_weekend, "Weekend", "Semaine"),
    pct = n / sum(n) * 100
  )
print(weekend_vs_semaine)
cat("\n")

# 6.3 Heures d'arrivée
cat("6.3 HEURES D'ARRIVÉE\n")
cat(rep("-", 80), "\n")
heure_stats <- data_frequentation %>%
  mutate(heure_arrivee = hour(arrival_time)) %>%
  summarise(
    heure_min = min(heure_arrivee),
    heure_median = median(heure_arrivee),
    heure_mean = mean(heure_arrivee),
    heure_max = max(heure_arrivee)
  )

cat("Heure la plus tôt    :", heure_stats$heure_min, "h\n")
cat("Heure médiane        :", round(heure_stats$heure_median, 1), "h\n")
cat("Heure moyenne        :", round(heure_stats$heure_mean, 1), "h\n")
cat("Heure la plus tard   :", heure_stats$heure_max, "h\n\n")

# Distribution par plages horaires
plages_horaires <- data_frequentation %>%
  mutate(
    heure_arrivee = hour(arrival_time),
    plage = cut(heure_arrivee,
                breaks = c(0, 9, 12, 14, 18, 24),
                labels = c("Avant 9h", "9h-12h", "12h-14h", "14h-18h", "Après 18h"),
                include.lowest = TRUE)
  ) %>%
  count(plage) %>%
  mutate(pct = n / sum(n) * 100)

cat("DISTRIBUTION PAR PLAGES HORAIRES:\n")
print(plages_horaires)
cat("\n")

# 6.4 Durées de visite
cat("6.4 DURÉES DE VISITE\n")
cat(rep("-", 80), "\n")
duree_stats <- data_frequentation %>%
  filter(!is.na(duration_minutes)) %>%
  summarise(
    duree_min = min(duration_minutes),
    duree_q1 = quantile(duration_minutes, 0.25),
    duree_median = median(duration_minutes),
    duree_mean = mean(duration_minutes),
    duree_q3 = quantile(duration_minutes, 0.75),
    duree_max = max(duration_minutes),
    duree_sd = sd(duration_minutes),
    nb_moins_30min = sum(duration_minutes < 30),
    nb_plus_6h = sum(duration_minutes > 360)
  )

cat("Durée minimale      :", duree_stats$duree_min, "minutes\n")
cat("1er quartile        :", round(duree_stats$duree_q1), "minutes\n")
cat("Durée médiane       :", round(duree_stats$duree_median), "minutes\n")
cat("Durée moyenne       :", round(duree_stats$duree_mean), "minutes (", 
    round(duree_stats$duree_mean/60, 1), "h)\n")
cat("3ème quartile       :", round(duree_stats$duree_q3), "minutes\n")
cat("Durée maximale      :", duree_stats$duree_max, "minutes (", 
    round(duree_stats$duree_max/60, 1), "h)\n")
cat("Écart-type          :", round(duree_stats$duree_sd), "minutes\n")
cat("Visites < 30 min    :", duree_stats$nb_moins_30min, 
    "(", round(duree_stats$nb_moins_30min/nrow(data_frequentation)*100, 1), "%)\n")
cat("Visites > 6h        :", duree_stats$nb_plus_6h, 
    "(", round(duree_stats$nb_plus_6h/nrow(data_frequentation)*100, 1), "%)\n\n")

# Distribution par catégories
duree_categories <- data_frequentation %>%
  filter(!is.na(duration_minutes)) %>%
  mutate(
    categorie = cut(duration_minutes,
                    breaks = c(0, 60, 180, 360, Inf),
                    labels = c("Courte (<1h)", "Moyenne (1-3h)", 
                               "Longue (3-6h)", "Très longue (6h+)"),
                    include.lowest = TRUE)
  ) %>%
  count(categorie) %>%
  mutate(pct = n / sum(n) * 100)

cat("DISTRIBUTION PAR CATÉGORIES:\n")
print(duree_categories)
cat("\n")

# 6.5 Ancienneté des usagers au moment de la visite
cat("6.5 ANCIENNETÉ DES USAGERS\n")
cat(rep("-", 80), "\n")
anciennete_stats <- data_frequentation %>%
  filter(!is.na(seniority_days)) %>%
  summarise(
    anc_median = median(seniority_days),
    anc_mean = mean(seniority_days),
    anc_max = max(seniority_days),
    pct_nouveaux = sum(seniority_days == 0) / n() * 100,
    pct_moins_7j = sum(seniority_days <= 7) / n() * 100,
    pct_moins_30j = sum(seniority_days <= 30) / n() * 100
  )

cat("Ancienneté médiane            :", round(anciennete_stats$anc_median), "jours\n")
cat("Ancienneté moyenne            :", round(anciennete_stats$anc_mean), "jours\n")
cat("Ancienneté maximale           :", anciennete_stats$anc_max, "jours\n")
cat("% visites le jour inscription :", round(anciennete_stats$pct_nouveaux, 1), "%\n")
cat("% visites < 7 jours          :", round(anciennete_stats$pct_moins_7j, 1), "%\n")
cat("% visites < 30 jours         :", round(anciennete_stats$pct_moins_30j, 1), "%\n\n")

cat(rep("=", 80), "\n")
cat("FIN SECTION 1 - ANALYSE FRÉQUENTATION\n")
cat(rep("=", 80), "\n\n")