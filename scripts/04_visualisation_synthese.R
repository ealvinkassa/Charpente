# ============================================================================
# RAPPORT EXÉCUTIF - SYNTHÈSE STRATÉGIQUE
# ============================================================================

# library(tidyverse)
# library(lubridate)
# library(scales)

# Charger les données
data_frequentation <- readRDS("data/processed/data_frequentation.rds")
data_usagers_comportement <- readRDS("data/processed/data_usagers_comportement.rds")

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                         ║\n")
cat("║                        RAPPORT EXÉCUTIF DE SYNTHÈSE                     ║\n")
cat("║                     ANALYSE DE FRÉQUENTATION ET D'ENGAGEMENT            ║\n")
cat("║                                                                         ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# SECTION 1 : CHIFFRES CLÉS
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ 1. CHIFFRES CLÉS                                                      │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

# Calculer les KPIs principaux
kpis <- list(
  # Volumes
  total_visites = nrow(data_frequentation),
  total_usagers = n_distinct(data_frequentation$phone),
  periode_jours = as.numeric(difftime(max(data_frequentation$visit_date), 
                                      min(data_frequentation$visit_date), 
                                      units = "days")) + 1,
  
  # Moyennes
  visites_par_jour = nrow(data_frequentation) / n_distinct(data_frequentation$visit_date),
  visites_par_usager = nrow(data_frequentation) / n_distinct(data_frequentation$phone),
  duree_moy_visite = mean(data_frequentation$duration_minutes, na.rm = TRUE),
  
  # Engagement
  taux_retention = sum(data_usagers_comportement$nb_visites > 1) / 
    nrow(data_usagers_comportement) * 100,
  score_engagement_moyen = mean(data_usagers_comportement$score_engagement, na.rm = TRUE),
  
  # Démographie
  age_moyen = mean(data_usagers_comportement$age, na.rm = TRUE),
  pct_femmes = sum(data_usagers_comportement$sex == "Feminin", na.rm = TRUE) / 
    sum(!is.na(data_usagers_comportement$sex)) * 100
)

cat("📊 VOLUMES\n")
cat("   • Total de visites              :", format(kpis$total_visites, big.mark = " "), "\n")
cat("   • Usagers uniques               :", format(kpis$total_usagers, big.mark = " "), "\n")
cat("   • Période d'observation         :", kpis$periode_jours, "jours\n")
cat("   • Moyenne visites/jour          :", round(kpis$visites_par_jour, 1), "\n\n")

cat("🎯 ENGAGEMENT\n")
cat("   • Visites par usager (moyenne)  :", round(kpis$visites_par_usager, 1), "\n")
cat("   • Durée moyenne par visite      :", round(kpis$duree_moy_visite), "min (", 
    round(kpis$duree_moy_visite/60, 1), "h)\n")
cat("   • Taux de rétention             :", round(kpis$taux_retention, 1), "%\n")
cat("   • Score d'engagement moyen      :", round(kpis$score_engagement_moyen, 1), "/100\n\n")

cat("👥 DÉMOGRAPHIE\n")
cat("   • Âge moyen                     :", round(kpis$age_moyen, 1), "ans\n")
cat("   • Proportion de femmes          :", round(kpis$pct_femmes, 1), "%\n\n")

# ============================================================================
# SECTION 2 : FAITS SAILLANTS
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ 2. FAITS SAILLANTS                                                    │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

# Top espace
top_espace <- data_frequentation %>% count(visited_space, sort = TRUE) %>% slice(1)
cat("🏆 ESPACE LE PLUS POPULAIRE\n")
cat("   ", as.character(top_espace$visited_space), "(", top_espace$n, "visites)\n\n")

# Jour le plus actif
jour_actif <- data_frequentation %>%
  mutate(jour = wday(visit_date, label = TRUE, abbr = FALSE, week_start = 1)) %>%
  count(jour, sort = TRUE) %>%
  slice(1)
cat("📅 JOUR LE PLUS ACTIF\n")
cat("   ", as.character(jour_actif$jour), "(", jour_actif$n, "visites)\n\n")

# Heure de pointe
heure_pointe <- data_frequentation %>%
  mutate(heure = hour(arrival_time)) %>%
  count(heure, sort = TRUE) %>%
  slice(1)
cat("⏰ HEURE DE POINTE\n")
cat("   ", heure_pointe$heure, "h (", heure_pointe$n, "arrivées)\n\n")

# Ville principale
ville_top <- data_frequentation %>%
  filter(!is.na(city)) %>%
  count(city, sort = TRUE) %>%
  slice(1)
cat("🌍 VILLE D'ORIGINE PRINCIPALE\n")
cat("   ", as.character(ville_top$city), "(", ville_top$n, "visites)\n\n")

# ============================================================================
# SECTION 3 : SEGMENTATION DES USAGERS
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ 3. SEGMENTATION DES USAGERS                                           │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

segments <- data_usagers_comportement %>%
  mutate(
    segment = case_when(
      nb_visites == 1 ~ "Occasionnel",
      nb_visites <= 5 ~ "Explorateur",
      nb_visites <= 10 ~ "Régulier",
      TRUE ~ "Fidèle"
    )
  ) %>%
  group_by(segment) %>%
  summarise(
    nb_usagers = n(),
    pct = n() / nrow(data_usagers_comportement) * 100,
    total_visites = sum(nb_visites),
    pct_visites = total_visites / sum(data_usagers_comportement$nb_visites) * 100,
    duree_moy = mean(duree_moyenne_visite),
    score_moy = mean(score_engagement, na.rm = TRUE)
  ) %>%
  arrange(match(segment, c("Occasionnel", "Explorateur", "Régulier", "Fidèle")))

for(i in 1:nrow(segments)) {
  cat("■", segments$segment[i], "\n")
  cat("   • Usagers                       :", format(segments$nb_usagers[i], big.mark = " "),
      "(", round(segments$pct[i], 1), "% du total)\n")
  cat("   • Génèrent                      :", format(segments$total_visites[i], big.mark = " "),
      "visites (", round(segments$pct_visites[i], 1), "%)\n")
  cat("   • Durée moyenne/visite          :", round(segments$duree_moy[i]), "min\n")
  cat("   • Score engagement              :", round(segments$score_moy[i], 1), "/100\n\n")
}

# ============================================================================
# SECTION 4 : PERFORMANCE TEMPORELLE
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ 4. PERFORMANCE TEMPORELLE                                             │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

# Semaine vs Weekend
tempo <- data_frequentation %>%
  mutate(est_weekend = wday(visit_date) %in% c(1, 7)) %>%
  group_by(est_weekend) %>%
  summarise(
    nb_visites = n(),
    pct = n() / nrow(data_frequentation) * 100,
    duree_moy = mean(duration_minutes, na.rm = TRUE)
  )

cat("📊 SEMAINE vs WEEKEND\n")
cat("   • Visites en semaine            :", 
    format(tempo$nb_visites[!tempo$est_weekend], big.mark = " "),
    "(", round(tempo$pct[!tempo$est_weekend], 1), "%)\n")
cat("   • Visites en weekend            :", 
    format(tempo$nb_visites[tempo$est_weekend], big.mark = " "),
    "(", round(tempo$pct[tempo$est_weekend], 1), "%)\n\n")

# Distribution horaire
cat("🕐 RÉPARTITION HORAIRE\n")
plages <- data_frequentation %>%
  mutate(
    plage = cut(hour(arrival_time),
                breaks = c(0, 9, 12, 14, 18, 24),
                labels = c("Avant 9h", "9h-12h", "12h-14h", "14h-18h", "Après 18h"))
  ) %>%
  count(plage) %>%
  mutate(pct = n / sum(n) * 100)

for(i in 1:nrow(plages)) {
  cat("   •", as.character(plages$plage[i]), ":", 
      sprintf("%5s", format(plages$n[i], big.mark = " ")),
      "visites (", sprintf("%4.1f", plages$pct[i]), "%)\n")
}
cat("\n")

# ============================================================================
# SECTION 5 : DIVERSITÉ ET COMPORTEMENTS
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ 5. DIVERSITÉ ET COMPORTEMENTS                                         │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

# Espaces
cat("📍 UTILISATION DES ESPACES\n")
cat("   • Nombre d'espaces actifs       :", n_distinct(data_frequentation$visited_space), "\n")
cat("   • Espaces/usager (moyenne)      :", 
    round(mean(data_usagers_comportement$nb_espaces_differents), 1), "\n")

mono_vs_multi <- data_usagers_comportement %>%
  summarise(
    mono = sum(nb_espaces_differents == 1),
    multi = sum(nb_espaces_differents > 1)
  )

cat("   • Usagers mono-espace           :", format(mono_vs_multi$mono, big.mark = " "),
    "(", round(mono_vs_multi$mono/(mono_vs_multi$mono + mono_vs_multi$multi)*100, 1), "%)\n")
cat("   • Usagers multi-espaces         :", format(mono_vs_multi$multi, big.mark = " "),
    "(", round(mono_vs_multi$multi/(mono_vs_multi$mono + mono_vs_multi$multi)*100, 1), "%)\n\n")

# Origines géographiques
cat("🌐 DIVERSITÉ GÉOGRAPHIQUE\n")
cat("   • Nombre de villes              :", n_distinct(data_frequentation$city, na.rm = TRUE), "\n")

concentration <- data_frequentation %>%
  filter(!is.na(city)) %>%
  count(city, sort = TRUE) %>%
  mutate(pct_cumul = cumsum(n) / sum(n) * 100)

top5_villes <- sum(concentration$pct_cumul[1:5])
cat("   • Concentration (Top 5 villes)  :", round(top5_villes, 1), "% des visites\n\n")

# ============================================================================
# SECTION 6 : OPPORTUNITÉS STRATÉGIQUES
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ 6. OPPORTUNITÉS STRATÉGIQUES                                          │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

# Potentiel de conversion
occasionnels <- sum(data_usagers_comportement$nb_visites == 1)
explorateurs <- sum(data_usagers_comportement$nb_visites >= 2 & 
                      data_usagers_comportement$nb_visites <= 5)

cat("💡 CONVERSION\n")
cat("   • Occasionnels à convertir      :", format(occasionnels, big.mark = " "),
    "usagers (potentiel élevé)\n")
cat("   • Explorateurs à fidéliser      :", format(explorateurs, big.mark = " "),
    "usagers\n\n")

# Usagers à risque
date_ref <- max(data_usagers_comportement$derniere_visite, na.rm = TRUE)
a_risque <- data_usagers_comportement %>%
  filter(
    nb_visites >= 3,
    as.numeric(difftime(date_ref, derniere_visite, units = "days")) > 30
  ) %>%
  nrow()

cat("⚠️  RÉTENTION\n")
cat("   • Usagers réguliers à risque    :", format(a_risque, big.mark = " "),
    "usagers (3+ visites, >30j inactifs)\n\n")

# Optimisation spatiale
espaces_sous_utilises <- data_frequentation %>%
  count(visited_space) %>%
  filter(n < 50) %>%
  nrow()

cat("🏢 OPTIMISATION SPATIALE\n")
cat("   • Espaces sous-utilisés         :", espaces_sous_utilises,
    "espaces (<50 visites)\n")
cat("   • Action recommandée            : Promotion ciblée ou réaffectation\n\n")

# ============================================================================
# SECTION 7 : RECOMMANDATIONS PRIORITAIRES
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ 7. RECOMMANDATIONS PRIORITAIRES                                       │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

cat("🎯 PRIORITÉ 1 : FIDÉLISATION DES OCCASIONNELS\n")
cat("   Action : Programme d'onboarding renforcé pour les primo-visiteurs\n")
cat("   Impact : Potentiel de", format(occasionnels, big.mark = " "), 
    "usagers à convertir\n\n")

cat("🎯 PRIORITÉ 2 : RÉENGAGEMENT DES USAGERS INACTIFS\n")
cat("   Action : Campagne de réactivation ciblée (emails, offres spéciales)\n")
cat("   Impact :", format(a_risque, big.mark = " "), 
    "usagers réguliers à reconquérir\n\n")

cat("🎯 PRIORITÉ 3 : OPTIMISATION DES PLAGES HORAIRES CREUSES\n")
cat("   Action : Événements/ateliers aux heures creuses (avant 9h, après 18h)\n")
cat("   Impact : Meilleure utilisation de la capacité\n\n")

cat("🎯 PRIORITÉ 4 : DIVERSIFICATION DES USAGES\n")
cat("   Action : Parcours découverte multi-espaces\n")
cat("   Impact :", format(mono_vs_multi$mono, big.mark = " "), 
    "usagers mono-espace à faire évoluer\n\n")

# ============================================================================
# FOOTER
# ============================================================================

cat("┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ MÉTHODOLOGIE                                                          │\n")
cat("└─────────────────────────────────────────────────────────────────────────────┘\n\n")

cat("Ce rapport est basé sur l'analyse de", format(kpis$total_visites, big.mark = " "),
    "visites de", format(kpis$total_usagers, big.mark = " "), "usagers\n")
cat("sur une période de", kpis$periode_jours, "jours.\n\n")

cat("Les données ont été nettoyées, harmonisées et enrichies pour garantir\n")
cat("la fiabilité des analyses. Le score d'engagement est calculé sur la base\n")
cat("de multiples critères : fréquence, durée, diversité et régularité.\n\n")

cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                            FIN DU RAPPORT                               ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n\n")