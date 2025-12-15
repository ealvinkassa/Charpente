# ============================================================================
# VISUALISATIONS - COMPORTEMENT DES USAGERS
# ============================================================================

# library(tidyverse)
# library(scales)
# library(patchwork)
# library(ggridges)
# library(viridis)

# Charger les données
data_usagers_comportement <- readRDS("data/processed/data_usagers_comportement.rds")

# Thème personnalisé
theme_custom <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ============================================================================
# VIZ 10 : NOMBRE DE VISITES
# ============================================================================

# 10.1 Distribution du nombre de visites
p17 <- data_usagers_comportement %>%
  filter(nb_visites <= 30) %>%
  ggplot(aes(x = nb_visites)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", alpha = 0.8, color = "white") +
  geom_vline(aes(xintercept = median(nb_visites)),
             color = "#A23B72", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = median(data_usagers_comportement$nb_visites) + 2,
           y = Inf, 
           label = paste("Médiane:", median(data_usagers_comportement$nb_visites)),
           vjust = 2, hjust = 0, color = "#A23B72", fontface = "bold") +
  labs(
    title = "Distribution du nombre de visites par usager",
    subtitle = "Usagers avec ≤ 30 visites",
    x = "Nombre de visites",
    y = "Nombre d'usagers"
  ) +
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/17_distribution_nb_visites.png", p17, width = 12, height = 6, dpi = 300)

# 10.2 Catégories d'engagement
p18 <- data_usagers_comportement %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Occasionnel (1 visite)", 
                                             "Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  count(categorie_nb_visites) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = categorie_nb_visites, y = n, fill = categorie_nb_visites)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Répartition des usagers par catégorie d'engagement",
    x = NULL,
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  theme_custom +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/figures/18_categories_engagement.png", p18, width = 12, height = 7, dpi = 300)

# ============================================================================
# VIZ 11 : SCORES D'ENGAGEMENT
# ============================================================================

# 11.1 Distribution du score d'engagement
p19 <- data_usagers_comportement %>%
  filter(!is.na(score_engagement)) %>%
  ggplot(aes(x = score_engagement)) +
  geom_histogram(bins = 50, fill = "#06A77D", alpha = 0.8, color = "white") +
  geom_vline(aes(xintercept = median(score_engagement, na.rm = TRUE)),
             color = "#A23B72", linewidth = 1.2, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(score_engagement, na.rm = TRUE)),
             color = "#F18F01", linewidth = 1.2, linetype = "dotted") +
  annotate("text", x = median(data_usagers_comportement$score_engagement, na.rm = TRUE),
           y = Inf, label = "Médiane", vjust = 2, color = "#A23B72", 
           fontface = "bold", hjust = -0.1) +
  annotate("text", x = mean(data_usagers_comportement$score_engagement, na.rm = TRUE),
           y = Inf, label = "Moyenne", vjust = 4, color = "#F18F01", 
           fontface = "bold", hjust = -0.1) +
  labs(
    title = "Distribution du score d'engagement",
    x = "Score d'engagement (0-100)",
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/19_distribution_score_engagement.png", p19, width = 12, height = 6, dpi = 300)

# 11.2 Score d'engagement par catégorie de visites
p20 <- data_usagers_comportement %>%
  filter(!is.na(score_engagement)) %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Occasionnel (1 visite)", 
                                             "Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  ggplot(aes(x = categorie_nb_visites, y = score_engagement, 
             fill = categorie_nb_visites)) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Score d'engagement par catégorie d'usagers",
    x = NULL,
    y = "Score d'engagement"
  ) +
  theme_custom +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/figures/20_score_par_categorie.png", p20, width = 12, height = 7, dpi = 300)

# ============================================================================
# VIZ 12 : DURÉES
# ============================================================================

# 12.1 Durée totale cumulée
p21 <- data_usagers_comportement %>%
  filter(duree_totale_minutes <= 3000) %>%
  ggplot(aes(x = duree_totale_minutes / 60)) +
  geom_histogram(bins = 50, fill = "#2E86AB", alpha = 0.8, color = "white") +
  geom_vline(aes(xintercept = median(duree_totale_minutes / 60, na.rm = TRUE)),
             color = "#A23B72", linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Distribution de la durée totale cumulée par usager",
    subtitle = "Usagers avec ≤ 50h cumulées",
    x = "Durée totale cumulée (heures)",
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/21_distribution_duree_totale.png", p21, width = 12, height = 6, dpi = 300)

# 12.2 Durée moyenne par visite selon catégorie
p22 <- data_usagers_comportement %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Occasionnel (1 visite)", 
                                             "Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  ggplot(aes(x = categorie_nb_visites, y = duree_moyenne_visite, 
             fill = categorie_nb_visites)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "red", color = "darkred") +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Durée moyenne de visite par catégorie d'usagers",
    subtitle = "Losange rouge = moyenne",
    x = NULL,
    y = "Durée moyenne par visite (minutes)"
  ) +
  theme_custom +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/figures/22_duree_moy_par_categorie.png", p22, width = 12, height = 7, dpi = 300)

# 12.3 Catégories de durée
p23 <- data_usagers_comportement %>%
  mutate(
    categorie_duree = factor(categorie_duree,
                             levels = c("Courte (<1h)", "Moyenne (1-3h)",
                                        "Longue (3-6h)", "Très longue (6h+)"))
  ) %>%
  count(categorie_duree) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = categorie_duree, y = n, fill = categorie_duree)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Répartition des usagers par catégorie de durée moyenne",
    x = NULL,
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  theme_custom +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/figures/23_categories_duree.png", p23, width = 12, height = 7, dpi = 300)

# ============================================================================
# VIZ 13 : ANCIENNETÉ ET FIDÉLISATION
# ============================================================================

# 13.1 Distribution de l'ancienneté
p24 <- data_usagers_comportement %>%
  filter(anciennete_jours <= 120) %>%
  ggplot(aes(x = anciennete_jours)) +
  geom_histogram(bins = 50, fill = "#06A77D", alpha = 0.8, color = "white") +
  geom_vline(aes(xintercept = median(anciennete_jours, na.rm = TRUE)),
             color = "#A23B72", linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Distribution de l'ancienneté des usagers",
    subtitle = "Ancienneté ≤ 120 jours",
    x = "Ancienneté (jours entre première et dernière visite)",
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/24_distribution_anciennete.png", p24, width = 12, height = 6, dpi = 300)

# 13.2 Relation visites × ancienneté
p25 <- data_usagers_comportement %>%
  filter(nb_visites <= 50, anciennete_jours <= 150) %>%
  ggplot(aes(x = anciennete_jours, y = nb_visites)) +
  geom_point(alpha = 0.3, color = "#2E86AB", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72", 
              fill = "#A23B72", alpha = 0.2, linewidth = 1.2) +
  labs(
    title = "Relation entre ancienneté et nombre de visites",
    subtitle = "Usagers avec ≤50 visites et ≤150 jours d'ancienneté",
    x = "Ancienneté (jours)",
    y = "Nombre de visites"
  ) +
  theme_custom

ggsave("outputs/figures/25_visites_vs_anciennete.png", p25, width = 12, height = 7, dpi = 300)

# 13.3 Taux de rétention par cohorte
p26 <- data_usagers_comportement %>%
  mutate(
    mois_inscription = floor_date(registration_date, "month"),
    a_revenu = nb_visites > 1
  ) %>%
  group_by(mois_inscription) %>%
  summarise(
    nb_usagers = n(),
    nb_revenus = sum(a_revenu),
    taux_retention = nb_revenus / nb_usagers * 100
  ) %>%
  filter(nb_usagers >= 20) %>%
  ggplot(aes(x = mois_inscription, y = taux_retention)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_point(aes(size = nb_usagers), color = "#2E86AB", alpha = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Taux de rétention par cohorte d'inscription",
    subtitle = "Pourcentage d'usagers revenus au moins une fois",
    x = "Mois d'inscription",
    y = "Taux de rétention (%)",
    size = "Nb usagers"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_custom +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/26_retention_par_cohorte.png", p26, width = 14, height = 7, dpi = 300)

# ============================================================================
# VIZ 14 : RÉGULARITÉ
# ============================================================================

# 14.1 Distribution de la régularité
p27 <- data_usagers_comportement %>%
  filter(!is.na(regularite_jours), regularite_jours > 0, regularite_jours <= 60) %>%
  ggplot(aes(x = regularite_jours)) +
  geom_histogram(bins = 50, fill = "#06A77D", alpha = 0.8, color = "white") +
  geom_vline(xintercept = 7, color = "#A23B72", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = 14, color = "#F18F01", linewidth = 1, linetype = "dashed") +
  annotate("text", x = 7, y = Inf, label = "Très régulier", 
           vjust = 2, hjust = 1.1, color = "#A23B72", fontface = "bold") +
  annotate("text", x = 14, y = Inf, label = "Régulier", 
           vjust = 2, hjust = -0.1, color = "#F18F01", fontface = "bold") +
  labs(
    title = "Distribution de la régularité des visites",
    subtitle = "Écart moyen entre visites (usagers avec 2+ visites, ≤60 jours)",
    x = "Écart moyen entre visites (jours)",
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/27_distribution_regularite.png", p27, width = 12, height = 6, dpi = 300)

# 14.2 Régularité par catégorie d'engagement
p28 <- data_usagers_comportement %>%
  filter(!is.na(regularite_jours), regularite_jours > 0) %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  ggplot(aes(x = categorie_nb_visites, y = regularite_jours, 
             fill = categorie_nb_visites)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Régularité des visites par catégorie d'engagement",
    subtitle = "Plus l'écart est faible, plus l'usager est régulier",
    x = NULL,
    y = "Écart moyen entre visites (jours)"
  ) +
  theme_custom +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/figures/28_regularite_par_categorie.png", p28, width = 12, height = 7, dpi = 300)

# ============================================================================
# VIZ 15 : DIVERSITÉ SPATIALE
# ============================================================================

# 15.1 Distribution du nombre d'espaces
p29 <- data_usagers_comportement %>%
  count(nb_espaces_differents) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(nb_espaces_differents), y = n, fill = nb_espaces_differents)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")),
            vjust = -0.3, fontface = "bold", size = 3.5) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Nombre d'espaces différents visités par usager",
    x = "Nombre d'espaces différents",
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/29_nb_espaces_differents.png", p29, width = 12, height = 7, dpi = 300)

# 15.2 Diversité spatiale par catégorie d'engagement
p30 <- data_usagers_comportement %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Occasionnel (1 visite)", 
                                             "Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  ggplot(aes(x = categorie_nb_visites, y = nb_espaces_differents, 
             fill = categorie_nb_visites)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "red", color = "darkred") +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Diversité spatiale par catégorie d'engagement",
    subtitle = "Losange rouge = moyenne",
    x = NULL,
    y = "Nombre d'espaces différents visités"
  ) +
  theme_custom +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/figures/30_diversite_par_categorie.png", p30, width = 12, height = 7, dpi = 300)

# 15.3 Top espaces préférés
p31 <- data_usagers_comportement %>%
  count(espace_prefere, sort = TRUE) %>%
  head(15) %>%
  mutate(espace_prefere = fct_reorder(espace_prefere, n)) %>%
  ggplot(aes(x = n, y = espace_prefere, fill = n)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = comma(n)), hjust = -0.2, fontface = "bold") +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Top 15 des espaces préférés",
    subtitle = "Espace le plus visité par chaque usager",
    x = "Nombre d'usagers",
    y = NULL
  ) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/31_top_espaces_preferes.png", p31, width = 12, height = 8, dpi = 300)

# ============================================================================
# VIZ 16 : COMPORTEMENT HORAIRE
# ============================================================================

# 16.1 Distribution des heures moyennes d'arrivée
p32 <- data_usagers_comportement %>%
  filter(!is.na(heure_arrivee_moyenne)) %>%
  ggplot(aes(x = heure_arrivee_moyenne)) +
  geom_histogram(bins = 40, fill = "#2E86AB", alpha = 0.8, color = "white") +
  geom_density(aes(y = after_stat(count) * 0.5), color = "#A23B72", 
               linewidth = 1.2, adjust = 1.5) +
  labs(
    title = "Distribution des heures moyennes d'arrivée par usager",
    x = "Heure moyenne d'arrivée",
    y = "Nombre d'usagers"
  ) +
  scale_x_continuous(breaks = seq(0, 24, 2), labels = paste0(seq(0, 24, 2), "h")) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/32_heures_moy_arrivee.png", p32, width = 12, height = 6, dpi = 300)

# 16.2 Profils horaires
p33 <- data_usagers_comportement %>%
  filter(!is.na(heure_arrivee_moyenne)) %>%
  mutate(
    profil_horaire = case_when(
      heure_arrivee_moyenne < 10 ~ "Lève-tôt\n(avant 10h)",
      heure_arrivee_moyenne < 14 ~ "Matin/Midi\n(10h-14h)",
      heure_arrivee_moyenne < 18 ~ "Après-midi\n(14h-18h)",
      TRUE ~ "Soirée\n(après 18h)"
    ),
    profil_horaire = factor(profil_horaire,
                            levels = c("Lève-tôt\n(avant 10h)", 
                                       "Matin/Midi\n(10h-14h)",
                                       "Après-midi\n(14h-18h)", 
                                       "Soirée\n(après 18h)"))
  ) %>%
  count(profil_horaire) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = profil_horaire, y = n, fill = profil_horaire)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Profils horaires des usagers",
    x = NULL,
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/33_profils_horaires.png", p33, width = 12, height = 7, dpi = 300)

# ============================================================================
# VIZ 17 : COMPORTEMENT WEEKEND
# ============================================================================

# 17.1 Distribution du % de visites weekend
p34 <- data_usagers_comportement %>%
  filter(nb_visites >= 2) %>%
  ggplot(aes(x = pct_visites_weekend)) +
  geom_histogram(bins = 50, fill = "#F18F01", alpha = 0.8, color = "white") +
  geom_vline(xintercept = 50, color = "#A23B72", linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Distribution du pourcentage de visites en weekend",
    subtitle = "Usagers avec 2+ visites",
    x = "% de visites en weekend",
    y = "Nombre d'usagers"
  ) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/34_pct_visites_weekend.png", p34, width = 12, height = 6, dpi = 300)

# ============================================================================
# VIZ 18 : ANALYSES CROISÉES
# ============================================================================

# 18.1 Matrice de corrélation (variables comportementales)
library(corrplot)
png("outputs/figures/35_matrice_correlation.png", width = 12, height = 12, units = "in", res = 300)

cor_data <- data_usagers_comportement %>%
  select(nb_visites, duree_totale_minutes, duree_moyenne_visite,
         nb_espaces_differents, anciennete_jours, regularite_jours,
         heure_arrivee_moyenne, pct_visites_weekend, score_engagement, age) %>%
  filter(complete.cases(.)) %>%
  cor()

corrplot(cor_data, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         col = colorRampPalette(c("#A23B72", "white", "#2E86AB"))(200),
         title = "Matrice de corrélation - Variables comportementales",
         mar = c(0,0,2,0))

dev.off()

# 18.2 Relation visites × durée totale par catégorie
p36 <- data_usagers_comportement %>%
  filter(nb_visites <= 50, duree_totale_minutes <= 5000) %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Occasionnel (1 visite)", 
                                             "Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  ggplot(aes(x = nb_visites, y = duree_totale_minutes / 60, 
             color = categorie_nb_visites)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Relation entre nombre de visites et durée totale cumulée",
    x = "Nombre de visites",
    y = "Durée totale cumulée (heures)",
    color = "Catégorie"
  ) +
  theme_custom

ggsave("outputs/figures/36_visites_vs_duree_totale.png", p36, width = 12, height = 7, dpi = 300)

# 18.3 Segmentation engagement × durée
p37 <- data_usagers_comportement %>%
  filter(!is.na(score_engagement)) %>%
  mutate(
    niveau_engagement = case_when(
      score_engagement < 33 ~ "Faible",
      score_engagement < 66 ~ "Moyen",
      TRUE ~ "Fort"
    ),
    niveau_engagement = factor(niveau_engagement, 
                               levels = c("Faible", "Moyen", "Fort")),
    categorie_duree = factor(categorie_duree,
                             levels = c("Courte (<1h)", "Moyenne (1-3h)",
                                        "Longue (3-6h)", "Très longue (6h+)"))
  ) %>%
  count(niveau_engagement, categorie_duree) %>%
  group_by(niveau_engagement) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = niveau_engagement, y = pct, fill = categorie_duree)) +
  geom_col(position = "fill", alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_fill(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Composition des catégories de durée par niveau d'engagement",
    x = "Niveau d'engagement",
    y = "Proportion",
    fill = "Catégorie de durée"
  ) +
  theme_custom

ggsave("outputs/figures/37_engagement_vs_duree.png", p37, width = 12, height = 7, dpi = 300)

# 18.4 Âge × Nombre de visites
p38 <- data_usagers_comportement %>%
  filter(!is.na(age), nb_visites <= 30) %>%
  mutate(
    tranche_age = cut(age, 
                      breaks = c(0, 20, 25, 30, 40, 100),
                      labels = c("<20 ans", "20-25 ans", "26-30 ans", 
                                 "31-40 ans", ">40 ans"))
  ) %>%
  ggplot(aes(x = tranche_age, y = nb_visites, fill = tranche_age)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Nombre de visites par tranche d'âge",
    x = "Tranche d'âge",
    y = "Nombre de visites"
  ) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/38_age_vs_visites.png", p38, width = 12, height = 7, dpi = 300)

# ============================================================================
# VIZ 19 : ANALYSES PAR GENRE
# ============================================================================

# 19.1 Engagement par genre
p39 <- data_usagers_comportement %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = sex, y = nb_visites, fill = sex)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "red", color = "darkred") +
  scale_fill_manual(values = c("Masculin" = "#2E86AB", "Feminin" = "#A23B72")) +
  labs(
    title = "Nombre de visites par genre",
    subtitle = "Losange rouge = moyenne",
    x = "Genre",
    y = "Nombre de visites"
  ) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/39_visites_par_genre.png", p39, width = 10, height = 7, dpi = 300)

# 19.2 Durée moyenne par genre
p40 <- data_usagers_comportement %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = sex, y = duree_moyenne_visite, fill = sex)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "red", color = "darkred") +
  scale_fill_manual(values = c("Masculin" = "#2E86AB", "Feminin" = "#A23B72")) +
  labs(
    title = "Durée moyenne de visite par genre",
    subtitle = "Losange rouge = moyenne",
    x = "Genre",
    y = "Durée moyenne par visite (minutes)"
  ) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/40_duree_par_genre.png", p40, width = 10, height = 7, dpi = 300)

# 19.3 Score d'engagement par genre
p41 <- data_usagers_comportement %>%
  filter(!is.na(sex), !is.na(score_engagement)) %>%
  ggplot(aes(x = sex, y = score_engagement, fill = sex)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "red", color = "darkred") +
  scale_fill_manual(values = c("Masculin" = "#2E86AB", "Feminin" = "#A23B72")) +
  labs(
    title = "Score d'engagement par genre",
    subtitle = "Losange rouge = moyenne",
    x = "Genre",
    y = "Score d'engagement"
  ) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/41_engagement_par_genre.png", p41, width = 10, height = 7, dpi = 300)

# 19.4 Catégories d'engagement par genre
p42 <- data_usagers_comportement %>%
  filter(!is.na(sex)) %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Occasionnel (1 visite)", 
                                             "Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  count(sex, categorie_nb_visites) %>%
  group_by(sex) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sex, y = pct, fill = categorie_nb_visites)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3, fontface = "bold", size = 3) +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Distribution des catégories d'engagement par genre",
    x = "Genre",
    y = "Pourcentage (%)",
    fill = "Catégorie"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_custom +
  theme(legend.position = "bottom")

ggsave("outputs/figures/42_categories_par_genre.png", p42, width = 12, height = 8, dpi = 300)

# ============================================================================
# VIZ 20 : PANELS RÉCAPITULATIFS
# ============================================================================

# 20.1 Dashboard comportemental
p43_1 <- data_usagers_comportement %>%
  mutate(
    categorie_nb_visites = factor(categorie_nb_visites,
                                  levels = c("Occasionnel (1 visite)", 
                                             "Explorateur (2-5)", 
                                             "Régulier (6-10)", 
                                             "Fidèle (11+)"))
  ) %>%
  count(categorie_nb_visites) %>%
  ggplot(aes(x = "", y = n, fill = categorie_nb_visites)) +
  geom_col(width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(title = "Catégories d'engagement", fill = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

p43_2 <- data_usagers_comportement %>%
  filter(!is.na(score_engagement)) %>%
  mutate(
    niveau = cut(score_engagement, 
                 breaks = c(0, 33, 66, 100),
                 labels = c("Faible", "Moyen", "Fort"))
  ) %>%
  count(niveau) %>%
  ggplot(aes(x = "", y = n, fill = niveau)) +
  geom_col(width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c("#F18F01", "#F18F01", "#2E86AB")) +
  labs(title = "Niveaux d'engagement", fill = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

p43_3 <- data_usagers_comportement %>%
  count(statut_final) %>%
  ggplot(aes(x = "", y = n, fill = statut_final)) +
  geom_col(width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c("usager" = "#2E86AB", "visiteur" = "#A23B72")) +
  labs(title = "Statut final", fill = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

p43_4 <- data_usagers_comportement %>%
  mutate(
    diversite = case_when(
      nb_espaces_differents == 1 ~ "1 espace",
      nb_espaces_differents <= 3 ~ "2-3 espaces",
      TRUE ~ "4+ espaces"
    )
  ) %>%
  count(diversite) %>%
  ggplot(aes(x = "", y = n, fill = diversite)) +
  geom_col(width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB")) +
  labs(title = "Diversité spatiale", fill = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

p43 <- (p43_1 + p43_2) / (p43_3 + p43_4) +
  plot_annotation(
    title = "Dashboard récapitulatif - Comportement des usagers",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

ggsave("outputs/figures/43_dashboard_comportement.png", p43, width = 14, height = 10, dpi = 300)

# 20.2 Métriques clés
metriques <- data_usagers_comportement %>%
  summarise(
    nb_total_usagers = n(),
    nb_moy_visites = round(mean(nb_visites), 1),
    duree_moy_totale = round(mean(duree_totale_minutes) / 60, 1),
    score_moy_engagement = round(mean(score_engagement, na.rm = TRUE), 1),
    taux_retention = round(sum(nb_visites > 1) / n() * 100, 1),
    nb_espaces_moy = round(mean(nb_espaces_differents), 1)
  )

# Créer un graphique de métriques clés
p44 <- data.frame(
  metrique = c("Usagers", "Visites/usager", "Heures cumulées",
               "Score engagement", "Taux rétention %", "Espaces visités"),
  valeur = c(metriques$nb_total_usagers, 
             metriques$nb_moy_visites,
             metriques$duree_moy_totale,
             metriques$score_moy_engagement,
             metriques$taux_retention,
             metriques$nb_espaces_moy),
  couleur = c("#2E86AB", "#06A77D", "#F18F01", "#A23B72", "#2E86AB", "#06A77D")
) %>%
  ggplot(aes(x = fct_inorder(metrique), y = valeur, fill = couleur)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = format(valeur, big.mark = " ")), 
            vjust = -0.5, fontface = "bold", size = 6) +
  scale_fill_identity() +
  labs(
    title = "Métriques clés - Comportement des usagers",
    x = NULL,
    y = "Valeur"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_custom +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, face = "bold", size = 11))

ggsave("outputs/figures/44_metriques_cles.png", p44, width = 14, height = 7, dpi = 300)

cat("\n✅ Toutes les visualisations comportementales ont été générées!\n\n")