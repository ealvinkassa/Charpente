# ============================================================================
# VISUALISATIONS - ANALYSE FRÉQUENTATION
# ============================================================================

# library(tidyverse)
# library(lubridate)
# library(scales)
# library(patchwork)
# library(ggridges)
# library(viridis)

# Charger les données
data_frequentation <- readRDS("data/processed/data_frequentation.rds")

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
# VIZ 1 : ÉVOLUTION TEMPORELLE DE LA FRÉQUENTATION
# ============================================================================

# 1.1 Fréquentation journalière
p1 <- data_frequentation %>%
  count(visit_date) %>%
  ggplot(aes(x = visit_date, y = n)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  geom_point(color = "#2E86AB", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#A23B72", 
              fill = "#A23B72", alpha = 0.2) +
  labs(
    title = "Évolution de la fréquentation quotidienne",
    subtitle = paste0("Du ", format(min(data_frequentation$visit_date), "%d/%m/%Y"),
                      " au ", format(max(data_frequentation$visit_date), "%d/%m/%Y")),
    x = "Date",
    y = "Nombre de visites"
  ) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/01_frequentation_journaliere.png", p1, width = 12, height = 6, dpi = 300)

# 1.2 Fréquentation par mois
p2 <- data_frequentation %>%
  mutate(mois = floor_date(visit_date, "month")) %>%
  count(mois) %>%
  ggplot(aes(x = mois, y = n)) +
  geom_col(fill = "#06A77D", alpha = 0.8) +
  geom_text(aes(label = comma(n)), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    title = "Fréquentation mensuelle",
    x = "Mois",
    y = "Nombre de visites"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_custom +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/02_frequentation_mensuelle.png", p2, width = 10, height = 6, dpi = 300)

# 1.3 Fréquentation par jour de la semaine
p3 <- data_frequentation %>%
  mutate(
    jour_semaine = wday(visit_date, label = TRUE, abbr = FALSE, week_start = 1),
    est_weekend = wday(visit_date) %in% c(1, 7)
  ) %>%
  count(jour_semaine, est_weekend) %>%
  mutate(jour_semaine = fct_inorder(jour_semaine)) %>%
  ggplot(aes(x = jour_semaine, y = n, fill = est_weekend)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = comma(n)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(
    values = c("FALSE" = "#2E86AB", "TRUE" = "#F18F01"),
    labels = c("Semaine", "Weekend"),
    name = ""
  ) +
  labs(
    title = "Fréquentation par jour de la semaine",
    x = "Jour",
    y = "Nombre de visites"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_custom +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/03_frequentation_jour_semaine.png", p3, width = 10, height = 6, dpi = 300)

# ============================================================================
# VIZ 2 : ANALYSE HORAIRE
# ============================================================================

# 2.1 Distribution des heures d'arrivée
p4 <- data_frequentation %>%
  mutate(heure_arrivee = hour(arrival_time)) %>%
  ggplot(aes(x = heure_arrivee)) +
  geom_histogram(binwidth = 1, fill = "#06A77D", color = "white", alpha = 0.8) +
  geom_density(aes(y = after_stat(count)), color = "#A23B72", 
               linewidth = 1.2, adjust = 1.5) +
  labs(
    title = "Distribution des heures d'arrivée",
    x = "Heure d'arrivée",
    y = "Nombre de visites"
  ) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste0(seq(0, 23, 2), "h")) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/04_distribution_heures_arrivee.png", p4, width = 12, height = 6, dpi = 300)

# 2.2 Heatmap jour x heure
p5 <- data_frequentation %>%
  mutate(
    jour_semaine = wday(visit_date, label = TRUE, abbr = FALSE, week_start = 1),
    heure_arrivee = hour(arrival_time)
  ) %>%
  count(jour_semaine, heure_arrivee) %>%
  ggplot(aes(x = heure_arrivee, y = jour_semaine, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), color = "white", fontface = "bold", size = 3) +
  scale_fill_viridis_c(option = "plasma", name = "Visites") +
  labs(
    title = "Carte de chaleur : Jour × Heure",
    x = "Heure d'arrivée",
    y = "Jour de la semaine"
  ) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste0(seq(0, 23, 2), "h")) +
  theme_custom +
  theme(legend.position = "right")

ggsave("outputs/figures/05_heatmap_jour_heure.png", p5, width = 14, height = 7, dpi = 300)

# ============================================================================
# VIZ 3 : DURÉES DE VISITE
# ============================================================================

# 3.1 Distribution des durées
p6 <- data_frequentation %>%
  filter(!is.na(duration_minutes), duration_minutes <= 600) %>%
  ggplot(aes(x = duration_minutes)) +
  geom_histogram(bins = 50, fill = "#2E86AB", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = median(duration_minutes, na.rm = TRUE)),
             color = "#A23B72", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = median(data_frequentation$duration_minutes, na.rm = TRUE) + 30,
           y = Inf, label = paste("Médiane:", 
                                  round(median(data_frequentation$duration_minutes, na.rm = TRUE)), "min"),
           vjust = 2, hjust = 0, color = "#A23B72", fontface = "bold") +
  labs(
    title = "Distribution des durées de visite",
    subtitle = "Visites de moins de 10 heures",
    x = "Durée (minutes)",
    y = "Nombre de visites"
  ) +
  scale_x_continuous(breaks = seq(0, 600, 60)) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/06_distribution_durees.png", p6, width = 12, height = 6, dpi = 300)

# 3.2 Boxplot durées par jour de semaine
p7 <- data_frequentation %>%
  filter(!is.na(duration_minutes), duration_minutes <= 600) %>%
  mutate(jour_semaine = wday(visit_date, label = TRUE, abbr = FALSE, week_start = 1)) %>%
  ggplot(aes(x = jour_semaine, y = duration_minutes, fill = jour_semaine)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "red", color = "darkred") +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Durées de visite par jour de la semaine",
    subtitle = "Losange rouge = moyenne",
    x = "Jour",
    y = "Durée (minutes)"
  ) +
  theme_custom +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/07_durees_par_jour.png", p7, width = 10, height = 6, dpi = 300)

# 3.3 Durée par catégorie
p8 <- data_frequentation %>%
  filter(!is.na(duration_minutes)) %>%
  mutate(
    categorie = cut(duration_minutes,
                    breaks = c(0, 60, 180, 360, Inf),
                    labels = c("Courte\n(<1h)", "Moyenne\n(1-3h)", 
                               "Longue\n(3-6h)", "Très longue\n(6h+)"),
                    include.lowest = TRUE)
  ) %>%
  count(categorie) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = categorie, y = n, fill = categorie)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")),
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#F18F01", "#06A77D", "#2E86AB", "#A23B72")) +
  labs(
    title = "Répartition des visites par catégorie de durée",
    x = "Catégorie",
    y = "Nombre de visites"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/08_categories_duree.png", p8, width = 10, height = 6, dpi = 300)

# ============================================================================
# VIZ 4 : ANALYSE DÉMOGRAPHIQUE
# ============================================================================

# 4.1 Pyramide des âges
p9 <- data_frequentation %>%
  filter(!is.na(age), !is.na(sex)) %>%
  mutate(
    tranche_age = cut(age, breaks = seq(15, 75, 5), include.lowest = TRUE)
  ) %>%
  count(tranche_age, sex) %>%
  mutate(n = ifelse(sex == "Masculin", -n, n)) %>%
  ggplot(aes(x = tranche_age, y = n, fill = sex)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Masculin" = "#2E86AB", "Feminin" = "#A23B72"),
                    name = "") +
  scale_y_continuous(labels = function(x) abs(x)) +
  labs(
    title = "Pyramide des âges des visiteurs",
    x = "Tranche d'âge",
    y = "Nombre de visites"
  ) +
  theme_custom

ggsave("outputs/figures/09_pyramide_ages.png", p9, width = 10, height = 8, dpi = 300)

# 4.2 Distribution des âges par genre
p10 <- data_frequentation %>%
  filter(!is.na(age), !is.na(sex)) %>%
  ggplot(aes(x = age, fill = sex, color = sex)) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(values = c("Masculin" = "#2E86AB", "Feminin" = "#A23B72")) +
  scale_color_manual(values = c("Masculin" = "#2E86AB", "Feminin" = "#A23B72")) +
  labs(
    title = "Distribution des âges par genre",
    x = "Âge (années)",
    y = "Densité",
    fill = "Genre",
    color = "Genre"
  ) +
  theme_custom

ggsave("outputs/figures/10_distribution_ages_genre.png", p10, width = 12, height = 6, dpi = 300)

# ============================================================================
# VIZ 5 : ANALYSE SPATIALE
# ============================================================================

# 5.1 Top espaces visités
p11 <- data_frequentation %>%
  count(visited_space, sort = TRUE) %>%
  head(15) %>%
  mutate(visited_space = fct_reorder(visited_space, n)) %>%
  ggplot(aes(x = n, y = visited_space, fill = n)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = comma(n)), hjust = -0.2, fontface = "bold") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Top 15 des espaces les plus visités",
    x = "Nombre de visites",
    y = NULL
  ) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/11_top_espaces.png", p11, width = 12, height = 8, dpi = 300)

# 5.2 Durée moyenne par espace
p12 <- data_frequentation %>%
  filter(!is.na(duration_minutes)) %>%
  group_by(visited_space) %>%
  summarise(
    n = n(),
    duree_moy = mean(duration_minutes)
  ) %>%
  filter(n >= 50) %>%
  mutate(visited_space = fct_reorder(visited_space, duree_moy)) %>%
  ggplot(aes(x = duree_moy, y = visited_space, fill = duree_moy)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(duree_moy), " min")), 
            hjust = -0.1, fontface = "bold", size = 3.5) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Durée moyenne de visite par espace",
    subtitle = "Espaces avec au moins 50 visites",
    x = "Durée moyenne (minutes)",
    y = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/12_duree_par_espace.png", p12, width = 12, height = 8, dpi = 300)

# ============================================================================
# VIZ 6 : ANALYSE GÉOGRAPHIQUE
# ============================================================================

# 6.1 Top villes
p13 <- data_frequentation %>%
  filter(!is.na(city)) %>%
  count(city, sort = TRUE) %>%
  head(20) %>%
  mutate(city = fct_reorder(city, n)) %>%
  ggplot(aes(x = n, y = city, fill = n)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = comma(n)), hjust = -0.2, fontface = "bold") +
  scale_fill_gradient(low = "#06A77D", high = "#2E86AB") +
  labs(
    title = "Top 20 des villes d'origine des visiteurs",
    x = "Nombre de visites",
    y = NULL
  ) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/13_top_villes.png", p13, width = 12, height = 9, dpi = 300)

# ============================================================================
# VIZ 7 : MOTIFS DE VISITE
# ============================================================================

# 7.1 Top motifs
p14 <- data_frequentation %>%
  count(visit_reason, sort = TRUE) %>%
  head(15) %>%
  mutate(
    visit_reason = str_wrap(visit_reason, 40),
    visit_reason = fct_reorder(visit_reason, n)
  ) %>%
  ggplot(aes(x = n, y = visit_reason, fill = n)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = comma(n)), hjust = -0.2, fontface = "bold") +
  scale_fill_viridis_c(option = "cividis") +
  labs(
    title = "Top 15 des motifs de visite",
    x = "Nombre de visites",
    y = NULL
  ) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_custom +
  theme(legend.position = "none")

ggsave("outputs/figures/14_top_motifs.png", p14, width = 12, height = 8, dpi = 300)

# ============================================================================
# VIZ 8 : ANCIENNETÉ
# ============================================================================

# 8.1 Distribution de l'ancienneté
p15 <- data_frequentation %>%
  filter(!is.na(seniority_days), seniority_days <= 100) %>%
  ggplot(aes(x = seniority_days)) +
  geom_histogram(bins = 50, fill = "#06A77D", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, color = "#A23B72", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = 5, y = Inf, 
           label = paste0("Visites le jour\nd'inscription: ",
                          sum(data_frequentation$seniority_days == 0, na.rm = TRUE)),
           vjust = 2, hjust = 0, color = "#A23B72", fontface = "bold") +
  labs(
    title = "Distribution de l'ancienneté au moment de la visite",
    subtitle = "Ancienneté ≤ 100 jours",
    x = "Ancienneté (jours)",
    y = "Nombre de visites"
  ) +
  scale_y_continuous(labels = comma) +
  theme_custom

ggsave("outputs/figures/15_distribution_anciennete.png", p15, width = 12, height = 6, dpi = 300)

# ============================================================================
# VIZ 9 : PANELS COMBINÉS
# ============================================================================

# 9.1 Vue d'ensemble genre
p16_1 <- data_frequentation %>%
  count(sex) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = sex)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Masculin" = "#2E86AB", "Feminin" = "#A23B72")) +
  labs(title = "Répartition par genre", fill = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "bottom")

p16_2 <- data_frequentation %>%
  mutate(est_weekend = wday(visit_date) %in% c(1, 7)) %>%
  count(est_weekend) %>%
  mutate(
    label = ifelse(est_weekend, "Weekend", "Semaine"),
    pct = n / sum(n) * 100
  ) %>%
  ggplot(aes(x = "", y = pct, fill = label)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Semaine" = "#06A77D", "Weekend" = "#F18F01")) +
  labs(title = "Semaine vs Weekend", fill = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "bottom")

p16_3 <- data_frequentation %>%
  count(visitor_status) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = visitor_status)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 5) +
  scale_fill_manual(values = c("usager" = "#2E86AB", "visiteur" = "#A23B72")) +
  labs(title = "Usagers vs Visiteurs", fill = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "bottom")

p16 <- p16_1 + p16_2 + p16_3 +
  plot_annotation(
    title = "Vue d'ensemble des caractéristiques de fréquentation",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

ggsave("outputs/figures/16_vue_ensemble.png", p16, width = 15, height = 6, dpi = 300)

cat("\n✅ Toutes les visualisations ont été générées et sauvegardées dans outputs/\n\n")