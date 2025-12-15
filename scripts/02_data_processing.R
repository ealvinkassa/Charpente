# ==============================================================================
# PIPELINE COMPLET - NETTOYAGE ET PRÉPARATION DES DONNÉES
# De l'importation brute aux bases de travail finales
# ==============================================================================

# PACKAGES ====================================================================

# packages <- c(
#   "openxlsx", "lubridate", "hms", "dplyr", "janitor", 
#   "stringr", "stringdist", "tidyverse"
# )
# 
# # Installation et chargement
# for (pkg in packages) {
#   if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
#     install.packages(pkg, dependencies = TRUE)
#     library(pkg, character.only = TRUE)
#   }
# }
# 
# cat("✓ Tous les packages chargés avec succès\n\n")

# ==============================================================================
# ÉTAPE 1 : IMPORTATION DES DONNÉES BRUTES
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 1 : IMPORTATION DES DONNÉES BRUTES\n")
cat("================================================================================\n\n")

data_presences_raw <- read.xlsx("data/raw/presences_scop.xlsx", 
                                detectDates = TRUE,
                                sep.names = "_", 
                                na.strings = "NA")

data_usagers_raw <- read.xlsx("data/raw/usagers_scop.xlsx",
                              detectDates = TRUE,
                              sep.names = "_", 
                              na.strings = "NA")

cat("Dimensions data_presences :", nrow(data_presences_raw), "lignes x", 
    ncol(data_presences_raw), "colonnes\n")
cat("Dimensions data_usagers   :", nrow(data_usagers_raw), "lignes x", 
    ncol(data_usagers_raw), "colonnes\n\n")

# ==============================================================================
# ÉTAPE 2 : NETTOYAGE DE data_presences
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 2 : NETTOYAGE DE data_presences\n")
cat("================================================================================\n\n")

# Fonction de nettoyage HTML
clean_html_entities <- function(x) {
  if (!is.character(x)) return(x)
  
  replacements <- c(
    "&amp;" = "&", "&lt;" = "<", "&gt;" = ">",
    "&quot;" = "\"", "&#39;" = "'", "&apos;" = "'",
    "&nbsp;" = " ", "&eacute;" = "é", "&Eacute;" = "É",
    "&egrave;" = "è", "&agrave;" = "à", "&acirc;" = "â",
    "&ocirc;" = "ô", "&ucirc;" = "û", "&icirc;" = "î",
    "&iuml;" = "ï", "&euml;" = "ë", "&ccedil;" = "ç"
  )
  
  for (entity in names(replacements)) {
    x <- gsub(entity, replacements[[entity]], x, fixed = TRUE)
  }
  
  # Entités numériques décimales
  while (any(grepl("&#[0-9]+;", x, perl = TRUE))) {
    matches <- gregexpr("&#([0-9]+);", x, perl = TRUE)
    for (i in seq_along(x)) {
      if (matches[[i]][1] != -1) {
        all_matches <- regmatches(x[i], matches[i])[[1]]
        replacements_vec <- sapply(all_matches, function(m) {
          num <- sub("&#([0-9]+);", "\\1", m, perl = TRUE)
          intToUtf8(as.integer(num))
        })
        regmatches(x[i], matches[i]) <- list(replacements_vec)
      }
    }
  }
  
  # Entités hexadécimales
  while (any(grepl("&#x[0-9A-Fa-f]+;", x, perl = TRUE))) {
    hex_matches <- gregexpr("&#x([0-9A-Fa-f]+);", x, perl = TRUE)
    for (i in seq_along(x)) {
      if (hex_matches[[i]][1] != -1) {
        all_hex <- regmatches(x[i], hex_matches[i])[[1]]
        replacements_hex <- sapply(all_hex, function(m) {
          hx <- sub("&#x([0-9A-Fa-f]+);", "\\1", m, perl = TRUE)
          intToUtf8(strtoi(hx, base = 16))
        })
        regmatches(x[i], hex_matches[i]) <- list(replacements_hex)
      }
    }
  }
  
  return(x)
}

# Nettoyage
data_presences <- data_presences_raw %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), ""))) %>%
  mutate(across(where(is.character), clean_html_entities)) %>%
  rename(
    last_name      = nom,
    first_name     = prenom,
    visited_space  = espace,
    sex            = sexe,
    phone          = telephone,
    visit_reason   = motif,
    visitor_status = statut,
    visit_date     = date,
    arrival_time   = heure_d_apos_arrivee,
    departure_time = heure_de_depart
  ) %>%
  mutate(
    # Noms en Title Case
    last_name  = str_to_title(last_name),
    first_name = str_to_title(first_name),
    
    # Types temporels
    visit_date     = as.Date(visit_date),
    arrival_time   = hms::parse_hm(arrival_time),
    departure_time = hms::parse_hm(departure_time),
    
    # Facteurs (pas d'harmonisation nécessaire)
    sex            = factor(sex, levels = c("Masculin", "Feminin")),
    visited_space  = factor(visited_space),
    visit_reason   = factor(visit_reason),
    visitor_status = factor(visitor_status),
    
    # Téléphone en character
    phone = as.character(phone)
  )

cat("✓ data_presences nettoyé\n")
cat("  - Entités HTML supprimées\n")
cat("  - Noms standardisés (Title Case)\n")
cat("  - Types de données convertis\n\n")

# ==============================================================================
# ÉTAPE 3 : NETTOYAGE ET HARMONISATION DE data_usagers
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 3 : NETTOYAGE ET HARMONISATION DE data_usagers\n")
cat("================================================================================\n\n")

# 3.1 Nettoyage de base
data_usagers <- data_usagers_raw %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), ""))) %>%
  rename(
    last_name         = nom,
    first_name        = prenom_s,
    phone             = telephone,
    city              = nationalite,
    university        = universite,
    field_of_study    = filliere,
    profile           = profil,
    sex               = sexe,
    birth_date        = date_de_naissance,
    disability        = handicap,
    email             = email,
    status            = statut,
    registration_date = date_d_apos_enregistrement,
    registration_time = heure_d_apos_enregistrement
  ) %>%
  mutate(
    # Noms en Title Case
    last_name  = str_to_title(last_name),
    first_name = str_to_title(first_name),
    
    # Dates
    birth_date        = as.Date(birth_date, format = "%d-%m-%Y"),
    registration_date = as.Date(registration_date),
    registration_time = hms::parse_hms(registration_time),
    
    # Facteurs
    sex        = factor(sex, levels = c("Masculin", "Feminin")),
    disability = factor(disability, levels = c("NON", "OUI")),
    status     = factor(status),
    
    # Variables à harmoniser (rester en character pour le moment)
    university     = as.character(university),
    city           = as.character(city),
    profile        = as.character(profile),
    field_of_study = as.character(field_of_study),
    
    # Autres
    phone = as.character(phone),
    email = tolower(trimws(email))
  )

cat("✓ data_usagers nettoyé (base)\n\n")

# 3.2 Fonction d'harmonisation avancée avec détection d'acronymes

harmonize_with_acronyms <- function(data, var_name, threshold = 0.15, 
                                    invalid_values = NULL,
                                    manual_acronyms = NULL) {
  
  cat("------------------------------------------------------------\n")
  cat("HARMONISATION DE:", toupper(var_name), "\n")
  cat("------------------------------------------------------------\n")
  
  # Extraire valeurs uniques
  values_raw <- data %>%
    filter(!is.na(.data[[var_name]]), trimws(.data[[var_name]]) != "") %>%
    pull(.data[[var_name]]) %>%
    unique()
  
  cat("Valeurs uniques avant harmonisation:", length(values_raw), "\n")
  
  # Filtrer valeurs invalides
  if (!is.null(invalid_values)) {
    invalid_mask <- tolower(values_raw) %in% tolower(invalid_values)
    values_raw <- values_raw[!invalid_mask]
  }
  
  # Normaliser pour comparaison
  values_normalized <- tolower(trimws(values_raw))
  names(values_normalized) <- values_raw
  
  # PHASE 1 : Détection d'acronymes ==========================================
  
  # Fonction pour extraire acronyme potentiel
  extract_acronym <- function(text) {
    # Prendre premières lettres de chaque mot
    words <- strsplit(text, "[[:space:]-]+")[[1]]
    words <- words[nchar(words) > 0]
    acronym <- paste0(substring(words, 1, 1), collapse = "")
    toupper(acronym)
  }
  
  # Créer mapping acronymes
  acronym_mapping <- list()
  
  # Acronymes manuels fournis
  if (!is.null(manual_acronyms)) {
    for (acronym in names(manual_acronyms)) {
      full_name <- manual_acronyms[[acronym]]
      acronym_mapping[[tolower(acronym)]] <- full_name
    }
  }
  
  # Détection automatique d'acronymes
  for (val in values_raw) {
    val_lower <- tolower(trimws(val))
    
    # Si c'est un mot court en majuscules (potentiel acronyme)
    if (nchar(val) <= 6 && val == toupper(val)) {
      
      # Chercher les noms complets qui pourraient correspondre
      for (candidate in values_raw) {
        if (nchar(candidate) > nchar(val) * 2) {  # Nom significativement plus long
          candidate_acronym <- extract_acronym(tolower(candidate))
          
          if (tolower(val) == tolower(candidate_acronym)) {
            acronym_mapping[[val_lower]] <- candidate
            cat("  ✓ Acronyme détecté:", val, "→", candidate, "\n")
            break
          }
        }
      }
    }
  }
  
  # PHASE 2 : Distance de chaînes ============================================
  
  distance_matrix <- stringdistmatrix(values_normalized, values_normalized, 
                                      method = "jw")
  rownames(distance_matrix) <- values_raw
  colnames(distance_matrix) <- values_raw
  
  similar_pairs <- which(distance_matrix < threshold & distance_matrix > 0, 
                         arr.ind = TRUE)
  similar_pairs <- similar_pairs[similar_pairs[,1] < similar_pairs[,2], , 
                                 drop = FALSE]
  
  # Créer groupes
  groups <- as.list(values_raw)
  names(groups) <- values_raw
  
  if (nrow(similar_pairs) > 0) {
    paires_df <- data.frame(
      valeur1 = rownames(distance_matrix)[similar_pairs[,1]],
      valeur2 = rownames(distance_matrix)[similar_pairs[,2]],
      distance = distance_matrix[similar_pairs]
    ) %>% arrange(distance)
    
    for (i in 1:nrow(paires_df)) {
      v1 <- paires_df$valeur1[i]
      v2 <- paires_df$valeur2[i]
      
      group1 <- names(groups)[sapply(groups, function(g) v1 %in% g)]
      group2 <- names(groups)[sapply(groups, function(g) v2 %in% g)]
      
      if (length(group1) > 0 && length(group2) > 0 && group1 != group2) {
        groups[[group1]] <- c(groups[[group1]], groups[[group2]])
        groups[[group2]] <- NULL
      }
    }
    
    cat("Paires similaires détectées:", nrow(paires_df), "\n")
  }
  
  # PHASE 3 : Création du dictionnaire final =================================
  
  # Fonction pour choisir le meilleur représentant
  choose_best <- function(group, data, var_name) {
    counts <- data %>%
      filter(.data[[var_name]] %in% group) %>%
      count(.data[[var_name]], sort = TRUE)
    
    best <- counts %>%
      mutate(
        name_length = nchar(.data[[var_name]]),
        starts_upper = grepl("^[A-Z]", .data[[var_name]]),
        all_upper = .data[[var_name]] == toupper(.data[[var_name]]) & name_length <= 6,
        clean_end = !grepl("[[:space:]]+$|[[:punct:]]+$", .data[[var_name]]),
        no_digits = !grepl("[0-9]", .data[[var_name]]),
        quality_score = (as.integer(starts_upper) * 2) + 
          (as.integer(!all_upper) * 3) +
          (as.integer(clean_end) * 2) +
          (as.integer(no_digits) * 1)
      ) %>%
      arrange(desc(n), desc(quality_score), desc(name_length)) %>%
      slice(1) %>%
      pull(.data[[var_name]])
    
    return(best)
  }
  
  # Créer dictionnaire
  mapping_dict <- list()
  
  for (group in groups) {
    if (length(group) > 1) {
      standard_name <- choose_best(group, data, var_name)
      for (variant in group) {
        mapping_dict[[variant]] <- standard_name
      }
    } else {
      mapping_dict[[group]] <- str_to_title(trimws(group))
    }
  }
  
  # Ajouter mappings d'acronymes
  for (acronym_key in names(acronym_mapping)) {
    # Trouver toutes les variations de cet acronyme dans values_raw
    matches <- values_raw[tolower(values_raw) == acronym_key]
    for (match in matches) {
      mapping_dict[[match]] <- acronym_mapping[[acronym_key]]
    }
  }
  
  # PHASE 4 : Application ===============================
  
  harmonize_value <- function(value, mapping, invalid_vals, acronym_map) {
    if (is.na(value)) return(NA_character_)
    
    value_clean <- trimws(value)
    if (value_clean == "") return(NA_character_)
    
    # Valeurs invalides
    if (!is.null(invalid_vals) && tolower(value_clean) %in% tolower(invalid_vals)) {
      return(NA_character_)
    }
    
    # Vérifier acronymes d'abord
    if (!is.null(acronym_map) && tolower(value_clean) %in% names(acronym_map)) {
      return(acronym_map[[tolower(value_clean)]])
    }
    
    # Mapping standard
    if (value_clean %in% names(mapping)) {
      return(mapping[[value_clean]])
    }
    
    return(str_to_title(value_clean))
  }
  
  new_col <- paste0(var_name, "_harm")
  
  data[[new_col]] <- sapply(data[[var_name]], 
                            harmonize_value, 
                            mapping = mapping_dict,
                            invalid_vals = invalid_values,
                            acronym_map = acronym_mapping)
  
  # Statistiques
  n_before <- length(unique(data[[var_name]][!is.na(data[[var_name]])]))
  n_after <- length(unique(data[[new_col]][!is.na(data[[new_col]])]))
  
  cat("Valeurs uniques après harmonisation:", n_after, "\n")
  cat("Réduction:", n_before - n_after, "variations\n")
  cat("Taux de réduction:", round((n_before - n_after) / n_before * 100, 1), "%\n\n")
  
  return(data)
}

# 3.3 Application de l'harmonisation

# Acronymes manuels connus
acronyms_university <- list(
  "uac" = "Université d'Abomey-Calavi",
  "una" = "Université Nationale d'Agriculture",
  "unstim" = "Université Nationale des Sciences",
  "ucao" = "Université Catholique de l'Afrique de l'Ouest",
  "eig" = "École Internationale de Graphisme du Bénin",
  "ism" = "Institut Supérieur de Management",
  "ine" = "Institut National de l'Eau",
  "fsa" = "Faculté des Sciences Agronomiques",
  "faseg" = "Faculté des Sciences Économiques et de Gestion",
  "fashs" = "Faculté des Sciences Humaines et Sociales",
  "flash" = "Faculté des Lettres Arts et Sciences Humaines",
  "fllac" = "Faculté des Lettres Langues Arts et Communication"
)

# University
data_usagers <- harmonize_with_acronyms(
  data = data_usagers,
  var_name = "university",
  threshold = 0.15,
  invalid_values = c("oui", "université", "pas d'université", "none", "na", "n/a"),
  manual_acronyms = acronyms_university
)

# City
data_usagers <- harmonize_with_acronyms(
  data = data_usagers,
  var_name = "city",
  threshold = 0.15,
  invalid_values = c("na", "n/a", "none")
)

# Profile
data_usagers <- harmonize_with_acronyms(
  data = data_usagers,
  var_name = "profile",
  threshold = 0.15,
  invalid_values = c("none", "inconnu", "na", "n/a")
)

# Field of study
data_usagers <- harmonize_with_acronyms(
  data = data_usagers,
  var_name = "field_of_study",
  threshold = 0.15,
  invalid_values = c("none", "na", "n/a")
)

# Remplacer colonnes originales
data_usagers <- data_usagers %>%
  mutate(
    university     = university_harm,
    city           = city_harm,
    profile        = profile_harm,
    field_of_study = field_of_study_harm
  ) %>%
  select(-university_harm, -city_harm, -profile_harm, -field_of_study_harm)

cat("✓ Harmonisation complète terminée pour data_usagers\n\n")

# ==============================================================================
# ÉTAPE 4 : VÉRIFICATION DE LA QUALITÉ DE JOINTURE
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 4 : VÉRIFICATION DE LA QUALITÉ DE JOINTURE\n")
cat("================================================================================\n\n")

# Téléphones uniques
phones_presences <- unique(data_presences$phone[!is.na(data_presences$phone)])
phones_usagers <- unique(data_usagers$phone[!is.na(data_usagers$phone)])

# Correspondances
phones_communs <- intersect(phones_presences, phones_usagers)
taux_match <- length(phones_communs) / length(phones_presences) * 100

cat("Téléphones uniques dans presences:", length(phones_presences), "\n")
cat("Téléphones uniques dans usagers  :", length(phones_usagers), "\n")
cat("Téléphones en commun             :", length(phones_communs), "\n")
cat("Taux de correspondance           :", round(taux_match, 2), "%\n\n")

# Visites enrichissables
visites_enrichies <- sum(data_presences$phone %in% phones_usagers, na.rm = TRUE)
cat("Visites qui seront enrichies     :", visites_enrichies, "/", 
    nrow(data_presences), 
    "(", round(visites_enrichies / nrow(data_presences) * 100, 1), "%)\n\n")

# Vérifier doublons dans usagers
doublons <- data_usagers %>%
  count(phone) %>%
  filter(n > 1)

if (nrow(doublons) > 0) {
  cat("⚠️ ATTENTION:", nrow(doublons), "téléphones en double dans data_usagers!\n")
  print(doublons)
  cat("\n")
} else {
  cat("✓ Aucun doublon détecté dans data_usagers\n\n")
}

# ==============================================================================
# ÉTAPE 5 : CRÉATION DE data_frequentation (BASE DE TRAVAIL 1)
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 5 : CRÉATION DE data_frequentation\n")
cat("================================================================================\n\n")

data_frequentation <- data_presences %>%
  left_join(
    data_usagers %>% select(-last_name, -first_name, -sex, -status),
    by = "phone",
    suffix = c("", "_usager")
  ) %>%
  mutate(
    # Durée de visite
    duration_minutes = as.numeric(difftime(departure_time, arrival_time, 
                                           units = "mins")),
    
    # Âge au moment de la visite
    age = ifelse(
      !is.na(birth_date),
      floor(time_length(interval(birth_date, visit_date), "years")),
      NA_integer_
    ),
    
    # Ancienneté (jours depuis inscription)
    seniority_days = ifelse(
      !is.na(registration_date),
      as.numeric(visit_date - registration_date),
      NA_real_
    )
  ) %>%
  # Factoriser variables harmonisées
  mutate(
    university     = factor(university),
    city           = factor(city),
    profile        = factor(profile),
    field_of_study = factor(field_of_study)
  )

cat("✓ data_frequentation créé\n")
cat("  Dimensions:", nrow(data_frequentation), "lignes x", 
    ncol(data_frequentation), "colonnes\n")
cat("  Variables calculées: duration_minutes, age, seniority_days\n\n")


# Résumé des NA
na_summary <- data.frame(
  variable = names(data_frequentation),
  nb_na = sapply(data_frequentation, function(x) sum(is.na(x))),
  pct_na = round(sapply(data_frequentation, function(x) 
    sum(is.na(x)) / length(x) * 100), 1)
) %>%
  arrange(desc(nb_na)) %>%
  filter(nb_na > 0)

cat("Valeurs manquantes par variable:\n")
print(na_summary, row.names = FALSE)
cat("\n")

# ==============================================================================
# ÉTAPE 6 : CRÉATION DE data_usagers_comportement (BASE DE TRAVAIL 2)
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 6 : CRÉATION DE data_usagers_comportement\n")
cat("================================================================================\n\n")

data_usagers_comportement <- data_frequentation %>%
  group_by(phone) %>%
  summarise(
    # Identité (prendre la première occurrence)
    last_name  = first(last_name),
    first_name = first(first_name),
    sex        = first(sex),
    age        = first(age),
    birth_date = first(birth_date),
    city       = first(city),
    university = first(university),
    field_of_study = first(field_of_study),
    profile    = first(profile),
    disability = first(disability),
    registration_date = first(registration_date),
    
    # Métriques comportementales
    nb_visites = n(),
    premiere_visite = min(visit_date, na.rm = TRUE),
    derniere_visite = max(visit_date, na.rm = TRUE),
    anciennete_jours = as.numeric(max(visit_date, na.rm = TRUE) - 
                                    min(visit_date, na.rm = TRUE)),
    
    # Durées
    duree_totale_minutes = sum(duration_minutes, na.rm = TRUE),
    duree_moyenne_visite = mean(duration_minutes, na.rm = TRUE),
    duree_mediane_visite = median(duration_minutes, na.rm = TRUE),
    
    # Espaces
    nb_espaces_differents = n_distinct(visited_space, na.rm = TRUE),
    espace_prefere = names(sort(table(visited_space), decreasing = TRUE))[1],
    
    # Motifs
    motif_principal = names(sort(table(visit_reason), decreasing = TRUE))[1],
    
    # Temporalité
    heure_arrivee_moyenne = mean(as.numeric(arrival_time) / 3600, na.rm = TRUE),
    nb_visites_weekend = sum(wday(visit_date) %in% c(1, 7), na.rm = TRUE),
    pct_visites_weekend = round(sum(wday(visit_date) %in% c(1, 7), na.rm = TRUE) / 
                                  n() * 100, 1),
    
    # Régularité (écart-type entre visites en jours)
    regularite_jours = ifelse(n() > 1, 
                              sd(as.numeric(visit_date)), 
                              NA_real_),
    
    # Statut
    statut_final = first(visitor_status),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Catégories d'engagement
    categorie_nb_visites = case_when(
      nb_visites == 1 ~ "Occasionnel (1 visite)",
      nb_visites <= 5 ~ "Explorateur (2-5)",
      nb_visites <= 10 ~ "Régulier (6-10)",
      TRUE ~ "Fidèle (11+)"
    ),
    
    categorie_duree = case_when(
      duree_totale_minutes < 60 ~ "Courte (< 1h)",
      duree_totale_minutes < 180 ~ "Moyenne (1-3h)",
      duree_totale_minutes < 360 ~ "Longue (3-6h)",
      TRUE ~ "Très longue (6h+)"
    ),
    
    # Score d'engagement (0-100)
    score_engagement = pmin(100, round(
      (nb_visites * 10) + 
        (duree_totale_minutes / 10) + 
        (nb_espaces_differents * 5) +
        ifelse(anciennete_jours > 30, 20, anciennete_jours / 1.5),
      1
    ))
  )

cat("✓ data_usagers_comportement créé\n")
cat("  Dimensions:", nrow(data_usagers_comportement), "usagers uniques x", 
    ncol(data_usagers_comportement), "variables\n")
cat("  Variables comportementales: nb_visites, durées, espaces, régularité, etc.\n\n")

# Distribution des catégories
cat("Distribution des catégories d'engagement:\n")
print(table(data_usagers_comportement$categorie_nb_visites))
cat("\n")

# ==============================================================================
# ÉTAPE 7 : EXPORT DES BASES FINALES
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 7 : EXPORT DES BASES FINALES\n")
cat("================================================================================\n\n")

# Créer dossier si nécessaire
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# Sauvegarder
saveRDS(data_frequentation, "data/processed/data_frequentation.rds")
saveRDS(data_usagers_comportement, "data/processed/data_usagers_comportement.rds")

cat("✓ Bases sauvegardées dans data/processed/\n")
cat("  - data_frequentation.rds (", nrow(data_frequentation), "lignes)\n")
cat("  - data_usagers_comportement.rds (", nrow(data_usagers_comportement), "lignes)\n\n")

# ==============================================================================
# RÉSUMÉ FINAL
# ==============================================================================

cat("================================================================================\n")
cat("PIPELINE TERMINÉ AVEC SUCCÈS\n")
cat("================================================================================\n\n")

cat("📊 BASES DE TRAVAIL CRÉÉES:\n\n")

cat("1. data_frequentation\n")
cat("   - Chaque ligne = une visite enrichie avec profil usager\n")
cat("   - ", nrow(data_frequentation), " visites\n")
cat("   - Variables: flux (dates, heures, durées) + profils (âge, université, etc.)\n")
cat("   - Utilisation: analyses temporelles, spatiales, comportementales\n\n")

cat("2. data_usagers_comportement\n")
cat("   - Chaque ligne = un usager unique avec métriques agrégées\n")
cat("   - ", nrow(data_usagers_comportement), " usagers\n")
cat("   - Variables: nb_visites, durées, espaces, régularité, score_engagement\n")
cat("   - Utilisation: segmentation, clustering, scoring, rétention\n\n")

cat("✅ Données prêtes pour l'analyse!\n\n")

# cat("PROCHAINES ÉTAPES RECOMMANDÉES:\n")
# cat("  1. Analyse exploratoire avec visualisations\n")
# cat("  2. Segmentation (clustering) sur data_usagers_comportement\n")
# cat("  3. Analyse genre approfondie\n")
# cat("  4. Modélisation de la rétention\n\n")

cat("================================================================================\n")