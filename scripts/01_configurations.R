# =============================================================================
# CONFIGURATION COMPLÈTE DES PACKAGES - PROJET SÈMÈ CITY OPEN PARK
# =============================================================================
# Auteur: Analyse de fréquentation
# Date: 2025-12-14
# Description: Installation, chargement et configuration des packages
# =============================================================================

# 1. LISTE COMPLÈTE DES PACKAGES ----
# Tous les packages utilisés dans le projet, sans doublon
packages <- c(
  # Gestion des conflits (à charger en premier)
  "conflicted",
  
  # Manipulation de données de base
  "tidyverse",      # Inclut: dplyr, ggplot2, tidyr, purrr, tibble, stringr, readr, forcats
  "data.table",
  "plyr",           # ATTENTION: doit être chargé AVANT dplyr
  
  # Gestion des dates et heures
  "lubridate",
  "hms",
  "zoo",
  
  # Import/Export de données
  "openxlsx",
  
  # Nettoyage et exploration
  "janitor",
  "summarytools",
  "naniar",
  "VIM",
  "mice",
  
  # Manipulation de texte
  "stringi",
  "stringdist",
  "fuzzyjoin",
  
  # Visualisation
  "scales",
  "patchwork",
  "ggridges",
  "viridis",
  "corrplot",
  "corrgram",
  "plotly",
  "svglite",
  "gganimate",
  "ggalluvial",
  "GGally",
  
  # Cartographie
  "rnaturalearth",
  "rnaturalearthdata",
  "leaflet",
  
  # Statistiques et modélisation
  "psych",
  "car",
  "e1071",
  
  # Machine Learning
  "caret",
  "randomForest",
  "gbm",
  "glmnet",
  "xgboost",
  "pROC",
  "ROSE",
  
  # Analyse multivariée
  "FactoMineR",
  "factoextra",
  "reshape2",
  
  # Séries temporelles et prévisions
  "forecast",
  "prophet",
  "tsibble",
  
  # Interprétabilité des modèles
  "pdp",
  "iml",
  "DALEX",
  
  # Analyse de survie
  "survival",
  "survminer",
  
  # Applications interactives
  "shiny",
  "shinydashboard",
  
  # Données externes
  "WDI",
  
  # Utilitaires
  "gridExtra",
  "pacman"
)

# 2. FONCTION D'INSTALLATION DES PACKAGES MANQUANTS ----
install_if_missing <- function(pkgs) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  VÉRIFICATION ET INSTALLATION DES PACKAGES\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Packages déjà installés
  installed <- rownames(installed.packages())
  to_install <- setdiff(pkgs, installed)
  
  if (length(to_install) == 0) {
    cat("✓ Tous les packages sont déjà installés (", length(pkgs), " packages)\n\n")
    return(invisible(NULL))
  }
  
  cat("→ Packages à installer:", length(to_install), "\n")
  cat("  ", paste(to_install, collapse = ", "), "\n\n")
  
  # Installer les packages manquants
  for (pkg in to_install) {
    cat(sprintf("  Installation de '%s'... ", pkg))
    
    tryCatch({
      # Cas particulier pour corrgram (dépôt spécifique)
      if (pkg == "corrgram") {
        install.packages(pkg, repos = "http://cran.us.r-project.org", 
                         dependencies = TRUE, quiet = TRUE)
      } else {
        install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      }
      cat("✓\n")
    }, error = function(e) {
      cat("✗ ERREUR\n")
      warning(sprintf("Impossible d'installer '%s': %s", pkg, e$message))
    })
  }
  
  cat("\n✓ Installation terminée\n\n")
}

# 3. FONCTION DE CHARGEMENT DES PACKAGES ----
load_packages <- function(pkgs) {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  CHARGEMENT DES PACKAGES\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Ordre spécifique pour éviter les conflits
  priority_order <- c("conflicted", "plyr", pkgs)
  priority_order <- unique(priority_order)  # Supprimer les doublons
  
  errors <- character(0)
  loaded_count <- 0
  
  for (pkg in priority_order) {
    tryCatch({
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE)
      )
      loaded_count <- loaded_count + 1
    }, error = function(e) {
      errors <- c(errors, sprintf("  ✗ %s: %s", pkg, e$message))
    })
  }
  
  if (length(errors) > 0) {
    cat("⚠ Erreurs de chargement:\n")
    cat(paste(errors, collapse = "\n"), "\n\n")
  }
  
  cat(sprintf("✓ %d packages chargés avec succès\n\n", loaded_count))
}

# 4. GESTION DES CONFLITS DE NAMESPACE ----
configure_conflicts <- function() {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  RÉSOLUTION DES CONFLITS DE NAMESPACE\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # S'assurer que conflicted est chargé
  if (!requireNamespace("conflicted", quietly = TRUE)) {
    warning("Le package 'conflicted' n'est pas disponible")
    return(invisible(NULL))
  }
  
  library(conflicted)
  
  # Préférences dplyr (vs plyr, stats, base)
  conflicts_prefer(dplyr::count, .quiet = TRUE)
  conflicts_prefer(dplyr::summarise, .quiet = TRUE)
  conflicts_prefer(dplyr::summarize, .quiet = TRUE)
  conflicts_prefer(dplyr::mutate, .quiet = TRUE)
  conflicts_prefer(dplyr::filter, .quiet = TRUE)
  conflicts_prefer(dplyr::select, .quiet = TRUE)
  conflicts_prefer(dplyr::rename, .quiet = TRUE)
  conflicts_prefer(dplyr::arrange, .quiet = TRUE)
  conflicts_prefer(dplyr::lag, .quiet = TRUE)
  conflicts_prefer(dplyr::first, .quiet = TRUE)
  conflicts_prefer(dplyr::last, .quiet = TRUE)
  
  # Préférences lubridate (vs base, stats, data.table)
  conflicts_prefer(lubridate::month, .quiet = TRUE)
  conflicts_prefer(lubridate::year, .quiet = TRUE)
  conflicts_prefer(lubridate::week, .quiet = TRUE)
  conflicts_prefer(lubridate::wday, .quiet = TRUE)
  conflicts_prefer(lubridate::yday, .quiet = TRUE)
  conflicts_prefer(lubridate::mday, .quiet = TRUE)
  conflicts_prefer(lubridate::quarter, .quiet = TRUE)
  conflicts_prefer(lubridate::day, .quiet = TRUE)
  conflicts_prefer(lubridate::date, .quiet = TRUE)
  conflicts_prefer(lubridate::interval, .quiet = TRUE)
  conflicts_prefer(lubridate::hour, .quiet = TRUE)
  conflicts_prefer(lubridate::minute, .quiet = TRUE)
  conflicts_prefer(lubridate::second, .quiet = TRUE)
  
  # Préférences caret
  conflicts_prefer(caret::cluster, .quiet = TRUE)
  conflicts_prefer(caret::confusionMatrix, .quiet = TRUE)
  
  # Préférences pdp
  conflicts_prefer(pdp::partial, .quiet = TRUE)
  
  # Préférences plotly (vs ggplot2)
  conflicts_prefer(plotly::layout, .quiet = TRUE)
  
  cat("✓ Conflits résolus:\n")
  cat("  • dplyr prioritaire sur plyr et stats\n")
  cat("  • lubridate prioritaire sur base pour les dates\n")
  cat("  • caret, pdp, plotly: fonctions spécifiques protégées\n\n")
}

# 5. OPTIONS GLOBALES ----
configure_options <- function() {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  CONFIGURATION DES OPTIONS GLOBALES\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Options d'affichage
  options(
    scipen = 999,           # Désactiver la notation scientifique
    digits = 4,             # 4 chiffres significatifs
    max.print = 100,        # Limiter l'affichage
    width = 120,            # Largeur de la console
    stringsAsFactors = FALSE # Ne pas convertir automatiquement en facteurs
  )
  
  # Seed pour la reproductibilité
  set.seed(2025)
  
  # Thème ggplot2 par défaut
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))
  }
  
  cat("✓ Options configurées:\n")
  cat("  • Notation scientifique désactivée\n")
  cat("  • Seed fixé à 2025\n")
  cat("  • Thème ggplot2: theme_minimal()\n\n")
}

# 6. FONCTION PRINCIPALE DE SETUP ----
setup_project <- function() {
  start_time <- Sys.time()
  
  cat("\n")
  cat("███████████████████████████████████████████████████████████████\n")
  cat("█                                                        █\n")
  cat("█        SETUP PROJET SÈMÈ CITY OPEN PARK - ANALYSE      █\n")
  cat("█                 Fréquentation & Insights               █\n")
  cat("█                                                        █\n")
  cat("███████████████████████████████████████████████████████████████\n\n")
  
  # Étape 1: Installation
  install_if_missing(packages)
  
  # Étape 2: Chargement
  load_packages(packages)
  
  # Étape 3: Résolution des conflits
  configure_conflicts()
  
  # Étape 4: Configuration des options
  configure_options()
  
  # Résumé final
  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "secs"), 2)
  
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  ✓ CONFIGURATION TERMINÉE\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  cat(sprintf("⏱ Durée totale: %s secondes\n", duration))
  cat(sprintf("📦 Packages configurés: %d\n", length(packages)))
  cat(sprintf("📅 Date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat("\n→ Vous pouvez maintenant commencer votre analyse !\n\n")
}

# =============================================================================
# EXÉCUTION
# =============================================================================

# Lancer le setup complet
setup_project()

# =============================================================================
# VÉRIFICATION POST-SETUP (optionnel)
# =============================================================================

# Fonction de diagnostic
diagnostic_setup <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  DIAGNOSTIC DE L'ENVIRONNEMENT\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Version de R
  cat("Version de R:", R.version.string, "\n")
  
  # Packages chargés
  loaded <- search()
  loaded_packages <- loaded[grepl("^package:", loaded)]
  cat("Packages chargés:", length(loaded_packages), "\n")
  
  # Espace mémoire
    mem_info <- gc()
  cat("Mémoire utilisée par R:\n")
  cat("  - Ncells:", format(mem_info[1, 2], big.mark = " "), "MB\n")
  cat("  - Vcells:", format(mem_info[2, 2], big.mark = " "), "MB\n")
  cat("  - Total:", format(sum(mem_info[, 2]), big.mark = " "), "MB\n")
  
  # Warnings
  if (length(warnings()) > 0) {
    cat("\n⚠ Warnings détectés:", length(warnings()), "\n")
  } else {
    cat("\n✓ Aucun warning\n")
  }
  
  cat("\n")
}

# Décommenter pour exécuter le diagnostic
diagnostic_setup()
