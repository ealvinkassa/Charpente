# Packages ----

# Lancer l'installation des packages et la configuration en l'environnement
source("scripts/01_configurations.R")


# Data -----
# Le fichier suivant importe les bases de données brutes, les nettoie, et crée les bases de
# données prêtes pour analyse.

source("scripts/02_data_processing.R")


# Analyse exploratoire des données (EDA)

source("scripts/03_analysis_frequentation.R")
source("scripts/03_analysis_comportement.R")
source("scripts/03_analysis_insights_avances.R")


# Visualisations associées

source("scripts/04_visualisation_frequentation.R")
source("scripts/04_visualisation_comportement.R")
source("scripts/04_visualisation_synthese.R")



# Analyses de l'impact des facteurs & prédiction de fa fréquentation

source("scripts/05_objectif_predict_frequentation.R")
source("scripts/05_objectif_identification_facteurs_influence.R")
source("scripts/05_objectif_predict_comportement.R")
source("scripts/05_model_predict_retention.R")


# Tests en cours
# source("scripts/05_objectif_allocation_ressources.R")






