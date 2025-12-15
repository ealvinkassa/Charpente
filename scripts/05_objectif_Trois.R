################################################################################
# OBJECTIF 3 : PRÉDICTION DU COMPORTEMENT DES USAGERS (VERSION CORRIGÉE)
################################################################################

# =============================================================================
# 1. CONFIGURATION
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, lubridate, caret, randomForest, pROC,
  survival, survminer
)

set.seed(2025)
options(scipen = 999)

# =============================================================================
# 2. PRÉPARATION DES DONNÉES
# =============================================================================

data_usagers <- data_usagers_comportement %>%
  mutate(
    anciennete_semaines = anciennete_jours / 7,
    frequence_hebdo = nb_visites / pmax(anciennete_semaines, 1),
    intensite = duree_totale_minutes / pmax(anciennete_jours, 1),
    jours_depuis_derniere_visite = as.numeric(Sys.Date() - derniere_visite),
    est_inactif_60j = jours_depuis_derniere_visite > 60,
    categorie_actuelle = case_when(
      nb_visites == 1 ~ "Occasionnel",
      nb_visites <= 5 ~ "Explorateur",
      nb_visites <= 10 ~ "Régulier",
      TRUE ~ "Fidèle"
    )
  )


data_usagers <- data_usagers_comportement %>%
  mutate(
    anciennete_semaines = anciennete_jours / 7,
    frequence_hebdo = nb_visites / pmax(anciennete_semaines, 1),
    intensite = duree_totale_minutes / pmax(anciennete_jours, 1),
    jours_depuis_derniere_visite = as.numeric(Sys.Date() - derniere_visite),
    est_inactif_60j = jours_depuis_derniere_visite > 60
  )



# =============================================================================
# 3. CIBLES
# =============================================================================

data_usagers <- data_usagers %>%
  mutate(
    va_progresser = ifelse(
      categorie_actuelle == "Occasionnel" &
        duree_moyenne_visite > median(duree_moyenne_visite, na.rm = TRUE) &
        !est_inactif_60j,
      1, 0
    ),
    va_churner = ifelse(
      nb_visites >= 3 & est_inactif_60j,
      1, 0
    ),
    va_multi_espaces = ifelse(
      nb_espaces_differents == 1 & nb_visites >= 3,
      1, 0
    )
  )

# =============================================================================
# 4. MODÈLE PROGRESSION
# =============================================================================

data_prog <- data_usagers %>%
  filter(categorie_actuelle == "Occasionnel") %>%
  select(
    phone, va_progresser,
    age, sex, city,
    duree_moyenne_visite, jours_depuis_derniere_visite
  ) %>%
  drop_na()

data_prog$va_progresser <- factor(data_prog$va_progresser, labels = c("Non", "Oui"))

split <- createDataPartition(data_prog$va_progresser, p = 0.75, list = FALSE)
train_prog <- data_prog[split, ]
test_prog  <- data_prog[-split, ]

model_prog <- train(
  va_progresser ~ . - phone,
  data = train_prog,
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  metric = "ROC"
)

# =============================================================================
# 5. MODÈLE CHURN
# =============================================================================

data_churn <- data_usagers %>%
  filter(nb_visites >= 3) %>%
  select(
    phone, va_churner,
    age, sex, city,
    frequence_hebdo, jours_depuis_derniere_visite
  ) %>%
  drop_na()

data_churn$va_churner <- factor(data_churn$va_churner, labels = c("Non", "Oui"))

split <- createDataPartition(data_churn$va_churner, p = 0.75, list = FALSE)
train_churn <- data_churn[split, ]
test_churn  <- data_churn[-split, ]

model_churn <- train(
  va_churner ~ . - phone,
  data = train_churn,
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  metric = "ROC"
)

# =============================================================================
# 6. MODÈLE MULTI-ESPACES
# =============================================================================

data_multi <- data_usagers %>%
  filter(nb_espaces_differents == 1) %>%
  select(
    phone, va_multi_espaces,
    age, sex, city,
    nb_visites, duree_moyenne_visite
  ) %>%
  drop_na()

data_multi$va_multi_espaces <- factor(data_multi$va_multi_espaces, labels = c("Non", "Oui"))

split <- createDataPartition(data_multi$va_multi_espaces, p = 0.75, list = FALSE)
train_multi <- data_multi[split, ]
test_multi  <- data_multi[-split, ]

model_multi <- train(
  va_multi_espaces ~ . - phone,
  data = train_multi,
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  metric = "ROC"
)

# =============================================================================
# 7. SCORING (CORRIGÉ ET ROBUSTE)
# =============================================================================

data_scoring <- data_usagers %>%
  mutate(
    score_progression = NA_real_,
    score_churn = NA_real_,
    score_multi_espaces = NA_real_
  )

# Progression
pred_prog <- predict(model_prog, newdata = data_prog, type = "prob") %>%
  mutate(phone = data_prog$phone, score_progression = Oui * 100)

data_scoring <- data_scoring %>%
  left_join(pred_prog %>% select(phone, score_progression), by = "phone")

# Churn
pred_churn <- predict(model_churn, newdata = data_churn, type = "prob") %>%
  mutate(phone = data_churn$phone, score_churn = Oui * 100)

data_scoring <- data_scoring %>%
  left_join(pred_churn %>% select(phone, score_churn), by = "phone")

# Multi-espaces
pred_multi <- predict(model_multi, newdata = data_multi, type = "prob") %>%
  mutate(phone = data_multi$phone, score_multi_espaces = Oui * 100)

data_scoring <- data_scoring %>%
  left_join(pred_multi %>% select(phone, score_multi_espaces), by = "phone")

# =============================================================================
# 8. EXPORT
# =============================================================================

write.csv(data_scoring, "usagers_avec_scores.csv", row.names = FALSE)
saveRDS(model_prog, "modele_progression.rds")
saveRDS(model_churn, "modele_churn.rds")
saveRDS(model_multi, "modele_multi_espaces.rds")

cat("\n✅ OBJECTIF 3 — VERSION CORRIGÉE ET STABLE\n")
