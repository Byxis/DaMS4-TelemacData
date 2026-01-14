#!/usr/bin/env Rscript

# Usage: Rscript rf.R kebab_matrix.RData

library(randomForest)
library(tools)

args <- commandArgs(trailingOnly = TRUE)

nom_fichier_entree <- args[1]

# Chemin vers le dossier source
dossier_source <- "RData"
chemin_entree  <- file.path(dossier_source, nom_fichier_entree)

# Chemin vers le dossier destination
dossier_models <- file.path("models", "rf_70-30")
if (!dir.exists(dossier_models)) {
  dir.create(dossier_models, recursive = TRUE)
}

# Génération du nom de sortie : models/rf_70-30/rf1_nom_du_lieu.rds
nom_lieu       <- file_path_sans_ext(nom_fichier_entree)
chemin_sortie  <- file.path(dossier_models, paste0("rf1_", nom_lieu, ".rds"))


load(chemin_entree)
df <- df_final 

set.seed(123)
idx   <- sample(1:nrow(df), 0.7 * nrow(df)) # 70% pour l'entrainement, 30% pour le test
train <- df[idx, ]
test  <- df[-idx, ]

m_rf <- randomForest(hauteur ~ ., data = train, ntree = 500, importance = TRUE)


saveRDS(m_rf, chemin_sortie)

# prédiction
exemple_ligne  <- test[1, ] 
valeur_reelle  <- exemple_ligne$hauteur
valeur_predite <- predict(m_rf, newdata = exemple_ligne)

cat("Prédiction : \n")
cat("Valeur Réelle  :", valeur_reelle, "\n")
cat("Valeur Prédite :", valeur_predite, "\n")
cat("Valeur absolue :", abs(valeur_predite - valeur_reelle), "\n")

pred_rf <- predict(m_rf, test)
r2      <- 1 - sum((test$hauteur - pred_rf)^2) / sum((test$hauteur - mean(test$hauteur))^2)
cat("R2 :", round(r2, 4), "\n")