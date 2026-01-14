#!/usr/bin/env Rscript

# Usage: Rscript rf.R kebab_matrix.RData

library(randomForest)
library(tools)

# 1. RÉCUPÉRATION DE L'ARGUMENT (ex: kebab_matrix.RData)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Erreur : Donnez le nom du fichier. Exemple : Rscript rf.R kebab_matrix.RData", call. = FALSE)
}
# 
nom_fichier_entree <- args[1]

# 2. CONSTRUCTION DES CHEMINS
# Chemin vers le dossier source
dossier_source <- "RData"
chemin_entree  <- file.path(dossier_source, nom_fichier_entree)

# Chemin vers le dossier destination
dossier_models <- "models"
if (!dir.exists(dossier_models)) {
  dir.create(dossier_models)
}

# Génération du nom de sortie : models/rf1_nom_du_lieu.rds
nom_lieu       <- file_path_sans_ext(nom_fichier_entree)
chemin_sortie  <- file.path(dossier_models, paste0("rf1_", nom_lieu, ".rds"))

# 3. CHARGEMENT
if (!file.exists(chemin_entree)) {
  stop(paste("Erreur : Le fichier est introuvable dans :", chemin_entree), call. = FALSE)
}

cat("Chargement de :", chemin_entree, "...\n")
load(chemin_entree)
df <- df_final 

# 4. PRÉPARATION (70/30)
set.seed(123)
idx   <- sample(1:nrow(df), 0.7 * nrow(df))
train <- df[idx, ]
test  <- df[-idx, ]

# 5. ENTRAÎNEMENT
cat("Entraînement du modèle [rf1] pour", nom_lieu, "...\n")
m_rf <- randomForest(hauteur ~ ., data = train, ntree = 500, importance = TRUE)

# 6. SAUVEGARDE
saveRDS(m_rf, chemin_sortie)
cat("--> Modèle sauvegardé dans :", chemin_sortie, "\n")

# 7. PERFORMANCE RAPIDE
pred_rf <- predict(m_rf, test)
r2      <- 1 - sum((test$hauteur - pred_rf)^2) / sum((test$hauteur - mean(test$hauteur))^2)
cat("Précision (R2) :", round(r2, 4), "\n")