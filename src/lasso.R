# USAGE: depuis la racine du projet, executer 
# Rscript lasso.R <fichier_data>.RData

# Installation et chargement des bibliothèques nécessaires
if (!require("glmnet")) install.packages("glmnet")
library(glmnet)
if (!require("here")) install.packages("here")
library(here)


# --- GESTION DES ARGUMENTS ---
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Erreur : Vous devez fournir un nom de fichier (ex: centre_sully_matrix.RData)")
}

rootdir <- here()
filename <- args[1]
input_path <- file.path(rootdir,"RData", filename)

output_filename <- paste0(tools::file_path_sans_ext(filename), "_lasso.rds")
output_dir <- file.path(rootdir,"models/lasso")
output_path <- file.path(output_dir, output_filename)

# 1. Chargement des données
if (!file.exists(input_path)) {
  stop(paste("Erreur : Le fichier", input_path, "est introuvable."))
}

# 1. Chargement des données dans un environnement pour ne pas interférer avec autres modèles

temp_env <- new.env()
load(input_path, envir = temp_env)
obj_names <- names(temp_env)
data_matrix <- as.matrix(temp_env[[obj_names[1]]])

# 2. Préparation des variables
# On sépare les prédicteurs (X) de la variable cible (y : hauteur d'eau)
# La dernière colonne est la hauteur donnée par Telemac
y <- data_matrix[, ncol(data_matrix)]
X <- data_matrix[, -ncol(data_matrix)]

# 3. Entraînement du modèle Lasso avec K-Fold Cross-Validation (K=10)
# alpha = 1 correspond au Lasso (alpha = 0 serait pour Ridge)
set.seed(123) # Pour la reproductibilité
lasso_cv <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

# 4. Affichage des résultats
print("--- Résumé du modèle Lasso (Validation Croisée) ---")
print(lasso_cv)


# Visualisation de l'évolution de l'erreur en fonction de lambda
# plot(lasso_cv)

# 5. Extraction des coefficients au meilleur lambda (lambda.min)
# Le meilleur lambda est celui qui minimise l'erreur quadratique moyenne
best_lambda <- lasso_cv$lambda.min
cat("\nMeilleur Lambda (min) : ", best_lambda, "\n")

coeffs <- coef(lasso_cv, s = "lambda.min")
print("--- Coefficients du modèle (au lambda optimal) ---")
print(coeffs)

# 6. Évaluation rapide
# Calcul du R-carré pour le modèle final sur les données d'entraînement
y_pred <- predict(lasso_cv, s = "lambda.min", newx = X)
r_squared <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
cat("\nR² du modèle sur les données d'entraînement : ", round(r_squared, 4), "\n")


# 7. Sauvegarde du modèle avec le nouveau nom
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Création du répertoire :", output_dir, "\n")
}
saveRDS(lasso_cv, file = output_path)
cat("\nModèle exporté avec succès vers :", output_path, "\n")