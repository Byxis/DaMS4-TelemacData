library(keras3)
library(tensorflow)

# -- Configuration Chemins --
DATA_DIR <- "./data"
MODEL_PATH <- "models/deeplearning/telemac_deep_model.keras"
STATS_PATH <- "normalization_stats.RData"

# Fonction utilitaire pour charger ou recalculer les stats de normalisation
ensure_stats <- function() {
  if (file.exists(STATS_PATH)) {
    load(STATS_PATH, envir = .GlobalEnv)
    cat("Statistiques de normalisation chargées.\n")
  } else {
    cat("Statistiques manquantes. Recalcul en cours à partir des données...\n")
    files <- list.files(DATA_DIR, pattern = "_maxH_sully.csv", full.names = TRUE)
    if(length(files) == 0) stop("Aucune donnée trouvée pour calculer les stats !")
    
    X_list <- list()
    for (f in files) {
      base_name <- basename(f)
      params_part <- gsub(".*=(.*)_maxH.*", "\\1", base_name)
      params <- as.numeric(unlist(strsplit(params_part, ",")))
      X_list[[length(X_list) + 1]] <- params
    }
    X_mat <- do.call(rbind, X_list)
    
    X_mean <- apply(X_mat, 2, mean)
    X_std <- apply(X_mat, 2, sd)
    X_std[X_std == 0] <- 1
    
    save(X_mean, X_std, file = STATS_PATH)
    assign("X_mean", X_mean, envir = .GlobalEnv)
    assign("X_std", X_std, envir = .GlobalEnv)
    cat("Statistiques calculées et sauvegardées.\n")
  }
}

plot_comparison <- function(real, pred, diff, title) {
  couleurs <- colorRampPalette(c("white", "pink", "#ffd440", "yellow", "green"))(100)
  couleurs_diff <- colorRampPalette(c("blue", "white", "red"))(100)
  
  # Rotation pour aligner avec l'affichage R standard
  rotate <- function(m) t(m)[, nrow(m):1]
  
  par(mfrow = c(1, 3), mar = c(2, 2, 3, 1))
  
  # Réel
  image(1:64, 1:64, rotate(real), col = couleurs, main = "Telemac (Réel)", axes=F)
  
  # Prédit
  image(1:64, 1:64, rotate(pred), col = couleurs, main = "Deep Learning", axes=F)
  
  # Différence
  max_diff <- max(abs(diff))
  # Normaliser diff pour le plot
  image(1:64, 1:64, rotate(diff), col = couleurs_diff, 
        main = paste("Erreur (MAE=", round(mean(abs(diff)), 4), ")", sep=""), axes=F)
}

# --- Main ---

if (!file.exists(MODEL_PATH)) {
  stop(paste("Le modèle n'existe pas à :", MODEL_PATH, "\nVeuillez d'abord entraîner le modèle (via 'deep_learning_model.R' ou 'train_quick.R')."))
}

ensure_stats()
model <- load_model(MODEL_PATH)
cat("Modèle chargé avec succès.\n")

# Sélection aléatoire d'un fichier
files <- list.files(DATA_DIR, pattern = "_maxH_sully.csv", full.names = TRUE)

if (length(files) == 0) {
  stop(paste("Aucun fichier de données trouvé dans :", normalizePath(DATA_DIR, mustWork = FALSE), 
             "\nVérifiez que le répertoire existe et contient des fichiers .csv."))
}

sample_file <- sample(files, 1)

# Chargement et Préparation
base_name <- basename(sample_file)
params_part <- gsub(".*=(.*)_maxH.*", "\\1", base_name)
params <- as.numeric(unlist(strsplit(params_part, ",")))[1:8] # Garder les 8 premiers si extra

mat_real <- as.matrix(read.csv(sample_file, header = TRUE, row.names = 1))

# Prédiction
x_input <- matrix((params - X_mean) / X_std, nrow = 1)
pred_tensor <- model %>% predict(x_input, verbose = 0)
mat_pred <- pred_tensor[1, , , 1]
mat_pred <- t(mat_pred) # La prédiction sort transposée par rapport au csv

# Calculs
diff <- mat_real - mat_pred
mae <- mean(abs(diff))
mse <- mean(diff^2)

cat("\n--- Résultats de l'Analyse ---\n")
cat("Fichier :", base_name, "\n")
cat("MAE (Mean Absolute Error) :", mae, "\n")
cat("RMSE (Root Mean Sq Error):", sqrt(mse), "\n")

# Plot
plot_comparison(mat_real, mat_pred, diff, base_name)
