library(keras3)
library(tensorflow)

# -- Configuration Chemins --
DATA_DIR <- "../data" # Assuming running from src/
if(!dir.exists(DATA_DIR)) DATA_DIR <- "./data" # Fallback if running from root

MODEL_PATH <- "models/deeplearning/telemac_deep_model.keras"
STATS_PATH <- "normalization_stats.RData"

# Load Model & Stats
if (!file.exists(MODEL_PATH) || !file.exists(STATS_PATH)) {
  stop("Model or stats file missing.")
}

load(STATS_PATH)
model <- load_model(MODEL_PATH)

files <- list.files(DATA_DIR, pattern = "_maxH_sully.csv", full.names = TRUE)
if (length(files) == 0) stop("No data files found.")

# Coordinates for Centre Sully (Row 23, Col 18 from 1-based indexing logic in location_extracter)
# Row 23, Col 18
target_row <- 23
target_col <- 18

real_values <- numeric(length(files))
pred_values <- numeric(length(files))

cat("Processing", length(files), "files for Centre Sully analysis...\n")

for (i in seq_along(files)) {
  f <- files[i]
  base_name <- basename(f)
  
  # Parse params
  params_part <- gsub(".*=(.*)_maxH.*", "\\1", base_name)
  params <- as.numeric(unlist(strsplit(params_part, ",")))[1:8]
  
  # Read Real
  mat_real <- as.matrix(read.csv(f, header = TRUE, row.names = 1))
  
  # Predict
  x_input <- matrix((params - X_mean) / X_std, nrow = 1)
  pred_tensor <- model %>% predict(x_input, verbose = 0)
  mat_pred <- pred_tensor[1, , , 1]
  mat_pred <- t(mat_pred) # Transpose to match orientation
  
  # Extract values
  real_values[i] <- mat_real[target_row, target_col]
  pred_values[i] <- mat_pred[target_row, target_col]
  
  if (i %% 50 == 0) cat(i, "/", length(files), "\n")
}

# Calculate R2
sse <- sum((real_values - pred_values)^2)
sst <- sum((real_values - mean(real_values))^2)
r_squared <- 1 - (sse / sst)

# --- Graphique ---

# Chemin d'export du graphique
plot_path <- "Performance_DeepLearning_Sully.png"
limit_range <- range(c(real_values, pred_values))

# Ouverture du périphérique graphique PNG
png(filename = plot_path, width = 800, height = 800, res = 120)

# Création du graphique
plot(real_values, pred_values, 
     xlab = "Hauteurs d'eau réelles (Telemac)", 
     ylab = "Hauteurs d'eau prédites (Deep Learning)",
     main = "Performance DeepLearning - Centre Sully",
     pch = 16, 
     col = rgb(0.2, 0.4, 0.6, 0.4), # Bleu avec transparence
     xlim = limit_range, 
     ylim = limit_range,
     asp = 1) # Ratio 1:1 pour que la droite soit à 45°

# Ajout de la droite de perfection y = x
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

# Ajout du R²
text(x = limit_range[1], y = limit_range[2], 
     labels = paste("R² =", round(r_squared, 4)), 
     pos = 4, cex = 1.2, font = 2, col = "darkred")

grid()

# Fermeture et sauvegarde du fichier
dev.off()

cat("Graphique de performance exporté vers :", plot_path, "\n")
