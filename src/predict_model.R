library(keras3)
library(tensorflow)

# -- Configuration --
MODEL_PATH <- "models/deeplearning/telemac_deep_model.keras"
STATS_PATH <- "normalization_stats.RData"

plot_grid <- function(mat, title, rotate = FALSE) {
  couleurs <- colorRampPalette(c("white", "pink", "#ffd440", "yellow", "green"))(100)
  vals <- t(mat)
  
  if (rotate) {
    vals <- t(vals)
    vals <- vals[, 1:ncol(vals)]
  }
  
  image(1:64, 1:64, vals, 
        col = couleurs, 
        ylim = c(64, 1), 
        main = title,
        axes = FALSE,
        xlab = "", ylab = "")
  axis(1)
  axis(2)
}

model <- load_model(MODEL_PATH)
load(STATS_PATH)


cat("Modèle et statistiques de normalisation chargés.\n")
cat("Moyennes params:", paste(names(X_mean), round(X_mean, 2), sep="="), "\n")

# -- Comparaison Réel vs Prédit sur données existantes --
files <- list.files("../data", pattern = "_maxH_sully.csv", full.names = TRUE)

n_test <- 3

set.seed(123) 
sample_indices <- sample(seq_along(files), n_test)

par(mfrow = c(n_test, 2), mar = c(2, 2, 2, 1))

cat("\n--- Comparaison (Réel vs Prédit) ---\n")

for (i in sample_indices) {
  f <- files[i]
  base_name <- basename(f)
  
  params_part <- gsub(".*=(.*)_maxH.*", "\\1", base_name)
  params <- as.numeric(unlist(strsplit(params_part, ",")))[1:8]
  
  mat_real <- as.matrix(read.csv(f, header = TRUE, row.names = 1))
  
  params_norm <- (params - X_mean) / X_std
  x_input <- matrix(params_norm, nrow = 1)
  pred_tensor <- model %>% predict(x_input, verbose = 0)
  mat_pred <- pred_tensor[1, , , 1]
  
  mse_val <- mean((mat_real - mat_pred)^2)
  
  plot_grid(mat_real, paste("RÉEL\n", substr(base_name, 1, 20), "..."), rotate = FALSE)
  plot_grid(mat_pred, paste("PRÉDIT \nMSE:", format(mse_val, digits=4)), rotate = TRUE)
}

# -- Prédiction sur de nouvelles données inconnues --

all_qmax <- sapply(files, function(f) {
  bn <- basename(f)
  p_part <- gsub(".*=(.*)_maxH.*", "\\1", bn)
  p <- as.numeric(unlist(strsplit(p_part, ",")))
  return(p[7])
})
max_qmax <- max(all_qmax, na.rm = TRUE)
qmax_scenario <- max_qmax * 2

new_params <- X_mean
if ("qmax" %in% names(X_mean)) {
   new_params["qmax"] <- qmax_scenario
} else {
   new_params[7] <- qmax_scenario
}

x_input_new <- matrix((new_params - X_mean) / X_std, nrow = 1)

pred_new_tensor <- model %>% predict(x_input_new, verbose = 0)
mat_new <- pred_new_tensor[1, , , 1]

dev.new()
par(mfrow = c(1, 1))
title_plot <- paste("PRÉDICTION SCÉNARIO EXTRÊME\nDébit =", round(qmax_scenario, 0), "(Max + 5%)")
plot_grid(mat_new, title_plot, rotate = TRUE)