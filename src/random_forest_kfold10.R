# USAGE: depuis la racine du projet, executer
#   Rscript src/random_forest_cli.R <fichier_data>.RData
# ou
#   Rscript src/random_forest_cli.R <poi>
# Exemple:
#   Rscript src/random_forest_cli.R gare_sully_matrix.RData
#   Rscript src/random_forest_cli.R gare_sully

rm(list = ls())

# -------------------------
# Dependencies
# -------------------------

if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

if (!require("here")) install.packages("here")
library(here)

# -------------------------
# Args
# -------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Erreur : Vous devez fournir un point d'interet (ex: gare_sully) ou un fichier (ex: gare_sully_matrix.RData)")
}

rootdir <- here()
arg1 <- args[1]

# If user passes only poi (no extension), build filename
if (grepl("\\.RData$", arg1, ignore.case = TRUE)) {
  filename <- arg1
} else {
  filename <- paste0(arg1, "_matrix.RData")
}

input_path <- file.path(rootdir, "RData", filename)
if (!file.exists(input_path)) {
  stop(paste0(
    "Erreur : Le fichier ", input_path, " est introuvable.\n",
    "Fichiers disponibles dans RData/ :\n",
    paste(list.files(file.path(rootdir, "RData"), pattern = "\\.RData$", full.names = FALSE), collapse = "\n")
  ))
}

# -------------------------
# Load data 
# -------------------------

temp_env <- new.env(parent = emptyenv())
load(input_path, envir = temp_env)

obj_names <- ls(temp_env)
if (length(obj_names) == 0) stop("Erreur : aucun objet dans le fichier .RData")

data_obj <- temp_env[[obj_names[1]]]

if (!(is.matrix(data_obj) || is.data.frame(data_obj))) {
  stop("Erreur : l'objet charge n'est ni une matrice ni un data.frame")
}
if (ncol(data_obj) != 9) {
  stop(paste0("Erreur : attendu 9 colonnes (8 parametres + y), trouve ", ncol(data_obj)))
}

df <- as.data.frame(data_obj)
for (j in 1:9) df[[j]] <- as.numeric(df[[j]])
colnames(df) <- c(paste0("x", 1:8), "y")

# -------------------------
# Metrics helpers
# -------------------------

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

# -------------------------
# 10-fold cross-validation
# -------------------------

set.seed(123)
k <- 10
n <- nrow(df)
fold_id <- sample(rep(1:k, length.out = n))

metrics <- data.frame(fold = integer(), RMSE = numeric(), MAE = numeric(), R2 = numeric())

for (fold in 1:k) {
  train_df <- df[fold_id != fold, , drop = FALSE]
  test_df  <- df[fold_id == fold, , drop = FALSE]
  
  rf_fit <- randomForest(
    y ~ .,
    data = train_df,
    ntree = 500,
    importance = TRUE
  )
  
  pred <- predict(rf_fit, newdata = test_df)
  
  metrics <- rbind(metrics, data.frame(
    fold = fold,
    RMSE = rmse(test_df$y, pred),
    MAE  = mae(test_df$y, pred),
    R2   = r2(test_df$y, pred)
  ))
}

cat("--- Random Forest (10-fold CV) ---\n")
cat("Input file:", filename, "\n\n")
print(metrics)

cat("\n--- Mean CV metrics ---\n")
print(data.frame(
  RMSE_mean = mean(metrics$RMSE),
  RMSE_sd   = sd(metrics$RMSE),
  MAE_mean  = mean(metrics$MAE),
  MAE_sd    = sd(metrics$MAE),
  R2_mean   = mean(metrics$R2),
  R2_sd     = sd(metrics$R2)
))

# -------------------------
# Train final model on all data
# -------------------------

final_model <- randomForest(
  y ~ .,
  data = df,
  ntree = 500,
  importance = TRUE
)

cat("\n--- Summary(final_model) ---\n")
print(summary(final_model))

cat("\n--- Variable importance ---\n")
print(importance(final_model))

# -------------------------
# Save model to models/random_forest/
# -------------------------

output_dir <- file.path(rootdir, "models", "rf_kfold-10")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_filename <- paste0(tools::file_path_sans_ext(filename), "_rf.rds")
output_path <- file.path(output_dir, output_filename)

# Save a list so you keep both model + CV metrics + metadata
to_save <- list(
  model = final_model,
  cv_metrics = metrics,
  input_file = filename,
  created_at = as.character(Sys.time())
)

saveRDS(to_save, file = output_path)
cat("\nModele exporte avec succes vers :", output_path, "\n")

# -------------------------
# Single-point prediction
# -------------------------

test_index <- 137

x_test <- df[test_index, 1:8, drop = FALSE]
y_true <- df$y[test_index]
y_pred <- predict(final_model, newdata = x_test)

cat("\nSimulation index:", test_index, "\n")
print(x_test)
cat("True height   :", y_true, "\n")
cat("RF prediction :", y_pred, "\n")
cat("Abs error     :", abs(y_true - y_pred), "\n")
