
# Charge le modèle Deep Learning
load_dl_model <- function() {
  tryCatch({
    model_path <- "../../models/deeplearning/telemac_deep_model.keras"
    
    if (file.exists(model_path)) {
      model_path <- normalizePath(model_path, winslash = "/", mustWork = FALSE)
      load_model(model_path)
    } else {
      message("DL model not found at: ", model_path)
      NULL
    }
  }, error = function(e) {
    message("Error loading DL model: ", e$message)
    NULL
  })
}

# Charge les modèles Lasso
load_lasso_models <- function() {
  lasso_dir <- "../../models/lasso"
  full_path <- suppressWarnings(normalizePath(lasso_dir, winslash = "/", mustWork = FALSE))
  
  if (!dir.exists(lasso_dir)) {
    print(paste("Lasso dir does not exist:", lasso_dir, "->", full_path))
    return(list())
  }
  
  f_count <- length(list.files(lasso_dir, pattern = "_matrix_lasso.rds$"))
  print(paste("Lasso dir:", lasso_dir, "->", full_path, "Files Found:", f_count))
  
  if (f_count == 0) {
    print(paste("No Lasso models found in:", full_path))
    return(list())
  }
  
  lasso_dir <- full_path
  files <- list.files(lasso_dir, pattern = "_matrix_lasso.rds$", full.names = TRUE)
  print(paste("Found files:", length(files)))
  
  models <- list()
  for (f in files) {
    # Extract location name: "caserne_pompiers_matrix_lasso.rds" -> "caserne_pompiers"
    fname <- basename(f)
    loc_name <- sub("_matrix_lasso.rds$", "", fname)
    
    try({
      models[[loc_name]] <- readRDS(f)
    })
  }
  
  print(paste("Lasso models loaded:", paste(names(models), collapse=", ")))
  return(models)
}

# Charge les statistiques de normalisation pour le modèle DL
load_normalization_stats <- function() {
  stats_path <- "../../normalization_stats.RData"
  
  if (!is.null(stats_path) && file.exists(stats_path)) {
    e <- new.env()
    load(stats_path, envir = e)
    return(list(mean = e$X_mean, std = e$X_std))
  } else {
    return(NULL)
  }
}

# Charge les points d'intérêt
load_points_interet <- function() {
  rdata_path <- "../../RData/points_interet.RData"
  
  if (!is.null(rdata_path) && file.exists(rdata_path)) {
    load(rdata_path)
    return(points_interet)
  } else {
    return(list())
  }
}

# Entraîne un modèle Lasso pour un point spécifique
train_lasso_for_point <- function(point_name) {
  input_path <- paste0("../../RData/", point_name, "_matrix.RData")
  output_dir <- "../../models/lasso"
  output_path <- file.path(output_dir, paste0(point_name, "_matrix_lasso.rds"))
  
  if (!file.exists(input_path)) {
    message("Erreur : Le fichier ", input_path, " est introuvable.")
    return(NULL)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if (file.exists(output_path)) {
    unlink(output_path)
  }
  
  # Charger les données
  temp_env <- new.env()
  load(input_path, envir = temp_env)
  obj_names <- names(temp_env)
  data_matrix <- as.matrix(temp_env[[obj_names[1]]])
  
  # Séparer X et y
  y <- data_matrix[, ncol(data_matrix)]
  X <- data_matrix[, -ncol(data_matrix)]
  
  # Vérifier si y est constant
  v <- var(y, na.rm = TRUE)
  if (is.na(v) || v == 0) {
    message("WARNING: Target variable y is constant for ", point_name, " - Saving constant model.")
    const_model <- list(type = "constant", value = mean(y, na.rm = TRUE))
    saveRDS(const_model, file = output_path)
    message("Modèle constant sauvegardé : ", output_path)
    return(const_model)
  }
  
  # Entraîner le modèle Lasso
  tryCatch({
    set.seed(123)
    lasso_cv <- cv.glmnet(X, y, alpha = 1, nfolds = 10)
    
    # Sauvegarder le modèle
    saveRDS(lasso_cv, file = output_path)
    
    # Calculer le R²
    y_pred <- predict(lasso_cv, s = "lambda.min", newx = X)
    r_squared <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    
    message("Modèle Lasso entraîné pour ", point_name)
    message("  R² : ", round(r_squared, 4))
    message("  Lambda optimal : ", round(lasso_cv$lambda.min, 6))
    message("  Sauvegardé : ", output_path)
    
    return(lasso_cv)
  }, error = function(e) {
    message("Erreur lors de l'entraînement du Lasso pour ", point_name, " : ", e$message)
    return(NULL)
  })
}

