
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

# Charge les modèles Random Forest K-Fold 10
load_rf_kfold_models <- function() {
  rf_dir <- "../../models/rf_kfold-10"
  full_path <- suppressWarnings(normalizePath(rf_dir, winslash = "/", mustWork = FALSE))
  
  if (!dir.exists(rf_dir)) {
    message("RF K-Fold dir does not exist: ", rf_dir)
    return(list())
  }
  
  files <- list.files(rf_dir, pattern = "_matrix_rf.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("No RF K-Fold models found in: ", full_path)
    return(list())
  }
  
  models <- list()
  for (f in files) {
    fname <- basename(f)
    # "gare_sully_matrix_rf.rds" -> "gare_sully"
    loc_name <- sub("_matrix_rf.rds$", "", fname)
    
    try({
      saved_obj <- readRDS(f)
      # Le modèle K-Fold sauvegarde une liste avec $model
      if (is.list(saved_obj) && !is.null(saved_obj$model)) {
        models[[loc_name]] <- saved_obj$model
      } else {
        models[[loc_name]] <- saved_obj
      }
    })
  }
  
  message("RF K-Fold models loaded: ", paste(names(models), collapse=", "))
  return(models)
}

# Charge les modèles Random Forest 70/30
load_rf_7030_models <- function() {
  rf_dir <- "../../models/rf_70-30"
  full_path <- suppressWarnings(normalizePath(rf_dir, winslash = "/", mustWork = FALSE))
  
  if (!dir.exists(rf_dir)) {
    message("RF 70/30 dir does not exist: ", rf_dir)
    return(list())
  }
  
  files <- list.files(rf_dir, pattern = "^rf1_.*\\.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("No RF 70/30 models found in: ", full_path)
    return(list())
  }
  
  models <- list()
  for (f in files) {
    fname <- basename(f)
    # "rf1_gare_sully_matrix.rds" -> "gare_sully"
    loc_name <- sub("^rf1_", "", fname)
    loc_name <- sub("_matrix.rds$", "", loc_name)
    
    try({
      models[[loc_name]] <- readRDS(f)
    })
  }
  
  message("RF 70/30 models loaded: ", paste(names(models), collapse=", "))
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

# Réentraîne tous les modèles Lasso existants
retrain_all_lasso_models <- function() {
  lasso_dir <- "../../models/lasso"
  
  if (!dir.exists(lasso_dir)) {
    message("Aucun répertoire de modèles Lasso trouvé")
    return(list(success = 0, failed = 0, names = character(0)))
  }
  
  files <- list.files(lasso_dir, pattern = "_matrix_lasso.rds$", full.names = FALSE)
  
  if (length(files) == 0) {
    message("Aucun modèle Lasso à réentraîner")
    return(list(success = 0, failed = 0, names = character(0)))
  }
  
  success_count <- 0
  failed_count <- 0
  retrained_names <- character(0)
  
  for (f in files) {
    # Extraire le nom du point: "caserne_pompiers_matrix_lasso.rds" -> "caserne_pompiers"
    point_name <- sub("_matrix_lasso.rds$", "", f)
    
    message("--- Réentraînement du modèle Lasso pour: ", point_name, " ---")
    
    result <- train_lasso_for_point(point_name)
    
    if (!is.null(result)) {
      success_count <- success_count + 1
      retrained_names <- c(retrained_names, point_name)
    } else {
      failed_count <- failed_count + 1
    }
  }
  
  message("Réentraînement terminé : ", success_count, " succès, ", failed_count, " échecs")
  return(list(success = success_count, failed = failed_count, names = retrained_names))
}

# Régénère toutes les matrices de données pour les points d'intérêt
regenerate_all_matrices <- function(catalog_df, points_data) {
  if (is.null(catalog_df) || nrow(catalog_df) == 0) {
    message("Aucun catalogue de données disponible")
    return(list(success = 0, failed = 0))
  }
  
  locs <- points_data$coordoneesLocales
  if (is.null(locs) || length(locs) == 0) {
    message("Aucun point d'intérêt à traiter")
    return(list(success = 0, failed = 0))
  }
  
  success_count <- 0
  failed_count <- 0
  
  for (nm in names(locs)) {
    message("--- Régénération de la matrice pour: ", nm, " ---")
    
    cible <- locs[[nm]]
    
    donnees_extraites <- lapply(1:nrow(catalog_df), function(i) {
      row_cat <- catalog_df[i, ]
      f <- row_cat$full_path
      
      mat <- tryCatch({
        as.matrix(read.csv(f, header = TRUE, row.names = 1))
      }, error = function(e) return(NULL))
      
      if (is.null(mat)) return(NULL)
      
      params_vec <- numeric(8)
      for (j in seq_along(PARAM_NAMES)) {
        params_vec[j] <- row_cat[[ PARAM_NAMES[j] ]]
      }
      
      r_idx <- if("row.y" %in% names(cible)) cible["row.y"] else cible["row"]
      c_idx <- if("col.x" %in% names(cible)) cible["col.x"] else cible["col"]
      
      if (is.null(r_idx) || is.null(c_idx) || is.na(r_idx) || is.na(c_idx)) return(NULL)

      mat_row <- c_idx
      mat_col <- 65 - r_idx
      
      if (mat_row < 1 || mat_row > nrow(mat) || mat_col < 1 || mat_col > ncol(mat)) return(NULL)
      
      valeur_xy <- mat[mat_row, mat_col]
      
      return(c(params_vec, valeur_xy))
    })
    
    donnees_extraites <- Filter(Negate(is.null), donnees_extraites)
    
    if (length(donnees_extraites) == 0) {
      message("Échec extraction pour: ", nm)
      failed_count <- failed_count + 1
      next
    }
    
    matrice_finale <- do.call(rbind, donnees_extraites)
    df_final <- as.data.frame(matrice_finale)
    colnames(df_final) <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm", "hauteur")
    
    dest_dir <- "../../RData/"
    dest_dir_normalized <- normalizePath(dest_dir, winslash = "/")
    save(df_final, file = file.path(dest_dir_normalized, paste0(nm, "_matrix.RData")))
    
    message("Matrice régénérée pour: ", nm)
    success_count <- success_count + 1
  }
  
  message("Régénération terminée : ", success_count, " succès, ", failed_count, " échecs")
  return(list(success = success_count, failed = failed_count))
}

# Entraîne un modèle Random Forest K-Fold 10 pour un point
train_rf_kfold_for_point <- function(point_name) {
  if (!require("randomForest", quietly = TRUE)) {
    message("Package randomForest non disponible")
    return(NULL)
  }
  
  input_path <- paste0("../../RData/", point_name, "_matrix.RData")
  output_dir <- "../../models/rf_kfold-10"
  output_path <- file.path(output_dir, paste0(point_name, "_matrix_rf.rds"))
  
  if (!file.exists(input_path)) {
    message("Fichier introuvable: ", input_path)
    return(NULL)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  tryCatch({
    temp_env <- new.env()
    load(input_path, envir = temp_env)
    obj_names <- names(temp_env)
    df <- as.data.frame(temp_env[[obj_names[1]]])
    
    # Renommer les colonnes pour RF
    colnames(df) <- c(paste0("x", 1:8), "y")
    for (j in 1:9) df[[j]] <- as.numeric(df[[j]])
    
    # K-Fold Cross-Validation
    set.seed(123)
    k <- 10
    n <- nrow(df)
    fold_id <- sample(rep(1:k, length.out = n))
    
    metrics <- data.frame(fold = integer(), R2 = numeric())
    
    for (fold in 1:k) {
      train_df <- df[fold_id != fold, , drop = FALSE]
      test_df <- df[fold_id == fold, , drop = FALSE]
      
      rf_fit <- randomForest::randomForest(y ~ ., data = train_df, ntree = 500)
      pred <- predict(rf_fit, newdata = test_df)
      
      r2_fold <- 1 - sum((test_df$y - pred)^2) / sum((test_df$y - mean(test_df$y))^2)
      metrics <- rbind(metrics, data.frame(fold = fold, R2 = r2_fold))
    }
    
    # Entraîner le modèle final sur toutes les données
    final_model <- randomForest::randomForest(y ~ ., data = df, ntree = 500, importance = TRUE)
    
    # Sauvegarder
    to_save <- list(
      model = final_model,
      cv_metrics = metrics,
      input_file = paste0(point_name, "_matrix.RData"),
      created_at = as.character(Sys.time())
    )
    
    saveRDS(to_save, file = output_path)
    
    message("RF K-Fold entraîné pour ", point_name, " - R² moyen: ", round(mean(metrics$R2), 4))
    return(final_model)
    
  }, error = function(e) {
    message("Erreur RF K-Fold pour ", point_name, ": ", e$message)
    return(NULL)
  })
}

# Entraîne un modèle Random Forest 70/30 pour un point
train_rf_7030_for_point <- function(point_name) {
  if (!require("randomForest", quietly = TRUE)) {
    message("Package randomForest non disponible")
    return(NULL)
  }
  
  input_path <- paste0("../../RData/", point_name, "_matrix.RData")
  output_dir <- "../../models/rf_70-30"
  output_path <- file.path(output_dir, paste0("rf1_", point_name, "_matrix.rds"))
  
  if (!file.exists(input_path)) {
    message("Fichier introuvable: ", input_path)
    return(NULL)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  tryCatch({
    temp_env <- new.env()
    load(input_path, envir = temp_env)
    obj_names <- names(temp_env)
    df <- as.data.frame(temp_env[[obj_names[1]]])
    
    # Renommer les colonnes
    colnames(df) <- c(paste0("x", 1:8), "hauteur")
    
    # Split 70/30
    set.seed(123)
    idx <- sample(1:nrow(df), 0.7 * nrow(df))
    train <- df[idx, ]
    test <- df[-idx, ]
    
    # Entraîner
    m_rf <- randomForest::randomForest(hauteur ~ ., data = train, ntree = 500, importance = TRUE)
    
    # Calculer R²
    pred_rf <- predict(m_rf, test)
    r2 <- 1 - sum((test$hauteur - pred_rf)^2) / sum((test$hauteur - mean(test$hauteur))^2)
    
    saveRDS(m_rf, output_path)
    
    message("RF 70/30 entraîné pour ", point_name, " - R²: ", round(r2, 4))
    return(m_rf)
    
  }, error = function(e) {
    message("Erreur RF 70/30 pour ", point_name, ": ", e$message)
    return(NULL)
  })
}

# Réentraîne tous les modèles RF K-Fold
retrain_all_rf_kfold_models <- function() {
  rdata_dir <- "../../RData"
  
  if (!dir.exists(rdata_dir)) {
    message("Aucun répertoire RData trouvé")
    return(list(success = 0, failed = 0, names = character(0)))
  }
  
  files <- list.files(rdata_dir, pattern = "_matrix.RData$", full.names = FALSE)
  files <- files[files != "points_interet.RData"]
  
  if (length(files) == 0) {
    message("Aucune matrice à traiter")
    return(list(success = 0, failed = 0, names = character(0)))
  }
  
  success_count <- 0
  failed_count <- 0
  trained_names <- character(0)
  
  for (f in files) {
    point_name <- sub("_matrix.RData$", "", f)
    message("--- Entraînement RF K-Fold pour: ", point_name, " ---")
    
    result <- train_rf_kfold_for_point(point_name)
    
    if (!is.null(result)) {
      success_count <- success_count + 1
      trained_names <- c(trained_names, point_name)
    } else {
      failed_count <- failed_count + 1
    }
  }
  
  message("RF K-Fold terminé : ", success_count, " succès, ", failed_count, " échecs")
  return(list(success = success_count, failed = failed_count, names = trained_names))
}

# Réentraîne tous les modèles RF 70/30
retrain_all_rf_7030_models <- function() {
  rdata_dir <- "../../RData"
  
  if (!dir.exists(rdata_dir)) {
    message("Aucun répertoire RData trouvé")
    return(list(success = 0, failed = 0, names = character(0)))
  }
  
  files <- list.files(rdata_dir, pattern = "_matrix.RData$", full.names = FALSE)
  files <- files[files != "points_interet.RData"]
  
  if (length(files) == 0) {
    message("Aucune matrice à traiter")
    return(list(success = 0, failed = 0, names = character(0)))
  }
  
  success_count <- 0
  failed_count <- 0
  trained_names <- character(0)
  
  for (f in files) {
    point_name <- sub("_matrix.RData$", "", f)
    message("--- Entraînement RF 70/30 pour: ", point_name, " ---")
    
    result <- train_rf_7030_for_point(point_name)
    
    if (!is.null(result)) {
      success_count <- success_count + 1
      trained_names <- c(trained_names, point_name)
    } else {
      failed_count <- failed_count + 1
    }
  }
  
  message("RF 70/30 terminé : ", success_count, " succès, ", failed_count, " échecs")
  return(list(success = success_count, failed = failed_count, names = trained_names))
}
