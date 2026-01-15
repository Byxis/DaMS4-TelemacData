# Initialise les observers pour l'onglet Prédiction
init_prediction_tab <- function(input, output, session, catalog_df, points_data, reference_simulation, dl_model, dl_stats, lasso_models, rf_kfold_models, rf_7030_models) {
  
  # -- Vérification des bornes des paramètres --
  observe({
    req(catalog_df)
    
    # Vérification des bornes pour chaque paramètre et coloration des sliders en rouge si hors de la plage connue
    for (p in PARAM_NAMES) {
       slider_id <- paste0("pred_", p)
       val <- input[[slider_id]]
       
       if (!is.null(val) && !is.null(catalog_df[[p]])) {
          mn <- min(catalog_df[[p]], na.rm = TRUE)
          mx <- max(catalog_df[[p]], na.rm = TRUE)
          
          # Vérification si la valeur est strictement en dehors de la plage [min, max] des données d'entraînement
          is_out <- (val < mn || val > mx)
          
          session$sendCustomMessage("toggle-slider-color", list(id=slider_id, isOutOfBounds=is_out))
       }
    }
  })
  
  # -- Prédiction Deep Learning --
  pred_matrix <- reactive({
    if (is.null(dl_model)) {
      return(NULL)
    }
    
    vals_stats <- dl_stats()
    if (is.null(vals_stats)) {
      showNotification("Erreur: Statistiques de normalisation introuvables.", type = "error")
      return(NULL)
    }
    
    # Order: er, ks2, ks3, ks4, ks_fp, of, qmax, tm
    params <- c(
      input$pred_er,
      input$pred_ks2,
      input$pred_ks3,
      input$pred_ks4,
      input$pred_ks_fp,
      input$pred_of,
      input$pred_qmax,
      input$pred_tm
    )
    
    names(params) <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm")
    
    norm_params <- (params - vals_stats$mean) / vals_stats$std
    
    x_input <- matrix(norm_params, nrow = 1)
    
    pred_tensor <- dl_model %>% predict(x_input, verbose = 0)
    mat_pred <- pred_tensor[1, , , 1]
    
    # Rotation de la matrice pour l'affichage (Transposition)
    return(t(mat_pred))
  })
  
  output$dl_map <- renderPlot({
    result_mat <- pred_matrix()
    
    if (is.null(result_mat)) {
       plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
       text(0, 0, "Modèle introuvable ou erreur de calcul.", cex=1.5)
       return()
    }
    
    # Utilisation du même renderer que la carte principale
    render_telemac_map(result_mat, points_data())
  })
  
  output$dl_points_table <- renderTable({
    mat <- pred_matrix()
    pts <- points_data()
    ref_row <- reference_simulation()
    
    if (is.null(mat) || is.null(pts$coordoneesLocales)) return(NULL)
    
    # Chargement de la matrice de référence si autorisé
    mat_ref <- NULL
    if (!is.null(ref_row)) {
        try({
            path <- ref_row$full_path
            if (file.exists(path)) {
               mat_ref <- as.matrix(read.csv(path, header = TRUE, row.names = 1))
            }
        })
    }
    
    # Préparation des inputs pour Lasso et RF
    lasso_input_mat <- matrix(NA, nrow=1, ncol=8)
    colnames(lasso_input_mat) <- PARAM_NAMES
    
    for(i in seq_along(PARAM_NAMES)) {
       lasso_input_mat[1, i] <- input[[paste0("pred_", PARAM_NAMES[i])]]
    }
    
    # Input pour RF (nommé x1 à x8)
    rf_input <- as.data.frame(lasso_input_mat)
    colnames(rf_input) <- paste0("x", 1:8)
    
    l_models <- lasso_models()
    rf_kfold <- rf_kfold_models()
    rf_7030 <- rf_7030_models()

    locs <- pts$coordoneesLocales
    df_res <- data.frame(
      Point = character(), 
      Predit = numeric(), 
      Simule = numeric(), 
      Lasso = numeric(), 
      RF_KFold = numeric(), 
      RF_7030 = numeric(), 
      stringsAsFactors = FALSE
    )
    
    for (nm in names(locs)) {
       p <- locs[[nm]]
       r <- if("row.y" %in% names(p)) p["row.y"] else p["row"]
       c <- if("col.x" %in% names(p)) p["col.x"] else p["col"]
       
       val_pred <- NA
       val_ref <- NA
       val_lasso <- NA
       val_rf_kfold <- NA
       val_rf_7030 <- NA
       
       if (!is.null(r) && !is.null(c) && !is.na(r) && !is.na(c)) {
           if (r >= 1 && r <= 64 && c >= 1 && c <= 64) {
               mat_row <- c
               mat_col <- 65 - r
               
               # Matrice de prédiction Deep Learning
               if (!is.null(mat)) {
                   if (mat_row >= 1 && mat_row <= nrow(mat) && mat_col >= 1 && mat_col <= ncol(mat)) {
                        val_pred <- mat[mat_row, mat_col]
                   }
               }
               
               # Matrice de référence Telemac (CSV standard)
               if (!is.null(mat_ref)) {
                   if (mat_row >= 1 && mat_row <= nrow(mat_ref) && mat_col >= 1 && mat_col <= ncol(mat_ref)) {
                        val_ref <- mat_ref[mat_row, mat_col]
                   }
               }
           }
       }
       
       # Prédiction Lasso
       if (nm %in% names(l_models)) {
           try({
               m <- l_models[[nm]]
            
               if (is.list(m) && !is.null(m$type) && m$type == "constant") {
                   val_lasso <- as.numeric(m$value)
               } else {
                   val_lasso <- as.numeric(predict(m, newx = lasso_input_mat, s = "lambda.min"))
               }
           }, silent = TRUE)
       }
       
       # Prédiction RF K-Fold
       if (nm %in% names(rf_kfold)) {
           try({
               m <- rf_kfold[[nm]]
               val_rf_kfold <- as.numeric(predict(m, newdata = rf_input))
           }, silent = TRUE)
       }
       
       # Prédiction RF 70/30
       if (nm %in% names(rf_7030)) {
           try({
               m <- rf_7030[[nm]]
               # RF 70/30 utilise "hauteur" comme variable cible
               rf_input_7030 <- rf_input
               colnames(rf_input_7030) <- paste0("x", 1:8)
               val_rf_7030 <- as.numeric(predict(m, newdata = rf_input_7030))
           }, silent = TRUE)
       }
       
       disp_name <- gsub("_", " ", nm)
       df_res <- rbind(df_res, data.frame(
           Point = disp_name, 
           Predit = val_pred, 
           Simule = val_ref,
           Lasso = val_lasso,
           RF_KFold = val_rf_kfold,
           RF_7030 = val_rf_7030
       ))
    }
    
    # Construction du tableau final avec colonnes dynamiques
    df_final <- data.frame(Point = df_res$Point)
    df_final[["Prédit (DeepLearning)"]] <- df_res$Predit
    
    if (any(!is.na(df_res$Lasso))) {
         df_final[["Prédit (Lasso)"]] <- df_res$Lasso
    }
    
    if (any(!is.na(df_res$RF_KFold))) {
         df_final[["RF (K-Fold)"]] <- df_res$RF_KFold
    }
    
    if (any(!is.na(df_res$RF_7030))) {
         df_final[["RF (70/30)"]] <- df_res$RF_7030
    }
    
    # Colonne Telemac à la fin
    if (!is.null(mat_ref)) {
        df_final[["Simulé (Telemac)"]] <- df_res$Simule
    }
    
    df_final

  }, striped = TRUE, hover = TRUE, spacing = "xs")
}
