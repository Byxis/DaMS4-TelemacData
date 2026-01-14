# Initialise les observers pour l'onglet Prédiction
init_prediction_tab <- function(input, output, session, catalog_df, points_data, reference_simulation, dl_model, dl_stats, lasso_models) {
  
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
    
    # Préparation des inputs pour Lasso    
    lasso_input_mat <- matrix(NA, nrow=1, ncol=8)
    colnames(lasso_input_mat) <- PARAM_NAMES
    
    for(i in seq_along(PARAM_NAMES)) {
       lasso_input_mat[1, i] <- input[[paste0("pred_", PARAM_NAMES[i])]]
    }
    
    l_models <- lasso_models()

    locs <- pts$coordoneesLocales
    df_res <- data.frame(Point = character(), Predit = numeric(), Simule = numeric(), Lasso = numeric(), stringsAsFactors = FALSE)
    
    for (nm in names(locs)) {
       p <- locs[[nm]]
       r <- if("row.y" %in% names(p)) p["row.y"] else p["row"]
       c <- if("col.x" %in% names(p)) p["col.x"] else p["col"]
       
       val_pred <- NA
       val_ref <- NA # NA si pas de matrice de référence
       val_lasso <- NA
       
       if (!is.null(r) && !is.null(c) && !is.na(r) && !is.na(c)) {
           if (r >= 1 && r <= 64 && c >= 1 && c <= 64) {
               
               # Matrice de prédiction (Alignée via t()) -> Accès direct
               if (!is.null(mat)) {
                   if (r <= nrow(mat) && c <= ncol(mat)) {
                        val_pred <- mat[r, c]
                   }
               }
               
               # Matrice de référence (CSV standard) -> Accès direct
               if (!is.null(mat_ref)) {
                   if (r <= nrow(mat_ref) && c <= ncol(mat_ref)) {
                        val_ref <- mat_ref[r, c]
                   }
               }
           }
       }
       
       # Prédiction Lasso
       # 'nm' est la clé dans points_data, par exemple "caserne_pompiers"
       if (nm %in% names(l_models)) {
           try({
               m <- l_models[[nm]]
            
               if (is.list(m) && !is.null(m$type) && m$type == "constant") {
                   val_lasso <- as.numeric(m$value)
               } else {
                   val_lasso <- as.numeric(predict(m, newx = lasso_input_mat, s = "lambda.min"))
               }
           })
       }
       
       disp_name <- gsub("_", " ", nm)
       df_res <- rbind(df_res, data.frame(
           Point = disp_name, 
           Predit = val_pred, 
           Simule = val_ref,
           Lasso = val_lasso
       ))
    }
    
    # Style dynamique des colonnes
    cols_to_keep <- c("Point", "Prédit (DeepLearning)")
    
    df_final <- df_res[, c("Point", "Predit")]
    colnames(df_final) <- cols_to_keep
    
    if (!is.null(mat_ref)) {
        df_final[["Simulé (Telemac)"]] <- df_res$Simule
    }
    
    # Vérification si une prédiction Lasso existe
    if (any(!is.na(df_res$Lasso))) {
         df_final[["Prédit (Lasso)"]] <- df_res$Lasso
    }
    
    df_final

  }, striped = TRUE, hover = TRUE, spacing = "xs")
}
