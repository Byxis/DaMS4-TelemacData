# Initialise les observers pour l'onglet Ajouter un Point
init_points_tab <- function(input, output, session, catalog_df, points_data, current_matrix, lasso_models, rf_kfold_models, rf_7030_models) {
  
  # -- Click sur la carte --
  output$click_map <- renderPlot({
    # Affichage de la matrice actuelle de l'onglet Visualisation
    render_telemac_map(current_matrix(), points_data())
  })
  
  observeEvent(input$map_click, {
    if (input$input_method == "click") {
      c_x <- input$map_click$x
      c_y <- input$map_click$y
      
      new_row <- round(64 - c_x)
      new_col <- round(c_y)
      
      new_row <- max(1, min(64, new_row))
      new_col <- max(1, min(64, new_col))
      
      updateNumericInput(session, "local_row", value = new_row)
      updateNumericInput(session, "local_col", value = new_col)
    }
  })
  
  # -- Sync des inputs --
  observeEvent(c(input$local_row, input$local_col), {
    if (input$input_method %in% c("local", "click")) {
       lam <- grid_to_lambert(input$local_row, input$local_col)
       updateNumericInput(session, "lambert_x", value = lam$x)
       updateNumericInput(session, "lambert_y", value = lam$y)
       
       wgs <- lambert_to_wgs84(lam$x, lam$y)
         if (!is.null(wgs)) {
             updateNumericInput(session, "wgs_lat", value = wgs$lat)
             updateNumericInput(session, "wgs_lon", value = wgs$lon)
         }
    }
  })

  observeEvent(c(input$lambert_x, input$lambert_y), {
    if (input$input_method == "lambert") {
      grid <- lambert_to_grid(input$lambert_x, input$lambert_y)
      updateNumericInput(session, "local_row", value = grid$row)
      updateNumericInput(session, "local_col", value = grid$col)
      
      wgs <- lambert_to_wgs84(input$lambert_x, input$lambert_y)
         if (!is.null(wgs)) {
             updateNumericInput(session, "wgs_lat", value = wgs$lat)
             updateNumericInput(session, "wgs_lon", value = wgs$lon)
         }
    }
  })
  
  observeEvent(c(input$wgs_lat, input$wgs_lon), {
    if (input$input_method == "wgs84") {
      
      lam <- wgs84_to_lambert(input$wgs_lat, input$wgs_lon)
      if (!is.null(lam)) {
          updateNumericInput(session, "lambert_x", value = lam$x)
          updateNumericInput(session, "lambert_y", value = lam$y)
          
          grid <- lambert_to_grid(lam$x, lam$y)
          updateNumericInput(session, "local_row", value = grid$row)
          updateNumericInput(session, "local_col", value = grid$col)
      }
    }
  })
  
  # -- Création du point --
  observeEvent(input$create_poi, {
    req(input$poi_name)
    n <- input$poi_name
    
    r <- input$local_row
    c <- input$local_col
    gx <- input$lambert_x
    gy <- input$lambert_y
    
    current_list <- points_data()
    
    if (is.null(current_list$coordonneesLambert93)) current_list$coordonneesLambert93 <- list()
    current_list$coordonneesLambert93[[n]] <- c(x = gx, y = gy)
    
    if (is.null(current_list$coordoneesLocales)) current_list$coordoneesLocales <- list()
    current_list$coordoneesLocales[[n]] <- c(row = r, col = c)
    
    points_data(current_list)
    points_interet <- current_list 
    
    # Sauvegarde du point
    points_interet_path <- "../../RData/points_interet.RData"
    save(points_interet, file = points_interet_path)
    output$status_msg <- renderText({ paste("Point", n, "sauvegardé. Calcul en cours...") })

    # -- Calcul de la matrice --
    cible <- current_list$coordoneesLocales[[n]]
    
    if (is.null(catalog_df)) {
       output$status_msg <- renderText({ "Erreur : Pas de données." })
       return()
    }
    
    withProgress(message = 'Calcul de la matrice...', value = 0, {
      
      donnees_extraites <- lapply(1:nrow(catalog_df), function(i) {
        if (i %% 50 == 0) incProgress(50/nrow(catalog_df))
        
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
    })
    
    donnees_extraites <- Filter(Negate(is.null), donnees_extraites)
    
    if (length(donnees_extraites) == 0) {
        output$status_msg <- renderText({ paste("Point", n, "sauvegardé. Erreur: échec extraction.") })
        return()
    }
    
    matrice_finale <- do.call(rbind, donnees_extraites)
    df_final <- as.data.frame(matrice_finale)
    colnames(df_final) <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm", "hauteur")
    
    dest_dir <- "../../RData/"
    dest_dir_normalized <- normalizePath(dest_dir, winslash = "/")
    save(df_final, file = file.path(dest_dir_normalized, paste0(n, "_matrix.RData")))
    
    output$status_msg <- renderText({ paste("Point", n, "sauvegardé. Entraînement du modèle Lasso...") })
    
    # Entraîner le modèle Lasso pour ce point
    lasso_model <- train_lasso_for_point(n)
    
    if (!is.null(lasso_model)) {
      # Recharger tous les modèles Lasso pour inclure le nouveau
      models <- load_lasso_models()
      lasso_models(models)
      
      output$status_msg <- renderText({ 
        paste("Point", n, "créé avec succès !\n",
              "Matrice :", paste0(n, "_matrix.RData"), "\n",
              "Modèle Lasso :", paste0(n, "_matrix_lasso.rds"))
      })
    } else {
      output$status_msg <- renderText({ 
        paste("Point", n, "sauvegardé mais erreur lors de l'entraînement du Lasso")
      })
    }
  })

  # -- Suppression du point --
  observe({
    current_list <- points_data()
    locs <- current_list$coordoneesLocales
    if (is.null(locs)) return()
    
    nm <- names(locs)
    
    if (length(nm) > 5) {
       choices_del <- nm[6:length(nm)]
       choices_del <- rev(choices_del)
       updateSelectInput(session, "point_to_delete", choices = choices_del)
    } else {
       updateSelectInput(session, "point_to_delete", choices = character(0))
    }
  })
  
  # -- Suppression du point --
  observeEvent(input$delete_point, {
     req(input$point_to_delete)
     rem_name <- input$point_to_delete
     
     current_list <- points_data()
     
     if (!is.null(current_list$coordonneesLambert93[[rem_name]])) {
         current_list$coordonneesGps[[rem_name]] <- NULL
     }
     if (!is.null(current_list$coordoneesLocales[[rem_name]])) {
         current_list$coordoneesLocales[[rem_name]] <- NULL
     }
     
     points_data(current_list)
     points_interet <- current_list
     
     points_interet_path <- "../../RData/points_interet.RData"
     save(points_interet, file = points_interet_path)
     
     output$status_msg <- renderText({ paste("Point", rem_name, "supprimé avec succès.") })
  })
  
  # -- Réentraînement de tous les modèles --
  observeEvent(input$retrain_all_btn, {
    output$retrain_status_msg <- renderText({ "Régénération des matrices en cours..." })
    
    withProgress(message = 'Réentraînement des modèles...', value = 0, {
      
      # Étape 1: Régénérer toutes les matrices
      incProgress(0.1, detail = "Régénération des matrices...")
      matrix_result <- regenerate_all_matrices(catalog_df, points_data())
      
      # Étape 2: Réentraîner les modèles Lasso
      incProgress(0.25, detail = "Entraînement des modèles Lasso...")
      lasso_result <- retrain_all_lasso_models()
      
      # Étape 3: Réentraîner les modèles RF K-Fold
      incProgress(0.5, detail = "Entraînement des modèles RF K-Fold...")
      rf_kfold_result <- retrain_all_rf_kfold_models()
      
      # Étape 4: Réentraîner les modèles RF 70/30
      incProgress(0.75, detail = "Entraînement des modèles RF 70/30...")
      rf_7030_result <- retrain_all_rf_7030_models()
      
      # Étape 5: Recharger tous les modèles
      incProgress(0.9, detail = "Rechargement des modèles...")
      lasso_models(load_lasso_models())
      rf_kfold_models(load_rf_kfold_models())
      rf_7030_models(load_rf_7030_models())
      
      incProgress(1.0, detail = "Terminé!")
    })
    
    output$retrain_status_msg <- renderText({ 
      paste0(
        "Réentraînement terminé !\n",
        "Matrices : ", matrix_result$success, " succès, ", matrix_result$failed, " échecs\n",
        "Lasso : ", lasso_result$success, " succès\n",
        "RF K-Fold : ", rf_kfold_result$success, " succès\n",
        "RF 70/30 : ", rf_7030_result$success, " succès"
      )
    })
    
    total_models <- lasso_result$success + rf_kfold_result$success + rf_7030_result$success
    showNotification(
      paste("Réentraînement terminé :", total_models, "modèles réentraînés"),
      type = "message"
    )
  })
}
