function(input, output, session) {
  catalog_df <- load_catalog()
  points_data <- reactiveVal(list())
  
  observe({
    load("../RData/points_interet.RData")
    points_data(points_interet)
  })
  
  # -- Dynamic Selectors Logic --
  output$selectors_ui <- renderUI({
    if (is.null(catalog_df)) return(helpText("Aucun fichier de données trouvé ou erreur de parsing."))
    lapply(PARAM_NAMES, function(p) {
       selectInput(paste0("sel_", p), label = p, choices = NULL) 
    })
  })
  
  # Observer to update choices based on previous selections
  observe({
    req(catalog_df)
    current_df <- catalog_df
    
    for (i in seq_along(PARAM_NAMES)) {
      p <- PARAM_NAMES[i]
      inputId <- paste0("sel_", p)
      choices <- sort(unique(current_df[[p]]))
      current_val <- input[[inputId]]
      selected_val <- if (!is.null(current_val) && (current_val %in% choices)) current_val else choices[1]
      
      updateSelectInput(session, inputId, choices = choices, selected = selected_val)
      if (!is.null(selected_val)) {
        current_df <- current_df[current_df[[p]] == selected_val, ]
      }
    }
  })
  
  # -- Selected File Logic --
  selected_file_row <- reactive({
    req(catalog_df)
    
    df <- catalog_df
    for (p in PARAM_NAMES) {
      val <- input[[paste0("sel_", p)]]
      if (is.null(val)) return(NULL)
       df <- df[df[[p]] == as.numeric(val), ]
    }
    
    if (nrow(df) == 1) {
      return(df[1, ])
    } else {
      return(NULL)
    }
  })
  
  output$selection_status <- renderText({
    row <- selected_file_row()
    if (is.null(row)) {
      "Veuillez sélectionner tous les paramètres pour identifier un fichier unique."
    } else {
      paste("Fichier sélectionné :\n", basename(row$file_name))
    }
  })
  
  # -- Load Matrix --
  current_matrix <- reactive({
    row <- selected_file_row()
    if (is.null(row)) return(NULL)
    
    data <- read.csv(row$full_path, row.names = 1)
    as.matrix(data)
  })
  
  # -- Plotting --
  output$matrix_plot <- renderPlot({
    render_telemac_map(current_matrix(), points_data())
  })
  
  output$points_values_table <- renderTable({
    mat <- current_matrix()
    pts <- points_data()
    
    if (is.null(mat) || is.null(pts$coordoneesLocales)) return(NULL)
    
    locs <- pts$coordoneesLocales
    df_res <- data.frame(Point = character(), Hauteur = numeric(), stringsAsFactors = FALSE)
    
    for (nm in names(locs)) {
       p <- locs[[nm]]
       r <- if("row.y" %in% names(p)) p["row.y"] else p["row"]
       c <- if("col.x" %in% names(p)) p["col.x"] else p["col"]
       
       val <- NA
       if (!is.null(r) && !is.null(c) && !is.na(r) && !is.na(c)) {
           vis_r <- c
           vis_c <- 65 - r
           
           if (vis_r >= 1 && vis_r <= nrow(mat) && vis_c >= 1 && vis_c <= ncol(mat)) {
               val <- mat[vis_r, vis_c]
           }
       }
       
       disp_name <- gsub("_", " ", nm)
       df_res <- rbind(df_res, data.frame(Point = disp_name, Hauteur = val))
    }
    df_res
  }, striped = TRUE, hover = TRUE, spacing = "xs")
  
  output$click_map <- renderPlot({
    render_telemac_map(current_matrix(), points_data())
  })
  
  # -- Click on map --
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
  
  # -- Inputs Sync --
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
  
  # -- Create Point --
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
    
    # Save Point
    save(points_interet, file = "../RData/points_interet.RData")
    output$status_msg <- renderText({ paste("Point", n, "sauvegardé. Calcul en cours...") })

    # -- Calculate Matrix --
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
        vis_r <- c_idx
        vis_c <- 65 - r_idx
        
        if (vis_r < 1 || vis_r > nrow(mat) || vis_c < 1 || vis_c > ncol(mat)) return(NULL)
        
        valeur_xy <- mat[vis_r, vis_c]
        
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
    
    save(df_final, file = file.path("../RData/", paste0(n, "_matrix.RData")))
    
    output$status_msg <- renderText({ paste("Point", n, "sauvegardé et calcul terminé :", paste0(n, "_matrix.RData")) })
  })

  # -- Delete Point Logic --
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
  
  # -- Delete Point --
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
     
     save(points_interet, file = "../RData/points_interet.RData")
     
     output$status_msg <- renderText({ paste("Point", rem_name, "supprimé avec succès.") })
  })
}
