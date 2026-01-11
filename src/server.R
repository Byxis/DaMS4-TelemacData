function(input, output, session) {
  
  # Data setup
  catalog_df <- load_catalog()
  
  # Points data
  points_data <- reactiveVal(list())
  
  observe({
    # Load RData if exists
    path_rdata <- "../RData/points_interet.RData"
    if (!file.exists(path_rdata)) path_rdata <- "./RData/points_interet.RData"
    
    if (file.exists(path_rdata)) {
      load(path_rdata)
      points_data(points_interet)
    }
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
      
      # Available choices given previous filters
      choices <- sort(unique(current_df[[p]]))
      
      # What is currently in the input?
      current_val <- input[[inputId]]
      
      # logic:
      # If current_val is valid in new choices, keep it.
      # If not, pick the first one.
      # If current_val is NULL (init), pick first.
      
      selected_val <- if (!is.null(current_val) && (current_val %in% choices)) current_val else choices[1]
      
      # Update the input
      updateSelectInput(session, inputId, choices = choices, selected = selected_val)
      
      # Narrow down dataset for the NEXT parameter
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
       gps <- local_to_gps(input$local_row, input$local_col)
       updateNumericInput(session, "gps_x", value = gps$x)
       updateNumericInput(session, "gps_y", value = gps$y)
    }
  })
  
  observeEvent(c(input$gps_x, input$gps_y), {
    if (input$input_method == "gps") {
      loc <- gps_to_local(input$gps_x, input$gps_y)
      updateNumericInput(session, "local_row", value = loc$row)
      updateNumericInput(session, "local_col", value = loc$col)
    }
  })
  
  # -- Create Point --
  observeEvent(input$create_poi, {
    req(input$poi_name)
    n <- input$poi_name
    
    r <- input$local_row
    c <- input$local_col
    gx <- input$gps_x
    gy <- input$gps_y
    
    current_list <- points_data()
    
    if (is.null(current_list$coordonneesGps)) current_list$coordonneesGps <- list()
    current_list$coordonneesGps[[n]] <- c(x = gx, y = gy)
    
    if (is.null(current_list$coordoneesLocales)) current_list$coordoneesLocales <- list()
    current_list$coordoneesLocales[[n]] <- c(row = r, col = c)
    
    points_data(current_list)
    points_interet <- current_list 
    
    # Save Point
    save_path <- "../RData/points_interet.RData"
    if (!dir.exists("../RData")) save_path <- "./RData/points_interet.RData"
    
    save(points_interet, file = save_path)
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
    
    save_dir <- "../RData"
    if (!dir.exists(save_dir)) save_dir <- "./RData"
    
    save_path_mat <- file.path(save_dir, paste0(n, "_matrix.RData"))
    save(df_final, file = save_path_mat)
    
    output$status_msg <- renderText({ paste("Point", n, "sauvegardé et calcul terminé :", basename(save_path_mat)) })
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
     
     if (!is.null(current_list$coordonneesGps[[rem_name]])) {
         current_list$coordonneesGps[[rem_name]] <- NULL
     }
     if (!is.null(current_list$coordoneesLocales[[rem_name]])) {
         current_list$coordoneesLocales[[rem_name]] <- NULL
     }
     
     points_data(current_list)
     points_interet <- current_list
     
     save_path <- "../RData/points_interet.RData"
     if (!dir.exists("../RData")) save_path <- "./RData/points_interet.RData"
     save(points_interet, file = save_path)
     
     output$status_msg <- renderText({ paste("Point", rem_name, "supprimé avec succès.") })
  })
}
