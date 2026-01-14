# Initialise les observers pour l'onglet Visualisation
init_visualization_tab <- function(input, output, session, catalog_df, points_data, reference_simulation) {
  
  # -- Slider qui s'adapte aux données --
  observe({
    req(catalog_df)
    
    for (p in PARAM_NAMES) {
      vals <- catalog_df[[p]]
      if (length(vals) > 0) {
        v_min <- min(vals, na.rm = TRUE)
        v_max <- max(vals, na.rm = TRUE)
        spread <- v_max - v_min
        
        if (spread == 0) spread <- if (v_max != 0) abs(v_max * 0.1) else 1
        
        # On étend la plage de valeurs de 100% de la plage des données
        new_min <- v_min - spread
        new_max <- v_max + spread
        
        # On applique des contraintes physiques (non-négatif)
        if (p %in% c("ks2", "ks3", "ks4", "ks_fp", "qmax", "tm", "er")) {
           new_min <- max(0, new_min)
        }
        
        # On arrondit pour une meilleure lisibilité
        if (p %in% c("qmax", "tm")) {
           new_min <- floor(new_min / 100) * 100
           new_max <- ceiling(new_max / 100) * 100
           safe_val <- round(mean(vals, na.rm = TRUE) / 100) * 100
        } else {
           new_min <- floor(new_min * 10) / 10
           new_max <- ceiling(new_max * 10) / 10
           safe_val <- round(mean(vals, na.rm = TRUE), 2)
        }
        
        updateSliderInput(session, paste0("pred_", p),
                          min = new_min,
                          max = new_max,
                          value = safe_val)
      }
    }
  })
  
  # -- Logique des selecteurs dynamiques --
  output$selectors_ui <- renderUI({
    if (is.null(catalog_df)) return(helpText("Aucun fichier de données trouvé ou erreur de parsing."))
    lapply(PARAM_NAMES, function(p) {
       selectInput(paste0("sel_", p), label = p, choices = NULL) 
    })
  })
  
  # Observer pour mettre à jour les choix basé sur les selections précédentes
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
  
  # -- Logique du fichier sélectionné --
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
  
  # -- Transfert des paramètres de l'onglet Visualisation à l'onglet Prédiction --
  observeEvent(input$apply_params, {
    row <- selected_file_row()
    if (is.null(row)) {
        showNotification("Impossible d'appliquer: Aucun fichier unique sélectionné.", type = "warning")
        return()
    }
    
    for (p in PARAM_NAMES) {
       val <- row[[p]]
       if (!is.null(val)) {
         updateSliderInput(session, paste0("pred_", p), value = as.numeric(val))
       }
    }
    
    reference_simulation(row)
    updateTabsetPanel(session, "main_tabs", selected = "Prédiction")
    showNotification("Paramètres appliqués à l'onglet Prédiction.", type = "message")
  })
  
  # -- Chargement de la matrice --
  current_matrix <- reactive({
    row <- selected_file_row()
    if (is.null(row)) return(NULL)
    
    data <- read.csv(row$full_path, row.names = 1)
    as.matrix(data)
  })
  
  # -- Affichage de la matrice --
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
           if (r >= 1 && r <= 64 && c >= 1 && c <= 64) {
               if (r <= nrow(mat) && c <= ncol(mat)) {
                   val <- mat[r, c]
               }
           }
       }
       
       disp_name <- gsub("_", " ", nm)
       df_res <- rbind(df_res, data.frame(Point = disp_name, Hauteur = val))
    }
    df_res
  }, striped = TRUE, hover = TRUE, spacing = "xs")
  
  # Retourne la matrice actuellement sélectionnée
  return(current_matrix)
}
