# Source des modules serveur
source("server/models_loader.R")
source("server/visualization_tab.R")
source("server/prediction_tab.R")
source("server/points_tab.R")

function(input, output, session) {
  
  # ============================================================================
  # INITIALISATION DES DONNÉES
  # ============================================================================
  
  catalog_df <- load_catalog()
  points_data <- reactiveVal(list())
  reference_simulation <- reactiveVal(NULL)
  
  # ============================================================================
  # CHARGEMENT DES MODÈLES
  # ============================================================================
  
  # Modèle Deep Learning
  dl_model <- load_dl_model()
  
  # Modèles Lasso
  lasso_models <- reactiveVal(list())
  observe({
    models <- load_lasso_models()
    lasso_models(models)
  })
  
  # Statistiques de normalisation pour DL
  dl_stats <- reactive({
    load_normalization_stats()
  })
  
  # Points d'intérêt
  observe({
    pts <- load_points_interet()
    points_data(pts)
  })
  
  # ============================================================================
  # INITIALISATION DES ONGLETS
  # ============================================================================
  
  # Onglet Visualisation (retourne current_matrix)
  current_matrix <- init_visualization_tab(
    input, output, session,
    catalog_df, points_data, reference_simulation
  )
  
  # Onglet Prédiction
  init_prediction_tab(
    input, output, session,
    catalog_df, points_data, reference_simulation,
    dl_model, dl_stats, lasso_models
  )
  
  # Onglet Ajouter un Point (utilise current_matrix de Visualisation)
  init_points_tab(
    input, output, session,
    catalog_df, points_data, current_matrix, lasso_models
  )
}
