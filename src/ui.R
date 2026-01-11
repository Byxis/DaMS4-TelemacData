fluidPage(
  titlePanel("DaMS4 Telemac Data Viewer"),
  
  tabsetPanel(
    tabPanel("Visualisation",
      sidebarLayout(
        sidebarPanel(
          h4("Paramètres du modèle"),
          uiOutput("selectors_ui"),
          verbatimTextOutput("selection_status")
        ),
        mainPanel(
          plotOutput("matrix_plot", height = "600px"),
          hr(),
          h4("Valeurs aux points d'intérêt"),
          tableOutput("points_values_table")
        )
      )
    ),
    
    tabPanel("Nouveau Point",
      sidebarLayout(
        sidebarPanel(
          textInput("poi_name", "Nom du point:", value = "nouveau_point"),
          radioButtons("input_method", "Méthode d'entrée:",
                       choices = c("Coordonnées Locales" = "local",
                                   "GPS" = "gps",
                                   "Click sur carte" = "click")),
          conditionalPanel(
            condition = "input.input_method == 'local' || input.input_method == 'click'",
            numericInput("local_row", "Ligne (Row):", value = 32, min = 1, max = 64),
            numericInput("local_col", "Colonne (Col):", value = 32, min = 1, max = 64)
          ),
          conditionalPanel(
            condition = "input.input_method == 'gps'",
            numericInput("gps_x", "X:", value = (X_MIN+X_MAX)/2),
            numericInput("gps_y", "Y:", value = (Y_MIN+Y_MAX)/2)
          ),
          actionButton("create_poi", "Créer & Calculer", class = "btn-primary"),
          hr(),
          # actionButton("calc_matrix", "Calculer Matrice", class = "btn-success"),
          verbatimTextOutput("status_msg"),
          hr(),
          h4("Supprimer un point"),
          selectInput("point_to_delete", "Points créés :", choices = NULL),
          actionButton("delete_point", "Supprimer", class = "btn-danger")
        ),
        mainPanel(
           plotOutput("click_map", click = "map_click", height = "600px"),
           helpText("Note: Sélectionnez les paramètres dans l'onglet Visualisation pour afficher la carte.")
        )
      )
    ),
    
    tabPanel("Blank",
      h3("Onglet vide")
    )
  )
)
