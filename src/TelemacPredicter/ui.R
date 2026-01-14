fluidPage(
  tags$head(
    tags$style(HTML("
      .slider-out-of-bounds .irs-bar { background-color: #ff4444 !important; border-top: 1px solid #ff0000; border-bottom: 1px solid #ff0000; }
      .slider-out-of-bounds .irs-bar-edge { background-color: #ff4444 !important; border: 1px solid #ff0000; }
      .slider-out-of-bounds .irs-single { background-color: #ff4444 !important; }
    ")),
    tags$script(HTML("
      $(function() {
        Shiny.addCustomMessageHandler('toggle-slider-color', function(message) {
          var el = $('#' + message.id).closest('.shiny-input-container');
          if (message.isOutOfBounds) {
            el.addClass('slider-out-of-bounds');
          } else {
            el.removeClass('slider-out-of-bounds');
          }
        });
      });
    "))
  ),
  titlePanel("DaMS4 Telemac Data Viewer"),
  
  tabsetPanel(id = "main_tabs",
    tabPanel("Visualisation",
      sidebarLayout(
        sidebarPanel(
          h4("Paramètres du modèle"),
          uiOutput("selectors_ui"),
          verbatimTextOutput("selection_status"),
          hr(),
          actionButton("apply_params", "Utiliser pour prédiction", class = "btn-info")
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
                       choices = c("Coordonnées Locales (Row/Col)" = "local",
                                   "Lambert 93 (Projected)" = "lambert",
                                   "GPS (WGS84)" = "wgs84",
                                   "Click sur carte" = "click")),
          conditionalPanel(
            condition = "input.input_method == 'local' || input.input_method == 'click'",
            numericInput("local_row", "Ligne (Row):", value = 32, min = 1, max = 64),
            numericInput("local_col", "Colonne (Col):", value = 32, min = 1, max = 64)
          ),
          conditionalPanel(
            condition = "input.input_method == 'lambert'",
            numericInput("lambert_x", "X (Lambert):", value = (X_MIN+X_MAX)/2),
            numericInput("lambert_y", "Y (Lambert):", value = (Y_MIN+Y_MAX)/2)
          ),
          conditionalPanel(
            condition = "input.input_method == 'wgs84'",
            tagList(
                numericInput("wgs_lat", "Latitude:", value = 47.0),
                numericInput("wgs_lon", "Longitude:", value = 4.5)
              )
          ),
          actionButton("create_poi", "Créer le point", class = "btn-primary"),
          hr(),
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
    
    tabPanel("Prédiction",
      sidebarLayout(
        sidebarPanel(
          h4("Paramètres de simulation"),
          sliderInput("pred_er", "er: Erosion de la brèche (partie de digue)", min=0, max=1, value=0, step=0.01),
          sliderInput("pred_ks2", "ks2: Frottement (Zone 2 - Lit)", min=15, max=40, value=25, step=0.1),
          sliderInput("pred_ks3", "ks3: Frottement (Zone 3 - Lit)", min=0, max=30, value=15, step=0.1),
          sliderInput("pred_ks4", "ks4: Frottement (Zone 4 - Lit)", min=15, max=40, value=25, step=0.1),
          sliderInput("pred_ks_fp", "ks_fp: Frottement (Plaine inondable)", min=5, max=25, value=15, step=0.1),
          sliderInput("pred_of", "of: Surverse déclenchant brèche (Offset)", min=-0.5, max=0.5, value=0, step=0.01),
          sliderInput("pred_qmax", "qmax: Débit maximum (m3/s)", min=2000, max=30000, value=5000, step=100),
          sliderInput("pred_tm", "tm: Temps au pic de crue (s)", min=50000, max=900000, value=100000, step=1000)
        ),
        mainPanel(
          plotOutput("dl_map", height = "600px"),
          hr(),
          h4("Valeurs prédites avec DeepLearning"),
          tableOutput("dl_points_table")
        )
      )
    )
  )
)
