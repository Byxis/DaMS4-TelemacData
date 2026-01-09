library(ggplot2)

# -- CALCUL DES COORDONNEES -- #

X_MIN <- 652003.44
Y_MIN <- 6738994
X_MAX <- 656229.94
Y_MAX <- 6741633
N_COLS <- 64
N_ROWS <- 64

DX <- (X_MAX - X_MIN) / N_COLS
DY <- (Y_MAX - Y_MIN) / N_ROWS

raw_points <- list(
  gare_sully       = c(x = 653035.06, y = 6740125),
  parc_chateau     = c(x = 653722.62, y = 6741027),
  centre_sully     = c(x = 653151.38, y = 6740696.5),
  caserne_pompiers = c(x = 652769.81, y = 6739413),
  domaine_epinoy   = c(x = 657000.31, y = 6740304)
)

gps_list <- list()
locales_list <- list()

for (nom in names(raw_points)) {
  pt <- raw_points[[nom]]
  
  col_idx <- floor((pt["x"] - X_MIN) / DX) + 1
  row_from_bottom <- floor((pt["y"] - Y_MIN) / DY)
  row_idx <- N_ROWS - row_from_bottom
  
  gps_list[[nom]] <- pt
  locales_list[[nom]] <- c(row = row_idx, col = col_idx)
}

points_interet <- list(
  coordonneesGps = gps_list,
  coordoneesLocales = locales_list
)

print(points_interet$coordoneesLocales)
save(points_interet, file = "./RData/points_interet.RData")

# -- TEST EN PLOT -- #
load("./RData/points_interet.RData")

data <- read.csv("./data/er,ks2,ks3,ks4,ks_fp,of,qmax,tm=0.0447351163649,23.5190092312871,31.7359848081414,18.4741073065111,19.015498629231,-0.0689854550227523,21158.4754810948,679267.098263043_maxH_sully.csv", row.names = 1)
matrice <- as.matrix(data)

locales <- points_interet$coordoneesLocales

px <- sapply(locales, function(p) 64- p["row.y"])
py <- sapply(locales, function(p) p["col.x"])
noms <- names(locales)

couleurs <- colorRampPalette(c("white", "pink", "#ffd440", "yellow", "green"))(100)

image(1:64, 1:64, t(matrice), 
      col = couleurs, 
      ylim = c(64, 1), 
      xlab = "Colonnes (X)", 
      ylab = "Lignes (Y)")

points(px, py, pch = 18, col = "red", cex = 1.5)
text(px, py, labels = gsub("_", " ", noms), pos = 3, col = "black", font = 2, cex = 0.8)

