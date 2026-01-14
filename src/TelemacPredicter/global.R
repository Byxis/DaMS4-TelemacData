library(shiny)
library(ggplot2)
library(sf)
library(keras3)
library(tensorflow)
library(glmnet)

# -- Constants --
X_MIN <- 652003.44
Y_MIN <- 6738994
X_MAX <- 656229.94
Y_MAX <- 6741633
N_COLS <- 64
N_ROWS <- 64
DX <- (X_MAX - X_MIN) / N_COLS
DY <- (Y_MAX - Y_MIN) / N_ROWS

PARAM_NAMES <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm")

# -- Helper Functions --
lambert_to_grid <- function(x, y) {
  col_idx <- floor((x - X_MIN) / DX) + 1
  row_from_bottom <- floor((y - Y_MIN) / DY)
  row_idx <- N_ROWS - row_from_bottom
  return(list(row = row_idx, col = col_idx))
}

grid_to_lambert <- function(row, col) {
  x <- X_MIN + (col - 0.5) * DX
  y <- Y_MIN + (N_ROWS - row + 0.5) * DY
  return(list(x = x, y = y))
}

wgs84_to_lambert <- function(lat, lon) {
  pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  pt_lamb <- st_transform(pt, 2154)
  coords <- st_coordinates(pt_lamb)

  return(list(x = coords[1], y = coords[2]))
}

lambert_to_wgs84 <- function(x, y) {
  pt <- st_sfc(st_point(c(x, y)), crs = 2154)
  pt_wgs <- st_transform(pt, 4326)
  coords <- st_coordinates(pt_wgs)

  return(list(lat = coords[2], lon = coords[1]))
}

load_catalog <- function() {
  # Robust data directory discovery
  candidates <- c("../../data", "../data", "data")
  data_dir <- NULL
  for (d in candidates) {
    if (dir.exists(d)) {
      data_dir <- normalizePath(d, winslash = "/", mustWork = FALSE)
      break
    }
  }
  
  if (is.null(data_dir)) return(NULL)
  
  files <- list.files(data_dir, pattern = "_maxH_sully.csv", full.names = FALSE)
  
  if (length(files) == 0) return(NULL)
  
  # Format: "er,ks2,ks3,ks4,ks_fp,of,qmax,tm=[VALUES]_maxH_sully.csv"
  parsed_list <- lapply(files, function(f) {
    parts <- strsplit(f, "tm=")[[1]]
    if (length(parts) < 2) return(NULL)
    
    rest <- parts[2]
    val_part <- sub("_maxH_sully.csv", "", rest)
    
    vals <- strsplit(val_part, ",")[[1]]
    if (length(vals) < 8) return(NULL)
    
    vals <- as.numeric(vals[1:8])
    names(vals) <- PARAM_NAMES
    
    d <- as.list(vals)
    d$file_name <- f
    d$full_path <- file.path(data_dir, f)
    as.data.frame(d, stringsAsFactors = FALSE)
  })
  
  do.call(rbind, parsed_list)
}

# -- Plotting Helper --
render_telemac_map <- function(matrice, poi_list) {
  if (is.null(matrice)) {
    plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
    text(0, 0, "Sélectionnez les paramètres\npour voir la carte", cex=1.5)
    return()
  }
  
  locales <- poi_list$coordoneesLocales
  
  px <- numeric(length(locales))
  py <- numeric(length(locales))
  valid_pts <- logical(length(locales))
  
  if (length(locales) > 0) {
    for (i in seq_along(locales)) {
      p <- locales[[i]]
      r <- if("row.y" %in% names(p)) p["row.y"] else p["row"]
      c <- if("col.x" %in% names(p)) p["col.x"] else p["col"]
      
      if (!is.null(r) && !is.null(c) && !is.na(r) && !is.na(c)) {
        px[i] <- 64 - r
        py[i] <- c
        valid_pts[i] <- TRUE
      }
    }
  }
  
  noms <- names(locales)
  
  couleurs <- colorRampPalette(c("white", "pink", "#ffd440", "yellow", "green"))(100)
  
  # Calcul des valeurs min et max
  val_min <- min(matrice, na.rm = TRUE)
  val_max <- max(matrice, na.rm = TRUE)
  
  # Sauvegarder les paramètres graphiques
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  # Layout : carte principale (gauche) + colorbar (droite)
  layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))
  
  # -- Carte principale --
  par(mar = c(4, 4, 2, 1))
  image(1:64, 1:64, t(matrice), 
        col = couleurs, 
        ylim = c(64, 1), 
        xlab = "Colonnes (X)", 
        ylab = "Lignes (Y)")
        
  if (any(valid_pts)) {
    points(px[valid_pts], py[valid_pts], pch = 18, col = "red", cex = 1.5)
    text(px[valid_pts], py[valid_pts], labels = gsub("_", " ", noms[valid_pts]), pos = 3, col = "black", font = 2, cex = 0.8)
  }
  
  # -- Colorbar --
  par(mar = c(4, 0.5, 2, 3))
  
  # Créer une matrice pour la barre de couleurs (verticale)
  colorbar_matrix <- matrix(seq(val_min, val_max, length.out = 100), nrow = 100, ncol = 1)
  
  image(1, seq(val_min, val_max, length.out = 100), t(colorbar_matrix),
        col = couleurs,
        axes = FALSE,
        xlab = "", ylab = "")
  
  # Ajouter l'axe avec les valeurs
  axis(4, at = pretty(c(val_min, val_max), n = 5), las = 1)
  mtext("Hauteur d'eau", side = 4, line = 2, cex = 0.8)
}
