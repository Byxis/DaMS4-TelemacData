
load("./RData/points_interet.RData")

# Available interest points :
# - gare_sully
# - parc_chateau
# - centre_sully
# - caserne_pompiers
# - domaine_epinoy NOT WORKING AS OUT OF SCOPE

X <- "gare_sully"

fichiers <- list.files("./data", pattern = "_maxH_sully.csv", full.names = TRUE)

cible <- points_interet$coordoneesLocales[[X]]

if (is.null(cible)) {
  stop(paste("Le point d'intérêt", X, "n'existe pas dans les données chargées."))
}

donnees_extraites <- lapply(fichiers, function(f) {
  
  nom_fichier <- basename(f)
  params_bruts <- gsub(".*=(.*)_maxH.*", "\\1", nom_fichier)
  
  params_vec <- as.numeric(unlist(strsplit(params_bruts, ",")))
  params_vec <- params_vec[1:8] 
  
  matrice <- as.matrix(read.csv(f, header = TRUE, row.names = 1))
  print(nom_fichier)
    
  r_idx <- cible["row.y"]
  c_idx <- cible["col.x"]
  
  vis_r <- c_idx
  vis_c <- 65 - r_idx
  
  valeur_xy <- matrice[vis_r, vis_c]
  
  return(c(params_vec, valeur_xy))
})

donnees_extraites

matrice_finale <- do.call(rbind, donnees_extraites)
df_final <- as.data.frame(matrice_finale)

colnames(df_final) <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm", "hauteur")

save_path <- paste0("./RData/", X, "_matrix.RData")
save(df_final, file = save_path)
head(df_final)
