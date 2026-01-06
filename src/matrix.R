load("./data/points_interet.RData")

dossier <- "./data"
fichiers <- list.files(dossier, pattern = "_maxH_sully.csv", full.names = TRUE)

cible = points_interet$coordoneesLocales$caserne_pompiers

cible

donnees_extraites <- lapply(fichiers, function(f) {
  
  nom_fichier <- basename(f)
  params_bruts <- gsub(".*=(.*)_maxH.*", "\\1", nom_fichier)
  
  params_vec <- as.numeric(unlist(strsplit(params_bruts, ",")))
  params_vec <- params_vec[1:8] 
  
  matrice <- as.matrix(read.csv(f, header = TRUE, row.names = 1))
  print(nom_fichier)
  valeur_xy <- matrice[cible["row.y"], cible["col.x"]]
  
  return(c(params_vec, valeur_xy))
})

matrice_finale <- do.call(rbind, donnees_extraites)
df_final <- as.data.frame(matrice_finale)

colnames(df_final) <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm", "hauteur")

head(df_final)
