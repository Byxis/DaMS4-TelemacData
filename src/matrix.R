
load("./RData/points_interet.RData")

# Iterate over all available points
available_points <- names(points_interet$coordoneesLocales)
print(paste("Generating matrices for:", paste(available_points, collapse=", ")))

fichiers <- list.files("./data", pattern = "_maxH_sully.csv", full.names = TRUE)

# Initialize storage for all points
data_points_list <- list()
for (pt_name in available_points) {
    data_points_list[[pt_name]] <- list()
}

total_files <- length(fichiers)
for (i in seq_along(fichiers)) {
    f <- fichiers[i]
    if (i %% 100 == 0) print(paste("Processing file", i, "/", total_files))
    
    # Parse params
    nom_fichier <- basename(f)
    params_bruts <- gsub(".*=(.*)_maxH.*", "\\1", nom_fichier)
    params_vec <- as.numeric(unlist(strsplit(params_bruts, ",")))
    params_vec <- params_vec[1:8]
    
    matrice <- as.matrix(read.csv(f, header = TRUE, row.names = 1))
    
    for (pt_name in available_points) {
        cible <- points_interet$coordoneesLocales[[pt_name]]
        if (!is.null(cible)) {
            r_idx <- if("row.y" %in% names(cible)) as.integer(cible["row.y"]) else as.integer(cible["row"])
            c_idx <- if("col.x" %in% names(cible)) as.integer(cible["col.x"]) else as.integer(cible["col"])
            
            valeur_xy <- NA
            if (!is.na(r_idx) && !is.na(c_idx) && r_idx >= 1 && r_idx <= nrow(matrice) && c_idx >= 1 && c_idx <= ncol(matrice)) {
                valeur_xy <- matrice[r_idx, c_idx]
            } else {
                # If coordinates are out of bounds or NA, we assign NA.
                valeur_xy <- NA
            }
            
            # Store: params + value
            data_points_list[[pt_name]][[length(data_points_list[[pt_name]]) + 1]] <- c(params_vec, valeur_xy)
        }
    }
}

# Aggregate and Save for each point
if (!dir.exists("./RData")) dir.create("./RData")

for (pt_name in available_points) {
    print(paste("Saving data for:", pt_name))
    raw_list <- data_points_list[[pt_name]]
    if (length(raw_list) > 0) {
        matrice_finale <- do.call(rbind, raw_list)
        df_final <- as.data.frame(matrice_finale)
        colnames(df_final) <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm", "hauteur")
        
        save_path <- paste0("./RData/", pt_name, "_matrix.RData")
        save(df_final, file = save_path)
    }
}
