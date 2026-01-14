for file in ../RData/*.RData; do
    # On extrait uniquement le nom du fichier (sans le chemin ../RData/)
    filename=$(basename "$file")
    
    echo "Traitement de : $filename"
    
    # Exécution du script R avec le nom du fichier en argument
    Rscript ./lasso.R "$filename"
done
