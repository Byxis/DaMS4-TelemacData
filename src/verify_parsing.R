
# Script to verify parsing logic

data_dir <- "../data"
if (!dir.exists(data_dir)) {
  data_dir <- "data"
  if (!dir.exists(data_dir)) {
      data_dir <- "c:/Users/Alexis/OneDrive/Travail/DaMS4/S7/DaMS4-TelemacData/data"
  }
}
print(paste("Data Dir:", data_dir))

files <- list.files(data_dir, pattern = "_maxH_sully.csv", full.names = FALSE)
print(paste("Found", length(files), "files"))

if (length(files) > 0) {
    f <- files[1]
    print(paste("Sample file:", f))
    
    PARAM_NAMES <- c("er", "ks2", "ks3", "ks4", "ks_fp", "of", "qmax", "tm")
    
    parts <- strsplit(f, "tm=")[[1]]
    if (length(parts) >= 2) {
        rest <- parts[2]
        val_part <- sub("_maxH_sully.csv", "", rest)
    
        vals <- strsplit(val_part, ",")[[1]]
        print("Extracted values strings:")
        print(vals)
        
        if (length(vals) >= 8) {
             nums <- as.numeric(vals[1:8])
             print("Numeric values:")
             print(setNames(nums, PARAM_NAMES))
        } else {
             print("Not enough values")
        }
    } else {
        print("Split 'tm=' failed")
    }
}
