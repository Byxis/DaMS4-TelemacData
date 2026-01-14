rm(list = ls())
set.seed(42)

# -------------------------
# User parameter
# -------------------------

poi <- "gare_sully"   # "parc_chateau", "centre_sully", "gare_sully", "caserne_pompiers"

# -------------------------
# Project paths
# -------------------------
get_project_root <- function() {
  script_path <- tryCatch(
    normalizePath(sys.frames()[[1]]$ofile),
    error = function(e) NULL
  )
  if (!is.null(script_path)) {
    return(dirname(dirname(script_path)))
  }
  getwd()
}

project_root <- get_project_root()
rdata_dir <- file.path(project_root, "RData")
stopifnot(dir.exists(rdata_dir))

# -------------------------
# Load POI matrix
# -------------------------
rdata_file <- file.path(rdata_dir, paste0(poi, "_matrix.RData"))
stopifnot(file.exists(rdata_file))

e <- new.env(parent = emptyenv())
load(rdata_file, envir = e)

obj_names <- ls(e)
is_good_dataset <- function(x) {
  (is.matrix(x) || is.data.frame(x)) && ncol(x) == 9
}

good <- obj_names[
  vapply(obj_names, function(nm) {
    is_good_dataset(get(nm, envir = e))
  }, logical(1))
]

stopifnot(length(good) >= 1)

df <- as.data.frame(get(good[1], envir = e))
for (j in 1:9) df[[j]] <- as.numeric(df[[j]])
colnames(df) <- c(paste0("x", 1:8), "y")

# -------------------------
# Random Forest
# -------------------------
library(randomForest)

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) {
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}

# -------------------------
# 10-fold cross-validation
# -------------------------
k <- 10
n <- nrow(df)
fold_id <- sample(rep(1:k, length.out = n))

metrics <- data.frame(fold = integer(), RMSE = numeric(), MAE = numeric(), R2 = numeric())

for (fold in 1:k) {
  train_df <- df[fold_id != fold, ]
  test_df  <- df[fold_id == fold, ]
  
  rf_fit <- randomForest(y ~ ., data = train_df, ntree = 500, importance = TRUE)
  pred <- predict(rf_fit, newdata = test_df)
  
  metrics <- rbind(
    metrics,
    data.frame(
      fold = fold,
      RMSE = rmse(test_df$y, pred),
      MAE  = mae(test_df$y, pred),
      R2   = r2(test_df$y, pred)
    )
  )
}

cat("\nPOI:", poi, "\n")
print(metrics)
print(data.frame(
  RMSE_mean = mean(metrics$RMSE),
  RMSE_sd   = sd(metrics$RMSE),
  MAE_mean  = mean(metrics$MAE),
  MAE_sd    = sd(metrics$MAE),
  R2_mean   = mean(metrics$R2),
  R2_sd     = sd(metrics$R2)
))

# -------------------------
# Final model
# -------------------------
final_model <- randomForest(y ~ ., data = df, ntree = 500, importance = TRUE)
print(summary(final_model))
print(importance(final_model))

# -------------------------
# Single-point prediction
# -------------------------
test_index <- 137

x_test <- df[test_index, 1:8, drop = FALSE]
y_true <- df$y[test_index]
y_pred <- predict(final_model, newdata = x_test)

cat("\nSimulation index:", test_index, "\n")
print(x_test)
cat("True height   :", y_true, "\n")
cat("RF prediction :", y_pred, "\n")
cat("Abs error     :", abs(y_true - y_pred), "\n")
