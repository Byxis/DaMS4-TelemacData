library(keras3)
library(tensorflow)

# -- Configuration --
BATCH_SIZE <- 32
EPOCHS <- 50
VALIDATION_SPLIT <- 0.2

# -- Chargement des données --
load_data <- function() {
  files <- list.files("../data", pattern = "_maxH_sully.csv", full.names = TRUE)
  
  X_list <- list()
  Y_list <- list()
  
  for (f in files) {
    base_name <- basename(f)
    params_part <- gsub(".*=(.*)_maxH.*", "\\1", base_name)
    params <- as.numeric(unlist(strsplit(params_part, ",")))
    
    mat <- tryCatch({
       as.matrix(read.csv(f, header = TRUE, row.names = 1))
    }, error = function(e) NULL)
    
    if (is.null(mat) || nrow(mat) != 64 || ncol(mat) != 64) next
    
    X_list[[length(X_list) + 1]] <- params
    Y_list[[length(Y_list) + 1]] <- as.vector(mat)
  }
  
  X <- do.call(rbind, X_list)
  Y_flat <- do.call(rbind, Y_list)
  
  # -- Normalisation des entrées --
  X_mean <- apply(X, 2, mean)
  X_std <- apply(X, 2, sd)
  X_std[X_std == 0] <- 1
  
  X_norm <- scale(X, center = X_mean, scale = X_std)
  save(X_mean, X_std, file = "normalization_stats.RData")
  
  num_samples <- nrow(X)
  Y <- array_reshape(Y_flat, c(num_samples, 64, 64, 1))
  
  return(list(X = X_norm, Y = Y))
}

data <- load_data()
X <- data$X
Y <- data$Y


# -- Construction du Modèle (Deep ResNet Decoder) --
layer_res_block <- function(x, filters, kernel_size = c(3,3), dropout = 0.1) {
  shortcut <- x
  
  y <- x %>%
    layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(dropout) %>%
    layer_conv_2d(filters = filters, kernel_size = kernel_size, padding = "same") %>%
    layer_batch_normalization()
  
  layer_add(list(y, shortcut)) %>%
    layer_activation("relu")
}

input_layer <- layer_input(shape = c(8))

x <- input_layer %>%
  layer_dense(units = 4 * 4 * 512) %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_reshape(target_shape = c(4, 4, 512))

x <- x %>% layer_res_block(filters = 512)

x <- x %>% 
  layer_conv_2d_transpose(filters = 256, kernel_size = c(3,3), strides = c(2,2), padding = "same") %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_res_block(filters = 256)

x <- x %>% 
  layer_conv_2d_transpose(filters = 128, kernel_size = c(3,3), strides = c(2,2), padding = "same") %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_res_block(filters = 128)

x <- x %>% 
  layer_conv_2d_transpose(filters = 64, kernel_size = c(3,3), strides = c(2,2), padding = "same") %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_res_block(filters = 64)

x <- x %>% 
  layer_conv_2d_transpose(filters = 32, kernel_size = c(3,3), strides = c(2,2), padding = "same") %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_res_block(filters = 32)

output <- x %>%
  layer_conv_2d(filters = 1, kernel_size = c(3,3), padding = "same", activation = "linear")

model <- keras_model(inputs = input_layer, outputs = output)

summary(model)

# -- Compilation --
model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(learning_rate = 0.001), 
  metrics = c("mae")
)

# -- Entraînement --
callbacks_list <- list(
  callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 10, min_lr = 0.00001),
  callback_early_stopping(monitor = "val_loss", patience = 20, restore_best_weights = TRUE)
)

history <- model %>% fit(
  x = X,
  y = Y,
  epochs = 200,
  batch_size = BATCH_SIZE,
  validation_split = VALIDATION_SPLIT,
  callbacks = callbacks_list,
  verbose = 1
)

# -- Sauvegarde --
if (!dir.exists("models/deeplearning")) dir.create("models/deeplearning", recursive = TRUE)
save_model(model, "models/deeplearning/telemac_deep_model.keras")
plot(history)
