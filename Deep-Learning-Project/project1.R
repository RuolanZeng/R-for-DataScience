require(jpeg)
require(OpenImageR)
require(RCurl)
library(keras)

# load train data
train_path <- "http://www.utdallas.edu/~yxs173830/cnnproject1/training/"
train_path_list <- list()
for (i in 1:10){
  train_path_list[i] <- paste(train_path,"n",i,"/n",i,"%20(", sep = "")
}
train_number <- c(111, 110, 122, 105, 113, 106, 114, 106, 104, 105)

train_data <- list()
train_label <- list()
num <- 1

for (i in 1:10){
  for(j in 1:train_number[i]){
    image <- readJPEG(getURLContent(paste(train_path_list[i],j,").jpg", sep = ""),binary=TRUE))
    rezised_image <- resizeImage(image, width = 100, height = 100, method = 'bilinear')
    train_data[[num]] <- rezised_image
    train_label[num] <- i
    num <- num+1
  }
}


for (i in 1:(num-1)) {train_data[[i]] <- array(train_data[[i]], dim <- c(100, 100, 3))}

train_image_array<-array(dim = c(num-1,100,100,3))

for (i in 1:(num-1)) {
  for (j in 1:100) {
    for (k in 1:100) {
      for (l in 1:3) {
        train_image_array[i,j,k,l]<-train_data[[i]][j,k,l]
      }
    }
  }
}

train_label <- array(as.numeric(unlist(train_label)))
train_label_encode <- to_categorical(train_label)
# train_label_encode <- train_label_encode[,2:11]

# load test data
test_path <- "http://www.utdallas.edu/~yxs173830/cnnproject1/validation/"
test_path_list <- list()
for (i in 1:10){
  test_path_list[i] <- paste(test_path,"n",i,"/n",i,"%20(", sep = "")
}
test_number <- c(28, 27, 30, 26, 28, 26, 28, 27, 26, 26)

test_data <- list()
test_label <- list()
num <- 1

for (i in 1:10){
  for(j in 1:test_number[i]){
    image <- readJPEG(getURLContent(paste(test_path_list[i],j,").jpg", sep = ""),binary=TRUE))
    rezised_image <- resizeImage(image, width = 100, height = 100, method = 'bilinear')
    test_data[[num]] <- rezised_image
    test_label[num] <- i
    num <- num+1
  }
}

for (i in 1:(num-1)) {test_data[[i]] <- array(test_data[[i]], dim <- c(100, 100, 3))}

test_image_array<-array(dim = c(num-1,100,100,3))

for (i in 1:(num-1)) {
  for (j in 1:100) {
    for (k in 1:100) {
      for (l in 1:3) {
        test_image_array[i,j,k,l]<-test_data[[i]][j,k,l]
      }
    }
  }
}

test_label <- array(as.numeric(unlist(test_label)))
test_label
test_label_encode <- to_categorical(test_label)
# test_label_encode <- test_label_encode[,2:11]


#model

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(100, 100, 3)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")

model <- model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 11, activation = "softmax")

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history<-model %>% fit(
  train_image_array, train_label_encode, 
  epochs = 15, batch_size=64
)


results <- model %>% evaluate(test_image_array, test_label_encode)
results

predict_classes(model,test_image_array)

