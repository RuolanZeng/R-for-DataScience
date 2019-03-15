source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library (EBImage)

data = read.csv("/Users/ruolanzeng/Documents/UTD/R/R-for-DataScience/CNN/test.txt")
colnames(data) <- c("url")
View(data)

image1 = readImage(toString(data$url[1]))
image1 <- resize(image1, w = 300, h = 300)
grayimg <- channel(image1, "gray")
image_array <-as.array(grayimg)

for (i in 2:length(data$url)){
  print(data$url[i])
  image = readImage(toString(data$url[i]))
  image <- resize(image, w = 300, h = 300)
  grayimg <- channel(image, "gray")
  image_array <-rbind(image_array,as.array(grayimg))
}

dim(image_array) <- c(length(data$url),300,300)
dim(image_array)

label1 <- array(1,16)
label2 <- array(0,6)

labels <- c(label1, label2)
labels

index = sample(1:nrow(image_array),round(0.8*nrow(image_array)))
train_image <- as.array(image_array[index,,])
test_image <- as.array(image_array[-index,,])

train_label <- as.array(labels[index])
test_label <- as.array(labels[-index])

train_image <- array_reshape(train_image, c(nrow(train_image), 300, 300, 1))
test_image <- array_reshape(test_image, c(nrow(test_image), 300, 300, 1))

library(keras)

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(300, 300, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")

model <- model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 1, activation = "softmax")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(
  train_image, train_label, 
  epochs = 10, batch_size=64
)

results <- model %>% evaluate(test_image, test_label)
results
