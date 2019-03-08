# install.packages("tm")
library(tm)

data <- read.csv("https://github.com/lypf2018/TextClassificationYouTubeContent/raw/master/dataset/Youtube05-Shakira.csv")
content <- data$CONTENT

########## pre-processing

corpus <- (VectorSource(content))
corpus <- Corpus(corpus)

# remove url
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
corpus <- tm_map(corpus, removeURL)
# for (i in 1:50) print(corpus[[i]]$content)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

for (j in seq(corpus)) {
  corpus[[j]] <- gsub("/", " ", corpus[[j]])
  corpus[[j]] <- gsub("@", " ", corpus[[j]])
  corpus[[j]] <- gsub("\\|", " ", corpus[[j]])
  corpus[[j]] <- gsub("\u2028", " ", corpus[[j]])  # This is an ascii character that did not translate, so it had to be removed.
  corpus[[j]] <- gsub("[^\x01-\x7F]", " ", corpus[[j]]) # remove emotions
}

corpus <- tm_map(corpus,removeWords,stopwords("english"))
corpus <- tm_map(corpus, tolower)

# stem 
# install.packages("SnowballC")
library(SnowballC)
corpus <- tm_map(corpus, stemDocument) # stem(eg.cats -> cat),need package SnowballC

corpus <- tm_map(corpus, stripWhitespace) 

temp_labels <- data$CLASS
text <- c()
labels <- c()
for (i in 1:370)
{ if (nchar(corpus[[i]]$content) != 0)
{text <- c(text, corpus[[i]]$content)
    labels <- c(labels, temp_labels[i])}
}

########## tokenize

library(keras)
tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>% fit_text_tokenizer(text)
sequences <- texts_to_sequences(tokenizer, text)

########## split data into train and test parts

index = sort(sample(length(sequences), length(sequences)*.7))

train_data <- sequences[index]
test_data <- sequences[-index]
train_label <- labels[index]
test_label <- labels[-index]

vectorize_sequences <- function(sequences,
                                dimension = 10000) {
  results <- matrix(0, nrow = length(sequences),
                    ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}

x_train <- vectorize_sequences(train_data)
# Our vectorized test data
x_test <- vectorize_sequences(test_data)

# Vectorize Labels
y_train <- as.numeric(train_label)
y_test <- as.numeric(test_label)

########## create deep network using Keras

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu",
              input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 10,
              batch_size = 512)


########## validation part 

val_indices <- 1:150
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 10,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

plot(history)

########## Apply model on test part

results <- model %>% evaluate(x_test, y_test)
results

