## Code for constructing an LSTM model to predict the next character
## to follow an input string. Also contains code to use this model
## to predict possible next words following an input string. Note that 
## this code was not deployed in the final product because the
## model was inefficient. 

options(java.parameters = "-Xmx8000m")
library(tm)
library(reshape2)
library(knitr)
library(data.table)
library(keras)
library(kerasR)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)

maxlen <- 40 #maximum characters to use in a prediction.

# The variables here need to put paths to the files in the input dataset. 
con1 <- file(blogs_file, "r")
con2 <- file(news_file, "r")
con3 <- file(twitter_file, "r")

corpus <- c(readLines(con1), readLines(con2), readLines(con3))
corpus <- sample(corpus, length(corpus))

tokenize_corpus <- function(text) {
  text <- str_to_lower(text) %>%
    str_c(collapse = "\n") %>%
    tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE)
}
corpus <- tokenize_corpus(corpus)

# Cuts the corpus down to only the following characters
chars <- strsplit("abcdefghijklmnopqrstuvqxyz.,?!' ", split = '')[[1]]
corpus <- corpus[corpus %in% chars]

prepare_text <- function(text, by){
   if(missing(by)) by <- 3
   seq.max <- length(text) - maxlen - 1
   if(seq.max < 1) seq.max <- 1
   dataset <- map(
      seq(1, seq.max, by = by), 
      ~list(sentence = text[.x:(.x + maxlen - 1)], next_char = text[.x + maxlen])
   )
   dataset <- transpose(dataset)
   x <- array(0, dim = c(length(dataset$sentence), maxlen, length(chars)))
   y <- array(0, dim = c(length(dataset$sentence), length(chars)))

   for(i in 1:length(dataset$sentence)){
  
      x[i,,] <- sapply(chars, function(x){
           as.integer(x == dataset$sentence[[i]])
      })
  
      y[i,] <- as.integer(chars == dataset$next_char[[i]])
   }
   return(list('x' = x, 'y' = y))
}

prep <- prepare_text(corpus)
x <- prep$x
y <- prep$y
rm(prep)

model <- keras_model_sequential()

model %>%
  layer_lstm(128, input_shape = c(maxlen, length(chars))) %>%
  layer_dense(length(chars)) %>%
  layer_activation("softmax")

optimizer <- optimizer_rmsprop(lr = 0.01)

model %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = optimizer
)

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

on_epoch_end <- function(epoch, logs) {
  
  cat(sprintf("epoch: %02d ---------------\n\n", epoch))
  
  for(diversity in c(0.2, 0.5, 1, 1.2)){
    
    cat(sprintf("diversity: %f ---------------\n\n", diversity))
    
    start_index <- sample(1:(length(corpus) - maxlen), size = 1)
    sentence <- corpus[start_index:(start_index + maxlen - 1)]
    generated <- ""
    
    for(i in 1:400){
      
      x <- sapply(chars, function(x){
        as.integer(x == sentence)
      })
      x <- array_reshape(x, c(1, dim(x)))
      
      preds <- predict(model, x)
      next_index <- sample_mod(preds, diversity)
      next_char <- chars[next_index]
      
      generated <- str_c(generated, next_char, collapse = "")
      sentence <- c(sentence[-1], next_char)
      
    }
    
    cat(generated)
    cat("\n\n")
    
  }
}

print_callback <- callback_lambda(on_epoch_end = on_epoch_end)

model %>% fit(
  x, y,
  batch_size = 128,
  epochs = 25,
  callbacks = print_callback
)

keras_save(model, path) # Uses the kerasR library to save the model

predict_completion <- function(text, model, max_iter) {
    if(missing(max_iter)) max_iter <- 100
    predicted_text <- text
    completion <- ''
    for (i in 1:max_iter) {
    	token_text <- tokenize_corpus(predicted_text)
        token_text <- token_text[max(c(length(token_text) - 
		                  maxlen + 1), 1):length(token_text)]
    	prep <- prepare_text(token_text)
    	x <- prep$x
	rm(prep)
	if (sum(is.na(x)) > 0) {
	   x <- x[!(is.na(x))]
	   len <- length(x)/length(chars)
	   dim(x) <- c(len, length(chars))
	   addlen <- (maxlen - len) * length(chars)
	   x_add <- numeric(addlen)
	   dim(x_add) <- c(maxlen - len, length(chars))
	   x <- rbind(x_add, x)
	   dim(x) <- c(1, maxlen, length(chars))
	}
	pred <- predict_classes(model, x)
	next_char <- chars[pred]
	predicted_text <- paste(predicted_text, next_char, sep='')
	completion <- paste(completion, next_char, sep='')
	if (next_char == ' ') {
	   return(completion)
	}
    }
    print('Max iterations reached')
    return(completion)
}
	
convert_to_feature_matrix <- function(sentence) {    
      x <- sapply(chars, function(x){
        as.integer(x == sentence)
      })
      x <- array_reshape(x, c(1, dim(x)))
      if (dim(x)[2] < maxlen) {
    	prep <- prepare_text(sentence)
    	x <- prep$x
	rm(prep)
	if (sum(is.na(x)) > 0) {
	   x <- x[!(is.na(x))]
	   len <- length(x)/length(chars)
	   dim(x) <- c(len, length(chars))
	   addlen <- (maxlen - len) * length(chars)
	   x_add <- numeric(addlen)
	   dim(x_add) <- c(maxlen - len, length(chars))
	   x <- rbind(x_add, x)
	   dim(x) <- c(1, maxlen, length(chars))
	} 
      } else if (dim(x)[2] > maxlen) {
	x <- x[, (dim(x)[2] - maxlen + 1):(dim(x)[2]),]
	x <- array_reshape(x, c(1, dim(x)))
      }
      return(x)
}

generate_completion <- function(model, text, diversity, max_iter) {
    if(missing(max_iter)) max_iter <- 100
    if(missing(diversity)) diversity <- 0.2
    if (length(text) == 1) {
       sentence <- tokenize_corpus(text)
    } else {
       sentence <- text[(length(text) - maxlen + 1):length(text)]
    }
    generated <- ""
    
    for(i in 1:max_iter){
      x <- convert_to_feature_matrix(sentence)
      preds <- predict(model, x)
      next_index <- sample_mod(preds, diversity)
      next_char <- chars[next_index]
      
      generated <- str_c(generated, next_char, collapse = "")
      if (length(sentence) > maxlen) {
         sentence <- c(sentence[-1], next_char)
      } else {sentence <- c(sentence, next_char)}
      if (next_char == ' ') return(generated)
    }
    print('Max iterations reached')
    return(generated)
}

find_top_predictions <- function(model, text, top, max_iter, div_step, add_whitespace) {
    if(missing(add_whitespace)) add_whitespace <- F
    if(missing(max_iter)) max_iter <- 5
    if(missing(div_step)) div_step <- 0.01
    if (add_whitespace) text <- paste(text, ' ', sep='')
    diversity <- 0.01
    weights <- 1.0/diversity
    predictions <- generate_completion(model, text, diversity=diversity)
    pred_table <- data.table(predictions, weights)
    for (batch in 1:max_iter) {
       diversity <- seq.default(diversity + div_step, diversity + 10*div_step, div_step)
       weights <- 1.0/diversity
       predictions <- character(10)
       for (i in 1:10) {
           predictions[i] <- generate_completion(model, text, diversity=diversity[i])
       }
       pred_table <- rbind(pred_table, data.table(predictions, weights))
       diversity <- diversity[10]
       top_table <- pred_table[,list(weight=sum(weights)),by=predictions]
       if (length(top_table$predictions) >= top) {
           top_table <- top_table[order(weights, decreasing=T),]
	   return(trimws(top_table$predictions[1:top]))
       }
    }
    top_table <- top_table[order(weights, decreasing=T),]
    return(trimws(top_table$predictions[1:top]))
}
