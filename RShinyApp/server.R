
library(shiny)
library(data.table)
library(stringr)

shinyServer(function(input, output, session) {
#  unigramProbs <- rowSums(read.csv('data/unigramProbs.csv', 
#                           row.names = 1))
  unigramProbs <- rowSums(read.csv('data/unigrams_top3.csv', 
                                   row.names = 1))
  unigrams_top3 <- unigramProbs
  bigramProbs <- rowSums(read.csv('data/bigramProbs.csv', 
                                   row.names = 1))
  bigrams_wper <- rowSums(read.csv('data/bigrams_wper.csv', 
                                  row.names = 1, header=F))
  bigrams_wcom <- rowSums(read.csv('data/bigrams_wcom.csv', 
                                   row.names = 1, header=F))
  trigramProbs <- rowSums(read.csv('data/trigramProbs.csv', 
                                   row.names = 1))
  capgrams <- as.character((read.csv('data/capgrams.csv'))$x)
  #capgrams <- names(unigramProbs)[!(tolower(names(unigramProbs)) == names(unigramProbs))]
  names(unigramProbs) <- tolower(names(unigramProbs))
  
  
  capitalize_first <- function(x) {
    tokens <- strsplit(x, '')[[1]]
    tokens[1] <- toupper(tokens[1])
    return(paste(tokens, collapse=''))
  }
  
  check_cap <- function(x) {
    tokens <- strsplit(x, '')[[1]]
    if (length(grep('[a-zA-Z]', tokens[1]))) {
      tokens[1] <- toupper(tokens[1])
    } else {
      tokens[1] <- 'z'}
    return(paste(tokens, collapse=''))
  }
  
  get_last_tokens <- function(string, n) {
    pattern <- paste("\\S+(\\s+\\S+){", n - 1, "}$", sep="")
    return(substring(string, str_locate(string, pattern)[,1]))
  }
  
  get_nminus1_gram <- function(ngram) {
    return(sub("\\s+\\S+$", "", ngram))
  }
  
  bigram_minus1 <- get_nminus1_gram(names(bigramProbs))
  trigram_minus1 <- get_nminus1_gram(names(trigramProbs))
  should_capitalize <- function(word) {
    if (word %in% tolower(capgrams)) {
      return(capgrams[tolower(capgrams) == word])
    } else {
      return(word)
    }
  }
  
  should_capitalize_table <- function(word.table) {
    cap_mask <- names(word.table) %in% capgrams
    names(word.table)[cap_mask] <- sapply(names(word.table)[cap_mask], 
                                          should_capitalize)
    return(word.table)
  }
  
  find_potential_ngrams <- function(ngram, threshold, top, cur_length) {
    n <- length(strsplit(ngram, " ")[[1]])
    plus1 <- list(bigramProbs, trigramProbs)[[n]]
    plus1_root <- list(bigram_minus1, trigram_minus1)[[n]]
    potential_ngrams <- plus1[plus1_root == ngram]
    if (n > 1) {
      if (length(potential_ngrams) == 0) {
        potential_ngrams <- c(potential_ngrams, 
                              find_potential_ngrams(get_last_tokens(ngram, n-1), threshold,
                                                    top, cur_length))
      } else if ((length(unique(names(get_last_tokens(potential_ngrams, 1)))) + cur_length < top)
                 | (max(potential_ngrams) < threshold)) {
        potential_ngrams <- c(potential_ngrams, 
                              find_potential_ngrams(get_last_tokens(ngram, n-1), threshold,
                                                    top, cur_length + length(unique(names(get_last_tokens(potential_ngrams, 1))))))
      }
    }
    return(potential_ngrams)
  }
  
  ngram_model <- function(ngram) {
    threshold <- 0.05
    top <- 3
    if (trimws(ngram) == '') {
      return(sapply(names(unigrams_top3)[1:top], capitalize_first, 
             USE.NAMES = F))
    }
    ngram <- trimws(tolower(gsub("[?!]", '.', ngram)))
    splitcomma <- trimws(strsplit(paste(ngram, ' '), ',')[[1]])
    if (length(splitcomma) > 1) {
      ngram <- splitcomma[length(splitcomma)]
      if (trimws(ngram) == '') {
        return(names(bigrams_wcom[1:top]))
        
      }
    }
    splitper <- trimws(strsplit(paste(ngram, ' '), '\\.')[[1]])
    if (length(splitper) > 1) {
      ngram <- splitper[length(splitper)]
      if (trimws(ngram) == '') {
          return(names(bigrams_wper[1:top]))
      }
    }
    splitgram <- strsplit(ngram, " ")[[1]]
    n <- length(splitgram)
    if (n > 2) {
      splitgram <- c(splitgram[(n-1)], splitgram[n])
      ngram <- paste(splitgram, collapse=' ')
      n <- 2
    }
    potential_ngrams <- find_potential_ngrams(ngram, threshold, top, 0)
    if(length(potential_ngrams) == 0) {
      potential_ngrams <- unigramProbs
    } 
    potential_ngrams <- sort(potential_ngrams, decreasing = T)
    potential_ngrams<- unique(get_last_tokens(names(potential_ngrams), 1))[1:min(c(length(potential_ngrams), top))]
    potential_ngrams <- sapply(potential_ngrams, should_capitalize,
                                  USE.NAMES = F)
    not_missing <- length(potential_ngrams[!(is.na(potential_ngrams))])
    if (not_missing < 3) {
      potential_ngrams <- c(potential_ngrams[!(is.na(potential_ngrams))], names(unigrams_top3))[1:3]
    }
    return(potential_ngrams)
  }
  
  
  output$suggest1 <- renderUI({
  actionButton("suggest1", label = label1())
  })
  label1 <- reactive({
      label <- renderText({ ngram_model(input$text)[1] })
  })
  output$suggest2 <- renderUI({
    actionButton("suggest2", label = label2())
  })
  label2 <- reactive({
    label <- renderText({ ngram_model(input$text)[2] })
  })
  output$suggest3 <- renderUI({
    actionButton("suggest3", label = label3())
  })
  label3 <- reactive({
    label <- renderText({ ngram_model(input$text)[3] })
  })
  
  observeEvent(input$suggest1,
               updateTextInput(session, "text", value=paste(input$text, ngram_model(input$text)[1]))
  )
  observeEvent(input$suggest2,
                  updateTextInput(session, "text", value=paste(input$text, ngram_model(input$text)[2]))
  )
  observeEvent(input$suggest3,
               updateTextInput(session, "text", value=paste(input$text, ngram_model(input$text)[3]))
  )
})
