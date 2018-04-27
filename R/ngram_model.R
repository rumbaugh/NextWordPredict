### This code uses probabilities of every N-gram occurring calculated
### in calc_ngram_probs.R to make predictions for the next word in an
### input sequence. You need to set these probabilities as the 
### variables unigramProbs, bigramProbs, and trigramProbs. The 
### variable unigrams_top3 should also be set as the top 3 most 
### probable unigrams. The variable capgrams also needs to be set,
### which should be a character vector containing all unigrams that 
### should be capitalized in the output, with the correct cases. 

options(java.parameters = "-Xmx8000m") #solved a Java memory error
library(tm)
library(RWeka)
library(reshape2)
library(knitr)
library(stringr)
library(data.table)

# Useful functions
get_last_tokens <- function(string, n) {
    # Takes input string and returns the last n words in it. Divisions
    # between words are found by looking for a pattern of non-whitespace
    # followed by whitespace followed by whitespace.
    pattern <- paste("\\S+(\\s+\\S+){", n - 1, "}$", sep="")
    return(substring(string, str_locate(string, pattern)[,1]))
}

get_nminus1_gram <- function(ngram) {
    # Takes a string (representing an N-gram) and returns the string
    # representing it's (N-1)-gram (i.e., removes the first word). 
    return(sub("\\s+\\S+$", "", ngram))
}

capitalize_first <- function(x) {
   # Takes a string and capitalizes the first letter
   tokens <- strsplit(x, '')[[1]]
   tokens[1] <- toupper(tokens[1])
   return(paste(tokens, collapse=''))
}

# Find (N-1)-grams for reference later.
bigram_minus1 <- get_nminus1_gram(names(bigramProbs))
trigram_minus1 <- get_nminus1_gram(names(trigramProbs))

unigramProbs <- sort(unigramProbs, decreasing=T)

should_capitalize <- function(word) {
    # Takes a word as input and returns the correct case by checking
    # it against capgrams. 
    if (word %in% tolower(capgrams)) {
      return(capgrams[tolower(capgrams) == word])
    } else {
      return(word)
    }
}
  
should_capitalize_table <- function(word.table) {
    # Applies the should_capitalize function to a table, as opposed
    # to a string. 
    cap_mask <- names(word.table) %in% capgrams
    names(word.table)[cap_mask] <- sapply(names(word.table)[cap_mask], 
                                          should_capitalize)
    return(word.table)
}
  
find_potential_ngrams <- function(ngram, threshold, top, cur_length) {
    ### Given an N-gram, finds the (N+1)-grams that begin with it.
    ### Inputs - 
    ###    ngram      - A string containing N words. 
    ###    threshold  - Probability cutoff used by the backoff model.
    ###	     		If the maximum probability of potential
    ###			(N+1)-grams is below this, it calls this function
    ###			on the same input ngram with the first word
    ###			removed and appends the output.   
    ###    top        - Return at least this many potential N-grams.
    ###    cur_length - Variable to preserve the length of the output
    ###	   	        between recursive calls. Always set this to 0
    ###			when calling this function from outside itself.
    ### Output - A character vector containing top N-grams. Because of
    ###	         the backoff model, these won't necessarily be 
    ###		 (N+1)-grams relative to N for the initial call. 
    ### The variables bigramProbs, trigramProbs, bigram_minus1, and 
    ### trigram_minus1 need to be set prior to calling this function.

    n <- length(strsplit(ngram, " ")[[1]])
    # If expanding this function to accept N-grams of higher order, 
    # append the corresponding variables corresponding to those orders
    # in the following two lines.
    plus1 <- list(bigramProbs, trigramProbs)[[n]]
    plus1_root <- list(bigram_minus1, trigram_minus1)[[n]]

    # Initially sets the potential output to be the (N+1)-grams that
    # complete the input N-gram. 
    potential_ngrams <- plus1[plus1_root == ngram]
    if (n > 1) {
      # If the input N-grams are at least bigrams, checks the backoff
      # model to potentially recursively call the function.
      if (length(potential_ngrams) == 0) {
        # If there are no potential ngrams, we obviously need to call
	# recursively to find some. 
        potential_ngrams <- c(potential_ngrams, 
                      find_potential_ngrams(get_last_tokens(ngram, n-1), 
		      threshold, top, cur_length))
      } else if 
      ((length(unique(names(get_last_tokens(potential_ngrams, 1)))) 
      	     + cur_length < top)
             | (max(potential_ngrams) < threshold)) {
	# If the maximum probability is less than the threshold value,
	# or if we haven't found at least 'top' many potential n-grams,
	# we call the function recursively.
        potential_ngrams <- c(potential_ngrams, 
            find_potential_ngrams(get_last_tokens(ngram, n-1), 
	    threshold, top, cur_length + 
            length(unique(names(get_last_tokens(potential_ngrams, 1))))))
      }
    }
    return(potential_ngrams)
}
  
ngram_model <- function(ngram) {
    ### Given an N-gram, finds the (N+1)-grams that begin with it.
    ### Inputs - 
    ###    ngram      - A string containing N words. 
    ### Removed Inputs -
    ### These inputs have been removed to simplify calling the function.
    ### They have been set in the first few lines to their default 
    ### values but could be added back in. 
    ###    threshold  - Probability cutoff used by the backoff model.
    ###	     		If the maximum probability of potential
    ###			(N+1)-grams is below this, it calls this function
    ###			on the same input ngram with the first word
    ###			removed and appends the output.   
    ###    top        - Return at least this many potential N-grams.
    ### Output - A character vector containing top N-grams. Because of
    ###	         the backoff model, these won't necessarily be 
    ###		 (N+1)-grams relative to N for the initial call. 
    ### The variables bigram_wper and bigram_wcom need to be set
    ### prior to calling this function. These should contain the top 3
    ### most likely words to occur after sentence-ending punctuation
    ### or a comma, respectively. In addition, unigramProbs, bigramprobs,
    ### unigram_minus1, and bigram_minus1 must be set for the 
    ### find_potential_ngrams function which is called internally. 

    threshold <- 0.05
    top <- 3
    if (trimws(ngram) == '') {
      # If the input is empty or only whitespace, return the top 3
      # most likely unigrams, capitalized because it's the start 
      # of a sentence. 
      return(sapply(names(unigrams_top3)[1:top], capitalize_first, 
             USE.NAMES = F))
    }
    # Pre-process the input by stripping whitespace, putting it into
    # lower case, and substituting period for all sentence-ending
    # punctuation marks. 
    ngram <- trimws(tolower(gsub("[?!]", '.', ngram)))

    # Checks if the last token is a comma followed by zero or more
    # white spaces. If so, return the top 3 most common words found
    # to follow commas. If not, cuts off the input at the last comma.
    splitcomma <- trimws(strsplit(paste(ngram, ' '), ',')[[1]])
    if (length(splitcomma) > 1) {
      ngram <- splitcomma[length(splitcomma)]
      if (trimws(ngram) == '') {
        return(names(bigrams_wcom[1:top]))
      }
    }

    # Checks if the last token is a period followed by zero or more
    # white spaces. If so, return the top 3 most common words found
    # to follow sentence-ending punctuation. If not, cuts off the 
    # input at the last period. Note that ! and ? are 
    # represented as periods at this point in the function. 
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
      # Since this is currently set up as a trigram model, it can only
      # make predictions off the last two words. If the input contains
      # more than that, cuts it down to the last two. This clause must
      # be updated if upgrading this function to a higher order model.
      splitgram <- c(splitgram[(n-1)], splitgram[n])
      ngram <- paste(splitgram, collapse=' ')
      n <- 2
    }
    
    # Finds potential (N+1)-grams to complete the input N-gram
    potential_ngrams <- find_potential_ngrams(ngram, threshold, top, 0)
    if(length(potential_ngrams) == 0) {
      # If no potential N-grams were found, return the most probable
      # unigrams
      potential_ngrams <- unigramProbs
    } 
    potential_ngrams <- sort(potential_ngrams, decreasing = T)
    # Takes the completion to be the last word in the potential N-grams.
    potential_ngrams<- unique(get_last_tokens(names(potential_ngrams), 
    		         1))[1:min(c(length(potential_ngrams), top))]
    potential_ngrams <- sapply(potential_ngrams, should_capitalize,
                          USE.NAMES = F) # Applies correct capitalization
    not_missing <- length(potential_ngrams[!(is.na(potential_ngrams))])
    if (not_missing < 3) {
      # If fewer than 3 potential N-grams are returned, add on to that
      # the most probably unigrams until you have 3 total to output. 
      potential_ngrams <- c(potential_ngrams[!(is.na(potential_ngrams))], 
      		            names(unigrams_top3))[1:3]
    }
    return(potential_ngrams)
}
  
