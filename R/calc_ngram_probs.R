### This code calculates the probabilities of every N-gram in a
### corpus occuring, using Kneser-Ney smoothing. The final model
### used for the R Shiny App used unigrams, bigrams, and trigrams,
### with the mincnt parameter (lowest count N-grams to include)
### set at 10. 

options(java.parameters = "-Xmx8000m") #solved a Java memory error
library(tm)
library(RWeka)
library(reshape2)
library(knitr)
library(stringr)
library(data.table)

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

# Loads data into a tm VCorpus structure. Set data_dir to be directory 
# containing all data files to load
vc_wcap <- VCorpus(DirSource(data_dir), readerControl = list(language='lat'))

# Initial pre-processing of corpus. Numbers are removed and white space 
# stripped in all cases. The pipeline brances off after this to create a
# VCorpus object with and without capitalization. This is used later to 
# figure out which words need to be capitalized in the final product.
vc_wcap <- tm_map(vc_wcap, removeNumbers)
vc_wcap <- tm_map(vc_wcap, stripWhitespace)
vc <- tm_map(vc_wcap, content_transformer(tolower))

# For the object with capitalization, all non-alphabetical characters 
# (except ') are removed.
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
vc_wcap <- tm_map(vc_wcap, f, "[^ a-zA-Z']")

# The pipeline branches off again to create objects that focus on sentence
# ending punctuation (.?!) and commas. This is to find out the most common
# words that follow these punctuation marks for the final product. In each
# case, all other punctuation is removed (except ') and the [.?!] or commas
# are replaced with the words 'periodhere' and 'commahere', respectively
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
vc_wper <- tm_map(vc, f, "[^ a-zA-Z.?!']")
f <- content_transformer(function(x, pattern) gsub(pattern, ".", x))
vc_wper <- tm_map(vc, f, "[?!]")
f <- content_transformer(function(x, pattern) gsub(pattern, " periodhere ", x))
vc_wper <- tm_map(vc, f, "[.]")
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
vc_wcom <- tm_map(vc, f, "[^ a-zA-Z,']")
f <- content_transformer(function(x, pattern) gsub(pattern, " commahere ", x))
vc_wcom <- tm_map(vc, f, "[,]")

vc <- tm_map(vc, removePunctuation) # Remove punctuation for main pipeline


# Functions for creating N-grams from the corpus, using RWeka. Can be 
# trivially extended to higher N by increasing the min and max parameters
BigramTokenizer <- function(x) NGramTokenizer(x, 
		Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, 
		 Weka_control(min = 3, max = 3))

# Document Term Matrices are constructed for different N-grams. Bigram DTMs
# are constructed for the 'periodhere' and 'commahere' pipelines.
vcDTM <- DocumentTermMatrix(vc, control = list(wordLengths=c(1, Inf)))
vc_bigram_tdm <- DocumentTermMatrix(vc, 
	      control = list(wordLengths=c(1, Inf), 
	      tokenize = BigramTokenizer))
vc_bigram_wcom <- DocumentTermMatrix(vc_wcom, 
	       control = list(wordLengths=c(1, Inf), 
	       tokenize = BigramTokenizer, removepunctuation=F))
vc_bigram_wper <- DocumentTermMatrix(vc_wper, 
	       control = list(wordLengths=c(1, Inf),
	        tokenize = BigramTokenizer, removepunctuation=F))
vc_trigram_tdm <- DocumentTermMatrix(vc, 
	       control = list(wordLengths=c(1, Inf), 
	       tokenize = TrigramTokenizer))

mincnts <- 10 #N-grams that occur less often than this are cut

# Calculates counts for every unique N-gram in the corpus
unigrams <- colSums(as.matrix(vcDTM))
unigrams <- unigrams[unigrams > mincnt]
bigrams <- colSums(as.matrix(vc_bigram_tdm))
bigrams <- bigrams[bigrams > mincnt]
trigrams <- colSums(as.matrix(vc_trigram_tdm))
trigrams <- trigrams[trigrams > mincnt]


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

check_cap <- function(x) {
   # Function used for checking if a word is capitalized. 
   # Modification of capitalize_first that never returns NA.
   # Returns lowercase 'z' if first character isn't alphabetical
   # in order to catch these words later.
   tokens <- strsplit(x, '')[[1]]
   if (length(grep('[a-zA-Z]', tokens[1]))) {
      tokens[1] <- toupper(tokens[1])
   } else {
      tokens[1] <- 'z'}
   return(paste(tokens, collapse=''))
}

# Creates DTM for the VCorpus object with capitalize preserved, and
# counts occurence of each N-gram, observing capitalization.
vcDTM_cap <- DocumentTermMatrix(vc_wcap, control = list(tolower=F, wordLengths=c(1, Inf)))
capgrams <- colSums(as.matrix(vcDTM_cap))
capgrams <- capgrams[capgrams > mincnt]

# Cuts out all N-grams that don't start with a capital letter. 
capgrams <- capgrams[sapply(names(capgrams), check_cap) ==
	    				     names(capgrams)]

# Matches capitalized N-grams with corresponding case-insensitive unigrams.
lower_uni <- unigrams[tolower(names(capgrams))]
no.na.lower.uni.mask <- !(is.na(names(lower_uni)))
lower_uni <- lower_uni[no.na.lower.uni.mask]

# Calculates what percentage of occurences of a given word are
# capitalized. Preserves the ones where it's over half. This will
# be used later in the app to determine what words should be 
# capitalized when returned as predictions.
capgrams <- capgrams[no.na.lower.uni.mask]/lower_uni
capgrams <- names(capgrams)[capgrams > 0.5]

# Finds the most common words that occur after commas or sentence
# ending punctuation. The ones that occur after sentence-ending 
# punctuation are capitalized to present users with the correct
# case when output by the app.
bigrams_wcom <- colSums(as.matrix(vc_bigram_wcom))
bigrams_wcom <- bigrams_wcom[bigrams_wcom > mincnt]

bigrams_wcom <- bigrams_wcom['commahere' == 
	     	get_nminus1_gram(names(bigrams_wcom))]
bigrams_wcom = sort(bigrams_wcom, decreasing=T)
bigrams_wcom <- bigrams_wcom / sum(bigrams_wcom)
bigrams_wcom <- bigrams_wcom[1:3]
names(bigrams_wcom) <- get_last_tokens(names(bigrams_wcom), 1)


bigrams_wper <- colSums(as.matrix(vc_bigram_wper))
bigrams_wper <- bigrams_wper[bigrams_wper > mincnt]

bigrams_wper <- bigrams_wper['periodhere' == 
	     	get_nminus1_gram(names(bigrams_wper))]
bigrams_wper = sort(bigrams_wper, decreasing=T)
bigrams_wper <- bigrams_wper / sum(bigrams_wper)
bigrams_wper <- bigrams_wper[1:4]
names(bigrams_wper) <- as.character(sapply(get_last_tokens(names(bigrams_wper), 1), capitalize_first))
if ('Periodhere' %in% names(bigrams_wper)) {
   bigrams_wper <- bigrams_wper[names(bigrams_wper) != 'Periodhere']
} else {
   bigrams_wper <- bigrams_wper[1:3]
}



kneserNey <- function(ngrams, delta, prevcall) {
    ### Calculates probabilities of N-grams occuring using Kneser-Ney
    ### smoothing. 
    ### Inputs:
    ###     ngrams   - Character vector containing all n-grams. Each 
    ###	      	       n-gram must have the same number of words in it. 
    ###	    delta    - Discount parameter for Kneser-Ney probability.
    ###		       Common values are between 0 and 1. A value larger
    ###		       than n effectively means probabilities for n-grams 
    ###		       that occur n times or fewer are set to 0. Default
    ###		       value of 0.75.
    ###     prevcall - The Kneser-Ney formula is recursive. The 
    ###	    	       probabilities for N-grams depend on those for the
    ###		       corresponding (N-1)-grams. The calculations can be
    ###		       sped up by inputting the call of this function for
    ###		       those (N-1)-grams, which should be input as this
    ###		       parameter. If it is missing, it will perform all
    ###		       recursive calls. 
    ### The function also requires the variables unigrams, bigrams,
    ### and trigrams (the count of occurences of each of these n-grams)
    ### to be previously set.

    n <- length(strsplit(names(ngrams[1]), " ")[[1]])
    if (n == 1) {
       # Special case for unigram probabilities. Calculates the number of
       # bigrams that end with each unigram, divided by the number of bigrams.
       bigrams_ending_with <- 
       	  table(get_last_tokens(names(bigrams), 1))[names(unigrams)]
       names(bigrams_ending_with) <- names(unigrams)
       bigrams_ending_with[is.na(bigrams_ending_with)] <- 0
       return(bigrams_ending_with/length(bigrams))
    }	  
    if(missing(delta)) {
        delta <- 0.75
    }
    nminus1grams <- list(unigrams, bigrams)[[n-1]] 
    # If modifying for use for higher order N-grams, need to make sure
    # the above list has all N-grams up through N-1.

    #N-grams with first word removed
    end_grams <- nminus1grams[get_last_tokens(names(ngrams), n-1)] 
    #N-grams with last word removed
    root_grams <- get_nminus1_gram(names(ngrams))

    numerator <- ngrams - delta
    numerator[numerator < 0] <- 0
    denominator <- nminus1grams[root_grams]
    names(denominator) <- names(numerator)
    denominator[is.na(denominator)] <- Inf

    count_nminus1gram <- table(root_grams)[root_grams]	
    names(count_nminus1gram) <- names(numerator)

    if(missing(prevcall)) {
       recursive_call <- kneserNey(end_grams, delta)[names(end_grams)]
    } else {
       recursive_call <- prevcall[names(end_grams)]
    }
    names(recursive_call) <- names(ngrams)
    recursive_call[is.na(recursive_call)] <- 0

    if (n == 2) {
       # Formula differs slightly for bigrams and higher order N-grams
       return(numerator/denominator + delta * count_nminus1gram / denominator* recursive_call)
    }
    return(numerator/denominator + delta * count_nminus1gram / length(ngrams) * recursive_call)
}

# Calculate probabilities for all N-grams
unigramProbs <- kneserNey(unigrams, 0.75)
unigrams_top3 <- sort(unigramProbs, decreasing=T)[1:3]
# Note the calls below include the prevcall parameter as input to 
# speed up calculations. 
bigramProbs <- kneserNey(bigrams, 0.75, unigramProbs)
trigramProbs <- kneserNey(trigrams, 0.75, bigramProbs)



## The following are some space-saving features that can be employed

# Cut zero-probability n-grams
unigramProbs <- unigramProbs[unigramProbs > 0]
bigramProbs <- bigramProbs[bigramProbs > 0]
trigramProbs <- trigramProbs[unigramProbs > 0]
unigrams_top3 <- sort(unigramProbs, decreasing = T)[1:3]

# Puts probabilities into log space then stores them as integers. Rank
# is preserved, so they do not need to be converted back. 
bigramProbs <- 100*round(log(bigramProbs, 10),2)
trigramProbs <- 100*round(log(trigramProbs, 10),2)

# The following code finds the top 3 most probable N-grams corresponding
# to any given (N-1)-gram root. For example, "about the", "about to",
# and "about a" might be (N+1)-grams corresponding to the unigram "about".
# Since the final model I deployed gave back the top 3 most probable
# next words, anything after that is not necessary and could be cut. 
bigram_roots <- unique(names(bigram_minus1))
bigram_table <- table(bigram_minus1)
keep_bigrams <- names(bigram_table[bigram_table <4])
keep_bigrams <- names(bigramProbs[as.logical(match(bigram_minus1, keep_bigrams, nomatch=F))])
for (bigram in names(bigram_table[bigram_table > 3])) {
   root_table <- sort(bigramProbs[bigram_minus1 == bigram], decreasing=T)
   keep_bigrams <- c(keep_bigrams, names(root_table[1:3])) 
}
bigramProbs <- bigramProbs[keep_bigrams]

trigram_roots <- unique(names(trigram_minus1))
trigram_table <- table(trigram_minus1)
keep_trigrams <- names(trigram_table[trigram_table <4])
keep_trigrams <- names(trigramProbs[as.logical(match(trigram_minus1, keep_trigrams, nomatch=F))])
for (trigram in names(trigram_table[trigram_table > 3])) {
   root_table <- sort(trigramProbs[trigram_minus1 == trigram], decreasing=T)
   keep_trigrams <- c(keep_trigrams, names(root_table[1:3])) 
}
trigramProbs <- trigramProbs[keep_trigrams]
