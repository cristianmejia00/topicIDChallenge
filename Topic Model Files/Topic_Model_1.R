# 20170523 Topic Models
# 20170525 Adapted for the topic identification Challenge
# Topic Model 1/3
# Model and data preparation
# We prepare the text that is going to be used in the model. We also choose key parameters.

#########################################
# Load libraries

library(tm)
library(lda)
library(LDAvis)
library(servr)
library(proxy)
library(SnowballC)
library(data.table)
library(Rmpfr)

#########################################
# Set parameters

# Open the file
myData <- challenge

# Which column(s) to merge and analyse
columns <- c("TI", "AB")

# Choose folder to save reports
setwd(choose.dir())

# Name of the project
pName <- "test"

# Words to neglect from analysis
myStopWords <- c("robot", "analysis", "paper")

# Topic Model Parameters
G <- 500 #iterations
alpha <- 0.02
eta <- 0.02

# More options
useStemming <- TRUE
fullReports <- TRUE

#########################################
# Functions
# Text cleaner
tidytext <- function(myData, columns, useStemming, myStopWords) {
  documents <- apply(myData[columns], 1, paste, collapse = ". ")
  text <- Corpus(VectorSource(documents))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, removeWords, myStopWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- unlist(sapply(1:length(text), function(x){return(text[[x]]$content)}))
  return(text)
}

#########################################
# Execute

# Get the clean text to feed the topic model
myText <- tidytext(myData, columns, useStemming, myStopWords)

# Obtain the documents that are not blank
blankLines <- unname(sapply(myText, nchar))
myDataCorrect <- myData[blankLines>2,]

###################################################
# Preparation
# From http://cpsievert.github.io/LDAvis/reviews/reviews.html

# tokenize on space and output as a list:
doc.list <- strsplit(data2$clean_text, "[[:space:]]+")
doc.list <- lapply(doc.list, function(x) x[which(nchar(x)>1)]) #Ensure we remove white spaces, very important for the visualizacion step
# https://github.com/cpsievert/LDAvis/issues/23

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)
vocab[1:100] #top 100 terms

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

######################################################
#model fitting

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus
