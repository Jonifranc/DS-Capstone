setwd("~/Projects/DS-Capstone")
library(R.utils)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(SnowballC)
library(NLP)
library(magrittr)
library(data.table)
library(slam)
library(stringr)

options(scipen=999)

blogsText <- file("corpus/en_US/en_US.blogs.txt", "rb")
blogsLines <- countLines(blogsText)
blogsLinesSample <- floor(blogsLines * 0.05)
close(blogsText)

newsText <- file("corpus/en_US/en_US.news.txt", "rb")
newsLines <- countLines(newsText)
newsLinesSample <- floor(newsLines * 0.05)
close(newsText)

twitterText <- file("corpus/en_US/en_US.twitter.txt", "rb")
twitterLines <- countLines(twitterText)
twitterLinesSample <- floor(twitterLines * 0.05)
close(twitterText)

### BLOGS ###
blogsText <- file("corpus/en_US/en_US.blogs.txt", "r")

readLines <- 0

blogsTextSample <- character()
while(readLines < blogsLinesSample) {
  line <- readLines(blogsText, n = 1)
  if (length(line) == 0) {
    break
  }
  blogsTextSample <- c(blogsTextSample, line)
  readLines <- readLines + 1
}

close(blogsText)

### NEWS ###
newsText <- file("corpus/en_US/en_US.news.txt", "r")

readLines <- 0

newsTextSample <- character()
while(readLines < newsLinesSample) {
  line <- readLines(newsText, n = 1)
  if (length(line) == 0) {
    break
  }
  newsTextSample <- c(newsTextSample, line)
  readLines <- readLines + 1
}

close(newsText)

### TWITTER ###
twitterText <- file("corpus/en_US/en_US.twitter.txt", "r")

readLines <- 0

twitterTextSample <- character()
while(readLines < twitterLinesSample) {
  line <- readLines(twitterText, n = 1)
  if (length(line) == 0) {
    break
  }
  twitterTextSample <- c(twitterTextSample, line)
  readLines <- readLines + 1
}

close(twitterText)

rm(line, readLines)

fullCorpus <- c(blogsTextSample, newsTextSample, twitterTextSample)
fullCorpus <- iconv(fullCorpus, "latin1", "ASCII", sub = "")

rm(blogsTextSample, newsTextSample, twitterTextSample)

fullCorpus <- gsub("(http[^ ]*)|(ftp[^ ]*)|(www\\.[^ ]*)", "", fullCorpus) # URLs
fullCorpus <- gsub("([A-Za-z][\\.]\\s*){1,}([A-Za-z][\\.])", "", fullCorpus) # Abbreviations
fullCorpus <- gsub("(?<=^| )[-.]*\\d+(?:\\.\\d+)?(?= |\\.?$)|\\d+(?:,\\d{3})+(\\.\\d+)*",
                   "", fullCorpus, perl = TRUE) # Numbers

textSampleCorpus <- VCorpus(VectorSource(fullCorpus))
textSampleCorpus <- tm_map(textSampleCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
textSampleCorpus <- tm_map(textSampleCorpus, removeNumbers)
textSampleCorpus <- tm_map(textSampleCorpus, content_transformer(tolower))
textSampleCorpus <- tm_map(textSampleCorpus, stripWhitespace)

UnigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
TrigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
QuadgramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
FivegramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)

unigrams <- TermDocumentMatrix(textSampleCorpus, control = list(tokenize = UnigramTokenizer, wordLengths = c(1, Inf)))
bigrams <- TermDocumentMatrix(textSampleCorpus, control = list(tokenize = BigramTokenizer))
trigrams <- TermDocumentMatrix(textSampleCorpus, control = list(tokenize = TrigramTokenizer))
quadgrams <- TermDocumentMatrix(textSampleCorpus, control = list(tokenize = QuadgramTokenizer))
fivegrams <- TermDocumentMatrix(textSampleCorpus, control = list(tokenize = FivegramTokenizer))

tdmToFreq <- function(tdm) {
  # Takes tm TermDocumentMatrix and processes into frequency data.table
  freq <- sort(row_sums(tdm, na.rm=TRUE), decreasing=TRUE)
  word <- names(freq)
  data.table(word=word, freq=freq)
}

freq_1 <- tdmToFreq(unigrams)
freq_2 <- tdmToFreq(bigrams)
freq_3 <- tdmToFreq(trigrams)
freq_4 <- tdmToFreq(quadgrams)
freq_5 <- tdmToFreq(fivegrams)

most_likely_unigrams <- freq_1[which(freq_1$freq >= 800), ]

processGram <- function(dt) {
  # Add to n-gram data.table pre (before word) and cur (word itself)
  dt[, c("pre", "cur"):=list(unlist(strsplit(word, "[ ]+?[a-z]+$")),
                             unlist(strsplit(word, "^([a-z]+[ ])+"))[2]),
     by=word]
}

processGram(freq_2)
processGram(freq_3)
processGram(freq_4)
processGram(freq_5)

saveRDS(most_likely_unigrams, file = "top_unigrams.rds")
saveRDS(freq_2, file = "freq2.rds")
saveRDS(freq_3, file = "freq3.rds")
saveRDS(freq_4, file = "freq4.rds")
saveRDS(freq_5, file = "freq5.rds")

# Most Likely unigrams
most_likely_unigrams <- readRDS(file = "top_unigrams.rds")
# 2-grams
freq_2 <- readRDS(file = "freq2.rds")
# 3-grams
freq_3 <- readRDS(file = "freq3.rds")
# 4-grams
freq_4 <- readRDS(file = "freq4.rds")
# 5-grams
freq_5 <- readRDS(file = "freq5.rds")

predict_next_word <- function(raw) {
  sentence <- tolower(raw) %>%
    removePunctuation %>%
    removeNumbers %>%
    stripWhitespace %>%
    str_trim %>%
    strsplit(split=" ") %>%
    unlist

  predicted <- character()

  for (i in min(length(sentence), 4):1) {
    gram <- paste(tail(sentence, i), collapse=" ")

    if (i == 4) {
      predicted <- c(predicted, freq_5[which(freq_5$pre == paste(gram))[1:10], ]$cur)
    } else if (i == 3) {
      predicted <- c(predicted, freq_4[which(freq_4$pre == paste(gram))[1:10], ]$cur)
    } else if (i == 2) {
      predicted <- c(predicted, freq_3[which(freq_3$pre == paste(gram))[1:10], ]$cur)
    } else {
      predicted <- c(predicted, freq_2[which(freq_2$pre == paste(gram))[1:10], ]$cur)
    }
  }

  table <- sort(table(na.omit(predicted)), decreasing = TRUE)

  predicted <- names(table[1:6])

  if (length(na.omit(predicted)) == 6) {
    predicted
  } else {
    # Lookup most likely unigrams to complete prediction
    unigram_selection <- 6 - length(na.omit(predicted))
    likely <- most_likely_unigrams[sample(nrow(most_likely_unigrams), unigram_selection), ]
    predicted <- c(predicted, likely[with(likely, order(-freq)), ]$word)
    na.omit(predicted)[1:6]
  }
}
