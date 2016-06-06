setwd("~/Projects/DS-Capstone")
library(R.utils)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

# Task 0: Understanding the Problem

# NLP common preprocessing steps

# Preprocessing text is called tokenization or text normalization. Things to consider include:
## Throw away unwanted stuff (e.g., HTML tags – but sometimes they are valuable, UUencoding, etc.)

## Word boundaries: white space and punctuations – but words like Ph.D., isn’t, e-mail, C|net
## or $19.99 are problematic; people typically use manually created regular expression rules.

## Stemming (Lemmatization): This is optional. English words like ‘look’ can be inflected with
## a morphological suffix to produce ‘looks, looking, looked’. They share the same stem ‘look’.
## Often it is beneficial to map all inflected forms into the stem. This is a complex process,
## since there can be many exceptional cases (e.g., department vs. depart, be vs. were).
## The most commonly used stemmer is the Porter Stemmer.

## Stopword removal: the most frequent words often do not carry much meaning.

## Capitalization, case folding: often it is convenient to lower case every character.
## Counterexamples include ‘US’ vs. ‘us’. Use with care.

# Now you have clean text, there are two concepts:
## Word token: occurrences of a word.
## Word type: unique word as a dictionary entry.

# A vocabulary lists the word types. A typical vocabulary has 10,000 or more words (types).
## A corpus is a large collection of text, e.g., several years’ newspapers. A vocabulary
## can be created from a corpus. Often people apply a frequency cutoff to exclude word
## types with small counts.

blogsText <- file("corpus/en_US/en_US.blogs.txt", "rb")
blogsLines <- countLines(blogsText)
blogsLinesSample <- floor(blogsLines * 0.01)
close(blogsText)

newsText <- file("corpus/en_US/en_US.news.txt", "rb")
newsLines <- countLines(newsText)
newsLinesSample <- floor(newsLines * 0.01)
close(newsText)

twitterText <- file("corpus/en_US/en_US.twitter.txt", "rb")
twitterLines <- countLines(twitterText)
twitterLinesSample <- floor(twitterLines * 0.01)
close(twitterText)

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

library(tm)
library(SnowballC)

blogsTextSampleCorpus <- VCorpus(VectorSource(blogsTextSample))
inspect(blogsTextSampleCorpus[2])
# <<VCorpus>>
# Metadata:  corpus specific: 0, document level (indexed): 0
# Content:  documents: 1

# [[1]]
# <<PlainTextDocument>>
# Metadata:  7
# Content:  chars: 22
blogsTextSample[2]
# [1] "We love you Mr. Brown."
nchar(blogsTextSample[2])
# [1] 22
writeLines(as.character(blogsTextSampleCorpus[[2]]))
# We love you Mr. Brown.

blogsTextSampleCorpus <- tm_map(blogsTextSampleCorpus, content_transformer(tolower))

blogsTextSampleCorpus <- tm_map(blogsTextSampleCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
#blogsTextSampleCorpus <- tm_map(blogsTextSampleCorpus, stemDocument)
blogsTextSampleCorpusNSW <- tm_map(blogsTextSampleCorpus, removeWords, stopwords("english"))
blogsTextSampleCorpus <- tm_map(blogsTextSampleCorpus, stripWhitespace)
blogsTextSampleCorpusNSW <- tm_map(blogsTextSampleCorpusNSW, stripWhitespace)


## Delete punctuation symbols, evaluate better transformations?

documentTermMatrix <- DocumentTermMatrix(blogsTextSampleCorpus)
documentTermMatrixNSW <- DocumentTermMatrix(blogsTextSampleCorpusNSW)
inspect(documentTermMatrix[1:10, 5:15])
findFreqTerms(documentTermMatrix, 3)
findAssocs(documentTermMatrix, "america", 0.8)

TDM <- TermDocumentMatrix(blogsTextSampleCorpus)

m <- as.matrix(documentTermMatrix)
v <- sort(rowSums(m), decreasing=TRUE)

# writeCorpus(blogsTextSampleCorpus, path = "corpus-write-example/")
# (blogsTestSampleCleanCorpus <- VCorpus(DirSource("corpus-write-example/", encoding = "UTF-8")))
dtmInspect <- inspect(documentTermMatrix)
freqMat <- data.frame(ST = colnames(dtmInspect), Freq = colSums(dtmInspect))
row.names(freqMat) <- NULL

dtmInspectNSW <- inspect(documentTermMatrixNSW)
freqMatNSW <- data.frame(ST = colnames(dtmInspectNSW), Freq = colSums(dtmInspectNSW))
row.names(freqMatNSW) <- NULL

col <- brewer.pal(6, "Accent")
wordcloud(blogsTextSampleCorpusNSW, min.freq = 9, scale = c(5, 1), rot.per = 0.35,
          random.color = T, max.word = 100, random.order = FALSE, colors = col)

col <- brewer.pal(4, "Accent")
wordcloud(blogsTextSampleCorpus, min.freq = 9, scale = c(5, 1), rot.per = 0.35,
          random.color = T, max.word = 100, random.order = FALSE, colors = col)

histogram <- ggplot(freqMat, aes(Freq)) + geom_histogram(aes(y=0.5*..density..), binwidth = 0.5) + geom_density(col = 2)
histogram

(enUSFullCorpus <- VCorpus(DirSource("corpus/en_US/", encoding = "UTF-8")))
