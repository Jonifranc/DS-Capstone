Data Science Specialization Capstone Project
========================================================
author: Luciano M. Lattes
date: June, 2016

Application Details
========================================================

The application created for this Capstone Projects is a simple **Shiny** app that predicts text based on user input.

Based on the Capstone Dataset, which contains text from blogs, news and Twitter, a custom *n-gram backoff model* was constructed for achieving the objective with the help of `tm` package.

The model
========================================================

An n-gram is a contiguous sequence of n items from a given sequence of text or speech.

The back-off algorithm estimates the conditional probability of a word given its history in the n-gram. It accomplishes this estimation by "backing-off" to models with smaller histories under certain conditions.

For this particular case, I decided to work with 1, 2, 3, 4 and 5-grams.

Data preparation (1/3)
========================================================

In order to go from the text files of the original corpus to the datasets with the n-grams, I obtained a significative sample of each source (blogs, news and Twitter) of roughly `5%`.

Then, each `5%` was merged together and a series of transformations was applied in order to obtain data useful for predictions, meaning that unnecessary characters as well as other constructions were removed as follows:

Data preparation (2/3)
========================================================


```r
fullCorpus <- iconv(fullCorpus, "latin1", "ASCII", sub = "")
fullCorpus <- gsub("(http[^ ]*)|(ftp[^ ]*)|(www\\.[^ ]*)", "", fullCorpus) # URLs
fullCorpus <- gsub("([A-Za-z][\\.]\\s*){1,}([A-Za-z][\\.])", "", fullCorpus) # Abbreviations
fullCorpus <- gsub("(?<=^| )[-.]*\\d+(?:\\.\\d+)?(?= |\\.?$)|\\d+(?:,\\d{3})+(\\.\\d+)*", "", fullCorpus, perl = TRUE) # Numbers
```

Use ASCII characters only and remove URLs, abbreviations and numbers.

Data preparation (3/3)
========================================================


```r
textSampleCorpus <- VCorpus(VectorSource(fullCorpus))
textSampleCorpus <- tm_map(textSampleCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
textSampleCorpus <- tm_map(textSampleCorpus, removeNumbers)
textSampleCorpus <- tm_map(textSampleCorpus, content_transformer(tolower))
textSampleCorpus <- tm_map(textSampleCorpus, stripWhitespace)
```

Apply `removePunctuation`, `removeNumbers`, `tolower` and `stripWhitespace` transformations.

n-gram dataset construction
========================================================

Using a `TermDocumentMatrix` as pivot, five datasets were created containing the n-grams and their frequencies.

Each dataset has three main parts, the `pre` containing the first `n - 1` words of a given n-gram, the `word` meaning the last word of the n-gram and `freq` as the number of occurrences of the n-gram in the corpus.

The backoff (1/2)
========================================================

The steps for determining the most probable next word given an input are the following:

1) Obtain the full sentence from user input

2) Get the last n - 1 (4 in the first case) words of the sentence

3) Check if those words appear in the `pre` of the first n-gram (5-gram in the first case) dataset

The backoff (2/2)
========================================================

4) Store the term `word` corresponding to the matches above and continue with the next n-grams reducing the sentence length until its length is 1

5) The terms `word` appearing in the 5-gram are more important than the following n-grams so they are returned first; terms are also ordered by `freq`

6) If there are not enough words matching the user sentence in the n-gram datasets, fallback to the list of most likely monograms

Visit https://github.com/llattes/DS-Capstone for additional details
