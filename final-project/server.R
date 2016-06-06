library(shiny)
library(shinyjs)
library(tm)

load_data <- function () {
  # Most Likely unigrams
  most_likely_unigrams <<- readRDS(file = "data/top_unigrams.rds")
  # 2-grams
  freq_2 <<- readRDS(file = "data/freq2.rds")
  # 3-grams
  freq_3 <<- readRDS(file = "data/freq3.rds")
  # 4-grams
  freq_4 <<- readRDS(file = "data/freq4.rds")
  # 5-grams
  freq_5 <<- readRDS(file = "data/freq5.rds")

  hide("loading_page")
  show("main_content")
}

predict_next_word <- function (raw) {
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

  predicted <- names(table[1:10])

  if (length(na.omit(predicted)) == 10) {
    predicted
  } else {
    # Lookup most likely unigrams to complete prediction
    unigram_selection <- 10 - length(na.omit(predicted))
    likely <- most_likely_unigrams[sample(nrow(most_likely_unigrams), unigram_selection), ]
    predicted <- c(predicted, likely[with(likely, order(-freq)), ]$word)
    na.omit(predicted)[1:10]
  }
}

check_content <- function (raw) {
  sentence <- tolower(raw) %>%
    removePunctuation %>%
    removeNumbers %>%
    stripWhitespace %>%
    str_trim
  if (nchar(sentence) == 0) {
    TRUE
  } else {
    FALSE
  }
}

shinyServer(
  function(input, output, session) {
    load_data()

    output$suggestion <- renderText({
      if (check_content(input$typing)) {
        "Don't know where to start? Try with one of the following words: "
      } else {
        "Suggested words: "
      }
    })

    output$text1 <- renderText({
      predict_next_word(input$typing)
    })
  }
)
