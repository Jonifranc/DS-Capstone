library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),

  div(
    id = "loading_page",
    style = "margin: 20px;",

    h1("Loading..."),
    h3("Please wait for the prediction application to finish loading its resources"),
    div(
      img(src = "loader.gif", style = "display: block; margin-left: auto; margin-right: auto;"),
      style = "padding: 20px;"
    )
  ),

  hidden(
    div(
      id = "main_content",
      style = "margin: 20px;",

      verticalLayout(
        wellPanel(
          h1("Text prediction algorithm"),
          h5("For the Data Science Specialization, Johns Hopkins University")
        ),

        wellPanel(
          helpText("Add text in english to predict the next word."),

          textInput("typing",
                    label = "Input text: ")
        ),

        wellPanel(
          div(
            style = "font-weight: bold;",
            textOutput("suggestion")
          ),
          div(
            style = "margin: 10px; padding: 10px; font-weight: bold; background-color: #e8e8e8; border: 2px solid #ff876d;",
            textOutput("text1")
          ),
          br(),
          helpText("The leftmost words in the list above are likely to be the most suitable for the input text.")
        ),

        div(
          style = "text-align: right;",
          "See source code at ",
          a(href = "https://github.com/llattes/DS-Capstone", "https://github.com/llattes/DS-Capstone")
        )
      )
    )
  )
))
