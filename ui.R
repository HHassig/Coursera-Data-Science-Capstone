library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
    # Application title.
    titlePanel("What's Next?!"),
  
    sidebarLayout(
        sidebarPanel(
            textInput("obs", "Enter some text here:"),
            
            helpText("Here we will analyze your text to predict what word comes next."),
            
            submitButton("Predict!")
        ),
      
      mainPanel(
          h6("You entered:"),
          textOutput("initialText"),
          br(),
          h6("Removing special formatting:"),
          textOutput("Translated"),
          br(),
          br(),
          h3("What's next:"),
          div(textOutput("topGuess"), style = "color:red"),
          br(),
          h3("This was done by looking at the following:"),
          tableOutput("view")
    )
  )
))
