options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(data.table)
library(NLP)
library(tm)

#Organize the server side to display
shinyServer(function(input, output) {
    output$initialText <- renderText({
        originalInput <- input$obs
        return(originalInput)
    })
    
    #Summary
    output$Translated <- renderText({
        originalInput <- input$obs
        translatedInput <- translatedInput(originalInput)
        return(translatedInput)
    })
    
    #Summary, again !
    output$topGuess <- renderText({
        originalInput <- input$obs
        translatedInput <- translatedInput(originalInput)
        topGuess <- "The word that we believe will appear next in your sentence will show up here :)"
        translatedInputSplit <- spliceTranslateInput(originalInput)
        wordCounter <- length(translatedInputSplit)
        
        if(wordCounter==1){
            topGuess <- wordCounter1(translatedInputSplit)
        }
        if(wordCounter==2){
            topGuess <- wordCounter2(translatedInputSplit)
        }
        if(wordCounter==3){
            topGuess <- wordCounter3(translatedInputSplit)
        }
        if(wordCounter > 3){
            Words_to_Search <- c(translatedInputSplit[wordCounter - 2],
                                 translatedInputSplit[wordCounter - 1],
                                 translatedInputSplit[wordCounter])
            topGuess <- wordCounter3(Words_to_Search)
        }
        return(topGuess)
    })
    
    #Display the top observations
    output$view <- renderTable({
        originalInput <- input$obs
        translatedInputSplit <- spliceTranslateInput(originalInput)
        wordCounter <- length(translatedInputSplit)
        
        if(wordCounter==1){
            topGuess <- wordCounter1(translatedInputSplit)
        }
        if(wordCounter==2){
            topGuess <- wordCounter2(translatedInputSplit)
        }
        if(wordCounter==3){
            topGuess <- wordCounter3(translatedInputSplit)
        }
        if(wordCounter > 3){
            searchWords <- c(translatedInputSplit[wordCounter - 2],
                                 translatedInputSplit[wordCounter - 1],
                                 translatedInputSplit[wordCounter])
            topGuess <- wordCounter3(searchWords)
        }
      
        if(exists("secondGuess", where = -1)){
            secondGuess
        }else{
            XNgramsTable <- data.frame(Word=NA, Likelihood=NA)
        }
      
    })
})