library(shiny)
library(data.table)
library(NLP)
library(tm)

#Read in Dataset
Dataset <- fread("data.txt")
setkeyv(Dataset, c('w1', 'w2', 'w3', 'w4', 'freq'))

translatedInput <- function(Text){
    modifyInput <- tolower(Text)
    modifyInput <- stripWhitespace(modifyInput)
    modifyInput <- gsub("[^\\p{L}\\s]+", "", modifyInput, ignore.case=F, perl=T)
    return(modifyInput)
}

spliceTranslateInput <- function(Text){
    modifyInput <- tolower(Text)
    modifyInput <- stripWhitespace(modifyInput)
    modifyInput <- gsub("[^\\p{L}\\s]+", "", modifyInput, ignore.case=F, perl=T)
    spliceTranslateInput <- unlist(strsplit(modifyInput, " "))
    return(spliceTranslateInput)
}

wordCounter1 <- function(textInputA){
    NgramsTable <<- Dataset[list("<s>", textInputA[1])]
    NgramsTable <<- NgramsTable[NgramsTable$w3!="<s>", ]
    NgramsTable <<- NgramsTable[order(NgramsTable$freq, decreasing=TRUE), ]
    
    #List Alternatives
    secondGuess <<- as.data.frame(NgramsTable)
    secondGuess <<- secondGuess[1:5, c("w3", "freq")]
    secondGuess <<- secondGuess[!is.na(secondGuess$freq), ]
    secondGuess <<- secondGuess[!duplicated(secondGuess), ]
    if(nrow(secondGuess)==0){
        secondGuess <<- data.frame(Word=NA, Likelihood=NA)
    }else{
        secondGuess$freq <- round(secondGuess$freq/sum(secondGuess$freq)*100, 1)
        secondGuess <<- secondGuess
        colnames(secondGuess) <<- c("Word", "Likelihood")
        rownames(secondGuess) <<- NULL
    }
    
    guessOutput <- NgramsTable$w3[1]
    if(is.na(guessOutput)|is.null(guessOutput)){
        guessOutput <- "Hmmmmm, this is a difficult one. Please check your spelling."
    }
    
    return(guessOutput)
}

wordCounter2 <- function(textInputB){
    NgramsTable <<- Dataset[list("<s>", textInputB[1], textInputB[2])]
    NgramsTable <<- NgramsTable[NgramsTable$w4!="<s>", ]
    NgramsTable <<- NgramsTable[order(NgramsTable$freq, decreasing=TRUE), ]
    
    #List Alternatives
    secondGuess <<- as.data.frame(NgramsTable)
    secondGuess <<- secondGuess[1:5, c("w4", "freq")]
    secondGuess <<- secondGuess[!is.na(secondGuess$freq), ]
    secondGuess <<- secondGuess[!duplicated(secondGuess), ]
    if(nrow(secondGuess)==0){
        secondGuess <<- data.frame(Word=NA, Likelihood=NA)
    }else{
        secondGuess$freq <- round(secondGuess$freq/sum(secondGuess$freq)*100, 1)
        secondGuess <<- secondGuess
        colnames(secondGuess) <<- c("Word", "Likelihood")
        rownames(secondGuess) <<- NULL
    }
    
    guessOutput <- NgramsTable$w4[1]
    if(is.na(guessOutput)|is.null(guessOutput)){       
        guessOutput <- wordCounter1(textInputB[2])
    }
    
    return(guessOutput)
}

wordCounter3 <- function(textInputC){
    NgramsTable <<- Dataset[list("<s>", textInputC[1], textInputC[2], textInputC[3])]
    NgramsTable <<- NgramsTable[NgramsTable$w5!="<s>", ]
    NgramsTable <<- NgramsTable[order(NgramsTable$freq, decreasing=TRUE), ]
    
    #List Alternatives
    secondGuess <<- as.data.frame(NgramsTable)
    secondGuess <<- secondGuess[1:5, c("w5", "freq")]
    secondGuess <<- secondGuess[!is.na(secondGuess$freq), ]
    secondGuess <<- secondGuess[!duplicated(secondGuess), ]
    if(nrow(secondGuess)==0){
        secondGuess <<- data.frame(Word=NA, Likelihood=NA)
    }else{
        secondGuess$freq <- round(secondGuess$freq/sum(secondGuess$freq)*100, 1)
        secondGuess <<- secondGuess
        colnames(secondGuess) <<- c("Word", "Likelihood")
        rownames(secondGuess) <<- NULL
    }
    
    guessOutput <- NgramsTable$w5[1]
    if(is.na(guessOutput)|is.null(guessOutput)){
        shortInput <- c(textInputC[2], textInputC[3])
        guessOutput <- wordCounter2(shortInput)
        if(is.na(guessOutput)|is.null(guessOutput)){
            guessOutput <- wordCounter1(textInputC[3])
        }
    }
    
    return(guessOutput)
}