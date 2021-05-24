
## process_input
library(tm)
library(NLP)

processInput <- function(text){
  text <- removeNumbers(text);
  text <- removePunctuation(text);
  text <- stripWhitespace(text);
  text <- tolower(text);
  return(unlist(strsplit(text," ")));
}

return_prediction <- function(input){
  tokens <- processInput(input)
  len = length(tokens)
  if(len >= 3){
    pred <- gram4[[paste(tokens[(len-2) :len], collapse=" ")]]
    if (is.null(pred)){
      pred <- gram3[[paste(tokens[(len-1) :len], collapse=" ")]]
      if (is.null(pred)){
        pred <- gram2[[ tokens[len]]]
      } else{return(pred)}
    } else{return(pred) }
  } else if (len == 2){
    pred <- gram3[[paste(tokens[(len-1) :len], collapse=" ")]]
    if (is.null(pred)){
      pred <- gram2[[ tokens[len]]]
    } else{return(pred)}
  }else if (len == 1){
    pred <- gram2[[ tokens[len]]]
    if (is.null(pred)){
      return(NULL)
    } else {return(pred)}
  }
  
}

top <- function(pPred, n){
  if(nrow(pPred) > n){
    pPred <- dplyr::sample_n(pPred, size = n, weight = pPred$prob)
    pPred <- pPred[order(-pPred$prob),]
  }
  return(pPred[,1])
}


