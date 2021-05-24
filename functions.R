

create_ngram_freqDF <- function(N, corpus, split=FALSE){ 
  "Split the corpus into Ngrams and create a df of their frequency- 
  If split, one column per word will be created"
  TOKENIZER_NGRAN <-function(x) unlist(lapply(ngrams(words(x), N), 
                                              paste, collapse = " "), use.names = FALSE)
  ngramTDM <- TermDocumentMatrix(corpus, control = list(tokenize = TOKENIZER_NGRAN)) # Get the n-gram
  matrix_gram <- slam::row_sums(ngramTDM)
  matrix_gram <- matrix_gram[order(-matrix_gram)]
  df_freq <- data.frame(word = names(matrix_gram),
                        freq=matrix_gram, stringsAsFactors=FALSE) # Fequency n-gram
  rownames(df_freq) <- NULL
  print(nrow(df_freq))
  if (split){
    df_freq$pred <- "";
    df_freq$pred <- lapply(df_freq$word, function(x) unlist(strsplit(as.character(x), split=' '))[[N]]);
    print("Splitting ngrams...")
    df_freq$word <- lapply(df_freq$word, function(x) paste(unlist(strsplit(as.character(x), split=' '))[-N], collapse=" "));
    # for (i in 1:nrow(df_freq)) {
    #   wordList <- unlist(strsplit(as.character(df_freq[i,"word"])," "));
    #   key <- paste(wordList[1:(N-1)], collapse=" ");
    #   df_freq[i, 1] <- key;
    #   df_freq[i, 3] <- wordList[N];
    # }
  }
  return(df_freq)
}
good_tunning_smoothing <-function(df_freq){
  "Calculate the probability for each event (event: frequency of occurrence being c).
  Returns a dictionary"
  freqfreq <- plyr::count(df_freq$freq)
  names(freqfreq) <- c("c","Nc")
  freqfreq <- rbind(c(0,freqfreq[1,2]),freqfreq)
  freqfreq$P = 0
  N <- sum(df_freq$freq)
  renorm = 0
  for (i in 1:nrow(freqfreq)){
    if(i == nrow(freqfreq)){
      freqfreq[i,3] = freqfreq[i,1] / N; 
    }else{
      freqfreq[i,3] = (freqfreq[i,1] + 1) * (freqfreq[i+1,2]/(N*freqfreq[i,2]));
    }
    renorm = renorm + (freqfreq[i,3]*freqfreq[i,2]);
  }
  freqfreq$P = freqfreq$P * renorm;
  
  d <- numvecdict()
  for (i in 1:nrow(freqfreq)) {
    d[[freqfreq[i,1]]] <- freqfreq[i, 3]
  }
  return(d)
  
}
dict_word_freq <- function(df_frequency) {
  "Convert the df of N-gram frequency to a dictionary word:frequency, being word the NGRAM without the predictor"
  df <- plyr::count(df_frequency$word)
  d <- dict();
  for (i in 1:nrow(df)) {
    key <- as.character(df[i,1]);
    d[[key]] <- df[i,2]
  }
  rm(df);
  return(d)
}
create_df_probabilities <- function(targetword, df_frequency, dict_prob){
  df <- df_frequency %>% filter(word == targetword) %>% select(pred, freq)
  df$prob <- sapply(df$freq, function(i) dict_prob[[i]])
  return(df)
}
create_full_ngram <- function(df_frequency, dict_prob, list){
  ngram <- dict();
  unique_words <- unique(df_frequency$word)
  print(length(unique_words))
  if (list){
    print("Creating list of dfs")
    x<-lapply(unique_words, function(x) create_df_probabilities(x, df_frequency, dict_prob));
    names(x) <- unique_words
    return(x)
  }
  else{
    for (w in unique_words) {
      df_word <- create_df_probabilities(w, df_frequency, dict_prob)
      ngram[[w]] <- df_word
    }
    return(ngram)
  }
  
  
}
