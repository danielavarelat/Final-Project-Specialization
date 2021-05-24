library(readr)
library(MODIS)
library(dict)
library(tidyverse)
library(dplyr)
library(tm)
rm(list = ls(all.names = TRUE))


setwd(paste0(getwd(),"/data"))

conTwitter <- file(paste0(FOLDER,"/", "en_US/en_US.twitter.txt"), "r") 
conNews <- file(paste0(FOLDER,"/", "en_US/en_US.news.txt"), "r") 
conBlogs <- file(paste0(FOLDER,"/", "en_US/en_US.blogs.txt"), "r") 
Tfull <- readLines(conTwitter)
Bfull <- readLines(conBlogs)
Nfull <- readLines(conNews)
close(conTwitter)
close(conNews)
close(conBlogs)
rm(conTwitter, conNews, conBlogs)

### Sampling
set.seed(100)
Tsample <- Tfull[rbinom(length(Tfull) * 0.1, length(Tfull), 0.5)]
Bsample <- Bfull[rbinom(length(Bfull) * 0.1, length(Bfull), 0.5)]
Nsample <- Nfull[rbinom(length(Nfull) * 0.1, length(Nfull), 0.5)]
AllSample <- paste(Tsample, Bsample, Nsample)
rm(Tsample, Bsample, Nsample)
rm(Tfull, Bfull, Nfull)

## process - keep stopwords
corpus <- VCorpus(VectorSource(AllSample))
clean_corp <- tm_map(corpus, function(x) gsub("[^a-zA-Z0-9 ]","",x))
clean_corp <- tm_map(clean_corp, tolower)
clean_corp <- tm_map(clean_corp, removeNumbers)
clean_corp <- tm_map(clean_corp, stripWhitespace)
profanity_url <- "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
list_words <- read.table(url(profanity_url))$V1
prof_vector <- VectorSource(list_words)
clean_corp <- tm_map(clean_corp, removeWords, prof_vector) 
plain_corpus <- tm_map(clean_corp, PlainTextDocument)
rm(corpus, list_words, prof_vector, clean_corp, AllSample)


saveRDS(TRIGRAM, "plain_corpus.rds");


## BIGRAMS
setwd("/Users/dvarelat/Documents/curso")
source("functions.R")
df_freq2 <- create_ngram_freqDF(2, plain_corpus, split = TRUE)
df_freq_filtered2 <- df_freq2[df_freq2$freq>2,];
dict_freqDist2 <- good_tunning_smoothing(df_freq2)
Rprof(tf <- "rprof_bigram.log", memory.profiling=TRUE)
TWOGRAM<- create_full_ngram(df_freq_filtered2, dict_freqDist2)
Rprof(NULL)
summaryRprof(tf)
saveRDS(TWOGRAM, "2gram_Dictionary.rds");

reove
## TRIGRAM
setwd("/Users/dvarelat/Documents/curso")
source("functions.R")
df_freq3 <- create_ngram_freqDF(3, plain_corpus, split = TRUE)
df_freq_filtered3 <- df_freq3[df_freq3$freq>2,];
dict_freqDist3 <- good_tunning_smoothing(df_freq3)
Rprof(tf <- "rprof_trigram.log", memory.profiling=TRUE)
TRIGRAM<- create_full_ngram(df_freq_filtered3, dict_freqDist3)
Rprof(NULL)
summaryRprof(tf)
saveRDS(TRIGRAM, "3gram_Dictionary.rds");

### FOUR
setwd("/Users/dvarelat/Documents/curso")
source("functions.R")
df_freq4 <- create_ngram_freqDF(4, plain_corpus, split = FALSE)
df_freq_filtered4 <- df_freq4[df_freq4$freq>2,];
dict_freqDist4 <- good_tunning_smoothing(df_freq4)
Rprof(tf <- "rprof_fourgram.log", memory.profiling=TRUE)
FOURGRAM<- create_full_ngram(df_freq_filtered4, dict_freqDist4, list=TRUE)
Rprof(NULL)
summaryRprof(tf)  
saveRDS(FOURGRAM, "4gram_list.rds");
remove(df_freq)


### try in parallel
library(parallel)
library(doParallel)
numCores <- detectCores()
registerDoSEQ()
cl <- makeCluster(4)

unique_words <- unique(df_freq_filtered4$word)
registerDoParallel(cl)
ptm_mc_OUT <- proc.time()  
xP<-mclapply(unique_words, 
            FUN = create_df_probabilities, df_freq_filtered4, dict_freqDist4, 
            mc.cores=6)
time_mclapply_OUT <- proc.time() - ptm_mc_OUT
ptm_mc_OUT <- proc.time()  

names(xP) <- unique_words
saveRDS(xP, "4gram_list.rds");

xP[["when it comes"]]



#save.image(file='myEnvironment.RData')
#load("/Users/dvarelat/Documents/curso/final/myEnvironment.RData")






