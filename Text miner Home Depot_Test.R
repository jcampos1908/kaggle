# Ghetto TEXT MINER

####################
### START OF FUNCTIONS
####################
#' cleans data to point as basis for all further datasets
baseDataCleaning <- function(corpus_ST){
  ##### Pre-processing ####
  #create the toSpace content transformer
  toSpace <- content_transformer(function(x, pattern){return (gsub(pattern," ", x))})
  corpus_ST<- tm_map(corpus_ST, toSpace, "-")
  corpus_ST<- tm_map(corpus_ST, toSpace, ":")
  
  #toX <- content_transformer(function(y, pattern){return (gsub(pattern,"x", y))})
  #corpus_ST<- tm_map(corpus_ST, toX, "*")
  
  
  ##### Transformation #####
  # Elminate Extra Whitespace #
  myCorpus <- tm_map(corpus_ST, stripWhitespace)
  # Convert to Lower Case #
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  for(j in seq(myCorpus))   
  {   
    myCorpus[[j]]$content <- gsub("@", " ", myCorpus[[j]]$content)   
  } 
  # remove numbers
  #myCorpus <- tm_map(myCorpus, removeNumbers)
  # remove stopwords
  remove <- stopwords("english")
  myStopwords <- remove[-143]
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  # Elminate Extra Whitespace #
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  myCorpus
}


#' cleaned corpus to documenttermmatrix
CleanedCorpusToDTM <- function(corpus){
  #Final Preprocess Step
  myCorpus <- tm_map(corpus, PlainTextDocument)  
  
  ######### Data Matrix #######
  dtm <- DocumentTermMatrix(myCorpus) 
  dtm_ST<-(removeSparseTerms(dtm, 0.999))
  print(dim(dtm_ST))
  print(dim(dtm))
  print(class(dtm_ST))
  dtm_ST
}

####################
### END OF FUNCTIONS
####################
library(tm)
library(dplyr)
options(header=F, stringsAsFactors=F)

# Set working directory 
#setwd("C:/Users/campje01/Desktop/Home Depot/Data ") 
setwd("C:/Users/campje01/Desktop/Home Depot/Data ") 

# Load the data
mydataT <- read.csv("test.csv", header=T)
corpus_STT <- Corpus(VectorSource(mydataT$search_term))
myCorpusT <- baseDataCleaning(corpus_STT)
dtm_STT <- CleanedCorpusToDTM(myCorpusT)

mydata_descT <- read.csv("product_descriptions.csv", header=T)
merged_dataT <- merge(mydataT,mydata_descT,by="product_uid",all.x = TRUE)

save.image("baseDataPrepT.RData")

corpus_PTT <- Corpus(VectorSource(merged_dataT$product_title))
myCorpus_PTT <- baseDataCleaning(corpus_PTT)
dtm_PTT <- CleanedCorpusToDTM(myCorpus_PTT)


corpus_DST <- Corpus(VectorSource(merged_dataT$product_description))
myCorpus_DST <- baseDataCleaning(corpus_DST)
dtm_DST <- CleanedCorpusToDTM(myCorpus_DST)

final_data_question_markT <- cbind(dtm_DST, dtm_PTT, dtm_STT)
final_data_question_markT2 <- cbind(dtm_PTT, dtm_STT)


save.image("step3.RData")
