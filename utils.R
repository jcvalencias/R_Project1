# Cleaning tweets function
cleanTweets <- function(tweet){
  # Remove http links
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("http\\w+", "", tweet)
  # Remove retweets
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Remove #Hashtag
  tweet = gsub("#\\w+", " ", tweet)
  # Remove user names @people
  tweet = gsub("@\\w+", " ", tweet)
  # Remove punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Remove numbers
  tweet = gsub("[[:digit:]]", " ", tweet)
  # Remove white spaces
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # Converting encoding of words and lower case
  tweet <- stringi::stri_trans_general(tweet, "latin-ascii")
  tweet <- tryTolower(tweet)
  tweet <- iconv(tweet, from = "UTF-8", to = "ASCII")
}

# Function to clean Corpus
cleanCorpus <- function(myCorpus){
  library(tm)
  myCorpus <- tm_map(myCorpus, tolower)
  # Remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # Remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
}

# lower case
tryTolower = function(x)
{
  y = NA

  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


