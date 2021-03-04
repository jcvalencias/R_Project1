# Mini-Project 1 - Sentiment Analysis

# Required packages
#install.packages("twitteR")
#install.packages("httr")
library(twitteR)
library(httr)

# Loading cleaning library
source('utils.R')

# Twitter Api keys
key <- "3uTFq5nQpZ1apbDr9PPqkxgHx"
secret <- "iC3gqkypVLimVJHD1ZU7uCnu2YHKm4CZqN9IUty3IlBoN8K5d2"
token <- "1314164306544676866-ETJWktr74yRVMi07hLZI06qLuB3fBZ"
tokensecret <- "VrLLAyBK9ZBzXBvaiINx7dRjWCM41TGNoGuHykk3cT0o6"

setup_twitter_oauth(key, secret, token, tokensecret)


## Loading Tweets 

# Checking time line from user
userTimeline("dsacademybr")

# Gathering tweets
topic <- "Big Data"
qtd_tweets <- 100
language <- "en"
tweetdata = searchTwitter(topic, n = qtd_tweets, lang = language)


head(tweetdata)


## Text mining

# install.packages("tm")
# install.packages("SnowballC")
library(SnowballC)
library(tm)
options(warn=-1)

# Data Wrangling
tweetlist <- sapply(tweetdata, function(x) x$getText())
tweetlist <- iconv(tweetlist, to = "utf-8", sub="")
tweetlist <- cleanTweets(tweetlist)
tweetcorpus <- Corpus(VectorSource(tweetlist))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower))
tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, stopwords()))

# Converting Corpus object to plain text
# tweetcorpus <- tm_map(tweetcorpus, PlainTextDocument)
termo_por_documento = as.matrix(TermDocumentMatrix(tweetcorpus), control = list(stopwords = c(stopwords("english"))))



## Wordcloud


# install.packages("RColorBrewer")
# install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)

# generating Wordcloud
pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

# Text to Matrix
tweettdm <- TermDocumentMatrix(tweetcorpus)
tweettdm

# Find frequent terms
findFreqTerms(tweettdm, lowfreq = 11)

# Find Associations
findAssocs(tweettdm, 'data science', 0.60)


tweet2tdm <- removeSparseTerms(tweettdm, sparse = 0.9)


tweet2tdmscale <- scale(tweet2tdm)

# Distance Matrix
tweetdist <- dist(tweet2tdmscale, method = "euclidean")

# Preparing Dendrogram
tweetfit <- hclust(tweetdist)

# Creating Dendrogram
plot(tweetfit)

# Verifying groups
cutree(tweetfit, k = 4)

# Visualizing word groups in Dendrogram
rect.hclust(tweetfit, k = 3, border = "red")


## Sentiment Analysis

# Function to evaluate the sentiment
#install.packages("stringr")
#install.packages("plyr")
library(stringr)
library(plyr)

sentimento.score = function(sentences, pos.words, neg.words)
{
  
  # Creating array scores
  scores = lapply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words )
  
  scores.df = data.frame(text = sentences, score = unlist(scores))
  return(scores.df)
}

# Positive and negative words
pos = readLines("Positive_Words.txt")
neg = readLines("Negative_Words.txt")

# Creating test data
teste = c("Big Data is the future", "awesome experience",
          "analytics could not be bad", "learn to use big data")

# Testing on data
testesentimento = sentimento.score(teste, pos, neg)
class(testesentimento)

# Verifying score
# 0 - neither positive nor negative score of sentiment
# 1 - the expression has a positive word 
# -1 - the expression has a negative word
testesentimento$score


## Generating Sentiment Analysis

# Tweets by Country
catweets = searchTwitter("ca", n = 500, lang = "en")
usatweets = searchTwitter("usa", n = 500, lang = "en")

# Getting text
catxt = sapply(catweets, function(x) x$getText())
usatxt = sapply(usatweets, function(x) x$getText())

# Vector that will contain both countries tweets
paisTweet = c(length(catxt), length(usatxt))

# joining texts
paises = c(catxt, usatxt)

# Applying sentiment score function
scores = sentimento.score(paises, pos, neg)

# Computing Score by Country
scores$paises = factor(rep(c("ca", "usa"), paisTweet))
scores$pos = as.numeric(scores$score >= 1)
scores$neg = as.numeric(scores$score <= -1)

# Computing Total
numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

# Global Score
global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)
boxplot(score ~ paises, data = scores)

# Generating a Histogram with lattice package
# install.packages("lattice")
library("lattice")
histogram(data = scores, ~score|paises, main = "Sentiment Analysis", xlab = "", sub = "Score")



## Using Naive Bayes classifier
# https://cran.r-project.org/src/contrib/Archive/Rstem/
# https://cran.r-project.org/src/contrib/Archive/sentiment/

install.packages("Rstem_0.4-1.tar.gz", sep = "", repos = NULL, type = "source")
install.packages("sentiment_0.2.tar.gz",sep = "", repos = NULL, type = "source")
install.packages("ggplot2")
library(Rstem)
library(sentiment)
library(ggplot2)

# Gathering tweets
tweetpt = searchTwitter("bigdata", n = 1500, lang = "pt")

# getting texts
tweetpt = sapply(tweetpt, function(x) x$getText())

# String cleaning
tweetpt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetpt)
# Removing @
tweetpt = gsub("@\\w+", "", tweetpt)
# Removing punctuation
tweetpt = gsub("[[:punct:]]", "", tweetpt)
# Removing digits
tweetpt = gsub("[[:digit:]]", "", tweetpt)
# Removing html links
tweetpt = gsub("http\\w+", "", tweetpt)
# Removing unnecessary spaces
tweetpt = gsub("[ \t]{2,}", "", tweetpt)
tweetpt = gsub("^\\s+|\\s+$", "", tweetpt)

# Function to lower strings
try.error = function(x)
{
  # Creating missing value
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# Lower case
tweetpt = sapply(tweetpt, try.error)

# Removing NAs
tweetpt = tweetpt[!is.na(tweetpt)]
names(tweetpt) = NULL

# Classifying emotion
class_emo = classify_emotion(tweetpt, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]

# Replacing NA's with "Neutro"
emotion[is.na(emotion)] = "Neutro"

# Classifying polarity
class_pol = classify_polarity(tweetpt, algorithm = "bayes")
polarity = class_pol[,4]

# Generating dataframe of results
sent_df = data.frame(text = tweetpt, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)

# Sorting Dataframe
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels = names(sort(table(emotion), 
                                                                decreasing=TRUE))))


# Emotions founded
ggplot(sent_df, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Categories", y = "Number of Tweets") 

# Polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Categories of Sentiment", y = "Number of Tweets")











