#######  Sentimental analysis from Twitter, pre-processing, data cleansing, and visualisation  ##########
### Author: Barbara Salas ###

#Install Packages
install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("wordcloud")

# Load Required Packages
library(SnowballC)
library(tm)
library(twitteR)
library(syuzhet)
library(wordcloud)

# Authentification keys - replace with values provided by Twitter
consumer_key <- #here your token between single quotation marks
consumer_secret <- #here your token between single quotation marks
access_token <- #here your token between single quotation marks
access_secret <- #here your token between single quotation marks

#This function wraps the OAuth authentication handshake functions for a twitteR session
#use a local file to cache OAuth access credentials between R sessions? Answer 1: Yes
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#*********Extraction and pre-processing*********************
#Newstalk_tweets

# The following command extracts last 1000 tweets from each Twitter account
Newstalk_tweets = userTimeline("NewstalkZB", n=1000)

#to check tweets and its punctuation
head(Newstalk_tweets)

# Convert tweets into a data frame for easier pre-processing. It can checked clickling in tweets.df in Data window
tweets.df <- twListToDF(Newstalk_tweets)

#to view the table that contains tweets before processing
head(tweets.df)

# Use a 'find and replace' function to remove symbol and irrelevant signs from tweets
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("\n","",tweets.df2) 
tweets.df2 <- gsub(" \n","",tweets.df2)
tweets.df2 <- gsub("\n\nhttps:*","",tweets.df2) 
tweets.df2 <- gsub("\n\n","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
#this command bellow eliminates all punctuations
tweets.df2 <- gsub("[[:punct:]]","",tweets.df2)

#to check and look for another sign to remove.
tweets.df2

#to display tweets
head(tweets.df2)

#*********Sentiment analysis: identification of tweets for each emotional category*********************

# Convert data frame into a vector before performing sentiment analysis
word.df <- as.vector(tweets.df2)
head(word.df)

# Perform sentiment analysis to score tweets on an emotion
emotion.df <- get_nrc_sentiment(word.df)

# Combine tweets to sentiment scores
emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)
View(emotion.df2)

# Score tweets based on positive and negative sentiment
sent.value <- get_sentiment(word.df)

# Filter based on positive, negative and neutral tweets. This is to save score respect to a tweet.
positive.tweets <- word.df[sent.value > 0]
negative.tweets <- word.df[sent.value < 0]
neutral.tweets <- word.df[sent.value == 0]

# We can display each of the above using the head() command
head(positive.tweets)
head(negative.tweets)
head(neutral.tweets)

#********* Most positive and negative tweets ************

# Select the most positive sentiment (highest sent.value)
most.positive <- word.df[sent.value == max(sent.value)]

# Display tweet with most positive sentiment
most.positive

# Select the most negative sentiment (lowest sent.value)
most.negative <- word.df[sent.value <= min(sent.value)] 

# Display tweet with most negative sentiment
most.negative 

# We can also display a selection of neutral tweets
head(neutral.tweets)

# Count number of positive, negative and neutral tweets. This is to create a pie chart to see proportion of them.
pos <- length(positive.tweets)
neut <- length(neutral.tweets)
neg <- length(negative.tweets)

#to show the number of positive,neutral and negative tweets 
pos
neut
neg

#**************** Pie chart *****************

# First combine the values and assign to a vector
tweets_vector <- c(pos, neut, neg)

#to view this concatenation.
tweets_vector

# Now define the labels to be used in a pie chart 
labels <- c("Positive", "Neutral", "Negative")
pct <- round(tweets_vector/sum(tweets_vector)*100)

#add percents to labels
lbls <- paste(labels,pct)

#add % to labels
lbls <- paste(lbls, "%", sep = "")

#to get a 3D pie chart, the package 'plotrix have to be installed'
install.packages("plotrix")
library(plotrix)

pie3D(tweets_vector,labels=lbls,explode=0.1, main="Sentiment Analysis for Newstalk tweets")

#************* TDM ******************

# Create a corpus (collection of words) from the data frame of cleaned tweets
tweet_corpus <- Corpus(VectorSource(word.df))

# create term document matrix applying some restrictions

tdm <- TermDocumentMatrix(tweet_corpus,
                          control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                         stopwords = c("Newstalk", "listen", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix to calculate word frequencies
tdm.matrix <- as.matrix(tdm)

#To visualise the matrix for words frequency
tdm.matrix

#**************** Wordcloud ***********************

# to get word counts in decreasing order
word_freqs <- sort(rowSums(tdm.matrix), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm <- data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud with words that appear at least 50 times.
wordcloud(dm$word, dm$freq, min.freq = 50, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# To find associations/ correlations between words with most frequent words (min.freq=50), findAssocs function is used.
#Parameters are indicated Where 0 means never occurs and 1 is always  occurs. For 0.65:
findAssocs(tdm,"christchurch",0.65)
findAssocs(tdm,"attack",0.65)
findAssocs(tdm,"terror",0.65)

#for a correlation of 25%:
findAssocs(tdm,"christchurch",0.25)
findAssocs(tdm,"attack",0.25)
findAssocs(tdm,"terror",0.25)

#*********Extraction and pre-processing for nzherald_tweets*********************

# The following command extracts last 1000 tweets from each Twitter account
nzherald_tweets = userTimeline("nzherald", n=1000)

#to check tweets and its punctuation
head(nzherald_tweets)

# Convert tweets into a data frame for easier pre-processing. It can checked clickling in tweets.df in Data window
tweets.df <- twListToDF(nzherald_tweets)

#to view the table that contains tweets before processing
head(tweets.df)

# Use a 'find and replace' function to remove symbol and irrelevant signs from tweets
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("\n","",tweets.df2) 
tweets.df2 <- gsub(" \n","",tweets.df2)
tweets.df2 <- gsub("\n\nhttps:*","",tweets.df2) 
tweets.df2 <- gsub("\n\n","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
#this command bellow eliminates all punctuations
tweets.df2 <- gsub("[[:punct:]]","",tweets.df2)

#Remove this specific expression from nzherald_tweets:
tweets.df2 <- gsub("\U0001f3a7","",tweets.df2)

#to check and look for another sign to remove.
tweets.df2

#to display tweets
head(tweets.df2)

#*********Sentiment analysis: identification of tweets for each emotional category*********************

# Convert data frame into a vector before performing sentiment analysis
word.df <- as.vector(tweets.df2)
head(word.df)

# Perform sentiment analysis to score tweets on an emotion
emotion.df <- get_nrc_sentiment(word.df)

# Combine tweets to sentiment scores
emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)
View(emotion.df2)

# Score tweets based on positive and negative sentiment
sent.value <- get_sentiment(word.df)

# Filter based on positive, negative and neutral tweets. This is to save score respect to a tweet.
positive.tweets <- word.df[sent.value > 0]
negative.tweets <- word.df[sent.value < 0]
neutral.tweets <- word.df[sent.value == 0]

# We can display each of the above using the head() command
head(positive.tweets)
head(negative.tweets)
head(neutral.tweets)

#********* Most positive and negative tweets ************

# Select the most positive sentiment (highest sent.value)
most.positive <- word.df[sent.value == max(sent.value)]

# Display tweet with most positive sentiment
most.positive

# Select the most negative sentiment (lowest sent.value)
most.negative <- word.df[sent.value <= min(sent.value)] 

# Display tweet with most negative sentiment
most.negative 

# We can also display a selection of neutral tweets
head(neutral.tweets)

# Count number of positive, negative and neutral tweets. This is to create a pie chart to see proportion of them.
pos <- length(positive.tweets)
neut <- length(neutral.tweets)
neg <- length(negative.tweets)

#to show the number of positive,neutral and negative tweets 
pos
neut
neg

#**************** Pie chart *****************

# First combine the values and assign to a vector
tweets_vector <- c(pos, neut, neg)

#to view this concatenation.
tweets_vector

# Now define the labels to be used in pie chart 
labels <- c("Positive", "Neutral", "Negative")
pct <- round(tweets_vector/sum(tweets_vector)*100)

#add percents to labels
lbls <- paste(labels,pct)

#add % to labels
lbls <- paste(lbls, "%", sep = "")

#to get 3D pie chart, the package 'plotrix have to be installed'
install.packages("plotrix")
library(plotrix)

pie3D(tweets_vector,labels=lbls,explode=0.1, main="Sentiment Analysis for nzherald tweets")

#************* TDM ******************

# Create a corpus (collection of words) from the data frame of cleaned tweets
tweet_corpus <- Corpus(VectorSource(word.df))

# create term document matrix applying some restrictions
#for nzherald_tweets
tdm <- TermDocumentMatrix(tweet_corpus,
                          control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                         stopwords = c("nzherald", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix to calculate word frequencies
tdm.matrix <- as.matrix(tdm)

#To visualise the matrix for words frequency
tdm.matrix

#**************** Wordcloud ***********************

# to get word counts in decreasing order
word_freqs <- sort(rowSums(tdm.matrix), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm <- data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud with words that appear at least 50 times.
wordcloud(dm$word, dm$freq, min.freq = 50, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# To find associations/ correlations between words with most frequent words (min.freq=50), findAssocs function is used.
#Parameters are indicated Where 0 means never occurs and 1 is always  occurs. For 0.65:
findAssocs(tdm,"christchurch",0.65)
findAssocs(tdm,"mosque",0.65)
findAssocs(tdm,"police",0.65)

#for a correlation of 25%:
findAssocs(tdm,"christchurch",0.25)
findAssocs(tdm,"mosque",0.25)
findAssocs(tdm,"police",0.25)

#*********Extraction and pre-processing for NZStuff_tweets*********************

# The following command extracts last 1000 tweets from each Twitter account
Stuff_tweets = userTimeline("NZStuff", n=1000)

#to check tweets and its punctuation
head(Stuff_tweets)

# Convert tweets into a data frame for easier pre-processing. It can checked clickling in tweets.df in Data window
tweets.df <- twListToDF(Stuff_tweets)

#to view the table that contains tweets before processing
head(tweets.df)

# Use a 'find and replace' function to remove symbol and irrelevant signs from tweets
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("\n","",tweets.df2) 
tweets.df2 <- gsub(" \n","",tweets.df2)
tweets.df2 <- gsub("\n\nhttps:*","",tweets.df2) 
tweets.df2 <- gsub("\n\n","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
#this command bellow eliminates all punctuations
tweets.df2 <- gsub("[[:punct:]]","",tweets.df2)

#to check and look for another sign to remove.
tweets.df2

#to display tweets
head(tweets.df2)

#*********Sentiment analysis: identification of tweets for each emotional category*********************

# Convert data frame into a vector before performing sentiment analysis
word.df <- as.vector(tweets.df2)
head(word.df)

# Perform sentiment analysis to score tweets on an emotion
emotion.df <- get_nrc_sentiment(word.df)

# Combine tweets to sentiment scores
emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)
View(emotion.df2)

# Score tweets based on positive and negative sentiment
sent.value <- get_sentiment(word.df)

# Filter based on positive, negative and neutral tweets. This is to save score respect to a tweet.
positive.tweets <- word.df[sent.value > 0]
negative.tweets <- word.df[sent.value < 0]
neutral.tweets <- word.df[sent.value == 0]

# We can display each of the above using the head() command
head(positive.tweets)
head(negative.tweets)
head(neutral.tweets)

#********* Most positive and negative tweets ************

# Select the most positive sentiment (highest sent.value)
most.positive <- word.df[sent.value == max(sent.value)]

# Display tweet with most positive sentiment
most.positive

# Select the most negative sentiment (lowest sent.value)
most.negative <- word.df[sent.value <= min(sent.value)] 

# Display tweet with most negative sentiment
most.negative 

# We can also display a selection of neutral tweets
head(neutral.tweets)

# Count number of positive, negative and neutral tweets. This is to create a pie chart to see proportion of them.
pos <- length(positive.tweets)
neut <- length(neutral.tweets)
neg <- length(negative.tweets)

#to show the number of positive,neutral and negative tweets 
pos
neut
neg

#**************** Pie chart *****************

# First combine the values and assign to a vector
tweets_vector <- c(pos, neut, neg)

#to view this concatenation.
tweets_vector

# Now define the labels to be used in pie chart 
labels <- c("Positive", "Neutral", "Negative")
pct <- round(tweets_vector/sum(tweets_vector)*100)

#add percents to labels
lbls <- paste(labels,pct)

#add % to labels
lbls <- paste(lbls, "%", sep = "")

#to get 3D pie chart, the package 'plotrix have to be installed'
install.packages("plotrix")
library(plotrix)

pie3D(tweets_vector,labels=lbls,explode=0.1, main="Sentiment Analysis for NZStuff tweets")

#************* TDM ******************

# Create a corpus (collection of words) from the data frame of cleaned tweets
tweet_corpus <- Corpus(VectorSource(word.df))

# create term document matrix applying some restrictions
#Stuff_tweets
tdm <- TermDocumentMatrix(tweet_corpus,
                          control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                         stopwords = c("Stuff", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))



# define tdm as matrix to calculate word frequencies
tdm.matrix <- as.matrix(tdm)

#To visualise the matrix for words frequency
tdm.matrix

#**************** Wordcloud ***********************

# to get word counts in decreasing order
word_freqs <- sort(rowSums(tdm.matrix), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm <- data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud with words that appear at least 50 times.
wordcloud(dm$word, dm$freq, min.freq = 40, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# To find associations/ correlations between words with most frequent words (min.freq=50), findAssocs function is used.
#Parameters are indicated Where 0 means never occurs and 1 is always  occurs. For 0.65:
findAssocs(tdm,"christchurch",0.65)
findAssocs(tdm,"rugby",0.65)
findAssocs(tdm,"super",0.65)

#for a correlation of 25%:
findAssocs(tdm,"christchurch",0.25)
findAssocs(tdm,"rugby",0.25)
findAssocs(tdm,"super",0.25)

#************************* Text mining (Word Cloud and Bar plot)******************************************

# New libraries have to be installed and loaded to perform a text mining
install.packages("RWeka")
install.packages("RSentiment")
install.packages("DT")

library('RWeka')
library('RSentiment')
library('DT')

#********** For Newstalk ******
#tweets from Newstalk Twitter account, are cleaned again to avoid a confusion with the other accounts 
tweets.df <- twListToDF(Newstalk_tweets)

tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("\n","",tweets.df2) 
tweets.df2 <- gsub(" \n","",tweets.df2)
tweets.df2 <- gsub("\n\nhttps:*","",tweets.df2) 
tweets.df2 <- gsub("\n\n","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
#this command bellow eliminates all punctuations
tweets.df2 <- gsub("[[:punct:]]","",tweets.df2)

# Convert data frame into a vector 
word.df <- as.vector(tweets.df2)

#create a corpus
corpus <- Corpus(VectorSource(word.df))

# Cleanse corpus from corpus created before
#strip extra whitespaces
corpus = tm_map(corpus, stripWhitespace)

#to stemming a document
corpus = tm_map(corpus, stemDocument)

# Create a Document Term Matrix from the Corpus (it is different to tdm created before)
dtm_up <- DocumentTermMatrix(corpus, control = list(wordLengths=c(5, 15),
                                       stopwords = c("Newstalk", "listen", "english")))

# Examine DTM object to look for another stop word.
inspect(dtm_up)

# Calculating Sentiments. This sums frequencies of each word in the matrix
freq_up <- colSums(as.matrix(dtm_up))
head(freq_up)

# Now calculate the sentiment of each term in the DTM
sentiments_up = calculate_sentiment(names(freq_up))
head(sentiments_up)

# to combine the sentiment of each term and its frequency
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
head(sentiments_up)

# to filter positive and negative sentiments that are saved into separate variables
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]
head(sent_pos_up)
head(sent_neg_up)

# To display number of positive and negative sentiments
cat("We have higher negative Sentiments: ",sum(sent_neg_up$freq_up),
    " than positive: ",sum(sent_pos_up$freq_up))

# To analyse positive sentiment in a table
DT::datatable(sent_pos_up)

# for a frequency of 10 
wordcloud(sent_pos_up$text,sent_pos_up$freq_up, min.freq=10,random.order=FALSE, colors=brewer.pal(6,"Dark2"))

# To analyse negative sentiment in a table
DT::datatable(sent_neg_up)

#for frequency of 10 
wordcloud(sent_neg_up$text,sent_neg_up$freq_up, min.freq=10,random.order=FALSE, colors=brewer.pal(6,"Dark2"))


#*********** Bar plot ****************

#to get a barplot to visualise this result, install libraries
install.packages ("ggplot2")
install.packages("tidytext")

library(ggplot2) 
library(tidytext)

#to get a dataframe (table of words)
DF <- tidy(dtm_up)
head(DF)
head(DF$term)

#to convert a dataframe into a vector with the same elements.
some_txt <- as.vector(DF$term)
head(some_txt)

# Perform a sentiment analysis on the vector using the described function 
mysentiment<-get_nrc_sentiment((some_txt))

#each word gives an score to express a sentiment
head(mysentiment)

# Get the sentiment score for each emotion. 
#Each column is added to get a total for each sentiment

mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

#to get the total number of emotions
head(mysentiment.positive)
head(mysentiment.anger)
head(mysentiment.anticipation)
head(mysentiment.disgust)
head(mysentiment.negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis) 
barplot(yAxis, names.arg = xAxis, 
        xlab = "Sentiment Analysis", ylab = "Score", main = "Sentiment for Newstalk tweets", 
        col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, 
        cex.sub = 0.8, col.sub = "blue")

colSums(mysentiment)


#********** For nzherald ******

# Convert tweets into a data frame for easier pre-processing. 
tweets.df <- twListToDF(nzherald_tweets)

#to view the table that contains tweets before processing
head(tweets.df)

# Use a 'find and replace' function to remove symbol and irrelevant signs from tweets
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("\n","",tweets.df2) 
tweets.df2 <- gsub(" \n","",tweets.df2)
tweets.df2 <- gsub("\n\nhttps:*","",tweets.df2) 
tweets.df2 <- gsub("\n\n","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
#this command bellow eliminates all punctuations
tweets.df2 <- gsub("[[:punct:]]","",tweets.df2)

#Remove this specific expression from nzherald_tweets:
tweets.df2 <- gsub("\U0001f3a7","",tweets.df2)

# Convert data frame into a vector 
word.df <- as.vector(tweets.df2)

#create a corpus
corpus <- Corpus(VectorSource(word.df))

# Cleanse corpus from corpus created before
#strip extra whitespaces
corpus = tm_map(corpus, stripWhitespace)

#to stemming a document
corpus = tm_map(corpus, stemDocument)

# Create a Document Term Matrix from the Corpus (it is different to tdm created before)
dtm_up <- DocumentTermMatrix(corpus, control = list(wordLengths=c(5, 15),
                                                    stopwords = c("nzherald", "english")))

# Examine DTM object to look for another stop word.
inspect(dtm_up)

# Calculating Sentiments. This sums frequencies of each word in the matrix
freq_up <- colSums(as.matrix(dtm_up))
head(freq_up)

# Now calculate the sentiment of each term in the DTM
sentiments_up = calculate_sentiment(names(freq_up))
head(sentiments_up)

# to combine the sentiment of each term and its frequency
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
head(sentiments_up)

# to filter positive and negative sentiments that are saved into separate variables
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]
head(sent_pos_up)
head(sent_neg_up)

# To display number of positive and negative sentiments
cat("We have higher negative Sentiments: ",sum(sent_neg_up$freq_up),
    " than positive: ",sum(sent_pos_up$freq_up))

# To analyse positive sentiment in a table
DT::datatable(sent_pos_up)

# for a frequency of 10 
wordcloud(sent_pos_up$text,sent_pos_up$freq_up, min.freq=10,random.order=FALSE, colors=brewer.pal(6,"Dark2"))

# To analyse negative sentiment in a table
DT::datatable(sent_neg_up)

#for frequency of 10 
wordcloud(sent_neg_up$text,sent_neg_up$freq_up, min.freq=10,random.order=FALSE, colors=brewer.pal(6,"Dark2"))


#*********** Bar plot ****************

#to get a barplot to visualise this result, install libraries
library(ggplot2) 
library(tidytext)

install.packages ("ggplot2")
install.packages("tidytext")

#to get a dataframe (table of words)
DF <- tidy(dtm_up)
head(DF)
head(DF$term)

#to convert a dataframe into a vector
some_txt <- as.vector(DF$term)
head(some_txt)

# Perform sentiment analysis on the vector using function described
mysentiment<-get_nrc_sentiment((some_txt))

#each word gives an score to express a sentiment
head(mysentiment)

# Get the sentiment score for each emotion. Each column is added to get a total for each sentiment
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

head(mysentiment.negative)
# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis) 
barplot(yAxis, names.arg = xAxis, 
        xlab = "Sentiment Analysis", ylab = "Score", main = "Sentiment for nzherald tweets", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")

#to display total scores for each emotion
colSums(mysentiment)


#********** For NZStuff ******

#tweets from NZStuff Twitter account, are cleaned again to avoid a confusion with the other accounts 
tweets.df <- twListToDF(Stuff_tweets)

tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("\n","",tweets.df2) 
tweets.df2 <- gsub(" \n","",tweets.df2)
tweets.df2 <- gsub("\n\nhttps:*","",tweets.df2) 
tweets.df2 <- gsub("\n\n","",tweets.df2) 
tweets.df2 <- gsub("#","",tweets.df2) 
#this command bellow eliminates all punctuations
tweets.df2 <- gsub("[[:punct:]]","",tweets.df2)

# Convert data frame into a vector 
word.df <- as.vector(tweets.df2)

#create a corpus
corpus <- Corpus(VectorSource(word.df))

# Cleanse corpus from corpus created before
#strip extra whitespaces
corpus = tm_map(corpus, stripWhitespace)

#to stemming a document
corpus = tm_map(corpus, stemDocument)

# Create a Document Term Matrix from the Corpus (it is different to tdm created before)
dtm_up <- DocumentTermMatrix(corpus, control = list(wordLengths=c(5, 15),
                                                    stopwords = c("Stuff", "english")))

# Examine DTM object to look for another stop word.
inspect(dtm_up)

# Calculating Sentiments. This sums frequencies of each word in the matrix
freq_up <- colSums(as.matrix(dtm_up))
head(freq_up)

# Now calculate the sentiment of each term in the DTM
sentiments_up = calculate_sentiment(names(freq_up))
head(sentiments_up)

# to combine the sentiment of each term and its frequency
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
head(sentiments_up)

# to filter positive and negative sentiments that are saved into separate variables
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]
head(sent_pos_up)
head(sent_neg_up)

# To display number of positive and negative sentiments
cat("We have higher negative Sentiments: ",sum(sent_neg_up$freq_up),
    " than positive: ",sum(sent_pos_up$freq_up))

# To analyse positive sentiment in a table
DT::datatable(sent_pos_up)

# for a frequency of 10 
wordcloud(sent_pos_up$text,sent_pos_up$freq_up, min.freq=10,random.order=FALSE, colors=brewer.pal(6,"Dark2"))

# To analyse negative sentiment in a table
DT::datatable(sent_neg_up)

#for a frequency of 10 
wordcloud(sent_neg_up$text,sent_neg_up$freq_up, min.freq=10,random.order=FALSE, colors=brewer.pal(6,"Dark2"))

#to get a barplot to visualise this result, install libraries

install.packages ("ggplot2")
install.packages("tidytext")
library(ggplot2) 
library(tidytext)

#to get a dataframe (table of words)
DF <- tidy(dtm_up)
head(DF)
head(DF$term)

#to convert a dataframe into a vector
some_txt <- as.vector(DF$term)
head(some_txt)

# Perform sentiment analysis on the vector using function described
mysentiment<-get_nrc_sentiment((some_txt))

#each word gives an score to express a sentiment
head(mysentiment)

# Get the sentiment score for each emotion. Each column is added to get a total for each sentiment
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

head(mysentiment.negative)
# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis) 
barplot(yAxis, names.arg = xAxis, 
        xlab = "Sentiment Analysis", ylab = "Score", main = "Sentiment for NZStuff tweets", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")

#to display total scores for each emotion
colSums(mysentiment)

#*******************************END*****************************************************