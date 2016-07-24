setwd(PATH)
############################# NLP with TWITTER API ###############################

### Load libraries ###
library(tm)
library(SnowballC)
library(quanteda)
library(caret)
library(ROAuth)
library(streamR)
library(caTools)
library(wordcloud)
# Internal dependencies that should be downloaded if not already installed
# library(klaR)
# library(MASS)
# library(rjson)
# library(RCurl)
# library(bitops)
# library(NLP)

######## Text preprocessing functions #######

# Function takes a corpus as input and minCount 
# Words that appear in less than minCount will be removed
# It returns a dataframe that represents the document term matrix of this corpus
# The function also tokenizes the test, removes punctuation, whitespaces, stop words
# and stems the tokens
# The output consists of unigrams, and bigrams of words in corpus
preprocess <- function(corpus, minCount = 20) {
  # make everything lowercase
  corpus <- tm_map(corpus, content_transformer(tolower), lazy = FALSE)
  # remove whitespaces
  corpus  <- tm_map(corpus, stripWhitespace, lazy = FALSE)
  # remove punctuation
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE, lazy = FALSE)
  # remove stop words from corpus
  corpus <- tm_map(corpus, removeWords, words=c(stopwords("english")), lazy = FALSE)
  # reduce words to their stem using Porter's stemming algorithm
  corpus <- tm_map(corpus, stemDocument, lazy = FALSE)
  quanteda.corpus <- corpus(corpus)
  # unigrams
  unigrams.dfm <- dfm(quanteda.corpus, ngrams = 1, verbose = FALSE)
  unigrams.trim <- trim(unigrams.dfm, minCount = minCount) 
  unigrams.tfidfdtm <- weight(unigrams.trim, type = "tfidf")
  unigrams.dtmmat <- as.matrix(unigrams.tfidfdtm)
  
  bigrams.dfm <- dfm(quanteda.corpus, ngrams = 2, verbose = FALSE)
  bigrams.trim <- trim(bigrams.dfm, minCount = minCount) 
  bigrams.tfidfdtm <- weight(bigrams.trim, type = "tfidf")
  bigrams.dtmmat <- as.matrix(bigrams.tfidfdtm)
  
  
  tfidfdtmmat <- cbind(unigrams.dtmmat, bigrams.dtmmat)
  tfidfdtm.df <- as.data.frame(tfidfdtmmat)
  return(tfidfdtm.df)
}

# Helper function used to reformat the structure of document term matrix (dtm) of test data
# to that of train data
# Takes dtm of both train and test data and outputs reformatted dtm for test data
prepare_newData <- function(trainData, new.dtm){
  # Create an empty dataframe with column names same as features in training data
  train.features <- names(trainData)
  newData <- matrix(data = rep(0, length(train.features) * nrow(new.dtm)), 
                    nrow = nrow(new.dtm), ncol = length(train.features))
  colnames(newData) <- train.features
  row.names(newData) <- row.names(new.dtm)
  
  # scores for features common to both train and test are copied from test data
  # For features present in test data but unseen in train data the scores remain 0
  common.features <- intersect(train.features, names(new.dtm))
  for(i in 1:length(common.features)) {
    newData[,common.features[i]] <- new.dtm[,common.features[i]]
  }
  newData <- as.data.frame(newData)
  return(newData)
}


###### Twitter API authorization #####
## Registing OAuth token
## Step 1: Go to dev.twitter.com and sign in
## Step 2: Go to Documentation, find "Manage Your Apps" on the page and click on it
## Step 3: Then click on "Create New App"
## Step 4: Fill name, description, and website (it can be anything "http://url.com" works for me)
## Step 5: Agree to user conditions and create
## Step 6: In "Keys and Access Tokens" tab, copy "consumer key" and "consumer secret" and paste below
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- 	YOURKEY
consumerSecret <-	YOURSECRET

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)
## Run this line and go to the URL that appears on screen
## This will open a page, click "authorize my app", get the PIN and enter it to R console
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## You can save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file = "my_oauth")
# loading OAuth token
load("my_oauth")


###### Training Data #####
# Data source - https://www.kaggle.com/crowdflower/first-gop-debate-twitter-sentiment
sentiment <- read.csv("Sentiment.csv", comment.char="#", stringsAsFactors=FALSE)
# class label is given as "sentiment" attribute which can be "Neutral", "Negative", or "Positive"
# Confidence of this label is given as "sentiment_confidence"
# we only consider records with sentiment confidence >= 0.65 for training
train.data <- sentiment[which(sentiment$sentiment_confidence >= 0.65),c(16, 6)]
# bar plot for percentage of each of sentiments in this training data
plot(table(train.data$sentiment))
table(train.data$sentiment)

# preprocessing tweets
# remove retweet entities
train.data$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", train.data$text)
# remove at people
train.data$text = gsub("@\\w+", "", train.data$text)
# remove html links
train.data$text = gsub("http\\w+", "", train.data$text)
train.data$text = gsub("https\\w+", "", train.data$text)
# remove numbers
train.data$text = gsub("[[:digit:]]", "", train.data$text)
# remove empty sentences
train.data <- train.data[which(train.data$text != ""),]

# Undersampling data so that each class label is equally distributed
set.seed(123)
negative.samples <- train.data[which(train.data$sentiment == "Negative"), ]
negative.samples <- negative.samples[sample(c(1:nrow(negative.samples)), size = 1500), ]

positive.samples <- train.data[which(train.data$sentiment == "Positive"), ]
positive.samples <- positive.samples[sample(c(1:nrow(positive.samples)), size = 1500), ]

train.data <- rbind.data.frame(negative.samples, positive.samples)
plot(table(train.data$sentiment))
# data frame containing only the tweets
train.text <- data.frame(train.data$text)

# Create a corpus of training data
train.corpus <- Corpus(DataframeSource(train.text))
# train.dtm is the document term matrix for this corpus containing unigrams, bigrams, trigrams. 
# tf-idf based score is used in this matrix
train.dtm <- preprocess(train.corpus)

# Adding labels for training data from original dataframe
train.dtm$label <- as.factor(train.data$sentiment)

# train-test split to analyze algorithm performance
set.seed(42)

split = sample.split(train.dtm$label, SplitRatio = 0.7)
trainSubset = subset(train.dtm, split==TRUE)
testSubset = subset(train.dtm, split==FALSE)
testData = testSubset[,-ncol(testSubset)]


set.seed(100)
modelNN = train(as.factor(label) ~.,  
                    data = trainSubset, 
                    trControl = trainControl(method = "cv", number = 5),
                    method='pcaNNet',
                    verbose = FALSE)
# Important variables according to ctree model
nnet.imp <- varImp(modelNN)
print(nnet.imp)
plot(nnet.imp, top = 20, main = "Neural Network Important features")

# prediction on test data
nnet.pred <- predict(modelNN, testData, type = "raw")

# Error metrics
nnet.metrics <- confusionMatrix(nnet.pred, testSubset$label)
print(nnet.metrics)

########### neural network model training ###########
# Train neural network model using training data dtm and class labels
# 5-fold crossvalidation is used internally in training
modelNN = train(as.factor(label) ~.,  
                 data = train.dtm, 
                 trControl = trainControl(method = "cv", number = 5),
                 method='pcaNNet',
                 verbose = FALSE)
# Important variables according to the model
nnet.imp <- varImp(modelNN)
print(nnet.imp)
plot(nnet.imp, top = 40, main = "Neural Network Important features")

################## Get live twitter streams for sentiment analysis ##################
# The following code can be modified and run for different key words
# Training part (code until previous section) does not have to be repeated 

####### Twitter live stream capture #######
# capturing tweets, following are some explanations about the parameters
# track = c(): string or string vector containing keywords to track
# timeout: numeric, maximum length of time (in seconds) of connection to stream. 
# The default is 0, which will keep the connection open permanently.
# if want to scrap 1 minutes of tweets mentioning obama, set timeout = 60
# tweets: numeric, maximum number of tweets to be collected when function is called
# locations: numeric, a vector of longitude, latitude pairs (with the southwest corner coming first) 
# specifying sets of bounding boxes to filter public statuses by. 
# See the locations parameter information in the Streaming API documentation for details: 
# http://dev.twitter.com/docs/streaming-apis/parameters#locations
phrase = "kasich"
filterStream(file.name="tweets_keyword.json", track=c(phrase), timeout=60, tweets=100, language="en", oauth=my_oauth)
# parsing tweets into dataframe
tweets <- parseTweets("tweets_keyword.json", verbose = TRUE)

###### Test data predictions ######
# Subset only text from tweets parsed
tweets <- data.frame(tweets[, 1])
# Create a new corpus for test data
test.corpus <- Corpus(DataframeSource(tweets))

#For Windows Users
test.corpus <- tm_map(test.corpus, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                      mc.cores=1)
#For mac users 
#test.corpus <- tm_map(test.corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
#                      mc.cores=1)

# Perform the same preprocessing steps as training data
test.dtm <- preprocess(test.corpus)

# prediction on test data using naive bayes model
nn.newData <- prepare_newData(modelNN$trainingData[, -ncol(modelNN$trainingData)],
                                       test.dtm)

wordcloud(words = names(nn.newData), 
          freq = colMeans(nn.newData), max.words = 100,
          random.order = FALSE,scale=c(1.7,0.1), 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

nnPred <- predict(modelNN, nn.newData)
# Distribution of sentiment in predicted data
plot(table(nnPred))
summary(nnPred)