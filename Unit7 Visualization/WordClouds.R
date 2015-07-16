#
# MITx: 15.071x The Analytics Edge
#

# VISUALIZING TEXT DATA USING WORD CLOUDS

# Earlier in the course, we used text analytics as a predictive tool, using word frequencies as independent variables in our models. However, sometimes our goal is to understand commonly occurring topics in text data instead of to predict the value of some dependent variable. In such cases, word clouds can be a visually appealing way to display the most frequent words in a body of text.
# 
# A word cloud arranges the most common words in some text, using size to indicate the frequency of a word. For instance, this is a word cloud for the complete works of Shakespeare, removing English stopwords:
#   
#   Shakespeare word cloud
# 
# While we could generate word clouds using free generators available on the Internet, we will have more flexibility and control over the process if we do so in R. We will visualize the text of tweets about Apple, a dataset we used earlier in the course. As a reminder, this dataset (which can be downloaded from tweets.csv) has the following variables:
#   
#   Tweet -- the text of the tweet
# 
# Avg -- the sentiment of the tweet, as assigned by users of Amazon Mechanical Turk. The score ranges on a scale from -2 to 2, where 2 means highly positive sentiment, -2 means highly negative sentiment, and 0 means neutral sentiment. 


tweets<-read.csv('tweets.csv', stringsAsFactors = FALSE)

str(tweets)

# Load tm package
library(tm)

# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet))

# Convert to lower-case

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, stopwords("english"))

# Create matrix
frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 
#inspect(frequencies[1000:1005,505:515])

# Check for sparsity
#findFreqTerms(frequencies, lowfreq=20)


#Install and load the "wordcloud" package, which is needed to build word clouds.
install.packages("wordcloud")
library(wordcloud)

allTweets = tweetsSparse = as.data.frame(as.matrix(frequencies))

colnames(allTweets)

sort(colSums(allTweets))

# build wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

# wordcloud only negative tweets
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets), random.order=FALSE )

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), rot.per=2)

?wordcloud

library("RColorBrewer")

# show all palettes
display.brewer.all() 
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),
          colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)]
)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),
          brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]
)

