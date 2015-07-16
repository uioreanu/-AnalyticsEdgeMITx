# 
#	The Analytics Edge
#	Unit5 Text analytics
#	DETECTING VANDALISM ON WIKIPEDIA
#	Calin Uioreanu
#

#DETECTING VANDALISM ON WIKIPEDIA
#
#Wikipedia is a free online encyclopedia that anyone can edit and contribute to. It is available in many languages and is growing all the time. On the English language version of Wikipedia:
#
#There are currently 4.7 million pages.
#There have been a total over 760 million edits (also called revisions) over its lifetime.
#There are approximately 130,000 edits per day.
#One of the consequences of being editable by anyone is that some people vandalize pages. This can take the form of removing content, adding promotional or inappropriate content, or more subtle shifts that change the meaning of the article. With this many articles and edits per day it is difficult for humans to detect all instances of vandalism and revert (undo) them. As a result, Wikipedia uses bots - computer programs that automatically revert edits that look like vandalism. In this assignment we will attempt to develop a vandalism detector that uses machine learning to distinguish between a valid edit and vandalism.
#
#The data for this problem is based on the revision history of the page Language. Wikipedia provides a history for each page that consists of the state of the page at each revision. Rather than manually considering each revision, a script was run that checked whether edits stayed or were reverted. If a change was eventually reverted then that revision is marked as vandalism. This may result in some misclassifications, but the script performs well enough for our needs.
#
#As a result of this preprocessing, some common processing tasks have already been done, including lower-casing and punctuation removal. The columns in the dataset are:
#
#Vandal = 1 if this edit was vandalism, 0 if not.
#Minor = 1 if the user marked this edit as a "minor edit", 0 if not.
#Loggedin = 1 if the user made this edit while using a Wikipedia account, 0 if they did not.
#Added = The unique words added.
#Removed = The unique words removed.
#Notice the repeated use of unique. The data we have available is not the traditional bag of words - rather it is the set of words that were removed or added. For example, if a word was removed multiple times in a revision it will only appear one time in the "Removed" column.
#

# cleanup
rm(list=ls())
gc()

# load data
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)

wiki$Vandal = as.factor(wiki$Vandal)

dim(wiki)

table(wiki$Vandal)
#   0    1 
#2061 1815 

# Load tm package
library(tm)

# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))

# remove stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

# stemming
corpusAdded = tm_map(corpusAdded, stemDocument)

# Create matrix
dtmAdded = DocumentTermMatrix(corpusAdded)

# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)

# Create data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))

# prepend all the words with the letter A, by using the command
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

wordsAdded

# same steps for removed words

# Create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))

# remove stopwords
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

# stemming
corpusRemoved = tm_map(corpusRemoved, stemDocument)

# Create matrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)

# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

# Create data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

# prepend all the words with the letter A, by using the command
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# Combine the two data frames into a data frame called wikiWords
wikiWords = cbind(wordsAdded, wordsRemoved)

# add the Vandal field
wikiWords$Vandal <- wiki$Vandal

# split the data
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

dim(train); dim(test)
#[1] 2713  329
#[1] 1163  329

# What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
table(test$Vandal)
#  0   1 
#618 545 
618/nrow(test)
# 0.5313844


# Build a CART model
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal~., data=train, method="class")

prp(wikiCART)

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(predict(wikiCART, newdata=test, type="class"), test$Vandal)
#	  0   1
#  0 618 533
#  1   0  12

(618+12)/nrow(test)
#0.5417025


#We weren't able to improve on the baseline using the raw textual information. More specifically, the words themselves were not useful. There are other options though, and in this section we will try two techniques - identifying a key class of words, and counting words.
wikiWords2 = wikiWords

#We can search for the presence of a web address in the words added by searching for "http" in the Added column. The grepl function returns TRUE if a string is found in another string, e.g.
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

# build a new CART model
wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")

prp(wikiCART2)

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(predict(wikiCART2, newdata=wikiTest2, type="class"), wikiTest2$Vandal)
#      0   1
#  0 609 488
#  1   9  57

(609+57)/nrow(wikiTest2)
# 0.5726569

# using the word count available in the form of the document-term matrices (DTMs)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")

prp(wikiCART2)

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(predict(wikiCART2, newdata=wikiTest2, type="class"), wikiTest2$Vandal)
#      0   1
#  0 514 297
#  1 104 248
(514+248)/nrow(wikiTest2) 
# 0.6552021

# We have two pieces of "metadata" (data about data) that we haven't yet used. 
wikiWords3 = wikiWords2

# add the two original variables Minor and Loggedin to this new data frame:
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)

wikiCART3 = rpart(Vandal~., data=wikiTrain3, method="class")

prp(wikiCART3)

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(predict(wikiCART3, newdata=wikiTest3, type="class"), wikiTest2$Vandal)
#      0   1
#  0 595 304
#  1  23 241
(595+241)/nrow(wikiTest2) 
# 0.7188306

