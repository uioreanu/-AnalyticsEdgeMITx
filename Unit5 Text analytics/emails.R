# 
#	The Analytics Edge
#	Unit5 Text analytics
#	SEPARATING SPAM FROM HAM (PART 1)
#	Calin Uioreanu
#

#SEPARATING SPAM FROM HAM (PART 1)
#Nearly every email user has at some point encountered a "spam" email, which is an unsolicited message often advertising a product, containing links to malware, or attempting to scam the recipient. Roughly 80-90% of more than 100 billion emails sent each day are spam emails, most being sent from botnets of malware-infected computers. The remainder of emails are called "ham" emails.
#
#As a result of the huge number of spam emails being sent across the Internet each day, most email providers offer a spam filter that automatically flags likely spam messages and separates them from the ham. Though these filters use a number of techniques (e.g. looking up the sender in a so-called "Blackhole List" that contains IP addresses of likely spammers), most rely heavily on the analysis of the contents of an email via text analytics.
#
#In this homework problem, we will build and evaluate a spam filter using a publicly available dataset first described in the 2006 conference paper "Spam Filtering with Naive Bayes -- Which Naive Bayes?" by V. Metsis, I. Androutsopoulos, and G. Paliouras. The "ham" messages in this dataset come from the inbox of former Enron Managing Director for Research Vincent Kaminski, one of the inboxes in the Enron Corpus. One source of spam messages in this dataset is the SpamAssassin corpus, which contains hand-labeled spam messages contributed by Internet users. The remaining spam was collected by Project Honey Pot, a project that collects spam messages and identifies spammers by publishing email address that humans would know not to contact but that bots might target with spam. The full dataset we will use was constructed as roughly a 75/25 mix of the ham and spam messages.

# cleanup
rm(list=ls())
gc()

# load data
emails = read.csv("emails.csv", stringsAsFactors=FALSE)

# How many of the emails are spam?
table(emails$spam)
#   0    1 
#4360 1368 

#  How many characters are in the longest email in the dataset
summary(nchar(emails$text))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   13.0   508.8   979.0  1557.0  1894.0 43950.0

# Which row contains the shortest email in the dataset? 
which.min(nchar(emails$text))
#[1] 1992


# Load tm package
library(tm)

# Create corpus
corpus = Corpus(VectorSource(emails$text))

corpus[[1]]$content

# Pre-process data
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm
#<<DocumentTermMatrix (documents: 5728, terms: 28687)>>
#Non-/sparse entries: 481719/163837417
#Sparsity           : 100%
#Maximal term length: 24
#Weighting          : term frequency (tf)

#To obtain a more reasonable number of terms, limit dtm to contain terms appearing in at least 5% of documents, and store this result as spdtm (don't overwrite dtm, because we will use it in a later step of this homework). How many terms are in spdtm?

# Remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)
spdtm
#<<DocumentTermMatrix (documents: 5728, terms: 330)>>
#Non-/sparse entries: 213551/1676689
#Sparsity           : 89%
#Maximal term length: 10
#Weighting          : term frequency (tf)


# Create data frame
emailsSparse  = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

#colSums() is an R function that returns the sum of values for each variable in our data frame. Our data frame contains the number of times each word stem (columns) appeared in each email (rows). Therefore, colSums(emailsSparse) returns the number of times a word stem appeared across all the emails in the dataset. What is the word stem that shows up most frequently across all the emails in the dataset? Hint: think about how you can use sort() or which.max() to pick out the maximum frequency.

sort(colSums(emailsSparse))
#      hou       will       vinc    subject        ect      enron 
#     5577       8252       8532      10202      11427      13388 

#Add a variable called "spam" to emailsSparse containing the email spam labels. You can do this by copying over the "spam" variable from the original data frame (remember how we did this in the Twitter lecture).
emailsSparse$spam<-emails$spam

#How many word stems appear at least 5000 times in the ham emails in the dataset? Hint: in this and the next question, remember not to count the dependent variable we just added.
sum(colSums(emailsSparse)>5000)
# [1] 8

sort(colSums(subset(emailsSparse, spam == 0)))
#We can read the most frequent terms in the ham dataset with sort(colSums(subset(emailsSparse, spam == 0))). "enron", "ect", "subject", "vinc", "will", and "hou" appear at least 5000 times in the ham dataset.

# How many word stems appear at least 1000 times in the spam emails in the dataset?
sum((colSums(subset(emailsSparse, spam == 1)))>1000)
#	[1] 4
# We can limit the dataset to the spam emails with subset(emailsSparse, spam == 1). Therefore, we can read the most frequent terms with sort(colSums(subset(emailsSparse, spam == 1))). "subject", "will", and "compani" are the three stems that appear at least 1000 times. Note that the variable "spam" is the dependent variable and is not the frequency of a word stem.

#BUILDING MACHINE LEARNING MODELS

emailsSparse$spam = as.factor(emailsSparse$spam)

# split the data
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

dim(train); dim(test)
#[1] 4010  331
#[1] 1718  331


#Using the training set, train the following three machine learning models. The models should predict the dependent variable "spam", using all other available variables as independent variables. Please be patient, as these models may take a few minutes to train.

#1) A logistic regression model called spamLog. You may see a warning message here - we'll discuss this more later.
spamLog<-glm(spam~., data=train, family="binomial")
#Warning messages:
#1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 

# 2) A CART model called spamCART, using the default parameters to train the model (don't worry about adding minbucket or cp). Remember to add the argument method="class" since this is a binary classification problem.
# Build a CART model
library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")

#3) A random forest model called spamRF, using the default parameters to train the model (don't worry about specifying ntree or nodesize). Directly before training the random forest model, set the random seed to 123 (even though we've already done this earlier in the problem, it's important to set the seed right before training the model so we all obtain the same results. Keep in mind though that on certain operating systems, your results might still be slightly different).
library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data=train)


#For each model, obtain the predicted spam probabilities for the training set. Be careful to obtain probabilities instead of predicted classes, because we will be using these values to compute training set AUC values. Recall that you can obtain probabilities for CART models by not passing any type parameter to the predict() function, and you can obtain probabilities from a random forest by adding the argument type="prob". For CART and random forest, you need to select the second column of the output of the predict() function, corresponding to the probability of a message being spam.
predTrainLog<-predict(spamLog, newdata=train, type="response")
predTrainCART <-predict(spamCART)[,2]
predTrainRF <-predict(spamRF, type="prob")[,2]

#How many of the training set predicted probabilities from spamLog are less than 0.00001?
sum(predLog<0.00001)
#[1] 3046

#How many of the training set predicted probabilities from spamLog are more than 0.99999?
sum(predLog>0.99999)
#[1] 954

#How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
sum(predLog<=0.99999 & predLog>=0.00001)
#[1] 10


# slight ERROR
#PROBLEM 3.1 - BUILDING MACHINE LEARNING MODELS  

predTrainRF = predict(spamRF, type="prob")[,2]
#Error in predict.randomForest(spamRF, type = "prob") : 
#  'prob' or 'vote' not meaningful for regression
(predTrainRF = predict(spamRF))
#character(0).4004 character(0).4005 character(0).4006 character(0).4007 character(0).4008 character(0).4009 
#     0.0029069767      0.0000000000      0.0000000000      0.0000000000      0.0000000000      0.1141791045 


#How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?
summary(spamLog)
#0

#How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree? Recall that we suspect these word stems are specific to Vincent Kaminski and might affect the generalizability of a spam filter built with his ham data.
prp(spamCART)

#What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, predTrainLog>0.5)
#   FALSE TRUE
#  0  3052    0
#  1     4  954
(3052+954)/nrow(train)
#0.9990025

library(ROCR)
predROCR = prediction(predTrainLog, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

#What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions? (Remember that if you used the type="class" argument when making predictions, you automatically used a threshold of 0.5. If you did not add in the type argument to the predict function, the probabilities are in the second column of the predict output.)
table(train$spam, predTrainCART>0.5) 
#    FALSE TRUE
#  0  2885  167
#  1    64  894
(2885+894)/nrow(train)
# 0.942394


library(ROCR)
predROCR = prediction(predTrainCART, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values


#What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions? (Remember that your answer might not match ours exactly, due to random behavior in the random forest algorithm on different operating systems.)
table(train$spam, predTrainRF>0.5) 
#    FALSE TRUE
#  0  3007   45
#  1    52  906
(3007+906)/nrow(train)
# 0.9758105

#Which model had the best training set performance, in terms of accuracy and AUC?
# Logistic regression Logistic regression - correct  
# CART  
# Random forest

# EVALUATING ON THE TEST SET

#What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(test$spam, predict(spamLog, newdata=test, type="response")>0.5)
#    FALSE TRUE
#  0  1257   51
#  1    34  376
(1257+376)/nrow(test)
# 0.9505239

#What is the testing set AUC of spamLog?
library(ROCR)
predROCR = prediction(predict(spamLog, newdata=test, type="response"), train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
#  0.9696044

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
table(test$spam, predict(spamCART, newdata=test)[,2]>0.5)
#    FALSE TRUE
#  0  1228   80
#  1    24  386
(1228+386)/nrow(test)
#0.9394645

#What is the testing set AUC of spamCART?
library(ROCR)
predROCR = prediction(predict(spamCART, newdata=test)[,2], train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
# 0.9696044


# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(test$spam, predict(spamRF, newdata=test)>0.5) 
#    FALSE TRUE
#  0  1288   20
#  1    24  386
(1288+386)/nrow(test)
#0.9743888

#What is the testing set AUC of spamCART?
library(ROCR)
predROCR = prediction(predict(spamRF, newdata=test), train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
# 0.9696044

#Which model had the best testing set performance, in terms of accuracy and AUC?
# Logistic regression  
# CART  
# Random forest CORRECT












