# 
#	The Analytics Edge
#	Unit5 Text analytics
#	AUTOMATING REVIEWS IN MEDICINE
#	Calin Uioreanu
#

#AUTOMATING REVIEWS IN MEDICINE
#
#The medical literature is enormous. Pubmed, a database of medical publications maintained by the U.S. National Library of Medicine, has indexed over 23 million medical publications. Further, the rate of medical publication has increased over time, and now there are nearly 1 million new publications in the field each year, or more than one per minute.
#
#The large size and fast-changing nature of the medical literature has increased the need for reviews, which search databases like Pubmed for papers on a particular topic and then report results from the papers found. While such reviews are often performed manually, with multiple people reviewing each search result, this is tedious and time consuming. In this problem, we will see how text analytics can be used to automate the process of information retrieval.
#
#The dataset consists of the titles (variable title) and abstracts (variable abstract) of papers retrieved in a Pubmed search. Each search result is labeled with whether the paper is a clinical trial testing a drug therapy for cancer (variable trial). These labels were obtained by two people reviewing each search result and accessing the actual paper if necessary, as part of a literature review of clinical trials testing drug therapies for advanced and metastatic breast cancer.

# cleanup
rm(list=ls())
gc()

# load data
trial = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

#How many characters are there in the longest abstract? (Longest here is defined as the abstract with the largest number of characters.)

summary(nchar(trial$abstract))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      0    1196    1583    1481    1821    3708 

# How many search results provided no abstract? (HINT: A search result provided no abstract if the number of characters in the abstract field is zero.)
nrow(subset(trial, nchar(abstract)==0))
#[1] 112

# Find the observation with the minimum number of characters in the title (the variable "title") out of all of the observations in this dataset. What is the text of the title of this article? Include capitalization and punctuation in your response, but don't include the quotes.
trial[which.min(nchar(trial$title)),]


# build corpus for title and abstract

# Load tm package
library(tm)

# Create corpus
corpusTitle = Corpus(VectorSource(trial$title))
corpusAbstract = Corpus(VectorSource(trial$abstract))



# Pre-process data
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# Create matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# Filter out sparse terms: Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# Create data frame
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))


#What is the most frequent word stem across all the abstracts? Hint: you can use colSums() to compute the frequency of a word across all the abstracts.
sort(colSums(dtmAbstract))
#       respons    chemotherapi           group       treatment          cancer          breast         patient 
#           2051            2344            2668            2894            3726            3859            8381 

# combine dtmTitle and dtmAbstract into a single data frame to make predictions
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#combine dtmTitle and dtmAbstract into a single data frame called dtm:
dtm = cbind(dtmTitle, dtmAbstract)

dim(dtm)
#[1] 1860  366
dtm$trial<-trial$trial
dim(dtm)
#[1] 1860  367

# split the data
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

dim(train); dim(test)
#[1] 1302  367
#[1] 558 367

table(trial$trial)

#   0    1 
#1043  817 
table(test$trial)
#  0   1 
#313 245 

# What is the accuracy of the baseline model on the training set? (Remember that the baseline model predicts the most frequent outcome in the training set for all observations.)
#  0   1 
#313 245 
313/nrow(test)
#[1] 0.5609319

# Build a CART model
library(rpart)
library(rpart.plot)

trialCART = rpart(trial~., data=train, method="class")

prp( trialCART)

#Obtain the training set predictions for the model (do not yet predict on the test set). Extract the predicted probability of a result being a trial (recall that this involves not setting a type argument, and keeping only the second column of the predict output). What is the maximum predicted probability for any result?
summary(predict(trialCART, newdata=test)[,2])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.05455 0.13640 0.28750 0.41730 0.78230 0.87190 

#For these questions, use a threshold probability of 0.5 to predict that an observation is a clinical trial.
table(train$trial, predict(trialCART, newdata=train)[,2]>0.5)
#    FALSE TRUE
#  0   631   99
#  1   131  441

#What is the training set accuracy of the CART model?
(631+441)/nrow(train)
# 0.8233487
#What is the training set sensitivity of the CART model?
(441)/(441+131)
# 0.770979
#What is the training set specificity of the CART model?
631/(631+99)
#0.8643836

library(ROCR)
predROCR = prediction(predict(trialCART, newdata=train)[,2], train$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# predict on test
predTest<-predict(trialCART, newdata=test, type="class")


table(test$trial, predTest)
#      0   1
#  0 261  52
#  1  83 162


#What is the testing set accuracy, assuming a probability threshold of 0.5 for predicting that a result is a clinical trial?
(261+162)/nrow(test)
# 0.7580645

# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
predROCR = prediction(predict(trialCART, newdata=test)[,2], test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values
# 0.8371063



#The decision maker for this problem, a researcher performing a review of the medical literature, would use a model (like the CART one we built here) in the following workflow:
#
#1) For all of the papers retreived in the PubMed Search, predict which papers are clinical trials using the model. This yields some initial Set A of papers predicted to be trials, and some Set B of papers predicted not to be trials. (See the figure below.)
#
#2) Then, the decision maker manually reviews all papers in Set A, verifying that each paper meets the study's detailed inclusion criteria (for the purposes of this analysis, we assume this manual review is 100% accurate at identifying whether a paper in Set A is relevant to the study). This yields a more limited set of papers to be included in the study, which would ideally be all papers in the medical literature meeting the detailed inclusion criteria for the study.
#
#3) Perform the study-specific analysis, using data extracted from the limited set of papers identified in step 2.
#
#This process is shown in the figure below.
#
#
#
#PROBLEM 5.1 - DECISION-MAKER TRADEOFFS  (1/1 point)
#What is the cost associated with the model in Step 1 making a false negative prediction?
#
# A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3.  A paper will be mistakenly added to Set A, definitely affecting the quality of the results of Step 
# 
#>>>>3.  A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3.
# 
# A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3. - correct  There is no cost associated with a false negative prediction.
# SHOW ANSWER  You have used 1 of 1 submissions
#PROBLEM 5.2 - DECISION-MAKER TRADEOFFS  (1/1 point)
#What is the cost associated with the model in Step 1 making a false positive prediction?
#
#>>>>A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3. 
#
#A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3. - correct  A paper will be mistakenly added to Set A, definitely affecting the quality of the results of Step 3.  A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3.  There is no cost associated with a false positive prediction.
#
#PROBLEM 5.3 - DECISION-MAKER TRADEOFFS  (1/1 point)
#Given the costs associated with false positives and false negatives, which of the following is most accurate?
#
# A false positive is more costly than a false negative; the decision maker should use a probability threshold greater than 0.5 for the machine learning model.  A false positive is more costly than a false negative; the decision maker should use a probability threshold less than 0.5 for the machine learning model.  A false negative is more costly than a false positive; the decision maker should use a probability threshold greater than 0.5 for the machine learning model.  
# 
# >>>>A false negative is more costly than a false positive; the decision maker should use a probability threshold less than 0.5 for the machine learning model. 



