############################
#
#	Analytics Edge MIT Course
#	Unit3 Logistic Regression
#
#	POPULARITY OF MUSIC RECORDS
#	By : Calin Uioreanu
#
############################

# source('songs.R', print.eval=T)

# preps
rm(list=ls())
gc()

# fetching data
songs<-read.csv('songs.csv')

#######################
# a bit of data exploration
str(songs)
summary (songs) # No NAs, wow!
barplot(table(songs$year))

(songs[songs$artistname=='Michael Jackson'&songs$Top10==1,][1:4])
#     year         songtitle      artistname             songID
#4329 2001 You Rock My World Michael Jackson SOBLCOF13134393021
#6207 1995 You Are Not Alone Michael Jackson SOJKNNO13737CEB162
#6210 1995    Black or White Michael Jackson SOBBRFO137756C9CB7
#6218 1995 Remember the Time Michael Jackson SOIQZMT136C9704DA5
#6915 1992     In The Closet Michael Jackson SOKIOOC12AF729ED9E

# timesignature distribution
sort(unique(songs$timesignature))
table(songs$timesignature) # or hist()

# which song has the highest tempo
songs[which.max(songs$tempo),]

#######################
# building the regression model

# data split
train<-subset(songs, year<=2009)
test<-subset(songs, year>2009)
dim(train); dim(test)
#[1] 7201   39
#[1] 373  39

# remove categorical variables
# year is a complex topic, we'll come back to it later
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[ , !(names(train) %in% nonvars) ]
test = test[ , !(names(test) %in% nonvars) ]

# building logistic regression model
model.glm<-glm(Top10~., data=train, family="binomial")
summary(model.glm);

model.glm2 = glm(Top10 ~ . - loudness, data=train, family=binomial)
model.glm3 = glm(Top10 ~ . - energy, data=train, family=binomial)

# accuracy of the model on test dataset using confusion matrix
table(test$Top10, predict(model.glm3, newdata=test, type="response")>0.45)
#    FALSE TRUE
#  0   309    5
#  1    40   19
(309+19)/nrow(test)
#[1] 0.8793566

# baseline model: no songs makes it to the Top
314/nrow(test)
#[1] 0.8418231

# so a slight improvement to the baseline model

# calculate the sensitivity for 0.45 threshold
19/(40+19)
#[1] 0.3220339

# calculate the specificity of the model for 0.45 threshold
309/(309+5)
#[1] 0.9840764

# Model 3 has a very high specificity, meaning that it favors 
# specificity over sensitivity. While Model 3 only captures less 
# than half of the Top 10 songs, it still can offer a competitive edge, 
# since it is very conservative in its predictions.

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predict(model.glm3, newdata=test, type="response"), test$Top10)
as.numeric(performance(ROCRpred, "auc")@y.values)
#[1] 0.848969
ROCRperf = performance(ROCRpred, "tpr", "fpr")


# how does rf perform?
model.rf<-randomForest(Top10~., data=train, importance=T, ntree=300)
ROCRpred.rf = prediction(predict(model.rf, newdata=test), test$Top10)
as.numeric(performance(ROCRpred.rf, "auc")@y.values)
# [1] 0.8235453
ROCRperf.rf = performance(ROCRpred.rf, "tpr", "fpr")

# rf has a worse AUC and therefore less prediction power than the logistic regression model
#varImpPlot(model.rf)

# but how about a rf model without the multicolliniarity of energy-loudness?
model.rf3<-randomForest(Top10 ~ . - energy, data=train, importance=T, ntree=300)
ROCRpred.rf3 = prediction(predict(model.rf3, newdata=test), test$Top10)
as.numeric(performance(ROCRpred.rf3, "auc")@y.values)
# [1] 0.8244089
ROCRperf.rf3 = performance(ROCRpred.rf3, "tpr", "fpr")

par(mfrow=c(3,1))
# Add threshold labels 
plot(ROCRperf, main="regression glm", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf.rf, main="randomForest .", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf.rf3, main="rf . -energy", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# rf has a worse AUC and therefore less prediction power than the logistic regression model

