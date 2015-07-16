############################
#
#	Analytics Edge MIT Course
#	Unit2 Linear Regression
#
#	DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA 
#	By : Calin Uioreanu
#
############################

# source('flu.R', print.eval=T)

if (getwd()=='C:/Users/calin/Documents') {
  setwd('../TEST/AnalyticsEdgeMIT/')
}

list.of.packages <- c("zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# read train data from HTTP
#url<-'http://courses.edx.org/c4x/MITx/15.071x_2/asset/FluTrain.csv'
url<-'FluTrain.csv'
# Load "FluTrain.csv" into a data frame called FluTrain
FluTrain<-read.csv(url)
str(FluTrain)

#url<-'http://courses.edx.org/c4x/MITx/15.071x_2/asset/FluTest.csv'
url<-'FluTest.csv'
# Load "FluTrain.csv" into a data frame called FluTrain
FluTest<-read.csv(url)

cat ("Train/Test data read from url\n")
cat ("Press [enter] to continue\n")
line <- readline()


#"Week" - The range of dates represented by this observation, in year/month/day format.
#"ILI" - This column lists the percentage of ILI-related physician visits for the corresponding week.
#"Queries" - This column lists the fraction of queries that are ILI-related for the corresponding week, adjusted to be between 0 and 1 (higher values correspond to more ILI-related search queries).

# exploration graph
plot(FluTrain, main="explore the training set FluMain")

line <- readline()

# 2nd exploration, week by ILI
plot(FluTrain$Week, FluTrain$ILI, type="l", main="week by ILI")

# which week corresponds to the highest percentage of ILI-related physician visits?
FluTrain[which.max(FluTrain$ILI),]$Week
#  2009-10-18 - 2009-10-24

# Which week corresponds to the highest percentage of ILI-related query fraction?
plot(FluTrain$Week, FluTrain$Queries, type="l", main="Which week corresponds to the highest percentage of ILI-related query fraction?\n")
# [1] 2009-10-18 - 2009-10-24
FluTrain[which.max(FluTrain$Queries),]$Week
# so the queries matched the physician visits
line <- readline()

# What best describes the distribution of values of ILI?
hist(FluTrain$ILI, breaks=100)
# right skewed
cat ("What best describes the distribution of values of ILI?\n")
line <- readline()


# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?.
plot(FluTrain$Queries, log(FluTrain$ILI), main="Plot the natural logarithm of ILI versus Queries. What does the plot suggest?\n")

cat ("Building linear regression model\n")
line <- readline()

# Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?
FluTrend1<-lm(log(ILI) ~ Queries, data=FluTrain)

# plot the linear model
abline(FluTrend1, col="green", lwd=3)

# fetch the R2 of the train data
summary(FluTrend1)
# Multiple R-squared:  0.709,     Adjusted R-squared:  0.7083 

cat ("For a single variable linear regression model, 
there is a direct relationship between the R-squared and the correlation 
between the independent and the dependent variables. \n")

(cor(log(FluTrain$ILI), FluTrain$Queries)^2)

line <- readline()
# 0.7090201

cat ("applying the linear model on test data, result as inverse of log() <-> exp() \n")
line <- readline()

# fetched the predicted data
PredictTest<-exp(predict(FluTrend1, newdata=FluTest))
Result <- data.frame("week"=FluTest$Week, "ILI"=PredictTest)

#What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? 
cat ("What is our estimate for the percentage of ILI-related physician 
visits for the week of March 11, 2012? \n")
Result[which(Result$week=="2012-03-11 - 2012-03-17"),]

cat ("Estimated ILI: 2.187378 , real ILI: 2.293422\n")
line <- readline()

WeekId<-which(Result$week=="2012-03-11 - 2012-03-17")
cat ("What is the relative error betweeen the estimate (our prediction) and 
the observed value for the week of March 11, 2012? Note that the relative error 
is calculated as

(Observed ILI - Estimated ILI)/Observed ILI")

(1-(PredictTest[WeekId]/FluTest[WeekId,]$ILI))
line <- readline()

# RMSE
cat ("Calculating the Root Mean Square Error (RMSE) between our estimates 
and the actual observations for the 
percentage of ILI-related physician visits, on the test set\n")
SSE<-sum((PredictTest-FluTest$ILI)^2)
RMSE<-sqrt(SSE/nrow(FluTest))
cat(paste("SSE=", SSE, "RMSE=", RMSE))
line <- readline()

# PROBLEM 4.1 - TRAINING A TIME SERIES MODEL  (1 point possible)

library(zoo)

cat ("After installing and loading the zoo package, run the following commands 
to create the ILILag2 variable in the training set:\n");

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

cat ("How many values are missing in the new ILILag2 variable?\n")
(summary(FluTrain))
line <- readline()

cat ("Use the plot() function to plot the log of ILILag2 against the log of ILI. 
Which best describes the relationship between these two variables?\n")
plot(log(FluTrain$ILI), log(FluTrain$ILILag2))
line <- readline()

cat ("Train a linear regression model on the FluTrain dataset to predict 
the log of the ILI variable using the Queries variable as well as the log 
of the ILILag2 variable. Call this model FluTrend2.\n")
FluTrend2<-lm(log(ILI)~Queries + log(ILILag2), data=FluTrain)
abline(FluTrend2, col="red", lwd=2)
(summary(FluTrend2))

cat ("On the basis of R-squared value and significance of coefficients, 
which statement is the most accurate?\n")
(summary(FluTrend1))

cat ("Modify the code from the previous subproblem to add an 
ILILag2 variable to the FluTest data frame. How many missing values 
are there in this new variable?
\n")

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
(summary(FluTest))


tail(FluTrain); head(FluTest)
cat ("
					   Week      ILI   Queries  ILILag2
412 2011-11-20 - 2011-11-26 1.655415 0.4130146 1.440892
413 2011-11-27 - 2011-12-03 1.465723 0.4780876 1.462212
414 2011-12-04 - 2011-12-10 1.518106 0.4648074 1.655415
415 2011-12-11 - 2011-12-17 1.663954 0.4794157 1.465723
416 2011-12-18 - 2011-12-24 1.852736 0.5378486 1.518106
417 2011-12-25 - 2011-12-31 2.124130 0.6188579 1.663954
                     Week      ILI   Queries  ILILag2
1 2012-01-01 - 2012-01-07 1.766707 0.5936255       NA	# 1.852736
2 2012-01-08 - 2012-01-14 1.543401 0.4993360       NA	# 2.124130
3 2012-01-15 - 2012-01-21 1.647615 0.5006640 1.766707
4 2012-01-22 - 2012-01-28 1.684297 0.4794157 1.543401
5 2012-01-29 - 2012-02-04 1.863542 0.4714475 1.647615
6 2012-02-05 - 2012-02-11 1.864079 0.5033201 1.684297
\n");

FluTest$ILILag2[1]<-FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2]<-FluTrain$ILI[nrow(FluTrain)]

cat("
EVALUATING THE TIME SERIES MODEL IN THE TEST SET
Obtain test set predictions of the ILI variable from 
the FluTrend2 model, again remembering to call the exp() 
function on the result of the predict() function to obtain 
predictions for ILI instead of log(ILI).

What is the test-set RMSE of the FluTrend2 model?")

#SSE<-sum(
# fetched the predicted data
PredictTest2<-exp(predict(FluTrend2, newdata=FluTest))
SSE2<-sum((PredictTest2-FluTest$ILI)^2)
RMSE2<-sqrt(SSE2/nrow(FluTest))

cat("Which model obtained the best test-set RMSE?
")
cat(paste("RMSE=", RMSE, "RMSE2=", RMSE2, "\n"))