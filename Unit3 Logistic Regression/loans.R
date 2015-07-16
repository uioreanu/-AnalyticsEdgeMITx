############################
#
#	Analytics Edge MIT Course
#	Unit3 Logistic Regression
#
#	PREDICTING LOAN REPAYMENT
#	By : Calin Uioreanu
#
############################

# source('loan.R', print.eval=T)

# preps
rm(list=ls())
gc()

# read data
loans<-read.csv('loans.csv')

str(loans)
#'data.frame':   9578 obs. of  14 variables:
# $ credit.policy    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ purpose          : Factor w/ 7 levels "all_other","credit_card",..: 3 2 3 3 2 2 3 1 5 3 ...
# $ int.rate         : num  0.119 0.107 0.136 0.101 0.143 ...
# $ installment      : num  829 228 367 162 103 ...
# $ log.annual.inc   : num  11.4 11.1 10.4 11.4 11.3 ...
# $ dti              : num  19.5 14.3 11.6 8.1 15 ...
# $ fico             : int  737 707 682 712 667 727 667 722 682 707 ...
# $ days.with.cr.line: num  5640 2760 4710 2700 4066 ...
# $ revol.bal        : int  28854 33623 3511 33667 4740 50807 3839 24220 69909 5630 ...
# $ revol.util       : num  52.1 76.7 25.6 73.2 39.5 51 76.8 68.6 51.1 23 ...
# $ inq.last.6mths   : int  0 0 1 1 0 0 0 0 1 1 ...
# $ delinq.2yrs      : int  0 0 0 0 1 0 0 0 0 0 ...
# $ pub.rec          : int  0 0 0 0 0 0 1 0 0 0 ...
# $ not.fully.paid   : int  0 0 0 0 0 0 1 1 0 0 ...

# What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.
prop.table(table(loans$not.fully.paid))

# Which of the variables have at least one missing observation?
sapply(loans, function(x) sum(is.na(x)))

# missing values filled with multiple imputation
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# split data into train/test
library(caTools)
set.seed(144)
subset<-sample.split(loans$not.fully.paid, SplitRatio=0.70)
train<-loans[subset==T,]
test<-loans[subset==F,]

dim(loans); dim(train); dim(test);
#[1] 9578   14
#[1] 6705   14
#[1] 2873   14

# build logistic regression model on the trainset
model.glm<-glm(not.fully.paid~., data=train, family="binomial")

# evaluate the model
summary(model.glm)

# evaluate FICO
#(700*-9.406e-03)-(710*-9.406e-03)

# evaluate model strength on the test set
predicted.risk<-predict(model.glm, newdata=test, type="response")
test$predicted.risk<-predicted.risk

# build confusion matrix
table(test$not.fully.paid, test$predicted.risk>0.5)
#    FALSE TRUE
#  0  2400   13
#  1   457    3

# What is the accuracy of the logistic regression model? 
(2400+3)/nrow(test)
# 0.8364079

#What is the accuracy of the baseline model?
# everybody pays is the baseline model
table(test$not.fully.paid)
#   0    1 
#2413  460 
2413/nrow(test)
# [1] 0.8398886

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predict(model.glm, newdata=test, type="response"), test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)
# 0.6721337
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, main="regression glm", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# A "SMART BASELINE"
# a bivariate logistic regression model (aka a logistic regression model with a single independent variable) that predicts the dependent variable not.fully.paid using only the variable int.rate.

model.glm2<-glm(not.fully.paid~int.rate, data=train, family="binomial")
library(ROCR)
ROCRpred = prediction(predict(model.glm2, newdata=test, type="response"), test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)
# 0.6239081


#Make test set predictions for the bivariate model. What is the highest predicted probability of a loan not being paid in full on the testing set?
max(predict(model.glm2, newdata=test, type="response"))

#To compute interest revenue, consider a $c investment in a loan that has an annual interest rate r over a period of t years. Using continuous compounding of interest, this investment pays back c * exp(rt) dollars by the end of the t years, where exp(rt) is e raised to the r*t power.
#How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest? Hint: remember to convert the percentage to a proportion before doing the math. Enter the number of dollars, without the $ sign.

10*exp(0.06*3)
#[1] 11.97217



#In order to evaluate the quality of an investment strategy, we need to compute this profit for each loan in the test set. For this variable, we will assume a $1 investment (aka c=1). To create the variable, we first assign to the profit for a fully paid loan, exp(rt)-1, to every observation, and we then replace this value with -1 in the cases where the loan was not paid in full. All the loans in our dataset are 3-year loans, meaning t=3 in our calculations. Enter the following commands in your R console to create this new variable:

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

#What is the maximum profit of a $10 investment in any loan in the testing set (do not include the $ sign in your answer)?
10*max(test$profit)
#[1] 8.894769
plot(test$profit)

#A simple investment strategy of equally investing in all the loans would yield profit $20.94 for a $100 investment. But this simple investment strategy does not leverage the prediction model we built earlier in this problem. As stated earlier, investors seek loans that balance reward with risk, in that they simultaneously have high interest rates and a low risk of not being paid back.
#To meet this objective, we will analyze an investment strategy in which the investor only purchases loans with a high interest rate (a rate of at least 15%), but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. We will model an investor who invests $1 in each of the most promising 100 loans.
#First, use the subset() function to build a data frame called highInterest consisting of the test set loans with an interest rate of at least 15%.
highInterest<-subset(test, int.rate>0.15)
str(highInterest)
#'data.frame':   437 obs. of  16 variables:

#What is the average profit of a $1 investment in one of these high-interest loans (do not include the $ sign in your answer)?
sum(highInterest$profit)
#[1] 98.36937

prop.table(table(highInterest$not.fully.paid))
#        0         1 
#0.7482838 0.2517162 


#Next, we will determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks in increasing order and selecting the 100th element of this sorted list. Find the highest predicted risk that we will include by typing the following command into your R console:
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

#Use the subset() function to build a data frame called selectedLoans consisting of the high-interest loans with predicted risk not exceeding the cutoff we just computed. Check to make sure you have selected 100 loans for investment.
selectedLoans <- subset(highInterest, predicted.risk<cutoff)
dim(selectedLoans)

#What is the profit of the investor, who invested $1 in each of these 100 loans (do not include the $ sign in your answer)?
sum(selectedLoans$profit)
#[1] 32.24048

prop.table(table(selectedLoans$not.fully.paid))
#        0         1 
#0.8181818 0.1818182 

#We have now seen how analytics can be used to select a subset of the high-interest loans that were paid back at only a slightly lower rate than average, resulting in a significant increase in the profit from our investor's $100 investment. Although the logistic regression models developed in this problem did not have large AUC values, we see that they still provided the edge needed to improve the profitability of an investment portfolio.

#We conclude with a note of warning. Throughout this analysis we assume that the loans we invest in will perform in the same way as the loans we used to train our model, even though our training set covers a relatively short period of time. If there is an economic shock like a large financial downturn, default rates might be significantly higher than those observed in the training set and we might end up losing money instead of profiting. Investors must pay careful attention to such risk when making investment decisions.
