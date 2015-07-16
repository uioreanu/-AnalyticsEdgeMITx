############################
#
#	Analytics Edge MIT Course
#	Unit3 Logistic Regression
#
#	PREDICTING PAROLE VIOLATORS
#	By : Calin Uioreanu
#
############################

# source('parole.R', print.eval=T)

# preps
rm(list=ls())
gc()

# read data
parole<-read.csv('parole.csv')

# Which variables in this dataset are unordered factors with at least three levels?
str(parole)
table(parole$state)
table(parole$crime)

# factorize these variables
parole$state<-as.factor(parole$state)
parole$crime<-as.factor(parole$crime)

# output of summary identical to table
summary(parole$state)

# split data into train/test
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

dim(train); dim(test); dim(parole)
#[1] 473   9
#[1] 202   9
#[1] 675   9

# building a regression model
model.glm<-glm(violator~., data=train, family="binomial")

summary(model.glm);
#(Intercept)       -4.2411574  1.2938852  -3.278  0.00105 ** 
#male               0.3869904  0.4379613   0.884  0.37690    
#race               0.8867192  0.3950660   2.244  0.02480 *  
#age               -0.0001756  0.0160852  -0.011  0.99129    
#state2             0.4433007  0.4816619   0.920  0.35739    
#state3             0.8349797  0.5562704   1.501  0.13335    
#state4            -3.3967878  0.6115860  -5.554 2.79e-08 ***
#time.served       -0.1238867  0.1204230  -1.029  0.30359    
#max.sentence       0.0802954  0.0553747   1.450  0.14705    
#multiple.offenses  1.6119919  0.3853050   4.184 2.87e-05 ***
#crime2             0.6837143  0.5003550   1.366  0.17180    
#crime3            -0.2781054  0.4328356  -0.643  0.52054    
#crime4            -0.0117627  0.5713035  -0.021  0.98357    


# What can we say based on the coefficient of the multiple.offenses variable?
# 2) If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for a unit increase in the variable.
# Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses but is otherwise identical. 
exp(1)^1.6119919

#Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny.
test2<-data.frame('male'=1, 'race'=1, 'age'=50, 'state'=as.factor(1), 'time.served'=3, 'max.sentence'=12, 'multiple.offenses'=0, 'crime'=as.factor(2))

#According to the model, what are the odds this individual is a violator?
exp(1)^(-4.2411574) * 1*exp(1)^0.3869904 * 1*exp(1)^0.8867192 *
50*exp(1)^(-0.0001756) * 3*exp(1)^(-0.1238867) * 12*exp(1)^0.0802954 
# From the logistic regression equation, we have log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4. This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0. We conclude that log(odds) = -1.700629.

#Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted probability of violation is 1/(1+exp(1.700629)) = 0.154.

#According to the model, what is the probability this individual is a violator?
predict(model.glm, test2, type="response")
#       1 
#0.154383 

# test data now, What is the maximum predicted probability of a violation?
max(predict(model.glm, newdata=test, type="response"))
#[1] 0.9072791


table(test$violator, predict(model.glm, newdata=test, type="response")>0.5)
#FALSE TRUE
#  0   167   12
#  1    11   12
#What is the model's sensitivity?
12/(11+12)

#What is the model's specificity?
167/(167+12)

#What is the model's accuracy?
(167+12)/nrow(test)


#Consider a parole board using the model to predict whether parolees will be violators or not. The job of a parole board is to make sure that a prisoner is ready to be released into free society, and therefore parole boards tend to be particularily concerned with releasing prisoners who will violate their parole. Which of the following most likely describes their preferences and best course of action?
#The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5.

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predict(model.glm, newdata=test, type="response"), test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
#[1] 0.848969
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, main="regression glm", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Describe the meaning of AUC in this context.
# The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.



#Our goal has been to predict the outcome of a parole decision, and we used a publicly available dataset of parole releases for predictions. In this final problem, we'll evaluate a potential source of bias associated with our analysis. It is always important to evaluate a dataset for possible sources of bias.

#The dataset contains all individuals released from parole in 2004, either due to completing their parole term or violating the terms of their parole. However, it does not contain parolees who neither violated their parole nor completed their term in 2004, causing non-violators to be underrepresented. This is called "selection bias" or "selecting on the dependent variable," because only a subset of all relevant parolees were included in our analysis, based on our dependent variable in this analysis (parole violation). How could we improve our dataset to best address selection bias?

#There is no way to address this form of biasing. 

#We should use the current dataset, expanded to include the missing parolees. Each added parolee should be labeled with violator=0, because they have not yet had a violation. 

#We should use the current dataset, expanded to include the missing parolees. Each added parolee should be labeled with violator=NA, because the true outcome has not been observed for these individuals. 

#We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term. We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term. - correct

