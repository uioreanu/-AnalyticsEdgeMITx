############################
#
#	Analytics Edge MIT Course
#	Unit2 Linear Regression
#
#	FORECASTING ELANTRA SALES
#	By : Calin Uioreanu
#
############################

# source('elantra.R', print.eval=T)

if (getwd()=='C:/Users/calin/Documents') {
  setwd('../TEST/AnalyticsEdgeMIT/')
}
pauseScript<-1

#url<-'http://courses.edx.org/c4x/MITx/15.071x_2/asset/elantra.csv'
url<-'elantra.csv'

# load dataset
ds<-read.csv(url)
str(ds)

cat ("
Load the data set. Split the data set into training and testing 
sets as follows: place all observations for 2012 and earlier in 
the training set, and all observations for 2013 and 2014 into the testing set.
")

train<-subset(ds, Year<=2012)
test<-subset(ds, Year>2012)
dim(train); dim(test)

# pause
if (pauseScript==1) line <- readline()


cat ("
Build a linear regression model to predict monthly Elantra sales 
using Unemployment, CPI_all, CPI_energy and Queries as the 
independent variables. Use all of the training set data to do this.

What is the model R-squared? Note: In this problem, we will 
always be asking for the Multiple R-Squared of the model.
")

fit<-lm(ElantraSales~Unemployment + Queries + CPI_energy + CPI_all, data=train)

cat ("
Multiple R-squared:  0.4282,    Adjusted R-squared:  0.3544
")

# pause
if (pauseScript==1) line <- readline()

cat ("
MODELING SEASONALITY  

To incorporate the seasonal effect due to the month, 
build a new linear regression model that predicts monthly Elantra 
sales using Month as well as Unemployment, CPI_all, CPI_energy and Queries. 
Do not modify the training and testing data frames before building the model.

What is the model R-Squared?
")

fit2<-lm(ElantraSales~Unemployment + Queries + CPI_energy + CPI_all + Month, data=train)

# pause
if (pauseScript==1) line <- readline()

summary(fit2)
cat ("


Which of the following best describes the effect of adding Month?


The model is not better because the adjusted R-squared has 
gone down and none of the variables (including the new one) 
are very significant. 

the adjusted R-Squared is the R-Squared but adjusted to take 
into account the number of variables. If the adjusted R-Squared 
is lower, then this indicates that our model is not better and 
in fact may be worse. Furthermore, if none of the variables have 
become significant, then this also indicates that the 
model is not better.

")



fit3<-lm(ElantraSales~Unemployment + Queries + CPI_energy + CPI_all + as.factor(Month), data=train)

summary(fit3)
cat ("

You may be experiencing an uneasy feeling that there is something 
not quite right in how we have modeled the effect of the calendar 
month on the monthly sales of Elantras. If so, you are right. 
In particular, we added Month as a variable, but Month is an 
ordinary numeric variable. In fact, we must convert Month to 
a factor variable before adding it to the model.

Re-run the regression with the Month variable modeled as a factor variable. (Create a new variable that models the Month as a factor (using the as.factor function) instead of overwriting the current Month variable. We'll still use the numeric version of Month later in the problem.)

What is the model R-Squared?

# Multiple R-squared:  0.8193,    Adjusted R-squared:  0.6837
")

# pause
if (pauseScript==1) line <- readline()
# investigate the correlation matrix
cor(train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

cat ("

Simplifying the linear model, removing Queries

Multiple R-squared:  0.818,     Adjusted R-squared:  0.6967 

further reductions bring no improvement to the R-squared
")
# 
fit4<-lm(ElantraSales~Unemployment + CPI_energy + CPI_all + as.factor(Month), data=train)

# pause
if (pauseScript==1) line <- readline()

cat ("

make predictions on the test set. 
What is the sum of squared errors 
of the model on the test set?

baseline prediction: 14460

")

pred4 <- predict(fit4, newdata=test)
SSE<-sum((pred4-test$ElantraSales)^2)
SST<-sum((mean(train$ElantraSales)-test$ElantraSales)^2)
SST

# pause
if (pauseScript==1) line <- readline()

cat ("

Test set R-squared

You can compute the SST as the sum of the squared differences 
between ElantraSales in the testing set and the mean of 
ElantraSales in the training set:

SST = sum((mean(ElantraTrain$ElantraSales) - ElantraTest$ElantraSales)^2)

Then, using the SSE you computed previously, the R-squared 
is 1 minus the SSE divided by the SST.

RMSE<-1-(SSE/SST)
# 0.7280232

")

RMSE<-1-(SSE/SST)

# pause
if (pauseScript==1) line <- readline()


cat ("

What is the largest absolute error that we make in our test set predictions?

summary(abs(pred4-test$ElantraSales))
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  394.2   727.4  2216.0  2851.0  4067.0  7491.0 

7491.0 

")

summary(abs(pred4-test$ElantraSales))

cat ("

In which period (Month,Year pair) 
do we make the largest absolute error in our prediction?

")


test[which.max(abs(pred4-test$ElantraSales)),]