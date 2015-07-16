############################
#
#	Analytics Edge MIT Course
#	Unit2 Linear Regression
#
#	STATE DATA 
#	By : Calin Uioreanu
#
############################

# source('flu.R', print.eval=T)

if (getwd()=='C:/Users/calin/Documents') {
  setwd('../TEST/AnalyticsEdgeMIT/')
}
pauseScript<-1

data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

(str(statedata))
cat ("Read State Data\n")
if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}

cat ("Plot all of the states' centers with latitude on the y axis 
(the y variable in our dataset) and longitude on the x axis 
(the x variable in our dataset). 
The shape of the plot should look like the outline of the United States! 
Note that Alaska and Hawaii have had their coordinates adjusted to 
appear just off of the west coast.")

plot(statedata$x, statedata$y)
if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}


tapply(statedata$HS.Grad, statedata$state.region, mean)
cat ("
Using the tapply command, determine which region of the US 
(West, North Central, South, or Northeast) has the highest 
average high school graduation rate of all the states in the region:

Northeast         South North Central          West 
     53.96667      44.34375      54.51667      62.00000 
")
if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}

boxplot(Murder~state.region, statedata)
cat ("
Now, make a boxplot of the murder rate by region 
(for more information about creating boxplots in R, 
type ?boxplot in your console).

")
if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}

# outlier murder rate
which.max(subset(statedata, state.region=="Northeast")$Murder)

# a model to predict life expectancy by state using the state statistics.
fit <- lm(Life.Exp~Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)

# What is the coefficient for "Income" in your linear regression model?
summary(fit)
# -0.00002180	

# Now plot a graph of life expectancy
plot(statedata$Income, statedata$Life.Exp)

# The model we built does not display the relationship we saw from the plot 
# of life expectancy vs. income. Which of the following explanations seems 
# the most reasonable?
# Multicollinearity 

cat( "REFINING THE MODEL AND ANALYZING PREDICTIONS

Recall that we discussed the principle of simplicity: 
that is, a model with fewer variables is preferable to a model 
with many unnnecessary variables. Experiment with 
removing independent variables from the original model. 
Remember to use the significance of the coefficients 
to decide which variables to remove (remove the one with 
the largest p-value first, or the one with the t value 
closest to zero), and to remove them one at a time 
(this is called backwards variable selection). 
This is important due to multicollinearity issues - 
removing one insignificant variable may make another 
previously insignificant variable become significant.
")

if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}
cat(" Fit2 without Age ")
summary(fit2)
fit2 <- lm(Life.Exp~Population + Income + Illiteracy + Murder + HS.Grad + Frost, data=statedata)

cat(" Fit3 without Age and Illiteracy ")
summary(fit2)
fit3 <- lm(Life.Exp~Population + Income + Murder + HS.Grad + Frost, data=statedata)

cat(" Fit4 without Age, Illiteracy, Income ")
summary(fit2)
fit4 <- lm(Life.Exp~Population + Murder + HS.Grad + Frost, data=statedata)

cat("

Which state do we predict to have the lowest life expectancy?
Which state actually has the lowest life expectancy? 

")
if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}
(sort(predict(fit4))[1])
(statedata[which.min(statedata$Life.Exp),])


cat("

Which state do we predict to have the highest life expectancy?
Which state actually has the highest life expectancy?


")
#"backwards variable selection"
if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}
(rev(sort(predict(fit4)))[1])
(statedata[which.max(statedata$Life.Exp),])


cat("

Take a look at the vector of residuals 
(the difference between the predicted and actual values).

For which state do we make the smallest absolute error?
For which state do we make the largest absolute error?

")

(sort(abs(fit4$residuals)))