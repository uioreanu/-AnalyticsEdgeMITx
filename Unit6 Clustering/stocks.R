# 
#	The Analytics Edge
#	Unit6 Clustering
#   PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT
#	Calin Uioreanu
#

#In the second lecture sequence this week, we heard about cluster-then-predict, a methodology in which you first cluster observations and then build cluster-specific prediction models. In the lecture sequence, we saw how this methodology helped improve the prediction of heart attack risk. In this assignment, we'll use cluster-then-predict to predict future stock prices using historical stock data.
#
#When selecting which stocks to invest in, investors seek to obtain good future returns. In this problem, we will first use clustering to identify clusters of stocks that have similar returns over time. Then, we'll use logistic regression to predict whether or not the stocks will have positive future returns.
#
#For this problem, we'll use StocksCluster.csv, which contains monthly stock returns from the NASDAQ stock exchange. The NASDAQ is the second-largest stock exchange in the world, and it lists many technology companies. The stock price data used in this problem was obtained from infochimps, a website providing access to many datasets.
#
#Each observation in the dataset is the monthly returns of a particular company in a particular year. The years included are 2000-2009. The companies are limited to tickers that were listed on the exchange for the entire period 2000-2009, and whose stock price never fell below $1. So, for example, one observation is for Yahoo in 2000, and another observation is for Yahoo in 2001. Our goal will be to predict whether or not the stock return in December will be positive, using the stock returns for the first 11 months of the year.
#
#This dataset contains the following variables:
#
#ReturnJan = the return for the company's stock during January (in the year of the observation). 
#ReturnFeb = the return for the company's stock during February (in the year of the observation). 
#ReturnMar = the return for the company's stock during March (in the year of the observation). 
#ReturnApr = the return for the company's stock during April (in the year of the observation). 
#ReturnMay = the return for the company's stock during May (in the year of the observation). 
#ReturnJune = the return for the company's stock during June (in the year of the observation). 
#ReturnJuly = the return for the company's stock during July (in the year of the observation). 
#ReturnAug = the return for the company's stock during August (in the year of the observation). 
#ReturnSep = the return for the company's stock during September (in the year of the observation). 
#ReturnOct = the return for the company's stock during October (in the year of the observation). 
#ReturnNov = the return for the company's stock during November (in the year of the observation). 
#PositiveDec = whether or not the company's stock had a positive return in December (in the year of the observation). This variable takes value 1 if the return was positive, and value 0 if the return was not positive.
#For the first 11 variables, the value stored is a proportional change in stock value during that month. For instance, a value of 0.05 means the stock increased in value 5% during the month, while a value of -0.02 means the stock decreased in value 2% during the month.


rm(list=ls())
gc()

stocks = read.csv('StocksCluster.csv');

str(stocks)
#'data.frame':   11580 obs. of  12 variables:
# $ ReturnJan  : num  0.0807 -0.0107 0.0477 -0.074 -0.031 ...
# $ ReturnFeb  : num  0.0663 0.1021 0.036 -0.0482 -0.2127 ...
# $ ReturnMar  : num  0.0329 0.1455 0.0397 0.0182 0.0915 ...
# $ ReturnApr  : num  0.1831 -0.0844 -0.1624 -0.0247 0.1893 ...
# $ ReturnMay  : num  0.13033 -0.3273 -0.14743 -0.00604 -0.15385 ...
# $ ReturnJune : num  -0.0176 -0.3593 0.0486 -0.0253 -0.1061 ...
# $ ReturnJuly : num  -0.0205 -0.0253 -0.1354 -0.094 0.3553 ...
# $ ReturnAug  : num  0.0247 0.2113 0.0334 0.0953 0.0568 ...
# $ ReturnSep  : num  -0.0204 -0.58 0 0.0567 0.0336 ...
# $ ReturnOct  : num  -0.1733 -0.2671 0.0917 -0.0963 0.0363 ...
# $ ReturnNov  : num  -0.0254 -0.1512 -0.0596 -0.0405 -0.0853 ...
# $ PositiveDec: int  0 0 0 1 1 1 1 0 0 0 ...

boxplot(stocks)

prop.table(table(stocks$PositiveDec))
#       0        1 
#0.453886 0.546114 

#What is the maximum correlation between any two return variables in the dataset? 
cor(stocks)

              ReturnJan  ReturnFeb   ReturnMar   ReturnApr   ReturnMay ReturnJune   ReturnJuly    ReturnAug    ReturnSep  ReturnOct  ReturnNov PositiveDec 
ReturnJan    1.00000000  0.0667746 -0.09049680 -0.03767801 -0.04441142  0.0922383 -0.081429765 -0.022792019 -0.026437153  0.1429772  0.0676323  0.00472852 
ReturnFeb    0.06677458  1.0000000 -0.15598326 -0.19135192 -0.09552092  0.1699945 -0.061778509  0.131559786  0.043501771 -0.0873243 -0.1546583 -0.03817318 
ReturnMar   -0.09049680 -0.1559833  1.00000000  0.00972629 -0.00389279 -0.0859055  0.003374160 -0.022005400  0.076518327 -0.0119238  0.0373235  0.02240866 
ReturnApr   -0.03767801 -0.1913519  0.00972629  1.00000000  0.06382250 -0.0110278  0.080631932 -0.051756051 -0.028920972  0.0485400  0.0317618  0.09435353 
ReturnMay   -0.04441142 -0.0955209 -0.00389279  0.06382250  1.00000000 -0.0210745  0.090850264 -0.033125658  0.021962862  0.0171667  0.0480466  0.05820193 
ReturnJune   0.09223831  0.1699945 -0.08590549 -0.01102775 -0.02107454  1.0000000 -0.029152600  0.010710526  0.044747269 -0.0226360 -0.0652705  0.02340975 
ReturnJuly  -0.08142976 -0.0617785  0.00337416  0.08063193  0.09085026 -0.0291526  1.000000000  0.000713756  0.068947804 -0.0547089 -0.0483738  0.07436421 
ReturnAug   -0.02279202  0.1315598 -0.02200540 -0.05175605 -0.03312566  0.0107105  0.000713756  1.000000000  0.000740714 -0.0755946 -0.1164890  0.00416697 
ReturnSep   -0.02643715  0.0435018  0.07651833 -0.02892097  0.02196286  0.0447473  0.068947804  0.000740714  1.000000000 -0.0580792 -0.0197198  0.04163029 
ReturnOct    0.14297723 -0.0873243 -0.01192376  0.04854003  0.01716673 -0.0226360 -0.054708909 -0.075594561 -0.058079236  1.0000000  0.1916728 -0.05257496 
ReturnNov    0.06763233 -0.1546583  0.03732353  0.03176184  0.04804659 -0.0652705 -0.048373837 -0.116489034 -0.019719800  0.1916728  1.0000000 -0.06234656 
PositiveDec  0.00472852 -0.0381732  0.02240866  0.09435353  0.05820193  0.0234097  0.074364210  0.004166966  0.041630286 -0.0525750 -0.0623466  1.00000000 
																																                                      
# ReturnNov + ReturnOct																																                                      


#Which month (from January through November) has the largest mean return across all observations in the dataset?
colMeans(stocks)
#  ReturnJan   ReturnFeb   ReturnMar   ReturnApr   ReturnMay  ReturnJune  ReturnJuly   ReturnAug   ReturnSep   ReturnOct   ReturnNov PositiveDec 
# 0.01263160 -0.00760478  0.01940234  0.02630815  0.02473659  0.00593790  0.00305086  0.01619826 -0.01472077  0.00565084  0.01138744  0.54611399 

#INITIAL LOGISTIC REGRESSION MODEL

#  split the data into a training set and testing set, 70% in the training set and 30% in the testing set
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

# train a logistic regression model 
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)

# overall accuracy on the training set, using a threshold of 0.5
table(stocksTrain$PositiveDec, predict(StocksModel, type="response")>0.5)
#    FALSE TRUE
#  0   990 2689
#  1   787 3640
(3640+990)/nrow(stocksTrain)
# 0.5711818 -> not great

# overall accuracy on the testing set, using a threshold of 0.5
table(stocksTest$PositiveDec, predict(StocksModel, newdata=stocksTest, type="response")>0.5)
#    FALSE TRUE
#  0   417 1160
#  1   344 1553
(1553+417)/nrow(stocksTest)
# 0.5670697

# accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)
1897/nrow(stocksTest)
#[1] 0.5460564


#CLUSTERING STOCKS
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown to us at the time of prediction. Therefore, if we need to know the outcome value to perform the clustering, the methodology is no longer useful for prediction of an unknown outcome value.
#
#This is an important point that is sometimes mistakenly overlooked. If you use the outcome value to cluster, you might conclude your method strongly outperforms a non-clustering alternative. However, this is because it is using the outcome to determine the clusters, which is not valid.


#preProcess command from the caret package, which normalizes variables by subtracting by the mean and dividing by the standard deviation.
library(caret)

preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)


colMeans(normTrain)
#    ReturnJan     ReturnFeb     ReturnMar     ReturnApr     ReturnMay    ReturnJune    ReturnJuly     ReturnAug     ReturnSep 
# 1.330682e-17 -1.007214e-17 -8.431792e-18 -1.460048e-19 -1.000259e-18 -7.332770e-18  3.549057e-18  2.078051e-17 -6.781814e-18 
#    ReturnOct     ReturnNov 
#-5.161583e-18 -6.497723e-18 
colMeans(normTest)
#    ReturnJan     ReturnFeb     ReturnMar     ReturnApr     ReturnMay    ReturnJune    ReturnJuly     ReturnAug     ReturnSep 
#-0.0004185886 -0.0038621679  0.0058299150 -0.0363806373  0.0265120925  0.0431544402  0.0060164183 -0.0497332436  0.0293887872 
#    ReturnOct     ReturnNov 
# 0.0296723768  0.0171281833 

# Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
summary(normTrain$ReturnJan)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-4.57700 -0.48270 -0.07055  0.00000  0.35900 18.06000 
summary(normTest$ReturnJan)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-3.744000 -0.485700 -0.066860 -0.000419  0.357700  8.413000 
#From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan), we see that the average return in January is slightly higher in the training set than in the testing set. Since normTest was constructed by subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan is slightly negative in normTest.

# CLUSTERING STOCKS
k = 3
set.seed(144)
km = kmeans(normTrain, centers = k)

table(km$cluster)
#   1    2    3 
#3157 4696  253 

# we can use the flexclust package to obtain training set and testing set cluster assignments for our observations 

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTrain)
#clusterTrain
#   1    2    3 
#3157 4696  253 

table(clusterTest)
#clusterTest
#   1    2    3 
#1298 2080   96 

# Using the subset function, build data frames stocksTrain1, stocksTrain2, and stocksTrain3, containing the elements in the stocksTrain data frame assigned to clusters 1, 2, and 3, respectively (be careful to take subsets of stocksTrain, not of normTrain). Similarly build stocksTest1, stocksTest2, and stocksTest3 from the stocksTest data frame.

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

mean(stocksTrain1$PositiveDec)
#[1] 0.6024707
mean(stocksTrain2$PositiveDec)
#[1] 0.5140545
mean(stocksTrain3$PositiveDec)
#[1] 0.4387352

#CLUSTER-SPECIFIC PREDICTIONS
#Build logistic regression models StocksModel1, StocksModel2, and StocksModel3, which predict PositiveDec using all the other variables as independent variables. StocksModel1 should be trained on stocksTrain1, StocksModel2 should be trained on stocksTrain2, and StocksModel3 should be trained on stocksTrain3.
#
#Which variables have a positive sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3 and a negative sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3? Select all that apply.

StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")

coefficients(StocksModel1)
#(Intercept)   ReturnJan   ReturnFeb   ReturnMar   ReturnApr   ReturnMay  ReturnJune  ReturnJuly   ReturnAug   ReturnSep   ReturnOct   ReturnNov 
# 0.17223985  0.02498357 -0.37207369  0.59554957  1.19047752  0.30420906 -0.01165375  0.19769226  0.51272941  0.58832685 -1.02253506 -0.74847186 
coefficients(StocksModel2)
#(Intercept)   ReturnJan   ReturnFeb   ReturnMar   ReturnApr   ReturnMay  ReturnJune  ReturnJuly   ReturnAug   ReturnSep   ReturnOct   ReturnNov 
#  0.1029318   0.8845148   0.3176221  -0.3797811   0.4929105   0.8965492   1.5008787   0.7831487  -0.2448602   0.7368522  -0.2775631  -0.7874737 
coefficients(StocksModel3)
# (Intercept)    ReturnJan    ReturnFeb    ReturnMar    ReturnApr    ReturnMay   ReturnJune   ReturnJuly    ReturnAug    ReturnSep    ReturnOct    ReturnNov 
#-0.181895809 -0.009789345 -0.046883260  0.674179495  1.281466189  0.762511555  0.329433917  0.774164370  0.982605385  0.363806823  0.782242086 -0.873752144 

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")>0.5
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")>0.5
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")>0.5

table(stocksTest1$PositiveDec, PredictTest1)
table(stocksTest2$PositiveDec, PredictTest2)
table(stocksTest3$PositiveDec, PredictTest3)

table(stocksTest1$PositiveDec, predict(StocksModel1, newdata=stocksTest1, type="response")>0.5)
#   
#    FALSE TRUE
#  0    30  471
#  1    23  774
(30+774)/nrow(stocksTest1)
#0.6194145

table(stocksTest2$PositiveDec, predict(StocksModel2, newdata=stocksTest2, type="response")>0.5)
#   
#    FALSE TRUE
#  0   388  626
#  1   309  757
(388+757)/nrow(stocksTest2)
#0.5504808
table(stocksTest3$PositiveDec, predict(StocksModel3, newdata=stocksTest3, type="response")>0.5)
#   
#    FALSE TRUE
#  0    49   13
#  1    21   13
(49+13)/nrow(stocksTest3)
# 0.6458333

#To compute the overall test-set accuracy of the cluster-then-predict approach, we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

#What is the overall test-set accuracy of the cluster-then-predict approach, again using a threshold of 0.5?
table(AllPredictions, AllOutcomes)

#			  AllOutcomes
#AllPredictions    0    1
#         FALSE  467  353
#         TRUE  1110 1544
#> (467+1544)/(467+353+1110+1544)
#[1] 0.5788716

#We see a modest improvement over the original logistic regression model. Since predicting stock returns is a notoriously hard problem, this is a good increase in accuracy. By investing in stocks for which we are more confident that they will have positive returns (by selecting the ones with higher predicted probabilities), this cluster-then-predict model can give us an edge over the original logistic regression model.

