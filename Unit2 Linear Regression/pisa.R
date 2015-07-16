# 
#	The Analytics Edge
#	Unit2 Calin Uioreanu
#	PISA Study
#

pisaTrain<-read.csv('pisa2009train.csv');
str(pisaTrain);

pisaTest <-read.csv('pisa2009test.csv');
dim(pisaTest);

tapply(pisaTrain$readingScore, pisaTrain$male, mean, na.rm=T)
# 483.5325

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

dim(pisaTrain); dim(pisaTest);
#[1] 2414   24
#[1] 990  24

#PROBLEM 2.2 - UNORDERED FACTORS IN REGRESSION MODELS
sort(table(pisaTrain$raceeth))

#PROBLEM 3.1 - BUILDING A MODEL

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmPisa<-lm(readingScore~., data=pisaTrain);

#PROBLEM 3.2 - COMPUTING THE ROOT-MEAN SQUARED ERROR OF THE MODEL
predictPisa <- predict(lmPisa, pisaTest)
SSE<-sum(lmPisa$residuals^2)
RMSE<-sqrt(SSE/nrow(pisaTrain))
RMSE


SSE<-sum((predictPisa-pisaTest$readingScore)^2)
SST<-sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2)
SSE/SST
