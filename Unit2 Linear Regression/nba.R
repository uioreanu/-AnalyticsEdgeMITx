# 
#	The Analytics Edge
#	Unit2 Calin Uioreanu
#	NBA
#

#In this recitation, we'll apply some of the ideas from Moneyball to data from the National Basketball Association (NBA). Please download the datasets NBA_train.csv and NBA_test.csv, and save them to a location on your computer that you will remember. This data comes from Basketball-Reference.com.

NBA<-read.csv('NBA_train.csv');
dim(NBA);

nba_test<-read.csv('NBA_test.csv');
dim(nba_test);

attach(NBA); 

(cor(nba_train[3:20]))

table(W, Playoffs);
boxplot(NBA$W~NBA$Playoffs)

NBA$PTSdiff<-NBA$PTS-NBA$oppPTS
hist(NBA$PTSdiff)
plot(W, NBA$PTSdiff)


lm.NBA<-lm(PTS~., data=NBA[3:20]);summary(lm.NBA);

summary(PointsReg5<-lm(PTS~., data=NBA[9:20]));

SSE<-sum(PointsReg$residuals^2);

summary(PointsReg<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK, data=NBA));
SSE<-sum(PointsReg$residuals^2);
RMSE<-sqrt(SSE/nrow(NBA))
RMSE

summary(PointsReg2<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+STL+BLK, data=NBA))
summary(PointsReg3<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL+BLK, data=NBA))
summary(PointsReg4<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL, data=NBA))

(SSE4<-sum(PointsReg4$residuals^2))
(RMSE4<-sqrt(SSE4/nrow(NBA)))
#184.493

SSE<-sum((PointsPredict-nba_test$PTS)^2)
SST<-sum((mean(NBA$PTS)-nba_test$PTS)^2)
RMSE<-sqrt(SSE/nrow(nba_test))
