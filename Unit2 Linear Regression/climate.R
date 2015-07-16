# 
#	The Analytics Edge
#	Unit2 Calin Uioreanu
#	Climate Change
#


# There have been many studies documenting that the average global temperature has been increasing over the last century. The consequences of a continued rise in global temperature will be dire. Rising sea levels and an increased frequency of extreme weather events will affect billions of people.

climate<-read.csv('climate_change.csv');
str(climate);

climate.train<-subset(climate, Year<=2006); climate.test<-subset(climate, Year>2006); 
dim(climate.train); dim(climate.test);

lm.climate<-lm(Temp~., data=climate[3:10]);
summary(lm.climate);

# removing CH4
lm.climate<-lm(Temp~MEI+CO2+N2O+CFC.11+CFC.12+TSI+Aerosols, data=climate);

# PROBLEM 1.2 - CREATING OUR FIRST MODEL  (1/1 point)
# Typo: p<0.01 not p<0.05 (N2O)

lm.climate1<-lm(Temp~MEI+N2O+TSI+Aerosols, data=climate.train);	

# R provides a function, step, that will automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R2
lm.climate0<-step(lm.climate);


predict.climate<-predict(lm.climate0, climate.test)

SSE<-sum((predict.climate-climate.test$Temp)^2)
SSE
#[1] 0.2023186

SST<-sum((mean(climate.train$Temp)-climate.test$Temp)^2)
SST
# 0.26751

RMSE<-SSE/SST
#0.7563031

