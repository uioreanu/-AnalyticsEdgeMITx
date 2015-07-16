# 
#	The Analytics Edge
#	Unit2 Calin Uioreanu
#	Baseball players
#

#QUICK QUESTION  (1/1 point)
#Suppose you are the General Manager of a baseball team, and you are selecting TWO players for your team. You have a budget of $1,500,000, and you have the choice between the following players:




baseball<-read.csv('baseball.csv');
moneyball<-subset(baseball, Year<="2002")
attach(moneyball);

plot(jitter(RA), jitter(RS));
plot(jitter(RS-RA), jitter(W), pch=20, cex=0.5)

lm.RA<-lm(RA~OOBP+OSLG, data=moneyball)
lm.RS<-lm(RS~OBP+SLG)

lm.RS<-lm(RS~OBP+SLG, data=moneyball)
summary(lm.RS)

test<-data.frame('player'=c('Chavez', 'Giambi', 'Menechino', 'Myers', 'Pena'), 'OBP'=c(0.338, 0.391,0.369,0.313,0.361), 'SLG'=c(0.540,0.450,0.374,0.447,0.500))
test

predict(lm.RS, test);


# In 2012 and 2013, there were 10 teams in the MLB playoffs: the six teams that had the most wins in each baseball division, and four "wild card" teams. The playoffs start between the four wild card teams - the two teams that win proceed in the playoffs (8 teams remaining). Then, these teams are paired off and play a series of games. The four teams that win are then paired and play to determine who will play in the World Series. 

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012<-c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013<-c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
#[1] 0.3477129
cor(teamRank, wins2013)
#[1] -0.6556945
 


