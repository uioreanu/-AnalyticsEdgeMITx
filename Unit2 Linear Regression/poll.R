# 
#	The Analytics Edge
#	Unit1 Calin Uioreanu
#	Internet Privacy poll
#


poll<-read.csv('AnonymityPoll.csv'); attach(poll); str(poll);

table(Smartphone);


table(State);
table(State, Region=="South");


sort(tapply(Region=="South", State, sum))

table(Internet.Use, Smartphone)


limited<-subset(poll, Internet.Use==1 | Smartphone==1); str(limited);

table(limited$Info.On.Internet);

#  0   1   2   3   4   5   6   7   8   9  10  11 
#105  84  95 101 104  94  67  63  40  18  13   8 
table(limited$Worry.About.Info);

#  0   1 
#404 386 


prop.table(table(limited$Tried.Masking.Identity))


max(table(limited$Age, limited$Info.On.Internet))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet), pch=20)


tapply(limited$Info.On.Internet, limited$Smartphone, mean, na.rm=T)


