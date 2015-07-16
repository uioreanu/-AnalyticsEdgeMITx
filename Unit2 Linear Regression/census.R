# 
#	The Analytics Edge
#	Unit1 Calin Uioreanu
#	Census data
#

CPS<-read.csv('CPSData.csv');
table(State, is.na(MetroAreaCode))

plot(prop.table(table(State, is.na(MetroAreaCode)),1))
MetroAreaMap<-read.csv('MetroAreaCodes.csv');

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

sum(is.na(CPS$MetroArea))
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean, na.rm=T));
tapply(CPS$Race=="Asian", CPS$MetroArea, mean, na.rm=T);

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T))

CountryMap<-read.csv('CountryCodes.csv');
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

prop.table(table(CPS$MetroArea=='New York-Northern New Jersey-Long Island, NY-NJ-PA', CPS$Country!='United States'),1)

table(CPS$MetroArea, CPS$Country=='Somalia')
# or with tapply:
sort(tapply(CPS$Country=='Somalia', CPS$MetroArea, sum, na.rm=T))

