# 
#	The Analytics Edge
#	Unit1 Calin Uioreanu
#	STOCK DYNAMICS
#



vars<-c("IBM", "GE", "ProcterGamble", "CocaCola","Boeing");

for (i in 1:length(vars)) {
	fileName <- paste0(vars[i], "Stock.csv");
	print(paste("Reading ", fileName))
	fileData <-read.csv(fileName)
	assign(vars[i], fileData)
}
print("Loaded data:")
str(IBM)
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
print("Parsed Date field:")
str(IBM)

print("Max Date:")
max(IBM$Date, GE$Date, ProcterGamble$Date, CocaCola$Date, Boeing$Date);

print("Plotting:")
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red");
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue", lty=2)
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=2, col="green")

print("Plotting time range:")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
col<- c("blue", "green", "purple", "red", "orange");

legend("topleft", col=col, lwd=c(1, 1.5, 2),legend=vars)
abline(v=as.Date(c("1997-10-01")), lwd=1, col="cyan")

# mean prices by month
tapply(IBM$StockPrice, months(IBM$Date), mean)

rev(sort(tapply(GE$StockPrice, months(GE$Date), mean)))
rev(sort(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)))


