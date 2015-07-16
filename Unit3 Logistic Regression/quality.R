############################
#
#	Analytics Edge MIT Course
#	Unit3 Logistic Regression
#
#	Quality of healthcare
#	By : Calin Uioreanu
#
############################

# source('quality.R', print.eval=T)

if (getwd()=='C:/Users/calin/Documents' | getwd()=='C:/Dokumente und Einstellungen/cu/Eigene Dateien') {
  setwd('data_R/AnalyticsEdge/Unit3 Logistic Regression')
}
pauseScript<-1

dat <- read.csv('quality.csv');
str(dat)
cat ("Read source data\n")
if (pauseScript==1) {
	cat ("Press [enter] to continue\n")
	line <- readline()
}
