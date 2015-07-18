# 
#	The Analytics Edge
#	Unit6 Clustering
#   PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT
#	Calin Uioreanu
#
# VISUALIZING ATTRIBUTES OF PAROLE VIOLATORS (OPTIONAL)
# 
# In the crime lecture, we saw how we can use heatmaps to give a 2-dimensional representation of 3-dimensional data: we made heatmaps of crime counts by time of the day and day of the week. In this problem, we'll learn how to use histograms to show counts by one variable, and then how to visualize 3 dimensions by creating multiple histograms.
# 
# We'll use the parole data parole.csv from Unit 3. Before, we used this data to predict parole violators. Now, let's try to get a little more insight into this dataset using histograms. As a reminder, the variables in this dataset are:
# 
# male = 1 if the parolee is male, 0 if female
# race = 1 if the parolee is white, 2 otherwise
# 
# age = the parolee's age in years at the time of release from prison
# 
# state = a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. These three states were selected due to having a high representation in the dataset.
# 
# time.served = the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# 
# max.sentence = the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# 
# multiple.offenses = 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# 
# crime = a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# 
# violator = 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.

setwd('G:/data_R/-AnalyticsEdgeMITx/Unit7 Visualization/')
parole = read.csv('parole.csv')
str(parole)

parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# What fraction of parole violators are female?
prop.table(table(parole$male, parole$violator),2)

# In this dataset, which crime is the most common in Kentucky?
table(parole$state, parole$crime)
# 1   2   3   4
# 1  66   9  34  34
# 2  42  10  64   4
# 3  42  15  20   5
# 4 165  72  35  58

# Create a histogram to find out the distribution of the age of parolees, by typing the following command in your R console (you might need to load the ggplot2 package first by typing library(ggplot2) in your R console):
ggplot(data = parole, aes(x = age)) + geom_histogram()

#By default, geom_histogram divides the data into 30 bins. Change the width of the bins to 5 years by adding the argument "binwidth = 5" to geom_histogram.
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)

#Redo the histogram, adding the following argument to the geom_histogram function: color="blue". What does this do? Select all that apply.
ggplot(data = parole, aes(x = age)) + 
      geom_histogram(color="blue", binwidth = 5)

#how the age distribution of male parolees compares to the age distribution of female parolees.
#One option would be to create a heatmap with age on one axis and male (a binary variable in our data set) on the other axis. Another option would be to stick with histograms, but to create a separate histogram for each gender. ggplot has the ability to do this automatically using the facet_grid command.

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

#Puts the histograms side-by-side instead of on top of each other.
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)

# ADDING ANOTHER DIMENSION  
# An alternative to faceting is to simply color the different groups differently. To color the data points by group, we need to tell ggplot that a property of the data (male or not male) should be translated to an aesthetic property of the histogram. We can do this by setting the fill parameter within the aesthetic to male.
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)

#Coloring the groups differently is a good way to see the breakdown of age by sex within the single, aggregated histogram. However, the bars here are stacked, meaning that the height of the orange bars in each age bin represents the total number of parolees in that age bin, not just the number of parolees in that group.

# An alternative to a single, stacked histogram is to create two histograms and overlay them on top of each other. This is a simple adjustment to our previous command.
# We just need to:
#   1) Tell ggplot not to stack the histograms by adding the argument position="identity" to the geom_histogram function.
#   2) Make the bars semi-transparent so we can see both colors by adding the argument alpha=0.5 to the geom_histogram function.
ggplot(data = parole, aes(x = age, fill = male)) + 
      geom_histogram(binwidth = 5, position="identity", alpha=0.5) + 
      scale_fill_manual(values=colorPalette)

# the amount of time served by parolees
ggplot(data = parole, aes(x = time.served)) + geom_histogram(bin=1)

# changing the bin to 0.1 months
ggplot(data = parole, aes(x = time.served)) + geom_histogram(bin=0.1)

# use facet_grid to create a separate histogram of time.served for each value of the variable crime
ggplot(data = parole, aes(x = time.served)) + geom_histogram(bin=1) + facet_grid(. ~ crime)

# Which crime type has no observations where time served is less than one month?
# For which crime does the frequency of 5-6 month prison terms exceed the frequencies of each other term length?
ggplot(data = parole, aes(x = time.served)) + geom_histogram(bin=1) + facet_grid(. ~ crime)

#  instead of faceting the histograms, overlay them
ggplot(data=parole, aes(x=time.served, fill=crime)) + geom_histogram(binwidth=0.1, position="identity", alpha=0.5)


