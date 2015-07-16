#
# MITx: 15.071x The Analytics Edge
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

parole=read.csv('parole.csv')
str(parole)

parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

table(parole$male)
# 
# 0   1 
# 130 545 
