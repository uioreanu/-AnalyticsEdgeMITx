# 
#	The Analytics Edge
#	Unit6 Clustering
#   MARKET SEGMENTATION FOR AIRLINES
#	Calin Uioreanu
#

#Market segmentation is a strategy that divides a broad target market of customers into smaller, more similar groups, and then designs a marketing strategy specifically for each group. Clustering is a common technique for market segmentation since it automatically finds similar groups given a data set. 
#
#In this problem, we'll see how clustering can be used to find similar groups of customers who belong to an airline's frequent flyer program. The airline is trying to learn more about its customers so that it can target different customer segments with different types of mileage offers. 
#
#The file AirlinesCluster.csv contains information on 3,999 members of the frequent flyer program. This data comes from the textbook "Data Mining for Business Intelligence," by Galit Shmueli, Nitin R. Patel, and Peter C. Bruce. For more information, see the website for the book.
#
#There are seven different variables in the dataset, described below:
#
#Balance = number of miles eligible for award travel
#QualMiles = number of miles qualifying for TopFlight status
#BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
#BonusTrans = number of non-flight bonus transactions in the past 12 months
#FlightMiles = number of flight miles in the past 12 months
#FlightTrans = number of flight transactions in the past 12 months
#DaysSinceEnroll = number of days since enrolled in the frequent flyer program

rm(list=ls())
gc()

airlines = read.csv('AirlinesCluster.csv');

str(airlines)
#'data.frame':   3999 obs. of  7 variables:
# $ Balance        : int  28143 19244 41354 14776 97752 16420 84914 20856 443003 104860 ...
# $ QualMiles      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ BonusMiles     : int  174 215 4123 500 43300 0 27482 5250 1753 28426 ...
# $ BonusTrans     : int  1 2 4 1 26 0 25 4 43 28 ...
# $ FlightMiles    : int  0 0 0 0 2077 0 0 250 3850 1150 ...
# $ FlightTrans    : int  0 0 0 0 4 0 0 1 12 3 ...
# $ DaysSinceEnroll: int  7000 6968 7034 6952 6935 6942 6994 6938 6948 6931 ...

summary(airlines)
#    Balance          QualMiles         BonusMiles       BonusTrans    FlightMiles       FlightTrans     DaysSinceEnroll
# Min.   :      0   Min.   :    0.0   Min.   :     0   Min.   : 0.0   Min.   :    0.0   Min.   : 0.000   Min.   :   2   
# 1st Qu.:  18528   1st Qu.:    0.0   1st Qu.:  1250   1st Qu.: 3.0   1st Qu.:    0.0   1st Qu.: 0.000   1st Qu.:2330   
# Median :  43097   Median :    0.0   Median :  7171   Median :12.0   Median :    0.0   Median : 0.000   Median :4096   
# Mean   :  73601   Mean   :  144.1   Mean   : 17145   Mean   :11.6   Mean   :  460.1   Mean   : 1.374   Mean   :4119   
# 3rd Qu.:  92404   3rd Qu.:    0.0   3rd Qu.: 23801   3rd Qu.:17.0   3rd Qu.:  311.0   3rd Qu.: 1.000   3rd Qu.:5790   
# Max.   :1704838   Max.   :11148.0   Max.   :263685   Max.   :86.0   Max.   :30817.0   Max.   :53.000   Max.   :8296   

# Q: In this problem, we will normalize our data before we run the clustering algorithms. Why is it important to normalize the data before clustering? 
# A: If we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale. 

# Normalizing the data
library(caret)

# The first command pre-processes the data, and the second command performs the normalization. If you look at the summary of airlinesNorm, you should see that all of the variables now have mean zero. You can also see that each of the variables has standard deviation 1 by using the sd() function.
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)
#    Balance          QualMiles         BonusMiles        BonusTrans        FlightMiles       FlightTrans       DaysSinceEnroll   
# Min.   :-0.7303   Min.   :-0.1863   Min.   :-0.7099   Min.   :-1.20805   Min.   :-0.3286   Min.   :-0.36212   Min.   :-1.99336  
# 1st Qu.:-0.5465   1st Qu.:-0.1863   1st Qu.:-0.6581   1st Qu.:-0.89568   1st Qu.:-0.3286   1st Qu.:-0.36212   1st Qu.:-0.86607  
# Median :-0.3027   Median :-0.1863   Median :-0.4130   Median : 0.04145   Median :-0.3286   Median :-0.36212   Median :-0.01092  
# Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.00000  
# 3rd Qu.: 0.1866   3rd Qu.:-0.1863   3rd Qu.: 0.2756   3rd Qu.: 0.56208   3rd Qu.:-0.1065   3rd Qu.:-0.09849   3rd Qu.: 0.80960  
# Max.   :16.1868   Max.   :14.2231   Max.   :10.2083   Max.   : 7.74673   Max.   :21.6803   Max.   :13.61035   Max.   : 2.02284  

# HIERARCHICAL CLUSTERING  

# Compute distances
HierAirlines = dist(airlinesNorm , method = "euclidean")

# hierarchical clustering  
HierClust = hclust(HierAirlines, method="ward.D")

# Plot the dendrogram
plot(HierClust)

# Divide the data points into 5 clusters
# split data into 7 clusters
HierGroups = cutree(HierClust, k = 5)

# counts in each cluster
table(HierGroups)
#   1    2    3    4    5 
# 776  519  494  868 1342 

#Now, use tapply to compare the average values in each of the variables for the 5 clusters (the centroids of the clusters). You may want to compute the average values of the unnormalized data so that it is easier to interpret. You can do this for the variable "Balance" with the following command: tapply(airlines$Balance, HierGroups, mean)

summary(subset(airlines, HierGroups ==1))
#    Balance         QualMiles          BonusMiles      BonusTrans     FlightMiles       FlightTrans     DaysSinceEnroll
# Min.   :   148   Min.   :  0.0000   Min.   :    0   Min.   : 0.00   Min.   :   0.00   Min.   :0.0000   Min.   :2440   
# 1st Qu.: 21661   1st Qu.:  0.0000   1st Qu.: 2100   1st Qu.: 6.00   1st Qu.:   0.00   1st Qu.:0.0000   1st Qu.:5440   
# Median : 45426   Median :  0.0000   Median : 7322   Median :13.00   Median :   0.00   Median :0.0000   Median :6291   
# Mean   : 57867   Mean   :  0.6443   Mean   :10360   Mean   :10.82   Mean   :  83.18   Mean   :0.3028   Mean   :6235   
# 3rd Qu.: 84803   3rd Qu.:  0.0000   3rd Qu.:16710   3rd Qu.:15.00   3rd Qu.:   0.00   3rd Qu.:0.0000   3rd Qu.:7162   
# Max.   :238868   Max.   :500.0000   Max.   :49383   Max.   :31.00   Max.   :1600.00   Max.   :5.0000   Max.   :8296
summary(subset(airlines, HierGroups ==2))
#    Balance         QualMiles       BonusMiles       BonusTrans     FlightMiles     FlightTrans     DaysSinceEnroll
# Min.   :     0   Min.   :    0   Min.   :     0   Min.   : 0.00   Min.   :    0   Min.   : 0.000   Min.   :   2   
# 1st Qu.: 35223   1st Qu.:    0   1st Qu.:  5450   1st Qu.:10.00   1st Qu.:  750   1st Qu.: 2.000   1st Qu.:2586   
# Median : 77523   Median :    0   Median : 13300   Median :16.00   Median : 1950   Median : 5.000   Median :4546   
# Mean   :110669   Mean   : 1066   Mean   : 22882   Mean   :18.23   Mean   : 2613   Mean   : 7.403   Mean   :4402   
# 3rd Qu.:148256   3rd Qu.: 1565   3rd Qu.: 32217   3rd Qu.:24.50   3rd Qu.: 3398   3rd Qu.:10.000   3rd Qu.:6061   
# Max.   :737514   Max.   :11148   Max.   :153183   Max.   :86.00   Max.   :30817   Max.   :53.000   Max.   :8296   
summary(subset(airlines, HierGroups ==3))
#    Balance          QualMiles         BonusMiles       BonusTrans     FlightMiles      FlightTrans     DaysSinceEnroll
# Min.   :   3268   Min.   :   0.00   Min.   :     0   Min.   : 0.00   Min.   :   0.0   Min.   : 0.000   Min.   :1276   
# 1st Qu.:  83755   1st Qu.:   0.00   1st Qu.: 36703   1st Qu.:15.00   1st Qu.:   0.0   1st Qu.: 0.000   1st Qu.:4644   
# Median : 143341   Median :   0.00   Median : 50054   Median :18.00   Median :   0.0   Median : 0.000   Median :5712   
# Mean   : 198192   Mean   :  30.35   Mean   : 55796   Mean   :19.66   Mean   : 327.7   Mean   : 1.069   Mean   :5616   
# 3rd Qu.: 235531   3rd Qu.:   0.00   3rd Qu.: 67098   3rd Qu.:23.00   3rd Qu.: 500.0   3rd Qu.: 1.000   3rd Qu.:6646   
# Max.   :1704838   Max.   :2919.00   Max.   :263685   Max.   :53.00   Max.   :6393.0   Max.   :23.000   Max.   :8296   
summary(subset(airlines, HierGroups ==4))
#    Balance         QualMiles          BonusMiles      BonusTrans     FlightMiles      FlightTrans     DaysSinceEnroll
# Min.   :   329   Min.   :   0.000   Min.   :  454   Min.   : 4.00   Min.   :   0.0   Min.   :0.0000   Min.   : 248   
# 1st Qu.: 23847   1st Qu.:   0.000   1st Qu.: 7720   1st Qu.:13.00   1st Qu.:   0.0   1st Qu.:0.0000   1st Qu.:1816   
# Median : 44210   Median :   0.000   Median :16966   Median :15.00   Median :   0.0   Median :0.0000   Median :2672   
# Mean   : 52336   Mean   :   4.848   Mean   :20789   Mean   :17.09   Mean   : 111.6   Mean   :0.3445   Mean   :2841   
# 3rd Qu.: 71525   3rd Qu.:   0.000   3rd Qu.:29310   3rd Qu.:20.00   3rd Qu.:   0.0   3rd Qu.:0.0000   3rd Qu.:3698   
# Max.   :298927   Max.   :1490.000   Max.   :89360   Max.   :50.00   Max.   :3341.0   Max.   :6.0000   Max.   :7990   
summary(subset(airlines, HierGroups ==5))
#    Balance         QualMiles          BonusMiles      BonusTrans      FlightMiles      FlightTrans     DaysSinceEnroll
# Min.   :    50   Min.   :   0.000   Min.   :    0   Min.   : 0.000   Min.   :   0.0   Min.   :0.0000   Min.   :   8   
# 1st Qu.:  9508   1st Qu.:   0.000   1st Qu.:    0   1st Qu.: 0.000   1st Qu.:   0.0   1st Qu.:0.0000   1st Qu.:1694   
# Median : 20850   Median :   0.000   Median :  750   Median : 3.000   Median :   0.0   Median :0.0000   Median :2895   
# Mean   : 36256   Mean   :   2.511   Mean   : 2265   Mean   : 2.973   Mean   : 119.3   Mean   :0.4389   Mean   :3060   
# 3rd Qu.: 43217   3rd Qu.:   0.000   3rd Qu.: 2550   3rd Qu.: 5.000   3rd Qu.:   0.0   3rd Qu.:0.0000   3rd Qu.:4371   
# Max.   :282971   Max.   :1000.000   Max.   :24430   Max.   :18.000   Max.   :2000.0   Max.   :9.0000   Max.   :7521   


# the centroids of the clusters
lapply(split(airlines, HierGroups), colMeans)
#$`1`
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#   5.786690e+04    6.443299e-01    1.036012e+04    1.082345e+01    8.318428e+01    3.028351e-01    6.235365e+03 
#
#$`2`
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#   1.106693e+05    1.065983e+03    2.288176e+04    1.822929e+01    2.613418e+03    7.402697e+00    4.402414e+03 
#
#$`3`
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#   1.981916e+05    3.034615e+01    5.579586e+04    1.966397e+01    3.276761e+02    1.068826e+00    5.615709e+03 
#
#$`4`
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#   52335.913594        4.847926    20788.766129       17.087558      111.573733        0.344470     2840.822581 
#
#$`5`
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#   3.625591e+04    2.511177e+00    2.264788e+03    2.973174e+00    1.193219e+02    4.388972e-01    3.060081e+03 

m<-(lapply(split(airlines, HierGroups), colMeans))
for(i in 1:5) print(round(m[[i]],3))
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#      57866.905           0.644       10360.124          10.823          83.184           0.303        6235.365 
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#     110669.266        1065.983       22881.763          18.229        2613.418           7.403        4402.414 
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#     198191.575          30.346       55795.860          19.664         327.676           1.069        5615.709 
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#      52335.914           4.848       20788.766          17.088         111.574           0.344        2840.823 
#        Balance       QualMiles      BonusMiles      BonusTrans     FlightMiles     FlightTrans DaysSinceEnroll 
#      36255.910           2.511        2264.788           2.973         119.322           0.439        3060.081 


# K-MEANS CLUSTERING

set.seed(88);
kmeansClust = kmeans(airlinesNorm, centers = k, iter.max = 1000)

table(kmeansClust$cluster)
#   1    2    3    4    5 
# 408  141  993 1182 1275 

kmeansClust$centers
#    Balance  QualMiles BonusMiles BonusTrans FlightMiles FlightTrans DaysSinceEnroll
#1  1.444397  0.5111573   1.876928   1.033195    0.116995    0.144464        0.719804
#2  1.000541  0.6838223   0.614478   1.721489    3.855980    4.119614        0.274239
#3 -0.055806 -0.1410439   0.304136   0.710874   -0.121828   -0.128757       -0.339821
#4 -0.133317 -0.1149161  -0.349267  -0.337346   -0.183399   -0.196182        0.964092
#5 -0.405799 -0.0228108  -0.581648  -0.761905   -0.198960   -0.219658       -0.889775



















