# 
#	The Analytics Edge
#	Unit6 Clustering
#   DOCUMENT CLUSTERING WITH DAILY KOS
#	Calin Uioreanu
#


#Document clustering, or text clustering, is a very popular application of clustering algorithms. A web search engine, like Google, often returns thousands of results for a simple query. For example, if you type the search term "jaguar" into Google, around 200 million results are returned. This makes it very difficult to browse or find relevant information, especially if the search term has multiple meanings. If we search for "jaguar", we might be looking for information about the animal, the car, or the Jacksonville Jaguars football team. 
#
#Clustering methods can be used to automatically group search results into categories, making it easier to find relavent results. This method is used in the search engines PolyMeta and Helioid, as well as on FirstGov.gov, the official Web portal for the U.S. government. The two most common algorithms used for document clustering are Hierarchical and k-means. 
#
#In this problem, we'll be clustering articles published on Daily Kos, an American political blog that publishes news and opinion articles written from a progressive point of view. Daily Kos was founded by Markos Moulitsas in 2002, and as of September 2014, the site had an average weekday traffic of hundreds of thousands of visits. 
#
#The file dailykos.csv contains data on 3,430 news articles or blogs that have been posted on Daily Kos. These articles were posted in 2004, leading up to the United States Presidential Election. The leading candidates were incumbent President George W. Bush (republican) and John Kerry (democratic). Foreign policy was a dominant topic of the election, specifically, the 2003 invasion of Iraq. 
#
#Each of the variables in the dataset is a word that has appeared in at least 50 different articles (1,545 words in total). The set of  words has been trimmed according to some of the techniques covered in the previous week on text analytics (punctuation has been removed, and stop words have been removed). For each document, the variable values are the number of times that word appeared in the document. 

# read data
dailykos  <- read.csv('dailykos.csv');

# HIERARCHICAL CLUSTERING  

# Compute distances
kosDist = dist(dailykos , method = "euclidean")

# hierarchical clustering  
kosHierClust = hclust(kosDist, method="ward.D")

# Plot the dendrogram
plot(kosHierClust)

# good choices for the number of clusters:
# The choices 2 and 3 are good cluster choices according to the dendrogram, because there is a lot of space between the horizontal lines in the dendrogram in those cut off spots (draw a horizontal line across the dendrogram where it crosses 2 or 3 vertical lines). The choices of 5 and 6 do not seem good according to the dendrogram because there is very little space.

# split data into 7 clusters
kosGroups = cutree(kosHierClust, k = 7)

# counts in each cluster
table(kosGroups)
#kosGroups
#   1    2    3    4    5    6    7 
#1266  321  374  139  407  714  209 


kosCluster1 = subset(dailykos, kosGroups == 1)
kosCluster2 = subset(dailykos, kosGroups == 2)
kosCluster3 = subset(dailykos, kosGroups == 3)
kosCluster4 = subset(dailykos, kosGroups == 4)
kosCluster5 = subset(dailykos, kosGroups == 5)
kosCluster6 = subset(dailykos, kosGroups == 6)
kosCluster7 = subset(dailykos, kosGroups == 7)

# using split
kosClusters = split(dailykos, kosGroups)

# view top words in each cluster
for(i in 1:7) print(tail(sort(colSums(kosClusters[[i]]))))

#     state republican       poll   democrat      kerry       bush 
#       959        961       1144       1164       1345       2159 
#     bush  democrat challenge      vote      poll  november 
#      914       915      1315      1412      1556      3319 
#     elect    parties      state republican   democrat       bush 
#       616        623        868        944       1430       1648 
#campaign    voter presided     poll     bush    kerry 
#     199      214      226      499     1089     1173 
#      american       presided administration            war           iraq           bush 
#           444            456            501            723            988           1604 
#    race     bush    kerry    elect democrat     poll 
#     327      349      369      382      403      415 
#democrat    clark   edward     poll    kerry     dean 
#     449      522      545      578      826     1213

# K-MEANS CLUSTERING
set.seed(1000);
KMC = kmeans(dailykos, centers = k)

table(KMC$cluster)
#   1    2    3    4    5    6    7 
# 146  144  277 2063  163  329  308 

kosKmeanCluster1 = subset(dailykos, KMC$cluster == 1)
kosKmeanCluster2 = subset(dailykos, KMC$cluster == 2)
kosKmeanCluster3 = subset(dailykos, KMC$cluster == 3)
kosKmeanCluster4 = subset(dailykos, KMC$cluster == 4)
kosKmeanCluster5 = subset(dailykos, KMC$cluster == 5)
kosKmeanCluster6 = subset(dailykos, KMC$cluster == 6)
kosKmeanCluster7 = subset(dailykos, KMC$cluster == 7)

# using split
kosKClusters = split(dailykos, KMC$cluster)

# CTRL+L to clear the console, then:

# top words by cluster for hierarchical
print("HIERARCHICAL CLUSTERING")
for(i in 1:7) print(tail(sort(colMeans(kosClusters[[i]]))))

# top words by cluster for K-means
print("K-MEANS CLUSTERING")
for (i in 1:k) print(tail(sort(colMeans(kosKClusters[[i]]))))

#PROBLEM 2.5 - K-MEANS CLUSTERING
#Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
#
# Hierarchical Cluster 1  Hierarchical Cluster 2  Hierarchical Cluster 3  Hierarchical Cluster 4  Hierarchical Cluster 5  Hierarchical Cluster 6  Hierarchical Cluster 7  No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7. No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7. - correct


table(kosGroups, KMC$cluster)
         
#kosGroups    1    2    3    4    5    6    7
#        1    3   11   64 1045   32    0  111
#        2    0    0    0    0    0  320    1
#        3   85   10   42   79  126    8   24
#        4   10    5    0    0    1    0  123
#        5   48    0  171  145    3    1   39
#        6    0    2    0  712    0    0    0
#        7    0  116    0   82    1    0   10



















