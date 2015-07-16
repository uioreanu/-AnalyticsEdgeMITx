#
# MITx: 15.071x The Analytics Edge
#

# VISUALIZING NETWORK DATA
# 
# The cliche goes that the world is an increasingly interconnected place, and the connections between different entities are often best represented with a graph. Graphs are comprised of vertices (also often called "nodes") and edges connecting those nodes. In this assignment, we will learn how to visualize networks using the igraph package in R.
# 
# For this assignment, we will visualize social networking data using anonymized data from Facebook; this data was originally curated in a recent paper about computing social circles in social networks. In our visualizations, the vertices in our network will represent Facebook users and the edges will represent these users being Facebook friends with each other.
# 
# The first file we will use, edges.csv, contains variables V1 and V2, which label the endpoints of edges in our network. Each row represents a pair of users in our graph who are Facebook friends. For a pair of friends A and B, edges.csv will only contain a single row -- the smaller identifier will be listed first in this row. From this row, we will know that A is friends with B and B is friends with A.
# 
# The second file, users.csv, contains information about the Facebook users, who are the vertices in our network. This file contains the following variables:
#   
#   id: A unique identifier for this user; this is the value that appears in the rows of edges.csv
# 
# gender: An identifier for the gender of a user taking the values A and B. Because the data is anonymized, we don't know which value refers to males and which value refers to females.
# 
# school: An identifier for the school the user attended taking the values A and AB (users with AB attended school A as well as another school B). Because the data is anonymized, we don't know the schools represented by A and B.
# 
# locale: An identifier for the locale of the user taking the values A and B. Because the data is anonymized, we don't know which value refers to what locale.

# cleanup & setup
rm(list=ls())
gc()
if (getwd()=="C:/Dokumente und Einstellungen/cu/Eigene Dateien") setwd('data_R/AnalyticsEdge/Unit7 Visualization/')


edges = read.csv('edges.csv')
users = read.csv('users.csv')

dim(edges)
dim(users)

#In our dataset, what is the average number of friends per user? Hint: this question is tricky, and it might help to start by thinking about a small example with two users who are friends.
(nrow(edges)*2)/nrow(users)

#Out of all the students who listed a school, what was the most common locale?
table(users$locale, users$school)
# 
# A AB
# 3  0  0
# A  6  0  0
# B 31 17  2

# visualize networks via the igraph package
install.packages('igraph')
library(igraph)

#?graph.data.frame
g = graph.data.frame(edges, FALSE, users) g = graph.data.frame(edges, FALSE, users) - correct

# plot the network
plot(g, vertex.size=5, vertex.label=NA)

#the "degree" of a node is its number of friends. 
barplot(sort(degree(g)))

(sort(degree(g)))

# the average number of friends per user, again
mean(degree(g) )

# To visually draw attention to these nodes, we will change the size of the vertices so the vertices with high degrees are larger. To do this, we will change the "size" attribute of the vertices of our graph to be an increasing function of their degrees:
  
V(g)$size = degree(g)/2+2

#replot the network
plot(g, vertex.label=NA)

# change the color scheme according to gender
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

#replot the network
plot(g, vertex.label=NA)

# change the color scheme according to school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"

#replot the network
plot(g, vertex.label=NA)

# change the color scheme according to locale
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"

#replot the network
plot(g, vertex.label=NA)

#?igraph.plotting



