# Load our data:
murders = read.csv("murders.csv", stringsAsFactors=FALSE)

# description
str(murders)

# fetch states coordinates from R
statesMap = map_data('state')
#names(statesMap)

# plot US map
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", col="black")

# create matching lower cased state
murders$region = tolower(murders$State)

# join data frames based on region
murderMap = merge(statesMap, murders, by="region")

# summarize combined data
str(murderMap)

# plot US map combined with murders
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(col="black")

# heatmap US map murders
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(col="black") + scale_fill_gradient(low="black", high="red", guide="legend")

# heatmap US map population
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon(col="black") + scale_fill_gradient(low="black", high="red", guide="legend")

# heatmap US map murder rate (murders/population)
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders/Population)) + geom_polygon(col="black") + scale_fill_gradient(low="black", high="red", guide="legend")

# create murder-rate variable (needed for limit)
murderMap$MurderRate = murderMap$Murders/murderMap$Population

# remove Washington DC outlier
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(col="black") + scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0,100))

