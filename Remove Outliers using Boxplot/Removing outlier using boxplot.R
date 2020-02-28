car<- cars
View(car)

# multiplying values higher than 90 by 2 to show as an outlier
car$dist[which(car$dist >90)] <- c(car$dist[which(car$dist >90)]*2)

summary(car)

# box plot that shows the outliers
boxplot(car$dist)

# You can get the actual values of the outliers with this
boxplot(car$dist)$out


# Now you can assign the outlier values into a vector, 
# plot= false if you dont want to see the plot again

outliers <- boxplot(car$dis, plot=FALSE)$out

print(outliers)

# To find in which rows the outliers are

car[which(car$dist %in% outliers),]


# Now you can remove the rows containing the outliers:

Newcar <- car[-which(car$dist %in% outliers),]


boxplot(Newcar$dist)
