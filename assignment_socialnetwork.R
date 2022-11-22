install.packages("ggfortify")
install.packages("cluster")

library("cluster")
library("ggplot2")
library("dplyr")
library("ggfortify")

summary(iris)
head(iris)
data <- select(iris, c(1:4))

kmean <- kmeans(data, 3)
kmean$centers
kmean$cluster
autoplot(kmean, data, frame = TRUE)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(data)

kmean <- kmeans(data, 2)
kmean$centers
autoplot(kmean, data, frame=TRUE)

####Ozioma#####
install.packages("readr")
library(readr)
socialnetwork <- read.csv("socialnetwork.csv")
View(socialnetwork)

summary(socialnetwork)
head(socialnetwork)
kmean <- kmeans(mydata, centers=2)
kmean$centers
kmean$cluster
autoplot(kmean, mydata, frame = TRUE)

wssplot <- function(mydata, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(mydata)
##Scatterplot of dataset
ggplot(socialnetwork, aes(x=Age,y=Educational_level))+
  geom_point(aes(color=Name),size=5)

mydata <- socialnetwork[,2:3]
#C1_Andrew <- c(55,1.0)
#C2_Carolina <- c(37,5.0)

#kmean <- kmeans(mydata, centers= c(C1_Andrew,C2_Carolina),iter.max = 1)
kmean$centers
kmean$cluster
autoplot(kmean, mydata, frame = TRUE)



###onlin###
socialnetwork <- read.csv("socialnetwork.csv")
mydata <- socialnetwork[,2:3]

set.seed(240) # Setting seed
kmeans.re <- kmeans(mydata, centers = 2, nstart = 20)
kmeans.re

kmeans.re$cluster
