install.packages("plot3D")
install.packages("ggplot2")
install.packages("plotly")
install.packages("GGally")
install.packages("aplpack")
install.packages("corrplot")
install.packages("pheatmap")


library(plot3D)
library(ggplot2)
library(plotly)
library(GGally)
library(aplpack)
library(corrplot)
library(pheatmap)

nRec <- nrow(iris)
Sepal.Length <- iris[,1]
Sepal.Width <- iris[,2]
Petal.Length <- iris[,3]
Petal.Width <- iris[,4]
Species <- iris[,5]

View(iris)

#1
df <-  data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,Species)
ggplot(df,aes(x=Sepal.Length,y=Sepal.Width,size=Petal.Length))+geom_point()

#2
ggplot(df,aes(x=Sepal.Length,y=Sepal.Width,group=Species))+
  geom_point(aes(shape=Species,color=Species),size=5)

#3
plot_ly(x=Sepal.Length,y=Sepal.Width,z=Petal.Length,type="scatter3d",mode="markers")

#4
ggparcoord(data=df,
           columns=1:4)

#5
stars(df, draw.segments = TRUE,col.stars = 1:4, key.loc = c(20, 0.5))

#6
min(Petal.Length)
min(Petal.Width)
min(Sepal.Width)
min(Sepal.Length)

mean(Petal.Length)
mean(Petal.Width)
mean(Sepal.Width)
mean(Sepal.Length)

max(Petal.Length)
max(Petal.Width)
max(Sepal.Width)
max(Sepal.Length)


find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(Petal.Length)
find_mode(Petal.Width)
find_mode(Sepal.Width)
find_mode(Sepal.Length)

#7
fun <- function(x) 
{
  m <- dim(x)[2]
  res <- matrix(nrow = 3,ncol = m)
  rownames(res) <- c("Amp","mad","sd")
  colnames(res) <- colnames(x)
  for (i in 1:m)
  {
    res[1,i] <- max(x[,i])-min(x[,i])
    res[2,i] <- mean(abs(x[,i]-mean(x[,i])))
    res[3,i] <- sd(x[,i])
  }
  return(res)
}

fun(iris[-5])


sd(iris$Petal.Length)
sd(iris$Petal.Width)
sd(iris$Sepal.Width)
sd(iris$Sepal.Length)

#8
boxplot(iris[,1:4])

#9
plot(iris[,1:4])
pairs(iris[,1:4])
ggpairs(iris[,1:4])

MP <- cov(iris[,1:4],method ="pearson")
MP

M <- cor(iris[,1:4])
head(round(M,2))
corrplot(M)
ggcorr(iris[,1:4])

#10
irisMatrix <- as.matrix(iris[-5])
irisTransposedMatrix <- t(irisMatrix)[,nrow(irisMatrix):1]

heatmap(irisTransposedMatrix)


dataM <- as.matrix((iris[,1:4]))
pheatmap(dataM)

dataMS <- scale(dataM)
heatmap(dataMS)
heatmap(dataMS, Rowv = NA, Colv = NA)

my_colors <- colorRampPalette(c("cyan", "deeppink3"))
heatmap(dataM, col = my_colors("100"))


