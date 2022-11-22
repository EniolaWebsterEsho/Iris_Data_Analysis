library(arules)
data(Epub)
summary(Epub)
head(Epub)
inspect(head(Epub))

#1
data("SunBai")
summary(SunBai)
inspect(SunBai)

transactionInfo(SunBai)

nRec <-  nrow(SunBai)
items <- SunBai[,1]
transactionID <- SunBai[,2]
weight <- SunBai[,3]

hist(weight)

View(items)

hist(SunBai@itemInfo)
hist(SunBai@itemsetInfo[["weight"]])


#2
library(arules)
library(arulesViz)

data(SunBai)
summary(SunBai)
itemLabels(SunBai)
inspect(SunBai)


itemFrequencyPlot(SunBai, main = "Histogram of Item Frequency")


itemFrequencyPlot(SunBai, topN=10, cex.names = 1)



segment <- c(6.666667,3.333333, 5,
           1.666667, 1.666667, 3.333333,
           5, 3.333333)
pie(segment, itemLabels(SunBai), 
    main = "Pie Chart of Item Frequency", col = rainbow(8))



frequent_itemsets <- apriori(SunBai, 
                             parameter = list(
                               support=0.2, 
                               target ="frequent itemsets"))
summary(frequent_itemsets)
inspect(frequent_itemsets)

SunBai_rules <- apriori(SunBai, 
                        parameter = list(support=0.6, 
                                         confidence=0.6))

summary(SunBai_rules)
inspect(SunBai_rules)

items <- c("{A, B, C, D, E}","{C, F, G}","{A,B}",
           "{A}", "{C,F,G,H}", "{A,G,H}")
transactionID <- c("100","200","300","400","500","600")
weight <- c(0.5176528,0.4362571,0.2321374,0.1476262,0.5440458,0.4123691)
View(weight)
hist(weight)

SunBai_data <- data.frame(items,transactionID,weight)
SunBai_weight <- SunBai_data[,3]
View(SunBai_weight)
hist(SunBai_weight)
pie(items)

hist(SunBai_data,x=transactionID, y=weight)
#wrong
data <- select(SunBai_data, c(2:3))
hist(data)

data(Mushroom)
summary(Mushroom)
head(Epub)
inspect(head(Epub))

