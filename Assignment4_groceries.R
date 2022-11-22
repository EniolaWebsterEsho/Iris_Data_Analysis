install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

data("Groceries")
summary(Groceries)
View(Groceries)

itemLabels(Groceries)
inspect(Groceries)
itemFrequencyPlot(Groceries, topN=20, cex.names=1)

image(Groceries)

itemsets <- apriori(Groceries, 
                    parameter = list(
                      minlen=1, support=0.02,
                      target= "frequent itemsets"))
summary(itemsets)
inspect(itemsets)

groceries_rules <- apriori(Groceries, 
                           parameter = list(
                             support=0.06,
                             confidence=0.3))
summary(groceries_rules)
inspect(groceries_rules)

plot(groceries_rules)
plot(groceries_rules, method ="graph", limit=20)

subrules <- head(groceries_rules, n = 10, by ='confidence')
plot(subrules, method ="graph", engine="htmlwidget")
