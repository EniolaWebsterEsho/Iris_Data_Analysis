library(arules)
library(arulesViz)

market_basket <- 
  list(
    c("Chips","Cookies","Regular Soda","Ham"),
    c("Chips","Ham","Boneless Chicken","Diet Soda"),
    c("Ham","Bacon","Whole Chicken","Regular Soda"),
    c("Chips","Ham","Boneless Chicken","Diet Soda"),
    c("Chips","Bacon","Boneless Chicken"),
    c("Chips","Ham","Bacon","Whole Chicken","Regular Soda"),
    c("Chips","Cookies","Boneless Chicken","Diet Soda")
  )

trans <- as(market_basket, "transactions")
itemLabels(trans)
summary(trans)
inspect(trans)
itemFrequencyPlot(trans, topN=10, cex.names=1)

itemsets <- apriori(trans, parameter = list(minlen=1,
                                            support=0.14
                                            ))
summary(itemsets)
inspect(itemsets)
