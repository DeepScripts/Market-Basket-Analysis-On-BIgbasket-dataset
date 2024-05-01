# Load required libraries
library(arules)

# Read the CSV file into a data frame
file_path <- "C:\\Users\\shriv\\Downloads\\r project\\BigBasket Data.csv"
data <- read.csv(file_path)
str(data)
# Ensure 'Invoice No' and 'Product' are character vectors
data$Invoice.No <- as.factor(as.character(data$Invoice.No))
data$Category <- as.factor(data$Category)

# Extract relevant columns
data_new <- data[, c("Invoice.No", "Category")]
str(data_new)
data_new=na.omit(data_new)

#write csv
write.csv(data_new,"data3.csv",row.names = FALSE)

# Read the CSV file without specifying columns
transactions <- read.transactions(file="data3.csv", format="single", sep=",", cols=c(1,2), skip=1)
inspect(transactions[1:5])

  # Perform association rule mining using Apriori algorithm
  rules <- apriori(transactions, 
                   parameter = list(support = 0.01, confidence = 0.50))
  
  # Inspect the top 10 association rules
  inspect(head(sort(rules, by = "lift"), 10))
  inspect(sort(rules, by = "confidence"))
  itemFrequencyPlot(transactions, topN = 15)
  itemFrequencyPlot(transactions, support = 0.10)

