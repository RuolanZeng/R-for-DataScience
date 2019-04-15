# We use three packages as following, you may need to dowload them before running the code
require(readr)
require(arules)
require(plyr)

# load data
order_products <- read.csv("http://www.utdallas.edu/~yxs173830/cs6301hw6/order_products__train.csv", nrows = 20000)
products_data <- read.csv("http://www.utdallas.edu/~yxs173830/cs6301hw6/products.csv")
departments_data <- read.csv("http://www.utdallas.edu/~yxs173830/cs6301hw6/departments.csv")

transection_data <- merge(order_products[,1:2], products_data, by = "product_id") 
transection_data <- merge(transection_data, departments_data, by = "department_id")
transection_data <- arrange(transection_data, order_id) 

# products

# Converting data to a class of transactions
dt <- split(transection_data$product_name, transection_data$order_id)
dt2 <- as(dt,"transactions")
summary(dt2)

# Frequent Items Plotting - products
itemFrequencyPlot(dt2, topN = 20, type="absolute")

# Rules can be generated as below:
rules <- apriori (dt2,
                  parameter = list(supp = 0.001, conf = 0.5))
rules_conf <- sort (rules, by="confidence",
                    decreasing=TRUE) 
# show the support, lift and confidence for all rules
inspect(head(rules_conf))

# Departments

# Converting data to a class of transactions
dt <- split(transection_data$department, transection_data$order_id)
dt2 <- as(dt,"transactions")
summary(dt2)

# Frequent Items Plotting - products
itemFrequencyPlot(dt2, topN = 20, type="absolute")

# Rules can be generated as below:
rules <- apriori (dt2,
                  parameter = list(supp = 0.001, conf = 0.5))
rules_conf <- sort (rules, by="confidence",
                    decreasing=TRUE) 
# show the support, lift and confidence for all rules
inspect(head(rules_conf))



