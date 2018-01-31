
#

#http://www.rpubs.com/Guy_Larange/283625
#Loading Library
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(DT)
library(data.table)
library(plotly)
library(Matrix)
library(arules)

#Loading Dataset
aisles = fread("C:\\Users\\edingha\\Documents\\Business Analytics\\kaggle\\MBA_Kaggle\\aisles.csv")
departments = fread("C:\\Users\\edingha\\Documents\\Business Analytics\\kaggle\\MBA_Kaggle\\departments.csv")
order_products__prior = fread("C:\\Users\\edingha\\Documents\\Business Analytics\\kaggle\\MBA_Kaggle\\order_products__prior.csv")
order_products__train = fread("C:\\Users\\edingha\\Documents\\Business Analytics\\kaggle\\MBA_Kaggle\\order_products__train.csv")
orders = fread("C:\\Users\\edingha\\Documents\\Business Analytics\\kaggle\\MBA_Kaggle\\orders.csv")
products = fread("C:\\Users\\edingha\\Documents\\Business Analytics\\kaggle\\MBA_Kaggle\\products.csv")
#sample_submission = fread("C:\\Users\\edingha\\Documents\\Business Analytics\\kaggle\\MBA_Kaggle\\sample_submission.csv")



# Exploratory Data Analysis
str(orders,max.level=1)
kable(head(orders,12))
glimpse(orders)
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

orders %>%
  ggplot(aes(x=order_hour_of_day)) +
  geom_histogram(stat="count",fill="skyblue")

orders %>%
  ggplot(aes(x=order_dow)) +
  geom_histogram(stat="count",fill="skyblue")


orders %>%
  ggplot(aes(x=days_since_prior_order)) +
  geom_histogram(stat="count",fill="skyblue")
# How many items do people buy?
orders %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="skyblue", size=1)+geom_point(size=2, color="blue")

#What percentage of items bought are reordered?
order_products__prior %>%
  group_by(order_id) %>%
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="skyblue") +
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
##Bestsellers (Ranking by Support)

tmp <- order_products__prior %>%
  group_by(product_id) %>%
  summarize(count = n()) %>%
  top_n(12, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count))
kable(tmp)

#Top reorder proportion
tmp <-order_products__prior %>%
  group_by(product_id) %>%
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  filter(n>40) %>%
  top_n(10,wt=proportion_reordered) %>%
  arrange(desc(proportion_reordered)) %>%
  left_join(products,by="product_id")

kable(tmp)


#
order_products__prior %>%
  left_join(orders,by="order_id") %>%
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="skyblue")

#Analyzing SKU distribution with Treemaps
tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
tmp <- tmp %>% left_join(aisles,by="aisle_id")


library(treemap)
tmp2<-order_products__prior %>%
  group_by(product_id) %>%
  summarize(count=n()) %>%
  left_join(products,by="product_id") %>%
  ungroup() %>%
  group_by(department_id,aisle_id) %>%
  summarize(sumcount = sum(count)) %>%
  left_join(tmp, by = c("department_id", "aisle_id")) %>%
  mutate(onesize = 1)

treemap(tmp,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")
treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="",palette="Set3",border.col="#FFFFFF")


#. Performing Market Basket Analysis with arules apriori Rule

tmp <-order_products__prior %>%
  group_by(product_id) %>%
  left_join(products,by="product_id")

write.csv(tmp, file = "transactions.csv")
transactions<-read.transactions("C:\\Users\\edingha\\Documents\\transactions.csv", format = "single", sep = ",",cols = c(2,6))

summary(transactions)
inspect(transactions[1:3])


#Rules
groceryrules <- apriori(transactions, parameter = list(support =
                                                         0.001, confidence = 0.25))
# Association rules
summary(groceryrules)
inspect(groceryrules[1:10])
