install.packages("superml")
install.packages("textstem")
install.packages("e1071")
install.packages("neuralnet")
install.packages("gbm")
install.packages("quanteda")
install.packages("tm")

options(warn=-1)
library(dplyr)
library(tidyr)
library(superml)
library(zoo)
library(textstem)
library(stringr)
library(randomForest) 
library(shiny)
library(e1071)
library(neuralnet)
library(caret)
library(Metrics)
library(data.table)
library(ggplot2)
library(plyr)
library(gbm)
library(rpart)
library(quanteda)
library(tm)

train_df = read.csv(file = '/Users/meghanavs/Desktop/R-individual-Project/data/train.tsv', sep = '\t', header = TRUE)
View(train_df)
class(train_df)
test_df = read.csv(file = '/Users/meghanavs/Desktop/R-individual-Project/data/test.tsv', sep = '\t', header = TRUE)
View(test_df)

head(train_df)

str(train_df)

class(train_df$brand_name)

### EDA
#Summary
print("Train data dimension")
dim(train_df)
print("Test data dimension")
dim(test_df)

print("Train dataset Columns")
colnames(train_df)

print("Unique category count")
length(unique(train_df$category_name))
print("Unique item condition count")
length(unique(train_df$item_condition_id))
print("Unique brand count")
length(unique(train_df$brand_name))

print("Train data summary")
summary(train_df)
print("Test data summary")
summary(test_df)

# Histogram of Price
ggplot(data=train_df,aes(x=price)) + 
  geom_histogram(fill='blue') + 
  labs(title='Histogram of Prices')

ggplot(data=train_df,aes(x=log(price))) + 
  geom_histogram(fill='red') + 
  labs(title='Histogram of Log of Prices')

# Box Plot of Item vs Price
ggplot(data=train_df,aes(x=as.factor(item_condition_id),y=log(price+1))) +
  geom_boxplot(fill='orange',color='purple') + 
  labs(title='Box Plot of item_condition_id vs Log price')

ggplot(data=train_df,aes(x=as.factor(item_condition_id),y=price)) +
  geom_boxplot(fill='orange',color='purple')  + 
  labs(title='Box Plot of item condition id vs price')

#Anova between price and shipping
one.way <- aov(shipping ~ price, data = train_df)
summary(one.way)

# Density graph of shipping vs price
train_df %>%
  ggplot(aes(x=log(price+1),fill=factor(shipping))) +
  geom_density(alpha=0.5) + 
  labs(title='Plot of shipping price vs non shipping price')

# Top 10 Brand based on averge and median Price
data = ddply(train_df, .(brand_name), summarize, "mean"= mean(price), "median" = median(price))  
data %>% arrange(desc(mean)) %>% head(10)  %>%
  ggplot(aes(x= reorder(brand_name,mean),y=mean)) +
  geom_point(color='red') +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) + 
  labs(title='Top 10 brand based on average price')
