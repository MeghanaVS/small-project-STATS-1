# load necessary libraries
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
library(scales)

# load data
train_df = read.csv(file = '/Users/meghanavs/Desktop/R-individual-Project/data/train.tsv', sep = '\t', header = TRUE)
test_df = read.csv(file = '/Users/meghanavs/Desktop/R-individual-Project/data/test.tsv', sep = '\t', header = TRUE)

# Histogram of Price
ggplot(data=train_df,aes(x=price)) + 
  geom_histogram(fill='#FF7F50', color='#FF7F50', alpha=0.8) + 
  labs(title='Histogram of Prices') +
  theme(plot.title = element_text(hjust = 0.5, size=20))

ggplot(data=train_df,aes(x=log(price))) + 
  geom_histogram(fill='#FF7F50', color='#FF7F50', alpha=0.8) + 
  labs(title='Histogram of Log of Prices') +
  theme(plot.title = element_text(hjust = 0.5, size=20))

# Box Plot of Item vs Price
ggplot(data=train_df,aes(x=as.factor(item_condition_id),y=log(price+1))) +
  geom_boxplot(fill='#008080', color='#008080', alpha=0.8) + 
  labs(title='Box Plot of item_condition_id vs Log price') +
  theme(plot.title = element_text(hjust = 0.5, size=20))

ggplot(data=train_df,aes(x=as.factor(item_condition_id),y=price)) +
  geom_boxplot(fill='#008080', color='#008080', alpha=0.8)  + 
  labs(title='Box Plot of item condition id vs price') +
  theme(plot.title = element_text(hjust = 0.5, size=20))

# Density graph of shipping vs price
train_df %>%
  ggplot(aes(x=log(price+1), fill=factor(shipping))) +
  geom_density(alpha=0.6) + 
  scale_fill_manual(values=c("#1E90FF", "#FFD700")) +
  labs(title='Density Plot of Shipping vs Non-shipping Price') +
  theme(plot.title = element_text(hjust = 0.5, size=20))

# Top 10 Brand based on average and median Price
data = ddply(train_df, .(brand_name), summarize, "mean"= mean(price), "median" = median(price))  
data %>% arrange(desc(mean)) %>% head(10)  %>%
  ggplot(aes(x= reorder(brand_name,mean),y=mean)) +
  geom_point(color='#9370DB', size=3) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) + 
  labs(title='Top 10 Brands based on Average Price') +
  theme(plot.title = element_text(hjust = 0.5, size=20))

