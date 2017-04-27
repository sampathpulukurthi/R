rm(list=ls(all=T))
setwd("G:/Sampath/Resume/Interviews/NFL")

# Loading required libraries
require(xlsx)
require(ggplot2)
require(plyr)

# Loading the data
nfl.data = read.xlsx("Analysis.xlsx",sheetIndex = 1)
head(nfl.data)

# Exploring the data
str(nfl.data)
summary(nfl.data)

# Descriptive statistics
ggplot(nfl.data,aes(x=Season))+geom_bar(fill = "#98AFC7")+theme_bw()

ggplot(nfl.data,aes(x=Week))+geom_bar(fill = "#98AFC7")+scale_x_discrete(limits=unique(nfl.data$Week))+theme_bw()

# Total sales distribution
ggplot(nfl.data,aes(x=Sales))+
  scale_x_continuous(labels = c("0M","10M","20M","30M"))+
  geom_histogram(fill = "#98AFC7")+
  ggtitle("Sales Distribution")+theme_bw()

# Sales distribution by year
ggplot(nfl.data[nfl.data$Season == 2014,],aes(x=Sales))+
  scale_x_continuous(labels = c("0M","10M","20M","30M"))+
  geom_histogram(fill = "#42456F")+
  ggtitle("2014 Sales Distribution")+theme_bw()

ggplot(nfl.data[nfl.data$Season == 2015,],aes(x=Sales))+
  scale_x_continuous(labels = c("0M","4M","12M","16M","20M"))+
  geom_histogram(fill = "#42456F")+
  ggtitle("2015 Sales Distribution")+theme_bw()


# Check for any outliers in the data
#boxplot(nfl.data$Sales,title = "Sales distribution")

# Distribution of sales by each Season
ggplot(nfl.data,aes(x=Season,y = Sales,fill = Season))+geom_boxplot()+
scale_y_continuous(labels = c("0M","10M","20M","30M"))+
ggtitle("Season Sales distribution")+theme_bw()


# Total Sales by each season
ggplot(nfl.data,aes(x=Season,y = Sales,fill = Season))+geom_bar(stat = "identity")+
  scale_y_continuous(labels = c("0M","50M","100M","150M"))+
  ggtitle("Total Sales by Season")+ theme_bw()

# Total sales by each season and week

ggplot(nfl.data,aes(x=Week,y=Sales,group=Season,fill=Season))+
  geom_bar(stat="identity",position="dodge")+ 
  scale_y_continuous(labels = c("0M","10M","20M","30M"))+
  scale_x_discrete(limits=unique(nfl.data$Week))+ ggtitle("Weekly Sales by Season")+theme_bw()

# Trend Line chart of weekly sales by each year
p <- ggplot(data=nfl.data, aes(x=Week, y=Sales, group=Season,color = Season)) + 
  geom_line(size=0.8) + geom_point(size=3)+
  scale_y_continuous(labels = c("0M","10M","20M","30M"))+
  scale_x_discrete(limits=unique(nfl.data$Week))+
  ggtitle("Sales Trend by Season")+ 
  theme_bw()

p + theme(axis.text.x=element_text(angle=90,hjust=1))

# Net increase/decrease in the sales of the season
sales.2014 = sum(nfl.data[nfl.data$Season == 2014,]$Sales)
sales.2015 = sum(nfl.data[nfl.data$Season == 2015,]$Sales)

((sales.2015 - sales.2014)/sales.2014)*100

