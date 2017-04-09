rm(list=ls(all=T))

# Read dataset
stock_price = read.csv("E:/Interview Challenge/CorrelationOne/data/stock_returns_base150.csv",na.strings = c("","NA"))

# Data Quality check
if(nrow(stock_price) == sum(is.na(stock_price))) 
{
  print("No Missing data")
}else
{
  print("Missing data exists")
}

# Check number of missing values by each column 
column_missing = data.frame(apply(stock_price,2,function(x){sum(is.na(x))}))
column_missing$columns = row.names(column_missing)
names(column_missing)[1] = "Value"

# Bar chart 
require(ggplot2)
ggplot(column_missing,aes(x = columns, y = Value)) + 
  geom_bar(stat = "identity")+ggtitle("Missing Values")+theme_bw()

# Remove redundant rows with all missing values
stock_price = stock_price[complete.cases(stock_price$date),]

# summary of the data
summary(stock_price)
str(stock_price)

# Converting date variable into proper date format
require(lubridate)
stock_price$date = mdy_hm(stock_price$date)

# # Check for outliers
# boxplot(stock_price$S1,main="Stock1 Distribution")

# Transform the data to draw multiple boxplots
require(reshape2)
stock_price_melt <- melt(stock_price, id.var = "date")

require(ggplot2)
ggplot(data = stock_price_melt, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill="#b3d1ff"))+
  xlab("Variables")+ggtitle("Distribution plot")+theme_bw()

# Trend chart of stock S1
ggplot(data = stock_price,aes(x = date, y = S1)) +
  geom_line()+theme_bw()


# Train and Test data
Train = stock_price[1:50,]
Test = stock_price[51:100,]

# Exploratory data analysis(EDA)

# Correlation chart
library(corrgram)
corrgram(Train[,2:11], order= F, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Stock price correlation")


cor(Train[,2:11])

# Scatter chart
par(mfrow=c(2,2))

# Linear relation with S1
ggplot(Train,aes(x=S1,y = S7)) + 
  geom_point()+ ggtitle("S1 Vs S7")+ theme_bw()

ggplot(Train,aes(x=S1,y = S5)) + 
  geom_point()+ggtitle("S1 Vs S5") +theme_bw()

# Partially linear (S4,S8,S10)
ggplot(Train,aes(x=S1,y = S10)) + 
  geom_point()+ ggtitle("S1 Vs S10")+theme_bw()

rm(stock_price_melt,column_missing)

#####################################################
# Random Forest -- Feature selection
#####################################################

require(randomForest)
set.seed(1234)
rf_model = randomForest(S1~.,data = Train[,-1],ntree = 100)

var_imp = data.frame(rf_model$importance)
var_imp$variables = row.names(var_imp)
var_imp = var_imp[order(var_imp$IncNodePurity,decreasing = T),]

# S4, S10, S8 are relatively unimportant variables
# S6,S9 is highly correlated with all other variables (redundant variable)

# Variable Importance chart
ggplot(data = var_imp,aes(x=reorder(variables, -IncNodePurity),y = IncNodePurity))+
  geom_bar(stat = "identity",fill = "#8585ad")+xlab("Variables")+
  ggtitle("Variable Importance")+theme_bw()

# Considering required variables
Train_new = Train[,c("date","S1",c("S5","S7","S2","S3"))]


#####################################################
# Error Metric
#####################################################

# require(Metrics)
# rmse(Train$S1,train_pred)


absolute_dev <- function(actual,predicted)
{
  return(sum(abs(actual-predicted)))
}

#Train_error = absolute_dev(Train_new$S1,train_pred)


#####################################################
# Divide the training data into Train_X and Train_Y for model validation
#####################################################

Train_X = Train_new[1:40,]
Train_Y = Train_new[41:50,]

#####################################################
# Linear Regression
#####################################################

# lm_model = lm(S1~., data = Train_new[,-1])
# summary(lm_model)
# 
# linear_pred = predict(lm_model,Train_new[,-1])
# 
# Train_error_linear = absolute_dev(Train_new$S1,linear_pred)
# Train_error_linear

# 3.28 # Train_X and Train_Y
#12.384

Test_new = Test[,names(Train_new)]
#Test_predictions = predict(lm_model,Test_new[,-c(1,2)])

#### Actual VS predicted graph

Actual_predicted <- function(stock_price, Train_pred, Test_pred)
{
  predicted_plot = stock_price[,c("date","S1")]
  names(predicted_plot)[2] = "Actual"
  predicted_plot$Predicted = c(Train_pred,Test_pred)
  predicted_plot_melt <- melt(predicted_plot, id.var = "date")
  
  predict_plot <- ggplot(data = predicted_plot_melt, aes(x=date, y=value,colour = variable)) + 
    geom_line(size = 0.8)+
    xlab("Date")+ggtitle("Actual Vs Predicted")+theme_bw()
  
  return(predict_plot)
}

#linear_plot = Actual_predicted(stock_price,linear_pred,Test_predictions)
#plot(linear_plot)

#####################################################
# SVM
#####################################################

library(e1071)
svm_model <- svm(S1~. , data = Train_new[,-1])

svm_predictions <- predict(svm_model, Train_new[,-c(1,2)])
absolute_dev(Train_new$S1,svm_predictions)

# 3.10
# 10.63733

Test_new = Test[,names(Train_new)]
Svm_Test_predictions = predict(svm_model,Test_new[,-c(1,2)])

svm_plot = Actual_predicted(stock_price,svm_predictions,Svm_Test_predictions)
#plot(svm_plot)


# # perform a grid search
# tuneResult <- tune(svm, S1~.,  data = Train_X[,-1],
#                    ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
# )
# print(tuneResult)
# 
# # Draw the tuning graph
# plot(tuneResult)
# 
# ### Increment epsilon value from 0.2 to 0.4
# 
# tuneResult <- tune(svm, S1~.,  data = Train_X[,-1],
#                    ranges = list(epsilon = seq(0.2,0.4,0.01), cost = 2^(2:9))
# )
# 
# # Best tuned model
# tunedModel <- tuneResult$best.model
# tunedModelY <- predict(tunedModel, Train_Y[,-c(1,2)]) 
# 
# absolute_dev(Train_Y$S1,tunedModelY)
# 
# # this value can be different on your computer
# # because the tune method  randomly shuffles the data
# tunedModelRMSE <- rmse(error)


#####################################################
# PCA
#####################################################
prin_comp <- prcomp(Train[,c(3:11)], scale. = T)
names(prin_comp)

std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)

# Cumulative
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


Train.data <- data.frame(Stock1 = Train$S1, prin_comp$x)

# We are interested in 5 PCA's to explain 98% variance
Train.data <- Train.data[,c(1:6)]

## Linear regression model
lm_model = lm(Stock1~.,Train.data)
summary(lm_model)

# Train predictions
pca_predictions = predict(lm_model,Train.data[,c(2:6)])
absolute_dev(Train.data$Stock1,pca_predictions)
# 12.173
# 2.99

#transform test into PCA
test.data <- predict(prin_comp, newdata = Test[,c(3:11)])
test.data <- as.data.frame(test.data)

#select the first 5 components
test.data <- test.data[,1:5]

#make prediction on test data

pca_test_predictions = predict(lm_model,test.data)

pca_plot = Actual_predicted(stock_price,pca_predictions,pca_test_predictions)

#plot(pca_plot)


final_predictions = data.frame(Test$date,pca_test_predictions)
names(final_predictions) = c("Date","Value")

write.csv(final_predictions,"predictions.csv",row.names=F)






