
rm(list=ls(all=T))
x<-c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
     "MASS", "rpart", "gbm", "ROSE")
install.packages(x)
lapply(x, require, character.only = TRUE)

## Read the data

marketing_train = read.csv("marketing_training.csv",header=T,na.strings=c(" ","","NA"))
marketing_test = read.csv("marketing_test.csv",header=T,na.strings=c(" ","","NA"))
marketing_test$X = NULL

## Missing Data Count (Train)
missing_val = data.frame(apply(marketing_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(marketing_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()

## Missing Data Count (Test)
missing_val_test = data.frame(apply(marketing_test,2,function(x){sum(is.na(x))}))
missing_val_test$Columns = row.names(missing_val_test)
names(missing_val_test)[1] =  "Missing_percentage"
missing_val_test$Missing_percentage = (missing_val_test$Missing_percentage/nrow(marketing_test)) * 100
missing_val_test = missing_val_test[order(-missing_val_test$Missing_percentage),]

ggplot(data = missing_val_test[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Test)") + theme_bw()

## Explore the data
str(marketing_train)
str(marketing_test)

## Univariate Analysis and Variable Consolidation
marketing_train$schooling[marketing_train$schooling %in% "illiterate"] = "unknown"
marketing_train$default[marketing_train$default %in% "yes"] = "unknown"
marketing_train$default = as.factor(as.character(marketing_train$default))
marketing_train$marital[marketing_train$marital %in% "unknown"] = "married"
marketing_train$marital = as.factor(as.character(marketing_train$marital))
marketing_train$month[marketing_train$month %in% c("sep","oct","mar","dec")] = "dec"
marketing_train$month[marketing_train$month %in% c("aug","jul","jun","may","nov")] = "jun"
marketing_train$month = as.factor(as.character(marketing_train$month))
marketing_train$loan[marketing_train$loan %in% "unknown"] = "no"
marketing_train$loan = as.factor(as.character(marketing_train$loan))
marketing_train$schooling[marketing_train$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
marketing_train$schooling = as.factor(as.character(marketing_train$schooling))
marketing_train$profession[marketing_train$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
marketing_train$profession[marketing_train$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
marketing_train$profession = as.factor(as.character(marketing_train$profession))

## Correlation Plot 
numeric_index = sapply(marketing_train,is.numeric)
corrgram(marketing_train[,numeric_index], order= F,upper.panel=panel.pie, text.panel=panel.txt,main="Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(marketing_train,is.factor)
factor_data=marketing_train[,factor_index]
for (i in 1:10)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$responded,factor_data[,i])))
  print("next variable")
}

## Dimension Reduction
corr_var = c("pdays","emp.var.rate","day_of_week","loan","housing")
marketing_train_deleted = subset(marketing_train,select = names(marketing_train)
                                 [!names(marketing_train) %in% corr_var])

## BoxPlots - Distribution and Outlier Check
numeric_data=marketing_train[,numeric_index]
cnames=colnames(numeric_data)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (cnames[i]), x = "responded"), data = subset(marketing_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="responded")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn5,ncol=2)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

## kNN Imputation
set.seed(1234)
marketing_train_deleted = knnImputation(marketing_train_deleted, k=5)
sum(is.na(marketing_train_deleted))
str(marketing_train_deleted)

## Data Partition for internal evalution (Stratified Sampling)
set.seed(1234)
train.index <- createDataPartition(marketing_train_deleted$responded, p = .80, list = FALSE)
train <- marketing_train_deleted[ train.index,]
test  <- marketing_train_deleted[-train.index,]

########### Handling Unbalanced data by TOMEK & SMOTE ####################

## Creating dummy variables
set.seed(1234)
train$responded = ifelse(train$responded == "no",0,1 )
dummy = dummy.data.frame(train)
tomek = ubTomek(dummy[,-30], dummy[,30])
model_train_tomek = cbind(tomek$X,tomek$Y)
names(model_train_tomek)[30] = "responded"

## TOMEK Indices 
removed.index = tomek$id.rm

# Converting responded to factor variable
train$responded = ifelse(train$responded == 0,"no","yes")
train$responded = as.factor(train$responded)
train_tomek = train[-removed.index,]

## SMOTE
set.seed(1234)
train_tomek_smote <- SMOTE(responded ~ ., train_tomek, perc.over = 400)
table(train_tomek_smote$responded)

##################### Logistic Regression ##############
## Normal Data
logit_model_complete <- glm(responded ~.,family=binomial(link='logit'),data= train)
fitted.results.complete <- predict(logit_model_complete,newdata= test ,type='response')
fitted.results.complete <- ifelse(fitted.results.complete > 0.5,1,0)
conf_matrix_complete_logistic = table(test$responded,fitted.results.complete)
conf_matrix_complete_logistic
roc.curve(test$responded,fitted.results.complete)

## Smoted data
logit_model <- glm(responded ~.,family=binomial(link='logit'),data= train_tomek_smote)
summary(logit_model)
fitted.results <- predict(logit_model,newdata= test ,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
conf_matrix_logistic = table(test$responded,fitted.results)
conf_matrix_logistic
roc.curve(test$responded,fitted.results)


## Matthews correlation coefficient
tn=as.double(conf_matrix_logistic[1,1]);
fp=as.double(conf_matrix_logistic[1,2]);
fn=as.double(conf_matrix_logistic[2,1]);
tp=as.double(conf_matrix_logistic[2,2]);
MCC = (tp*tn - fp*fn) / sqrt( (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))

############################### Random Forest ######################################
## Normal Data
set.seed(1234)
rf_model_complete = randomForest(responded~.,train,ntree=500)
rf_test_predictions_complete = predict(rf_model_complete, test, type="class")
conf_matrix_complete_rf = table(test$responded,rf_test_predictions_complete)
confusionMatrix(conf_matrix_complete_rf)
roc.curve(test$responded,rf_test_predictions_complete)

## Smoted Data
set.seed(1234)
rf_model_tomek_smote = randomForest(responded~.,train_tomek_smote,ntree=500,mtry=2,sampsize=c(50,250),nodesize=1, rules = TRUE)
rf_test_predictions = predict(rf_model_tomek_smote, test, type="class")
conf_matrix_rf = table(test$responded,rf_test_predictions)
confusionMatrix(conf_matrix_rf)
roc.curve(test$responded,rf_test_predictions)

## Matthews correlation coefficient
tn=as.double(conf_matrix_rf[1,1]);
fp=as.double(conf_matrix_rf[1,2]);
fn=as.double(conf_matrix_rf[2,1]);
tp=as.double(conf_matrix_rf[2,2]);
MCC = (tp*tn - fp*fn) / sqrt( (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))

## Variable Importance List
rf_var_imp = data.frame(rf_model_tomek_smote$importance)
rf_var_imp$Variables = row.names(rf_var_imp)
rf_var_imp = rf_var_imp[order(-rf_var_imp$MeanDecreaseGini),]

########################## C50 ##################
## Normal Data
set.seed(1234)
C50_model_complete <- C5.0(responded ~.,train, trials = 100)
summary(C50_model_complete)
C50_test_predictions_complete = predict(C50_model_complete, test, type="class")
conf_matrix_complete_c50 = table(test$responded,C50_test_predictions_complete)
confusionMatrix(conf_matrix_complete_c50)
roc.curve(test$responded,C50_test_predictions_complete)

## Smoted Data
set.seed(1234)
C50_model <- C5.0(responded ~.,train_tomek_smote, trials = 100)
summary(C50_model)
C50_test_predictions = predict(C50_model, test, type="class")
conf_matrix_c50 = table(test$responded,C50_test_predictions)
confusionMatrix(conf_matrix_c50)
roc.curve(test$responded,C50_test_predictions)

## Matthews correlation coefficient
tn=as.double(conf_matrix_c50[1,1]);
fp=as.double(conf_matrix_c50[1,2]);
fn=as.double(conf_matrix_c50[2,1]);
tp=as.double(conf_matrix_c50[2,2]);
MCC = (tp*tn - fp*fn) / sqrt( (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))

## C50 Cross Validation 
folds <- createFolds(factor(train_tomek_smote$responded), k = 5, list = FALSE)
k=5
train_tomek_smote$id <- folds
list <- 1:k
prediction <- data.frame()
testsetCopy <- data.frame()
Acc_val = vector()
Recall_val = vector()
Precision_val = vector()

for(i in 1:k){
  trainingset <- subset(train_tomek_smote, id %in% list[-i])
  testset <- subset(train_tomek_smote, id %in% c(i))
  trainingset$id = NULL
  testset$id = NULL
  set.seed(1234)
  C50_model <- C5.0(responded ~.,trainingset, trials = 100)
  C50_test_predictions = predict(C50_model, testset, type="class")
  conf_matrix = table(testset$responded,C50_test_predictions)
  conf_matrix_results = confusionMatrix(conf_matrix)
  
  # Accuracy
  accu_metric = conf_matrix_results$overall['Accuracy']
  Recall_metric = conf_matrix_results$byClass['Neg Pred Value']
  Precision_metric = conf_matrix_results$byClass['Pos Pred Value']
  Acc_val = c(Acc_val, accu_metric)
  Recall_val = c(Recall_val,Recall_metric)
  Precision_val = c(Precision_val,Precision_metric)
  temp <- as.data.frame(predict(C50_model, testset[,-17]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,17]))
}

print(mean(Acc_val))
print(mean(Recall_val))
print(mean(Precision_val))
print(Recall_val)

## Recall for 2nd iteration is max 
i = 2
trainingset <- subset(train_tomek_smote, id %in% list[-i])
testset <- subset(train_tomek_smote, id %in% c(i))
trainingset$id = NULL
testset$id = NULL
set.seed(1234)
C50_model <- C5.0(responded ~.,trainingset, trials = 100)
C50_test_predictions = predict(C50_model, testset, type="class")
conf_matrix = table(testset$responded,C50_test_predictions)
conf_matrix_results = confusionMatrix(conf_matrix)
conf_matrix_results

### Testing best cross validated model on actual test 
C5_Actual_test_prediction = predict(C50_model,test[,-17])
conf_matrix_actual = table(test$responded,C5_Actual_test_prediction)
conf_matrix_results_actual = confusionMatrix(conf_matrix_actual)
conf_matrix_results_actual

############ Testing Final Random Forest Model ################

set.seed(1234)
folds <- createFolds(factor(train_tomek_smote$responded), k = 5, list = FALSE)
k=5
train_tomek_smote$id <- folds
list <- 1:k

prediction <- data.frame()
testsetCopy <- data.frame()
Acc_val = vector()
Recall_val = vector()
Precision_val = vector()

for(i in 1:k){
  trainingset <- subset(train_tomek_smote, id %in% list[-i])
  testset <- subset(train_tomek_smote, id %in% c(i))
  trainingset$id = NULL
  testset$id = NULL
  set.seed(1234)
  rf_model_tomek_smote = randomForest(responded~.,trainingset,ntree=500,mtry=2,sampsize=c(50,250),nodesize=1)
  rf_test_predictions = predict(rf_model_tomek_smote, testset, type="class")
  conf_matrix = table(testset$responded,rf_test_predictions)
  conf_matrix_results = confusionMatrix(conf_matrix)
  
  # Accuracy
  accu_metric = conf_matrix_results$overall['Accuracy']
  Recall_metric = conf_matrix_results$byClass['Neg Pred Value']
  Precision_metric = conf_matrix_results$byClass['Pos Pred Value']
  Acc_val = c(Acc_val, accu_metric)
  Recall_val = c(Recall_val,Recall_metric)
  Precision_val = c(Precision_val,Precision_metric)
  temp <- as.data.frame(predict(rf_model_tomek_smote, testset[,-17]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,17]))
}
print(mean(Acc_val))
print(mean(Recall_val))
print(mean(Precision_val))
print(Recall_val)

## Recall for 3rd iteration is max
i = 3
trainingset <- subset(train_tomek_smote, id %in% list[-i])
testset <- subset(train_tomek_smote, id %in% c(i))
trainingset$id = NULL
testset$id = NULL
set.seed(1234)
rf_model_tomek_smote = randomForest(responded~.,trainingset,ntree=500,mtry=2,sampsize=c(50,250),nodesize=1)
rf_test_predictions = predict(rf_model_tomek_smote, testset, type="class")
conf_matrix = table(testset$responded,rf_test_predictions)
conf_matrix_results = confusionMatrix(conf_matrix)
conf_matrix_results

### Testing best cross validated model on actual test 

rf_actual_test_predictions = predict(rf_model_tomek_smote,test[,-17])
conf_matrix_actual = table(test$responded,rf_actual_test_predictions)
conf_matrix_results_actual = confusionMatrix(conf_matrix_actual)
conf_matrix_results_actual
roc.curve(test$responded,rf_actual_test_predictions)

###### Scoring Marketing_Test data ######

## Data Manipulations

marketing_test$schooling[marketing_test$schooling %in% "illiterate"] = "unknown"
marketing_test$default[marketing_test$default %in% "yes"] = "unknown"
marketing_test$default = as.factor(as.character(marketing_test$default))
marketing_test$marital[marketing_test$marital %in% "unknown"] = "married"
marketing_test$marital = as.factor(as.character(marketing_test$marital))
marketing_test$month[marketing_test$month %in% c("sep","oct","mar","dec")] = "dec"
marketing_test$month[marketing_test$month %in% c("aug","jul","jun","may","nov")] = "jun"
marketing_test$month = as.factor(as.character(marketing_test$month))
marketing_test$loan[marketing_test$loan %in% "unknown"] = "no"
marketing_test$loan = as.factor(as.character(marketing_test$loan))
marketing_test$schooling[marketing_test$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
marketing_test$schooling = as.factor(as.character(marketing_test$schooling))
marketing_test$profession[marketing_test$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
marketing_test$profession[marketing_test$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
marketing_test$profession = as.factor(as.character(marketing_test$profession))


corr_var_test = c("pdays","emp.var.rate","day_of_week","loan","housing")
marketing_test = subset(marketing_test,select = names(marketing_test)[!names(marketing_test) %in% corr_var_test])
set.seed(1234)
marketing_test = knnImputation(marketing_test, k=5)
sum(is.na(marketing_test))
str(marketing_test)

rf_final_test_predictions = predict(rf_model_tomek_smote,marketing_test, type="class")
Final_Predictions=data.frame(rf_final_test_predictions)
write.csv(Final_Predictions,"Predictions.csv")
table(Final_Predictions)
