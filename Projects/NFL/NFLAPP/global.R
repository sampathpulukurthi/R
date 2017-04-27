# Loading required libraries
require(xlsx)
#require(rpart)
#require(ftsa)

# Reading the dataset
#Price_data <- read.csv("E:/Projects/Real_Estate/Version 2.0/AutoPredictPriceonNewLineBigML.csv")
#nfl.data <- read.xlsx("G:/Sampath/Resume/Interviews/NFL/Analysis.xlsx",sheetIndex = 1)
nfl.data <- read.xlsx("Analysis.xlsx",sheetIndex = 1)
att_levels <- names(nfl.data)
season_levels <- c("All","Regular","Post")

Prediction_data <- function(dataset,sub.season)
{
  #print(univariate)
  
  if(sub.season == "All")
  {
    dataset = dataset
  }
  
  if(sub.season == "Regular")
  {
    dataset = dataset[grep("Reg",dataset$Week),]
  }
  
  if(sub.season == "Post")
  {
    dataset = dataset[grep("Post",dataset$Week),]
  }
  
  #   #subsetting the data with required variables for model
  #   dataset_sub = dataset[,c("price","bd","ba","sqft","lot.size","parking","dom")]
  #   
  ## Splitting the data into train and test
  ## Split the first 80% rows as train and the rest as test 
  #data_train = dataset[1:(0.8*nrow(dataset)),] 
  #data_test = dataset[(nrow(data_train)+1):nrow(dataset),] 
     
  #   ## Decision Tree model
  #   rpart_model = rpart(price~., data = data_train)
  #   
  #   # Test predictions (Predict the price on test dataset)
  #   rpart_predictions = round(predict(rpart_model,data_test[,-1]))
  #   
  #   # Pred_Price as the test predictions column (train rows as NA's)
  #   dataset$Pred_Price = c(rep(NA,nrow(data_train)),rpart_predictions)
  #   
  #   # Reordering the columns
  #   dataset = subset(dataset,select = c("type","address","city","state","zip",
  #                                       "bd","ba","subdivision","sqft","lot.size","year","parking",
  #                                       "dom","url","latitude","longitude","price","Pred_Price" ))
  #   ##Mape on the test and Actual
  #   base_error_val = error(forecast = rpart_predictions, true = data_test$price, method = "mape")
  #   
  Model_list = list()
  Model_list[[1]] = dataset
  Model_list[[2]] = "base_error_val"
  
  return(Model_list)
  #return(dataset)
  
}


