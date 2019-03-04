install.packages("caret")
install.packages("doParallel")
install.packages("randomForest")
install.packages("microbenchmark")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("xlsx")
install.packages("lubridate")

  library(caret)
  library(doParallel)
  library(randomForest)
  library(microbenchmark)
  library(ggplot2)
  library(xlsx)
  library(lubridate)
  
  
  
  retail_data<- read.csv("D://project//sagar_consolidated.csv")
  
  View(retail_data)
  summary(retail_data)
  
  ### use initial plot 
  
  sheet <- retail_data
  attach(sheet)
  View(sheet)
  
  
  
  payment <- model.matrix(~fpaymentmode-1,sheet) ### as it has 4 categories
  sheet[,'shipment_transit'] <-as.numeric( difftime(shipment_date,order_date)) ## or sheet[,'shipment_transit'] <- as.Date(shipment_date,"%y/%m/%d") - as.Date(order_date,"%y/%m/%d")
  sheet[,'delivery_transit'] <- as.numeric(difftime(expected_delivery,shipment_date)) ## or sheet[,'delivery_transit'] <- as.Date(expected_delivery,"%y/%m/%d")-as.Date(shipment_date,"%y/%m/%d")
  sheet <-cbind(sheet,payment)
  sheet <- sheet[,-c(12:14,3)] ### removing date columns as they are converted into transit time
  View(sheet)
  summary(sheet)
  
  ##################### Random forest###############
  set.seed(1234)
  
  
  rf <-  sample(2,nrow(sheet),replace = TRUE,prob = c(.8,.2))
  
  traindata <- sheet[rf==1,]
  testdata <- sheet[rf==2,]
  str(sheet)
  str(traindata)
  
  
  sheet_rf <- randomForest(fdeliver_status ~ .,data = traindata,importance= T,ntree=500,mtry=3)
  plot(sheet_rf)
  sheet_rf
  varImpPlot(sheet_rf, main ='Decreasing order of importance',scale = T)
  
  ###for train  data set
  predict <- predict(sheet_rf,newdata = traindata)  ## rf retains as 1 as we trained sheet_rf with train data
  table(predict,traindata$fdeliver_status)
  mean(predict==traindata$fdeliver_status)    ##or ##confusionMatrix(predict ,traindata$fdeliver_status)
  
  #### for test data
  testpredict <- predict(sheet_rf,newdata = testdata)
  table(testpredict,testdata$fdeliver_status)
  mean(testpredict==testdata$fdeliver_status)  ## or## confusionMatrix(testpredict ,testdata$fdeliver_status)
  
  
  
  system.time(sheet_rf <- randomForest(fdeliver_status ~ .,data = traindata),gcFirst = T)
  
  
  randomforest_pred <- predict(sheet_rf,traindata)
  table(randomforest_pred,traindata$fdeliver_status)
  mean(randomforest_pred==traindata$fdeliver_status) ###98.3%
  
################# Random forest C50 ##########
install.packages("C50")
library(C50)

sheet_c50 <- C5.0(fdeliver_status ~ .,data = traindata)
print(sheet_c50)
### for train data
predict1 <- predict(sheet_c50,newdata = traindata)

table(predict1,traindata$fdeliver_status)
mean(predict1==traindata$fdeliver_status)

####for test data
testpredict <- predict(sheet_c50,newdata = testdata)
table(testpredict,testdata$fdeliver_status)
mean(testpredict==testdata$fdeliver_status)

system.time(sheet_c50 <- C5.0(fdeliver_status ~ .,data = traindata))

c50_ped <- predict(sheet_c50,sheet)


#########gradient boost #########
install.packages("gbm")
install.packages("verification")

library(gbm)
library(verification)

set.seed(5678)


sheet_gbm <-  sapply(sheet,as.numeric)

View(sheet_gbm)
sheet_gbm[,"fdeliver_status"] <- sheet_gbm[,"fdeliver_status"]-1
sheet_gbm <- data.frame(sheet_gbm)


num_sheet <-  sample(2,nrow(sheet_gbm),replace = TRUE,prob = c(.8,.2))

num_train_data <- sheet_gbm[num_sheet==1,]
num_test_data <- sheet_gbm[num_sheet==2,]



model_gbm <- gbm(fdeliver_status ~ .,data = num_train_data,distribution = "huberized",interaction.depth = 10, n.trees =1200)
print(model_gbm) ##### changed attributes for better accuracy for <1000 its overfitting 




pred_gbm <- predict(model_gbm,newdata = num_train_data,n.trees = 1200)
View(pred_gbm)
pred_gbm <- ifelse(pred_gbm>.5,1,0)
gbmtable <- table(num_train_data$fdeliver_status ,pred_gbm)
gbmtable
accuracy <- sum(diag(gbmtable))/sum(gbmtable)
accuracy ##97.5%



gbm_test_predict <- predict(model_gbm,newdata = num_test_data,n.trees = 1200)  ### for test data
gbm_test_predict <- ifelse(gbm_test_predict>.5,1,0)
gbm_test_table <- table(num_test_data$fdeliver_status ,gbm_test_predict)
gbm_test_table
accuracy <- sum(diag(gbm_test_table))/sum(gbm_test_table)
accuracy ###84.8



###change attributes of gradient boost for better predictions



model_gbm <- gbm(fdeliver_status ~ .,data = num_train_data,distribution = "bernoulli",interaction.depth = 20, n.trees =250)
print(model_gbm) ##### changed attributes for better accuracy for 600 its overfitting & 300 train data is almost 99% near to verfitting




pred_gbm <- predict(model_gbm,newdata = num_train_data,n.trees = 250)
View(pred_gbm)
pred_gbm <- ifelse(pred_gbm>.5,1,0)
gbmtable <- table(num_train_data$fdeliver_status ,pred_gbm)
gbmtable
accuracy <- sum(diag(gbmtable))/sum(gbmtable)
accuracy ###99.4



gbm_test_predict <- predict(model_gbm,newdata = num_test_data,n.trees = 250)  ### for test data
gbm_test_predict <- ifelse(gbm_test_predict>.5,1,0)
gbm_test_table <- table(num_test_data$fdeliver_status ,gbm_test_predict)
gbm_test_table
accuracy <- sum(diag(gbm_test_table))/sum(gbm_test_table)
accuracy ##91.7




system.time(model_gbm <- gbm(fdeliver_status ~ .,data = num_train_data,distribution = "bernoulli",interaction.depth = 20, n.trees =250))

gradientboost_pred <- predict(model_gbm,sheet_gbm,n.trees=250)
############ XGBoost##########

install.packages("xgboost")
install.packages("data.table")
install.packages("Matrix")
install.packages("vcd")
install.packages("DiagrammeR")
install.packages("dplyr")

library(xgboost)
library(data.table)
library(Matrix)
library(vcd)
library(DiagrammeR)
library(dplyr)



sheet[,"fdeliver_status"] <- as.numeric(fdeliver_status)-1  #### to convert to 0/1 for logisticxgb


### or convert ///sheetxgb <- sapply(sheetxgb, as.numeric)  convert all the columns to numeric and o/p is in matrix


sheetxgb <-  sheet[,]  %>% select_if(is.numeric)
View(sheetxgb)
samp_xgb <-  sample(2,nrow(sheetxgb),replace = TRUE,prob = c(.8,.2))

train_xgb <- sheetxgb[samp_xgb==1,]
test_xgb <- sheetxgb[samp_xgb==2,]
View(train_xgb)
attach(train_xgb)
attach(test_xgb)
View(train_xgb)

train_labels <- as.numeric(train_xgb$fdeliver_status)
test_labels <-  as.numeric(test_xgb$fdeliver_status)

new_train <- model.matrix(~. +0 ,data = train_xgb[,-1])
new_test <- model.matrix(~. +0 ,data = test_xgb[,-1])

xgb_train <- xgb.DMatrix(data = new_train, label = train_labels)
xgb_test <- xgb.DMatrix(data = new_test, label = test_labels)

model_xgb <- xgboost(xgb_train, nrounds = 2,  max_depth = 2, eta = 1, nthread = 2, objective="binary:logistic")
xgb.plot.tree(model = model_xgb)

View(train_labels)
pred_xgb <- predict(model_xgb,xgb_train,type ="response") ### for  training data
str(pred_xgb)
View(pred_xgb)


err1 <- mean(as.numeric(pred_xgb>.5) != train_labels)

print(paste("test_error=",err1))
accuracy1=1-err1
accuracy1


pred_test_xgb <- predict(model_xgb,xgb_test) ### for  test data
err <- mean(as.numeric(pred_xgb>.5) != test_labels)
print(paste("test_error=",err))
accuracy=1-err
accuracy



###changing parameters for xgb for better predictions

model_xgb <- xgboost(xgb_train, nrounds = 3,  max_depth = 6, eta = 0.3 , nthread = 4, objective="binary:logistic")
xgb.plot.tree(model = model_xgb)

View(train_labels)
pred_xgb <- predict(model_xgb,xgb_train,type ="response") ### for  training data
str(pred_xgb)
View(pred_xgb)


err1 <- mean(as.numeric(pred_xgb>.5) != train_labels)

print(paste("test_error=",err1))
accuracy1=1-err1
accuracy1


pred_test_xgb <- predict(model_xgb,xgb_test) ### for  test data
err <- mean(as.numeric(pred_xgb>.5) != test_labels)
print(paste("test_error=",err))
accuracy=1-err
accuracy

system.time(model_xgb <- xgboost(xgb_train, nrounds = 3,  max_depth = 6, eta = 0.3 , nthread = 4, objective="binary:logistic"))

sheet_label <- as.numeric(sheetxgb$fdeliver_status)  ## converting sheetdata into labels and reamining data toconvert into xgb.dmatrix
sheet_data <- model.matrix(~. +0 ,data = sheetxgb[,-1])
sheet_xgb_dmatrix <- xgb.DMatrix(data = sheet_data, label = sheet_label)
XGB_pred <- predict(model_xgb,sheet_xgb_dmatrix)


exp <- exp(pred_xgb)
prob_xgb <- exp/(1+exp)
probxgb1 <- as.numeric(prob_xgb)
probxgb1 <- as.data.frame(probxgb1)
View(probxgb1)

###################### adaa boost ############
install.packages("ada")
install.packages("rpart")
library(ada)
library(rpart)
set.seed(2233)


AB<-  sample(2,nrow(sheet),replace = TRUE,prob = c(.8,.2))

traindata <- sheet[AB==1,]
testdata <- sheet[AB==2,]
sheet_adda <- ada(fdeliver_status ~ .,data = traindata,iter=100)
print(sheet_adda)


pred_ab <- predict(sheet_adda,newdata = traindata )

table(pred_ab,traindata$fdeliver_status)
mean(pred_ab==traindata$fdeliver_status)
###### for test data#####
pred_test_ab <- predict(sheet_adda,newdata = testdata)
table(pred_test_ab ,testdata$fdeliver_status)
mean(pred_test_ab ==testdata$fdeliver_status)
View(pred_test_ab)
system.time(sheet_adda <- ada(fdeliver_status ~ .,data = traindata,iter=100))

adaboost_pred <- predict(sheet_adda,sheet)


retail_predictors <- cbind(randomforest_pred,c50_ped,gradientboost_pred,XGB_pred,adaboost_pred,sheet[,-1])




 ########## google sheets data exporting###########

install.packages("googlesheets")
library(googlesheets)
?googlesheets
gs_auth()

gs_ls()

my_googlesheets <-  gs_ls()
View(my_googlesheets)

google_retail  <-  gs_title("Retail, Supply Chain Project")

google_read <- gs_read(google_retail,ws=16)
 View(google_read)


################## My sql implementation#############
install.packages("RMySQL")
install.packages("RODBC")
install.packages("dplyr")

 
library(RMySQL)
library(DBI)
library(RODBC)
library(xlsx)
 library(dplyr)

 con <- dbConnect(MySQL(),
                  user="root", password="root",
                  dbname="retail_excelr", host="localhost")
 
 on.exit(dbDisconnect(con))
 
 con <- dbConnect(MySQL(),
                  user="root", password="root",
                  dbname="retail_excelr", host="localhost")
 
 dbListTables(con)
 
 rs <- dbSendQuery(con, "select price from sagar_consolidated limit 10;") ####db sendquery send query directly like command line client of mysql
  data1 <- dbFetch(rs, n=10)
  data1
 huh <- dbHasCompleted(rs)
 dbClearResult(rs)
 dbDisconnect(con)
 
 con <- dbConnect(MySQL(),
                  user="root", password="root",
                  dbname="retail_excelr", host="localhost")
 
 data_mysql <- dbSendQuery(con, "select * from sagar_consolidated;") #####****fetching directly complete table from mysql
data_mysql_fetch <-  dbFetch(data_mysql)
View(data_mysql_fetch)


### write output to Mysql directly

con <- dbConnect(MySQL(),
                 user="root", password="root",
                 dbname="retail_excelr", host="localhost")


dbListTables(con)

# write it back
check_local <- dbSendQuery(con,"SHOW VARIABLES LIKE 'local_infile';")
dbClearResult(check_local)
dbSendQuery(con,"SET GLOBAL local_infile = 1;") ### to write values to local switching local_infine to ON 
dbWriteTable(con, "predictors_list", retail_predictors, overwrite = TRUE)




