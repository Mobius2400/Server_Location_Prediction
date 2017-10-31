##Predict location of server given other parameters recorded in server creation form
library(caTools)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(tree)
library(plyr)
library(randomForest)
library(caret)
library(klaR)
library(e1071)
library(ggplot2)

#Read in data with all columns
setwd("C:/Users/Das_J/Dropbox/JnJ/Server_Location_Prediction")
application_server_data <- read.csv("application_servers.csv", fill = T)

#Remove identifier columns
application_server_data$"dv_requested_item" <- NULL
application_server_data$"what_is_your_application_name_" <- NULL
#Remove unecessary columns identified by domain expert
application_server_data$"mrc_description_entity_"  <- NULL
application_server_data$"legal_entity_description_entity_"  <- NULL
application_server_data$"dv_u_coe" <- NULL
application_server_data$"dv_u_primary_product_line" <- NULL
application_server_data$"dv_u_owning_company" <- NULL

#Reduce factor levels and convert to numerics
application_server_data$how_much_total_storage__gb__for_drive_letter_1_ <- as.numeric(as.character(application_server_data$how_much_total_storage__gb__for_drive_letter_1_))
application_server_data$how_much_total_storage__gb__for_drive_letter_1_[is.na(application_server_data$how_much_total_storage__gb__for_drive_letter_1_)] <- 0
application_server_data$how_much_total_storage__gb__for_drive_letter_2_ <- as.numeric(as.character(application_server_data$how_much_total_storage__gb__for_drive_letter_2_))
application_server_data$how_much_total_storage__gb__for_drive_letter_2_[is.na(application_server_data$how_much_total_storage__gb__for_drive_letter_2_)] <- 0
application_server_data$how_much_total_storage__gb__for_file_system_name_1_ <- as.numeric(as.character(application_server_data$how_much_total_storage__gb__for_file_system_name_1_))
application_server_data$how_much_total_storage__gb__for_file_system_name_1_[is.na(application_server_data$how_much_total_storage__gb__for_file_system_name_1_)] <- 0

#Remove NULL labels in location
application_server_data <- subset(application_server_data, location_ != "NULL")
application_server_data$location_ <- droplevels(application_server_data$location_)
#Fill in NA fields as NULL
application_server_data[is.na(application_server_data)] <- "NULL"

#Split data into training and test with 70/30 ratio
split <- sample.split(application_server_data, SplitRatio = 0.90)
train <- subset(application_server_data, split == TRUE)
#train_label <- train$location_
#train$location_ <- NULL
test <- subset(application_server_data, split == FALSE)
#test_label <- test$location_
#test$location_ <- NULL

#Create formula for decision tree
column_names <- colnames(application_server_data)
column_names <- column_names[column_names != "location_"]
predictors <- paste(column_names, collapse = " + ")
predictors <- gsub("location_ ", "", predictors)
formula <- paste("location_ ~ ",predictors,sep="")
#Create decision tree model
forest <- randomForest(formula=as.formula(formula), data=train, ntree=100, do.trace=T)
#tuneRF(x=train, y=train_label, ntreeTry=50, stepFactor=2, improve=0.05, trace=TRUE, plot=TRUE, doBest=FALSE)
#Predict using Decision Tree
Prediction <- predict(forest, test, type = "class")
#Calculate model accuracy
classes <- confusionMatrix(Prediction, test$location_)
overall <- classes$overall
accuracy <- overall['Accuracy']
cat(accuracy)
classes2 <- table(Prediction, test$location_)

#Visualize the model errors
plot(forest)