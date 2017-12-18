##Predict location of server given other parameters recorded in server creation form
suppressPackageStartupMessages(require(caTools))
suppressPackageStartupMessages(require(rpart))
suppressPackageStartupMessages(require(rattle))
suppressPackageStartupMessages(require(rpart.plot))
suppressPackageStartupMessages(require(RColorBrewer))
suppressPackageStartupMessages(require(tree))
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(randomForest))
suppressPackageStartupMessages(require(caret))
suppressPackageStartupMessages(require(klaR))
suppressPackageStartupMessages(require(e1071))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(optparse))

##Take in user input
option_list = list(
  make_option(c("-in", "--input_file"), action="store", default=NULL, type='character',
              help="the input file to train or predict from."),
  make_option(c("-t", "--train"), action="store_false", default=FALSE,
              help="if TRUE, model will train on input data. Default FALSE."),
  make_option(c("-p", "--predict"), action="store_false", default=TRUE,
              help="if TRUE, model will predict on input data. Default TRUE."),
  make_option(c("-v", "--verbose"), action="store_false", default=TRUE,
              help="Make the program verbose in logging.")  
)
opt = parse_args(OptionParser(option_list=option_list))

#Read in data with all columns
setwd("C:/Users/Das_J/Dropbox/JnJ/Server_Location_Prediction")
application_server_data <- read.csv("application_servers_oversampled.csv", fill = T)

#Remove identifier columns
application_server_data$"dv_requested_item" <- NULL
application_server_data$"what_is_your_application_name_" <- NULL
#Remove unecessary columns identified by domain expert
application_server_data$"mrc_description_entity_"  <- NULL
application_server_data$"legal_entity_description_entity_"  <- NULL
application_server_data$"dv_u_coe" <- NULL
application_server_data$"dv_u_primary_product_line" <- NULL
application_server_data$"dv_u_owning_company" <- NULL

#Reduce factor levels on regions based on Abhishek input
application_server_data$dv_u_ci_regions <- as.character(application_server_data$dv_u_ci_regions)
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'ASPAC, EMEA, LATAM, NA'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'LATAM, ASPAC'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, ASPAC'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, ASPAC, EMEA'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, EMEA, GLOBAL'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, GLOBAL'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, LATAM'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, LATAM, ASPAC'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, LATAM, ASPAC, EMEA'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'ASPAC, EMEA, GLOBAL, LATAM, NA, EMEA'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'GLOBAL, ASPAC, EMEA, LATAM, NA, EMEA'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'NA, LATAM, ASPAC, EMEA, GLOBAL'] <- 'NA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'ASPAC, EMEA'] <- 'EMEA'
application_server_data$dv_u_ci_regions[application_server_data$dv_u_ci_regions == 'EMEA, LATAM, ASPAC'] <- 'EMEA'
application_server_data$dv_u_ci_regions <- as.factor(application_server_data$dv_u_ci_regions)
application_server_data$location_[application_server_data$dv_u_ci_regions == 'NA'] <- 'NA enterprise data center'
application_server_data$location_[application_server_data$dv_u_ci_regions == 'EMEA'] <- 'EMEA enterprise data center'

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
split <- sample.split(application_server_data, SplitRatio = 0.80)
train <- subset(application_server_data, split == TRUE)
test <- subset(application_server_data, split == FALSE)

#Create formula for decision tree
column_names <- colnames(application_server_data)
column_names <- column_names[column_names != "location_"]
predictors <- paste(column_names, collapse = " + ")
predictors <- gsub("location_ ", "", predictors)
formula <- paste("location_ ~ ",predictors,sep="")

#Create Random Forest model
model <- randomForest(formula=as.formula(formula), data=train, ntree=100, do.trace=T)
#tuneRF(x=train, y=train_label, ntreeTry=50, stepFactor=2, improve=0.05, trace=TRUE, plot=TRUE, doBest=FALSE)

#Create Decision Tree
#model <- rpart(formula = as.formula(formula), data=train, method="class")
#fancyRpartPlot(model)

#Predict using Random Forest/Decision Tree
Prediction <- predict(model, test, type = "class")
#Calculate model accuracy
classes <- confusionMatrix(Prediction, test$location_)
overall <- classes$overall
accuracy <- overall['Accuracy']
cat(accuracy)
classes2 <- table(Prediction, test$location_)

#Visualize the model errors
plot(model)