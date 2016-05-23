# extract the csv file from the ziped file

test <- read.csv(gzfile("test.csv.gz"))
train <- read.csv(gzfile("train.csv.gz"))
destinations <- read.csv(gzfile("destinations.csv.gz"))
sample_submission <- read.csv(gzfile("sample_submission.csv.gz"))

# save the extracted files to .csv files for later use

write(test,"test.csv")
write(train,"train.csv")
write(destinations,"destinations.csv")
write(sample_submission,"sample_submission.csv")

# read csv files 

test <-read.csv("test.csv")
destinations <- read.csv("destinations.csv")
sample_submission <- read.csv("sample_submission.csv")

save.image()   # saving workspace in .RData file

# as train.csv file is too big (@4 GB) read.csv is not optimal way of reading. Use fread. It takes @ 5 minutes to complete read
require(data.table)

train <- fread("train.csv")

# use the twn percent of the train data for feature engineering and building model first. After the optimal model is identified, we can come back and use the entire train data

trainTenPercent <- train[1:3767029,]

# free up some space

rm(train)


###feature engineering the training dataset

tempDate$checkin <- as.Date(tempDate$srch_ci)

tempDate$checkout <- as.Date(tempDate$srch_co)

tempDate$staylength <- tempDate$checkout - tempDate$checkin

tempDate$inquiredate <- as.Date(tempDate$date_time)

tempDate$days_inquired_in_advance <- tempDate$checkin -tempDate$inquiredate

tempDate$hour_inquired <- as.POSIXlt(tempDate$date_time)$hour + (as.POSIXlt(tempDate$date_time)$min)/60 + (as.POSIXlt(tempDate$date_time)$sec)/3600

tempDateTime1$converted1 <- as.POSIXlt(strptime(tempDateTime1[,1], "%Y-%m-%d %H:%M:%S"))

tempDateTime1$inquiry_hour <- tempDateTime1$converted1$hour + tempDateTime1$converted1$min/60 + tempDateTime1$converted1$sec/3600

trainDateTime <- cbind(tempDateTime,tempDateTime1)    
trainDateTime1 <- subset(trainDateTime[,c(4:7,9,10)])   ## choosing specific columns
names(trainDateTime1)[5] <- paste("date_time")    ## renaming column name
trainDateTime <- trainDateTime1

temp_loc$user_location_id <- paste(temp_loc$user_location_country,temp_loc$user_location_region, temp_loc$user_location_city, sep = "_")
temp_loc$hotel_location_id <- paste(temp_loc$hotel_continent,temp_loc$hotel_country,temp_loc$hotel_market,temp_loc$srch_destination_id, sep = "_")

trainDateTimeLoc <- cbind(trainDateTime,temp_loc$user_location_id,temp_loc$hotel_location_id, deparse.level = 1)

train1 <- cbind(trainDateTimeLoc,trainTenPercent$orig_destination_distance,trainTenPercent$user_id,trainTenPercent$is_mobile,trainTenPercent$is_package, trainTenPercent$channel,trainTenPercent$srch_adults_cnt,trainTenPercent$srch_children_cnt, trainTenPercent$srch_rm_cnt,trainTenPercent$srch_destination_type_id,trainTenPercent$is_booking,trainTenPercent$cnt,trainTenPercent$hotel_cluster,trainTenPercent$site_id, deparse.level = 1)


#####changing the test data set to match feature engineered training set

testDateTime <- test[,c(3,4)]
head(testDateTime)

testDateTime$checkin <- as.Date(test$srch_ci)

testDateTime$checkout <- as.Date(test$srch_co)

testDateTime$staylength <- testDateTime$checkout - testDateTime$checkin

testDateTime$inquiredate <- as.Date(test$date_time) ## does not give time information

testDateTime$days_inquired_in_advance <- testDateTime$checkin -testDateTime$inquiredate

testDateTime$converted1 <- as.POSIXlt(strptime(test[,3], "%Y-%m-%d %H:%M:%S"))

testDateTime$inquiry_hour <- testDateTime$converted1$hour + testDateTime$converted1$min/60 + testDateTime$converted1$sec/3600

colnames(testDateTime)
testDateTime <- testDateTime[,c(3:5,7,1,9)]


testDateTime$user_location_id <- paste(test$user_location_country,test$user_location_region, test$user_location_city, sep = "_")
testDateTime$hotel_location_id <- paste(test$hotel_continent,test$hotel_country,test$hotel_market,test$srch_destination_id, sep = "_")
testDateTime$site_id <- paste(test$posa_continent,test$site_name, sep = "_")

test1 <- cbind(testDateTime,test$orig_destination_distance,test$user_id,test$is_mobile,test$is_package, test$channel,test$srch_adults_cnt,test$srch_children_cnt, test$srch_rm_cnt,test$srch_destination_type_id, deparse.level = 1)

names(test1)[10] <- paste("orig_destination_distance") 
names(test1)[11] <- paste("user_id") 
names(test1)[12] <- paste("is_mobile") 
names(test1)[13] <- paste("is_package") 
names(test1)[14] <- paste("channel") 
names(test1)[15] <- paste("srch_adults_cnt") 
names(test1)[16] <- paste("srch_children_cnt") 
names(test1)[17] <- paste("srch_rm_cnt") 
names(test1)[18] <- paste("srch_destination_type_id") 

train1 <- train1[,c(1:6,21,7:17,18:20)]
train1_booked <- subset(train1, is_booking == 1)

train1_booked$date_time <- as.character(train1_booked$date_time)

## first for simplicity purpose I'll ignore the data with NA values to build model. Later I will replace missing orig_destination_distance values with the predicted values

train1_booked_noNA <- na.omit(train1_booked) ## removing data with NA values..all the NAs are in orig_destination_distance

train1_booked_noNA$checkin <- as.Date(train1_booked_noNA$checkin)
train1_booked_noNA$checkout <- as.Date(train1_booked_noNA$checkout)


## finding how many booking are done for each hotel clusters

train1_booked$hotelcluster_bookingCount <- table(train1_booked$hotel_cluster)[train1_booked$hotel_cluster+1] ##keeping cluster 0 as 0 only and not converting to 100
hotel_cluster_count <- unique(train1_booked[,21:22])
plot(hotel_cluster_count, col = "blue", lwd = 2)  


## changing the variables to numeric or factor

train1_booked_noNA$staylength <- as.numeric(train1_booked_noNA$staylength)
train1_booked_noNA$days_inquired_in_advance <- as.numeric(train1_booked_noNA$days_inquired_in_advance)

train1_booked_noNA$channel <- as.factor(train1_booked_noNA$channel)
train1_booked_noNA$srch_destination_type_id <- as.factor(train1_booked_noNA$srch_destination_type_id)
train1_booked_noNA$hotel_cluster <- as.factor(train1_booked_noNA$hotel_cluster)

## adding count of hotel_location_id to be used as one of the feature instead of using hotel_location_id that has over 31K levels

train1_booked_noNA$hotel_location_id_count <- table(train1_booked_noNA$hotel_location_id)[train1_booked_noNA$hotel_location_id]



## Buliding decision tree after feature engineering of training data

library(rpart)
DecisionTree2 <- rpart(hotel_cluster ~ checkin + checkout + staylength + days_inquired_in_advance + date_time + inquiry_hour + user_location_id + hotel_location_id     
                       + orig_destination_distance + user_id + is_mobile + is_package + channel + srch_adults_cnt +
                         srch_children_cnt + srch_rm_cnt + srch_destination_type_id + is_booking + cnt + site_id, data = train1 )


DecisionTree2 <- rpart(hotel_cluster ~ checkin + staylength + days_inquired_in_advance + user_location_id + hotel_location_id     
                       + orig_destination_distance + user_id + is_mobile + is_package + channel + srch_adults_cnt +
                         srch_children_cnt + srch_rm_cnt + srch_destination_type_id + is_booking + site_id, data = train1 )



## packages needed for fancy plot

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(DecisionTree2)

prediction_DTree2 <- predict(DecisionTree2,test, type = "class")



### subsetting data for other model building 


x_train1 <- train1_booked_noNA[1:100000, c(3,4, 6,10, 13,15,18, 22)]
y_train1 <- train1_booked_noNA[1:100000,21]

x_train1_test <- train1_booked_noNA[100001:201406, c(3,4, 6,10, 13,15,18, 22)]
y_train1_test <- train1_booked_noNA[100001:201406, 21]

library(subsemble)
library(randomForest)

learner <- c("SL.randomForest", "SL.glm")
metalearner <- c("SL.glm")
subsets <- 2

## Random forest can not be used for more than 53 categories

fit <- subsemble(x=x_train1, y=y_train1, newx=x_train1_test, family = gaussian(), 
                 learner = learner, metalearner = metalearner,
                 subsets = subsets)

pred <- predict(fit, x_train1_test)
auc <- cvAUC(predictions=pred$pred, labels=y_train1_test)$cvAUC
print(auc)

## using model from caret package

library(caret)
names(getModelInfo())      # finding different models in caret
getModelInfo()$glm$type   # finding type of the given model

library(pROC)             ##for AUC functions

modelControl <- trainControl(method = 'cv', number = 3, returnResamp = 'none')

gbm_model1 <- train(x_train1,y_train1,method = 'gbm', trControl = modelControl)


