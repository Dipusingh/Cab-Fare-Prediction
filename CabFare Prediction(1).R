setwd("D:/projects/Project")

# Load important libraries
x= c("tidyverse", "ggplot2", "caret", "MASS", 
     "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',
      "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071"
     , "flexclust")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Load dataset
df_train= read.csv("D:/projects/Cab Fare Prediction/train_cab/train_cab.csv",
                   header = TRUE)
df_test= read.csv("D:/projects/Cab Fare Prediction/test/test.csv",
                   header = TRUE)


# get the summary of the data
summary(df_train)




##################DATA CLEANING #########################################
#Convert variables into proper datatypes
df_train$fare_amount= as.character(df_train$fare_amount)
df_train$fare_amount=  df_train$fare_amount[df_train$fare_amount != "430-"]
df_train$fare_amount=  df_train$fare_amount[df_train$fare_amount > "0"]
df_train$fare_amount= as.numeric(df_train$fare_amount)



#Missing Value Analysis
missing_val = data.frame(apply(df_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = 
  (missing_val$Missing_percentage/nrow(df_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val


# Imputing missing value
df_train$fare_amount[is.na(df_train$fare_amount)] =median(df_train$fare_amount,
                                                      na.rm = T)

df_train$passenger_count[is.na(df_train$passenger_count)] =median(df_train$passenger_count,
                                                          na.rm = T)


# Filter cordinate range for newyork 
df_train <- filter(df_train,
                        pickup_longitude < -72 &
                        pickup_longitude > -75 &
                        pickup_latitude > 40.2 &
                        pickup_latitude < 42 &
                        dropoff_longitude < -72 &
                        dropoff_longitude > -75 &
                        dropoff_latitude > 40 &
                        dropoff_latitude < 42 &
                        fare_amount >= 0 &
                        fare_amount <= 500 &
                        passenger_count < 10)
head(df_train)

#Now Split pickup date and time

df_train <- mutate(df_train,
                   pickup_datetime = ymd_hms(pickup_datetime),
                   pickup_month = as.factor(month(pickup_datetime)),
                   pickup_year = as.factor(year(pickup_datetime)),
                   #day = day(pickup_datetime),
                   dayOfWeek = as.factor(wday(pickup_datetime)),
                   pickup_hour = as.factor(hour(pickup_datetime)))


################Exploratory Data Anlysis##########################

# First check how fare amount is distributed 
g <- ggplot(df_train, aes(fare_amount))
g+geom_density(alpha= 0.8)
                      
#Lets See which year having maximum number of trip

pickup_year_FareAmount <- df_train %>%
  select(pickup_year, fare_amount) %>%
  group_by(pickup_year, fare_amount) %>%
  summarise(mean(fare_amount))

# Draw plot
ggplot(pickup_year_FareAmount, aes(x=pickup_year, y=fare_amount)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Year Vs Avg. fare", 
       caption="source: NYC") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Average fare amount over the month

pickup_month_FareAmount <- df_train %>%
  select(pickup_month, fare_amount) %>%
  group_by(pickup_month, fare_amount) %>%
  summarise(mean(fare_amount))

# Draw plot
ggplot(pickup_month_FareAmount, aes(x=pickup_month, y=fare_amount)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Month Vs Avg. fare", 
       caption="source: NYC") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Average fair per day
pickup_Day_FareAmount <- df_train %>%
  select(dayOfWeek, fare_amount) %>%
  group_by(dayOfWeek, fare_amount) %>%
  summarise(mean(fare_amount))

# Draw plot
ggplot(pickup_Day_FareAmount, aes(x=dayOfWeek, y=fare_amount)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Days Of the week Vs Avg. fare", 
       caption="source: NYC") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Average fair per hour
pickup_hr_FareAmount <- df_train %>%
  select(pickup_hour, fare_amount) %>%
  group_by(pickup_hour, fare_amount) %>%
  summarise(mean(fare_amount))

# Draw plot
ggplot(pickup_hr_FareAmount, aes(x=pickup_hour, y=fare_amount)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="picku hour  Vs Avg. fare", 
       caption="source: NYC") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#plot for pickup Location
# Creating clusters for pickup location
pickup_geoData <- select(df_train,pickup_longitude, pickup_latitude)
pickup_clusters <- flexclust::kcca(pickup_geoData, k = 15, kccaFamily("kmeans"))
pickup_geoData$pickup_cluster <- as.factor(pickup_clusters@cluster)
df_train$pickup_geoCluster <- pickup_geoData$pickup_cluster

pickup_geoDataPlot <- ggplot(pickup_geoData,aes(pickup_longitude, pickup_latitude, color = pickup_cluster))
pickup_geoDataPlot + geom_point(shape = 16, size = 0.2) +
  
  scale_colour_hue() + 
  coord_fixed() 

# Creating clusters for dropoff location
dropoff_geoData <- select(df_train, dropoff_longitude, dropoff_latitude)

dropoff_clusters <- flexclust::kcca(dropoff_geoData, k = 15,
                                    kccaFamily("kmeans"))
dropoff_geoData$dropoff_cluster <- as.factor(dropoff_clusters@cluster)
df_train$dropoff_geoCluster <- dropoff_geoData$dropoff_cluster

dropoff_geoDataPlot <- ggplot(dropoff_geoData,aes(dropoff_longitude, dropoff_latitude, color = dropoff_cluster))
dropoff_geoDataPlot + geom_point(shape = 16, size = 0.2) + 
  scale_colour_hue() + 
  coord_fixed()

# Calculating manhattan distance
df_train <- mutate(df_train, 
                   manhattanDist = abs(pickup_longitude - dropoff_longitude) + abs(pickup_latitude - dropoff_latitude))
df_train <- select(df_train, -c(pickup_datetime, pickup_hour))


###########MODELING#####################################################

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(df_train$fare_amount, p = .80, list = FALSE)
train = df_train[ train.index,]
test  = df_train[-train.index,]

################Linear regression##########################
# Since as per evaluation using python regression method provide the best output hence 
# i will use only regression for modeling here
lm.fit =lm(fare_amount~. ,data=train )
summary (lm.fit)
#Result Residual standard error: 7.367, Multiple R-squared:  0.5135
#djusted R-squared:  0.5125,  p-value: < 2.2e-16







































