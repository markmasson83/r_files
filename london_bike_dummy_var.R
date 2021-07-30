###rm(list = ls())
#load in dataset
getwd()

bike <- read.csv("london_bike.csv",header = TRUE)
str(bike)
head(bike)

options(scipen=250)



#reorder variables to bring dependent (cnt) to first column
bike <- bike[, c(2,1,3,4,5,6,7,8,9,10)]

#split the date time into different columns - month, day, year, hour
library(tidyverse)
bike <- separate(bike, timestamp, into = c("month","day","year"), sep = "/")
bike <- separate(bike, year, into = c("year","hour"), sep = " ")

#remake date column and retrieve day of week
#convert new columns to factors for later regression
library(lubridate)
bike$date <- as.Date(with(bike, paste(month, day, year,sep="-")), "%m-%d-%Y")
bike$day_of_week <- weekdays(bike$date)
bike$month <- as.factor(bike$month)
bike$day <- as.factor(bike$day)
bike$year <- as.factor(bike$year)
bike$hour <- as.factor(bike$hour)

summary(bike)

#create a data frame of variables to run correlation plot
library(corrplot)
bike_corr <- bike[,c(1,6,7,8,9,10,11,12,13)]
corrplot(cor(bike_corr), type="lower", method="number")
attach(bike)
rm(bike_corr) #don't need this moving forward

#t1 and t2 are highly correlated (0.99).  Remove t2.
bike <- bike[,-7]

#assumption is month and season are pretty much the same
#concatenate the two columns and deduplicate
month_and_year <- paste(bike$month,bike$season)
unique_m_and_y <- unique(month_and_year)
unique_m_and_y
rm(month_and_year) #don't need moving forward
rm(unique_m_and_y) #don't need moving forward

#remove season column
bike <- bike[,-12]

#weather_code, is_holiday, is_weekend, and day of week need to be categorical factors
bike$weather_code <- as.factor(bike$weather_code)
bike$is_holiday <- as.factor(bike$is_holiday)
bike$is_weekend <- as.factor(bike$is_weekend)
bike$day_of_week <- as.factor(bike$day_of_week)

#rename columns to make more sense
bike <- bike %>% rename(bikes_rented = cnt,
                        temperature = t1,
                        humidity = hum)

#create a is_workday column
bike$is_workday <- ifelse(is_weekend == 0 & is_holiday == 0, "1", "0")
bike$is_workday <- as.factor(bike$is_workday)

summary(bike)

#only 72 records in 2017, remove for cleanliness
#one record of 0, remove that record
bike <- bike[ which(bike$year == 2015 | bike$year == 2016),]
bike <- bike[ which(bike$bikes_rented > 0),]

#remove dates that seem irregular

#dates with less than 24 records
counts_by_day <- count(bike, date) #8 dates have less than 20 hours of records, will remove those in case of bad data
view(counts_by_day)
#dates with extremely high totals
days_sum <- aggregate(bike[,1], list(bike$date, bike$day_of_week), sum) #7/9 and 8/6 strikes
view(days_sum)

bike <- bike[ which(bike$date != "2016-06-24"),] #less than 20 records
bike <- bike[ which(bike$date != "2016-09-03"),] #less than 20 records
bike <- bike[ which(bike$date != "2016-03-27"),] #less than 20 records
bike <- bike[ which(bike$date != "2015-09-12"),] #less than 20 records
bike <- bike[ which(bike$date != "2015-10-13"),] #less than 20 records
bike <- bike[ which(bike$date != "2015-11-18"),] #less than 20 records
bike <- bike[ which(bike$date != "2016-02-08"),] #less than 20 records
bike <- bike[ which(bike$date != "2016-03-26"),] #less than 20 records
bike <- bike[ which(bike$date != "2015-07-09"),] #strikes
bike <- bike[ which(bike$date != "2015-08-06"),] #strikes
rm(counts_by_day) #don't need this moving forward
rm(days_sum) #don't need this moving forward

#find averages for each day/hour and see if there are potential influential variables
# bike_dayhour <- bike[ , c(13,5,1)]
# bike_dayhour$dayhour <- paste(bike_dayhour$day_of_week, bike_dayhour$hour)
# bike_dayhour <- aggregate(bike_dayhour[,3], list(bike_dayhour$dayhour), mean)
# bike_dayhour <- bike_dayhour %>% rename(dayhour = Group.1)
# bike2 <- bike
# bike2$dayhour <- paste(bike2$day_of_week, bike2$hour)
# bike3 <- merge(bike2,bike_dayhour,by="dayhour")
# bike3$diff <- bike3$bikes_rented - bike3$x
# bike3 <- bike3[,c(13,1,2,16,17,3,4,5,6,7,8,9,10,11,12,14,15)]
# bike <- bike3[ which(bike3$diff <= 1500 & bike3$diff >= -1500 ),]

summary(bike)
head(bike)

##############################################
# make dummy variables for linear regression #
##############################################

#dummy code month, day of week, hours, weather
bike <- bike %>% mutate(is_jan = case_when(month == "1" ~ 1, month != "1" ~ 0))
bike <- bike %>% mutate(is_feb = case_when(month == "2" ~ 1, month != "2" ~ 0))
bike <- bike %>% mutate(is_mar = case_when(month == "3" ~ 1, month != "3" ~ 0))
bike <- bike %>% mutate(is_apr = case_when(month == "4" ~ 1, month != "4" ~ 0))
bike <- bike %>% mutate(is_may = case_when(month == "5" ~ 1, month != "5" ~ 0))
bike <- bike %>% mutate(is_jun = case_when(month == "6" ~ 1, month != "6" ~ 0))
bike <- bike %>% mutate(is_jul = case_when(month == "7" ~ 1, month != "7" ~ 0))
bike <- bike %>% mutate(is_aug = case_when(month == "8" ~ 1, month != "8" ~ 0))
bike <- bike %>% mutate(is_sep = case_when(month == "9" ~ 1, month != "9" ~ 0))
bike <- bike %>% mutate(is_oct = case_when(month == "10" ~ 1, month != "10" ~ 0))
bike <- bike %>% mutate(is_nov = case_when(month == "11" ~ 1, month != "11" ~ 0))
bike <- bike %>% mutate(is_dec = case_when(month == "12" ~ 1, month != "12" ~ 0))

bike <- bike %>% mutate(day_sun = case_when(day_of_week == "Sunday" ~ 1, day_of_week != "Sunday" ~ 0))
bike <- bike %>% mutate(day_mon = case_when(day_of_week == "Monday" ~ 1, day_of_week != "Monday" ~ 0))
bike <- bike %>% mutate(day_tue = case_when(day_of_week == "Tuesday" ~ 1, day_of_week != "Tuesday" ~ 0))
bike <- bike %>% mutate(day_wed = case_when(day_of_week == "Wednesday" ~ 1, day_of_week != "Wednesday" ~ 0))
bike <- bike %>% mutate(day_thu = case_when(day_of_week == "Thursday" ~ 1, day_of_week != "Thursday" ~ 0))
bike <- bike %>% mutate(day_fri = case_when(day_of_week == "Friday" ~ 1, day_of_week != "Friday" ~ 0))
bike <- bike %>% mutate(day_sat = case_when(day_of_week == "Saturday" ~ 1, day_of_week != "Saturday" ~ 0))

bike <- bike %>% mutate(is_0 = case_when(hour == "0:00" ~ 1, hour != "0:00" ~ 0))
bike <- bike %>% mutate(is_1 = case_when(hour == "1:00" ~ 1, hour != "1:00" ~ 0))
bike <- bike %>% mutate(is_2 = case_when(hour == "2:00" ~ 1, hour != "2:00" ~ 0))
bike <- bike %>% mutate(is_3 = case_when(hour == "3:00" ~ 1, hour != "3:00" ~ 0))
bike <- bike %>% mutate(is_4 = case_when(hour == "4:00" ~ 1, hour != "4:00" ~ 0))
bike <- bike %>% mutate(is_5 = case_when(hour == "5:00" ~ 1, hour != "5:00" ~ 0))
bike <- bike %>% mutate(is_6 = case_when(hour == "6:00" ~ 1, hour != "6:00" ~ 0))
bike <- bike %>% mutate(is_7 = case_when(hour == "7:00" ~ 1, hour != "7:00" ~ 0))
bike <- bike %>% mutate(is_8 = case_when(hour == "8:00" ~ 1, hour != "8:00" ~ 0))
bike <- bike %>% mutate(is_9 = case_when(hour == "9:00" ~ 1, hour != "9:00" ~ 0))
bike <- bike %>% mutate(is_10 = case_when(hour == "10:00" ~ 1, hour != "10:00" ~ 0))
bike <- bike %>% mutate(is_11 = case_when(hour == "11:00" ~ 1, hour != "11:00" ~ 0))
bike <- bike %>% mutate(is_12 = case_when(hour == "12:00" ~ 1, hour != "12:00" ~ 0))
bike <- bike %>% mutate(is_13 = case_when(hour == "13:00" ~ 1, hour != "13:00" ~ 0))
bike <- bike %>% mutate(is_14 = case_when(hour == "14:00" ~ 1, hour != "14:00" ~ 0))
bike <- bike %>% mutate(is_15 = case_when(hour == "15:00" ~ 1, hour != "15:00" ~ 0))
bike <- bike %>% mutate(is_16 = case_when(hour == "16:00" ~ 1, hour != "16:00" ~ 0))
bike <- bike %>% mutate(is_17 = case_when(hour == "17:00" ~ 1, hour != "17:00" ~ 0))
bike <- bike %>% mutate(is_18 = case_when(hour == "18:00" ~ 1, hour != "18:00" ~ 0))
bike <- bike %>% mutate(is_19 = case_when(hour == "19:00" ~ 1, hour != "19:00" ~ 0))
bike <- bike %>% mutate(is_20 = case_when(hour == "20:00" ~ 1, hour != "20:00" ~ 0))
bike <- bike %>% mutate(is_21 = case_when(hour == "21:00" ~ 1, hour != "21:00" ~ 0))
bike <- bike %>% mutate(is_22 = case_when(hour == "22:00" ~ 1, hour != "22:00" ~ 0))
bike <- bike %>% mutate(is_23 = case_when(hour == "23:00" ~ 1, hour != "23:00" ~ 0))

bike <- bike %>% mutate(is_clear = case_when(weather_code == "1" ~ 1, weather_code != "1" ~ 0))
bike <- bike %>% mutate(is_scatclouds = case_when(weather_code == "2" ~ 1, weather_code != "2" ~ 0))
bike <- bike %>% mutate(is_brokclouds = case_when(weather_code == "3" ~ 1, weather_code != "3" ~ 0))
bike <- bike %>% mutate(is_cloudy = case_when(weather_code == "4" ~ 1, weather_code != "4" ~ 0))
bike <- bike %>% mutate(is_lightrain = case_when(weather_code == "7" ~ 1, weather_code != "7" ~ 0))
bike <- bike %>% mutate(is_thunderstorm = case_when(weather_code == "10" ~ 1, weather_code != "10" ~ 0))

#remove unneeded columns for regression ###I don't have to do this
bike <- bike[,-c(2,3,4,5,9,12,13)]

###################################################################
# begin analysis of data, starting with exploratory data analysis #
###################################################################

#histogram and frequency curve for bikes rented
hist(bike$bikes_rented, main="Histogram of Bikes Rented each Hour", xlab="Bikes Rented", col="darkmagenta")
library(ggplot2)
ggplot(bike, aes(x = bikes_rented)) + geom_density(aes(y = ..count../10), fill="red") +
  geom_vline(aes(xintercept = mean(bikes_rented)), linetype = "solid", color = "blue") +
  scale_y_continuous(name = "Frequency") + ggtitle("Frequency Curve of Bikes Rented")

#change factors for boxplot to ordered for day of week, month and hour
bike$weather_code <- factor(
  bike$weather_code,
  levels = c(1,2,3,4,7,10,26),
  labels = c('clear', 'scattered.clouds', 'broken.clouds', 'cloudy','light.rain','thunderstorm','snowfall'))

bike$month <- factor(
  bike$month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  labels = c('January','February', 'March', 'April',
             'May', 'June', 'July', 'August',
             'September','October', 'November', 'December'),
  ordered = TRUE)

bike$hour <- factor(
  bike$hour, levels = c("0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00",
                        "14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"),
  labels = c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23'),
  ordered = TRUE)

bike$day_of_week <- factor(bike$day_of_week, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                           labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                           ordered = TRUE)


#boxplots
ggplot(bike, aes(x=year, y=bikes_rented, color=year)) + geom_boxplot()
ggplot(bike, aes(x=month, y=bikes_rented, color=month)) + geom_boxplot()
ggplot(bike, aes(x=day_of_week, y=bikes_rented, color=day_of_week)) + geom_boxplot()
ggplot(bike, aes(x=is_workday, y=bikes_rented, color=is_workday)) + geom_boxplot()
ggplot(bike, aes(x=weather_code, y=bikes_rented, color=weather_code)) + geom_boxplot()
ggplot(bike, aes(x=hour, y=bikes_rented, color=hour)) + geom_boxplot()
ggplot(bike, aes(x=hour, y=bikes_rented, color=is_workday)) + geom_boxplot() #boxplot by hour divided up by workday

ggplot(bike, aes(x=temperature, y=bikes_rented)) + geom_point() + geom_smooth(method=lm)
ggplot(bike, aes(x=humidity, y=bikes_rented)) + geom_point() + geom_smooth(method=lm)
ggplot(bike, aes(x=wind_speed, y=bikes_rented)) + geom_point() + geom_smooth(method=lm)

#########################################################
# linear regression on each individual set of variables #
#########################################################

#linear regression of temperature
fit_temp <- lm(bikes_rented ~ temperature, data = bike)
summary(fit_temp)

#linear regression of humidity
fit_humidity <- lm(bikes_rented ~ humidity, data = bike)
summary(fit_humidity)

#linear regression of wind speed
fit_wind_speed <- lm(bikes_rented ~ wind_speed, data = bike)
summary(fit_wind_speed)

#linear regression of day of week
fit_day <- lm(bikes_rented ~ day_sun + day_mon + day_tue + day_wed + day_thu + day_fri, data = bike)
summary(fit_day)

#linear regression for is_workday
fit_workday <- lm(bikes_rented ~ is_workday, data = bike)
summary(fit_workday)

#linear regression of hour of day
fit_hour <- lm(bikes_rented ~ is_0 + is_1 + is_2 + is_3 + is_4 + is_5 + is_0 + is_1 + is_2 + is_3 + is_4 + is_5 +
                 is_6 + is_7 + is_8 + is_9 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 +
                 is_19 + is_20 + is_21 + is_22, data = bike)
summary(fit_hour)

#linear regression of month
fit_month <- lm(bikes_rented ~ is_jan + is_feb + is_mar + is_apr + is_may + is_jun + is_jul + is_aug + is_sep + is_oct +
                  is_nov, data = bike)
summary(fit_month)

#linear regression of year
fit_year <- lm(bikes_rented ~ year, data = bike)
summary(fit_year)

#linear regression for weather
fit_weather <- lm(bikes_rented ~ is_clear + is_scatclouds + is_brokclouds + is_cloudy + is_lightrain + is_thunderstorm, 
                  data = bike)
summary(fit_weather)

##########################################################
# create full linear regression model with all variables #
##########################################################

#split into 70/30 training and validation data set
training_size <- 0.70
set.seed(456)
training_rows <- sample(seq_len(nrow(bike)), size = floor(training_size * nrow(bike)))
bike_train <- bike[training_rows, ]
bike_valid <- bike[-training_rows, ]
rm(training_rows)
rm(training_size)

#run a full linear regression
train_fit <- lm(bikes_rented ~ temperature + humidity + wind_speed + is_workday +
                  is_0 + is_1 + is_2 + is_3 + is_4 + is_5 + is_6 + is_7 + is_8 + is_9 + is_10 + is_11 + is_12 +
                  is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22 +
                  is_feb + is_mar + is_apr + is_may + is_jun + is_jul + is_aug + is_sep + is_oct + is_nov + is_dec +
                  is_workday*is_0 + is_workday*is_1 + is_workday*is_2 + is_workday*is_3 + is_workday*is_4 + is_workday*is_5 + 
                  is_workday*is_6 + is_workday*is_7 + is_workday*is_8 + is_workday*is_9 + is_workday*is_10 + is_workday*is_11 + 
                  is_workday*is_12 + is_workday*is_13 + is_workday*is_14 + is_workday*is_15 + is_workday*is_16 + is_workday*is_17 + 
                  is_workday*is_18 + is_workday*is_19 + is_workday*is_20 + is_workday*is_21 + is_workday*is_22 + is_clear + is_scatclouds +
                  is_brokclouds + is_cloudy + is_lightrain + is_thunderstorm, data = bike_train)
summary(train_fit)


train_fit_only_workday_hour <- lm(bikes_rented ~ is_workday + is_0 + is_1 + is_2 + is_3 + is_4 + is_5 + is_6 + is_7 + is_8 + is_9 + is_10 + is_11 + is_12 +
                                    is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22 +
                                    is_workday*is_0 + is_workday*is_1 + is_workday*is_2 + is_workday*is_3 + is_workday*is_4 + is_workday*is_5 + 
                                    is_workday*is_6 + is_workday*is_7 + is_workday*is_8 + is_workday*is_9 + is_workday*is_10 + is_workday*is_11 + 
                                    is_workday*is_12 + is_workday*is_13 + is_workday*is_14 + is_workday*is_15 + is_workday*is_16 + is_workday*is_17 + 
                                    is_workday*is_18 + is_workday*is_19 + is_workday*is_20 + is_workday*is_21 + is_workday*is_22, data = bike_train)
summary(train_fit_only_workday_hour)

#histogram of train_fit resdiuals
hist(train_fit$residuals)

library(car)
vif(train_fit)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(train_fit, id.n = 3)
par(mfrow=c(1,1)) # Change back to 1 x 1

#predictors on validation data set
bike_valid$predicted <- predict(train_fit, newdata = bike_valid)
bike_valid$residual <- bike_valid$predicted - bike_valid$bikes_rented

qqnorm(bike_valid$residual)
qqline(bike_valid$residual)

#histograms of residuals
hist(train_fit$residual)
hist(bike_valid$residual)

attach(bike_valid)
plot(bikes_rented, residual)
abline(lm(residual ~ bikes_rented, data = bike_valid), col = "blue")

#residual standard error of training data set and validation data set
summary(train_fit)$sigma #training
sqrt(sum((predict(train_fit, bike_valid)-bike_valid$bikes_rented)^2)/(nrow(bike_valid)-67)) #validation


write.csv(bike,"C:/Users/markm/Documents/R/bike.csv",row.names = FALSE)


############################################################
# remove outliers on main data then try another regression #
############################################################

outliers <- boxplot(bike$bikes_rented, plot=FALSE)$out
outlier_records <- bike[which(bike$bikes_rented %in% outliers),]
records_to_remove <- as.vector(outlier_records$bikes_rented)
bike_noout <- bike[ ! bike$bikes_rented %in% records_to_remove,]
rm(outliers)
rm(outlier_records)
rm(records_to_remove)

#split into 70/30 training and validation data set
training_size <- 0.70
set.seed(456)
training_rows <- sample(seq_len(nrow(bike_noout)), size = floor(training_size * nrow(bike_noout)))
bike_noout_train <- bike_noout[training_rows, ]
bike_noout_valid <- bike_noout[-training_rows, ]
rm(training_rows)
rm(training_size)

train_noout_fit <- lm(bikes_rented ~ temperature + humidity + wind_speed + is_workday +
                  is_0 + is_1 + is_2 + is_3 + is_4 + is_5 + is_6 + is_7 + is_8 + is_9 + is_10 + is_11 + is_12 +
                  is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22 +
                  is_feb + is_mar + is_apr + is_may + is_jun + is_jul + is_aug + is_sep + is_oct + is_nov + is_dec +
                is_workday*is_0 + is_workday*is_1 + is_workday*is_2 + is_workday*is_3 + is_workday*is_4 + is_workday*is_5 + 
                  is_workday*is_6 + is_workday*is_7 + is_workday*is_8 + is_workday*is_9 + is_workday*is_10 + is_workday*is_11 + 
                  is_workday*is_12 + is_workday*is_13 + is_workday*is_14 + is_workday*is_15 + is_workday*is_16 + is_workday*is_17 + 
                  is_workday*is_18 + is_workday*is_19 + is_workday*is_20 + is_workday*is_21 + is_workday*is_22 + is_clear + is_scatclouds +
                  is_brokclouds + is_cloudy + is_lightrain + is_thunderstorm, data = bike_noout_train)
summary(train_noout_fit)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(train_noout_fit, id.n = 5)
par(mfrow=c(1,1)) # Change back to 1 x 1

#cooks distance
plot(train_noout_fit, 4, id.n = 10)



#TEMP - records from bike that are influential
bike_inf <- bike[c(8525,8501,8477,5254,17080,17128,12578,17012,17015,12098),]


#############################################
# can we run a linear model by day instead? #
#############################################

days_sum <- aggregate(bike[,1], list(bike$date, bike$day_of_week), sum)
days_avg <- aggregate(bike[,c(6,7,8)], list(bike$date, bike$day_of_week), mean)
by_day <- cbind(days_sum, days_avg)
rm(days_avg) #remove data set that was combined
rm(days_sum) #remove data set that was combined
by_day <- by_day[,-c(4,5)] #remove duplicated columns

#dummy code day of week for this daily data frame
by_day <- by_day %>% mutate(day_sun = case_when(Group.2 == "Sunday" ~ 1, Group.2 != "Sunday" ~ 0))
by_day <- by_day %>% mutate(day_mon = case_when(Group.2 == "Monday" ~ 1, Group.2 != "Monday" ~ 0))
by_day <- by_day %>% mutate(day_tue = case_when(Group.2 == "Tuesday" ~ 1, Group.2 != "Tuesday" ~ 0))
by_day <- by_day %>% mutate(day_wed = case_when(Group.2 == "Wednesday" ~ 1, Group.2 != "Wednesday" ~ 0))
by_day <- by_day %>% mutate(day_thu = case_when(Group.2 == "Thursday" ~ 1, Group.2 != "Thursday" ~ 0))
by_day <- by_day %>% mutate(day_fri = case_when(Group.2 == "Friday" ~ 1, Group.2 != "Friday" ~ 0))
by_day <- by_day %>% mutate(day_sat = case_when(Group.2 == "Saturday" ~ 1, Group.2 != "Saturday" ~ 0))

#pull month from date
library(lubridate)
by_day$month <- month(by_day$Group.1)

#dummy code month for this daily data frame
by_day <- by_day %>% mutate(is_jan = case_when(month == "1" ~ 1, month != "1" ~ 0))
by_day <- by_day %>% mutate(is_feb = case_when(month == "2" ~ 1, month != "2" ~ 0))
by_day <- by_day %>% mutate(is_mar = case_when(month == "3" ~ 1, month != "3" ~ 0))
by_day <- by_day %>% mutate(is_apr = case_when(month == "4" ~ 1, month != "4" ~ 0))
by_day <- by_day %>% mutate(is_may = case_when(month == "5" ~ 1, month != "5" ~ 0))
by_day <- by_day %>% mutate(is_jun = case_when(month == "6" ~ 1, month != "6" ~ 0))
by_day <- by_day %>% mutate(is_jul = case_when(month == "7" ~ 1, month != "7" ~ 0))
by_day <- by_day %>% mutate(is_aug = case_when(month == "8" ~ 1, month != "8" ~ 0))
by_day <- by_day %>% mutate(is_sep = case_when(month == "9" ~ 1, month != "9" ~ 0))
by_day <- by_day %>% mutate(is_oct = case_when(month == "10" ~ 1, month != "10" ~ 0))
by_day <- by_day %>% mutate(is_nov = case_when(month == "11" ~ 1, month != "11" ~ 0))
by_day <- by_day %>% mutate(is_dec = case_when(month == "12" ~ 1, month != "12" ~ 0))

#append daily data frame with holiday
bike_holidays <- bike[ which(bike$is_holiday == "1"),]
bike_holidays$date <- as.character(bike_holidays$date)
bike_holidays <- bike_holidays[,13]
library(dplyr)
by_day$Group.1 <- as.character(by_day$Group.1)
by_day <- by_day %>% mutate(is_holiday = case_when(Group.1 %in% bike_holidays ~ 1))
by_day[is.na(by_day)] <- 0
rm(bike_holidays) #remove vector used to define these dates

#append data frame with whether it rained or not
bike_rain_days <- bike[ which(bike$is_lightrain == 1),]
bike_rain_days$date <- as.character(bike_rain_days$date)
bike_rain_days <- bike_rain_days[,13]
by_day <- by_day %>% mutate(rained = case_when(Group.1 %in% bike_rain_days ~ 1))
by_day[is.na(by_day)] <- 0
rm(bike_rain_days) #remove vector used to define these dates

boxplot(by_day$x ~ by_day$Group.2)
hist(by_day$x)

#create daily linear model
daily_fit <- lm(x ~ day_sun + day_mon + day_tue + day_wed + day_thu + day_fri + is_jan + is_apr +
                  is_may + is_jun + is_jul + is_aug + is_sep + is_oct + is_nov + temperature + humidity + wind_speed + 
                  is_holiday + rained, data = by_day)
summary(daily_fit)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(daily_fit)
par(mfrow=c(1,1)) # Change back to 1 x 1

#observations for christmas eve, day, remove and rerun model
by_day_no_xmas <- by_day[ which(by_day$Group.1 != "2016-12-24" & by_day$Group.1 != "2016-12-25" & by_day$Group.1 != "2016-12-26"
                                & by_day$Group.1 != "2015-12-24" & by_day$Group.1 != "2015-12-25" & by_day$Group.1 != "2015-12-26"),]

daily_fit2 <- lm(x ~ day_sun + day_mon + day_tue + day_wed + day_thu + day_fri + is_jan + is_apr +
                   is_may + is_jun + is_jul + is_aug + is_sep + is_oct + is_nov + temperature + humidity + wind_speed + 
                  is_may*temperature + is_jul*temperature + is_aug*temperature + is_sep*temperature, 
                 data = by_day_no_xmas)
summary(daily_fit2)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(daily_fit2,id.n = 5)
par(mfrow=c(1,1)) # Change back to 1 x 1

residuals_fit2 <- daily_fit2$residuals

by_day_no_xmas$predicted <- predict(daily_fit2, newdata = by_day_no_xmas)
by_day_no_xmas$residual <- residuals_fit2

hist(by_day_no_xmas$residual)
