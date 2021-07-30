#Loading the dataset into R
churn_data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE, stringsAsFactors = TRUE)

#Examining the data
str(churn_data)

#Converting SeniorCitizen from 0/1 to No/Yes
churn_data$SeniorCitizen <- ifelse(churn_data$SeniorCitizen == 1, "Yes", "No")
churn_data$SeniorCitizen <- as.factor(churn_data$SeniorCitizen)

#Removing the CustomerID and TotalCharges fields
churn_data <- subset(churn_data, select = -c(1,20))

#rename "No phone service" to "No" for MultipleLines
churn_data$MultipleLines <- ifelse(churn_data$MultipleLines == "No phone service", "No", ifelse(churn_data$MultipleLines == "No", "No", "Yes"))
churn_data$MultipleLines <- as.factor(churn_data$MultipleLines)

#rename "No internet service" to "No" for several different variables
churn_data$OnlineSecurity <- ifelse(churn_data$OnlineSecurity == "No internet service", "No", ifelse(churn_data$OnlineSecurity == "No", "No", "Yes"))
churn_data$OnlineSecurity <- as.factor(churn_data$OnlineSecurity)

churn_data$OnlineBackup <- ifelse(churn_data$OnlineBackup == "No internet service", "No", ifelse(churn_data$OnlineBackup == "No", "No", "Yes"))
churn_data$OnlineBackup <- as.factor(churn_data$OnlineBackup)

churn_data$DeviceProtection <- ifelse(churn_data$DeviceProtection == "No internet service", "No", ifelse(churn_data$DeviceProtection == "No", "No", "Yes"))
churn_data$DeviceProtection <- as.factor(churn_data$DeviceProtection)

churn_data$TechSupport <- ifelse(churn_data$TechSupport == "No internet service", "No", ifelse(churn_data$TechSupport == "No", "No", "Yes"))
churn_data$TechSupport <- as.factor(churn_data$TechSupport)

churn_data$StreamingTV <- ifelse(churn_data$StreamingTV == "No internet service", "No", ifelse(churn_data$StreamingTV == "No", "No", "Yes"))
churn_data$StreamingTV <- as.factor(churn_data$StreamingTV)

churn_data$StreamingMovies <- ifelse(churn_data$StreamingMovies == "No internet service", "No", ifelse(churn_data$StreamingMovies == "No", "No", "Yes"))
churn_data$StreamingMovies <- as.factor(churn_data$StreamingMovies)

