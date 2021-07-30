rm(churn)

#file found at https://www.kaggle.com/blastchar/telco-customer-churn

setwd("C:/Users/markmasson/Desktop/R Work")
churn <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE, stringsAsFactors = TRUE)

str(churn)
summary(churn)
summary(churn$TotalCharges)
churn <- na.omit(churn)

#recode SeniorCitizen from 0/1 to No/Yes
churn$SeniorCitizen <- ifelse(churn$SeniorCitizen == 1, "Yes", "No")
churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)

#recode extra variables
levels(churn$MultipleLines)[levels(churn$MultipleLines)=="No phone service"] <- "No"
levels(churn$OnlineSecurity)[levels(churn$OnlineSecurity)=="No internet service"] <- "No"
levels(churn$OnlineBackup)[levels(churn$OnlineBackup)=="No internet service"] <- "No"
levels(churn$DeviceProtection)[levels(churn$DeviceProtection)=="No internet service"] <- "No"
levels(churn$TechSupport)[levels(churn$TechSupport)=="No internet service"] <- "No"
levels(churn$StreamingTV)[levels(churn$StreamingTV)=="No internet service"] <- "No"
levels(churn$StreamingMovies)[levels(churn$StreamingMovies)=="No internet service"] <- "No"

#for the purpose of this analysis, I don't need the customerID
churn <- subset(churn, select = -1)

#also noticed NAs for TotalCharges, and it's a combination of tenure and MonthlyCharges
cor(churn[,c(5,18,19)])

churn <- subset(churn, select = -19)

#have to integers variables remaining - tenure and MonthlyCharges - need to convert to categorical factors
summary(churn$tenure)
hist(churn$tenure,main="Histogram for Tenure Variable", xlab="Tenure", border="blue", col="green")

library(gtools)
churn$tenure <- quantcut(churn$tenure,q=3)
levels(churn$tenure)[levels(churn$tenure)=="[1,14]"] <- "Low"
levels(churn$tenure)[levels(churn$tenure)=="(14,47]"] <- "Medium"
levels(churn$tenure)[levels(churn$tenure)=="(47,72]"] <- "High"
summary(churn$tenure)

#monthly charges
summary(churn$MonthlyCharges)
hist(churn$MonthlyCharges,main="Histogram for MonthlyCharges Variable", xlab="Monthly Charges", border="blue", col="red")

churn$MonthlyCharges <- quantcut(churn$MonthlyCharges,q=3)
levels(churn$MonthlyCharges)[levels(churn$MonthlyCharges)=="[18.2,50.4]"] <- "Low"
levels(churn$MonthlyCharges)[levels(churn$MonthlyCharges)=="(50.4,84]"] <- "Medium"
levels(churn$MonthlyCharges)[levels(churn$MonthlyCharges)=="(84,119]"] <- "High"
summary(churn$MonthlyCharges)

#write the clean data set to a csv
write.csv(churn, file = "masson_clean_churn_dataset.csv")

#see final data set
str(churn)

#create univariate plots
barplot(table(churn$gender), main="Gender")
barplot(table(churn$SeniorCitizen), main="SeniorCitizen")
barplot(table(churn$Partner), main="Partner")
barplot(table(churn$Dependents), main="Dependents")
barplot(table(churn$tenure), main="Tenure")
barplot(table(churn$PhoneService), main="PhoneService")
barplot(table(churn$MultipleLines), main="MultipleLines")
barplot(table(churn$InternetService), main="InternetService")
barplot(table(churn$OnlineBackup), main="OnlineBackup")
barplot(table(churn$OnlineSecurity), main="OnlineSecurity")
barplot(table(churn$DeviceProtection), main="DeviceProtection")
barplot(table(churn$TechSupport), main="TechSupport")
barplot(table(churn$StreamingTV), main="StreamingTV")
barplot(table(churn$StreamingMovies), main="StreamingMovies")
barplot(table(churn$Contract), main="Contract")
barplot(table(churn$PaperlessBilling), main="PaperlessBilling")
barplot(table(churn$PaymentMethod), main="PaymentMethod")
barplot(table(churn$MonthlyCharges), main="MonthlyCharges")
barplot(table(churn$Churn), main="Churn")

#create bivariate visual plots
library(ggplot2)
library(gridExtra)
library(stringr)

grid.arrange(
  
  ggplot(churn, aes(x=gender,fill=Churn))+ 
    geom_bar(position = "fill", show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=SeniorCitizen,fill=Churn))+ 
    geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() +
    theme_minimal(),
  
  ggplot(churn, aes(x=Partner,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) +
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=Dependents,fill=Churn))+ 
    geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=PhoneService,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=MultipleLines,fill=Churn))+ 
    geom_bar(position = 'fill')+
    labs(y = NULL) + scale_fill_ordinal() + 
    theme_minimal(), 
  
  nrow = 3)

grid.arrange(
  ggplot(churn, aes(x=InternetService,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=OnlineSecurity,fill=Churn))+ 
    geom_bar(position = 'fill')+labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=OnlineBackup,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=DeviceProtection,fill=Churn))+ 
    geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=TechSupport,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  nrow = 3 
  
)

grid.arrange(
  ggplot(churn, aes(x=StreamingMovies,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=Contract,fill=Churn))+  
    geom_bar(position = 'fill')+labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=PaperlessBilling,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
  
  ggplot(churn, aes(x=PaymentMethod,fill=Churn))+
    geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() + theme_minimal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
  
  ggplot(churn, aes(x=MonthlyCharges,fill=Churn))+  
    geom_bar(position = 'fill')+labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(), 
  
  ggplot(churn, aes(x=tenure,fill=Churn))+  
    geom_bar(position = 'fill')+labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(), 
  
  nrow=3)

#split into a 70/30 training and validation split
library(caret)
set.seed(123)
train_index <- createDataPartition(churn$Churn, p=.7, list=FALSE)
churn_train <- churn[ train_index,]
churn_validate <- churn[-train_index,]

#ensure churn breakdown is same across new training and validation data sets
summary(churn$Churn)
summary(churn_train$Churn)
summary(churn_validate$Churn)

#perform mca on training data set using factominer
require(FactoMineR)
require(factoextra)
churn_mca <- MCA(churn_train, quali.sup = 19, graph=FALSE)
fviz_screeplot(churn_mca, addlabels=TRUE)
fviz_mca_var(churn_mca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE, ggtheme = theme_minimal())
fviz_contrib(churn_mca, choice = "var", axes = 1:4, top = 25)
fviz_mca_ind(churn_mca, 
             label = "none",
             habillage = "Churn", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 
fviz_mca_var(churn_mca, choice = "mca.cor", repel=TRUE, ggtheme = theme_minimal())
churn_eig <- get_eigenvalue(churn_mca)
churn_var <- get_mca_var(churn_mca)

#perform logistic regression on training data
churn_model <- glm(formula = Churn ~ ., family=binomial(link = "logit"), data = churn_train)
summary(churn_model)
probabilities_full <- predict(churn_model, churn_validate, type = "response")
predicted.classes_full <- ifelse(probabilities_full > 0.5, "Yes", "No")
observed.classes_full <- churn_validate$Churn
mean(predicted.classes_full == observed.classes_full)

library(MASS)
churn_stepwise <- stepAIC(churn_model, direction="backward")
summary(churn_stepwise)
probabilities <- predict(churn_stepwise, churn_validate, type = "response")
predicted.classes <- ifelse(probabilities > 0.7, "Yes", "No")
observed.classes <- churn_validate$Churn
mean(predicted.classes == observed.classes)

churn_forward <- stepAIC(churn_model, direction="forward")
summary(churn_forward)
anova(churn_forward, test="Chisq")

churn_both <- stepAIC(churn_model, direction="both")
summary(churn_both)
anova(churn_both, test="Chisq")

#table of deviance
anova(churn_stepwise, test="Chisq")

#odds ratios on stepwise model
exp(coef(churn_stepwise))

#ROC curve
install.packages("pROC")
library(pROC)
churn_roc <- plot.roc(churn_validate$Churn, probabilities, 
                      identity.col = "red", print.auc=TRUE,auc.polygon=TRUE,auc.polygon.col="blue", main = "ROC Curve - Churn")


