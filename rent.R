#read in data file

setwd("C:/Users/markmasson/Desktop/R Work")
rent <- read.csv("price.csv")
str(rent)
summary(rent)

#remove Months with NAs
rent <- rent[, -c(7:23)]


#transpose data usin melt feature of reshape2 package
library(reshape2)
rent_transpose <- melt(rent, id.vars=c("City.Code","City","Metro","County","State","Population.Rank"))
names(rent_transpose)[names(rent_transpose) == "value"] <- "median_rent"
str(rent_transpose)

#separate month and year
library(tidyverse)
rent_transpose <- separate(rent_transpose, variable, into = c("month","year"), sep = -4 )
rent_transpose$year <- as.integer(rent_transpose$year)

#remove period at end of year
library(stringr)
rent_transpose$month <- as.factor(str_remove(rent_transpose$month, "\\.$"))

#combine county and state columns since some county names are duplicated in different states
rent_transpose$county_and_state = paste(rent_transpose$County,rent_transpose$State)
rent_transpose$metro_and_state = paste(rent_transpose$Metro,rent_transpose$State)

#create a data set of just 2017 January, look for potential outliers
rent_jan2017 <- rent_transpose[ which(rent_transpose$year == 2017),]
boxplot(rent_jan2017$median_rent)$out
outliers2017 <- boxplot(rent_jan2017$median_rent, plot=FALSE)$out
outlier_cities <- rent_jan2017[which(rent_jan2017$median_rent %in% outliers2017),]
cities_to_remove <- as.vector(outlier_cities$City.Code)


rent_transpose <- rent_transpose[ ! rent_transpose$City.Code %in% cities_to_remove,]



#assign tiers to each STATE and create vectors of the STATE names
state_avg <- aggregate(rent_transpose[,9], list(rent_transpose$State), mean)

library(gtools)
state_avg$tier <- quantcut(state_avg$x,q=10)
summary(state_avg$tier)
levels(state_avg$tier)[levels(state_avg$tier)=="[904,1.02e+03]"] <- "1"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.02e+03,1.09e+03]"] <- "2"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.09e+03,1.12e+03]"] <- "3"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.12e+03,1.16e+03]"] <- "4"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.16e+03,1.22e+03]"] <- "5"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.22e+03,1.3e+03]"] <- "6"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.3e+03,1.41e+03]"] <- "7"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.41e+03,1.57e+03]"] <- "8"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.57e+03,1.8e+03]"] <- "9"
levels(state_avg$tier)[levels(state_avg$tier)=="(1.8e+03,2.47e+03]"] <- "10"


tier1_states <- data.frame(state_avg[which(state_avg$tier == 1),])
states_tier1 <- as.vector(tier1_states$Group.1)
rm(tier1_states)

tier2_states <- data.frame(state_avg[which(state_avg$tier == 2),])
states_tier2 <- as.vector(tier2_states$Group.1)
rm(tier2_states)

tier3_states <- data.frame(state_avg[which(state_avg$tier == 3),])
states_tier3 <- as.vector(tier3_states$Group.1)
rm(tier3_states)

tier4_states <- data.frame(state_avg[which(state_avg$tier == 4),])
states_tier4 <- as.vector(tier4_states$Group.1)
rm(tier4_states)

tier5_states <- data.frame(state_avg[which(state_avg$tier == 5),])
states_tier5 <- as.vector(tier5_states$Group.1)
rm(tier5_states)

tier6_states <- data.frame(state_avg[which(state_avg$tier == 6),])
states_tier6 <- as.vector(tier6_states$Group.1)
rm(tier6_states)

tier7_states <- data.frame(state_avg[which(state_avg$tier == 7),])
states_tier7 <- as.vector(tier7_states$Group.1)
rm(tier7_states)

tier8_states <- data.frame(state_avg[which(state_avg$tier == 8),])
states_tier8 <- as.vector(tier8_states$Group.1)
rm(tier8_states)

tier9_states <- data.frame(state_avg[which(state_avg$tier == 9),])
states_tier9 <- as.vector(tier9_states$Group.1)
rm(tier9_states)

tier10_states <- data.frame(state_avg[which(state_avg$tier == 10),])
states_tier10 <- as.vector(tier10_states$Group.1)
rm(tier10_states)

#assign a tier to new column based on the average rent value in data set
library(dplyr)
rent_transpose <- rent_transpose %>% mutate(state_tier = case_when(State %in% states_tier1 ~ 1,
                                           State %in% states_tier2 ~ 2,
                                           State %in% states_tier3 ~ 3,
                                           State %in% states_tier4 ~ 4,
                                           State %in% states_tier5 ~ 5,
                                           State %in% states_tier6 ~ 6,
                                           State %in% states_tier7 ~ 7,
                                           State %in% states_tier8 ~ 8,
                                           State %in% states_tier9 ~ 9,
                                           State %in% states_tier10 ~ 10))

#remove state avg data to keep things clean
rm(state_avg)
rm(states_tier1)
rm(states_tier2)
rm(states_tier3)
rm(states_tier4)
rm(states_tier5)
rm(states_tier6)
rm(states_tier7)
rm(states_tier8)
rm(states_tier9)
rm(states_tier10)

#assign tiers to each COUNTY and create vectors of the COUNTY names
county_avg <- aggregate(rent_transpose[,9], list(rent_transpose$county_and_state), mean)

library(gtools)
county_avg$tier <- quantcut(county_avg$x,q=10)
summary(county_avg$tier)
levels(county_avg$tier)[levels(county_avg$tier)=="[696,855]"] <- "1"
levels(county_avg$tier)[levels(county_avg$tier)=="(855,917]"] <- "2"
levels(county_avg$tier)[levels(county_avg$tier)=="(917,972]"] <- "3"
levels(county_avg$tier)[levels(county_avg$tier)=="(972,1.03e+03]"] <- "4"
levels(county_avg$tier)[levels(county_avg$tier)=="(1.03e+03,1.09e+03]"] <- "5"
levels(county_avg$tier)[levels(county_avg$tier)=="(1.09e+03,1.16e+03]"] <- "6"
levels(county_avg$tier)[levels(county_avg$tier)=="(1.16e+03,1.23e+03]"] <- "7"
levels(county_avg$tier)[levels(county_avg$tier)=="(1.23e+03,1.35e+03]"] <- "8"
levels(county_avg$tier)[levels(county_avg$tier)=="(1.35e+03,1.58e+03]"] <- "9"
levels(county_avg$tier)[levels(county_avg$tier)=="(1.58e+03,5.24e+03]"] <- "10"


tier1_countys <- data.frame(county_avg[which(county_avg$tier == 1),])
countys_tier1 <- as.vector(tier1_countys$Group.1)
rm(tier1_countys)

tier2_countys <- data.frame(county_avg[which(county_avg$tier == 2),])
countys_tier2 <- as.vector(tier2_countys$Group.1)
rm(tier2_countys)

tier3_countys <- data.frame(county_avg[which(county_avg$tier == 3),])
countys_tier3 <- as.vector(tier3_countys$Group.1)
rm(tier3_countys)

tier4_countys <- data.frame(county_avg[which(county_avg$tier == 4),])
countys_tier4 <- as.vector(tier4_countys$Group.1)
rm(tier4_countys)

tier5_countys <- data.frame(county_avg[which(county_avg$tier == 5),])
countys_tier5 <- as.vector(tier5_countys$Group.1)
rm(tier5_countys)

tier6_countys <- data.frame(county_avg[which(county_avg$tier == 6),])
countys_tier6 <- as.vector(tier6_countys$Group.1)
rm(tier6_countys)

tier7_countys <- data.frame(county_avg[which(county_avg$tier == 7),])
countys_tier7 <- as.vector(tier7_countys$Group.1)
rm(tier7_countys)

tier8_countys <- data.frame(county_avg[which(county_avg$tier == 8),])
countys_tier8 <- as.vector(tier8_countys$Group.1)
rm(tier8_countys)

tier9_countys <- data.frame(county_avg[which(county_avg$tier == 9),])
countys_tier9 <- as.vector(tier9_countys$Group.1)
rm(tier9_countys)

tier10_countys <- data.frame(county_avg[which(county_avg$tier == 10),])
countys_tier10 <- as.vector(tier10_countys$Group.1)
rm(tier10_countys)

#assign a tier to new column based on the average rent value in data set
library(dplyr)
rent_transpose <- rent_transpose %>% mutate(county_tier = case_when(county_and_state %in% countys_tier1 ~ 1,
                                                                    county_and_state %in% countys_tier2 ~ 2,
                                                                    county_and_state %in% countys_tier3 ~ 3,
                                                                    county_and_state %in% countys_tier4 ~ 4,
                                                                    county_and_state %in% countys_tier5 ~ 5,
                                                                    county_and_state %in% countys_tier6 ~ 6,
                                                                    county_and_state %in% countys_tier7 ~ 7,
                                                                    county_and_state %in% countys_tier8 ~ 8,
                                                                    county_and_state %in% countys_tier9 ~ 9,
                                                                    county_and_state %in% countys_tier10 ~ 10))

#remove county avg data to keep things clean
rm(county_avg)
rm(countys_tier1)
rm(countys_tier2)
rm(countys_tier3)
rm(countys_tier4)
rm(countys_tier5)
rm(countys_tier6)
rm(countys_tier7)
rm(countys_tier8)
rm(countys_tier9)
rm(countys_tier10)

#assign tiers to each METRO and create vectors of the METRO names
metro_avg <- aggregate(rent_transpose[,9], list(rent_transpose$metro_and_state), mean)

library(gtools)
metro_avg$tier <- quantcut(metro_avg$x,q=10)
summary(metro_avg$tier)
levels(metro_avg$tier)[levels(metro_avg$tier)=="[696,858]"] <- "1"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(858,911]"] <- "2"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(911,972]"] <- "3"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(972,1.02e+03]"] <- "4"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(1.02e+03,1.07e+03]"] <- "5"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(1.07e+03,1.14e+03]"] <- "6"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(1.14e+03,1.21e+03]"] <- "7"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(1.21e+03,1.3e+03]"] <- "8"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(1.3e+03,1.47e+03]"] <- "9"
levels(metro_avg$tier)[levels(metro_avg$tier)=="(1.47e+03,3.89e+03]"] <- "10"


tier1_metros <- data.frame(metro_avg[which(metro_avg$tier == 1),])
metros_tier1 <- as.vector(tier1_metros$Group.1)
rm(tier1_metros)

tier2_metros <- data.frame(metro_avg[which(metro_avg$tier == 2),])
metros_tier2 <- as.vector(tier2_metros$Group.1)
rm(tier2_metros)

tier3_metros <- data.frame(metro_avg[which(metro_avg$tier == 3),])
metros_tier3 <- as.vector(tier3_metros$Group.1)
rm(tier3_metros)

tier4_metros <- data.frame(metro_avg[which(metro_avg$tier == 4),])
metros_tier4 <- as.vector(tier4_metros$Group.1)
rm(tier4_metros)

tier5_metros <- data.frame(metro_avg[which(metro_avg$tier == 5),])
metros_tier5 <- as.vector(tier5_metros$Group.1)
rm(tier5_metros)

tier6_metros <- data.frame(metro_avg[which(metro_avg$tier == 6),])
metros_tier6 <- as.vector(tier6_metros$Group.1)
rm(tier6_metros)

tier7_metros <- data.frame(metro_avg[which(metro_avg$tier == 7),])
metros_tier7 <- as.vector(tier7_metros$Group.1)
rm(tier7_metros)

tier8_metros <- data.frame(metro_avg[which(metro_avg$tier == 8),])
metros_tier8 <- as.vector(tier8_metros$Group.1)
rm(tier8_metros)

tier9_metros <- data.frame(metro_avg[which(metro_avg$tier == 9),])
metros_tier9 <- as.vector(tier9_metros$Group.1)
rm(tier9_metros)

tier10_metros <- data.frame(metro_avg[which(metro_avg$tier == 10),])
metros_tier10 <- as.vector(tier10_metros$Group.1)
rm(tier10_metros)

#assign a tier to new column based on the average rent value in data set
library(dplyr)
rent_transpose <- rent_transpose %>% mutate(metro_tier = case_when(metro_and_state %in% metros_tier1 ~ 1,
                                                                    metro_and_state %in% metros_tier2 ~ 2,
                                                                    metro_and_state %in% metros_tier3 ~ 3,
                                                                    metro_and_state %in% metros_tier4 ~ 4,
                                                                    metro_and_state %in% metros_tier5 ~ 5,
                                                                    metro_and_state %in% metros_tier6 ~ 6,
                                                                    metro_and_state %in% metros_tier7 ~ 7,
                                                                    metro_and_state %in% metros_tier8 ~ 8,
                                                                    metro_and_state %in% metros_tier9 ~ 9,
                                                                    metro_and_state %in% metros_tier10 ~ 10))

#remove metro avg data to keep things clean
rm(metro_avg)
rm(metros_tier1)
rm(metros_tier2)
rm(metros_tier3)
rm(metros_tier4)
rm(metros_tier5)
rm(metros_tier6)
rm(metros_tier7)
rm(metros_tier8)
rm(metros_tier9)
rm(metros_tier10)

#add dummy coded month columns
rent_transpose <- rent_transpose %>% mutate(month_jan = case_when(month == "January" ~ 1, month != "January" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_feb = case_when(month == "February" ~ 1, month != "February" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_mar = case_when(month == "March" ~ 1, month != "March" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_apr = case_when(month == "April" ~ 1, month != "April" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_may = case_when(month == "May" ~ 1, month != "May" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_jun = case_when(month == "June" ~ 1, month != "June" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_jul = case_when(month == "July" ~ 1, month != "July" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_aug = case_when(month == "August" ~ 1, month != "August" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_sep = case_when(month == "September" ~ 1, month != "September" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_oct = case_when(month == "October" ~ 1, month != "October" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_nov = case_when(month == "November" ~ 1, month != "November" ~ 0))
rent_transpose <- rent_transpose %>% mutate(month_dec = case_when(month == "December" ~ 1, month != "December" ~ 0))

#dummy coded state variables
rent_transpose <- rent_transpose %>% mutate(state_AK = case_when(State == "AK" ~ 1, State != "AK" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_AL = case_when(State == "AL" ~ 1, State != "AL" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_AR = case_when(State == "AR" ~ 1, State != "AR" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_AZ = case_when(State == "AZ" ~ 1, State != "AZ" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_CA = case_when(State == "CA" ~ 1, State != "CA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_CO = case_when(State == "CO" ~ 1, State != "CO" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_CT = case_when(State == "CT" ~ 1, State != "CT" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_DC = case_when(State == "DC" ~ 1, State != "DC" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_DE = case_when(State == "DE" ~ 1, State != "DE" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_FL = case_when(State == "FL" ~ 1, State != "FL" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_GA = case_when(State == "GA" ~ 1, State != "GA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_HI = case_when(State == "HI" ~ 1, State != "HI" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_IA = case_when(State == "IA" ~ 1, State != "IA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_ID = case_when(State == "ID" ~ 1, State != "ID" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_IL = case_when(State == "IL" ~ 1, State != "IL" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_IN = case_when(State == "IN" ~ 1, State != "IN" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_KS = case_when(State == "KS" ~ 1, State != "KS" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_KY = case_when(State == "KY" ~ 1, State != "KY" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_LA = case_when(State == "LA" ~ 1, State != "LA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_MA = case_when(State == "MA" ~ 1, State != "MA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_MD = case_when(State == "MD" ~ 1, State != "MD" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_ME = case_when(State == "ME" ~ 1, State != "ME" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_MI = case_when(State == "MI" ~ 1, State != "MI" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_MN = case_when(State == "MN" ~ 1, State != "MN" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_MO = case_when(State == "MO" ~ 1, State != "MO" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_MS = case_when(State == "MS" ~ 1, State != "MS" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_MT = case_when(State == "MT" ~ 1, State != "MT" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_NC = case_when(State == "NC" ~ 1, State != "NC" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_ND = case_when(State == "ND" ~ 1, State != "ND" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_NE = case_when(State == "NE" ~ 1, State != "NE" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_NH = case_when(State == "NH" ~ 1, State != "NH" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_NJ = case_when(State == "NJ" ~ 1, State != "NJ" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_NM = case_when(State == "NM" ~ 1, State != "NM" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_NV = case_when(State == "NV" ~ 1, State != "NV" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_NY = case_when(State == "NY" ~ 1, State != "NY" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_OH = case_when(State == "OH" ~ 1, State != "OH" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_OK = case_when(State == "OK" ~ 1, State != "OK" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_OR = case_when(State == "OR" ~ 1, State != "OR" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_PA = case_when(State == "PA" ~ 1, State != "PA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_RI = case_when(State == "RI" ~ 1, State != "RI" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_SC = case_when(State == "SC" ~ 1, State != "SC" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_SD = case_when(State == "SD" ~ 1, State != "SD" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_TN = case_when(State == "TN" ~ 1, State != "TN" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_TX = case_when(State == "TX" ~ 1, State != "TX" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_UT = case_when(State == "UT" ~ 1, State != "UT" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_VA = case_when(State == "VA" ~ 1, State != "VA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_VT = case_when(State == "VT" ~ 1, State != "VT" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_WA = case_when(State == "WA" ~ 1, State != "WA" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_WI = case_when(State == "WI" ~ 1, State != "WI" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_WV = case_when(State == "WV" ~ 1, State != "WV" ~ 0))
rent_transpose <- rent_transpose %>% mutate(state_WY = case_when(State == "WY" ~ 1, State != "WY" ~ 0))


str(rent_transpose)
test_model <- lm(median_rent ~ year+county_tier+metro_tier+month_jan+month_feb+month_mar+month_apr+
                   month_may+month_jun+month_jul+month_aug+state_AL+
                   state_AR+state_AZ+state_CA+state_CO+state_CT+state_DC+state_DE+state_FL+state_GA+state_HI+state_IA+
                   state_ID+state_IL+state_IN+state_KS+state_KS+state_KY+state_MA+state_MD+state_MI+
                   state_MO+state_MS+state_MT+state_NC+state_NE+state_NJ+state_NM+
                   state_NY+state_OH+state_OK+state_PA+state_RI+state_SC+state_TN+state_TX+
                   state_VA+state_VT+state_WA+state_WI+state_WV, data=rent_transpose)
summary(test_model)


