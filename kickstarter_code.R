#read in data
setwd("C:/Users/markmasson/Desktop/R Work/kickstarter")
list.files()
kickstarter <- read.csv("kickstarter.csv")

#I only want completed kickstarter campaigns, so successful or failed
summary(kickstarter)
str(kickstarter)
kickstarter <- kickstarter[ which(kickstarter$state == "successful" | kickstarter$state == "failed" ),]

#look for NAs
summary(kickstarter)

#usd.pledged has some NA's, will use usd_pledged_real
#remove that column, along with goal.  also remove ID, as i do not need that column.
kickstarter <- kickstarter[ , -c(1,7,9,13)]

#usd_goal_real has 0, remove those
kickstarter <- kickstarter[ which(kickstarter$usd_goal_real > 99),]

#encode "state" as 1 for successful, 0 for failed, call column "success"
library(dplyr)
kickstarter <- kickstarter %>% mutate(success = case_when(state == "successful" ~ 1, state != "successful" ~ 0))

#need just the date from "launched" 
kickstarter$launched_date <- substring(kickstarter$launched, 1, 10)

#remove "launched"
kickstarter <- kickstarter[ , -6 ]

#convert deadline and launched_date to dates from factors
library(lubridate)
kickstarter$launched_date <- ymd(kickstarter$launched_date)
kickstarter$deadline_date <- ymd(kickstarter$deadline)
kickstarter <- kickstarter[ , -5]
str(kickstarter)

#calculate days to deadline
kickstarter$days_to_deadline <- kickstarter$deadline_date-kickstarter$launched_date
kickstarter$days_to_deadline <- as.numeric(kickstarter$days_to_deadline)

#calculate goal per day
kickstarter$goal_per_day <- kickstarter$usd_goal_real/kickstarter$days_to_deadline

library(dplyr)
kickstarter <- kickstarter %>% mutate(is_comics = case_when(main_category == "Comics" ~ 1, main_category != "Comics" ~ 0))
kickstarter <- kickstarter %>% mutate(is_crafts = case_when(main_category == "Crafts" ~ 1, main_category != "Crafts" ~ 0))
kickstarter <- kickstarter %>% mutate(is_dance = case_when(main_category == "Dance" ~ 1, main_category != "Dance" ~ 0))
kickstarter <- kickstarter %>% mutate(is_design = case_when(main_category == "Design" ~ 1, main_category != "Design" ~ 0))
kickstarter <- kickstarter %>% mutate(is_fashion = case_when(main_category == "Fashion" ~ 1, main_category != "Fashion" ~ 0))
kickstarter <- kickstarter %>% mutate(is_filmvideo = case_when(main_category == "Film & Video" ~ 1, main_category != "Film & Video" ~ 0))
kickstarter <- kickstarter %>% mutate(is_food = case_when(main_category == "Food" ~ 1, main_category != "Food" ~ 0))
kickstarter <- kickstarter %>% mutate(is_games = case_when(main_category == "Games" ~ 1, main_category != "Games" ~ 0))
kickstarter <- kickstarter %>% mutate(is_journalism = case_when(main_category == "Journalism" ~ 1, main_category != "Journalism" ~ 0))
kickstarter <- kickstarter %>% mutate(is_music = case_when(main_category == "Music" ~ 1, main_category != "Music" ~ 0))
kickstarter <- kickstarter %>% mutate(is_photography = case_when(main_category == "Photography" ~ 1, main_category != "Photography" ~ 0))
kickstarter <- kickstarter %>% mutate(is_publishing = case_when(main_category == "Publishing" ~ 1, main_category != "Publishing" ~ 0))
kickstarter <- kickstarter %>% mutate(is_technology = case_when(main_category == "Technology" ~ 1, main_category != "Technology" ~ 0))

summary(kickstarter)

#create model
head(kickstarter)
kickstarter_model <- glm(success ~ is_comics + is_crafts + is_dance + is_design + is_fashion + is_filmvideo + 
                           is_food + is_games + is_journalism + is_music + is_photography + is_publishing + 
                           is_technology + country + usd_goal_real + days_to_deadline + goal_per_day, 
                         family=binomial(link = "logit"), data = kickstarter)
summary(kickstarter_model)

library(MASS)
kickstarter_stepwise <- stepAIC(kickstarter_model, direction="backward")
summary(kickstarter_stepwise)

probabilities <- predict(kickstarter_stepwise, kickstarter, type = "response")
predicted.classes <- ifelse(probabilities > 0.7, "1", "0")
observed.classes <- kickstarter$success
mean(predicted.classes == observed.classes)

