# PRACA DYPLOMOWA - MODELE PROGNOZUJACE WYNIK WALKI MMA W FEDERACJI UFC #
# I DATA PREPARATION
# II EXPLORATORY DATA ANALYSIS


# Two approaches
# 1 - betting before the fight - we don't know current fight statistics
# 2 - betting at the end of fight - we know all current fight statistics

#Install packages ----


#install.packages("fBasics")
#install.packages("dplyr")
#install.packages("sqldf")
#Library ----

library(dplyr)
library(fBasics)
library(sqldf)
library(tidyverse)
library(reshape2)
library(readr)

#Notes ----
#basicStats(wzrost) #DESCRIPTIVE STATISTICS OF EACH VARIABLE 

#Prepare ----
rm(list = ls()) #usuniecie wszystkich elementow z pamieci podrecznej
setwd("C:/Users/tgusc/Documents/GitHub/CC_Taiwan_WNE/") #work directory na lapku
#work directory na tablecie


#Import raw data ----

cc_data <- read.csv("data/UCI_Credit_Card.csv")


#check_vars

table(cc_data$SEX)
table(cc_data$EDUCATION)
table(cc_data$MARRIAGE)

sapply(cc_data, 
       function(x) sum(x == ""))
# No missing values in whole dataset
# 25 variables - all numeric
# id - id of obs
# sex, education, marriage - numerical cactegorical variables

glimpse(cc_data)
basicStats(cc_data)
summary(cc_data)

table(cc_data$default.payment.next.month)


#CZYSZCZENIE DANYCH 

table(cc_data$SEX)
table(cc_data$EDUCATION)
table(cc_data$MARRIAGE)

#As seen previously, some categories are mislabeled or undocumented. Before proceeding, it is time to fix it.
#The 0 in MARRIAGE can be safely categorized as 'Other' (thus 3).
#The 0 (undocumented), 5 and 6 (label unknown) in EDUCATION can also be put in a 'Other' cathegory (thus 4)

#cc_data <- data.frame(cc_data)
#PROBLEM - jak zamienic wartosc 0 na 3? 2021-09-08
cc_data$MARRIAGE[cc_data$MARRIAGE==0] <- 3 #zmiana wartosci 0 na wartosc 3 - inne
cc_data$EDUCATION[cc_data$EDUCATION==0] <- 4 #zmiana wartosci 0 na wartosc 3 - inne
cc_data$EDUCATION[cc_data$EDUCATION==5] <- 4 #zmiana wartosci 5 na wartosc 3 - inne
cc_data$EDUCATION[cc_data$EDUCATION==6] <- 4 #zmiana wartosci 6 na wartosc 3 - inne

table(cc_data$EDUCATION)
table(cc_data$MARRIAGE)


#zamiana zmiennej celu i zmiennych kategorycznych na factory
cc_data=cc_data %>% mutate(default.payment.next.month=as.factor(default.payment.next.month))
cc_data=cc_data %>% mutate(SEX=as.factor(SEX))
cc_data=cc_data %>% mutate(EDUCATION=as.factor(EDUCATION))
cc_data=cc_data %>% mutate(MARRIAGE=as.factor(MARRIAGE))


levels(cc_data$SEX) <- c("Mężczyzna", "Kobieta")
levels(cc_data$EDUCATION) <- c("Magister lub wyższe","Licencjat lub równoważne","Szkola średnia","Inne")
levels(cc_data$MARRIAGE) <- c("Zamężna/żonaty","Kawaler/panna","Inne")

table(cc_data$SEX)
table(cc_data$EDUCATION)
table(cc_data$MARRIAGE)



#spróbuj napisac petle na nazwie zmiennej
### NARAZIE NIE WIEM JAK
table(cc_data$PAY_0)

for (year in c(0,2,3,4,5,6)){
        paste('cc_data$PAY_', year,'[cc_data$PAY_',year,'==-1] <- 0')
        paste('cc_data$PAY_', year,'[cc_data$PAY_',year,'==-1] <- 0')
}


table(cc_data$PAY_0)
table(cc_data$PAY_6)














