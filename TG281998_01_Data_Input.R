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
table(cc_data$MARRIAGE)//

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


#zamiana zmiennej celu i zmiennych kategorycznych na factory
cc_data=cc_data %>% mutate(default.payment.next.month=as.factor(default.payment.next.month))
cc_data=cc_data %>% mutate(SEX=as.factor(SEX))
cc_data=cc_data %>% mutate(EDUCATION=as.factor(EDUCATION))
cc_data=cc_data %>% mutate(MARRIAGE=as.factor(MARRIAGE))

table(cc_data$SEX)
table(cc_data$EDUCATION)
table(cc_data$MARRIAGE)

#As seen previously, some categories are mislabeled or undocumented. Before proceeding, it is time to fix it.
#The 0 in MARRIAGE can be safely categorized as 'Other' (thus 3).
#The 0 (undocumented), 5 and 6 (label unknown) in EDUCATION can also be put in a 'Other' cathegory (thus 4)



table(cc_data$MARRIAGE)

cc_data <- data.frame(cc_data)
#PROBLEM - jak zamienic wartosc 0 na 3? 2021-09-08
cc_data[cc_data$MARRIAGE==0] <- 3

df[df==1]<-11

levels(cc_data$SEX) <- c("Mężczyzna", "Kobieta")
levels(cc_data$EDUCATION) <- c("Magister lub wyższe","Licencjat lub równoważne","Szkola średnia","Inne")
levels(cc_data$MARRIAGE) <- c("Brak danych","Zamężna/żonaty","Kawaler/panna","Inne")

table(cc_data$MARRIAGE)


barplot(table(cc_data$default.payment.next.month), 
        col = "gold", 
        horiz = TRUE,  # wykres horyzontalny
        main = "Rozkład zmiennej zależnej w próbie - brak zapłacenia raty", # tytuł
        xlab = "liczebność") # etykieta osi x













hist(cc_data$default.payment.next.month,
     main = "Histogram tygodniowej liczby godzin pracy",
     xlab = "liczba godzin pracy",
     ylab = "liczebność",
     col = "light green")

#scatter ma sens dla ciaglych, dla factor dac tabele czestosci?
plot(cc_data$SEX, # liczba godzin pracy w tygodniu
     cc_data$default.payment.next.month,  # wynagrodzenie z pracy w tysiącach
     xlab = "Cena biletu w $",
     ylab = "Wiek")


#PO CO MI DATA FRAME I FACTORY? 
#ZMIEN FONT NA POLSKI Z OBSŁUGĄ POLSKICH ZNAKÓW - UTF8

cc_data$sex_factor <- as.factor(cc_data$sex)
cc_data$education_factor <- as.factor(cc_data$education)
cc_data$marriage_factor <- as.factor(cc_data$marriage)

cc_data$sex_factor = factor(cc_data$sex, levels=c("1","2"), labels=c("Mezczyzna","Kobieta"))
cc_data$education = factor(cc_data$education, levels=c("1","2","3","4"), labels=c("Magister lub wyzsze","Licencjat lub rownowazne","Szkola srednia","Inne"))
cc_data$marriage = factor(cc_data$marriage, levels=c("1","2"), labels=c("zamezna/zonaty","kawaler/panna","inne"))

