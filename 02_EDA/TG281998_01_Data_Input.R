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
setwd("C:/Tomek/Tomzip/Dokumenty/Nauka/Podyplomowe_2019_DataScience/Praca_Dyplomowa/")

#Import raw data ----

cc_data <- read.csv("data/UCI_Credit_Card.csv")


#check_vars



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

