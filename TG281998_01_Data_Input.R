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
#install.packages('ggplot2')
# install.packages('tidyverse')
#Library ----

library(dplyr)
library(fBasics)
library(sqldf)
library(tidyverse)
library(reshape2)
library(readr)

library(ggplot2)

library(tidyverse)

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

#Zamiana wartości PAY_X mniejszych od 0 na 0 - spłata zadłużenia
table(cc_data$PAY_0)
table(cc_data$PAY_2)
table(cc_data$PAY_3)
table(cc_data$PAY_4)
table(cc_data$PAY_5)
table(cc_data$PAY_6)


cc_data$PAY_0[cc_data$PAY_0==-1] <- 0 #zmiana wartosci -1 na wartosc 0 - terminowa spłata zadłużenia
cc_data$PAY_0[cc_data$PAY_0==-2] <- 0 #zmiana wartosci -2 na wartosc 0 - terminowa spłata zadłużenia

cc_data$PAY_2[cc_data$PAY_2==-1] <- 0 #zmiana wartosci -1 na wartosc 0 - terminowa spłata zadłużenia
cc_data$PAY_2[cc_data$PAY_2==-2] <- 0 #zmiana wartosci -2 na wartosc 0 - terminowa spłata zadłużenia

cc_data$PAY_3[cc_data$PAY_3==-1] <- 0 #zmiana wartosci -1 na wartosc 0 - terminowa spłata zadłużenia
cc_data$PAY_3[cc_data$PAY_3==-2] <- 0 #zmiana wartosci -2 na wartosc 0 - terminowa spłata zadłużenia

cc_data$PAY_4[cc_data$PAY_4==-1] <- 0 #zmiana wartosci -1 na wartosc 0 - terminowa spłata zadłużenia
cc_data$PAY_4[cc_data$PAY_4==-2] <- 0 #zmiana wartosci -2 na wartosc 0 - terminowa spłata zadłużenia

cc_data$PAY_5[cc_data$PAY_5==-1] <- 0 #zmiana wartosci -1 na wartosc 0 - terminowa spłata zadłużenia
cc_data$PAY_5[cc_data$PAY_5==-2] <- 0 #zmiana wartosci -2 na wartosc 0 - terminowa spłata zadłużenia

cc_data$PAY_6[cc_data$PAY_6==-1] <- 0 #zmiana wartosci -1 na wartosc 0 - terminowa spłata zadłużenia
cc_data$PAY_6[cc_data$PAY_6==-2] <- 0 #zmiana wartosci -2 na wartosc 0 - terminowa spłata zadłużenia

table(cc_data$PAY_0)
table(cc_data$PAY_2)
table(cc_data$PAY_3)
table(cc_data$PAY_4)
table(cc_data$PAY_5)
table(cc_data$PAY_6)

### Statystyki oczyszczonego zbioru danych

glimpse(cc_data)
summary(cc_data)


# STATYSTYKI OPISOWE DANYCH


ggplot() + 
  geom_bar(data = cc_data, aes(x = SEX, fill = default.payment.next.month))

ggplot() + 
  geom_bar(data = cc_data, aes(x = SEX, fill = default.payment.next.month)) +
  geom_text(aes(label=stat(SEX)), position = position_stack(vjust= 0.5), colour = "white", size = 5)

#SPRAWDZIĆ JAK DODAĆ WARTOŚCI DO SŁUPKÓW, CHYBA BĘDZIE TRZEBA TWORZYĆ ODDZIELNE ZBIORY DANYCH DLA WYKRESÓW

ggplot(data = cc_data, aes(x = SEX, fill = default.payment.next.month)) +
  geom_bar(position = "fill")


ggplot(data = cc_data, aes(x = SEX, fill = default.payment.next.month)) +
  geom_bar(position = "fill") +
  geom_text(aes(label=count), position = position_stack(vjust= 0.5), colour = "white", size = 5)


cc_data2 <- cc_data %>% 
  arrange(SEX, desc(default.payment.next.month)) %>% 
  group_by(SEX) %>% 
  mutate(label_sum = cumsum(1)) 


ggplot(data = cc_data2, aes(x = factor(SEX), y=label_sum fill = factor(default.payment.next.month))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle("Płeć w próbie") +
  xlab("Płeć") +
  ylab("Liczebność") +
  labs(fill = "Zmienna celu") +
  theme_minimal() +
  geom_text(aes(label = label_sum), position = position_stack(),
            vjust = -.5, color = "black", size = 3.5)


# W tym celu musimy dokonać dwie czynności:
# 
#   * ustalić parametr position na wartość `position_stack`,
#   * policzyć skumulowane sumy, które będą wyświetlane na wykresie.

final <- rs4 %>% 
  arrange(Age, desc(Gender)) %>% 
  group_by(Age) %>% 
  mutate(label_sum = cumsum(N)) 

ggplot(data = final, aes(x = factor(Age), y = N, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle("Płeć i wiek w próbie") +
  xlab("Wiek") +
  ylab("Populacja") +
  labs(fill = "Płeć") +
  theme_minimal() +
  geom_text(aes(label = label_sum), position = position_stack(),
            vjust = -.5, color = "black", size = 3.5) +
  scale_x_discrete(limits = c('17','18','19','20','21','22','23','24'))

# Możemy też zostawić sumy częściowe i ustawić je w połowie słupków:
final2 <- final %>% 
  mutate(label_sum2 = label_sum*.5)

ggplot(data = final2, aes(x = factor(Age), y = N, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle("Płeć i wiek w próbie") +
  xlab("Wiek") +
  ylab("Populacja") +
  labs(fill = "Płeć") +
  theme_minimal() +
  geom_label(aes(y = label_sum2, label = N), fill = 'white', position = position_stack(),
             vjust = 0.5, color = "black", size = 3) +
  scale_x_discrete(limits = c('17','18','19','20','21','22','23','24'))



