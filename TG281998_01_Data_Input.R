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
#install.packages('tidyverse')
#install.packages('corrplot')
#Library ----

library(dplyr)
library(fBasics)
library(sqldf)
library(tidyverse)
library(reshape2)
library(readr)

library(ggplot2)

library(tidyverse)
library(corrplot)

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


# rozwińmy powyższy kod o pogrupoanie danych wg stanu

test1 <- data.frame(table(cc_data$SEX, cc_data$default.payment.next.month))


ggplot(data = test1, aes(x = Var1, y=Freq, fill = Var2)) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle("Płeć w próbie - wartości bezwzględne") +
  xlab("Płeć") +
  ylab("Liczebność") +
  labs(fill = "Zmienna celu") +
  theme_minimal() +
  geom_text(aes(label = Freq), position = position_stack(),
            vjust = -.5, color = "black", size = 3.5)


test2 <- table(cc_data$default.payment.next.month, cc_data$SEX )
addmargins(test2) #procent z calej grupy
prop.table(test2,1) # procent z wiersza
prop.table(test2,2) # procent z kolumny

test3 <- as.data.frame(round(prop.table(test2,2)*100,2))

#Wykres slupkowy
ggplot(as.data.frame(test3),aes(x=factor(Var2),y=Freq,fill=Var1)) +
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(label=Freq),position="stack",vjust=1)+
  scale_fill_manual(values=c("grey60","grey80"))+
  theme_bw()


#KORELOGRAM - zmienne PAY i BIL_AMT skorelowane
cc_ilosciowe <- 
  map_lgl(cc_data, is.numeric) %>% 
  which() %>% names()

cc_korelacje <- cor(cc_data[,cc_ilosciowe],
                                use = "pairwise.complete.obs")

cc_ilosciowe_sort <- 
  sort(cc_ilosciowe[,"default.payment.next.month"], 
       decreasing = TRUE) %>% names()


corrplot.mixed(cc_korelacje,
               upper = "square",
               lower = "number",
               tl.col="black", # kolor etykietek (nazw zmiennych)
               tl.pos = "lt")  # pozycja etykietek (lt = left and top)

#HEATMAPA Z PROPORCJAMI - DO POPRACOWANIA

test5 <- select(cc_data, c(SEX, EDUCATION ,default.payment.next.month, ID))

test5=test5 %>% mutate(SEX=as.numeric(SEX))
test5=test5 %>% mutate(EDUCATION=as.numeric(EDUCATION))
test5=test5 %>% mutate(default.payment.next.month=as.numeric(default.payment.next.month))

cc_data %>% count(SEX, EDUCATION, default.payment.next.month) -> test6

test5 %>% count(SEX, EDUCATION, default.payment.next.month) -> test7

heatmap(as.matrix(test7),Colv = NA, Rowv = NA, scale="n")
