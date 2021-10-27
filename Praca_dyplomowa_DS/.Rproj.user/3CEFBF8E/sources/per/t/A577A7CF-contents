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
#install.packages('caret')
#install.packages('shiny')
#install.packages("randomForest")
#install.packages("ranger")
#install.packages("gbm")
#install.packages("xgboost")
#install.packages("fastAdaboost")
#install.packages("adaStump")
#install.packages("caret")
#install.packages("tibble")
#install.packages("here")
#install.packages("neuralnet")
#install.packages("pROC")
#Library ----

if (0) {
  devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.0.0/catboost-R-Windows-1.0.0.tgz',
                        INSTALL_opts = c("--no-multiarch"))
}


library(dplyr)
library(fBasics)
library(sqldf)
library(tidyverse)
library(reshape2)
library(readr)

library(ggplot2)

library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(ranger)

library(gbm)
library(xgboost)
library(caret)
library(tibble)
library(here)
library(catboost)

library(neuralnet)
library(here)
library(pROC)

#Notes ----
#basicStats(wzrost) #DESCRIPTIVE STATISTICS OF EACH VARIABLE 

#Prepare ----
rm(list = ls()) #usuniecie wszystkich elementow z pamieci podrecznej
setwd("C:/Users/tgusc/Documents/GitHub/CC_Taiwan_WNE/") #work directory na lapku
#setwd("C:/Users/User/Documents/GitHub/CC_Taiwan_WNE")#work directory na tablecie


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
#Wykomentowanie na rzecz logitu i korelogramu - cc_data=cc_data %>% mutate(default.payment.next.month=as.factor(default.payment.next.month))
cc_data=cc_data %>% mutate(SEX=as.factor(SEX))
cc_data=cc_data %>% mutate(EDUCATION=as.factor(EDUCATION))
cc_data=cc_data %>% mutate(MARRIAGE=as.factor(MARRIAGE))


levels(cc_data$SEX) <- c("Mężczyzna", "Kobieta")
levels(cc_data$EDUCATION) <- c("Magister lub wyzsze","Licencjat lub rownoważne","Szkola srednia","Inne")
levels(cc_data$MARRIAGE) <- c("Zamezna/zonaty","Kawaler/panna","Inne")

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

cc_data=cc_data %>% mutate(PAY_0=as.factor(PAY_0))
cc_data=cc_data %>% mutate(PAY_2=as.factor(PAY_2))
cc_data=cc_data %>% mutate(PAY_3=as.factor(PAY_3))
cc_data=cc_data %>% mutate(PAY_4=as.factor(PAY_4))
cc_data=cc_data %>% mutate(PAY_5=as.factor(PAY_5))
cc_data=cc_data %>% mutate(PAY_6=as.factor(PAY_6))

table(cc_data$PAY_0)
table(cc_data$PAY_2)
table(cc_data$PAY_3)
table(cc_data$PAY_4)
table(cc_data$PAY_5)
table(cc_data$PAY_6)

levels(cc_data$PAY_0) <- c("P0","P1","P2","P3","P4","P5","P6","P7","P8")
levels(cc_data$PAY_2) <- c("P0","P1","P2","P3","P4","P5","P6","P7","P8")
levels(cc_data$PAY_3) <- c("P0","P1","P2","P3","P4","P5","P6","P7","P8")
levels(cc_data$PAY_4) <- c("P0","P1","P2","P3","P4","P5","P6","P7","P8")
levels(cc_data$PAY_5) <- c("P0","P2","P3","P4","P5","P6","P7","P8")
levels(cc_data$PAY_6) <- c("P0","P2","P3","P4","P5","P6","P7","P8")

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

#SEX
ggplot(data = data.frame(table(cc_data$SEX, cc_data$default.payment.next.month)), aes(x = Var1, y=Freq, fill = Var2)) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle("Płeć w próbie") +
  xlab("Płeć") +
  ylab("Liczebność") +
  labs(fill = "Czy kredyt zostal splacony?\n0 - tak, 1 - nie") +
  theme_minimal() +
  geom_text(aes(label = Freq), position = position_stack(),
            vjust = -.5, color = "black", size = 3.5)

ggplot(as.data.frame(as.data.frame(round(prop.table(table(cc_data$default.payment.next.month, cc_data$SEX ),2)*100,2))),aes(x=factor(Var2),y=Freq,fill=Var1)) +
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(label=Freq),position="stack",vjust=1)+
  scale_fill_manual(values=c("grey60","grey80"))+
  ggtitle("Udział niespłaconych kredytów w podziale na płeć") +
  xlab("Płeć") +
  ylab("Proporcja") +
  labs(fill = "Czy kredyt zostal splacony?\n0 - tak, 1 - nie") +
  theme_bw()


#MARRIAGE

ggplot(data = data.frame(table(cc_data$MARRIAGE, cc_data$default.payment.next.month)), aes(x = Var1, y=Freq, fill = Var2)) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle("Stan cywilny w próbie") +
  xlab("Stan cywilny") +
  ylab("Liczebność") +
  labs(fill = "Czy kredyt zostal splacony?\n0 - tak, 1 - nie") +
  theme_minimal() +
  geom_text(aes(label = Freq), position = position_stack(),
            vjust = -.5, color = "black", size = 3.5)

ggplot(as.data.frame(as.data.frame(round(prop.table(table(cc_data$default.payment.next.month, cc_data$MARRIAGE ),2)*100,2))),aes(x=factor(Var2),y=Freq,fill=Var1)) +
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(label=Freq),position="stack",vjust=1)+
  scale_fill_manual(values=c("grey60","grey80"))+
  ggtitle("Udział niespłaconych kredytów w podziale na stan cywilny") +
  xlab("Stan cywilny") +
  ylab("Proporcja") +
  labs(fill = "Czy kredyt zostal splacony?\n0 - tak, 1 - nie") +
  theme_bw()


#EDUCATION

ggplot(data = data.frame(table(cc_data$EDUCATION, cc_data$default.payment.next.month)), aes(x = Var1, y=Freq, fill = Var2)) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle("Wyksztalcenie w probie") +
  xlab("Stan cywilny") +
  ylab("Liczebność") +
  labs(fill = "Czy kredyt zostal splacony?\n0 - tak, 1 - nie") +
  theme_minimal() +
  geom_text(aes(label = Freq), position = position_stack(),
            vjust = -.5, color = "black", size = 3.5)

ggplot(as.data.frame(as.data.frame(round(prop.table(table(cc_data$default.payment.next.month, cc_data$EDUCATION ),2)*100,2))),aes(x=factor(Var2),y=Freq,fill=Var1)) +
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(label=Freq),position="stack",vjust=1)+
  scale_fill_manual(values=c("grey60","grey80"))+
  ggtitle("Udział niespłaconych kredytów w podziale na wykształcenie") +
  xlab("Wykształcenie") +
  ylab("Proporcja") +
  labs(fill = "Czy kredyt zostal splacony?\n0 - tak, 1 - nie") +
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

#Zależności między zmiennymi

sum_sex_edu <- cc_data %>%
  group_by(SEX, EDUCATION, default.payment.next.month) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sum_sex_marr <- cc_data %>%
  group_by(SEX, MARRIAGE, default.payment.next.month) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sum_marr_edu <- cc_data %>%
  group_by(MARRIAGE, EDUCATION, default.payment.next.month) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#Podzial zbioru na czesc uczaca sie i testowa

set.seed(1235)
#zmiana zmiennej celu na factor
data_part <- createDataPartition(cc_data$default.payment.next.month,
                                    p = 0.7,
                                    list = FALSE)

cc_data_train <- cc_data[data_part,]
cc_data_test <- cc_data[-data_part,]

#i zbiorki, gdzie zmienna celu jest factorem
cc_data_train_fct <- cc_data_train
Cc_data_test_fct <- cc_data_test

cc_data_train_fct=cc_data_train_fct %>% mutate(default.payment.next.month=as.factor(default.payment.next.month)) 
Cc_data_test_fct=Cc_data_test_fct %>% mutate(default.payment.next.month=as.factor(default.payment.next.month)) 

cc_data_train_fct  <- cc_data_train_fct %>% as.data.frame()
Cc_data_test_fct <- Cc_data_test_fct %>% as.data.frame()

# Liczebności utworzonych zbiorów

nrow(cc_data_train)/nrow(cc_data)
nrow(cc_data_test)/nrow(cc_data)

prop.table(table(cc_data_train$default.payment.next.month))

prop.table(table(cc_data_test$default.payment.next.month))


### Wlasnosci prognostyczne? Feature engineering w R? 
### Sprawdzenie danych pod regresje i pod inne modele

zmienne_wszystkie <- c("LIMIT_BAL","AGE","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6",
                       "SEX","EDUCATION","MARRIAGE","PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")

zmienne_ilosciowe <- c("LIMIT_BAL","AGE","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")

zmienne_jakosciowe <- c("SEX","EDUCATION","MARRIAGE","PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")

#MODEL LOGISTYCZNY

names <- paste0("default.payment.next.month ~ ", paste0(zmienne_wszystkie, collapse = " + "))

cc_data_logit1 <- lm(as.formula(names),
              data = cc_data_train)

# zobaczmy wynik

summary(cc_data_logit1) # aR2 = 0.2065, model łącznie istotny (p-value F-statistic < 0.05)

# and now for something completely different
# random forest

#RANDOM FORESTS

model1.formula <- default.payment.next.month ~ LIMIT_BAL + AGE + SEX + EDUCATION + MARRIAGE +
BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6 + 
PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6

# x:        formuła modelu 
# data:     ramka danych (data frame) z obserwacjami
# ntree:    liczba drzew do skonstruowania (domyślnie = 500)
# mtry:     liczba zmiennych spośród których wybierane będą te do 
#           utworzenia węzłów (domyślnie = sqrt(liczba wszystkich predyktorów))
# replace:  TRUE jeżeli chcemy losować ze zwracaniem
# sampsize: liczebność próbki do tworzenia drzew, która jest losowana 
#           z pełnej próby (dla replace = TRUE domyślnie = nrow(x))
# importance: TRUE jeśli chcemy zapisać do wynikowego obiektu miary 
#             ważności zmiennych

cc_data.rf <- randomForest(model1.formula, 
                           data = cc_data_train_fct)

summary(cc_data.rf)

#GRADIENT BOOSTING

glimpse(cc_data_train)

cc_data.gbm <- 
  gbm(model1.formula,
      data = cc_data_train,
      distribution = "bernoulli",
      # całkowita liczba drzew
      n.trees = 500,
      # liczba interakcji zmiennych - de facto głębokość drzewa
      interaction.depth = 4,
      # shrinkage parameter - szybkość uczenia
      shrinkage = 0.01,
      verbose = FALSE)

# Predykcje na train oraz test

cc_data.pred.train.gbm <- predict(cc_data.gbm,
                                  cc_data_train, 
                                  # type = "response" daje 
                                  # w tym przypadku
                                  # prawdopodobieństwo sukcesu
                                  type = "response",
                                  # parametr n.trees umożliwia
                                  # wybranie iteracji, z której 
                                  # prognozę zwracamy
                                  n.trees = 500)


cc_data.pred.test.gbm <- predict(cc_data.gbm,
                                 Cc_data_test_fct, 
                                  # type = "response" daje 
                                  # w tym przypadku
                                  # prawdopodobieństwo sukcesu
                                  type = "response",
                                  # parametr n.trees umożliwia
                                  # wybranie iteracji, z której 
                                  # prognozę zwracamy
                                  n.trees = 500)


#Model przewiduje jedynki - czyli wyższe p_1 oznacza większe prawdopodobieństwo bycia "1"

#tuning parametrow modelu

parametry_gbm <- expand.grid(interaction.depth = c(1, 2, 4),
                             n.trees = c(100, 500),
                             shrinkage = c(0.01, 0.1), 
                             n.minobsinnode = c(100, 250, 500))

ctrl_cv3 <- trainControl(method = "cv", 
                         number = 3,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)


  set.seed(1235)
  cc_data.gbm2  <- train(model1.formula,
                         data = cc_data_train_fct,
                         distribution = "bernoulli",
                         method = "gbm",
                         tuneGrid = parametry_gbm,
                         #trControl = ctrl_cv3,
                         verbose = FALSE)
 # saveRDS(object = cc_data.gbm2,
  #        file   = here("output", "cc_data.gbm2"))
  
  ?make.names


#Wczytanie zapisanego zbioru danych
#cc_data.gbm2 <- readRDS(here("output", "cc_data.gbm2.rds"))

cc_data.gbm2
# best params -  n.trees=500 interaction.depth=4, shrinkage=0.01 n.minobsinnode=500
# accuracy - 0.8219956  kappa - 0.3835577

cc_data.pred.train.gbm_m2 <- predict(cc_data.gbm2,
                                  cc_data_train, 
                                  # type = "response" daje 
                                  # w tym przypadku
                                  # prawdopodobieństwo sukcesu
                                  type = "raw",
                                  # parametr n.trees umożliwia
                                  # wybranie iteracji, z której 
                                  # prognozę zwracamy
                                  n.trees = 500)


cc_data.pred.test.gbm_m2 <- predict(cc_data.gbm2,
                                 Cc_data_test_fct, 
                                 # type = "response" daje 
                                 # w tym przypadku
                                 # prawdopodobieństwo sukcesu
                                 type = "raw",
                                 # parametr n.trees umożliwia
                                 # wybranie iteracji, z której 
                                 # prognozę zwracamy
                                 n.trees = 500)


#Podsumowanie predykcji

acc_gbm_sum <- data.frame(matrix(NA, nrow = 21000, ncol = 0))

acc_gbm_sum$actual <- cc_data_train$default.payment.next.month
acc_gbm_sum$pred_gbm_m1 <- cc_data.pred.train.gbm
acc_gbm_sum$pred_gbm_m2 <- cc_data.pred.train.gbm_m2

acc_gbm_sum$pred_gbm_m1[acc_gbm_sum$pred_gbm_m1<=0.5] <- 0 
acc_gbm_sum$pred_gbm_m1[acc_gbm_sum$pred_gbm_m1>0.5] <- 1
#acc_gbm_sum$pred_gbm_m2[acc_gbm_sum$pred_gbm_m2<=0.5] <- 0 
#acc_gbm_sum$pred_gbm_m2[acc_gbm_sum$pred_gbm_m2>0.5] <- 1

confusionMatrix(factor(acc_gbm_sum$pred_gbm_m1),factor(acc_gbm_sum$actual),positive='1')
confusionMatrix(factor(acc_gbm_sum$pred_gbm_m2),factor(acc_gbm_sum$actual),positive='1')

acc_gbm_sum_test <- data.frame(matrix(NA, nrow = 9000, ncol = 0))
acc_gbm_sum_test$actual <- cc_data_test$default.payment.next.month
acc_gbm_sum_test$pred_gbm_m1 <- cc_data.pred.test.gbm
acc_gbm_sum_test$pred_gbm_m2 <- cc_data.pred.test.gbm_m2

acc_gbm_sum_test$pred_gbm_m1[acc_gbm_sum_test$pred_gbm_m1<=0.5] <- 0 
acc_gbm_sum_test$pred_gbm_m1[acc_gbm_sum_test$pred_gbm_m1>0.5] <- 1
#acc_gbm_sum_test$pred_gbm_m2[acc_gbm_sum_test$pred_gbm_m2<=0.5] <- 0 
#acc_gbm_sum_test$pred_gbm_m2[acc_gbm_sum_test$pred_gbm_m2>0.5] <- 1

confusionMatrix(factor(acc_gbm_sum_test$pred_gbm_m1),factor(acc_gbm_sum_test$actual),positive='1')
confusionMatrix(factor(acc_gbm_sum_test$pred_gbm_m2),factor(acc_gbm_sum_test$actual),positive='1')



########## XGBOOST 

# 1. nrounds - liczba iteracji do momentu zakończenia trenowania modelu
# 2. max_depth - maksymalna głębokość drzewa
#    zakres: [1, nieskończoność]
# 3. eta - wsp. learning rate
#    dla wysokich wartości eta trenowanie modelu odbywa się szybciej!
#    zakres: [0, 1]
#    Z kolei niskie wartości dają lepsze wyniki, pod warunkiem, że 
#    algorytm jest trenowany na odpowiednio dużej liczbie drzew.
#    To jednak (znacznie) zwiększa jego złożoność obliczeniową i czasową
# 4. gamma - Minimum Loss Reduction, minimalna redukcja optymizowanej funkcji 
#    celu wymagana do wykonania kolejnego podziału w bieżącym końcowym 
#    węźle drzewa. Wysoka wartość oznacza bardziej konserwatywny model.
#    Można próbować z różnymi wartościami tego parametru jednak 
#    za optymalizację modelu odpowiadają najczęściej inne parametry
#    zakres: [0, nieskończoność]
# 5. colsample_bytree - odsetek predyktorów wykorzystywanych w szukaniu
#    optymalnych podziałów (podobnie jak w lasach losowych). 
#    Wysokie wartości mogą prowadzić do przeuczenia modelu.
#    Niskie wartości mogą prowadzić do niższej dokładności modelu.
#    Zaleca się manipulowanie tym parametrem.
#    zakres: (0, 1]
# 6. min_child_weight - interpretacja podobna do minimalnej liczebności
#    obserwacji w węźle końcowym
#    zakres: [0, nieskończoność]
# 7. subsample - wielkośc podpróbki losowanej ze zbioru treningowego
#    wykorzystywanej do trenowania modelu. 
#    1 oznacza 100% (całość zbioru treningowego)
#    typowe wartości: [0.5, 1]


#Wybierz względnie dużą wartość learning rate. Domyślną jest 0.1, ale równie dobrze można zacząć od wartości
#między 0.05 i 0.2 (zależnie od problemu).
#Wyznacz optymalną liczbę drzew dla tej wartości learning rate. Zwykle jest to wartość ok. 40-70.
#Uwaga! Wybierz wartość, dla której Twój komputer policzy wynik wystarczająco szybko,
#ponieważ te parametry będą używane do testowania różnych parametrów wielkości drzewa.
#Dokonaj strojenia parametrów związanych z wielkością drzewa dla wybranej learning rate i wielkości drzewa.
#Obniż wartość learning rate i zwiększ odpowiednio liczbę drzew, aby uzyskać stabilniejszy model.

parametry_xgb <- expand.grid(nrounds = seq(20, 80, 10),
                             max_depth = c(4),
                             eta = c(0.25), 
                             gamma = 1,
                             colsample_bytree = c(0.2),
                             min_child_weight = c(150),
                             subsample = 0.8)

#Poziomy zmiennych jakościowych należy przekształcić, aby nie zaczynały się od cyfry.
#Jest to wymagane przez method = "xgbTree" w funkcji train().

levels(cc_data_train_fct$default.payment.next.month) <- c("No","Yes")
table(cc_data_train_fct$default.payment.next.month)

cc_data.xgb_m3 <- train(model1.formula,
                     data = cc_data_train_fct,
                     method = "xgbTree",
                     trControl = ctrl_cv3,
                     tuneGrid  = parametry_xgb)

glimpse(cc_data_train_fct)

cc_data.xgb_m3 # dla nrounds=80 najlepsze dopasowanie sensivity i najwiekszy ROC
#postaramy sie teraz dopasowac pozostale parametry

parametry_xgb2 <- expand.grid(nrounds = 80,
                              max_depth = seq(1, 15, 2),
                              eta = c(0.25), 
                              gamma = 1,
                              colsample_bytree = c(0.2),
                              min_child_weight = seq(50, 500, 50),
                              subsample = 0.8)


cc_data.xgb_m4 <- train(model1.formula,
                      data = cc_data_train_fct,
                      method = "xgbTree",
                      trControl = ctrl_cv3,
                      tuneGrid  = parametry_xgb2)

cc_data.xgb_m4 #ROC 0.7726374 dla max_depth = 13 i min_child_weight = 200

parametry_xgb3 <- expand.grid(nrounds = 80,
                              max_depth = 13,
                              eta = c(0.25), 
                              gamma = 1,
                              colsample_bytree = seq(0.1, 0.8, 0.1),
                              min_child_weight = 200,
                              subsample = 0.8)

cc_data.xgb_m5 <- train(model1.formula,
                      data = cc_data_train_fct,
                      method = "xgbTree",
                      trControl = ctrl_cv3,
                      tuneGrid  = parametry_xgb3)

cc_data.xgb_m5 #ROC 0.7773691 dla colsample_bytree = 0.7

parametry_xgb4 <- expand.grid(nrounds = 80,
                              max_depth = 13,
                              eta = c(0.25), 
                              gamma = 1,
                              colsample_bytree = 0.7,
                              min_child_weight = 200,
                              subsample = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95))


cc_data.xgb_m6 <- train(model1.formula,
                      data = cc_data_train_fct,
                      method = "xgbTree",
                      trControl = ctrl_cv3,
                      tuneGrid  = parametry_xgb4)

cc_data.xgb_m6 # The final values used for the model were nrounds = 80, max_depth = 13, eta = 0.25, gamma =
#1, colsample_bytree = 0.7, min_child_weight = 200 and subsample = 0.95.

parametry_xgb5 <- expand.grid(nrounds = 1000,
                              max_depth = 13,
                              eta = 0.01, 
                              gamma = 1,
                              colsample_bytree = 0.7,
                              min_child_weight = 200,
                              subsample = 0.95)

cc_data.xgb_m7 <- train(model1.formula,
                      data = cc_data_train_fct,
                      method = "xgbTree",
                      trControl = ctrl_cv3,
                      tuneGrid  = parametry_xgb5)

cc_data.xgb_m7

#najlepszy ROC uzyskany 0.7762343 -> po zwiekszeniu do 1000 rund i eta 0.01 roc zmienia sie na 0.7785012

#CATBOOST
#zbior w wymaganej postaci przez catboost
catboost_train    <- cc_data_train[,!names(cc_data_train) %in% c("id", "default.payment.next.month")] %>% as.data.frame()
catboost_train_targets <- cc_data_train$default.payment.next.month
catboost_train_pool    <- catboost.load_pool(data = catboost_train, label = catboost_train_targets)

cat_params <- list(iterations = 10000,
               learning_rate = 0.001,
               depth = 5,
               loss_function = 'CrossEntropy',
               eval_metric = 'Recall',
               random_seed = 1235,
               od_type = 'Iter',
               metric_period = 50,
               od_wait = 50,
               use_best_model = T
               )

cc_data.cat_m8 <- catboost.train(catboost_train_pool, params = cat_params)

cc_data.cat_m8

test_data    <- cc_data_test[,!names(cc_data_train) %in% c("id", "default.payment.next.month")] %>% as.data.frame()
test_targets <- cc_data_test$default.payment.next.month
real_pool    <- catboost.load_pool(test_data)

catboost_pred <- catboost.predict(cc_data.cat_m8, real_pool)

#Miary jakosci prognozy
tibble(
  pred = catboost_pred,
  actual = test_targets
) %>%
  mutate(ae = abs(pred - actual),
         se = (pred - actual) ^ 2) %>%
  summarize(mae = mean(ae),
            mse = mean(se))

#Wykres jakosci predyktorow

cat_fi_m8 <- catboost.get_feature_importance(cc_data.cat_m8, 
                                      pool = NULL, 
                                      type = 'FeatureImportance',
                                      thread_count = -1) 
tibble(
  feat = rownames(cat_fi_m8) %>% as.factor(),
  imp  = cat_fi_m8[, 1]
) %>% 
  ggplot(aes(reorder(feat, imp), imp)) +
  geom_bar(stat = "identity") + 
  coord_flip()

#NEURAL NET


#Trenowanie sieci neuronowej
#Wszystkie zmienne ciągłe (a zatem inne niż zmienne jakościowe)
#zanim trafią do sieci muszą zostać wystandaryzowane do wspólnej skali.


#Niestety, pakiet neuralnet nie działa sprawnie na zmiennych jakościowych (tj. na faktorach).
#Musimy je zatem wszystkie przekształcić do odpowiednich zmiennych zerojedynkowych (wymaga tego funkcja neuralnet).


cc_data_train_matrix <- 
  model.matrix(object = model1.formula, 
               data   = cc_data_train)
dim(cc_data_train_matrix)

colnames(cc_data_train_matrix)

#manualna poprawa nazw zmiennych
colnames(cc_data_train_matrix) <- gsub(" ", "_",  colnames(cc_data_train_matrix))
colnames(cc_data_train_matrix) <- gsub("/", "",   colnames(cc_data_train_matrix))

colnames(cc_data_train_matrix)

#analogicznie dla test
cc_data_test_matrix <- 
  model.matrix(object = model1.formula, 
               data   = cc_data_test)

colnames(cc_data_test_matrix) <- gsub(" ", "_",  colnames(cc_data_test_matrix))
colnames(cc_data_test_matrix) <- gsub("/", "",   colnames(cc_data_test_matrix))

colnames(cc_data_test_matrix)


#Następnie dokonamy standaryzacji (przeskalowania) zmiennych. 
#Ten etap jest niezbędny, ponieważ w sieciach neuronowych funkcje aktywujące poszczególne neurony przyjmują
#wartości między -1 a +1, zaś predyktory w zbiorze treningowym zwykle przyjmują wartości z różnych skal

(nn_train.maxs <- apply(cc_data_train_matrix, 2, max))
(nn_train.mins <- apply(cc_data_train_matrix, 2, min))

cc_data_train_matrix_scaled <- 
  as.data.frame(scale(cc_data_train_matrix, 
                      center = nn_train.mins, 
                      scale  = nn_train.maxs - nn_train.mins))

cc_data_test_matrix_scaled <- 
  as.data.frame(scale(cc_data_test_matrix, 
                      center = nn_train.mins, 
                      scale  = nn_train.maxs - nn_train.mins))


cc_data_train_matrix_scaled <- cc_data_train_matrix_scaled[,!names(cc_data_train_matrix_scaled) %in% c("(Intercept)", "PAY_4P1")] %>% as.data.frame()
cc_data_test_matrix_scaled <- cc_data_test_matrix_scaled[,!names(cc_data_test_matrix_scaled) %in% c("(Intercept)", "PAY_4P1")] %>% as.data.frame()

#zaktualizowana formula modelu

col_list <- 
  paste(c(colnames(cc_data_train_matrix_scaled[, -1])), collapse = "+")
col_list <- paste(c("default.payment.next.month ~ ", col_list), collapse = "")
(model.formula2 <- formula(col_list))

#Trenowanie modelu

cc_data.nn_m9 <- 
  data.frame(cc_data_train_matrix_scaled,
             default.payment.next.month= as.numeric(cc_data_train$default.payment.next.month)) %>%
  neuralnet(model.formula2,
            data = .,
            hidden = c(1), # liczba neuronów w warstwach ukrytych
            linear.output = FALSE, # T dla regresji, F dla klasyfikacji
            learningrate.limit = NULL,
            learningrate.factor = list(minus = 0.5, plus = 1.2),
            algorithm = "rprop+",
            threshold = 0.01,
            stepmax=1e+07)

cc_data.nn_m9$result.matrix

cc_data.nn_10 <- 
  data.frame(cc_data_train_matrix_scaled,
             default.payment.next.month= as.numeric(cc_data_train$default.payment.next.month)) %>%
  neuralnet(model.formula2,
            data = .,
            hidden = c(10,3), # liczba neuronów w warstwach ukrytych
            linear.output = FALSE, # T dla regresji, F dla klasyfikacji
            learningrate.limit = NULL,
            learningrate.factor = list(minus = 0.5, plus = 1.2),
            algorithm = "rprop+",
            threshold = 0.01,
            stepmax=1e+07)

cc_data.nn_m10$result.matrix

#Warning message:
#Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 
# stepmax=1e7 - opcja do trenowania jezeli nie osiaga kryterium konwergencji


#Choć nie istnieje jedna sztywna i uniwersalna reguła określająca właściwą liczbę warstw 
#i neuronów sieci to powszechnie przyjęte są pewne reguły kciuka. Dla większości aplikacji 
#zwykle wystarcza jedna wartswa ukryta (jeśli w ogóle jest potrzebna). Właściwa liczba neuronów 
#waha sie między wymiarem warstwy wejściowej a wymiarem warstwy wyjściowej, zwykle ok. 2/3 warstwy wejściowej.
#Nie gwarantuje to jednak w żaden sposób powodzenia w uczeniu sieci i skutecznej predykcji.

