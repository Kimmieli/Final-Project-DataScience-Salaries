library(dplyr)
library(readxl)
library(ggplot2)
install.packages('treemap')
library(treemap)







Salary2 = subset(Data.Fram.in.Excel.Data.Science.Salaries.2021, select = -c(1:23, 40:42))
salary3 <- as.matrix(Salary2)
str(Salary2$NewSector)
Salary2$NewSector <- as.numeric(Salary2$NewSector)
str(Salary2$NewSector)
str(Salary2$Python)
heatmap(salary3)

treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Type.of.ownership"), vSize="Avg.Salary.K.", type="index")
treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Sector"), vSize = "Lower.Salary", type = "index")
treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Sector"), vSize = "Upper.Salary", type = "index")


Data_Science_Salaries_cleaned_2021$NewSector <- NA
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Information Technology'] <- 1
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Biotech & Pharmaceuticals'] <- 2
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Business Services']<- 3
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Insurance'] <- 4
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Health Care'] <- 5
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Finance'] <- 6
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Manufacturing'] <- 7
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Aerospace & Defense'] <- 8
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Education'] <- 9
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Retail'] <- 10
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Oil,Gas, Energy & Utlities'] <- 11
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Government'] <- 12
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='-1'] <- 13
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Non-Profit'] <- 14
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Transportation & Logistics'] <- 15
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Real Estate'] <- 16
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Travel & Tourism'] <- 17
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Telecommunications'] <- 18
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Media'] <- 19
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Arts, Entertainment & Recreation'] <- 20
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Consumer'] <- 21
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Mining & Metals'] <- 22
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Construction, Repair & Maintenance'] <- 23
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Agriculture & Forestry'] <- 24
Data_Science_Salaries_cleaned_2021$NewSector[Data_Science_Salaries_cleaned_2021$NewSector=='Accounting & Legal'] <- 25

library(dplyr)
Data.Fram.in.Excel.Data.Science.Salaries.2021 <- subset (Data.Fram.in.Excel.Data.Science.Salaries.2021, select = -NewSector)

Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Information Technology'] <- "1"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Biotech & Pharmaceuticals'] <- "2"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Business Services']<- "3"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Insurance'] <- "4"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Health Care'] <- "5"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Finance'] <- "6"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Manufacturing'] <- "7"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Aerospace & Defense'] <- "8"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Education'] <- "9"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Retail'] <- "10"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Oil, Gas, Energy & Utilities'] <- "11"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Government'] <- "12"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='-1'] <- "13"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Non-Profit'] <- "14"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Transportation & Logistics'] <- "15"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Real Estate'] <- "16"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Travel & Tourism'] <- "17"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Telecommunications'] <- "18"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Media'] <- "19"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Arts, Entertainment & Recreation'] <- "20"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Consumer'] <- "21"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Mining & Metals'] <- "22"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Construction, Repair & Maintenance'] <- "23"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Agriculture & Forestry'] <- "24"
Data.Fram.in.Excel.Data.Science.Salaries.2021$NewSector[Data.Fram.in.Excel.Data.Science.Salaries.2021$Sector=='Accounting & Legal'] <- "25"


library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")
salarieslogit <- glm(as.factor(NewSector) ~ Python, data = Data.Fram.in.Excel.Data.Science.Salaries.2021, family = "binomial")
probabilities <- predict(salarieslogit, type = "response")

install.packages("qpcR")
library("qpcR")
bindSalaries <- qpcR:::cbind.na(Data.Fram.in.Excel.Data.Science.Salaries.2021, probabilities)
bindSalaries


# Ran a stepwise of means of skills.  Which had significance to where it can be compared to the other skills.

FitAll= lm(NewSector ~ Python + spark + aws + excel + sql + sas + keras + pytorch + scikit + tensor + hadoop + tableau + bi + flink + mongo + google_an, data = bindSalaries)
summary(FitAll)
step(FitAll, direction = 'backward')
fitSome = lm(NewSector ~ spark + aws + tensor + bi + mongo + google_an, data = bindSalaries)
summary(fitSome)


