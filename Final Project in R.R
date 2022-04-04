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
Salary2$Python <- as.numeric(Salary2$Python)
str(Salary2$Python)
heatmap(salary3)
install.packages('pheatmap')
library(pheatmap)
pheatmap(salary3, cutree_rows = 4)
install.packages('gplots')
library("gplots")
heatmap.2(salary3, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none")

## This heat nap gives the information of how the skills are influenced by salaries. Does the Salary make the skill more important.

treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Type.of.ownership"), vSize="Avg.Salary.K.", type="index")
treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Sector"), vSize = "Lower.Salary", type = "index")
treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Sector"), vSize = "Upper.Salary", type = "index")




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


library('rcompanion')
library('car')
library('effects')
library('multcomp')
library('IDPmisc')
library('dplyr')
library('psych')


## DATA WRANGLING

keeps <- c("Python", "spark", "aws", "sas", "keras", "scikit", "flink", "mongo", "excel", "sql", "pytorch", "tensor", "hadoop", "tableau", "bi", "google_an", "Location", "Sector")
dataKeep = Data.Science.Salaries._cleaned.2021[keeps]


dataKeep2 <- head(dataKeep, 25)
dataKeep2$Location <- as.integer(as.factor(dataKeep2$Location))
dataKeep2$Sector <- as.integer(as.factor(dataKeep2$Sector))

# TESTING ASSUMPTIONS
# Plot Nornmality


plotNormalHistogram(dataKeep2$Location)
dataLocation.plot <- sqrt(dataKeep2$Location)
plotNormalHistogram(dataLocation.plot)

plotNormalHistogram((dataKeep2$Sector))


# Test for Homogeneity of Variance

str(dataKeep2$Location)
dataKeep2$Location <- as.factor(dataKeep2$Location)
str(dataKeep2$Location)

str(dataKeep2$Sector)
dataKeep2$Sector <- as.factor(dataKeep2$Sector)
str(dataKeep2$Sector)

str(dataKeep2$Python)
dataKeep2$Python <- as.factor(dataKeep2$Python)
str(dataKeep2$Python)

leveneTest(dataLocation.plot ~ Sector, data = dataKeep2)
## This test had no significance. P-value - .2203 which is greater than .05.

##Test for Homogeneity of Regression Slopes
Homogeneity_RegrSlp = lm(dataLocation.plot ~ Python, data = dataKeep2)
anova(Homogeneity_RegrSlp)
### The P-value is not significant, so we can state that depending on if you have the skill of Python, it will not influence
### where the job is located.

#RUNNING THE ANCOVA ANALYSIS
ANCOVA = lm(dataLocation.plot ~ Python + Sector*Python, data = dataKeep2)
anova(ANCOVA)
## The test was not significant.  So having the skill 'Python' does not have any impact on
## which sector of a company in any location.
dataLocation.plot2 <- as.factor(dataLocation.plot)
rm(dataLocation.plot2)


#Final Project Analysis- Data Scientist Salaries
install.packages("car")
install.packages("caret")
install.packages("gvlma")
install.packages("predictmeans")
install.packages("e1071")
install.packages("lmtest")

#Loading Libraries
library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")


#Loading Dataset
Salaries = read.csv ("C:/Users/abiak/Documents/GitHub/Final-Project-DataScience-Salaries/Data Science Salaries _cleaned 2021.csv")

View (Salaries)

#Question Set -Up: Is the Average Salary of a Data Scientist influenced by the age of the company?

#Sub-setting the dataset

colnames(Salaries)

df <- c("Avg.Salary.K.","Age")
Salaries1 <- Salaries[df]

View (Salaries1)

## TESTING FOR ASSUMPTIONS

#1. Testing for Linearity

scatter.smooth(x=Salaries1$Avg.Salary.K., y=Salaries1$Age, main="Age of Company by Average Salary")

# Assumption for Linearity not met.

#2. Testing for Homoscedasticity

#Create a linear model

lmMod <- lm(Age~`Avg.Salary.K.` + Rating, data=Salaries)

#Creating a series of graphs to test for homoscedasticity.
par(mfrow=c(2,2))
plot(lmMod)

#Another Test for homoscedasticity with the Breush-Pagan test
lmtest::bptest(lmMod)


#As an alternative for testing homoscedasticity- The NCV Approach
car::ncvTest(lmMod)

#Interpretation: data is heteroscedastic,since p.value is significant for the NCV Test ,assumption of homoscedasticity not met.

#Using Box-Cox to transform the Dependent Variable (Correcting for Homoscedasticity)
BCMod1 <- caret::BoxCoxTrans(Salaries1$Avg.Salary.K.)
print(BCMod1)

#Binding the transformed DV to the sub-set data
Salaries2 <- cbind(Salaries1, newBCMod1=predict(BCMod1, Salaries1$Avg.Salary.K.))

View (Salaries2)

#Creating a new linear model with the transformed DV
lmMod2 <- lm(newBCMod1~Age, data=Salaries2)
lmtest::bptest(lmMod2)


#The GVLMA Library for Assumptions
gvlma(lmMod2)

#Not all assumptions met

#3. Screening for Outliers

#In X Space using Cook's Distance
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)

#Outliers in Rows 179,271,and 504

#Test for leverage
lev = hat(model.matrix(lmMod))
plot(lev)

Salaries2[lev>.2,]

#Testing for Outliers in the y-space
car::outlierTest(lmMod)

#p.values < 0.05 which means there are outliers in the y-space

#Testing for Outliers in both the x and y space
summary(influence.measures(lmMod))

#No outlier above 0.2

#Running and Interpreting the Analysis
summary (lmMod)



