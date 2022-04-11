library(dplyr)
library(readxl)
library(ggplot2)
install.packages('treemap')
library(treemap)








treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Type.of.ownership"), vSize="Avg.Salary.K.", type="index")
treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Sector"), vSize = "Lower.Salary", type = "index")
treemap(Data.Fram.in.Excel.Data.Science.Salaries.2021, index=c("Sector"), vSize = "Upper.Salary", type = "index")
# The tree maps show how much more money one would have by the type of Company.  There is more money working for a private company and a public company
# The Tree map also show is if there is a difference in salary between sectors.  There is not much of a difference in salary change lower and uppper. 
## The top 5 sectors that make the most are in IT, Biotech & Phafmaceuticals, Business, Insurance, and Finance.



library(dplyr)

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


# Ran a stepwise of means of skills. There was significance to where it can be compared to the other skills.

FitAll= lm(NewSector ~ Python + spark + aws + excel + sql + sas + keras + pytorch + scikit + tensor + hadoop + tableau + bi + flink + mongo + google_an, data = bindSalaries)
summary(FitAll)
step(FitAll, direction = 'backward')
fitSome = lm(NewSector ~ spark + aws + tensor + bi + mongo + google_an, data = bindSalaries)
summary(fitSome)

# Ater the stepswise the results were  that there was significance to if the skills of: spark, aws, tensor, bi, mongo, google_an; was either an important skill for the sector or not. 
## If we were to run a pobability test these skills will make a difference on what skills make a difference between sectors.

library('rcompanion')
library('car')
library('effects')
library('multcomp')
library('IDPmisc')
library('dplyr')
library('psych')


## DATA WRANGLING for ANCOVA
## Question to answer:  If a person looking for a job had a certain skill in data science, 
## does the Sector depend on where the job is located?

keeps <- c("Python", "spark", "aws", "sas", "keras", "scikit", "flink", "mongo", "excel", "sql", "pytorch", "tensor", "hadoop", "tableau", "bi", "google_an", "Location", "Sector")
dataKeep = Data.Fram.in.Excel.Data.Science.Salaries.2021[keeps]


dataKeep2 <- head(dataKeep, 25)
dataKeep2$Location <- as.integer(as.factor(dataKeep2$Location))
dataKeep2$Sector <- as.integer(as.factor(dataKeep2$Sector))

# TESTING ASSUMPTIONS
# Plot Nornmality


plotNormalHistogram(dataKeep2$Location)
dataLocation.plot <- sqrt(dataKeep2$Location)
plotNormalHistogram(dataLocation.plot)
# This plot shows normal distribution of skills by location.  

plotNormalHistogram((dataKeep2$Sector))
# This plot shows normal distribution of skills by Sector.

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
## There is no variance to where the skills one might have in a Sector in any location.

##Test for Homogeneity of Regression Slopes
Homogeneity_RegrSlp = lm(dataLocation.plot ~ Python, data = dataKeep2)
anova(Homogeneity_RegrSlp)
### The P-value is .5888, not significant. We can state that depending on if you have the skill of Python, it will not influence
### where the job is located.

#RUNNING THE ANCOVA ANALYSIS
ANCOVA = lm(dataLocation.plot ~ Python + Sector*Python, data = dataKeep2)
anova(ANCOVA)
## The test was not significant.  So having the skill 'Python' does not have any impact on
## which sector of a company in any location.

keepsP2 <- c("spark", "aws", "tensor","bi", "mongo", "google_an", "Location", "Sector")
dataKeep2 = Data.Fram.in.Excel.Data.Science.Salaries.2021[keepsP2]


dataKeepA <- head(dataKeep2, 25)
dataKeepA$Location <- as.integer(as.factor(dataKeepA$Location))
dataKeepA$Sector <- as.integer(as.factor(dataKeepA$Sector))

# TESTING ASSUMPTIONS
# Plot Nornmality


plotNormalHistogram(dataKeepA$Location)
dataLocation2.plot <- sqrt(dataKeepA$Location)
plotNormalHistogram(dataLocation2.plot)
# This plot shows normal distribution of skills by location.  

plotNormalHistogram((dataKeepA$Sector))
# This plot shows normal distribution of skills by Sector.

# Test for Homogeneity of Variance

str(dataKeepA$Location)
dataKeepA$Location <- as.factor(dataKeepA$Location)
str(dataKeepA$Location)

str(dataKeepA$Sector)
dataKeepA$Sector <- as.factor(dataKeepA$Sector)
str(dataKeepA$Sector)

str(dataKeepA$spark)
dataKeepA$Python <- as.factor(dataKeepA$spark)
str(dataKeepA$Python)

leveneTest(dataLocation2.plot ~ Sector, data = dataKeepA)
## This test had no significance. P-value - .2203 which is greater than .05.
## There is no variance to where the skills one might have in a Sector in any location.

##Test for Homogeneity of Regression Slopes
Homogeneity_RegrSlp1 = lm(dataLocation2.plot ~ spark, data = dataKeepA)
anova(Homogeneity_RegrSlp1)
### The P-value is 1.2493, not significant. We can state that depending on if you have the skill of spark, it will not influence
### where the job is located.

#RUNNING THE ANCOVA ANALYSIS
ANCOVA = lm(dataLocation2.plot ~ aws + Sector*spark, data = dataKeepA)
anova(ANCOVA)
## Not Significant Again.



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
# This plot shows that the Age of the company does not affect the growth of salary.  
# It shows that companies that have a higher age will not have as many high paying jobs as those that may be younger in age.  
# The steady salary are of companies less than 50 years old, and have a range of 50 to 150 thousand in salary.
# Assumption for Linearity not met.

#2. Testing for Homoscedasticity

#Create a linear model. Showing Residual Analysis

lmMod <- lm(Age~`Avg.Salary.K.` + Rating, data=Salaries)
# There is an inference of Heteroscedasticity, the residuals seem to increase as the fitted Y values increase.

#Creating a series of graphs to test for homoscedasticity.
par(mfrow=c(2,2))
plot(lmMod)

#Another Test for homoscedasticity with the Breush-Pagan test
lmtest::bptest(lmMod)
## This p-value is significant at .0009. This not Homoscedasticity.

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

#Outliers in Rows 179,271,and 504. These were the values that did not fit the norm.

#Test for leverage
lev = hat(model.matrix(lmMod))
plot(lev)

Salaries2[lev>.2,]
# According to the plot, there are extreme values in x space that are outliers.

#Testing for Outliers in the y-space
car::outlierTest(lmMod)

# p.values < 0.05 which means there are outliers in the y-space.

#Testing for Outliers in both the x and y space
summary(influence.measures(lmMod))

#No outlier above 0.2

#Running and Interpreting the Analysis
summary (lmMod)


df <- c("Avg.Salary.K.","Age")
Salaries1 <- Salaries[df]


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

# There are extreme values in the x and y space.  


#Running and Interpreting the Analysis
summary (lmMod)
# Linear regression  p-value of .6881 is not significant when comparing age of the company to avg salary and rating.


