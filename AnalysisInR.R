#Installing Packages in R

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

