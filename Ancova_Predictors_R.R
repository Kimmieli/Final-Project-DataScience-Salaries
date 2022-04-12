<<<<<<< Updated upstream
library('rcompanion')
library('car')
library('effects')
library('multcomp')
library('IDPmisc')
library('dplyr')
library('psych')

#Load in dataset

Salaries <- read.csv('C:/Users/sever/Desktop/JennsSchool/FinalProject/Datasets/data_cleaned_2021.csv')

# Data Wrangling

keeps <- c("spark", "aws", "mongo", "tensor", "bi", "google_an", "Location", "Sector")
dataKeep = Salaries[keeps]

dataKeep2 <- head(dataKeep, 25)
dataKeep2$Location <- as.integer(as.factor(dataKeep2$Location))
dataKeep2$Sector <- as.integer(as.factor(dataKeep2$Sector))

# Assemptions test
# Normalitity Plot

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

leveneTest(dataLocation.plot ~ Sector, data = dataKeep2)
# This test had no significance. P-value - 1.5616 which is greater than .05.


#Test for Homogeneity of Regression Slopes

Homogeneity_RegrSlp = lm(dataLocation.plot ~ spark, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ aws, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ tensor, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ bi, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ mongo, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ google_an, data = dataKeep2)
anova(Homogeneity_RegrSlp)

## NOne of the p-values are significant. We can say the location has no bering on the skill required. 

# The Ancova

ANCOVA = lm(dataLocation.plot ~ spark + Sector*spark, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ aws + Sector*aws, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ tensor + Sector*tensor, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ bi + Sector*bi, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ mongo + Sector*mongo, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ google_an + Sector*google_an, data = dataKeep2)
anova(ANCOVA)

=======
library('rcompanion')
library('car')
library('effects')
library('multcomp')
library('IDPmisc')
library('dplyr')
library('psych')

#Load in dataset

Salaries <- read.csv('C:/Users/sever/Desktop/JennsSchool/FinalProject/Datasets/data_cleaned_2021.csv')

# Data Wrangling

keeps <- c("spark", "aws", "mongo", "tensor", "bi", "google_an", "Location", "Sector")
dataKeep = Salaries[keeps]

dataKeep2 <- head(dataKeep, 25)
dataKeep2$Location <- as.integer(as.factor(dataKeep2$Location))
dataKeep2$Sector <- as.integer(as.factor(dataKeep2$Sector))

# Assemptions test
# Normalitity Plot

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

leveneTest(dataLocation.plot ~ Sector, data = dataKeep2)
# This test had no significance. P-value - 1.5616 which is greater than .05.


#Test for Homogeneity of Regression Slopes

Homogeneity_RegrSlp = lm(dataLocation.plot ~ spark, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ aws, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ tensor, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ bi, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ mongo, data = dataKeep2)
anova(Homogeneity_RegrSlp)

Homogeneity_RegrSlp = lm(dataLocation.plot ~ google_an, data = dataKeep2)
anova(Homogeneity_RegrSlp)

## NOne of the p-values are significant. We can say the location has no bering on the skill required. 

# The Ancova

ANCOVA = lm(dataLocation.plot ~ spark + Sector*spark, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ aws + Sector*aws, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ tensor + Sector*tensor, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ bi + Sector*bi, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ mongo + Sector*mongo, data = dataKeep2)
anova(ANCOVA)

ANCOVA = lm(dataLocation.plot ~ google_an + Sector*google_an, data = dataKeep2)
anova(ANCOVA)

>>>>>>> Stashed changes
# No significants in any of the Ancovas. Having any of the these skills are not impacted by the sector or location for any company.