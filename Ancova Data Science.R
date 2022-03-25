## Question to answer:  If a person looking for a job had a certain skill in data science, 
## does the Sector depend on where the job is located?


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
### The P-value is not significant, so we can state that depending on if you have the skill of Python, it will influence
### where the job is located.

#RUNNING THE ANCOVA ANALYSIS
ANCOVA = lm(dataLocation.plot ~ Python + Sector*Python, data = dataKeep2)
anova(ANCOVA)
## The test was not significant.  So having the skill 'Python' does not have any impact on
## which sector of a company in any location.
dataLocation.plot2 <- as.factor(dataLocation.plot)
rm(dataLocation.plot2)
