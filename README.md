
## Exploratory Data ananlysis, Prinicipal Comonent Classification and Linear Regression on IBM HR Attrition Data Kaggle

The following project uses various R-packages to explore relationship between relevant variables in IBM HR Attrition dataset. The following 
code below contains visualization and models used to analyzed the data. 

## Demo code

#------Packages----------# 

library(tidyverse)
library(knitr)
library(gridExtra)
library(purrr)
library(tibble)
library(psych)
library(performance)
library(see)

#------DATA------# 
IBM_HR <- read.csv("D:/Multi-level-Employee-Attrition/IBM_HR.csv")

EDA_COR <- signif(cor(IBM_HR[,c(4, 6, 7, 11, 13, 14, 17, 19, 
                                20, 21, 24, 25, 26, 29, 30, 31, 32, 33, 34, 35)]), 2)

ggcorrplot::ggcorrplot(EDA_COR)

#----Principal_Component_analysis----# 
- Attrition classification failed based on prinicpal component analysis
#----GLM-----# 
Mod1 <- glm(Attrition ~ DailyRate + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
                      MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + PerformanceRating + 
                       RelationshipSatisfaction + TotalWorkingYears + WorkLifeBalance + YearsInCurrentRole + 
                       YearsSinceLastPromotion + YearsWithCurrManager, family = binomial, data = IBM_HR)
summary(Mod1)

anova(Mod1)

model_performance(Mod1)

Mod2 <- glm(Attrition ~ DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + NumCompaniesWorked + 
              TotalWorkingYears + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, family = binomial, data = IBM_HR)

summary(Mod2)

model_performance(Mod2)

test_performance(Mod1, Mod2)

Mod3 <- glm(Attrition ~ DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + NumCompaniesWorked + 
              TotalWorkingYears + YearsInCurrentRole + YearsSinceLastPromotion, family = binomial, data = IBM_HR)

summary(Mod3)

model_performance(Mod3)

test_performance(Mod1, Mod3)

Performance_test_mods <- compare_performance(Mod1, Mod2, 
                                             Mod3, rank = T)

Performance_test_mods
pdf("Model_indicies_performance_glm.pdf", height = 12, width = 16, paper = "USr")
plot(Performance_test_mods)
dev.off()

 
pdf("Department_ratio_hike.pdf", height =12, width = 16, paper = "USr")
ggplot(data = IBM_HR, aes(x = PercentSalaryHike, color = Department)) + 
  geom_histogram(bins = 15)
dev.off()

#---------Hike-In-Salary------------#
Mod1_Salary <- lm(PercentSalaryHike ~ Education + JobInvolvement + JobLevel + NumCompaniesWorked + 
                    PerformanceRating + TotalWorkingYears + YearsInCurrentRole, data = IBM_HR)
summary(Mod1_Salary)

Mod2_Salary <- lm(PercentSalaryHike ~ PerformanceRating, data = IBM_HR)
summary(Mod2_Salary)

Mod3_Salary <- lm(PercentSalaryHike ~ PerformanceRating + YearsWithCurrManager + PerformanceRating * YearsWithCurrManager, data = IBM_HR)
summary(Mod3_Salary)

Mod4_Salary <- lm(PercentSalaryHike ~ PerformanceRating + YearsWithCurrManager + TrainingTimesLastYear, data = IBM_HR)
summary(Mod4_Salary)

anova(Mod1_Salary, Mod2_Salary, Mod3_Salary, Mod4_Salary)
compare_performance(Mod1_Salary, Mod2_Salary, Mod3_Salary, Mod4_Salary, rank = T)

#--------Mod2_Selected---------------# 

pdf("Performance_Stats.pdf", height =12, width = 16, paper = "USr")
check_model(Mod2_Salary)
dev.off()
