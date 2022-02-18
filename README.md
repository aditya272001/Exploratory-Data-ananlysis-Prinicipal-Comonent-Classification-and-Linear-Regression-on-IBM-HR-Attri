
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
![Screenshot 2022-02-18 124748](https://user-images.githubusercontent.com/96023170/154722026-a9f335e3-c1ef-41d2-96be-44a1e0475329.png)

#----Principal_Component_analysis----# 
- Attrition classification failed based on prinicpal component analysis
- ![Screenshot 2022-02-18 125039](https://user-images.githubusercontent.com/96023170/154722078-a92daefc-d473-4371-b0c4-9338e8811a97.png)

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
![Screenshot 2022-02-18 125313](https://user-images.githubusercontent.com/96023170/154722118-a34b670e-1158-4a22-9dd5-06054fd06e07.png)

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
![Screenshot 2022-02-18 215444](https://user-images.githubusercontent.com/96023170/154722263-32f381e5-9c63-4d7b-a98e-d219c29d0616.png)

 
pdf("Department_ratio_hike.pdf", height =12, width = 16, paper = "USr")
ggplot(data = IBM_HR, aes(x = PercentSalaryHike, color = Department)) + 
  geom_histogram(bins = 15)
dev.off()
![Screenshot 2022-02-18 215519](https://user-images.githubusercontent.com/96023170/154722500-1349c071-fe09-4707-9bd3-5ede71044b2d.png)

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
![Screenshot 2022-02-18 215540](https://user-images.githubusercontent.com/96023170/154722524-2f9c1852-f100-4971-a933-6d3e1bed81fc.png)


