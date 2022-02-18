#-----library------# 
library(tidyverse)
library(knitr)
library(gridExtra)
library(purrr)
library(tibble)
library(psych)
#------DATA------# 
IBM_HR <- read.csv("D:/Multi-level-Employee-Attrition/IBM_HR.csv")

EDA_COR <- signif(cor(IBM_HR[,c(4, 6, 7, 11, 13, 14, 17, 19, 
                                20, 21, 24, 25, 26, 29, 30, 31, 32, 33, 34, 35)]), 2)

ggcorrplot::ggcorrplot(EDA_COR)

formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>% 
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA)) 
}

cors <- function(df) { 
  # turn all three matrices (r, n, and P into a data frame)
  M <- Hmisc::rcorr(as.matrix(df))
  # return the three data frames in a list return(Mdf)
  Mdf <- map(M, ~data.frame(.x))
}

formatted_cors(IBM_HR[,c(4, 6, 7, 11, 13, 14, 17, 19, 
                         20, 21, 24, 25, 26, 29, 30, 31, 32, 33, 34, 35)]) %>% 
  ggplot(aes(x = measure1, y = measure2, fill = r)) + 
  geom_title() + 
  labs(x = NULL, y = NULL, fill = "Pearson's\n Correaltion", 
       title = "Correlations in IBM HR Data") + 
  # map a red, white and blue color scale to correspond to -1:1 sequential gradient scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  theme_classic() +
  # remove excess space on x and y axes
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  # change global font to roboto
  theme(text=element_text(family="Roboto"))

#-----Attrition-Predictors-----# 
#---Using-PCA---# 
IBM_HR$OverTime[IBM_HR$OverTime == 1] <- 1
IBM_HR$OverTime[IBM_HR$OverTime == "No"] <- 0
#---Particioning-Data---# 
set.seed(123)
Split_prob <- sample(2, nrow(IBM_HR), 
                     replace = T, 
                     prob = c(0.7, 0.3))
training <- IBM_HR[Split_prob==1,]
testing <- IBM_HR[Split_prob==2,]

#-PCA-#
PC <- prcomp(training[,c(4, 6, 11, 14, 17, 19, 21, 24, 25, 26, 
                       29, 31, 33)], 
             center = T, 
             scale. = T)
print(PC)
summary(PC)

#-Orthogonality-# 
pairs.panels(PC$x, 
             gap = 0, 
             bg = c("red", "blue")[training$Attrition], 
             pch = 21)

#-Bi-plot-# 
library(ggbiplot)

BiPlot <- ggbiplot(PC, 
                   obs.scale = 1, 
                   var.scale = 1, 
                   groups = training$Attrition, 
                   ellipse = T, 
                   circle = T, 
                   ellipse.prob = 0.50)

BiPlot <- BiPlot + scale_color_discrete(name = '')
BiPlot <- BiPlot + theme(legend.direction = 'horizontal', 
                         legend.position = 'top')
print(BiPlot)

#-Prediction-# 
trg <- predict(PC, training)
trg <- data.frame(trg, training[2])

tst <- predict(PC, testing)
tst <- data.frame(tst, testing$Attrition)

# Multi-nomial Logistic regression with three PCs 
library(nnet)

trg$Attrition <- relevel(trg$Attrition, ref = "Yes")

mymodel <- multinom(Attrition ~ PC1 + PC2 + PC3, data = trg)
summary(mymodel)

#-Confusion-Matrix-# 
p <- predict(mymodel, trg)
tab <- table(p, trg$Attrition)
tab

#-Highly-misclassified-datasets-based-on-PCA-# 

#-Prediction_through_linear_regression-#
IBM_HR$Attrition[IBM_HR$Attrition == "Yes"] <- 1         
IBM_HR$Attrition[IBM_HR$Attrition == "No"] <- 0
as.numeric(IBM_HR$Attrition)
IBM_HR[,2] <- as.numeric(IBM_HR$Attrition)

Mod1 <- glm(Attrition ~ DailyRate + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
                      MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + PerformanceRating + 
                       RelationshipSatisfaction + TotalWorkingYears + WorkLifeBalance + YearsInCurrentRole + 
                       YearsSinceLastPromotion + YearsWithCurrManager, data = IBM_HR)
summary(Mod1)

anova(Mod1)
