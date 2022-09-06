##Warne-Collaboration: Relative Brain Mass

library(lme4)
library(car)
library(readr)
library(moments) 
library(psych)
library(pastecs)
library(ggplot2)
library(ggbiplot)
library(tidyverse)
library(modelbased)
library(dplyr)

file.choose()
df <- read.csv("C:\\Users\\kjeme\\OneDrive\\Desktop\\Woodley Lab\\Aim 4 - Warne Collaboration\\WarneDataFrame.csv")
df$Treatment = factor(df$Treatment)

shapiro.test(df$Log_Body_Mass)
shapiro.test(df$Log_Brain_Mass)

leveneTest(df$Log_Brain_Mass, df$Treatment, center = mean, na.rm = TRUE)

#Data is normal and homoscedastic.

anovaBrainMass <- aov(Log_Brain_Mass~Treatment*Log_Body_Mass, data = df)
summary(anovaBrainMass, type = "III")
#Our ANCOVA: No significant interaction between our body mass and treatments, confirming that
#the slopes of lines for our trait (i.e. brain mass) were parallel among each trtmt group

anovaBrainMass$residuals
#The ancova calculated our residuals from this statistical model. We will add these residuals to our EMMs

model <- lm(Log_Brain_Mass~Treatment*Log_Body_Mass, data = df)
means_complex <- estimate_means(model)
means_complex$Mean
#These are our estimated marginal means for each treatment group 

residuals <- anovaBrainMass$residuals

# ID <-df$ID
# LogBrainMass <- df$log_Brain_Mass
# Treatment <- df$Treatment
# df2 <- cbind(ID, LogBrainMass)
# df2 <- cbind(df2, Treatment)
# w = complete.cases(df2)
# df2 = df2[w,]
# df2 <- cbind(df2, residuals)
# write.table(df2, file = "dfwithresidualsmass.csv", sep = ",")
#Now, I have my residuals as a seperate data frame. I will assign the correct residuals
#To the correct tadpole. Then, I will add residuals to the master csv file
#From there, I will add a new column with my EMMs for each correct treatment, and will add my residuals to EMMs
#This gives us our MAV for each brain mass. 

MABM_glmm <- glm(MAV_Brain_Mass~Treatment, data = df, family = "gaussian")
Anova(MABM_glmm)
#No significant effect on brain mass

ggplot(df, aes(x= Treatment, y = MAV_Brain_Mass, fill = Treatment)) +
  geom_boxplot(width = .8) +
  coord_cartesian(ylim = c(-2.75, -2)) +
  stat_boxplot(geom = "errorbar", width = .8) +
  theme_classic() +
  labs(x = "Treatment", y = "Mass-Adjusted Brain Mass") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "bold", size = 16))  +
  theme(axis.title = element_text(face = "bold", size = 18)) +
  theme(legend.position = c(.86, .92)) +
  theme(legend.title = element_text(size = 10)) +
  theme(legend.text = element_text(size = 10))

#Here is an initial plot visualizing our Body Mass Adjusted brain mass values. But because we took a log, we have to take the inverse log of these values when it comes time to plot. 
#But, the statistical analysis was conducted using these values. Done in master csv file
#https://www.rapidtables.com/calc/math/anti-log-calculator.html

ggplot(df, aes(x= Treatment, y = Relative_Brain_Mass, fill = Treatment)) +
  geom_boxplot(width = .8) +
  coord_cartesian(ylim = c(5, 8)) +
  stat_boxplot(geom = "errorbar", width = .8) +
  theme_classic() +
  labs(x = "Treatment", y = "Relative Brain Mass (mg)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "bold", size = 16))  +
  theme(axis.title = element_text(face = "bold", size = 18)) +
  theme(legend.position = "none") 
