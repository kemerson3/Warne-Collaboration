##Warne-Collaboration: Relative Brain Shape

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

df$Log_DL = as.numeric(df$Log_DL)
df$Log_TW = as.numeric(df$Log_TW)
df$Log_TL = as.numeric(df$Log_TL)
df$Log_OTW = as.numeric(df$Log_OTW)
df$Log_OTL = as.numeric(df$Log_OTL)
df$Log_DW = as.numeric(df$Log_DW)
df$Log_MW = as.numeric(df$Log_MW)

shapiro.test(df$Log_TW)
shapiro.test(df$Log_TL)
shapiro.test(df$Log_OTW)
shapiro.test(df$Log_OTL)
shapiro.test(df$Log_DW)
shapiro.test(df$Log_DL)
shapiro.test(df$Log_MW)
#All good except MW

leveneTest(df$Log_TW, df$Treatment, center = mean, na.rm = TRUE)
leveneTest(df$Log_TL, df$Treatment, center = mean, na.rm = TRUE)
leveneTest(df$Log_OTW, df$Treatment, center = mean, na.rm = TRUE)
leveneTest(df$Log_OTL, df$Treatment, center = mean, na.rm = TRUE)
leveneTest(df$Log_DW, df$Treatment, center = mean, na.rm = TRUE)
leveneTest(df$Log_DL, df$Treatment, center = mean, na.rm = TRUE)
leveneTest(df$Log_MW, df$Treatment, center = mean, na.rm = TRUE)
#All good

man1 <- manova(cbind(Log_TW, Log_TL, Log_OTW, Log_OTL, Log_DW, Log_DL, Log_MW)~Treatment*Log_Brain_Mass, data = df)
summary(man1)
#MANCOVA to test the homogeneity of slopes. We will look how each specific brain dimension responded to both treatments
#while maintaining relative brain mass as a covariate

summary.aov(man1)
#DL had a marginally significant interaction with the treatment + brain mass, 
#signifying the slopes of the lines for these traits were not parallel amongst treatments. 
#We will continue on and address this later, but it was marginal (0.045) 

modelTW <- lm(Log_TW ~ Treatment*Log_Brain_Mass, data = df)
means_complexTW <- estimate_means(modelTW)

modelTL <- lm(Log_TL ~ Treatment*Log_Brain_Mass, data = df)
means_complexTL <- estimate_means(modelTL)

modelOTW <- lm(Log_OTW ~ Treatment*Log_Brain_Mass, data = df)
means_complexOTW <- estimate_means(modelOTW)

modelOTL <- lm(Log_OTL ~ Treatment*Log_Brain_Mass, data = df)
means_complexOTL <- estimate_means(modelOTL)

modelDW <- lm(Log_DW ~ Treatment*Log_Brain_Mass, data = df)
means_complexDW <- estimate_means(modelDW)

modelDL <- lm(Log_DL ~ Treatment*Log_Brain_Mass, data = df)
means_complexDL <- estimate_means(modelDL)

modelMW <- lm(Log_MW ~ Treatment*Log_Brain_Mass, data = df)
means_complexMW <- estimate_means(modelMW)
#Now have correct EMMs for each log_brain shape measurement

residuals <- man1$residuals
man1$residuals
#These are our residuals for our brain shape measurements

write.table(residuals, file = "dfwithresidualsshape.csv", sep = ",")

df.pca = df[,28:34]

KMO(df.pca)
#Pass KMO

cortest.bartlett(df.pca)
#Pass Bartlett

pca.p <- principal(df.pca, nfactors = 3, rotate = "varimax")
#Output: Values = Eigenvalues, Scores = Factor Scores
#https://www.rdocumentation.org/packages/psych/versions/2.1.6/topics/principal

qplot(c(1:7), pca.p$values) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Eigenvalue") +
  ylim(0,6)

pca.p$values
#eigenvalues

pca.p$scores
#factor values

Factor <- pca.p$scores
df <- cbind(df, Factor)
#added to df

pca.p$loadings
#loadings

pca1_glmm <- glm(RC1~Treatment, data = df, na.action = na.omit, family = "gaussian")
Anova(pca1_glmm)

pca2_glmm <- glm(RC2~Treatment, data = df, na.action = na.omit, family = "gaussian")
Anova(pca2_glmm)
#close

pca3_glmm <- glm(RC3~Treatment, data = df, na.action = na.omit, family = "gaussian")
Anova(pca3_glmm)
#significant!

#Plots time
ggplot(df, aes(x= Treatment, y = RC1, fill = Treatment)) +
  geom_boxplot(width = .8) +
  stat_boxplot(geom = "errorbar", width = .8) +
  theme_classic() +
  labs(x = "Treatment", y = "PC1") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "bold", size = 16))  +
  theme(axis.title = element_text(face = "bold", size = 18)) +
  theme(legend.position = "none")


ggplot(df, aes(x= Treatment, y = RC2, fill = Treatment)) +
  geom_boxplot(width = .8) +
  stat_boxplot(geom = "errorbar", width = .8) +
  theme_classic() +
  labs(x = "Treatment", y = "PC2") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "bold", size = 16))  +
  theme(axis.title = element_text(face = "bold", size = 18)) +
  theme(legend.position = "none")

ggplot(df, aes(x= Treatment, y = RC3, fill = Treatment)) +
  geom_boxplot(width = .8) +
  stat_boxplot(geom = "errorbar", width = .8) +
  theme_classic() +
  labs(x = "Treatment", y = "PC3") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "bold", size = 16))  +
  theme(axis.title = element_text(face = "bold", size = 18)) +
  theme(legend.position = "none")
