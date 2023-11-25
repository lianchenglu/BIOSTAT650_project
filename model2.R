rm(list = ls())
gc()
set.seed(123)
library(car)
library(ggplot2)
library(olsrr)

############### (1) Data cleaning ########################################
## select variables
library(NHANES)
df0 <- NHANES
df <- NHANES[NHANES$Age >= 18 & NHANES$Age < 60,]
# colSums(is.na(df)) / nrow(df)
df <- df[, which(colSums(is.na(df)) / nrow(df) < 0.3)]
# exclude duplication
df <- df[!duplicated(df),]
names(df)
# df$BPSysAve
library(dplyr)

df2 <- df %>% select(
  SleepHrsNight,
  BMI,
  DirectChol,
  Age,
  Gender,
  Race1,
  TotChol,
  BPDiaAve,
  BPSysAve,
  AlcoholYear,
  Poverty,
  SexNumPartnLife,
  SexNumPartYear,
  DaysMentHlthBad,
  UrineFlow1,
  PhysActive,
  DaysPhysHlthBad,
  Smoke100,
  Depressed,
  HealthGen,
  SexAge
)

df3 <- na.omit(df2)
#df3$SleepHrsNight <- df3$SleepHrsNight * 60
#df3 <- df3[, -which(names(df3) %in% "SleepHrsNight")]
# cor(df3$BPSysAve,df3$BPDiaAve)
psych::describe(df3)
# psych::pairs.panels(df3)
hist(df3$SleepHrsNight)
# colSums(is.na(df2)) / nrow(df2)
fit0 <-
  lm(SleepHrsNight ~ .,
     data = df3)
#data type
df3$Gender <- ifelse(df3$Gender == "male", 0, 1)
df3$Smoke100 <- ifelse(df3$Smoke100 == "No", 0, 1)
df3$PhysActive <- ifelse(df3$PhysActive == "No", 0, 1)
df3 <- df3 %>%
  mutate(
    Race1 = case_when(
      Race1 == 'Black' ~ 1,
      Race1 == 'Hispanic' ~ 2,
      Race1 == 'Mexican' ~ 3,
      Race1 == 'White' ~ 4,
      Race1 == 'Other' ~ 5,
      TRUE ~ NA_integer_  # Default value if none of the conditions are met
    )
  )

## model_2 add known risk factors ##
m_2 = lm(
  BMI ~ SleepHrsNight + Age + Gender + factor(Race1) + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 +
    DaysPhysHlthBad + PhysActive,
  df3
)
summary(m_2)
car::Anova(m_2, type = "III")

########### model 2 diagnosis ###########
par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_2, which = 1)
plot(m_2, which = 2)
plot(m_2, which = 3)
plot(m_2, which = 4)
plot(m_2, which = 5)
plot(m_2, which = 6)
par(mfrow = c(1, 1)) # reset

m_2.yhat = m_2$fitted.values
m_2.res = m_2$residuals
m_2.h = hatvalues(m_2)
m_2.r = rstandard(m_2)
m_2.rr = rstudent(m_2)
#which subject is most outlying with respect to the x space
Hmisc::describe(m_2.h)
m_2.h[which.max(m_2.h)]


###################### Assumption:LINE ##############################

#(1)Linear: 2 approaches

# partial regression plots
car::avPlots(m_2)


#(2)Independence:

residuals <- resid(m_2)
acf(residuals, main = "Autocorrelation Function of Residuals")
pacf(residuals, main = "Partial Autocorrelation Function of Residuals")

#(3)E: constant var: residuals-fitted values; transform for variance-stable...(total: 4 solutions)
library(ggplot2)
car::residualPlots(m_2, type = "response")
plot(m_2, which = 1)
#or
ggplot(m_2, aes(x = m_2.yhat, y = m_2.res)) +
  geom_point(color = "blue", alpha = 0.8) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "black") +
  labs(title = "constant variance assumption",
       x = "y hat",
       y = "Residuals") +
  theme_minimal()
#conclusion: the constant variance assumption is basically not violated. The spread of the residuals appears to be fairly uniform across the range of predicted values, the assumption is more likely to hold


#(4)Normality: residuals freq - residuals (4 plots: his, box, Q-Q, shapiro); transform

#exam quartiles of the residuals
Hmisc::describe(m_2.res)
Hmisc::describe(m_2.res)$counts[c(".25", ".50", ".75")] #not symmetric
#histogram
par(mfrow = c(1, 1))
hist(m_2.res, breaks = 15)
# Q-Q plot
qq.m_2.res = car::qqPlot(m_2.res)
m_2.res[qq.m_2.res]

############### influential observations  #################

influence2 = data.frame(
  Residual = resid(m_2),
  Rstudent = rstudent(m_2),
  HatDiagH = hat(model.matrix(m_2)),
  CovRatio = covratio(m_2),
  DFFITS = dffits(m_2),
  COOKsDistance = cooks.distance(m_2)
)
# DFFITS
ols_plot_dffits(m_2)
influence2[order(abs(influence2$DFFITS), decreasing = T), ] %>% head()
#From the plot above, we can see 2 observations with the largest (magnitude) of DFFITS, observation 879 and 1769 By printing the corresponding values of DFFITS in the output dataset, we can obtain their DFFITS values: 0.5673 for observation 879, 0.5872 for observation 1769

# Cook's D
ols_plot_cooksd_bar(m_2)
influence2[order(influence2$COOKsDistance, decreasing = T), ] %>% head()
#From the plot above, we can see that the observation 879 and 1769 also have the largest Cook's Distance. By printing the corresponding values of Cook's D in the output dataset, we can obtain their Cook's D values:0.0108 for observation 879, 0.0145 for observation 1769

#leverage
ols_plot_resid_lev(m_2)
#high leverage
influence2[order(influence2$HatDiagH, decreasing = T), ] %>% head()
#high studentized residual
influence2[order(influence2$Rstudent, decreasing = T), ] %>% head()
#From the plot above, we can see that the observation 1155 has the largest leverage (0.0368). Observations 1862 has the largest (in magnitude) externally studentized residual (5.9649).


#From the plot above, there is 7 observations(1048,1769,1684, 74, 72, 1689, 1311) located in the intersection areas of both outlier and leverage, which is to say, those observations has both the leverage and the externally studentized residual exceeding their respective thresholds.Due to its large DIFFITS and Cook's D, they are potentially influential observations.
#The thresholds for the externally studentized residual are -2 and 2, i.e. 2 in magnitude. The thresholds for the leverage of the R default is 0.011

#From (DFFITS), observations 879 and 1769 appear to be influential observations. Observation 1155 has extraordinarily large leverage. Therefore, I choose to remove these 14 observations in the re-fitted mode

rm2.df3 = df3[-c(879, 1769, 1155, 1048, 1769, 1684, 74, 72, 1689, 1311), ]
rm.m_2 =  lm(
  BMI ~ SleepHrsNight + Age + Gender + Race1 + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 +
    DaysPhysHlthBad + PhysActive,
  rm2.df3
)
## Before removing these observations, the estimated coefficients are:
summary(m_2)$coef
## After removing these observations, the estimated coefficients are:
summary(rm.m_2)$coef
#### change percent
abs((rm.m_2$coefficients - m_2$coefficients) / (m_2$coefficients) * 100)

#The estimated regression coefficients doesn't change slightly after removing these observations. 5 of the estimates have changed by more than 10% after calculation. The p-value for the coefficient forSleepHrsNight    is becoming insignificant with 95% confidence level.

##################   multicollinearity   ######################
#Pearson correlations
var2 = c(
  "BMI",
  "SleepHrsNight",
  "Age",
  "Gender",
  "Race1",
  "TotChol",
  "BPDiaAve",
  "BPSysAve",
  "AlcoholYear",
  "Smoke100",
  "DaysPhysHlthBad",
  "PhysActive"
)
newData2 = df3[, var2]
library("corrplot")
par(mfrow = c(1, 2))
cormat2 = cor(as.matrix(newData2[, -c(1)], method = "pearson"))
p.mat2 = cor.mtest(as.matrix(newData2[, -c(1)]))$p
corrplot(
  cormat2,
  method = "color",
  type = "upper",
  number.cex = 1,
  diag = FALSE,
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90,
  p.mat = p.mat2,
  sig.level = 0.05,
  insig = "blank",
)

#None of the covariates seem strongly correlated.There is no evidence of collinearity from the pair-wise correlations.

# collinearity diagnostics (VIF)
car::vif(m_2)
#From the VIF values in the output above, once again we do not observe any potential collinearity issues. In fact, the VIF values are fairly small: none of the values exceed 10.

################ using log-transformed BMI  ##################
# log BMI
df3$logBMI = log(df3$BMI + 1)
m_2.log = lm(
  logBMI ~ SleepHrsNight + Age + Gender + Race1 + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 +
    DaysPhysHlthBad + PhysActive,
  df3
)
p21.log = ols_plot_resid_lev(m_2.log)
p22.log = ols_plot_cooksd_bar(m_2.log)
library(gridExtra)
p23.log = ggplot(m_2.log, aes(sample = rstudent(m_2.log))) + geom_qq() + stat_qq_line() +
  labs(title = "Q-Q plot")
p24.log = ggplot() + geom_point(aes(y = rstudent(m_2.log), x = m_2.log$fitted.values)) + labs(x = "Predicted Value", y = "Jacknife Residuals") +
  geom_hline(yintercept = c(-2, 2))
grid.arrange(p23.log, p24.log, nrow = 2)


p23 = ggplot(m_2, aes(sample = rstudent(m_2))) + geom_qq() + stat_qq_line() +
  labs(title = "Q-Q plot")
p24 = ggplot() + geom_point(aes(y = rstudent(m_2), x = m_2$fitted.values)) + labs(x = "Predicted Value", y = "Jacknife Residuals") +
  geom_hline(yintercept = c(-2, 2))
grid.arrange(p23, p24, nrow = 2)


m_2.3.yhat = m_2.log$fitted.values
m_2.3.res = m_2.log$residuals
m_2.3.h = hatvalues(m_2.log)
m_2.3.r = rstandard(m_2.log)
m_2.3.rr = rstudent(m_2.log)

par(mfrow = c(1, 1))
hist(m_2.3.res, breaks = 15)

car::avPlots(m_2.log)


#After looking at residuals from models using the log-transformed (natural log scale) BMI adjusted for other predictors, I agree that we should use log-transformed NIHScore because there is less of a discernible pattern in the residual plots. The residuals are also a lot less skewed once we log-transform this variable. Furthermore, there are fewer observations with extreme values on the QQ plot so the normality assumption appears to hold.

#collinearity diagnostics

car::vif(m_2.log)
#The VIF from both the models are the same. None of the VIF values are greater than 10. So there are no concerns about collinearity.
