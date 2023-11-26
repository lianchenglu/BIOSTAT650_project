rm(list = ls())
gc()
set.seed(123)
library(car)
library(olsrr)
library(ggplot2)
library(lmtest)
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

df3 <- df3 %>%
  mutate(
    HealthGen = case_when(
      HealthGen == 'Poor' ~ 1,
      HealthGen == 'Fair' ~ 2,
      HealthGen == 'Good' ~ 3,
      HealthGen == 'Vgood' ~ 4,
      HealthGen == 'Excellent' ~ 5,
      TRUE ~ NA_integer_  # Default value if none of the conditions are met
    )
  )
## model_3 add additional risk factors ##
m_3 = lm(
  BMI ~ SleepHrsNight + Age + Gender + factor(Race1)  + Poverty + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 + UrineFlow1 + DaysMentHlthBad +
    DaysPhysHlthBad + factor(HealthGen) + PhysActive,
  df3
)
summary(m_3)
car::Anova(m_3, type = "III")

########### model 3 diagnosis ###########
par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_3, which = 1)
plot(m_3, which = 2)
plot(m_3, which = 3)
plot(m_3, which = 4)
plot(m_3, which = 5)
plot(m_3, which = 6)
par(mfrow = c(1, 1)) # reset

m_3.yhat = m_3$fitted.values
m_3.res = m_3$residuals
m_3.h = hatvalues(m_3)
m_3.r = rstandard(m_3)
m_3.rr = rstudent(m_3)
#which subject is most outlying with respect to the x space
Hmisc::describe(m_3.h)
m_3.h[which.max(m_3.h)]


###################### Assumption:LINE ##############################

#(1)Linear: 2 approaches

# partial regression plots
car::avPlots(m_3)

#(2)Independence:

residuals <- resid(m_3)
acf(residuals, main = "Autocorrelation Function of Residuals")
pacf(residuals, main = "Partial Autocorrelation Function of Residuals")

dw_test <- dwtest(m_3)
print(dw_test)

#(3)E: constant var: residuals-fitted values; transform for variance-stable...(total: 4 solutions)

car::residualPlots(m_3, type = "response")
plot(m_3, which = 1)
#or
ggplot(m_3, aes(x = m_3.yhat, y = m_3.res)) +
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
Hmisc::describe(m_3.res)
Hmisc::describe(m_3.res)$counts[c(".25", ".50", ".75")] #not symmetric
#histogram
par(mfrow = c(1, 1))
hist(m_3.res, breaks = 15)
# Q-Q plot
qq.m_3.res = car::qqPlot(m_3.res)
m_3.res[qq.m_3.res]

############### influential observations  #################

influence3 = data.frame(
  Residual = resid(m_3),
  Rstudent = rstudent(m_3),
  HatDiagH = hat(model.matrix(m_3)),
  CovRatio = covratio(m_3),
  DFFITS = dffits(m_3),
  COOKsDistance = cooks.distance(m_3)
)
# DFFITS
ols_plot_dffits(m_3)
influence3[order(abs(influence3$DFFITS), decreasing = T), ] %>% head()
#From the plot above, we can see 2 observations with the largest (magnitude) of DFFITS, observation 879 and 1769 By printing the corresponding values of DFFITS in the output dataset, we can obtain their DFFITS values: 0.5673 for observation 879, 0.5872 for observation 1769

# Cook's D
ols_plot_cooksd_bar(m_3)
influence3[order(influence3$COOKsDistance, decreasing = T), ] %>% head()
#From the plot above, we can see that the observation 879 and 1769 also have the largest Cook’s Distance. By printing the corresponding values of Cook’s D in the output dataset, we can obtain their Cook’s D values:0.0108 for observation 879, 0.0145 for observation 1769

#leverage
ols_plot_resid_lev(m_3)
#high leverage
influence3[order(influence3$HatDiagH, decreasing = T), ] %>% head()
#high studentized residual
influence3[order(influence3$Rstudent, decreasing = T), ] %>% head()
#From the plot above, we can see that the observation 1155 has the largest leverage (0.0368). Observations 1862 has the largest (in magnitude) externally studentized residual (5.9649).


#From the plot above, there is 7 observations(1048,1769,1684, 74, 72, 1689, 1311) located in the intersection areas of both outlier and leverage, which is to say, those observations has both the leverage and the externally studentized residual exceeding their respective thresholds.Due to its large DIFFITS and Cook’s D, they are potentially influential observations.
#The thresholds for the externally studentized residual are -2 and 2, i.e. 2 in magnitude. The thresholds for the leverage of the R default is 0.011

#From (DFFITS), observations 879 and 1769 appear to be influential observations. Observation 1155 has extraordinarily large leverage. Therefore, I choose to remove these 14 observations in the re-fitted mode

rm3.df3 = df3[-c(879, 1769, 1155, 1048, 1769, 1684, 74, 72, 1689, 1311), ]
rm.m_3 =  lm(
  BMI ~ SleepHrsNight + Age + Gender + Race1  + Poverty + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 + UrineFlow1 + DaysMentHlthBad +
    DaysPhysHlthBad + HealthGen + PhysActive,
  rm3.df3
)
## Before removing these observations, the estimated coefficients are:
summary(m_3)$coef
## After removing these observations, the estimated coefficients are:
summary(rm.m_3)$coef
#### change percent
abs((rm.m_3$coefficients - m_3$coefficients) / (m_3$coefficients) * 100)

#The estimated regression coefficients doesn't change slightly after removing these observations. 5 of the estimates have changed by more than 10% after calculation. The p-value for the coefficient forSleepHrsNight    is becoming insignificant with 95% confidence level.

##################   multicollinearity   ######################
#Pearson correlations
var3 = c(
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
  "PhysActive",
  "Poverty",
  "UrineFlow1",
  "DaysMentHlthBad",
  "HealthGen"
)

newData3 = df3[, var3]
library("corrplot")
par(mfrow = c(1, 2))
cormat3 = cor(as.matrix(newData3[, -c(1)], method = "pearson"))
p.mat3 = cor.mtest(as.matrix(newData3[, -c(1)]))$p
corrplot(
  cormat3,
  method = "color",
  type = "upper",
  number.cex = 1,
  diag = FALSE,
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90,
  p.mat = p.mat3,
  sig.level = 0.05,
  insig = "blank",
)

#None of the covariates seem strongly correlated.There is no evidence of collinearity from the pair-wise correlations.

# collinearity diagnostics (VIF)
car::vif(m_3)
#From the VIF values in the output above, once again we do not observe any potential collinearity issues. In fact, the VIF values are fairly small: none of the values exceed 10.

################ using log-transformed BMI  ##################
# log BMI
df3$logBMI = log(df3$BMI + 1)
m_3.log = lm(
  logBMI ~ SleepHrsNight + Age + Gender + factor(Race1)  + Poverty + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 + UrineFlow1 + DaysMentHlthBad + DaysPhysHlthBad + factor(HealthGen) + PhysActive,
  df3
)
p31.log = ols_plot_resid_lev(m_3.log)
p32.log = ols_plot_cooksd_bar(m_3.log)

library(gridExtra)
p33.log = ggplot(m_3.log, aes(sample = rstudent(m_3.log))) + geom_qq() + stat_qq_line() +
  labs(title = "Q-Q plot")
p34.log = ggplot() + geom_point(aes(y = rstudent(m_3.log), x = m_3.log$fitted.values)) + labs(x = "Predicted Value", y = "Jacknife Residuals") +
  geom_hline(yintercept = c(-2, 2))
grid.arrange(p33.log, p34.log, nrow = 2)


p33 = ggplot(m_3, aes(sample = rstudent(m_3))) + geom_qq() + stat_qq_line() +
  labs(title = "Q-Q plot")
p34 = ggplot() + geom_point(aes(y = rstudent(m_3), x = m_3$fitted.values)) + labs(x = "Predicted Value", y = "Jacknife Residuals") +
  geom_hline(yintercept = c(-2, 2))
grid.arrange(p33, p34, nrow = 2)


m_3.3.yhat = m_3.log$fitted.values
m_3.3.res = m_3.log$residuals
m_3.3.h = hatvalues(m_3.log)
m_3.3.r = rstandard(m_3.log)
m_3.3.rr = rstudent(m_3.log)

par(mfrow = c(1, 1))
hist(m_3.3.res, breaks = 15)

car::avPlots(m_3.log)


#After looking at residuals from models using the log-transformed (natural log scale) BMI adjusted for other predictors, I agree that we should use log-transformed NIHScore because there is less of a discernible pattern in the residual plots. The residuals are also a lot less skewed once we log-transform this variable. Furthermore, there are fewer observations with extreme values on the QQ plot so the normality assumption appears to hold.

#collinearity diagnostics

car::vif(m_3.log)
#The VIF from both the models are the same. None of the VIF values are greater than 10. So there are no concerns about collinearity.
