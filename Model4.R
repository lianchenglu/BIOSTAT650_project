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
## model_4 add additional risk factors ##
m_full = lm(
  BMI ~ SleepHrsNight + Age + Gender + factor(Race1)  + Poverty + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 + UrineFlow1 + DaysMentHlthBad +
    DaysPhysHlthBad + factor(HealthGen) + PhysActive + SleepHrsNight*Age + SleepHrsNight*Gender,
  df3
)
summary(m_full)
car::Anova(m_full, type = "III")

########### model 4 diagnosis ###########
par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_full, which = 1)
plot(m_full, which = 2)
plot(m_full, which = 3)
plot(m_full, which = 4)
plot(m_full, which = 5)
plot(m_full, which = 6)
par(mfrow = c(1, 1)) # reset

m_full.yhat = m_full$fitted.values
m_full.res = m_full$residuals
m_full.h = hatvalues(m_full)
m_full.r = rstandard(m_full)
m_full.rr = rstudent(m_full)
#which subject is most outlying with respect to the x space
Hmisc::describe(m_full.h)
m_full.h[which.max(m_full.h)]


###################### Assumption:LINE ##############################

#(1)Linear: 2 approaches

# partial regression plots
car::avPlots(m_full)


#(2)Independence:

residuals <- resid(m_full)
acf(residuals, main = "Autocorrelation Function of Residuals")
pacf(residuals, main = "Partial Autocorrelation Function of Residuals")

#(3)E: constant var: residuals-fitted values; transform for variance-stable...(total: 4 solutions)

car::residualPlots(m_full, type = "response")
plot(m_full, which = 1)
#or
ggplot(m_full, aes(x = m_full.yhat, y = m_full.res)) +
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
Hmisc::describe(m_full.res)
Hmisc::describe(m_full.res)$counts[c(".25", ".50", ".75")] #not symmetric
#histogram
par(mfrow = c(1, 1))
hist(m_full.res, breaks = 15)
# Q-Q plot
qq.m_full.res = car::qqPlot(m_full.res)
m_full.res[qq.m_full.res]

############### influential observations  #################

influence4 = data.frame(
  Residual = resid(m_full),
  Rstudent = rstudent(m_full),
  HatDiagH = hat(model.matrix(m_full)),
  CovRatio = covratio(m_full),
  DFFITS = dffits(m_full),
  COOKsDistance = cooks.distance(m_full)
)
# DFFITS
ols_plot_dffits(m_full)
influence4[order(abs(influence4$DFFITS), decreasing = T), ] %>% head()
#From the plot above, we can see 2 observations with the largest (magnitude) of DFFITS, observation 879 and 1769 By printing the corresponding values of DFFITS in the output dataset, we can obtain their DFFITS values: 0.5673 for observation 879, 0.5872 for observation 1769

# Cook's D
ols_plot_cooksd_bar(m_full)
influence4[order(influence4$COOKsDistance, decreasing = T), ] %>% head()
#From the plot above, we can see that the observation 879 and 1769 also have the largest Cook's Distance. By printing the corresponding values of Cook's D in the output dataset, we can obtain their Cook's D values:0.0108 for observation 879, 0.0145 for observation 1769

#leverage
ols_plot_resid_lev(m_full)
#high leverage
influence4[order(influence4$HatDiagH, decreasing = T), ] %>% head()
#high studentized residual
influence4[order(influence4$Rstudent, decreasing = T), ] %>% head()
#From the plot above, we can see that the observation 1155 has the largest leverage (0.0368). Observations 1862 has the largest (in magnitude) externally studentized residual (5.9649).


#From the plot above, there is 7 observations(1048,1769,1684, 74, 72, 1689, 1311) located in the intersection areas of both outlier and leverage, which is to say, those observations has both the leverage and the externally studentized residual exceeding their respective thresholds.Due to its large DIFFITS and Cook's D, they are potentially influential observations.
#The thresholds for the externally studentized residual are -2 and 2, i.e. 2 in magnitude. The thresholds for the leverage of the R default is 0.011

#From (DFFITS), observations 879 and 1769 appear to be influential observations. Observation 1155 has extraordinarily large leverage. Therefore, I choose to remove these 14 observations in the re-fitted mode

rm4.df3 = df3[-c(879, 1769, 1155, 1048, 1769, 1684, 74, 72, 1689, 1311), ]
rm.m_full =  lm(
  BMI ~ SleepHrsNight + Age + Gender + factor(Race1)  + Poverty + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 + UrineFlow1 + DaysMentHlthBad +
    DaysPhysHlthBad + factor(HealthGen) + PhysActive + SleepHrsNight*Age + SleepHrsNight*Gender,
  rm4.df3
)
## Before removing these observations, the estimated coefficients are:
summary(m_full)$coef
## After removing these observations, the estimated coefficients are:
summary(rm.m_full)$coef
#### change percent
abs((rm.m_full$coefficients - m_full$coefficients) / (m_full$coefficients) * 100)

#The estimated regression coefficients doesn't change slightly after removing these observations. 5 of the estimates have changed by more than 10% after calculation. The p-value for the coefficient forSleepHrsNight    is becoming insignificant with 95% confidence level.

##################   multicollinearity   ######################
#Pearson correlations
var4 = c(
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
newData4 = df3[, var4]
library("corrplot")
par(mfrow = c(1, 2))
cormat4 = cor(as.matrix(newData4[, -c(1)], method = "pearson"))
p.mat4 = cor.mtest(as.matrix(newData4[, -c(1)]))$p
corrplot(
  cormat4,
  method = "color",
  type = "upper",
  number.cex = 1,
  diag = FALSE,
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90,
  p.mat = p.mat4,
  sig.level = 0.05,
  insig = "blank",
)

#None of the covariates seem strongly correlated.There is no evidence of collinearity from the pair-wise correlations.

# collinearity diagnostics (VIF)
car::vif(m_full)
#From the VIF values in the output above, once again we do not observe any potential collinearity issues. In fact, the VIF values are fairly small: none of the values exceed 10.

################ using log-transformed BMI  ##################
# log BMI
df3$logBMI = log(df3$BMI + 1)
m_full.log = lm(
  logBMI ~ SleepHrsNight + Age + Gender + factor(Race1)  + Poverty + TotChol + BPDiaAve + BPSysAve + AlcoholYear + Smoke100 + UrineFlow1 + DaysMentHlthBad + DaysPhysHlthBad + factor(HealthGen) + PhysActive +
    SleepHrsNight*Age+SleepHrsNight*Gender,
  df3
)
p41.log = ols_plot_resid_lev(m_full.log)
p42.log = ols_plot_cooksd_bar(m_full.log)
library(gridExtra)
p43.log = ggplot(m_full.log, aes(sample = rstudent(m_full.log))) + geom_qq() + stat_qq_line() +
  labs(title = "Q-Q plot")
p44.log = ggplot() + geom_point(aes(y = rstudent(m_full.log), x = m_full.log$fitted.values)) + labs(x = "Predicted Value", y = "Jacknife Residuals") +
  geom_hline(yintercept = c(-2, 2))
grid.arrange(p43.log, p44.log, nrow = 2)


p43 = ggplot(m_full, aes(sample = rstudent(m_full))) + geom_qq() + stat_qq_line() +
  labs(title = "Q-Q plot")
p44 = ggplot() + geom_point(aes(y = rstudent(m_full), x = m_full$fitted.values)) + labs(x = "Predicted Value", y = "Jacknife Residuals") +
  geom_hline(yintercept = c(-2, 2))
grid.arrange(p43, p44, nrow = 2)


m_full.3.yhat = m_full.log$fitted.values
m_full.3.res = m_full.log$residuals
m_full.3.h = hatvalues(m_full.log)
m_full.3.r = rstandard(m_full.log)
m_full.3.rr = rstudent(m_full.log)

par(mfrow = c(1, 1))
hist(m_full.3.res, breaks = 15)

car::avPlots(m_full.log)


#After looking at residuals from models using the log-transformed (natural log scale) BMI adjusted for other predictors, I agree that we should use log-transformed NIHScore because there is less of a discernible pattern in the residual plots. The residuals are also a lot less skewed once we log-transform this variable. Furthermore, there are fewer observations with extreme values on the QQ plot so the normality assumption appears to hold.

#collinearity diagnostics

car::vif(m_full.log)
#The VIF from both the models are the same. None of the VIF values are greater than 10. So there are no concerns about collinearity.


getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

new_data <- expand.grid(SleepHrsNight = seq(min(df3$SleepHrsNight), max(df3$SleepHrsNight), length.out = 100),
                        Age = quantile(df3$Age, probs = c(0.25, 0.5, 0.75)),
                        Gender = median(df3$Gender, na.rm = TRUE),
                        Race1 = median(df3$Race1, na.rm = TRUE),
                        Poverty = median(df3$Poverty, na.rm = TRUE),
                        TotChol = median(df3$TotChol, na.rm = TRUE),
                        BPDiaAve = median(df3$BPDiaAve, na.rm = TRUE),
                        BPSysAve = median(df3$BPSysAve, na.rm = TRUE),
                        AlcoholYear = median(df3$AlcoholYear, na.rm = TRUE),
                        Smoke100 = getMode(df3$Smoke100),
                        UrineFlow1 = median(df3$UrineFlow1, na.rm = TRUE),
                        DaysMentHlthBad = median(df3$DaysMentHlthBad, na.rm = TRUE),
                        DaysPhysHlthBad = median(df3$DaysPhysHlthBad, na.rm = TRUE),
                        HealthGen = getMode(df3$HealthGen),
                        PhysActive = getMode(df3$PhysActive)
)

# predict
new_data$predicted_BMI <- predict(m_full, newdata = new_data)
# interaction
library(ggplot2)
ggplot(new_data, aes(x = SleepHrsNight, y = predicted_BMI, group = factor(Age))) +
  geom_line(aes(color = factor(Age))) +
  labs(title = "Interaction between Sleep Hours and Age on BMI",
       x = "Sleep Hours per Night",
       y = "Predicted BMI")

# cross validation
library(caret)
splitIndex <-
  createDataPartition(df3$SleepHrsNight, p = 0.7, list = FALSE)
trainData <- df3[splitIndex, ]
testData <- df3[-splitIndex, ]
predictions <- predict(m_full, newdata = testData)
mse <- mean((testData$SleepHrsNight - predictions) ^ 2)
control <-
  trainControl(method = "cv", number = 10)  # 10-fold cross-validation
cv_model <-
  train(
    SleepHrsNight ~ .,
    data = df3,
    method = "lm",
    trControl = control
  )
cv_model
(cv_results <- cv_model$results)
