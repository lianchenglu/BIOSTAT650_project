rm(list = ls())
gc()
set.seed(123)
library(car)
library(ggplot2)
############### (1) Data cleaning ########################################
## select variables
library(NHANES)
df0 <- NHANES
df <- NHANES[NHANES$Age >= 18 & NHANES$Age < 60, ]
# colSums(is.na(df)) / nrow(df)
df <- df[, which(colSums(is.na(df)) / nrow(df) < 0.3)]
# exclude duplication
df <- df[!duplicated(df), ]
names(df)
# df$BPSysAve
library(dplyr)

df2 <- df %>% select(
  SleepHrsNight,
  BMI,
  Age,
  Gender,
  Race1,
  TotChol,
  BPDiaAve,
  BPSysAve,
  AlcoholYear,
  Poverty,
  DaysMentHlthBad,
  UrineFlow1,
  PhysActive,
  DaysPhysHlthBad,
  Smoke100,
  HealthGen
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

df3$logBMI = log(df3$BMI+1)



### multiple linear regression###
# model_1 add demographic
m_1.log= lm(logBMI ~ SleepHrsNight + Age + Gender + factor(Race1), df3)
summary(m_1.log)



car::Anova(m_1.log,type="III")



########### model 1.log diagnosis ###########

par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_1.log, which = 1)
plot(m_1.log, which = 2)
plot(m_1.log, which = 3)
plot(m_1.log, which = 4)
plot(m_1.log, which = 5)
plot(m_1.log, which = 6)
par(mfrow = c(1, 1)) # reset



m_1.log.yhat=m_1.log$fitted.values
m_1.log.res=m_1.log$residuals
m_1.log.h=hatvalues(m_1.log)
m_1.log.r=rstandard(m_1.log)
m_1.log.rr=rstudent(m_1.log)
#which subject is most outlying with respect to the x space
Hmisc::describe(m_1.log.h)
m_1.log.h[which.max(m_1.log.h)]


length(df3$Age)
length(df3$logBMI)
length(m_1.log.yhat)# why the length of yhat is diff with y

###################### Assumption:LINE ##############################

#(1)Linear: 2 approaches

# partial regression plots
car::avPlots(m_1.log)



#categoraize age ---beta plot
df3 <- df3 %>%
  mutate(Age_Group = cut(Age, breaks = c(18, 29, 39, 49, 59), labels = c("18-29", "30-39", "40-49", "50-59")))

summary_stats <- df3 %>%
  group_by(Age_Group) %>%
  summarise(Median_Age = median(Age), Beta_Coefficient = coef(m_1.log)['Age'])

ggplot(summary_stats, aes(x = Median_Age, y = Beta_Coefficient, group = Age_Group, color = Age_Group)) +
  geom_line() +
  geom_point() +
  labs(title = "Median Age vs. Beta Coefficient by Age Group",
       x = "Median Age",
       y = "Beta Coefficient")


#(2)Independence:

residuals <- resid(m_1.log)
acf(residuals, main = "Autocorrelation Function of Residuals")
pacf(residuals, main = "Partial Autocorrelation Function of Residuals")

# Assuming m_1.log is your linear regression model
# Assuming df3 is your data frame

library(lmtest)

# Perform Durbin-Watson test
dw_test_result <- dwtest(m_1.log, alternative = "two.sided")

# Print the Durbin-Watson test result
print(dw_test_result)




#(3)E: constant var: residuals-fitted values; transform for variance-stable...(total: 4 solutions)

car::residualPlots(m_1.log,type="response")
plot(m_1.log, which = 1)
#or
ggplot(m_1.log, aes(x = m_1.log.yhat, y = m_1.log.res)) +
  geom_point(color = "blue", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "constant variance assumption",
       x = "y hat",
       y = "Residuals") +
  theme_minimal()
#conclusion: the constant variance assumption is basically not violated. The spread of the residuals appears to be fairly uniform across the range of predicted values, the assumption is more likely to hold




#(4)Normality: residuals freq - residuals (4 plots: his, box, Q-Q, shapiro); transform

#exam quartiles of the residuals
Hmisc::describe(m_1.log.res)
Hmisc::describe(m_1.log.res)$counts[c(".25",".50",".75")] #not symmetric
#histogram
par(mfrow = c(1, 1))
hist(m_1.log.res,breaks = 15)
# Q-Q plot
qq.m_1.log.res=car::qqPlot(m_1.log.res)
m_1.log.res[qq.m_1.log.res]

############### influential observations  #################

influence = data.frame(Residual = resid(m_1.log), Rstudent = rstudent(m_1.log),
                       HatDiagH = hat(model.matrix(m_1.log)),
                       CovRatio = covratio(m_1.log), DFFITS = dffits(m_1.log),
                       COOKsDistance = cooks.distance(m_1.log))
# DFFITS
library(olsrr)
ols_plot_dffits(m_1.log)
influence[order(abs(influence$DFFITS),decreasing = T),] %>% head()
#From the plot above, we can see 2 observations with the largest (magnitude) of DFFITS, observation 879 and 1862 By printing the corresponding values of DFFITS in the output dataset, we can obtain their DFFITS values: 0.4147 for observation 879, 0.4028 for observation 1862.

# Cook's D
ols_plot_cooksd_bar(m_1.log)
influence[order(influence$COOKsDistance,decreasing = T),] %>% head()
#From the plot above, we can see that the observation 879 and 1862 also have the largest Cook’s Distance. By printing the corresponding values of Cook’s D in the output dataset, we can obtain their Cook’s D values:0.0213 for observation 879, 0.0200 for observation 1862.

#leverage
ols_plot_resid_lev(m_1.log)
#high leverage
influence[order(influence$HatDiagH,decreasing = T),] %>% head()
#high studentized residual
influence[order(influence$Rstudent,decreasing = T),] %>% head()
#From the plot above, we can see that the observation 325 has the largest leverage (0.0121). Observations 895 has the largest (in magnitude) externally studentized residual (6.1262).



#From the plot above, there is 11 observations(1809,745,496, 1876, 91, 1201, 1930, 1362, 1627, 1583,1400) located in the intersection areas of both outlier and leverage, which is to say, those observations has both the leverage and the externally studentized residual exceeding their respective thresholds.Due to its large DIFFITS and Cook’s D, they are potentially influential observations.
#The thresholds for the externally studentized residual are -2 and 2, i.e. 2 in magnitude. The thresholds for the leverage of the R default is 0.007

#From (DFFITS), observations 879 and 1862 appear to be influential observations. Observation 325 has extraordinarily large leverage. Therefore, I choose to remove these 14 observations in the re-fitted mode

rm.df3 = df3[-c(879,1862,325,1809,745,496, 1876, 91, 1201, 1930, 1362, 1627, 1583,1400),]
rm.m_1.log = lm(logBMI ~ SleepHrsNight + Age + Gender + factor(Race1), rm.df3)
## Before removing these observations, the estimated coefficients are:
summary(m_1.log)$coef
## After removing these observations, the estimated coefficients are:
summary(rm.m_1.log)$coef
#### change percent
abs((rm.m_1.log$coefficients - m_1.log$coefficients)/(m_1.log$coefficients) *100)

#The estimated regression coefficients doesn't change slightly after removing these observations. 5 of the estimates have changed by more than 10% after calculation. The p-value for the coefficient for gender and race 1_mexican are still insignificant with 95% confidence level.



##################   multicollinearity   ######################
#Pearson correlations
var= c("logBMI","SleepHrsNight","Age","Gender","Race1")
newData = df3[,var]
library("corrplot")
par(mfrow = c(1, 2))
cormat = cor(as.matrix(newData[,-c(1)], method = "pearson"))
p.mat = cor.mtest(as.matrix(newData[,-c(1)]))$p
corrplot(cormat,
         method = "color",
         type = "upper",
         number.cex = 1,
         diag = FALSE,
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 90,
         p.mat = p.mat,
         sig.level = 0.05,
         insig = "blank",
)

#None of the covariates seem strongly correlated.There is no evidence of collinearity from the pair-wise correlations.

# collinearity diagnostics (VIF)
car::vif(m_1.log)
#From the VIF values in the output above, once again we do not observe any potential collinearity issues. In fact, the VIF values are fairly small: none of the values exceed 10.
