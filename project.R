rm(list = ls())
gc()
set.seed(123)
############### (1) Data cleaning ########################################
library(NHANES)
df <- NHANES[NHANES$Age >= 18 & NHANES$Age < 60, ]
# colSums(is.na(df)) / nrow(df)
df <- df[, which(colSums(is.na(df)) / nrow(df) < 0.3)]
# colSums(is.na(df)) / nrow(df)
# df$BPSysAve
library(dplyr)

df2 <- df %>% select(
  SleepHrsNight,
  TotChol,
  DirectChol,
  Age,
  Gender,
  Race1,
  BMI,
  BPDiaAve,
  BPSysAve,
  AlcoholYear,
  Poverty,
  HomeRooms,
  SexNumPartnLife,
  SexNumPartYear,
  DaysMentHlthBad
)

Hmisc::describe(df2)
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

############### (2) Baseline characteristics ########################################

############### (3) linear regression model ########################################
##simple linear regression##
model1 = lm(df3$SleepHrsNight ~ df3$TotChol, data = df3)
summary(model1)

par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(model1, which = 1)
plot(model1, which = 2)
plot(model1, which = 3)
plot(model1, which = 4)
plot(model1, which = 5)
plot(model1, which = 6)
par(mfrow = c(1, 1)) # reset

## multiple linear regression##
m_initial = lm(SleepHrsNight ~ TotChol + Age + Gender + factor(Race1), df3)
summary(m_initial)
m_knrisk = lm(
  SleepHrsNight ~ TotChol + Age + Gender + factor(Race1) + BMI + BPDiaAve +
    BPSysAve + AlcoholYear + DaysMentHlthBad,
  df3
)
summary(m_knrisk)
m_full = lm(
  SleepHrsNight ~ TotChol + Age + Gender + factor(Race1) + BMI + BPDiaAve +
    BPSysAve + AlcoholYear + DaysMentHlthBad + HomeRooms + SexNumPartnLife +
    SexNumPartYear + Poverty,
  df3
)
summary(m_full)

par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_full, which = 1)
plot(m_full, which = 2)
plot(m_full, which = 3)
plot(m_full, which = 4)
plot(m_full, which = 5)
plot(m_full, which = 6)
par(mfrow = c(1, 1)) # reset

plot(
  df3$TotChol,
  df3$SleepHrsNight,
  main = "Scatter Plot with Linear Regression Line",
  xlab = "X-axis",
  ylab = "Y-axis"
)
#log outcome
df3$logSleepHrsNight = log(df3$SleepHrsNight + 1)
m_logfull_1 = lm(
  logSleepHrsNight ~ TotChol + Age + Gender + factor(Race1) + BMI + BPDiaAve +
    BPSysAve + AlcoholYear + DaysMentHlthBad + HomeRooms + SexNumPartnLife +
    SexNumPartYear + Poverty,
  df3
)
summary(m_logfull_1)
par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_logfull_1, which = 1)
plot(m_logfull_1, which = 2)
plot(m_logfull_1, which = 3)
plot(m_logfull_1, which = 4)
plot(m_logfull_1, which = 5)
plot(m_logfull_1, which = 6)
par(mfrow = c(1, 1)) # reset

#log x
df3$logTotChol = log(df3$TotChol + 1)
m_logfull_2 = lm(
  SleepHrsNight ~ logTotChol + Age + Gender + factor(Race1) + BMI + BPDiaAve +
    BPSysAve + AlcoholYear + DaysMentHlthBad + HomeRooms + SexNumPartnLife +
    SexNumPartYear + Poverty,
  df3
)
summary(m_logfull_2)

par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_logfull_2, which = 1)
plot(m_logfull_2, which = 2)
plot(m_logfull_2, which = 3)
plot(m_logfull_2, which = 4)
plot(m_logfull_2, which = 5)
plot(m_logfull_2, which = 6)
par(mfrow = c(1, 1)) # reset

# x^2
df3$sqTotChol = (df3$TotChol - mean(df3$TotChol)) ^ 2
m_sqfull_1 = lm(
  SleepHrsNight ~ TotChol + sqTotChol + Age + Gender + factor(Race1) + BMI +
    BPDiaAve + BPSysAve + AlcoholYear + DaysMentHlthBad + HomeRooms + SexNumPartnLife +
    SexNumPartYear + Poverty,
  df3
)
summary(m_sqfull_1)

par(mfrow = c(2, 3)) #read more from ?plot.lm
plot(m_sqfull_1, which = 1)
plot(m_sqfull_1, which = 2)
plot(m_sqfull_1, which = 3)
plot(m_sqfull_1, which = 4)
plot(m_sqfull_1, which = 5)
plot(m_sqfull_1, which = 6)
par(mfrow = c(1, 1)) # reset

############### (4) Diagnosis: 10-fold CV ########################################

library(caret)
splitIndex <-
  createDataPartition(df3$SleepHrsNight, p = 0.7, list = FALSE)
trainData <- df3[splitIndex,]
testData <- df3[-splitIndex,]
predictions <- predict(m_sqfull_1, newdata = testData)
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

############### (4) Diagnosis: Normality Assumption ########################################

library(ggplot2)
library(patchwork)
# Initializes an empty patchwork object
plot_list <- list()

# Draw a histogram for each numeric variable (except Race1 and Gender) and add it to the list
for (var in names(df3)) {
  if (is.numeric(df3[[var]]) && !(var %in% c("Race1", "Gender"))) {
    p <- ggplot(df3, aes(x = .data[[var]])) +
      geom_histogram(
        aes(y = after_stat(density)),
        binwidth = 1,
        fill = "blue",
        color = "black"
      ) +
      geom_density(col = "red") +
      ggtitle(paste("Histogram and Density for", var)) +
      xlab(var) +
      ylab("Density")
    plot_list[[length(plot_list) + 1]] <- p
  }
}

# Use patchwork to put all the charts together
combined_plot <- wrap_plots(plot_list, ncol = 2)
print(combined_plot)

df3 <- data.frame(df3)
library(dplyr)
# Shapiro-Wilk normality test is performed for each numerical variable in df3
results <- sapply(df3, function(x) {
  if (is.numeric(x)) {
    shapiro_test <- shapiro.test(x)
    return(c(shapiro_test$statistic, shapiro_test$p.value))
  } else {
    return(c(NA, NA))
  }
})
# Convert the result to a data box and name the column
results_df <- as.data.frame(t(results))
names(results_df) <- c("W", "p.value")
# Add a variable name as a new column
results_df$Variable <- rownames(results_df)
# Rearrange the order of columns
results_df <- results_df[, c("Variable", "W", "p.value")]
# Calculate the corrected P-value (for example, using Bonferroni correction)
results_df$p.adjusted <-
  p.adjust(results_df$p.value, method = "bonferroni")
print(results_df)


# Regular residuals
residual_1 <- fit0$residuals

# Standardized residuals
residual_2 <- rstandard(fit0)

# Studentized residuals
residual_3 <- rstudent(fit0)

# Externally studentized residuals
# Note: Externally studentized residuals are the same as studentized residuals in most cases
residual_4 <- rstudent(fit0)

# Creating a data frame to summarize these residuals
residual_summary <- data.frame(
  Residuals = c("Regular", "Standardized", "Studentized", "Externally Studentized"),
  Mean = c(mean(residual_1), mean(residual_2), mean(residual_3), mean(residual_4)),
  SD = c(sd(residual_1), sd(residual_2), sd(residual_3), sd(residual_4)),
  Min = c(min(residual_1), min(residual_2), min(residual_3), min(residual_4)),
  Max = c(max(residual_1), max(residual_2), max(residual_3), max(residual_4))
)

# Display the summary
print(residual_summary)


# Load necessary library
library(ggplot2)

# Assuming fit0 is your linear model
# fit0 <- lm(SleepMinNight ~ ., data = df3)

# Calculate standardized and studentized residuals
residual_2 <- rstandard(fit0)
residual_3 <- rstudent(fit0)

# Calculate leverage values
leverage_values <- hatvalues(fit0)

# Create a data frame for plotting
plot_data <- data.frame(
  Standardized_Residuals = residual_2,
  Difference = residual_3 - residual_2,
  Leverage = leverage_values
)

# Create the plot
ggplot(plot_data, aes(x = Standardized_Residuals, y = Difference)) +
  geom_point(aes(size = Leverage)) +
  ggtitle("Difference between Studentized and Standardized Residuals vs. Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Difference between Studentized and Standardized Residuals")

# Display the plot
print(ggplot)


# Load necessary library
library(ggplot2)

# Assuming fit0 is your linear model
# fit0 <- lm(SleepMinNight ~ ., data = df3)

# Calculate studentized and externally studentized residuals
residual_3 <- rstudent(fit0)
residual_4 <- rstudent(fit0)  # Externally studentized residuals are typically the same as studentized residuals

# Regular residuals
residual_1 <- fit0$residuals

# Create a data frame for plotting
plot_data <- data.frame(
  Studentized_Residuals = residual_3,
  Difference = residual_4 - residual_3,
  Residual_Squared = residual_1^2
)

# Create the plot
ggplot(plot_data, aes(x = Studentized_Residuals, y = Difference)) +
  geom_point(aes(size = Residual_Squared)) +
  ggtitle("Difference between Externally Studentized and Studentized Residuals vs. Studentized Residuals") +
  xlab("Studentized Residuals") +
  ylab("Difference between Externally Studentized and Studentized Residuals")

# Display the plot
print(ggplot)

# Load necessary library
library(ggplot2)

# Assuming fit0 is your linear model
# fit0 <- lm(SleepMinNight ~ ., data = df3)

# Calculate regular residuals
residual_1 <- fit0$residuals

# Get predicted values from the model
predicted_values <- predict(fit0)

# Create the plot
ggplot() +
  geom_point(aes(x = predicted_values, y = residual_1)) +
  ggtitle("Residuals vs. Predicted Values") +
  xlab("Predicted Values") +
  ylab("Residuals") +
  theme_minimal()

# Display the plot
print(ggplot)

# Load necessary library
library(ggplot2)

# Assuming fit0 is your linear model
# fit0 <- lm(SleepMinNight ~ ., data = df3)

# Calculate different types of residuals
residual_2 <- rstandard(fit0)
residual_3 <- rstudent(fit0)
residual_4 <- rstudent(fit0)  # Externally studentized residuals

# Get predicted values from the model
predicted_values <- predict(fit0)

# Plot for Standardized Residuals
ggplot() +
  geom_point(aes(x = predicted_values, y = residual_2)) +
  ggtitle("Standardized Residuals vs. Predicted Values") +
  xlab("Predicted Values") +
  ylab("Standardized Residuals") +
  theme_minimal()

# Plot for Studentized Residuals
ggplot() +
  geom_point(aes(x = predicted_values, y = residual_3)) +
  ggtitle("Studentized Residuals vs. Predicted Values") +
  xlab("Predicted Values") +
  ylab("Studentized Residuals") +
  theme_minimal()

# Plot for Externally Studentized Residuals
ggplot() +
  geom_point(aes(x = predicted_values, y = residual_4)) +
  ggtitle("Externally Studentized Residuals vs. Predicted Values") +
  xlab("Predicted Values") +
  ylab("Externally Studentized Residuals") +
  theme_minimal()


############### (5) Model Selection ########################################
step(fit0)
library(olsrr)
ols_step_forward_p(fit0,penter=0.1,details=F)
ols_step_forward_p(fit0,penter=0.05,details=F)
ols_mallows_cp(model =m_logfull_1, fullmodel =m_full)  # Mallows' Cp
ols_mallows_cp(model =m_logfull_2, fullmodel =m_full)  # Mallows' Cp
ols_mallows_cp(model =m_sqfull_1, fullmodel =m_full)  # Mallows' Cp
