# rm(list = ls())
# gc()
set.seed(123)
library(car)
library(ggplot2)
library(olsrr)
############### (1) Data cleaning ########################################
## select variables
library(NHANES)
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
  Gender,
  Age,
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

naniar::vis_miss(df2)
df3 <- na.omit(df2)
df4 <- df3 %>% select(BMI,
                      SleepHrsNight,
                      DaysMentHlthBad,
                      AlcoholYear,
                      DaysPhysHlthBad)
psych::pairs.panels(df4)

df4$BMI_Group = ifelse(df4$BMI >= 30, "BMI >= 30", "BMI < 30")

ggplot(df4, aes(x = BMI_Group, y = SleepHrsNight, fill = BMI_Group)) +
  geom_boxplot() +
  labs(title = "Sleep Hours by BMI Group",
       x = "BMI Group",
       y = "Sleep Hours per Night") +
  theme_minimal()


ggplot(data_complete, aes(x = Gender, y = BMI, fill = Gender)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "BMI Distribution by Gender", x = "Gender", y = "BMI")

library(ggplot2)

data_complete$BMI_Category <- ifelse(data_complete$BMI >= 30, "BMI >= 30", "BMI < 30")


ggplot(data_complete, aes(x = BMI_Category, y = SleepHrsNight, fill = BMI_Category)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "Sleep Hours by BMI Category", x = "BMI Category", y = "Hours of Sleep")


ggplot(data_complete, aes(x = BMI_Category, y = DaysMentHlthBad, fill = BMI_Category)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "DaysMentHlthBad by BMI Category", x = "BMI Category", y = "DaysMentHlthBad")

