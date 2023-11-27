### Set working directory
## Write your own code to set your current working directory to a folder with all files extracted from “Data.zip”
## Example: setwd("~/Documents/Biostat 600 2020/hw3")

### Read data ##################
library(NHANES)
library(dplyr)
df0 <- NHANES
df <- NHANES[NHANES$Age >= 18 & NHANES$Age < 60,]
df <- df[!duplicated(df),]

############### (1) Data cleaning ########################################
### Create a subset with only varaibles of interest: Age, Comorbidity1, CurrentSmoker, Depression, Fatalism, HiChol, Htn, NIHScore, Optimism, Pessimism, R_E, Sex, Spirituality
data2 <- df %>% select(
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

### "CurrentSmoker", "HiChol", "Htn", "R_E", "Sex" are binary variables, so set them as factors

#data type
data2$Gender <- ifelse(data2$Gender == "male", 0, 1)
data2$Smoke100 <- ifelse(data2$Smoke100 == "No", 0, 1)
data2$PhysActive <- ifelse(data2$PhysActive == "No", 0, 1)
data2 <- data2 %>%
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
data2 <- data2 %>%
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

data2$PhysActive = as.factor(data2$PhysActive)
data2$Smoke100 = as.factor(data2$Smoke100)
data2$HealthGen = as.factor(data2$HealthGen)
data2$Race1 = as.factor(data2$Race1)
data2$Gender = as.factor(data2$Gender)

### Create an indicator variable “Incomplete” (0 if all variables are complete; 1 if at least one variable is missing)
miss_num = apply(data2, 1, function(x) sum(is.na(x)))
data2$Incomplete = ifelse(miss_num>0, 1, 0)
data2$Incomplete = as.factor(data2$Incomplete)


############### (2) Compare complete to non-complete cases with graphs and t/χ2 tests ################
##### Create two categorical variables
## a) Create “Age_4Cat” with 3 levels (0/1/2/3):20, 40, 60
data2$Age_4Cat = cut(data2$Age, c(20, 40, 60, max(data2$Age, na.rm = T)), right=T, labels=c(0:2))

## b) Create “BMI_cat” with 3 levels (0/1/2/3): 18, 25, 30
data2$BMI_cat = cut(data2$BMI, c( 20, 40, 60, max(data2$BMI, na.rm = T)), right=F, labels=c(0:2))

## (c) Create a new dataset with only complete data; Create another new dataset with only incomplete data ################
data_complete = subset(data2, data2$Incomplete == 0)
data_incomplete = subset(data2, data2$Incomplete == 1)


## (d) null hypothesis: H0:p1=p2  Ha: H1:$p1 \neq p2$; Decision: Do not reject H0
n1f=2710
p1=sum(data_complete$BMI)/n1f
n2f=1509
p2=sum(data_incomplete$BMI)/n2f
f_diff=p1-p2
se_f=sqrt(p1*(1-p1)/n1f+p2*(1-p2)/n2f)
z <- f_diff/se_f
p_value_f <- 2 * (1 - pnorm(abs(z)))
low_bound_diff_f <- f_diff - 1.96 * se_f
high_bound_diff_f <- f_diff + 1.96 * se_f


## (e) Use t-tests (χ2 tests) to compare differences in the mean (or proportion) of a given variable comparing complete cases to incomplete cases
## Continuous variables: get mean(sd) and p-values from two-sample t-tests
# Prepare data
Cont_vars = c( "BMI",
               "SleepHrsNight",
               "Age",
               "TotChol",
               "BPDiaAve",
               "BPSysAve",
               "AlcoholYear",
               "DaysPhysHlthBad",
               "Poverty",
               "UrineFlow1",
               "DaysMentHlthBad"
               )

Cont_complete = subset(data_complete, select = Cont_vars)
Cont_incomplete = subset(data_incomplete, select = Cont_vars)
Cont_incomplete_numeric <- as.data.frame(lapply(Cont_incomplete, as.numeric))
Cont_complete_numeric <- as.data.frame(lapply(Cont_complete, as.numeric))
# Get mean and sd for complete cases
Cont_complete_mean = apply(Cont_complete_numeric, 2, mean)
Cont_complete_sd = apply(Cont_complete_numeric, 2, sd)
# Get mean and sd for incomplete cases
Cont_incomplete_mean = apply(Cont_incomplete_numeric, 2, mean, na.rm = T)
Cont_incomplete_sd = apply(Cont_incomplete_numeric, 2, sd, na.rm = T)
# Count # of complete cases for each continuous variable seperately in all complete cases dataset and incomplete cases dataset
Cont_Number_complete = apply(Cont_complete_numeric, 2, function(x) sum(complete.cases(x)))
Cont_Number_incomplete = apply(Cont_incomplete_numeric, 2, function(x) sum(complete.cases(x)))
# Determine whether the variance from complete cases and variance from incomplete cases are equal or not
Cont_F_P_value = sapply(1:length(Cont_vars), function(x) var.test(Cont_complete_numeric[,x], Cont_incomplete_numeric[,x], alternative = "two.sided")$p.value>=0.05)

# Get p-values from two-sample t-tests
Cont_P_value = sapply(1:length(Cont_vars), function(x) t.test(Cont_complete_numeric[,x], Cont_incomplete_numeric[,x], var.equal = Cont_F_P_value[x])$p.value)

# Generate the summary table used to fill in Table1
Cont_summary = data.frame(cbind(Cont_Number_complete, Cont_complete_mean, Cont_complete_sd,
                                Cont_Number_incomplete, Cont_incomplete_mean, Cont_incomplete_sd,
                                Cont_P_value))
format(Cont_summary, nsmall = 2) ## only keep the first two decimals

## Categorical/binary variables: get proportion and p-values from χ2 tests
## Binary variables
# Prepare data
Bi_vars = c("Gender", "Smoke100",  "PhysActive")
Bi_complete = subset(data_complete, select = Bi_vars)
Bi_incomplete = subset(data_incomplete, select = Bi_vars)
Bi_data = subset(data2, select = c(Bi_vars, "Incomplete"))


# Count # of complete cases for each binary variable seperately in all complete cases dataset and incomplete cases dataset
Bi_Number_complete = apply(Bi_complete, 2, function(x) sum(complete.cases(x)))
Bi_Number_incomplete = apply(Bi_incomplete, 2, function(x) sum(complete.cases(x)))
# Calculate the proportions of cases with level=1 for each binary variable seperately in all complete cases dataset and incomplete cases dataset
Bi_prop_complete = sapply(1:length(Bi_vars), function(x) prop.table(table(Bi_complete[,x]))[2])
Bi_prop_incomplete = sapply(1:length(Bi_vars), function(x) prop.table(table(Bi_incomplete[,x]))[2])
# Get p-values from χ2 tests

Bi_P_value = sapply(Bi_vars, function(x) chisq.test(table(Bi_data[[x]], Bi_data$Incomplete), correct = FALSE)$p.value)
# Generate the summary table used to fill in Table1
Bi_summary = data.frame(cbind(Bi_Number_complete, Bi_prop_complete, Bi_Number_incomplete,
                              Bi_prop_incomplete, Bi_P_value))
format(Bi_summary, nsmall = 2) ## only keep the first two decimals

## Categorical variables
# Prepare data
Cat_vars = c("HealthGen", "Race1")
Cat_complete = subset(data_complete, select = Cat_vars)
Cat_incomplete = subset(data_incomplete, select = Cat_vars)
Cat_data = subset(data2, select = c(Cat_vars, "Incomplete"))
# Count # of complete cases for each categorical variable seperately in all complete cases dataset and incomplete cases dataset
Cat_Number_complete = apply(Cat_complete, 2, function(x) sum(complete.cases(x)))
Cat_Number_incomplete = apply(Cat_incomplete, 2, function(x) sum(complete.cases(x)))
# Calculate the proportions of each level for each categorical variable seperately in all complete cases dataset and incomplete cases dataset
Cat_prop_complete = t(apply(Cat_complete, 2, function(x) prop.table(table(x))))
Cat_prop_incomplete = t(apply(Cat_incomplete, 2, function(x) prop.table(table(x))))
# Get p-values from χ2 tests

Cat_P_value = sapply(1:length(Cat_vars), function(x) chisq.test(table(Cat_data[[Cat_vars[x]]], Cat_data$Incomplete), correct = F)$p.value)


# Generate the summary table used to fill in Table1
Cat_summary = data.frame(cbind(Cat_Number_complete, Cat_prop_complete,
                               Cat_Number_incomplete,
                               Cat_prop_incomplete, Cat_P_value), check.names = F)
format(Cat_summary, nsmall = 2) ## only keep the first two decimals


