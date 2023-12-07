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

library(dplyr)
############### (2) Data categorize ########################################

library(gtsummary)
library(kableExtra)
df3 <- df3 %>%
  mutate(BMIcat = case_when(
    BMI < 18  ~ "<18",
    BMI >= 18 & BMI <= 30 ~ "18-30",
    BMI > 30  ~ ">30"
  ),
  BMIcat = factor(BMIcat, levels = c("<18", "18-30", ">30")))

############### (3) generate table ########################################
table_basic <- df3 %>%
  tbl_summary(
    by = BMIcat,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(2, 2),
                  all_categorical() ~ c(0, 1)),
    missing = "no"
  ) %>%
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p, digits=1)}%)"
  ) %>%
  modify_caption("Participant characteristics, by BMI category") %>%
  bold_labels() %>%
  add_overall(col_label = "**All participants**<br>N = {N}") %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "p-values from Kruskal-Wallis rank sum test"
  )

# View the table
print(table_basic)


############### (4) convert to latex ########################################

# Assuming 'table1' is already created using gtsummary

# Convert to a kableExtra object
kable_table <- as_kable(table_basic)

# Assuming 'kable_table' is already created using gtsummary
# Convert to a kableExtra object
latex_table <- kable_table %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Save the LaTeX table to file
save_kable(file = "table_for_overleaf.tex", latex_table)

############### (4) convert to excel ########################################
library(openxlsx)
# Assuming 'table_basic' is your gtsummary table
df_for_excel <- as.data.frame(table_basic)
write.xlsx(df_for_excel, file = "my_table.xlsx")

############### (5) convert to png ########################################
# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)



# Assuming 'df' is your data frame
df <- as.data.frame(table_basic)  # Your data frame here

# Convert the data frame to a table grob
table_grob <- tableGrob(df)

# Draw the plot (not shown on screen, only created for saving)
png(filename = "table_image.png", width = 900, height = 800)
grid.draw(table_grob)
dev.off()  # Close the PNG device



