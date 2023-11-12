library(readr)
DOHMH_HIV_AIDS_Annual_Report <- read_csv("data/DOHMH_HIV_AIDS_Annual_Report.csv")
View(DOHMH_HIV_AIDS_Annual_Report)

# Install renv:
if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")

renv::init()

renv::restore()


if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

DOHMH_HIV_AIDS_Annual_Report <- read.csv("data/DOHMH_HIV_AIDS_Annual_Report.csv", header = FALSE,
                         stringsAsFactors = TRUE)

# The following code (optional) can be used to name the attributes in the
# iris_dataset:

# names(iris_dataset) <- c("sepal length in cm", "sepal width in cm",
#                          "petal length in cm", "petal width in cm", "class")

if (!is.element("readr", installed.packages()[, 1])) {
  install.packages("readr", dependencies = TRUE)
}

## STEP 4. Load sample datasets that are provided as part of a package ----
if (!is.element("mlbench", installed.packages()[, 1])) {
  install.packages("mlbench", dependencies = TRUE)
}
require("mlbench")

data("DOHMH_HIV_AIDS_Annual_Report")
##Step 5

dim(DOHMH_HIV_AIDS_Annual_Report)

## Step 6
sapply(DOHMH_HIV_AIDS_Annual_Report, class)
## Step 7 Measure of Central Tendancy

DOHMH_HIV_AIDS_Annual_Report_freq <- DOHMH_HIV_AIDS_Annual_Report$Borough
cbind(frequency = table(DOHMH_HIV_AIDS_Annual_Report_freq),
      percentage = prop.table(table(DOHMH_HIV_AIDS_Annual_Report_freq)) * 100)

## Step 8
DOHMH_HIV_AIDS_Annual_Report_Borough_mode <- names(table(DOHMH_HIV_AIDS_Annual_Report$Borough))[
  which(table(DOHMH_HIV_AIDS_Annual_Report$Borough) == max(table(DOHMH_HIV_AIDS_Annual_Report$Borough)))
]
print(DOHMH_HIV_AIDS_Annual_Report_Borough_mode)
### STEP 9. Measure the distribution of the data for each variable ----
summary(DOHMH_HIV_AIDS_Annual_Report)

### STEP 10. Measure the standard deviation of each variable ----

sapply(DOHMH_HIV_AIDS_Annual_Report[, c(1,7, 9, 11, 14,15, 17)], sd)

### STEP 11. Measure the variance of each variable ----
sapply(DOHMH_HIV_AIDS_Annual_Report[, c(1,7, 9, 11, 14,15, 17)], var)

### STEP 12. Measure the kurtosis of each variable ----

if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

sapply(DOHMH_HIV_AIDS_Annual_Report[, c(1,7, 9, 11, 14,15, 17)],  kurtosis, type = 2)

### STEP 13. Measure the skewness of each variable ----

sapply(DOHMH_HIV_AIDS_Annual_Report[, c(1,7, 9, 11, 14,15, 17)],  skewness, type = 2)

## Measures of Relationship ----
## STEP 14. Measure the covariance between variables ----
DOHMH_HIV_AIDS_Annual_Report_cov <- cov(DOHMH_HIV_AIDS_Annual_Report[, c(1,7, 9, 11, 14,15, 17)])
View(DOHMH_HIV_AIDS_Annual_Report_cov)

## STEP 15. Measure the correlation between variables ----
DOHMH_HIV_AIDS_Annual_Report_cor <- cor(DOHMH_HIV_AIDS_Annual_Report[, c(1,7, 9, 11, 14,15, 17)])
View(DOHMH_HIV_AIDS_Annual_Report_cor)
# Inferential Statistics ----
## STEP 16. Perform ANOVA on the “crop_dataset” dataset ----

DOHMH_HIV_AIDS_Annual_Report_one_way_anova <- aov(Deaths ~ Borough,data = DOHMH_HIV_AIDS_Annual_Report)
summary(DOHMH_HIV_AIDS_Annual_Report_one_way_anova)

## Univariate Plots ----

### STEP 17. Create Histograms for Each Numeric Attribute ----
par(mfrow = c(6, 15))
for (i in 6:15) {
  hist(DOHMH_HIV_AIDS_Annual_Report[, i], main = names(DOHMH_HIV_AIDS_Annual_Report)[i])
}


## Multivariate Plots ----

### STEP 21. Create a Correlation Plot ----

if (!is.element("corrplot", installed.packages()[, 1])) {
  install.packages("corrplot", dependencies = TRUE)
}
require("corrplot")
corrplot(cor(DOHMH_HIV_AIDS_Annual_Report[, 7:15]), method = "circle")

renv::snapshot()
