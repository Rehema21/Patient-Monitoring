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

DOHMH_HIV_AIDS_Annual_Report_freq <- DOHMH_HIV_AIDS_Annual_Report$`Death rate`
cbind(frequency = table(DOHMH_HIV_AIDS_Annual_Report_freq),
      percentage = prop.table(table(DOHMH_HIV_AIDS_Annual_Report_freq)) * 100)

## Step 8
DOHMH_HIV_AIDS_Annual_Report_Death_rate_mode <- names(table(DOHMH_HIV_AIDS_Annual_Report$`Death rate`))[
  which(table(DOHMH_HIV_AIDS_Annual_Report$`Death rate`) == max(table(DOHMH_HIV_AIDS_Annual_Report$`Death rate`)))
]
print(DOHMH_HIV_AIDS_Annual_Report_Death_rate_mode)
### STEP 9. Measure the distribution of the data for each variable ----
summary(DOHMH_HIV_AIDS_Annual_Report)

### STEP 10. Measure the standard deviation of each variable ----

sapply(DOHMH_HIV_AIDS_Annual_Report[, c(7,8, 9,10, 11,12, 13, 14, 15,16, 17, 18)], sd)

### STEP 11. Measure the variance of each variable ----
sapply(DOHMH_HIV_AIDS_Annual_Report[, c(7,8, 9,10, 11,12, 13, 14, 15,16, 17, 18)], var)

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
print(names(DOHMH_HIV_AIDS_Annual_Report))  # Print column names
corrplot(cor(DOHMH_HIV_AIDS_Annual_Report[,1,2,3,4,5,15]), method = "circle")

renv::snapshot()



#Step 3

if (!is.element("NHANES", installed.packages()[, 1])) {
  install.packages("NHANES", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("NHANES")

## dplyr ----
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

## naniar ----
# Documentation:
#   https://cran.r-project.org/package=naniar or
#   https://www.rdocumentation.org/packages/naniar/versions/1.0.0
if (!is.element("naniar", installed.packages()[, 1])) {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("naniar")

## ggplot2 ----
# We require the "ggplot2" package to create more appealing visualizations
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")
# STEP 2. Create a subset of the variables/features ----
# We select only the following 13 features to be included in the dataset
names(DOHMH_HIV_AIDS_Annual_Report)

DOHMH_HIV_AIDS_Annual_Report<- DOHMH_HIV_AIDS_Annual_Report %>%
  select(`2011`, All...2, All...3, All...4, All...5, `3379`,
         `48.3`)
# STEP 3. Confirm the "missingness" in the Dataset before Imputation ----
# Are there missing values in the dataset?
any_na(DOHMH_HIV_AIDS_Annual_Report)

# How many?
n_miss(DOHMH_HIV_AIDS_Annual_Report)

# What is the percentage of missing data in the entire dataset?
prop_miss(DOHMH_HIV_AIDS_Annual_Report)

# How many missing values does each variable have?
DOHMH_HIV_AIDS_Annual_Report %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(DOHMH_HIV_AIDS_Annual_Report)

# What is the number and percentage of missing values grouped by
# each observation?
miss_case_summary(DOHMH_HIV_AIDS_Annual_Report)


if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Introduction ----
# Resampling methods are techniques that can be used to improve the performance
# and reliability of machine learning algorithms. They work by creating
# multiple training sets from the original training set. The model is then
# trained on each training set, and the results are averaged. This helps to
# reduce overfitting and improve the model's generalization performance.

# Resampling methods include:
## Splitting the dataset into train and test sets ----
## Bootstrapping (sampling with replacement) ----
## Basic k-fold cross validation ----
## Repeated cross validation ----
## Leave One Out Cross-Validation (LOOCV) ----

# STEP 1. Install and Load the Required Packages ----
## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## klaR ----
if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## LiblineaR ----
if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naivebayes ----
if (require("naivebayes")) {
  require("naivebayes")
} else {
  install.packages("naivebayes", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
DOHMH_HIV_AIDS_Annual_Report <- readr::read_csv(
  "data/DOHMH_HIV_AIDS_Annual_Report.csv",
  skip = 1
)

summary(DOHMH_HIV_AIDS_Annual_Report)
str(DOHMH_HIV_AIDS_Annual_Report)


## 1. Split the dataset ----
# Define an 80:20 train:test split ratio of the dataset
# Check column names
names(DOHMH_HIV_AIDS_Annual_Report)

train_index <- createDataPartition(DOHMH_HIV_AIDS_Annual_Report$`71`, # nolint
                                   p = 0.80, list = FALSE)
DOHMH_HIV_AIDS_Annual_Report_train <- DOHMH_HIV_AIDS_Annual_Report[train_index, ]
DOHMH_HIV_AIDS_Annual_Report_test <-DOHMH_HIV_AIDS_Annual_Report[-train_index, ]
train_control <- trainControl(method = "cv", number = 10)

DOHMH_HIV_AIDS_Annual_Report_model_lm <-
  caret::train(`71` ~ .,
               data = DOHMH_HIV_AIDS_Annual_Report_train,
               trControl = train_control, na.action = na.omit,
               method = "lm", metric = "RMSE")

### 2.b. Test the trained linear model using the testing dataset ----
predictions_lm <- predict(DOHMH_HIV_AIDS_Annual_Report_model_lm, DOHMH_HIV_AIDS_Annual_Report_test[, -14])

### 2.c. View the RMSE and the predicted values ====
print(DOHMH_HIV_AIDS_Annual_Report_model_lm)
print(predictions_lm)

## 3. Classification: LDA with k-fold Cross Validation ----

### 3.a. LDA classifier based on a 5-fold cross validation ----
# We train a Linear Discriminant Analysis (LDA) classifier based on a 5-fold
# cross validation train control but this time, using the churn variable for
# classification, not the customer value variable for regression.
train_control <- trainControl(method = "cv", number = 5)

# Assuming 'target_variable' is the name of your target variable
DOHMH_HIV_AIDS_Annual_Report_model_lda <- caret::train(`71` ~ .,
                                                       data = DOHMH_HIV_AIDS_Annual_Report_train,
                                                       trControl = trainControl, 
                                                       na.action = na.omit, 
                                                       method = "lda",
                                                       metric = "Accuracy")


### 3.b. Test the trained LDA model using the testing dataset ----
DOHMH_HIV_AIDS_Annual_Report_model_lda <- predict(DOHMH_HIV_AIDS_Annual_Report_model_lda,
                           DOHMH_HIV_AIDS_Annual_Report_test[, 1:13])

### 3.c. View the summary of the model and view the confusion matrix ----
print(DOHMH_HIV_AIDS_Annual_Report_model_lda)
caret::confusionMatrix(predictions_lda, churn_dateset_test$Churn)

## 4. Classification: Naive Bayes with Repeated k-fold Cross Validation ----
### 4.a. Train an e1071::naive Bayes classifier based on the churn variable ----
DOHMH_HIV_AIDS_Annual_Report_model_nb <-
  e1071::naiveBayes(`71` ~ ., data = DOHMH_HIV_AIDS_Annual_Report_train)

### 4.b. Test the trained naive Bayes classifier using the testing dataset ----
predictions_nb_e1071 <-
  predict(DOHMH_HIV_AIDS_Annual_Report_model_nb, DOHMH_HIV_AIDS_Annual_Report_test[, 1:13])

DOHMH_HIV_AIDS_Annual_Report_test$`71` <- factor(DOHMH_HIV_AIDS_Annual_Report_test$`71`)

### 4.c. View a summary of the naive Bayes model and the confusion matrix ----
print(DOHMH_HIV_AIDS_Annual_Report_model_nb)
caret::confusionMatrix(predictions_nb_e1071, DOHMH_HIV_AIDS_Annual_Report_test$`71`)
if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


# STEP 1. Install and Load the Required Packages ----
## ggplot2 ----
if (require("ggplot2")) {
  require("ggplot2")
} else {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## pROC ----
if (require("pROC")) {
  require("pROC")
} else {
  install.packages("pROC", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (require("dplyr")) {
  require("dplyr")
} else {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
  
}

data(DOHMH_HIV_AIDS_Annual_Report)
summary(DOHMH_HIV_AIDS_Annual_Report)
DOHMH_HIV_AIDS_Annual_Report_no_na <- na.omit(DOHMH_HIV_AIDS_Annual_Report)

## 2.b. Split the dataset ----
set.seed(7)

# We apply simple random sampling using the base::sample function to get
# 10 samples
train_index <- sample(1:dim(DOHMH_HIV_AIDS_Annual_Report)[1], 10) # nolint: seq_linter.
DOHMH_HIV_AIDS_Annual_Report_train <- DOHMH_HIV_AIDS_Annual_Report[train_index, ]
DOHMH_HIV_AIDS_Annual_Report_test <- DOHMH_HIV_AIDS_Annual_Report[-train_index, ]

## 2.c. Train the Model ----
train_control <- trainControl(method = "boot", number = 100)
names(DOHMH_HIV_AIDS_Annual_Report_train)
str(DOHMH_HIV_AIDS_Annual_Report_train)
DOHMH_HIV_AIDS_Annual_Report_model_lm <-
  train(`71` ~ ., data = DOHMH_HIV_AIDS_Annual_Report_train,
        na.action = na.omit, method = "lm", metric = "RMSE",
        trControl = train_control)

## 2.d. Display the Model's Performance ----
### Option 1: Use the metric calculated by caret when training the model ----
print(DOHMH_HIV_AIDS_Annual_Report_model_lm)

### Option 2: Compute the metric yourself using the test dataset ----
predictions <- predict(DOHMH_HIV_AIDS_Annual_Report_model_lm, DOHMH_HIV_AIDS_Annual_Report_test[, 1:6])

print(predictions)

#### RMSE ----
rmse <- sqrt(mean((DOHMH_HIV_AIDS_Annual_Report_test$cyl - predictions)^2))
print(paste("RMSE =", rmse))

#### SSR ----
# SSR is the sum of squared residuals (the sum of squared differences
# between observed and predicted values)
ssr <- sum((DOHMH_HIV_AIDS_Annual_Report_test$cyl - predictions)^2)
print(paste("SSR =", ssr))

#### SST ----
# SST is the total sum of squares (the sum of squared differences
# between observed values and their mean)
sst <- sum((longley_test$cyl - mean(DOHMH_HIV_AIDS_Annual_Report_test$cyl))^2)
print(paste("SST =", sst))

#### MAE ----
absolute_errors <- abs(predictions - DOHMH_HIV_AIDS_Annual_Report_test$cyl)
mae <- mean(absolute_errors)
print(paste("MAE =", mae))
####STEP 7a

if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 1. Install and Load the Required Packages ----
## stats ----
if (require("stats")) {
  require("stats")
} else {
  install.packages("stats", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## MASS ----
if (require("MASS")) {
  require("MASS")
} else {
  install.packages("MASS", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## glmnet ----
if (require("glmnet")) {
  require("glmnet")
} else {
  install.packages("glmnet", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## rpart ----
if (require("rpart")) {
  require("rpart")
} else {
  install.packages("rpart", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


library(readr)
DOHMH_HIV_AIDS_Annual_Report <- read_csv("data/DOHMH_HIV_AIDS_Annual_Report.csv")
View(DOHMH_HIV_AIDS_Annual_Report)

# A. Linear Algorithms ----
## 1. Linear Regression ----
### 1.a. Linear Regression using Ordinary Least Squares without caret ----

#### Load and split the dataset ----
data(DOHMH_HIV_AIDS_Annual_Report)

# Define an 80:20 train:test data split of the dataset.
train_index <- createDataPartition(DOHMH_HIV_AIDS_Annual_Report$`Year`,
                                   p = 0.8,
                                   list = FALSE)
DOHMH_HIV_AIDS_Annual_Report_train <- DOHMH_HIV_AIDS_Annual_Report[train_index, ]
DOHMH_HIV_AIDS_Annual_Report_test <- DOHMH_HIV_AIDS_Annual_Report[-train_index, ]
names(DOHMH_HIV_AIDS_Annual_Report)
#### Train the model ----
DOHMH_HIV_AIDS_Annual_Report_model_lm <- lm(Deaths ~ ., DOHMH_HIV_AIDS_Annual_Report_train)

#### Display the model's details ----
print(DOHMH_HIV_AIDS_Annual_Report_model_lm)

##step 8

if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Introduction ----
# The performance of the trained models can be compared visually. This is done
# to help you to identify and choose the top performing models.

# STEP 1. Install and Load the Required Packages ----
## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
library(readr)
DOHMH_HIV_AIDS_Annual_Report <- read_csv("data/DOHMH_HIV_AIDS_Annual_Report.csv")
View(DOHMH_HIV_AIDS_Annual_Report)

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

### LDA ----
set.seed(7)
Gender_model_lda <- train(Gender ~ ., data = DOHMH_HIV_AIDS_Annual_Report,
                       method = "lda", trControl = train_control)

### KNN ----
set.seed(7)
Gender_model_knn <- train(Gender ~ ., data = DOHMH_HIV_AIDS_Annual_Report,
                       method = "knn", trControl = train_control)

### SVM ----
set.seed(7)
Gender_model_svm <- train(Gender ~ ., data = DOHMH_HIV_AIDS_Annual_Report,
                       method = "svmRadial", trControl = train_control)

### Random Forest ----
set.seed(7)
Gender_model_rf <- train(Gender ~ ., data = DOHMH_HIV_AIDS_Annual_Report,
                      method = "rf", trControl = train_control)

results <- resamples(list(LDA = Gender_model_lda,
                          KNN = Gender_model_knn, SVM = Gender_model_svm,
                          RF = Gender_model_rf))

# STEP 4. Display the Results ----


summary(results)

## 2. Box and Whisker Plot ----


scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

diffs <- diff(results)

summary(diffs)

# step 9
# STEP 1. Install and Load the Required Packages ----
## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RRF ----
if (require("RRF")) {
  require("RRF")
} else {
  install.packages("RRF", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


# STEP 2. Load the Dataset ----
data(DOHMH_HIV_AIDS_Annual_Report)
dataset <- DOHMH_HIV_AIDS_Annual_ReportSonar
DOHMH_HIV_AIDS_Annual_Report_independent_variables <- dataset[, 1:60]
DOHMH_HIV_AIDS_Annual_Report_dependent_variables <- dataset[, 61]

# STEP 3. Train the Model ----


seed <- 7
metric <- "Accuracy"

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(seed)
mtry <- sqrt(ncol(sonar_independent_variables))
tunegrid <- expand.grid(.mtry = mtry)
DOHMH_HIV_AIDS_Annual_Report_model_default_rf <- train(Gender ~ ., data = dataset, method = "rf",
                                metric = metric,
                                # enables us to maintain mtry at a constant
                                tuneGrid = tunegrid,
                                trControl = train_control)
print(DOHMH_HIV_AIDS_Annual_Report_model_default_rf)

# STEP 4. Apply a "Random Search" to identify the best parameter value ----

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "random")
set.seed(seed)
mtry <- sqrt(ncol(sonar_independent_variables))

DOHMH_HIV_AIDS_Annual_Report_model_random_search_rf <- train(Gender ~ ., data = dataset, method = "rf",
                                      metric = metric,
                                      tuneLength = 12,
                                      trControl = train_control)

print(DOHMH_HIV_AIDS_Annual_Report_model_random_search_rf)
plot(DOHMH_HIV_AIDS_Annual_Report_model_random_search_rf)







