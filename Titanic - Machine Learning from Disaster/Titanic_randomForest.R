# - Setting the working directory

setwd("~/R Programming/Learning_R/Titanic - Machine Learning from Disaster")

# - Loading packages

library(tidyverse)

Titanic.train <- read.csv("train.csv", stringsAsFactors = FALSE)
Titanic.test <- read.csv("test.csv", stringsAsFactors = FALSE)


# - Explore the data

Titanic.train$IsTrainSet <- TRUE
Titanic.test$IsTrainSet <- FALSE

Titanic.test$Survived <- NA

Titanic.full <- rbind(Titanic.train, Titanic.test)

glimpse(Titanic.full)

table(Titanic.full$IsTrainSet) # - Train and test set for later stage

table(Titanic.full$Embarked) # - Two missing values in data

# - Removing missing value
Titanic.full[Titanic.full$Embarked == '', "Embarked"] <- 'S'

# - Missing values in Age
table(is.na(Titanic.full$Age))

# Imputing missing values with median of age

age.median <- median(Titanic.full$Age, na.rm = TRUE)

Titanic.full[is.na(Titanic.full$Age), "Age"] <- age.median

# - Missing value in fare columns and imputing them with median values
table(is.na(Titanic.full$Fare))

fare.median <- median(Titanic.full$Fare, na.rm = TRUE)

Titanic.full[is.na(Titanic.full$Fare), "Fare"] <- fare.median


# - Summary of full data

summary(Titanic.full)

# - Categorical casting

Titanic.full$Pclass <- as.factor(Titanic.full$Pclass)
Titanic.full$Sex <- as.factor(Titanic.full$Sex)
Titanic.full$Embarked <- as.factor(Titanic.full$Embarked)


# - Summary of full data

summary(Titanic.full)

# Train and Test sets

Titanic.train <- Titanic.full[Titanic.full$IsTrainSet == TRUE, ]
Titanic.test <- Titanic.full[Titanic.full$IsTrainSet == FALSE, ]

# changing outcome variable to factor
Titanic.train$Survived <- as.factor(Titanic.train$Survived)

# - Predictive Modeling 

Survived.equation <- "Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked"
Survived.formaula <- as.formula(Survived.equation)

# - install.packages("randomForest")
library(randomForest)


rf_model <- randomForest(Survived.formaula, data = Titanic.train,
                         ntree = 500,
                         mtry = 3,
                         nodesize = 0.01 * nrow(Titanic.test))


rf_model

summary(rf_model)


features.equation <- "Pclass + Age + Sex + SibSp + Parch + Fare + Embarked"

Survived <- predict(rf_model,
                    newdata = Titanic.test)

PassengerId <- (Titanic.test$PassengerId)
PassengerId <- as.data.frame(PassengerId)

PassengerId$Survived <- Survived

write.csv(PassengerId, file = "Titanic_ML_submission.csv")






