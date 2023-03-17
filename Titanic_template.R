#### Install & load package ------------
install.packages("readr") # file reading package
install.packages("ggplot2") # plotting package

#### Load package
library(readr)
library(ggplot2)

## Read data
titanic <- read.csv('C:/Users/yoyo/OneDrive/文件/雲端運算/R code/230224/Titanic.csv')

#### Data exploration
## Data understanding

dim(titanic)
summary(titanic)
str(titanic)
names(titanic)
head(titanic,n=3)


head(titanic,n=10)
## Transform column type
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
str(titanic)

#### Uni-variate analysis ------------
#### Categorical variable
## Variable: Survived
table(titanic$Survived)
table(titanic$Survived)/dim(titanic)[1] #計算比率


# Bar chart
g <- ggplot(data=titanic)
g+geom_bar(aes(x = Survived, fill = Survived))

## Variable: Sex
table(titanic$Sex)
table(titanic$Sex)/dim(titanic)[1]
# Bar chart
g <- ggplot(data=titanic)
g+geom_bar(aes(x = Sex, fill = Sex))

## Variable: Pclass
table(titanic$Pclass)
table(titanic$Pclass)/dim(titanic)[1]
# Bar chart
g <- ggplot(data=titanic)
g+geom_bar(aes(x = Pclass, fill =Pclass))


## Variable: Embarked
table(titanic$Embarked)
table(titanic$Embarked)/dim(titanic)[1]
# Bar chart
g <- ggplot(data=titanic)
g+geom_bar(aes(x = Embarked, fill =Embarked))


#少量遺漏值使用眾數進行補值
#Mode imputation
titanic$Embarked[titanic$Embarked == ''] <- 'S'
# Re-plot bar chart
g <- ggplot(data=titanic)
g+geom_bar(aes(x = Embarked, fill = Embarked))

#### Numerical variable
## Variable: Age
table(titanic$Age)
# Bar chart
g+geom_bar(aes(x = Age, fill =Age))

# Summary
summary(titanic$Age)

# Boxplot
g+geom_boxplot(aes(y= Age))


#遺漏值使用中位數進行補值
# Median imputation
age_median <-summary(titanic$Age)[3]
titanic$Age[is.na(titanic$Age)] <- age_median

# Histogram(先看最原始的data型態)
g + geom_histogram(aes(x = Age), bins = 70)
g + geom_histogram(aes(x = Age), bins = 35)
g + geom_histogram(aes(x = Age), bins = 7)

## Variable: Fare
summary(titanic$Fare)

# Boxplot
g+geom_boxplot(aes(y= Fare))

# Histogram
g + geom_histogram(aes(x = Fare), bins = 30)

#### Mixed data type
## Variable: Cabin
unique(titanic$Cabin)
length(unique(titanic$Cabin))
sum(titanic$Cabin=="")

# Imputing missing values with same Cabin
titanic$Cabin[titanic$Cabin==""] <- 'X0'

# Extract cabin area
titanic$Cabin_code <- substr(titanic$Cabin, 1, 1)

# Table
table(titanic$Cabin_code)

# Bar chart
g <- ggplot(data=titanic)
g+geom_bar(aes(x = Cabin_code, fill = Cabin_code))

## Variable: Ticket
unique(titanic$Ticket)
length(unique(titanic$Ticket))

#### Bi-variate analysis ------------
#### Categorical vs. Categorical
## Survived vs. Gender
# Bar chart
g+geom_bar(aes(x = Sex, fill=Survived))
g+geom_bar(aes(x = Survived, fill=Sex))


## Survived vs. Pclass
# Bar chart
g+geom_bar(aes(x = Survived, fill=Pclass))

## Survived vs. Embarked
# Bar chart
g+geom_bar(aes(x = Survived, fill=Embarked))

# Additional bar chart (Embarked vs. Sex)
g+geom_bar(aes(x = Embarked, fill=Sex))

## Survived vs. Cabin_code
# Bar chart




#### Multivariate analysis ----------
# Faceted bar chart
g+geom_bar(aes(x = Sex, fill=Survived)) + facet_wrap(~Pclass)

# Faceted ratio bar chart
g+geom_bar(aes(x = Sex, fill=Survived), position = position_fill()) + facet_wrap(~Pclass)



#### Prediction model ----------
## Install & load decision tree package
install.packages("rpart")
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

## Build model
model <- rpart(Survived ~ Age,
               data=titanic, method="class")

model <- rpart(Survived ~ Age+Sex,
               data=titanic, method="class")


## Plot decision tree
rpart.plot(model)

# Show variable importance
model$variable.importance
barplot(model$variable.importance)

## Load new data
titanic_new <- read.csv('C:/Users/yoyo/OneDrive/文件/雲端運算/R code/230224/Titanic_new.csv')

# Transform column type
titanic_new$Survived <- as.factor(titanic_new$Survived)
titanic_new$Pclass <- as.factor(titanic_new$Pclass)

# Predict new data
survived_pred <- predict(model, titanic_new, type = 'class')

## Prediction performance
# Confusion matrix
confusionMatrix <- table(survived_pred,titanic_new$Survived)

# Accuracy
accuracy <- sum(diag(confusionMatrix))/sum(confusionMatrix)
accuracy
