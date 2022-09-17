library(tidyverse)      # collection of best packages
library(caret)          # machine learning functions
library(MLmetrics)      # machine learning metrics
library(car)            # VIF calculation
library(rpart)          # decision tree
library(class)   

setwd("D:/DATA ANALYTICS/BY DATASETS/TITANIC")
train=read.csv("train.csv")
test=read.csv("test1.csv")


anyDuplicated(train)
colSums(is.na(train))
colSums(is.na(test))

unique(train$Cabin)
train$Cabin <- replace_na(train$Cabin, 'U')
test$Cabin <- replace_na(test$Cabin, 'U')

train$Surname <- sapply(str_split(train$Name, ','), `[`, 1) %>% str_trim()
temp <- sapply(str_split(train$Name, ','), `[`, 2)
train$Title <- sapply(str_split(temp, '\\.'), `[`, 1) %>% str_trim()
train <- train %>% select(-Name)

test$Surname <- sapply(str_split(test$Name, ','), `[`, 1) %>% str_trim()
temp <- sapply(str_split(test$Name, ','), `[`, 2)
test$Title <- sapply(str_split(temp, '\\.'), `[`, 1) %>% str_trim()
test <- test %>% select(-Name)

unique(train$Title)
unique(test$Title)
test[test$Title == 'Dona', 'Title'] = 'Mrs'

age_by_title <- train %>% 
  group_by(Title) %>% 
  summarise(median = median(Age, na.rm = TRUE))
head(age_by_title)

train <- merge(train, age_by_title)
train[is.na(train$Age), 'Age'] <- train[is.na(train$Age), 'median']
train <- train %>% select(-median)

test <- merge(test, age_by_title)
test[is.na(test$Age), 'Age'] <- test[is.na(test$Age), 'median']
test <- test %>% select(-median)

fare_by_pclass <- train %>% 
  group_by(Pclass) %>% 
  summarise(median = median(Fare, na.rm = TRUE))
head(fare_by_pclass,20)
train <- merge(train, fare_by_pclass)
train[is.na(train$Fare), 'Fare'] <- train[is.na(train$Fare), 'median']
train <- train %>% select(-median)

test <- merge(test, fare_by_pclass)
test[is.na(test$Fare), 'Fare'] <- test[is.na(test$Fare), 'median']
test <- test %>% select(-median)

table(train$Embarked)

train$Embarked <- replace_na(train$Embarked, 'S')
test$Embarked <- replace_na(test$Embarked, 'S')

colSums(is.na(train))
colSums(is.na(test))

train <- train %>% 
  mutate_at(vars(Pclass, Title, Survived, Sex, Cabin, Embarked), as.factor)

test <- test %>% 
  mutate_at(vars(Pclass, Title, Survived, Sex, Cabin, Embarked), as.factor)

all_survived <- c()
all_deceased <- c()
combined <- c()

for (s in unique(train$Surname)) {
  temp <- train %>% filter((Title == 'Master' | Sex == 'female') & (Surname == s))
  if (nrow(temp) >= 2) {
    if (all(temp['Survived'] == 1)) {
      all_survived <- append(all_survived, s)
    } else if (!any(temp['Survived'] == 1)) {
      all_deceased <- append(all_deceased, s)
    } else {
      combined <- append(combined, s)
    }
  }
}

cat('Family who all survived          :', sort(all_survived), '\n')
cat('Family who are all deceased      :', sort(all_deceased), '\n')

train <- train %>% select(-c(Cabin,Ticket,Surname))
test <- test %>% select(-c(Cabin, Ticket,Surname))
str(train)
str(test)

#Random Forest
set.seed(234)
rfmodel <- train(Survived~., 
                      data=train,
                      method='rf', 
                      trControl=trainControl(method="cv", number=5))

rfmodel$results
prf=predict(rfmodel,test)

#Support Vector Machine
set.seed(234)
svmmodel <- train(Survived~., 
                 data=train,
                 method='svmRadial', 
                 trControl=trainControl(method="cv", number=5))
svmmodel$results
psvm=predict(svmmodel,test)

#XG Boost
set.seed(234)
boostmodel <- train(Survived~., 
                  data=train,
                  method='gbm', 
                  preProcess= c('center', 'scale'),
                  trControl=trainControl(method="cv", number=5))
boostmodel


