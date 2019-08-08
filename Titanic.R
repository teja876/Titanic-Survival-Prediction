#setting directory
#setwd('~/coursera/Titanic')

#unzipping files
#path <- "~/coursera/Titanic"
#unzip("~/coursera/Titanic/all.zip", exdir = path)

#reading csv file
Titanic <- read.csv('~/coursera/Titanic/train.csv')
Titanic_test <- read.csv('test.csv')

#summarising data
head(Titanic)
summary(Titanic)
str(Titanic)
nrow(Titanic)
names(Titanic)

#Loading libraries
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)

#returns logical vector of dataframe containing na values
na_logic <- complete.cases(Titanic)

#returns sums of rows containing null values
colSums(is.na(Titanic))

#replacing missing values in age coloumn with mean ages
clean_Titanic <- mutate(Titanic, new_ages = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age))
Titanic_test <- mutate(Titanic_test, new_ages = ifelse(is.na(Age), mean(clean_Titanic$new_ages), Age)) 

#removing unecessary coloumns
clean_Titanic2 <- select(clean_Titanic, -c(Name, Ticket, Age, Embarked, Cabin, PassengerId))
clean_Titanic2$new_ages <- round(clean_Titanic2$new_ages)
Titanic_test <- select(Titanic_test, -c(Name, Ticket, Age, Embarked, Cabin, PassengerId))
Titanic_test$new_ages <- round(Titanic_test$new_ages)

#checking correlation between coloumns
pairs(clean_Titanic2)

str(clean_Titanic2$Cabin)

suvcount <- summarise(group_by(clean_Titanic2, Sex), length(Sex))

#plotting barplot of counts of people survived or not in between sex
count1 <- table(clean_Titanic2$Sex, clean_Titanic2$Survived)
barplot(count1, col = c('red', 'blue'), legend = rownames(count1), 
        main = "survival count between sex", xlab = "Survived or not",
        ylab = "count", beside = TRUE)


count2 <- table(clean_Titanic2$Pclass, clean_Titanic2$Survived)
barplot(count2, col = c('red', 'blue', 'green'), legend = rownames(count2), 
        main = "survival count between passenger class", 
        xlab = "Survived or not",
        ylab = "count", beside = TRUE)

boxplot(clean_Titanic2$new_ages ~ clean_Titanic2$Survived, col = 'green')

count3 <- table(clean_Titanic2$SibSp, clean_Titanic2$Survived)
barplot(count3, main = "survival count based on sibling count", 
        xlab = "surived or not", ylab = "sibling count")



#visualizing data with pair plot
pairs(clean_Titanic2[,-1])


par(mfrow = c(1,2), mar = c(5,4,2,1))
with(subset(clean_Titanic2, Survived == 0), hist(new_ages, main = "not survived", col = "green"))
with(subset(clean_Titanic2, Survived == 1), hist(new_ages, main = "Survived", col = "green"))


#below 2 plot are same but facets are included in one
qplot(new_ages, data = clean_Titanic2, color = as.factor(Survived))

qplot(new_ages, data = clean_Titanic2, facets = as.factor(Survived)~.,
      color = as.factor(Survived))

#converting gender into numeric  for bulding models
clean_Titanic2$Sex <- as.numeric(clean_Titanic2$Sex)
Titanic_test$Sex <- as.numeric(Titanic_test$Sex)
clean_Titanic2$Survived <- as.factor(clean_Titanic2$Survived)

#Modelling

y_train <- clean_Titanic2[, "Survived"]
x_train <- select(clean_Titanic2, -c("Survived"))
y_test <- Titanic_test

#Logistic Regression
logreg <- train(Survived ~ . , method = "glm", data = clean_Titanic2)
logreg.pred <- predict(logreg, newdata = x_train)
mean(y_train == logreg.pred)

#Random Forest
rf <- train(Survived~. , method = "rf", data = clean_Titanic2)
rf.pred <- predict(rf, newdata = x_train)
mean(y_train == rf.pred)

#Decision  Trees
dt <- train(Survived~., method = "rpart", data = clean_Titanic2)
print(dt)

#K-NN
knn <- train(Survived~., method = "knn", data = clean_Titanic2)
print(knn)

Result <- predict(rf, newdata = Titanic_test)







