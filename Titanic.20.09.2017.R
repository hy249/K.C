#Set Directory
setwd("C:/Data Science/kaggle/Titanic")

# To read the data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)


# Add a "Survived" variable to the test and apend it to the train
test$Survived <- "None"
data.apended <- rbind(train, test)
str(data.apended)

# Change Survived, P class,   to Factor
data.apended$Survived <- as.factor(data.apended$Survived)
data.apended$Pclass <- as.factor(data.apended$Pclass)
data.apended$Name <- as.character(data.apended$Name)
str(data.apended)


table(data.apended$Survived)
table(data.apended$Pclass)
table(data.apended$Sex)
summary(data.apended$Age)
#Age has 263 NA's! 19% of age is missing, this is too big for imputations!

head(train$Name)
#title is interesting!

length(unique(data.apended$Name))
# TWo ununique Names! Duplicated observation or just same names?

Same.Names <- data.apended[which(duplicated((data.apended$Name))), "Name"]
data.apended[which(data.apended$Name %in% Same.Names),]
str(data.apended$Name)

# To extract substrings "title" in a character vectorof "Name"
#substr(x, start, stop)
Name2 <- strsplit(data.apended$Name, ",")
Name2[1]

#create Family name and add it to the datasets:
Family.Name <- sapply(Name2, "[", 1)
data.apended[ , "Family.Name"] <- Family.Name

Name2 <- strsplit(sapply(Name2, "[", 2), " ")
titles <- sapply(Name2, "[", 2)
table(titles)
unique(titles)
# So there are similar titles that can be considered in one title
#The default code I used for merging them

titles[titles %in% c("???", "???")] <- "for example: Miss"
titles[titles %in% c("???", "???", "???")] <- "???"
titles[titles == "???"] <- "???."
table(titles)
# The complete code could be sent by request


data.apended[ , "Titles"] <- titles
data.apended$Titles <- as.factor(data.apended$Titles)

train[ , "Titles"] <- titles
data.apended$Titles <- as.factor(data.apended$Titles)

library(ggplot2)
# Suvived vs Pclass
ggplot(train, mapping=aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Suvived vs Sex
ggplot(train, mapping=aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Suvived vs Title
ggplot(data.apended[1:891,], mapping=aes(x = Titles, fill = factor(Survived))) +
  geom_bar() +
  xlab("Titles") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Survived vs Pclass vs Title
ggplot(data.apended[1:891,], aes(x = Titles, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass)

#Embarked!
str(data.apended$Embarked)
unique(data.apended$Embarked)
#But it doesnt make any sense that the "Embarked" explains survival or not,
#unless there was a discremination 

#Lets see what ggplot says
ggplot(train, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar() +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")
#Does not say anything special!

#numbers of companians:
data.apended$Companians <- data.apended$SibSp+data.apended$Parch
unique(data.apended$Companians)
data.apended$Companians <- as.factor(data.apended$Companians)
table(data.apended$Companians)

ggplot(data.apended[1:891,], aes(x = Companians, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass)
#interesting!


#Whats up with Cabin!
data.apended$Cabin[1:100]
#the first letter is interesting, maybe shows the level of the sheep
#this could be represented in Pclass already, lets have a look
#There are people with more than one cabine, There are people without a cabine
#Maybe cabines for all the ppl from the same family are mentioned only once in one of the members
data.apended$Cabin.Class <- as.factor(substr(data.apended$Cabin, 1, 1))
str(data.apended$Cabin.Class)
table(data.apended$Cabin.Class)


#And Fare!?
summary(data.apended$Fare)
unique(data.apended$Fare)

ggplot(data.apended[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")
#Fare does not give more signals than Pclass


library(Amelia)
missmap(data.apended[1:891,])
# So, age is significantly missing data


#Skewed data?!
table(train$Survived)
#Skewed but not heavily

########                 Lets Explore the Model            ##############

#To train a random forest algorythm:
library(randomForest)
Trained.RF.1 <- data.apended[1:891, c("Pclass", "Titles")]
train$survived <- as.factor(train$Survived)
rf.label <- train$Survived

set.seed(2468)
RF.1 <- randomForest(x = Trained.RF.1, y = rf.label, importance = TRUE, ntree = 1000)
RF.1
varImpPlot(RF.1)
#WoW, Titles does make sense!
# estimated error rate = 13.83%

# Random Forest BY Pclass, Title, & Companion
Trained.RF.2 <- data.apended[1:891, c("Pclass", "Titles", "Companians")]

set.seed(2468)
RF.2 <- randomForest(x = Trained.RF.2, y = rf.label, importance = TRUE, ntree = 1000)
RF.2
varImpPlot(RF.2)
# even less estimate of error rate!(13.18%)


# Random Forest BY Pclass, Title, & Parch
Trained. RF.3 <- data.apended[1:891, c("Pclass", "Titles", "Parch")]

set.seed(2468)
RF.3 <- randomForest(x = Trained. RF.3, y = rf.label, importance = TRUE, ntree = 1000)
RF.3
varImpPlot(RF.3)


# Random Forest by Pclass, Title, & Companion
Trained.RF.4 <- data.apended[1:891, c("Pclass", "Titles", "SibSp")]

set.seed(2468)
RF.4 <- randomForest(x = Trained.RF.4, y = rf.label, importance = TRUE, ntree = 1000)
RF.4
varImpPlot(RF.4)
#Estimated error rate = 13.63%


Test.dataset <- data.apended[892:1309, c("Pclass", "Titles", "Companians")]
Test.Prediction.1 <- predict(RF.2, Test.dataset)
table(Test.Prediction.1)

Kaggle.Submit.1 <- data.frame(PassengerId = rep(892:1309), Survived = Test.Prediction.1)

write.csv(Kaggle.Submit.1, file = "Kaggle.Submit.1.csv", row.names = FALSE)


############################## Cross Validation for avoiding overfittin
library(caret)
library(doSNOW)

set.seed(3546)
CV10 <- createMultiFolds(rf.label, k = 10, times = 10)



Train.Control.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                                index = CV10)

cluster <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


set.seed(27150)
FR.2.cv10.1 <- train(x = Trained. RF.2, y = rf.label, method = "rf", tuneLength = 3,
                     ntree = 1000, trControl = Train.Control.1)
stopCluster(cluster)
FR.2.cv10.1

### 5-FOLD CV

set.seed(17846)
CV5 <- createMultiFolds(rf.label, k = 5, times = 10)



Train.Control.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                                index = CV5)

cluster <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


set.seed(26354)
FR.2.cv5.1 <- train(x = Trained. RF.2, y = rf.label, method = "rf", tuneLength = 3,
                    ntree = 1000, trControl = Train.Control.2)
stopCluster(cluster)
FR.2.cv5.1


### 3-FOLD CV

set.seed(29461)
CV3 <- createMultiFolds(rf.label, k = 3, times = 10)



Train.Control.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                                index = CV3)

cluster <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


set.seed(37120)
FR.2.cv3.1 <- train(x = Trained. RF.3, y = rf.label, method = "rf", tuneLength = 3,
                    ntree = 100, trControl = Train.Control.2)
stopCluster(cluster)
FR.2.cv3.1

###################################################################################

# Create utility function
rp.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rp.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rp.cv)
}

# Grab features
features <- c("Pclass", "Titles", "Companians")
rp.train.1 <- data.combined[1:891, features]
rp.1.cv.1 <- rpart.cv(94622, rp.train.1, rf.label, Train.Control.3)
rp.1.cv.1

# Plot
prp(rp.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


################   TO BE CONTINUED   #####################
#####  THE FULL CODING WILL BE SENT BY YOUR REQUEST   ####







