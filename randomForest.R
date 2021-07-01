# Random Forest
# Step 1: Data Preprocessing
# Importing the dataset
dataset = read.csv('Trainog.csv', header = TRUE)
dataset <- dataset[,c(1,2,3,11,12,15)]
str(dataset)

dataset$Alert = factor(dataset$Alert, levels = c(1, 2, 3))


dataset$ï..Situation = factor(dataset$ï..Situation,
                              levels = c('Accident', 'Earthquake/Structural collapse',
                                         'Fire', 'Theft', 'Flood'),
                              labels = c(1, 2, 3, 4, 5))

dataset$Request.For  = factor(dataset$Request.For ,
                              levels = c('Myself', 'Others'),
                              labels = c(1, 2))

dataset$Gender  = factor(dataset$Gender ,
                         levels = c('F', 'M'),
                         labels = c(1, 2))

# Step 2: Splitting the dataset into the Training set(80%) and Test set(20%)
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Alert, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE)
# Step 3: Model Training
# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(Alert~., data = training_set,
                          ntree = 500)
# Step 4: Predicting the Test set results
predictions <- predict(classifier,test_set, type="class")
# Plot barplot and random forest
plot(predictions, col=c("red","green"), lwd = 5, main = "Random Forest",
     horiz = TRUE)
plot(randomForest(Alert ~ ., test_set, keep.forest=FALSE, ntree=250),log="y")
# Making the Confusion Matrix
#install.packages("caret")
library(caret)
confusionMatrix(predictions, factor(test_set$Alert))





