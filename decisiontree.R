# Decision Tree using cart
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
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Alert, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
# Step 3: Model Training
# Finding the Root node using the max information gain
#install.packages('FSelector')
library(FSelector)
information.gain(Alert~., training_set, unit="log2")
#install.packages("rpart")
library(rpart)
library(rpart.plot)
mydt <- rpart.control(minsplit=1, minbucket=1, maxdepth=10, cp=0.01)
mytree <- rpart(Alert~.,  method="class", data=training_set, control=mydt)
# Plotting the decision tree 
rpart.plot(main="Tree for 1Alert",mytree,
           shadow.col = "gray", box.palette = "RdYlGn",  extra=108, tweak = 1.9)
# Step 4: Testing the model and finding the predictions for test data
predictions <- predict(mytree,test_set, type="class")
# Confusion Matrix
#install.packages("caret")
library(caret)
confusionMatrix(predictions, factor(test_set$Alert))



