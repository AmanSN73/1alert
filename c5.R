# C5.0
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
# Fitting C5.0 to the Training set
#install.packages("C50")
library(C50)
classifier <- C5.0(Alert~., data=training_set)
# Plotting the c5 tree 
plot(classifier)
summary(classifier)
# Step 4: Testing the model and finding the predictions for test data
predictions <- predict(classifier, newdata=test_set)
# Making the Confusion Matrix
confusionMatrix(predictions, factor(test_set$Alert))
                                                             

dataset_p = read.csv('Admin.csv', header = TRUE)
dataset_p <- dataset_p[,c(2,3,4,12,13)]
str(dataset_p)



dataset_p$ï..Situation = factor(dataset_p$Situation,
                              levels = c('Accident', 'Earthquake/Structural collapse',
                                         'Fire', 'Theft', 'Flood'),
                              labels = c(1, 2, 3, 4, 5))

dataset_p$Request.For  = factor(dataset_p$Request.For ,
                              levels = c('myself', 'others'),
                              labels = c(1, 2))

dataset_p$Gender  = factor(dataset_p$Gender ,
                         levels = c('Female', 'Male'),
                         labels = c(1, 2))


predictions <- predict(classifier, newdata=dataset_p)
output <- factor(predictions,levels = c(1,2,3))
print(output)
plot(predictions)

library(xlsx)
write.xlsx(output, file ="result.xlsx")