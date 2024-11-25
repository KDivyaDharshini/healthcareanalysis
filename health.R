#EDA
dataset=read.csv(file = file.choose())
#summary statistics
summary(dataset)
summary(dataset$MaxHR)
dataset$Cholesterol
#structure of data
str(dataset)
#display first few col
head(dataset,4)
#checking for null values
any(is.na(dataset))
colSums(is.na(dataset))
#removing duplicate rows
data<-unique(dataset$ChestPainType)
data
#scaling(standardzation)
dataset$RestingBP<-scale(dataset$RestingBP)
dataset$RestingBP
#finding outlier
z_scores<-scale(dataset$Cholesterol)
z_scores
outlier<-which(abs(z_scores)>1)
dataset_out<-dataset[ -outlier, ]
dataset_out
#subseting
normal<-subset(dataset,dataset$HeartDisease==0)
heart_disease<-subset(dataset,dataset$HeartDisease==1)
normal
#feature engineering -binning
#encoding numerical to categorical
dataset$HeartDisease<-cut(dataset$HeartDisease,breaks = 2,labels = c("normal","heart disease"))
head(dataset$HeartDisease)
#plot age vs cholestrol
x=dataset$Age

y=dataset$Cholesterol
#install.packages("plotly")
library(plotly)

# Create an interactive scatter plot
plot_ly(dataset, x = x, y = y, type = "scatter", mode = "markers")
# Install and load necessary packages
#install.packages("caret")
library(caret)

# Load the heart disease dataset (you can replace this with your dataset)


# Check the structure of the dataset
str(dataset)

# Split the dataset into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(dataset$HeartDisease, p = 0.8, list = FALSE)
train_data <- dataset[splitIndex, ]
test_data <- dataset[-splitIndex, ]

# Create a decision tree model using the rpart algorithm

library(rpart)
model <- train(HeartDisease ~., data = train_data, method = "rpart")

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, test_data$HeartDisease)
print(conf_matrix)

