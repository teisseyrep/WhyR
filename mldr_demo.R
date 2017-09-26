##################################
########## MLDR PACKAGE ##########
##################################

library("mldr")
library("randomForest")

#Read data in MULAN format (useful function):
scene = mldr("scene")  #scene.arff and scene.xml files are in the current folder
scene1 = scene$dataset
scene = mldr("scene", label_amount = 6)

#Create mldr object from data.frame:
data1 = data.frame(matrix(0, nrow = 10, ncol = 5))
for (j in 1:5)
  data1[, j] = rnorm(10)
for (j in 6:10)
  data1[, j] = rbinom(10, 1, 0.5)
names(data1)[6:10] = paste0("Y", 1:5)
myData <-
  mldr_from_dataframe(data1, labelIndices = 6:10, name = "myData")

#Build-in datasets:
data(emotions)

#Basic information about the data:
summary(emotions)

#Plotting the results:
plot(emotions, type = "LB")
plot(emotions, type = "LSB")

#Transformations:
emotions_lp <- mldr_transform(emotions, "LP")
emotions_br <- mldr_transform(emotions, "BR")

#BR method:
L = length(emotions_br) #number of labels.
prediction = matrix(0, ncol = L, nrow = nrow(emotions_br[[1]])) #matrix containing the results of predictions.

for (j in 1:L) {
  cat("Model ", j, "\n")
  temp = emotions_br[[j]]
  model = randomForest(x = temp[, -ncol(temp)],
                       y = as.factor(temp[, ncol(temp)]),
                       ntree = 100)
  prediction[, j] = ifelse(predict(model, temp) == "1", 1, 0)
}

#Evaluation measures (most useful function):
res <- mldr_evaluate(emotions, prediction)
# Overfitting!!!





