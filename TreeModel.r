#importing the dataset
titanic_data <- read.csv(file = "data/train.csv",header = TRUE)
titanic_data$Survived <- as.factor(titanic_data$Survived)
#creating the trainig and testing data
samples <- sample.int(n = nrow(titanic_data),
                      size = floor(.9*nrow(titanic_data)),
                      replace = FALSE)
titanic_train <- titanic_data[samples,]
titanic_test  <- titanic_data[-samples,]
#building the model
require("tree")
model1 <- tree(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked,
               titanic_train)
model1
plot(model1)
text(model1)
pred1 <- predict(model1,titanic_test[,-2])
maxidx <-  function(arr){
  return(which(arr == max(arr)))
}
idx <- apply(pred1,c(1),maxidx)
idx
model_pred = c(0,1)[idx]
#confmat
confmat <-  table(model_pred,titanic_test$Survived)
accuracy <- sum(diag(confmat))/sum(confmat)
accuracy
#loading the test data given by kaggle
test1 <- read.csv(file = "data/test.csv", header = TRUE)
pred2 <- predict(model1,test1)
model_pred2 <- c(0,1)[idx]
length(model_pred2)
final_out <- cbind(test1$PassengerId,model_pred2)
write.csv(final_out,file = "titanic_output.csv")

