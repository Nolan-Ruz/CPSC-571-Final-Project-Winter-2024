###Download if you haven't###
install.packages("randomForest")
install.packages("randomForest")
install.packages("e1071")
install.packages("keras")
install.packages("tensorflow")

library(keras)
library(tensorflow)
install_tensorflow()

###Importing Data###
data = read.csv("tweetProcessedData.csv")
data$y = as.factor(data$y)

###Splitting into train/test###
index = sample(nrow(data), round(nrow(data) * 0.8), replace = F)

train = data[index, ]
test = data[-index, ]

###Random Forest###
library(randomForest)

#running random forest
rForest = randomForest(y ~., train, n.trees = 100)

#creating prediction model
predRForest = predict(rForest, test)

#confusion matrix
cmRForest = table(predRForest, test$y)

#accuracy calculation
sum(diag(cmRForest))/sum(cmRForest)

###Logistic Regression###
#running logistic regression
log = glm(y ~., train, family = "binomial")

#prediction model
predLog = predict(log, test, type = "response")
predLog = ifelse(predLog > 0.5, 1, 0)

#confusion matrix
cmLog = table(predLog, test$y)

#accuracy
sum(diag(cmLog))/sum(cmLog)

###SVM###
library(e1071)

#svm model
m = svm(y ~ ., train, kernel = "radial", gamma = 1)

#prediction model
predSVM = predict(m, test)

#confusion matrix
cmSVM = table(predSVM, test$y)

#accuracy
sum(diag(cmSVM))/sum(cmSVM)
  
###LTSM###
library(keras)
library(tensorflow)

#predictors
predictorsTrain = train[,-58]
predictorsTest = test[,-58]

#response variable
responseTrain = train[,58]
responseTrain = as.numeric(responseTrain)

responseTest = test[,58]
responseTest = as.numeric(responseTest)

#defining the model
model = keras_model_sequential()
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

#compiling the model
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)


#fit the model
history <- model %>% fit(
  as.matrix(predictorsTrain), responseTrain,
  epochs = 25,
  batch_size = 128,
  validation_split = 0.2
)

#plotting training history
plot(history)

#prediction
predLTSM = model %>% predict(as.matrix(predictorsTest)) %>%
  `>`(0.5) %>%
  k_cast("int32")

# Convert predLTSM to a vector
predLTSM = as.vector(predLTSM)

#confusion matrix
cmLTSM = table(predLTSM, responseTest)
rownames(cmLTSM) <- c("0")
colnames(cmLTSM) <- c("0", "1")

cmLTSM

#accuracy
model %>% evaluate(as.matrix(predictorsTest), responseTest) 
