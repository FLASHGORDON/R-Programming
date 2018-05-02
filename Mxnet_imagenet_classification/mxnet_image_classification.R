
catsdata <- readRDS('cat.rds')
dogsdata <- readRDS('dog.rds')

library(caret)
data <- rbind(catsdata,dogsdata)
index <- createDataPartition(data$label, p = 0.9, times = 1)
index <- unlist(index)
train <- data[index,]
test <- data[-index,]

train_mat <- data.matrix(train)
train_x <- t(train_mat[,-1])
train_y <- train_mat[,1]
dim(train_x) <- c(28,28,1,ncol(train_x))


test_mat <- data.matrix(test)
test_x <- t(test_mat[,-1])
test_y <- train_mat[,1]
dim(test_x) <- c(28,28,1,ncol(test_x))

library(mxnet)
mx_data <- mx.symbol.Variable('data')

conv_1 <- mx.symbol.Convolution(data = mx_data, kernel = c(5,5), num_filter = 30)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type=  'tanh')
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = 'max', kernel = c(2,2))
# 
# conv_2 <- mx.symbol.Convolution(data = tanh_1, kernel = c(5,5), num_filter = 50)
# tanh_2 <- mx.symbol.Activation(data = conv_2, act_type=  'tanh')
# pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = 'max', kernel = c(2,2))


flat <- mx.symbol.Flatten(data = pool_1)
fcl_1 <- mx.symbol.FullyConnected(data= flat, num_hidden = 50)
tanh_3 <- mx.symbol.Activation(data= fcl_1, act_type = 'tanh')

fcl_2 <- mx.symbol.FullyConnected(data= tanh_3, num_hidden = 2)

NN_model <- mx.symbom.SoftmaxOutput(data= fcl_2)

device <- mx.cpu()


model <- mx.model.FeedForward.Create(NN_model,X = train_x, y = train_y,
                                     ctx= device,
                                     num.round = 30,
                                     array.batch.size = 100,
                                     learning.rate = 0.07,
                                     momentum = 0.9,
                                     wd = 0.00001,
                                     eval.metric = mx.metric.accuracy)

model2 <- mx.model.mlp(X= train_x, y = train_y,
                       ctx = device,
                       num.round = 50,
                       array.batch.size= 100,
                       learning.rate = 0.07,
                       eval.metric = mx.metric.accuracy)


predict_probs <- predict(model, test_x)
predicted_labels <- max.col(t(predict_probs)) - 1
confusionMatrix(predicted_labels, test_y)


















  
  
  
  
  
  
  