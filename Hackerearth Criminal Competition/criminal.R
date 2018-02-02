Crimtrain <- read_csv('Criminal Dataset/criminal_train.csv')
Crimtest <- read_csv('Criminal Dataset/criminal_test.csv')

temp <- Crimtrain[,-c(1,72)]
temp2 <- NA
for(i in 1:70)
  for(j in i:70)
    if(cor(temp[,i], temp[,j])> 0.6 | cor(temp[,i], temp[,j]) < -0.6)
      temp2[i] <- j 

temp2 <- temp2+1


traindata <- Crimtrain[,-c(1,72, unique(temp2))]
testdata <- Crimtest[,-c(1, unique(temp2))]

write.csv(Crimtrain[,-c(unique(temp2))], 'train34.csv', row.names = FALSE)
write.csv(Crimtest[,-c(unique(temp2))], 'test34.csv', row.names= FALSE)

Crimtest$Criminal <- -1
labeltr <- Crimtrain$Criminal
labelts <- Crimtest$Criminal

convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

traindata <- convert.magic(traindata, 'numeric')
testdata <- convert.magic(testdata, 'numeric')

traindata <- as.matrix(traindata)
testdata <- as.matrix(testdata)

Dtrain <- xgb.DMatrix(traindata, label = labeltr)
Dtest <- xgb.DMatrix(testdata, label = labelts)

bst <- xgb.train(data = Dtrain, 
                 nrounds = 7000,
                 eval_metric = 'rmse',
                 objective = 'binary:logitraw',
                 learning_rate = 0.04,
                 max_depth = 15,
                 min_child_weight = 10,
                 random_state = 3,
                 colsample_bytree = 0.8, 
                 subsample = 0.8)



predictions <- predict(bst, Dtest)

for( i in 1:11430){
  if(predictions[i] > 0.5)
    prediction1[i] <- 1
  else prediction1[i] <- 0
}
Crimtest$Criminal <- prediction1

write.csv(Crimtest[,c('PERID',"Criminal")], 'submission34.csv', row.names = FALSE)








