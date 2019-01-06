# how to split data to train and test datasets
train_data_percent = 15
# how many times repeat each random forest run
repeat_each = 10

# tuning parameters for the Random Forest algorithm. 
# see: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf 
randomForest.mtry = 3:100
randomForest.ntree = 3:1000
randomForest.replace = c(TRUE, FALSE)
randomForest.maxnodes = 1:200
randomForest.nodesize = 1:200


