# how to split data to train and test datasets
train_data_percent = 15
# how many times repeat each random forest run
repeat_each = 10

# tuning parameters for the Random Forest algorithm. 
# see: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf 
randomForest.mtry = 1:10
randomForest.ntree = 3:10
randomForest.replace = c(TRUE, FALSE)
randomForest.maxnodes = 10:15
randomForest.nodesize = 10:15


