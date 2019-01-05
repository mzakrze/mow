# how to split data to train and test datasets
train_data_percent = 15
# how many times repeat each random forest run
repeat_each = 1

# tuning parameters for the Random Forest algorithm. 
# see: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf 
randomForest.mtry = 1
randomForest.ntree = 3
randomForest.replace = TRUE
randomForest.maxnodes = 10
randomForest.nodesize = 10

xArgName = "mtry"
yArgName = "ntree"
xArg = 1:10
yArg = 3:10