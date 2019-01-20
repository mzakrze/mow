config_ntree_start = 10
config_ntree_end = 200
config_repeat_each = 50

train_data_percent = 15

###################################

library("ggplot2")
library("randomForest")
library("pROC")
library("GA")
library(ROCR)


loadData <- function() {
  student_PK = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
  d1=read.table("./student-alcohol-consumption/student-mat.csv",sep=",",header=TRUE)
  d2=read.table("./student-alcohol-consumption/student-por.csv",sep=",",header=TRUE)
  d3=rbind(d1,d2)
  indices = duplicated(d3[, student_PK])
  d4 = d3[-indices,]
  d4
}

clustered <- function(data) {
  matrix = cbind(as.numeric(data$Walc), as.numeric(data$Dalc))
  # TODO - wydaje się, że powinienem ustawić centra jako c(1,1), c(5,5). Ale wtedy otrzymuje error "initial centers are not distinct"
  centers = cbind(c(1, 1), c(4,  5))
  k_means = kmeans(matrix, centers, iter.max = 100, trace = FALSE)
  data$is_alcoholic = k_means$cluster == 2
  data$is_alcoholic = as.factor(data$is_alcoholic)
  data
}

splitData <- function(allData) {
  data = allData[,-1 * grep("Dalc", colnames(allData))]
  data = data[,-1 * grep("Walc", colnames(data))]
  
  trainDataIndices = sample(nrow(data), nrow(data) * train_data_percent / 100)
  
  trainData = data[trainDataIndices,]
  trainDataResponse = trainData[, grep("is_alcoholic", colnames(trainData))]
  trainDataInput = trainData[,-1 * grep("is_alcoholic", colnames(trainData))]
  
  testData = data[-1 * trainDataIndices,]
  testDataResponse = testData[, grep("is_alcoholic", colnames(testData))]
  testDataInput = testData[,-1 * grep("is_alcoholic", colnames(testData))]
  
  list(
    trainDataInput = trainDataInput,
    trainDataResponse = trainDataResponse,
    testDataInput = testDataInput,
    testDataResponse = testDataResponse
  )
}

overfit_test <- function(splitData) {
  trainDataInput = splitData$trainDataInput
  trainDataResponse = splitData$trainDataResponse
  testDataInput = splitData$testDataInput
  testDataResponse = splitData$testDataResponse

  getAucFromBuiltModel <- function(rf_model, dataInput, dataResponse) {
    expected = dataResponse
    actual = predict(rf_model, dataInput, type = 'prob')[, 2]
    pred = prediction(actual, expected)
    perf = performance(pred, "auc")
    auc = perf@y.values[[1]]
    auc
  }

  buildModel <- function(ntree_in_step) {
    ntree = ntree_in_step
    randomForestResult = randomForest(
      x = trainDataInput,
      y = trainDataResponse,
      ntree = ntree,
      keep.forest = TRUE
    )
    randomForestResult    
  }

  auc_test_list = c()
  auc_train_list = c()
  for(ntree_step in config_ntree_start:config_ntree_end) {
    auc_test = 0
    auc_train = 0
    for(i in 1:config_repeat_each) {
        model = buildModel(ntree_step)
        auc_test_step = getAucFromBuiltModel(model, testDataInput, testDataResponse)
        auc_train_step = getAucFromBuiltModel(model, trainDataInput, trainDataResponse)
        auc_test = auc_test + auc_test_step
        auc_train = auc_train + auc_train_step
    }
    auc_test = auc_test / config_repeat_each
    auc_train = auc_train / config_repeat_each
    
    auc_test_list = c(auc_test_list, auc_test)
    auc_train_list = c(auc_train_list, auc_train)
  }

  list(auc_test = auc_test_list, auc_train = auc_train_list)
}


allData <- loadData()

allDataClustered = clustered(allData)

splitData = splitData(allDataClustered)

overfilt_results = overfit_test(splitData)

auc_test = overfilt_results$auc_test
auc_train = overfilt_results$auc_train

df = data.frame("test" = auc_test, "train" = auc_train, x = (config_ntree_start: config_ntree_end))

View(df)

overfit_plot <- ggplot(data = df, aes(y = test, x = x)) +
        ggtitle("Overfit test") +
        xlab("Number of trees in a forest") +
        ylab("AUC value") +
        geom_line(aes(y=test, col="test")) +
        geom_point(aes(y=test, col="test")) +
        geom_line(aes(y=train, col="train")) + 
        geom_point(aes(y=train, col="train")) + ylim(0, 1)








