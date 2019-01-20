# 1 weź parametry domyślne
# dla ntree in 1:800:
# avg = zapuść algorytm 20 razy - uśrednij
# a = zapuść raz alg
# b = zapuść raz alg
# c = zapuść raz alg

# narysuj na 1 wykresie avg, a, b ,c


config_ntree = seq(from = 1, to = 600, by = 5)
config_repeat_each = 25

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

stability_test <- function(splitData) {
  trainDataInput = splitData$trainDataInput
  trainDataResponse = splitData$trainDataResponse
  testDataInput = splitData$testDataInput
  testDataResponse = splitData$testDataResponse

  getAucFromBuiltModel <- function(rf_model) {
    expected = testDataResponse
    actual = predict(rf_model, testDataInput, type = 'prob')[, 2]
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

  avg_auc_list = c()
  auc_var_list = c()
  a_auc_list = c()
  b_auc_list = c()
  c_auc_list = c()
  for(ntree_step in config_ntree) {
    auc_repeat = c()
    for(i in 1:config_repeat_each) {
        model = buildModel(ntree_step)
        avg_auc_step = getAucFromBuiltModel(model)
        auc_repeat = c(auc_repeat, avg_auc_step)
    }
    avg_auc = sum(auc_repeat) / length(auc_repeat)
    auc_var = var(auc_repeat)

    model = buildModel(ntree_step)
    a_auc = getAucFromBuiltModel(model)
    model = buildModel(ntree_step)
    b_auc = getAucFromBuiltModel(model)
    model = buildModel(ntree_step)
    c_auc = getAucFromBuiltModel(model)

    avg_auc_list = c(avg_auc_list, avg_auc)
    auc_var_list = c(auc_var_list, auc_var)
    a_auc_list = c(a_auc_list, a_auc)
    b_auc_list = c(b_auc_list, b_auc)
    c_auc_list = c(c_auc_list, c_auc)
  }

  list(avg_result=avg_auc_list, 
    var_result=auc_var_list,
    a_result=a_auc_list, 
    b_result=b_auc_list, 
    c_result=c_auc_list)
}


allData <- loadData()

allDataClustered = clustered(allData)

splitData = splitData(allDataClustered)

stability_result = stability_test(splitData)

df = data.frame(
    "avg_result" = stability_result$avg_result, 
    "a_result" = stability_result$a_result,
    "b_result" = stability_result$b_result,
    "c_result" = stability_result$c_result,
    "var_result" = stability_result$var_result,
    x = config_ntree)

View(df)

overfit_plot <- ggplot(data = df, aes(x = x)) +
        ggtitle("AUC stability test") +
        xlab("Number of trees in a forest") +
        ylab("AUC value") +
        geom_line(aes(y=avg_result, col="avg_result")) +
        geom_point(aes(y=avg_result, col="avg_result")) +
        #geom_line(aes(y=a_result, col="a_result")) +
        #geom_point(aes(y=a_result, col="a_result")) +
        geom_line(aes(y=b_result, col="b_result")) +
        geom_point(aes(y=b_result, col="b_result")) +
        geom_line(aes(y=c_result, col="c_result")) +
        geom_point(aes(y=c_result, col="c_result"))

variance_plot <- ggplot(data = df, aes(y=var_result, x=x)) +
    ggtitle("AUC variance test") + 
    xlab("Number of trees in a forest") +
    ylab("AUC value") +
    geom_line() +
    geom_point()









