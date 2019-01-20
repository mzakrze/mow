theta_min <-
    c(
      ntree = 1,
      mtry = 1,
      maxnodes = 1,
      nodesize = 1
    )
  
theta_max <-
    c(
        ntree = 1000,
        mtry = 30,
        maxnodes = 400,
        nodesize = 400
    )   

train_data_percent = 15

fitness_function_repeat_each = 5

ga_pop_size = 50

ga_max_iter = 100
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
  
  trainData = data[trainDataIndices, ]
  trainDataResponse = trainData[, grep("is_alcoholic", colnames(trainData))]
  trainDataInput = trainData[,-1 * grep("is_alcoholic", colnames(trainData))]
  
  testData = data[-1 * trainDataIndices, ]
  testDataResponse = testData[, grep("is_alcoholic", colnames(testData))]
  testDataInput = testData[,-1 * grep("is_alcoholic", colnames(testData))]
  
  list (
    testDataInput = testDataInput,
    testDataResponse = testDataResponse,
    trainDataInput = trainDataInput,
    trainDataResponse = trainDataResponse
  )
}


find_parameters_with_genetic_algorithm <- function(trainDataInput, trainDataResponse) {
  validate_data_percent = 20
  validateDataIndices = 
    sample(nrow(trainDataInput), nrow(trainDataInput) * train_data_percent / 100)

  # dane do budowania lasu
  trainInput = trainDataInput[-1 * validateDataIndices, ]
  trainResponse = trainDataResponse[-1 * validateDataIndices]
  # dane do wyliczenia fitness function
  validateInput = trainDataInput[validateDataIndices, ]
  validateResponse = trainDataResponse[validateDataIndices]

  best_auc = 0
  best_params = list(
    mtry = NULL,
    ntree = NULL,
    replace = NULL,
    maxnodes = NULL,
    nodesize = NULL
  )

  fitnessFunc <- function(x) {
    ntree = floor(x[1])
    mtry = floor(x[2])
    maxnodes = floor(x[3])
    nodesize = floor(x[4])
    randomForestResult = randomForest(
      x = trainInput,
      y = trainResponse,
      ntree = ntree,
      mtry = mtry,
      nodesize = nodesize,
      maxnodes = maxnodes,
      keep.forest = TRUE
    )
    
    expected = validateResponse
    actual = predict(randomForestResult, validateInput, type = 'prob')[, 2]
    pred = prediction(actual, expected)
    perf = performance(pred, "auc")
    auc = perf@y.values[[1]]
    auc
  }

  repeat_and_average <- function(x) {
    acc = 0.0
    for (i in 1:fitness_function_repeat_each) {
      acc = acc + fitnessFunc(x)
    }
    acc / fitness_function_repeat_each
  }

  result <-
    ga(
      type = "real-valued",
      fitness = repeat_and_average,
      names = names(theta_min),
      lower = theta_min,
      upper = theta_max,
      popSize = ga_pop_size,
      maxiter = ga_max_iter
    )

  x <- result@solution
  
  ntree = floor(x[1])
  mtry = floor(x[2])
  replace = FALSE
  maxnodes = floor(x[3])
  nodesize = floor(x[4])
  best_params = list(
    mtry = mtry,
    ntree = ntree,
    replace = replace,
    maxnodes = maxnodes,
    nodesize = nodesize
  )

  best_params
}

build_model_default_params <- function(trainDataInput, trainDataResponse) {
  randomForestResult = randomForest(
    x = trainDataInput,
    y = trainDataResponse,
    keep.forest = TRUE
  )
  randomForestResult
}

build_model_with_params <- function(trainDataInput, trainDataResponse, params) {
  randomForestResult = randomForest(
    x = trainDataInput,
    y = trainDataResponse,
    ntree = params$ntree,
    mtry = params$mtry,
    nodesize = params$nodesize,
    maxnodes = params$maxnodes,
    keep.forest = TRUE
  )
  randomForestResult
}

plot_roc_analysis <- function(rf_model, testDataInput, testDataResponse, filename, plotTitle) {
  expected = testDataResponse
  actual = predict(rf_model, testDataInput, type = 'prob')[, 2]

  jpeg(paste(filename, '.jpg', sep = ""))
  plot(roc(expected, actual), print.auc = TRUE)
  mtext(plotTitle, side = 3)
  dev.off()
}

stringify_params <- function(params) {
  paste(
    "ntree=",
    params$ntree,
    ", mtry=",
    params$mtry,
    ", nodesize=",
    params$nodesize,
    ", maxnodes=",
    params$maxnodes,
    sep = ""
  )
}

splitData = splitData(clustered(loadData()))
trainDataInput = splitData$trainDataInput
trainDataResponse = splitData$trainDataResponse
testDataInput = splitData$testDataInput
testDataResponse = splitData$testDataResponse


best_params = find_parameters_with_genetic_algorithm(trainDataInput, trainDataResponse)
best_params_str = stringify_params(best_params)


optimized_model = build_model_with_params(trainDataInput, trainDataResponse, best_params)
default_model = build_model_default_params(trainDataInput, trainDataResponse)


plot_roc_analysis(optimized_model, testDataInput, testDataResponse, "optimized_model", best_params_str)
plot_roc_analysis(default_model, testDataInput, testDataResponse, "default_model", "Default random forest parameters")







