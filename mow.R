library("ggplot2")
library("randomForest")
library("pROC")

TRAIN_DATA_PERCENT = 85

Operation = list(RUN_SINGLE = "run_single", SEARCH_PARAMS = "search_params")
Save = list(YES = TRUE, NO = FALSE)

args <- commandArgs(trailingOnly = TRUE)

operation = NULL
save_flag = FALSE
i <- 1
while( i <= length(args)) {
    if("-o" == args[i] || "--operation" == args[i]){
        if(args[i + 1] == Operation$RUN_SINGLE) {
            operation = Operation$RUN_SINGLE
        } else if(args[i + 1] == Operation$SEARCH_PARAMS) {
            operation = Operation$SEARCH_PARAMS
        } else {
            stop(paste("Invalid '--operation' argument: ", args[i + 1]))
        }
        i = i + 2
        next
    }

    if("-s" == args[i] || '--save' == args[i]) {
        save_flag = Save$YES
        i = i + 1
        next
    }

    stop(paste("Dont understand command ", toString(args[i]), ". Exiting..."))
}

assertConfigOk <- function(){
    checkTuningParam <- function(var_name, var) {
        if(!exists(var_name) || is.null(var)) {
            stop(paste("Invalid config file for variable: ", var_name))
        } 
        if(operation == Operation$RUN_SINGLE && length(vec) > 1){
            stop(paste("Illegal value of ", name, " for 'run_single' mode"))
        }
    }
    check <- function(var_name, var) {
        if(!exists(var_name) || is.null(var)) {
            stop(paste("Invalid config file for variable: ", var_name))
        } 
        if(length(var) != 1) {
            stop(paste("Invalid config file for variable: ", var_name))
        }
    }
    check("train_data_percent", train_data_percent)
    check("repeat_each", repeat_each)
    checkTuningParam("randomForest.mtry", randomForest.mtry)
    checkTuningParam("randomForest.replace", randomForest.replace)
    checkTuningParam("randomForest.maxnodes", randomForest.maxnodes)
    checkTuningParam("randomForest.nodesize", randomForest.nodesize)
}

source('config.R')
assertConfigOk()

loadData <- function() {
    # TODO - chyba wypada połączyć 2 tabele ale copy-paste nie działa
    d1=read.table("../student-alcohol-consumption/student-mat.csv",sep=",",header=TRUE)
    d1
}

drawNoClustered <- function(data) {
    scatterplot <- ggplot(data = data, aes(x = Dalc, y = Walc)) + geom_jitter()
    jpeg('build/unclustered.jpg')
    scatterplot
}

clustered <- function(data) {
    matrix = cbind(as.numeric(data$Walc), as.numeric(data$Dalc))
    # TODO - wydaje się, że powinienem ustawić centra jako c(1,1), c(5,5). Ale wtedy otrzymuje error "initial centers are not distinct" 
    centers = cbind(c(1, 1), c(4,  5))
    k_means = kmeans(matrix, centers, iter.max = 100, trace=FALSE)
    data$is_alcoholic = k_means$cluster == 2
    data$is_alcoholic = as.factor(data$is_alcoholic)
    data
}

drawClustered <- function(data){
    scatterplot <- ggplot(data = data, aes(x = Dalc, y = Walc, color = is_alcoholic)) + geom_jitter()
    jpeg('build/clustered.jpg')
    scatterplot
}

allData <- loadData()

drawNoClustered(allData)

allData = clustered(allData)

drawClustered(allData)

data = allData[, -1 * grep("Dalc", colnames(allData))]
data = data[, -1 * grep("Walc", colnames(data))]

trainDataIndices = sample(nrow(data), nrow(data) * TRAIN_DATA_PERCENT / 100)

trainData = data[trainDataIndices, ]
trainDataResponse = trainData[, grep("is_alcoholic", colnames(trainData))]
trainDataInput = trainData[, -1 * grep("is_alcoholic", colnames(trainData))]

testData = data[-1 * trainDataIndices, ]
testDataResponse = testData[, grep("is_alcoholic", colnames(testData))]
testDataInput = testData[, -1 * grep("is_alcoholic", colnames(testData))]

randomForestResult = randomForest(
    x = trainDataInput, 
    y = trainDataResponse, 
    ntree = 83, 
    mtry = 9, 
    replace = TRUE, 
    nodesize = 10, 
    maxnodes = 17,
    keep.forest=TRUE)

stopifnot(randomForestResult$type == 'classification')

str(randomForestResult)

expected = testDataResponse
actual = predict(randomForestResult, testDataInput, type = 'prob')[, 2]

jpeg('build/roc_analysis.jpg')
plot(roc(expected, actual), print.auc=TRUE)
