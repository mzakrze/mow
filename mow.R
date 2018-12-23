library("ggplot2")
library("randomForest")
library("pROC")
library(ROCR)









readParams <- function(){

    args <- commandArgs(trailingOnly = TRUE)

    operation = NULL
    save_flag = FALSE
    i <- 1
    while( i <= length(args)) {
        if("-o" == args[i] || "--operation" == args[i]){
            if(args[i + 1] == Operation$SINGLE_RUN) {
                operation = Operation$SINGLE_RUN
            } else if(args[i + 1] == Operation$SEARCH_PARAMS) {
                operation = Operation$SEARCH_PARAMS
            } else {
                stop(paste("Invalid '--operation' argument: ", args[i + 1]))
            }
            i = i + 2
            next
        }

        if("-s" == args[i] || '--save' == args[i]) {
            save_flag = TRUE
            i = i + 1
            next
        }

        stop(paste("Dont understand command ", toString(args[i]), ". Exiting..."))
    }

    if(is.null(operation)){
        stop("No 'operation' argument given")
    }

    list(operation = operation, save_flag = save_flag)
}


loadConfig <- function(){
    checkTuningParam <- function(var_name, var) {
        if(!exists(var_name) || is.null(var)) {
            stop(paste("Invalid config file for variable: ", var_name))
        } 
        if(params$operation == Operation$SINGLE_RUN && length(var) > 1){
            stop(paste("Illegal value of ", var_name, " for 'single_run' mode"))
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

    ###
    #  Loading config file
    ###
    
    if(params$operation == Operation$SINGLE_RUN) {
      source('config_single_run.R')
    }
    if(params$operation == Operation$SEARCH_PARAMS){
      source('config_search_params.R')
    }

    check("train_data_percent", train_data_percent)
    check("repeat_each", repeat_each)
    checkTuningParam("randomForest.mtry", randomForest.mtry)
    checkTuningParam("randomForest.ntree", randomForest.ntree)
    checkTuningParam("randomForest.replace", randomForest.replace)
    checkTuningParam("randomForest.maxnodes", randomForest.maxnodes)
    checkTuningParam("randomForest.nodesize", randomForest.nodesize)
}


loadData <- function() {
    # TODO - chyba wypada połączyć 2 tabele ale copy-paste nie działa
    d1=read.table("./student-alcohol-consumption/student-mat.csv",sep=",",header=TRUE)
    d1
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
    jpeg(paste(result_folder_name, '/clustered.jpg', sep=""))
    scatterplot
}


runSingleRunMode <- function(splitData) {
    trainDataInput = splitData$trainDataInput
    trainDataResponse = splitData$trainDataResponse
    testDataInput = splitData$testDataInput
    testDataResponse = splitData$testDataResponse

    randomForestResult = randomForest(
        x = trainDataInput, 
        y = trainDataResponse, 
        ntree = randomForest.ntree, 
        mtry = randomForest.mtry, 
        replace = randomForest.replace, 
        nodesize = randomForest.nodesize, 
        maxnodes = randomForest.maxnodes,
        keep.forest=TRUE)
    stopifnot(randomForestResult$type == 'classification')

    expected = testDataResponse
    actual = predict(randomForestResult, testDataInput, type = 'prob')[, 2]

    rf_params = paste("ntree=", randomForest.ntree, ", mtry=", randomForest.mtry, ", nodesize=", randomForest.nodesize, ", maxnodes=", randomForest.maxnodes, sep="")

    jpeg(paste(result_folder_name, '/roc_analysis.jpg', sep=""))
    plot(roc(expected, actual), print.auc=TRUE)
    mtext(rf_params, side=3)
    dev.off()

    jpeg(paste(result_folder_name, '/importance_test.jpg', sep=""))
    varImpPlot(randomForestResult)
    mtext(rf_params, side=3)
    dev.off()
}


runSearchParamsMode <- function(splitData) {
    trainDataInput = splitData$trainDataInput
    trainDataResponse = splitData$trainDataResponse
    testDataInput = splitData$testDataInput
    testDataResponse = splitData$testDataResponse
    
    best_auc = 0
    best_params = list(mtry=NULL, ntree=NULL,replace=NULL,maxnodes=NULL,nodesize=NULL)
    for(mtry in randomForest.mtry) {
        for(ntree in randomForest.ntree) {
            for(replace in randomForest.replace) {
                for(maxnodes in randomForest.maxnodes) {
                    for(nodesize in randomForest.nodesize) {
                        randomForestResult = randomForest(
                            x = trainDataInput, 
                            y = trainDataResponse, 
                            ntree = ntree, 
                            mtry = mtry, 
                            replace = replace, 
                            nodesize = nodesize, 
                            maxnodes = maxnodes,
                            keep.forest=TRUE)
                        stopifnot(randomForestResult$type == 'classification')

                        expected = testDataResponse
                        actual = predict(randomForestResult, testDataInput, type = 'prob')[, 2]

                        pred = prediction(actual, expected)

                        perf = performance(pred,"auc") 

                        auc = perf@y.values[[1]]

                        if(best_auc < auc) {
                            best_auc = auc
                            best_params = list(mtry=mtry, ntree=ntree,replace=replace,maxnodes=maxnodes,nodesize=nodesize)
                            print(paste('new best:', best_auc))
                        }
                    }
                }
            }
        }
    }
}


saveConfigToFile <- function() {
    config_filename <- if(params$operation == Operation$SINGLE_RUN) {
      'config_single_run.R'
    } else if(params$operation == Operation$SEARCH_PARAMS){
      'config_search_params.R'
    }
    file.copy(from=config_filename, to=result_folder_name, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
}


splitData <- function(allData) {
    data = allData[, -1 * grep("Dalc", colnames(allData))]
    data = data[, -1 * grep("Walc", colnames(data))]

    trainDataIndices = sample(nrow(data), nrow(data) * train_data_percent / 100)

    trainData = data[trainDataIndices, ]
    trainDataResponse = trainData[, grep("is_alcoholic", colnames(trainData))]
    trainDataInput = trainData[, -1 * grep("is_alcoholic", colnames(trainData))]

    testData = data[-1 * trainDataIndices, ]
    testDataResponse = testData[, grep("is_alcoholic", colnames(testData))]
    testDataInput = testData[, -1 * grep("is_alcoholic", colnames(testData))]

    list(trainDataInput = trainDataInput, trainDataResponse = trainDataResponse, testDataInput = testDataInput, testDataResponse = testDataResponse)
}


createFolder <- function(folder_name){
    dir.create(file.path(getwd(), folder_name), showWarnings = FALSE)
}


resultFolderName <- function() { 
    gsub(" ", "-", paste("build/", params$operation, Sys.time(), sep=""))
}



##################################################################
###############      Main Function               #################
##################################################################

Operation = list(SINGLE_RUN = "single_run", SEARCH_PARAMS = "search_params")

params <- readParams() # TODO - odznaczyć załadowane w ten sposób zmienne od "reszty"
loadConfig()

result_folder_name = resultFolderName()

createFolder(result_folder_name)

saveConfigToFile()

allData <- loadData()

allDataClustered = clustered(allData)

drawClustered(allDataClustered)

splitData = splitData(allDataClustered)

# FIXME
# Zwracam uwagę, że ze względu na losowość nie można porównywać wyników pojedynczego uruchomienia lasu losowego.
# pewnie trzeba odpalić kilka(powiedzmy 10) razy i AUC uśrednić, ale nie wiem na pewno

if(params$operation == Operation$SINGLE_RUN) {
    runSingleRunMode(splitData)
} else if(params$operation == Operation$SEARCH_PARAMS) {
    runSearchParamsMode(splitData)
} else {
    stop("Invalid operation")
}
