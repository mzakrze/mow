library("ggplot2")
library("randomForest")

TRAIN_DATA_PERCENT = 85


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

data <- loadData()

drawNoClustered(data)

data = clustered(data)

drawClustered(data)

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

# TODO - coś jest chyba nie tak, bo 100% skuteczności xD
predict(randomForestResult, testDataInput) == testDataResponse