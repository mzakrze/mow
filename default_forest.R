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
  
  trainDataIndices = sample(nrow(data), nrow(data) * 15 / 100)
  
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


####################################3

allData <- loadData()

allDataClustered = clustered(allData)

splitData = splitData(allDataClustered)

trainDataInput = splitData$trainDataInput
trainDataResponse = splitData$trainDataResponse
testDataInput = splitData$testDataInput
testDataResponse = splitData$testDataResponse

randomForestResult = randomForest(
	x = trainDataInput,
	y = trainDataResponse,
	# brak wskazanych parametrów lasu, więc będą domyślne
	#ntree = randomForest.ntree,
	#mtry = randomForest.mtry,
	#replace = randomForest.replace,
	#nodesize = randomForest.nodesize,
	#maxnodes = randomForest.maxnodes,
	keep.forest = TRUE
)

expected = testDataResponse
actual = predict(randomForestResult, testDataInput, type = 'prob')[, 2]

plot(roc(expected, actual), print.auc = TRUE)

jpeg('adhoc_roc_analysis.jpg')
plot(roc(expected, actual), print.auc = TRUE)
mtext("Default random forest parameters", side = 3)
dev.off()