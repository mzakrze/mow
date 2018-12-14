library("ggplot2")

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


