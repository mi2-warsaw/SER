##
data <- load('data/radary2.Rdata')
data.bi <- dataobject[dataobject$class %in% c('Cessna','Mig-29'),]
data.bi$class <- as.factor(as.character(data.bi$class))
data.multi <- dataobject[dataobject$class %in% c('Cessna','Mi-8','Mig-29'),]
data.multi$class <- as.factor(as.character(data.multi$class))
data.multi2 <- dataobject[sample(nrow(dataobject),500),]


data(iris)
index     <- 1:nrow(iris)
testindex <- sample(index, round(length(index)/3))
testset   <- iris[testindex,]
trainset  <- iris[-testindex,]
class.idx <- which(colnames(iris) == "Species")

iris2 <- iris[1:100,]
iris2$Species <- as.factor(as.character(iris2$Species))


