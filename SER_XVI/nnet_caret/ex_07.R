## Michal Ramsza
## Proste wykorzystanie pakietu nnet
## i caret do automatycznej budowy modelu

## Wgranie koniecznych bibliotek
require(nnet)
require(caret)
# require(doMC)
# registerDoMC(2)

## Funkcja do tworzenia danych
data.create <- function(range, c=1, n=100, s=1){
	x <- runif(n, min(range), max(range));
	x <- sort(x);
	y <- 0.5 + 0.5*sin(c*x)+rnorm(length(x), 0, s);
	data.frame(x=x, y=y)
}

## Tworzenie danych
data <- data.create(c(-1,1), c=10, s=.1, n=200)
ind <- createDataPartition(data$y, p=.7, list=FALSE)
data.train <- data[ind,]
data.test <- data[-ind,]

## Tworzenie zakresu parametrow
## definiujacych siec neuronowa

new.grid <- expand.grid(.decay=seq(0,.1, length.out=5),
	.size=2:20)

## Automatyczne dobieranie struktury
## sieci neuronowej z kontrola metody
ctrl <- trainControl(method = "boot",
	verboseIter=TRUE)
	
data.fit <- train( y ~ x, 
	data=data.train,
	method="nnet",
	trControl=ctrl,
	tuneGrid=new.grid,
	trace=FALSE,
	maxit=10^3,
	linout=TRUE)

## Zachowanie nauczonej sieci na
## zbiorze testowym
data.pred <- predict(data.fit, data.test)

## Wizualizacja proby testowej i predykcji
png(file="ex_07_8_fig_1.png")
plot(data.test,
     pch=20, col=rgb(0,0,1,.8))
lines( data.test, col="blue")
points(data.test$x, data.pred,
       pch=3,
       col=rgb(1, 0, 0, .8))
lines( data.test$x, data.pred,
      col="red")
dev.off()

## Wizualizacja proby uczacej
png(file="ex_07_8_fig_2.png")
plot(data.train,
     pch=20, col=rgb(0,0,1,.8))
lines( data.train, col="blue")
dev.off()
