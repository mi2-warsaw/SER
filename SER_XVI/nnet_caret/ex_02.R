## Michal Ramsza
## Proste wykorzystanie pakietu nnet

## Wgranie koniecznych bibliotek
require(nnet)
require(caret)

## Funkcja do tworzenia danych
data.create <- function(range, coefs, n=100, s=1){
	x <- runif(n, min(range), max(range));
	x <- sort(x);
	y <- coefs[1];
	for(k in 2:length(coefs)){
		y <- coefs[k] * x^(k-1);
	}
	y <- y + rnorm(length(y), 0, s)
	data.frame(x=x, y=y)
}

## Przykladowe dane
data <- data.create(c(-1,1), c(0,0,1),s=.1)

## Podzial proby na uczaca i testowa
ind <- sort(createDataPartition(data$y, p=0.7, 
	list=FALSE))
data.train <- data[ind,]
data.test <- data[-ind,]

## Przykladowe zastosowanie funkcji
## nnet

## Estymacja malej sieci neuronowej
model.nn <- nnet( y ~ x,
    data = data.train,
    size = 200,
    decay = 0,
    linout = FALSE,
    skip = FALSE,
    maxit = 10^4,
    Hess = TRUE);

## Wyniki
data.predict.train <- data.frame(x=data.train$x,
	y=predict(model.nn, data.train)) 
data.predict.test <- data.frame(x=data.test$x,
	y=predict(model.nn, data.test))
	
## Wizualizacja
png(file="ex_02_2_fig_1.png")
plot(data.test, pch=20,
	xlab="", ylab="", col=rgb(1,0,0,.8),
	ylim=1.1*range(data.test$y))
points(data.predict.test, pch=3, cex=.7,
	col=rgb(0,0,1,.8))
lines(data.predict.test, col="blue")
dev.off()

png(file="ex_02_2_fig_2.png")
plot(data.train, pch=20,
	xlab="", ylab="", col=rgb(1,0,0,.8),
	ylim=1.1*range(data.train$y))
points(data.predict.train, pch=3, cex=.7,
	col=rgb(0,0,1,.8))
lines(data.predict.train, col="blue")
dev.off()

