## Michal Ramsza
## Proste wykorzystanie pakietu nnet

## Wgranie koniecznych bibliotek
require(nnet)
require(caret)

## Funkcja do tworzenia danych
data.create <- function(range, c=1, n=100, s=1){
	x <- runif(n, min(range), max(range));
	x <- sort(x);
	y <- 0.5 + 0.5*sin(c*x)+rnorm(length(x), 0, s);
	data.frame(x=x, y=y)
}


## Podstawowa petla obliczen

## Zakres zmiennosc liczy neuronow
k.range <- 1:100

## Blad na probie testowej

err <- c();
err.r <- c();
k.step <- 1;

for(k in k.range){

## Tworzenie danych
data <- data.create(c(-1,1), c=10, s=.1, n=200)

## Podzial proby na uczaca i testowa
ind <- sort(createDataPartition(data$y, p=0.7, 
	list=FALSE))
data.train <- data[ind,]
data.test <- data[-ind,]

sd.test <- 0;
n.step <- 1;

while(sd.test<.1 & n.step<10){
	## Estymacja malej sieci neuronowej
	model.nn <- nnet( y ~ x,
    	data = data.train,
    	size = k,
    	decay = 0,
    	linout = FALSE,
    	skip = FALSE,
    	maxit = 10^4,
    	Hess = TRUE);
    	
    model.nr <- nnet( y ~ x,
    	data = data.train,
    	size = k,
    	decay = 0.001,
    	lineout = FALSE,
    	skip = FALSE,
    	maxit = 10^4,
    	Hess = TRUE);

	## Tworzenie predykcji na probie testowej
	data.predict.test <- data.frame(x=data.test$x,
		y=predict(model.nn, data.test))

	data.predict.test.r <- data.frame(x=data.test$x,
		y=predict(model.nr, data.test))
	
	## Sprawdzanie jakosci sieci
	sd.test<- sd(data.predict.test$y)
	
	## Zwiekszanie licznika krokow
	n.step <- n.step +1 
}

## Wizualizacja
png(file=paste("ex_05_5_fig_3_", 
	toString(k.step), ".png", sep=""))
plot(data.test, pch=3, cex=.7,
	col=rgb(0,0,1,.8))
points(data.predict.test,
	pch=20, col=rgb(1,0,0,.8))
lines(data.predict.test,
	col="red")
dev.off()

k.step <- k.step + 1;

## Obliczenie bledu na probie testowej
err <- c(err, 
	sum((data.predict.test$y - data.test$y)^2))
	
err.r <- c(err.r, 
	sum((data.predict.test.r$y - data.test$y)^2))
	
}

## Wizualizacja bledu
png(file="ex_05_5_fig_4.png")
plot(k.range, err,
	pch=20, col="blue")
lines(lowess(data.frame(k.range, err), f=.25),
	col="blue")
points(k.range, err.r,
	pch=3, cex=0.7, col="red")
lines(lowess(data.frame(k.range, err.r), f=.25),
	col="red")
dev.off()

## Wizualizacja wag
png(file="ex_05_5_fig_5.png")
plot(model.nn$wts, 
	pch=20, col="blue",
	ylab="")
points(model.nr$wts,
	pch=3, cex=.7, col="red")
dev.off()