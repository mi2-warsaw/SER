## Michal Ramsza
## Proste wykorzystanie pakietu nnet

## Wgranie koniecznych bibliotek
require(nnet)
require(caret)

## Funkcja do tworzenia danych
data.create <- function(range, coefs, n=100, s=1){
	x <- runif(n, min(range), max(range));
	y <- coefs[1];
	for(k in 2:length(coefs)){
		y <- coefs[k] * x^(k-1);
	}
	y <- y + rnorm(length(y), 0, s)
	data.frame(x=x, y=y)
}

## Przykladowe dane
data <- data.create(c(-1,1), c(0,0,1),s=.1)
plot(data, cex=.7, pch=3, 
	col=rgb(0,0,1,.2))

## Przykladowe zastosowanie funkcji
## nnet

## Estymacja malej sieci neuronowej
model.nn <- nnet( y ~ x,
    data = data,
    size = 1,
    decay = 0,
    linout = FALSE,
    skip = FALSE,
    maxit = 1000,
    Hess = TRUE);

## Test na minimum
ww <- eigen( model.nn$Hessian, TRUE)$values;
print( ww);

## Wyniki
data.predict <- data.frame(x=data$x,
	y=predict(model.nn, data))
	
## Wizualizacja
png(file="ex_01_1_fig_1.png")
plot(data, pch=20,
	xlab="", ylab="", col=rgb(1,0,0,.8))
points(data.predict, pch=3, cex=.7,
	col=rgb(0,0,1,.8))
dev.off()

## Estymacja malej sieci neuronowej
model.nn <- nnet( y ~ x,
    data = data,
    size = 2,
    decay = 0,
    lineout = FALSE,
    skip = FALSE,
    maxit = 1000,
    Hess = TRUE);

## Test na minimum
ww <- eigen( model.nn$Hessian, TRUE)$values;
print( ww);

## Wyniki
data.predict <- data.frame(x=data$x,
	y=predict(model.nn, data))
	
## Wizualizacja
png(file="ex_01_1_fig_2.png")
plot(data, pch=20,
	xlab="", ylab="", col=rgb(1,0,0,.8))
points(data.predict, pch=3, cex=.7,
	col=rgb(0,0,1,.8))
dev.off()


