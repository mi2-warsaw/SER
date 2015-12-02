library(mirt)
library(polycor)

# sumulacja - rozkłady sum ------------------------------------------------
# parametry symulacji:
a  = matrix(rep( 1, 30), ncol=1)  # generujemy dane pod Raschem
N = 10^4  # dużo "osób", żeby było wiadomo, że wyniki będą stabilne
# różne zestawy łatwości/trudności zadań:
d1 = matrix(rnorm(30,  0  , 1  ), ncol=1)  # nieźle dobrane trudności zadań
d2 = matrix(c(
	rnorm(15,  1.5, 0.5),
	rnorm(15, -1.5, 0.5)
), ncol=1)  # tylko bardzo proste i bardzo trudne zadania
d3 = matrix(rnorm(30,  0  , 0.25), ncol=1)  # gwarantcja "sufitu" i "podłogi"
d4 = matrix(rnorm(30,  1.5, 0.5 ), ncol=1)  # tylko bardzo proste zadania
d5 = matrix(rnorm(30, -1.5, 0.5 ), ncol=1)  # tylko bardzo trudne zadania
# rozpatrzmy trzy różne rozkłady cechy ukrytej:
rozkladyGenerujace =list(
	norm  = matrix(scale(rnorm (N)), ncol=1),  # normalny
	lnorm = matrix(scale(rlnorm(N)), ncol=1),  # log-normalny
	unif  = matrix(scale(runif (N)), ncol=1)   # jednostajny
)
# funkcja symulująca "rozwiązywanie testu" i wyliczająca sumy punktów
symuluj = function(x, a, d, N, itemtype) {
	x=simdata(a, d, N, itemtype, Theta=x)
	return(apply(x, 1, sum))
}
# i samo symulowanie
rozkladySum1 = lapply(rozkladyGenerujace, symuluj, a=a, d=d1, N=N, itemtype="dich")
rozkladySum2 = lapply(rozkladyGenerujace, symuluj, a=a, d=d2, N=N, itemtype="dich")
rozkladySum3 = lapply(rozkladyGenerujace, symuluj, a=a, d=d3, N=N, itemtype="dich")
rozkladySum4 = lapply(rozkladyGenerujace, symuluj, a=a, d=d4, N=N, itemtype="dich")
rozkladySum5 = lapply(rozkladyGenerujace, symuluj, a=a, d=d5, N=N, itemtype="dich")
# funkcja rysująca rozkłady generujące i rozkłady sum
rysujRozklady = function(x, rozkladyGenerujace, d) {
	for (i in 1:length(rozkladyGenerujace)) {
		plot(density(rozkladyGenerujace[[i]]), main=paste0("rozkład generujący: ", names(rozkladyGenerujace)[i]), xlim=c(-5, 5), lwd=2, col=hsv(0,1,0.8))
		grid(col=grey(0.5))
		rug(-d, col=hsv(0,1,0.8))
		hist(x[[i]], 0:length(d), main=paste0("rozkład sumy punktów"), xlab="suma", xlim=c(0, length(d)), col=hsv(0,1,0.8))
		grid(col=grey(0.5))
	}
	invisible(0)
}
# i samo rysowanie
layout(matrix(1:6, nrow=2))
rysujRozklady(rozkladySum1, rozkladyGenerujace, d1)
rysujRozklady(rozkladySum2, rozkladyGenerujace, d2)
rysujRozklady(rozkladySum3, rozkladyGenerujace, d3)
rysujRozklady(rozkladySum4, rozkladyGenerujace, d4)
rysujRozklady(rozkladySum5, rozkladyGenerujace, d5)
layout(1)

# skalowanie wzrostu ------------------------------------------------------
# wczytywanie danych
setwd("")
dane=read.csv2("SER3psycho.csv", na.strings="")

# przekodujmy na 0-1 z odwróceniem pytań zadanych w formie "negatywnej"
#dane=na.omit(dane)
for (i in grep("_n$", names(dane))) {
	dane[, i] = relevel(dane[, i], "Tak")
}
maska = grep("^p[[:digit:]]", names(dane))
dane01 = dane
dane01[, maska] =
	lapply(dane[, maska], function(x) {return(as.numeric(x) - 1)})

# zobaczmy nasze dane
summary(dane01)
(trudnosciOds=sort(1-sapply(dane01[, maska], mean, na.rm=TRUE)))
dotchart(trudnosciOds, xlim=c(0, 1), pch=16)
grid()
hist(dane01$wzrost, 40, xlim=c(155, 195))
#hist(dane01$rozmiar_buta, 12, xlim=c(36, 48))
# korelacje
(korelacje=cor(dane01, use="pairwise.complete.obs"))

# skalujemy
maska1 = grep("^p[[:digit:]]", names(dane01))
(m1R = mirt(dane01[, maska1], model=1, itemtype="Rasch"))
(m1  = mirt(dane01[, maska1], model=1, itemtype="2PL"  ))

# zobaczmy, jak z parametrami modeli
dyskryminacjeM1R = unlist(lapply(coef(m1R), function(x) {return(x[1, colnames(x) == "a1"])}))
dyskryminacjeM1  = unlist(lapply(coef(m1 ), function(x) {return(x[1, colnames(x) == "a1"])}))
trudnosciM1R = unlist(lapply(coef(m1R, IRTpars=TRUE), function(x) {return(x[1, colnames(x) == "b"])}))
trudnosciM1  = unlist(lapply(coef(m1 , IRTpars=TRUE), function(x) {return(x[1, colnames(x) == "b"])}))

plot(trudnosciM1R, dyskryminacjeM1R, pch=16)
grid()
kolejnoscM1R = order(trudnosciM1R)
text(trudnosciM1R[kolejnoscM1R], dyskryminacjeM1R[kolejnoscM1R], sub("_.$", "", names(dyskryminacjeM1R))[kolejnoscM1R], pos=rep(c(1,3), 6), cex=0.7)
plot(trudnosciM1 , dyskryminacjeM1 , pch=16)
grid()
kolejnoscM1 = order(trudnosciM1)
text(trudnosciM1[kolejnoscM1], dyskryminacjeM1[kolejnoscM1], sub("_.$", "", names(dyskryminacjeM1))[kolejnoscM1], pos=rep(c(3,1), 6), cex=0.7)

itemfit(m1R)
itemfit(m1R, group.size=20, empirical.plot=6)
itemfit(m1R, group.size=20, empirical.plot=7)
itemfit(m1)
itemfit(m1 , group.size=20, empirical.plot=6)
itemfit(m1 , group.size=20, empirical.plot=5)
itemfit(m1 , group.size=20, empirical.plot=6)

# wyrzućmy pytanie 3.
maska2 = grep("^p([12456789]|1[012])_", names(dane01))
(m2R = mirt(dane01[, maska2], model=1, itemtype="Rasch"))
(m2  = mirt(dane01[, maska2], model=1, itemtype="2PL"  ))

dyskryminacjeM2R = unlist(lapply(coef(m2R), function(x) {return(x[1, colnames(x) == "a1"])}))
dyskryminacjeM2  = unlist(lapply(coef(m2 ), function(x) {return(x[1, colnames(x) == "a1"])}))
trudnosciM2R = unlist(lapply(coef(m2R, IRTpars=TRUE), function(x) {return(x[1, colnames(x) == "b"])}))
trudnosciM2  = unlist(lapply(coef(m2 , IRTpars=TRUE), function(x) {return(x[1, colnames(x) == "b"])}))

plot(trudnosciM2R, dyskryminacjeM2R, pch=16)
grid()
kolejnoscM2R = order(trudnosciM2R)
text(trudnosciM2R[kolejnoscM2R], dyskryminacjeM1R[kolejnoscM2R], sub("_.$", "", names(dyskryminacjeM2R))[kolejnoscM2R], pos=rep(c(1,3), 6), cex=0.7)
plot(trudnosciM2 , dyskryminacjeM2 , pch=16)
grid()
kolejnoscM2 = order(trudnosciM2)
text(trudnosciM2[kolejnoscM2], dyskryminacjeM2[kolejnoscM2], sub("_.$", "", names(dyskryminacjeM2))[kolejnoscM2], pos=rep(c(3,1), 6), cex=0.7)

# oszacowania wzrostu
oszacowania=cbind(
	wzrost = dane01$wzrost,
	suma1 = apply(dane01[, maska1], 1, mean, na.rm=TRUE) * length(maska1),
	suma2 = apply(dane01[, maska2], 1, mean, na.rm=TRUE) * length(maska2),
	m1  = fscores(m1 , full.scores=TRUE, scores.only=TRUE)[, 1],
	m1R = fscores(m1R, full.scores=TRUE, scores.only=TRUE)[, 1],
	m2  = fscores(m2 , full.scores=TRUE, scores.only=TRUE)[, 1],
	m2R = fscores(m2R, full.scores=TRUE, scores.only=TRUE)[, 1]
)
cor(oszacowania)
panel = function(x, y) {
	points(x, y, pch=16)
	grid(col=grey(0.5))
	abline(lm(y~x),col="red")
	legend("bottomright", legend=paste0("r=", format(cor(x, y), digit=3, nsmall=3)), bty="n", text.font=2, cex=1.2)
	invisible(0)
}	
pairs(oszacowania[, grep("wzrost|1", colnames(oszacowania))], lower.panel=panel, upper.panel=NULL)

# wyliczmy błąd w centymetrach
bladM1R = with(as.data.frame(oszacowania), {
	scale(m1R) * sd(wzrost) + mean(wzrost) - wzrost
})
summary(bladM1R)
mean(bladM1R^2)
mean(bladM1R^2)^0.5

