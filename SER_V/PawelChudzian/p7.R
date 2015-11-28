## Literatura:
# A tutorial on support vector machines for pattern recognition. Christopher J. C. Burges. Data Mining and Knowledge Discovery, 2(2), 1998
# Support Vector Machines in R. Alexandros Karatzoglou, David Meyer. Journal of Statistical Software, 15(9), 2006


















## svmpath
library(svmpath)
train.idx <- sample(100,66)
test.idx <- setdiff(1:100, train.idx)
x <- as.matrix(iris[train.idx,1:4])
y <- (as.numeric(iris[train.idx,5])-1.5)*2
path.C <- svmpath(x, y, kernel = radial.kernel, param.kernel = mean(s[c(1,3)]))
plot(path.C)
mg <- predict(path.C, type = "margin")
al <- predict(path.C, type = "alpha")
pred <- predict(path.C, as.matrix(iris[test.idx,1:4]))







