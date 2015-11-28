### obrazowanie
# svm
svm.model.k <- svm(Species ~ ., data = iris, cross = 5, 
                   cost = 100, gamma = 0.01)
print(svm.model.k$tot.accuracy)

plot(svm.model.k, data = iris, Petal.Width ~ Petal.Length, 
     slice = list(Sepal.Width = mean(iris$Sepal.Width),
                  Sepal.Length = mean(iris$Sepal.Length)))

plot(svm.model.k, data = iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Petal.Width = mean(iris$Petal.Width),
                  Petal.Length = mean(iris$Petal.Length)))

plot(cmdscale(dist(iris[,-5])), 
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:nrow(iris) %in% svm.model.k$index + 1])

# ksvm
model.ksvm.k <- ksvm(Species ~ ., data = iris[1:100,], 
                     C = 100, kernel = rbfdot(0.1))
plot(model.ksvm.k) ## Tylko dla dwoch wymiarow

model.ksvm.k <- ksvm(Species ~ ., data = iris[1:100,c(1,2,5)], C = 100,
                     kernel = rbfdot(0.1))
plot(model.ksvm.k, data = iris[1:100,c(1,2,5)])

