### kernel

### svm: liniowe, wielomianowe, radialne, sigmoidalne
model.svm.linear <- svm(Species ~ ., data = iris, 
                        cost = 100, cross = 5, 
                        kernel = "linear")
model.svm.linear$tot.accuracy

model.svm.poly <- svm(Species ~ ., data = iris, 
                      cost = 100, cross = 5, 
                      kernel = "poly", gamma = 1,
                      coef0 = 0, degree = 3)
model.svm.poly$tot.accuracy

model.svm.radial <- svm(Species ~ ., data = iris, 
                        cost = 100, cross = 5, 
                        kernel = "radial", gamma = 0.1)
model.svm.radial$tot.accuracy
## w svm nie ma mozliwosci zdefiniowania wlasnego!

### w ksvm: liniowe, wielomianowe, radialne i wiele innych 
# tak:
model.ksvm.radial <- ksvm(Species ~ ., data = iris, 
                          C = 100, cross = 5, 
                          kernel = "rbfdot", kpar = list(sigma = 0.1))

# lub tak:
model.ksvm.radial <- ksvm(Species ~ ., data = iris, 
                          C = 100, cross = 5, 
                          kernel = rbfdot(sigma = 0.1))

# przyklad wlasnej definicji
k <- function(x, y) {(sum(x * y))}
class(k) <- "kernel"

# tak
model.ksvm.custom <- ksvm(Species ~ ., data = iris, 
                          C = 100, cross = 5, 
                          kernel = k) # CZAS!


# lub tak: 
iris.mat <- as.matrix(iris[,1:4])
km.custom <- kernelMatrix(kernel = k, iris.mat)
model.ksvm.custom.2 <- ksvm(km.custom, y = iris[,5],
                            C = 100, cross = 5) # CZAS!

# albo tak:
cp <- crossprod(t(as.matrix(iris[,1:4])))
km.cp <- as.kernelMatrix(cp)
model.ksvm.custom.3 <- ksvm(km.cp, y = iris[,5],
                            C = 100, cross = 5) # CZAS!


#Liniowe i liniowe-vanilladot - inny algorytm optymalizacji
k(c(1,2,3),c(4,5,6))
vanilladot()(c(1,2,3),c(4,5,6))

model.ksvm.linear <- ksvm(Species ~ ., data = iris, 
                          C = 100, cross = 5, 
                          kernel = vanilladot())

print(model.ksvm.linear)
print(model.ksvm.custom)

