### Dobor parametrow
# C vs nu
svm.model <- svm(Species ~ ., data = iris, 
                 type = "nu-classification", nu = 0.2)

ksvm.model <- ksvm(Species ~ ., data = iris, 
                   type = "nu-svc", nu = 0.2)

s <- sigest(Species ~ ., data = iris[1:100,], scaled = FALSE)
k = rbfdot(sigma = mean(s[c(1,3)]))



### tune w svm
obj <- tune(svm, Species~., data = iris, 
            ranges = list(cost = 10^(-2:4), gamma = 10^(-4:2)),
            tunecontrol = tune.control(sampling = "cross", cross = 5))
plot(obj, transform.x = log10, , transform.y = log10, 
     xlab = "C", ylab = expression(log[10](gamma)))

print(obj$best.parameters)
svm.model.k <- svm(Species ~ ., data = iris, cross = 10, 
                   cost = obj$best.parameters$cost, 
                   gamma = obj$best.parameters$gamma)
svm.model.k$tot.accuracy
svm.model.k$tot.nSV

#### kpca w ksvm
require(mlbench)
require(scatterplot3d)
circles <- mlbench.circle(250)
plot(circles)

kpc <- kpca(x = circles$x, kernel = rbfdot(sigma = 1))

plot(rotated(kpc), col = as.integer(circles$classes), 
     xlab = "1st Principal Component",
     ylab = "2nd Principal Component")
       
scatterplot3d(rotated(kpc)[,1:min(3,ncol(rotated(kpc)))], 
              color = as.integer(circles$classes))

kpc <- kpca(x = circles$x, 
            kernel = polydot(degree = 2, scale = 1, offset = 0))
plot(rotated(kpc), col = as.integer(circles$classes), 
     xlab = "1st Principal Component",
     ylab = "2nd Principal Component")
scatterplot3d(rotated(kpc)[,1:max(3,ncol(rotated(kpc)))], 
              color = as.integer(circles$classes))

model.rbf <- ksvm(x = circles$x, y = circles$classes, 
                  kernel = rbfdot(sigma = 0.1), 
                  cross = 10)
model.poly <- ksvm(x = circles$x, y = circles$classes, 
                   kernel = polydot(degree = 2, scale = 1, offset = 0), 
                   cross = 10)
model.kpca <- ksvm(x = rotated(kpc), y = circles$classes, 
                   kernel = vanilladot(), 
                   cross = 10)
print(model.rbf)
print(model.poly)
print(model.kpca)


