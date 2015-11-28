### wiele klas & walidacja krzyzowa
summary(iris)

### svm: one-against-one
model.svm <- svm(Species ~ ., data = iris,
                 kernel = "radial", gamma = 0.05,
                 cost = 100, type = "C-classification",
                 cross = 5) # CV
paste("svm cv-error: ",round(100-model.svm$tot.accuracy,2))
paste("#SV: ",model.svm$tot.nSV)
table(pred = predict(object = model.svm, newdata = iris[,-class.idx]),
      true = iris[,class.idx])



### ksvm: one-against-one
model.ksvm <- ksvm(Species ~ ., data = iris,
                   kernel = rbfdot(sigma = 0.05),
                   C = 100, type = "C-svc",
                   cross = 5)
paste("ksvm cv-error: ",round(100*cross(model.ksvm),2))
paste("#SV: ",attr(model.ksvm,"nSV"))
table(pred = predict(object = model.ksvm, newdata = iris[,-class.idx]),
      true = iris[,class.idx])

### ksvm: spoc-svc & kbb-svc
model.ksvm <- ksvm(Species ~ ., data = iris,
                   kernel = rbfdot(sigma = 0.05),
                   C = 100, type = "spoc-svc",
                   cross = 5)
paste("ksvm cv-error: ",round(100*cross(model.ksvm),2))
paste("#SV: ",attr(model.ksvm,"nSV"))
table(pred = predict(object = model.ksvm, newdata = iris[,-class.idx]),
      true = iris[,class.idx])


### svmlight: one-against-one & one-against-all - x 1 LOO
table(data.multi2$class)
model.svmlight <- svmlight(class ~ ., data = data.multi2,
                           svm.options = "-z c -c 100 -t 2 -g 0.05 -v 0 -x 1",
                           pathsvm = 'svm_light_windows32/',
                           temp.dir = 'svm_light_windows32/tmp/',
                           class.type = "oao")
length(model.svmlight$svm.model)

model.svmlight <- svmlight(class ~ ., data = data.multi2,
                           svm.options = "-z c -c 100 -t 2 -g 0.05 -v 0 -x 1",
                           pathsvm = 'svm_light_windows32/',
                           temp.dir = 'svm_light_windows32/tmp/',
                           class.type = "oaa")
length(model.svmlight$svm.model)

# nie ma dostepu do LOO (-x 1)
pred <- predict(object = model.svmlight, newdata = data.multi2[,-1])
table(pred = pred$class, true = data.multi2[,1])

