## e1071 (libsvm)
## kernlab (libsvm & bsvm)
## klaR (SVMlight)
library(e1071)
library(kernlab)
library(klaR)

### interfejs i zadanie binarne
summary(iris2)

### svm (e1071)
model.svm <- svm(Species ~ ., data = iris2,
                 kernel = "radial", gamma = 0.05,
                 cost = 100, 
                 type = "C-classification", scale = TRUE)
summary(model.svm)
names(model.svm)

pred <- predict(model.svm, newdata = iris2)
head(pred)

svm.model <- svm(Species ~ ., data = iris2,  probability = TRUE)
pred <- predict(object = svm.model, newdata = iris2[,-class.idx], 
                probability = T)
head(attr(pred, "probabilities"))



### ksvm (kernlab)
model.ksvm <- ksvm(Species ~ ., data = iris2,
                   kernel = rbfdot(sigma = 0.05),
                   C = 100, 
                   type = "C-svc", scale = T)
print(model.ksvm)
names(attributes(model.ksvm))
pred <- predict(model.ksvm, newdata = iris2)
head(pred)

ksvm.model <- ksvm(Species ~ ., data = iris2,  prob.model = TRUE)
head(predict(object = ksvm.model, newdata = iris2[,-class.idx], 
             type = 'probabilities'))



### svmlight (klaR)
model.svmlight <- svmlight(Species ~ ., data = iris2,
                           svm.options = "-z c -c 100 -t 2 -g 0.05 -v 0",
                           pathsvm = 'svm_light_windows32/',
                           temp.dir = 'svm_light_windows32/tmp/',
                           del = FALSE)
names(model.svmlight)
length(model.svmlight$svm.model)
model.svmlight$svm.model[[1]]
pred <- predict(model.svmlight, newdata = iris2)
str(pred)
head(pred$class)
head(pred$posterior)

