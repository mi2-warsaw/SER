#### czasy
mb.iris.rbf <- microbenchmark(
  svm(Species ~ ., data = iris, kernel = "radial", gamma = 0.05, cost = 100, type = "C-classification"),
  ksvm(Species ~ ., data = iris, kernel = rbfdot(sigma = 0.05), C = 100, type = "C-svc"),
  svmlight(Species ~ ., data = iris, svm.options = "-z c -c 100 -t 2 -g 0.05 -v 0", pathsvm = '~/Dropbox/ser/svmlight'),
  times = 100
)

svm       4.250468
ksvm      10.295731
svmlight  37.735141

dim(data.multi)
mb.radar.rbf <- microbenchmark(
  svm(class ~ ., data = data.multi, kernel = "radial", gamma = 0.05, cost = 100, type = "C-classification"),
  ksvm(class ~ ., data = data.multi, kernel = rbfdot(sigma = 0.05), C = 100, type = "C-svc"),
  svmlight(class ~ ., data = data.multi, svm.options = "-z c -c 100 -t 2 -g 0.05 -v 0", pathsvm = '~/Dropbox/ser/svmlight'),
  times = 100
)

svm       419.9100
ksvm      337.3193
svmlight  1609.2876

mb.radar.poly <- microbenchmark(
  svm(class ~ ., data = data.multi, kernel = "poly", degree = 3, gamma = 1, coef0 = 1, cost = 1, type = "C-classification"),
  ksvm(class ~ ., data = data.multi, kernel = polydot(3, 1, 1), C = 1, type = "C-svc"),
  svmlight(class ~ ., data = data.multi, svm.options = "-z c -c 1 -t 1 -d 3 -s 1 -c 1 -v 0", pathsvm = '~/Dropbox/ser/svmlight'),
  times = 100
)

svm       279.1787
ksvm      195.6544
svmlight  6812.1471

