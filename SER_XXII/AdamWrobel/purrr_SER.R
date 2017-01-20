# install and load tidyverse
install.packages("tidyverse")
library(tidyverse)

# map function
# execute function iterativly over dimension of a given object

# use on a data.frame
df <- data.frame(a = 1:10, b = 11:20)
map(df, mean)

# use on a list
l <- list(a = 1:10, b = 11:20)
map(l, mean)

# use on a vector
vector <- c(a = 1, b = 2)
map(vector, mean)

# get results in a different form than a list:
map_dbl(l, mean)
map_lgl(l, is.numeric)
map_chr(l, typeof)

# lapply versions
vapply(l, mean, FUN.VALUE = double(1))
vapply(l, is.numeric, FUN.VALUE = logical(1))
vapply(l, typeof, FUN.VALUE = character(1))

# pass additonal arguments
map(l, quantile)
map(l, quantile, probs = 0.95)
sapply(l, quantile, probs = 0.95)
map_dbl(l, quantile, probs = 0.95)
vapply(l, quantile, probs = 0.95, FUN.VALUE = double(1))

# execute summary function on object list of data.frames (on iris example)
iris_list <- split(iris,iris$Species)
map(iris_list, summary)

# linear model example
data_list %>% map(head,3)
data_list %>% map(lm) %>% map(summary)
models <- map(data_list, lm)

# create a bigger sample
n_obs <- list(1000,1000,1000)
data_list2 <- map(n_obs,simulation)
models <- map(data_list2, lm)

# ploting function
ploting <- function(model_object,...){
  plot(y = model_object$fitted, x = model_object$model$y,...)
}

map(models, ploting)
map(models, ploting) %>% str


# walk, map and pipes
walk(models, ploting, col = 'cadetblue3')
map(data_list2, lm) %>% walk(ploting, col = 'chartreuse1') %>% map(summary)

# subsetting
map(data_list2, lm) %>% walk(ploting, col = 'chocolate1') %>% map(summary) %>% 
  map('coefficients')

# anonymous functions
setwd('D:/R_projects/Purrr - SER/data')
X11();par(mfrow = c(1,2))
dir() %>% walk(~plot(read.csv2(.)[1:80,'death_probability'],type = 'l'))

## mapping over many arguments

# one argument
n <- list(5,10,20)
map(n, rnorm)

# two arguments
means <- list(1,5,10)
map2(n,means, rnorm)

# p arguments
sds <- list(0.1,1,0.5)
simulation <- pmap(list(n = n, mean = means, sd = sds), rnorm)

## mapping with side effects - walk function
simulation <- pmap(list(n = list(100,100,100), mean = means, sd = sds), rnorm)
par(mfrow = c(1,3))
walk(simulation, hist)

# walking over multiple arguments
titles <- c('Normal(1, 0.1)', 'Normal(5, 1)', 'Normal(10,0.5)')
pwalk(list(x = simulation, main = titles), hist, xlab = '')

# walk, pipes and map
simulation %>% walk(hist, col = 'coral1') %>% map(summary)

pmap(list(n = list(100,100,100), mean = means, sd = sds), rnorm) %>%
  walk(hist, col = 'darkgoldenrod2') %>% map(summary)




