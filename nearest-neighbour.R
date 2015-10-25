# Let's try with real data!

library(RANN)
library(dplyr)
library(ggplot2)

training.index <- sample(x = 1:150, size = 130)
test.index <- which(!(1:150 %in% training.index))

training.set <- iris[training.index, ]
test.set <- iris[test.index, ]
incomplete.test <- test.set %>% select(-Species)

ggplot(data = training.set) +
    geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width, 
                             color = Species)) +
    geom_point(data = incomplete.test, mapping = aes(x = Sepal.Length, 
                                                     y = Sepal.Width))

model <- nn2(data = training.set, query = incomplete.test)

