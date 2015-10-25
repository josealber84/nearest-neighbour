# Let's try with real data!

library(RANN)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(genalg)

# Config
setwd("~/data/nearest-neighbour")
seed(22)

# Read data
train <- read.csv("data/train.csv", 
                  stringsAsFactors=FALSE)
store <- read.csv("data/store.csv", 
                  stringsAsFactors=FALSE)
test <- read.csv("data/test.csv", 
                 stringsAsFactors=FALSE)

# Re-structure data
all.training.data <- ReStructure(train, store, test)


# Change variable importance
modified.all.training.data <- all.training.data
modified.all.training.data$Open <- all.training.data$Open * 100000
all.training.data$CompetitionDistance <- all.training.data$CompetitionDistance / 1000
all.training.data$SchoolHoliday <- all.training.data$SchoolHoliday * 100000

# Create training and test set
index.training <- sample(x = 1:nrow(all.training.data), size = 1000000)
index.cv <- !(1:nrow(all.training.data) %in% index.training)
training.set <- all.training.data[index.training, ]
cv.set <- all.training.data[index.cv, ] %>% filter(Sales != 0)
incomplete.training <- training.set %>% select(-Sales)
incomplete.cv <- cv.set %>% select(-Sales)

# Launch world
model <- nn2(data = incomplete.training, query = incomplete.cv)

# Make prediction 
sales.prediction <- training.set[model$nn.idx[, 1], "Sales"]

# Calculate error
rmspe <- RMSPE(cv.set$Sales, sales.prediction)

# See results
model$nn.idx[1, ]
cv.set[1, ]
training.set[model$nn.idx[1, 1:3], ]

QUEDA PENDIENTE LA CONVERSIÓN A ALGORITMO GENÉTICO SIGUIENDO LA DEFINICION DE LA PIZARRA



