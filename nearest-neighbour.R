# Let's try with real data!
#
# Chromosome = [I1 I2 I3 ... IN N F]
#
#

library(RANN)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(genalg)

source("genalg/plot.rbga.R")
source("genalg/rbga.bin.R")
source("genalg/rbga.R")
source("genalg/summary.rbga.R")
source("functions.R")

# Config
setwd("~/data/nearest-neighbour")
set.seed(22)

# Read data
train <- read.csv("data/train.csv", 
                  stringsAsFactors=FALSE)
store <- read.csv("data/store.csv", 
                  stringsAsFactors=FALSE)
test <- read.csv("data/test.csv", 
                 stringsAsFactors=FALSE)

# Re-structure data
all.training.data <- ReStructure(train, store, test)
all.training.data <- Normalize(all.training.data)

# Create training and test set
index.training <- sample(x = 1:nrow(all.training.data), size = 1000000)
index.cv <- !(1:nrow(all.training.data) %in% index.training)

# Launch world
min.values <- c(rep(0, ncol(all.training.data) - 1), 1, 1)
max.values <- c(rep(100, ncol(all.training.data) - 1), 3, 2)
world <- rbga(stringMin = min.values, stringMax = max.values, popSize = 50,
              iters = 100, verbose = TRUE, evalFunc = Evaluate, 
              mutationChance = 0.25,
              data = all.training.data, index.training = index.training,
              index.cv = index.cv)



