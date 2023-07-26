m(list = ls())
library(caret)
library(tidyverse)
library(foreign)
dung <- read.spss("F:/tkdb/bankloan.sav", to.data.frame = TRUE)
sub_data <- dung %>% 
  filter(!is.na(default)) %>% 
  select(-preddef1, -preddef2, -preddef3, -ed)

sub_data %>% str()

set.seed(123)
id <- createDataPartition(sub_data$default, p = 0.5, list = FALSE)
train <- sub_data[id, ]
test <- sub_data[-id, ]


# Grid of tuning parameters to try:
fitGrid <- expand.grid(.size = c(5, 10, 15, 20), 
                       .decay = c(0.001, 0.01, 0.1))

# Set the seeds for using parallel processing: 
set.seed(1)
seeds <- vector(mode = "list", length = 11) # number of resamples + 1 for final model
for(i in 1:10) seeds[[i]] <- sample.int(n = 1000, 12) #  12 is the number of tuning parameter combinations
seeds[[11]] <- 1 # for the last model
seeds

library(doParallel)
# Find out how many cores are available: 
detectCores()
## [1] 4
# Create cluster with desired number of cores: 
cl <- makeCluster(4)
# Register cluster: 
registerDoParallel(cl)
# Find out how many cores are being used
getDoParWorkers()
## [1] 4
# Fit model using ANN: 
set.seed(1)
model.ct.nn <- train(default ~ .,
                     data = train,
                     method = "nnet",
                     maxit = 1000,
                     linout = FALSE,
                     trControl = fitControl,
                     tuneGrid = fitGrid,
                     trace = FALSE,
                     #metric = "Sens", # maximize sensitivity to "Yes" values
                     allowParallel = TRUE)

stopCluster(cl)
registerDoSEQ()

model.ct.nn

rf.Grid <- expand.grid(mtry = seq(from = 3, to = 18, by = 3))
nrow(rf.Grid)
## [1] 6
set.seed(1)
rf.seeds <- vector(mode = "list", length = 11) # length is = (nresampling) + 1
for(i in 1:10) rf.seeds[[i]]<- sample.int(n = 1000, 6) # 6 is the number of tuning parameters (mtry possibilities)
rf.seeds[[11]] <- 1 # for the last model
rf.seeds

library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

model.rf <- train(default ~.,
                  data = train, 
                  method = "rf",
                  ntree = 100,
                  importance = TRUE,
                  na.action = na.omit,
                  tuneGrid = rf.Grid,
                  trControl = rf.Control,
                  #metric = "Sens",
                  allowParallel=TRUE)
stopCluster(cl)
registerDoSEQ()
plot(model.rf, metric = "Sens")