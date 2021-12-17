
suppressMessages(library(caret))
suppressMessages(library(GGally))

# create models to test other functions

set.seed(25)
# for regression
control <- caret::trainControl(method = "cv",
                               number = 5,
                               savePredictions = TRUE)

traindata.reg <- caret::LPH07_2(150)[, c(1:7, 21)]
traindata.reg$Var06 <- cut(traindata.reg$Var06, 2)
traindata.reg$Var07 <- cut(traindata.reg$Var07, 3)

index <- caret::createDataPartition(traindata.reg$y, p = 0.75, list = FALSE)

testdata.reg <- traindata.reg[-index, ]
traindata.reg <- traindata.reg[index, ]

model.rf.reg <- caret::train(y ~ ., data = traindata.reg, method = "rf", trControl = control)
model.brt.reg <- caret::train(y ~ ., data = traindata.reg, method = "gbm", trControl = control, verbose = FALSE)


set.seed(25)
# for classification
control <- caret::trainControl(method = "cv",
                               number = 5,
                               classProbs = TRUE,
                               savePredictions = TRUE,
                               summaryFunction = caret::twoClassSummary)

traindata <- caret::twoClassSim(n = 150)[, c(1:7, 16)]
traindata$TwoFactor1 <- cut(traindata$TwoFactor1, 2, letters[1:2])
traindata$TwoFactor2 <- cut(traindata$TwoFactor2, 3, letters[3:5])

index <- caret::createDataPartition(traindata$Class, p = 0.75, list = FALSE)

testdata <- traindata[-index, ]
traindata <- traindata[index, ]

model.rf <- caret::train(Class ~ ., data = traindata, method = "rf", metric = "ROC", trControl = control)
model.brt <- caret::train(Class ~ ., data = traindata, method = "gbm", metric = "ROC", trControl = control, verbose = FALSE)
models <- list(model.rf, model.brt)

set.seed(25)
generate_raster <- function(x) raster::raster(vals = sample(x, 2500, replace = TRUE), nrows = 50, ncols = 50)
r.class <- raster::stack(lapply(testdata[, -8], generate_raster))

set.seed(25)
r.reg <- raster::stack(lapply(testdata.reg[, -8], generate_raster))
