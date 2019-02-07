#Program : randomForest with Crossvalidation and Gridsearch
#### Random Forest and Boosting 
chrn$Churn = as.factor(chrn$Churn)

ids = sample(nrow(chrn), nrow(chrn)*0.8)

train = chrn[ids,]
test = chrn[-ids,]
library(randomForest)

rftree = randomForest( Churn ~ . , data = train, ntree = 70, mtry = 4, max_depth = 8, nodesize = 5 )
?randomForest

test$pred = predict(rftree, newdata = test)

table(test$Churn, test$pred)

## variable importance 

varImpPlot(rftree)


p = 67/(67+7)
r = 67/(67+24)

2*p*r/(p+r)

## Gridsearch and cross validation 

library(caret)
library(randomForest)
library(mlbench)

control = trainControl(method="repeatedcv", number=5, search = "grid") 
set.seed(1238)

## Parameter search using grid 

tune = expand.grid(.mtry=c(4,5,6))

rf_gridsearch = train(as.factor(Churn) ~., data=train, method="rf", metric="Accuracy", tuneGrid=tune, trControl=control)

print(rf_gridsearch, showSD = T)

pred = predict(rf_gridsearch, newdata = test)

#### MOdel performance 

table(test$Churn, pred)

### More parameters in grid search 

tune = expand.grid(.mtry=c(4,5,6), .ntree=c(25,30,35), .Max_depth= c(8,9))
tune

## custom paramerters for RandomForest 
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree","Max_depth"), class = rep("numeric", 3), label = c("mtry", "ntree","Max_depth"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

### 

rf_gridsearch = train(as.factor(Churn) ~., data=train, method=customRF, metric="Accuracy", tuneGrid=tune, trControl=control)
print(rf_gridsearch,showSD = T)
plot(rf_gridsearch)
rf_gridsearch$results

names(rf_gridsearch)

summary(rf_gridsearch)

## performance of the model by caret RF
pred_rs = predict(rf_gridsearch, test)

table(test$Churn, pred_rs)

##### Boosting (gbm)
library(caret)
tune = expand.grid(shrinkage = c(0.1,0.01,0.05))

fitControl = trainControl(method = "repeatedcv", number = 4)
gbmFit1 = train(as.factor(Churn) ~ ., data = train, method = "gbm", trControl = fitControl, verbose = T)

print(gbmFit1, showSD = T)
gbmFit1$results
names(gbmFit1)

gbmFit1$metric

test$pred = predict(gbmFit1, test,type= "prob")[,2] 

hist(test$pred)

test$class = ifelse(test$pred >= 0.5 , 1 , 0)

table(test$Churn, test$class)

### GBM 
library(gbm)
?gbm
gbm.fit = gbm(
  formula = Churn ~ .,
  distribution = "bernoulli",
  data = train,
  n.trees = 1000,
  interaction.depth = 5,
  shrinkage = 0.05,
  cv.folds = 5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = T
) 

gbmtree = gbm( Churn ~ . , data= train, distribution = "bernoulli", n.trees = 1500, shrinkage = 0.01, cv.folds = 3, verbose = T, interaction.depth = 5, bag.fraction = 0.6, n.minobsinnode = 5)


gbmtree = gbm( Churn ~ . , data= train, distribution = "bernoulli", n.trees = 1500, shrinkage = 0.01, cv.folds = 3,  interaction.depth = 5, bag.fraction = 0.6, verbose = T)

print(gbmtree)

### Perfromance of gbm ( Deviance vs Number of trees)
gbm.perf(gbmtree, method = "cv")

## predictions 

pred = predict(gbmtree , newdata = test, n.trees = 1334)

pred_class = ifelse(pred >=0.5, 1, 0)

table(test$Churn, pred_class)

### Variable Importance in gbm
summary(gbmtree, cBars = 6) 

par(mar = c(5, 8, 1, 1))
summary(
  gbmtree, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

install.packages("vip")



