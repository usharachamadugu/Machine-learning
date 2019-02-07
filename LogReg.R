data1 = read.csv("C:/Users/phsivale/Documents/Trainings/titanic.csv",
                 na.strings=c(""," ","NA","?","  "))
head(data1)
names(data1)
colsToUse = c('pclass','survived','sex','age','fare',
              'sibsp','parch','embarked','home_dest')

data1 = data1[,colsToUse]
summary(data1)
str(data1)

data1$home_dest = NULL

####
data1$survived = as.factor(data1$survived)
data1$pclass = as.factor(data1$pclass)



### KNN Imputation
# install.packages('DMwR')
library(DMwR)
data2 = knnImputation(data = data1,k=3)
summary(data2)
sum(is.na(data2))
summary(data1)

data2$home_dest = NULL
#### Handling Sibsp and Parch

table(data1$sibsp)

# data2$sibsp_cat = ifelse(data2$sibsp > 2,'>2',data2$sibsp)
# data2$sibsp_cat = as.factor(data2$sibsp_cat)
# 
# table(data1$parch)
# data2$parch_cat = ifelse(data2$parch > 1,'>1',data2$parch)
# data2$parch_cat = as.factor(data2$parch_cat)
summary(data2)

# ### Dropping sibsp and parch
# data2$sibsp = NULL
# data2$parch = NULL


summary(data2)


data2$pclass = as.factor(data2$pclass)


## Capping the fare column's outliers to 99Pecerntile
quantile(data2$fare,0.99)

data2$fare[data2$fare > quantile(data2$fare,0.99)] = quantile(data2$fare,0.99)
hist(data2$fare)
data2$fare = sqrt(data2$fare)
summary(data2)

##### train test split
set.seed(123)
rows = 1:nrow(data2)
trainRows = sample(rows,round(0.7*nrow(data2)))

trainData = data2[trainRows,]
testData = data2[-trainRows,]

summary(data2)

### Model building
model = glm(survived~.-fare-parch-embarked,
            data=trainData,
            family = binomial(link = "logit"))

summary(model)

## Predictions
preds = predict(model,testData,type = 'response')
### Type= response returns probabilities . Default is logodds
testData$preds = preds

##Metrics
testData$preds2 = ifelse(testData$preds > 0.5,1,0)
table(testData$survived,testData$preds2,dnn=c('actuals','preds'))

#### ROC
library(ROCR)
pred = prediction(preds , testData$survived)
perf= performance(pred, "tpr","fpr")
# perf= performance(pred, "tnr","fnr")
# perf= performance(pred, "prec","rec")
# plot(perf,colorize = T)

plot(perf, colorize=T, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(1.2,1.2), avg="threshold", lwd=3,
     main= "ROC")

AUC_1 = performance(pred, measure = 'auc')@y.values[[1]]
AUC_1
