library(caret)
library(randomForest)
library(gbm)

trainAdd<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testAdd<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training1 <- read.csv(url(trainAdd), na.strings=c(" ","NA","#DIV/0!","","-"))
Validation <- read.csv(url(testAdd), na.strings=c(" ","NA","#DIV/0!","","-"))

Sub1<-createDataPartition(training1$classe,p=0.6,list=FALSE)
training<-training1[Sub1,]
test<-training1[-Sub1,]
rm(Sub1)

summary(training)
dim(training[complete.cases(training),])
NullIndex<-lapply(training,function(x) sum(ifelse(is.na(x),1,0))/nrow(training))
NullIndex<-(NullIndex[NullIndex>0])
NullIndex[which.min(NullIndex[NullIndex>0])]

col_to_omit<-names(NullIndex)
training_cl<-training[,!colnames(training)%in%col_to_omit]
training_cl<-training_cl[,8:ncol(training_cl)]
set.seed(1366)
t<-tuneRF(training_cl[,colnames(training_cl)!="classe"],training_cl$classe,ntreeTry = 5000,trace=FALSE)
best.t<-t[t[, 2] == min(t[, 2]), 1]
RFmod<-randomForest(classe~.,data=training_cl,ntree=5000,mtry=best.t,
                    replace=TRUE,keep.forest=TRUE,importance=TRUE)
varImpPlot(RFmod)

boostFit <- train(classe ~ ., method = "gbm", data = training_cl, verbose = F, 
                  trControl = trainControl(method = "cv", number = 10))

Validation$magnet_dumbbell_z<-as.numeric(Validation$magnet_dumbbell_z)
Validation$magnet_forearm_y<-as.numeric(Validation$magnet_forearm_y)
Validation$magnet_forearm_z<-as.numeric(Validation$magnet_forearm_z)

test$Pred_classe<-predict(RFmod,test)
test$Pred_classe_gbm<-predict(boostFit,test)
confusionMatrix(test$Pred_classe,test$classe)
confusionMatrix(test$Pred_classe_gbm,test$classe)
predict(RFmod,Validation)
