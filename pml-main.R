setwd("H:/others/Practical Machine Learning/Project")
input = "H:/others/Practical Machine Learning/"
library("caret")
library("pls")
library("Hmisc")
library("rattle")

######-------------------------Data Prepare--------------------------------#####
trainData = read.table("pml-training.csv",na.strings=c("NA",""),
               header = T,nrows = -1,fill=TRUE,sep=",",strip.white = T, 
               as.is=T,row.names = NULL,check.names=FALSE )
#19622*160
testData = read.table("pml-testing.csv",na.strings=c("NA",""),
                       header = T,nrows = -1,fill=TRUE,sep=",",strip.white = T, 
                       as.is=T,row.names = NULL, check.names=FALSE )
#20*160

#If there are more than 50% NAs in one value, do not include that variable
trainDataSub = trainData[,(colMeans(is.na(trainData)) <0.5)]
dim(trainDataSub)
#[1] 19622    60


uniqueV = sapply(trainDataSub,function(x) length(unique(x)))
dubiousX = names(uniqueV[uniqueV<5])
dubiousX
# new_window

notNo = sapply(trainDataSub,function(x) is.numeric(x))
notNumeric = names(notNo[notNo == FALSE])
notNumeric
# "user_name"      "cvtd_timestamp" "new_window"     "classe" 
unique(trainDataSub[,"new_window"])
#no yes
# trainDataSub$new_window_no[trainDataSub$new_window == "yes"]=1
# trainDataSub$new_window_no[trainDataSub$new_window == "no"]=0
# table(trainDataSub$new_window,trainDataSub$new_window_no)
trainDataSub = trainDataSub[,-which(names(trainDataSub) %in% c("user_name","cvtd_timestamp","new_window"))]
dim(trainDataSub)
#19622 57


for (i in 1:ncol(trainDataSub)){
  if(class(trainDataSub[,i]) == "numeric"){
    jpeg(file = paste(input,colnames(trainDataSub)[i],".jpg"))
    hist(trainDataSub[,i],main = colnames(trainDataSub)[i])
    dev.off()
  }
  
}


table(trainDataSub$classe)
# A    B    C    D    E 
# 5580 3797 3422 3216 3607

#delete the var for row numbers
trainDataSub = trainDataSub[,-which(colnames(trainDataSub) %in% c("","raw_timestamp_part_1","raw_timestamp_part_2","num_window"))]

# set.seed(12321)
# inTrain = createDataPartition(y = trainDataSub$classe,p=0.7,list=FALSE)
# training = trainDataSub[inTrain,]
# testing=trainDataSub[-inTrain,]
# dim(training)
# [1] 13737   53
# dim(testing)
# [1]   5885 53



######-------------------------Model_tree--------------------------------#####
trainDataSub$classe = as.factor(trainDataSub$classe)
ctrl = trainControl(method = "cv",number = 10)
modFit1 = train(classe ~ .,method="rpart",data = trainDataSub,trControl = ctrl)
fancyRpartPlot(modFit1$finalModel)
predict(modFit1,newdata = testData)
varImp(modFit1)
save(modFit1,file = "modFit1.RData")
######-------------------------Model_treebag--------------------------------#####
ctrl = trainControl(method = "cv",number = 10)
modFit2 = train(classe ~ .,method="treebag",data = trainDataSub,trControl = ctrl)
modFit2
modFit2$results
modFit2$finalModel
save(modFit2,file = "modFit2.RData")
load("modFit2.RData")
predict(modFit2,newdata = testData)
varImp(modFit2)

######-------------------------Model_rf--------------------------------#####
ctrl = trainControl(method = "cv",number = 2)
modFit3 = train(classe ~ .,method="rf",data = trainDataSub,trControl = ctrl,prox = TRUE)
save(modFit3,file = "modFit3.RData")
load("modFit3.RData")
predict(modFit3,newdata = testData)
######-------------------------Model_boosting--------------------------------#####
ctrl = trainControl(method = "cv",number = 3)
modFit4 = train(classe ~ .,method="gbm",data = trainDataSub,trControl = ctrl,verbose=FALSE)
save(modFit4,file = "modFit4.RData")
modFit4$finalModel
modFit4 
predict(modFit4,newdata= testData)
######-------------------------Submission--------------------------------#####
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

answers = c("B","A","B","A","A","E","D","B","A","A","B","C","B","A","E","E","A","B","B","B")
pml_write_files(answers)
