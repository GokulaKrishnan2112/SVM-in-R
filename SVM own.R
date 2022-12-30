library(e1071)
require(ggplot2)
bank=read.csv("D:\\Imarticus\\Study Related\\DATASETS\\bank-additional-full.csv",sep = ",")
View(bank)
unique(bank$job)

#Checking Null Values
prop.table(table(is.na(bank)))*100 #No null values present
#Converting the required column as numeric values
bank[, c(2:10,15,21)]=lapply(bank[c(2:10,15,21)],as.factor)
levels(bank$month)=c(4,8,12,7,6,3,5,11,10,9)
levels(bank$job)=c(1:13)
levels(bank$marital)=c(1:4)
levels(bank$education)=c(1:8)
levels(bank$default)=c(1:3)
levels(bank$housing)=c(1:3)
levels(bank$loan)=c(1:3)
levels(bank$contact)=c(1,2)
levels(bank$day_of_week)=c(5,1,4,2,3)
levels(bank$poutcome)=c(1,2,3)
levels(bank$y)=c(0,1)




set.seed(234)
train<-sample(nrow(bank),100)
bank_train<-bank[train, ]
bank_test<-bank[-train, ]

m1<-svm(y~.,data=bank_train,kernel = "linear",cost = 0.1)
summary(m1)

p1<-predict(m1,bank_test,type="class")
p2<-predict(m1,bank_train,type="class")
require(caret)
bank_test
confusionMatrix(p1,bank_test[ ,21])
confusionMatrix(p2,bank_train[,21])
require(base)


trcont<-trainControl(method="repeatedcv",number=10,repeats=3)
model1<- train(y~.,data=bank_train,method = "svmLinear",trControl=trcont,preProcess=c("center","scale"),tuneLength = 10)
model1
pred1<-predict(model1,bank_test)
confusionMatrix(pred1,bank_test[,21])

m2<-svm(y~.,data=bank_train,kernel = "radial",cost = 0.1)
summary(m2)
pred_train_m2=predict(m2,bank_train,type="Class")
pred_test_m2=predict(m2,bank_test,type="Class")

confusionMatrix(pred_test_m2,bank_test[,21])
confusionMatrix(pred_train_m2,bank_train[,21])

m3=svm(y~.,data=bank_train,kernel="sigmoid",cost=0.25)
summary(m3)
pred_train_m3=predict(m3,bank_train)
pred_test_m3=predict(m3,bank_test)

confusionMatrix(pred_train_m3,bank_train[,21])
confusionMatrix(pred_test_m3,bank_test[,21])

model_2<-svm(y~.,data=bank_train,kernel = "linear",cost = 0.5)
summary(model_2)

pred_train_model=predict(model_2,bank_train,type="Class")
pred_test_model=predict(model_2,bank_test,type="Class")
confusionMatrix(pred_train_model,bank_train[,21])
confusionMatrix(pred_test_model,bank_test[,21])



#m1_train_accuracy=89%,m1_test_accuray=92%
#model_1 accuracy=81%
#m2_train_accuray=85%,m2_test_accuracy=88%
#m3_train_accuracy=85%,m3_test_accuracy=88%
#model_1_train_accuracy=97%,model_1_accuracy=87%

