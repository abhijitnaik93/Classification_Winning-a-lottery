#Loading the Data
train=read.csv(file.choose())#,na.strings=c(""))#loandata file
dim(train)
names(train)
head(train)
str(train)

test = read.csv(file.choose())
names(test)


#Data Cleaning
#Checking the Missing Value
sum(complete.cases(train))
sapply(train,function(x) sum(is.na(x)))
#str(loan.raw)
#filling the missing values
train$Age [is.na(train$Age)] = round(mean(train$Age,na.rm=TRUE))

sum(complete.cases(test))
sapply(test,function(x) sum(is.na(x)))

test$Age [is.na(test$Age)] = round(mean(test$Age,na.rm=TRUE))

# Removing the un used attributes 
train=train[,-11]
names(train)

test = test[,-10]

names(test)
str(test)
#?cor
#Data preparation
train_cor = train[,c(-2,-4,-5,-9,-11)]
names(train_cor)
str(train_cor)
res=cor(train_cor[,1:6])
round(res,2)
#Removing PClass
str(train)
train=train[,-3]
names(train)

test = test [,-2]
names(test)
str(train_cor)
train_cor = train_cor[,-2]
str(train_cor)
train_corr=as.numeric(train$Survived)
str(train_corr)
#checking the correlation between independent and dependednt variables
cor(train_cor[,1:5],train_corr)
str(train)
#train=train[,-9]
#str(train)
#names(train)
#names(test)

#test = test[,-8]
train$Survived=factor(train$Survived,levels=c(0,1),labels=c("No","Yes"))
summary(train)
#Dividing the data
set.seed(222)
d = sort(sample(nrow(train), nrow(train)*.7))
train_train<-train[d,]
train_test<-train[-d,]
dim(train_train)
dim(train_test)


#Testing accuracy
set.seed(222)
d = sort(sample(nrow(train), nrow(train)*.7))
train_train<-train[d,]
train_test<-train[-d,]
dim(train_train)
dim(train_test)

class(train$Survived)
class(pred.naiveBayes)

#Naive Bayes

library(e1071)
model.naiveBayes <- naiveBayes(Survived ~ ., data = train_train, laplace = 3)
pred.naiveBayes <- predict(model.naiveBayes, train_test,type="class")
head(pred.naiveBayes)

#Performance Matrix for Naive Bayes
conf.matrix = table(train_test$Survived,pred.naiveBayes)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")

print(conf.matrix)

#Error
1-(sum(diag(conf.matrix))/sum(conf.matrix))
#Accuracy
(sum(diag(conf.matrix))/sum(conf.matrix))
#Naive Bayes from the built model

pred.naiveBayesF <- predict(model.naiveBayes,test,type="class")
head(pred.naiveBayesF)

#Final = cbind(test$PassengerId, pred.naiveBayesF)
#head(Final)
output = data.frame(test$PassengerId, pred.naiveBayesF)
output
write.csv(output,file="submission.csv")



#SVM
library(e1071)
model.svm=svm(formula = Survived ~ ., data = train_train)
print(model.svm)
pred.svm<- predict(model.svm, train_test)
plot(pred.svm)

#Performance Matrix for SVM
conf.matrix = table(train_test$Survived,pred.svm)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

#Error
1-(sum(diag(conf.matrix))/sum(conf.matrix))
#Accuracy
(sum(diag(conf.matrix))/sum(conf.matrix))
