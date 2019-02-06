#library(mlbench)
#library(psych)
library(glmnet)
library(caret)

train=read.csv(file.choose())
train=train[,-1]
dim(train)
target=(train$SalePrice)

j=c()
c=c()


for(i in 1:length(train)){
  if(class(train[,i])=="factor"){
    j=c(j,i)
  }
}
j
length(j)
#Alley,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,
#FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PoolQC,Fence,
#MiscFeature

#convert all NA's to No
train$Alley = factor(train$Alley, levels=c(levels(train$Alley), 'No'))
train$Alley[is.na(train$Alley)] = 'No'
sum(is.na(train$Alley))
#convert all NA's to No
train$BsmtQual = factor(train$BsmtQual, levels=c(levels(train$BsmtQual), 'No'))
train$BsmtQual[is.na(train$BsmtQual)] = 'No'
sum(is.na(train$BsmtQual))
#convert all NA's to No
train$BsmtCond = factor(train$BsmtCond, levels=c(levels(train$BsmtCond), 'No'))
train$BsmtCond[is.na(train$BsmtCond)] = 'No'
levels(train$BsmtCond)
sum(is.na(train$BsmtCond))
#convert all NA's to No
train$BsmtExposure = factor(train$BsmtExposure, levels=c(levels(train$BsmtExposure), 'No Bsmt'))
train$BsmtExposure[is.na(train$BsmtExposure)] = 'No Bsmt'
train$BsmtExposure
levels(train$BsmtExposure)
sum(is.na(train$BsmtExposure))
#convert all NA's to No
train$FireplaceQu = factor(train$FireplaceQu, levels=c(levels(train$FireplaceQu), 'No'))
train$FireplaceQu[is.na(train$FireplaceQu)] = 'No'
levels(train$FireplaceQu)
sum(is.na(train$FireplaceQu))

#convert all NA's to No
train$GarageType = factor(train$GarageType, levels=c(levels(train$GarageType), 'No'))
train$GarageType[is.na(train$GarageType)] = 'No'
levels(train$GarageType)
sum(is.na(train$GarageType))

#convert all NA's to No
train$GarageFinish = factor(train$GarageFinish, levels=c(levels(train$GarageFinish), 'No'))
train$GarageFinish[is.na(train$GarageFinish)] = 'No'
levels(train$GarageFinish)
sum(is.na(train$GarageFinish))
#convert all NA's to No
train$GarageQual = factor(train$GarageQual, levels=c(levels(train$GarageQual), 'No'))
train$GarageQual[is.na(train$GarageQual)] = 'No'
levels(train$GarageQual)
sum(is.na(train$GarageQual))

train$GarageCond = factor(train$GarageCond, levels=c(levels(train$GarageCond), 'No'))
train$GarageCond[is.na(train$GarageCond)] = 'No'
levels(train$GarageCond)

train$PoolQC = factor(train$PoolQC, levels=c(levels(train$PoolQC), 'No'))
train$PoolQC[is.na(train$PoolQC)] = 'No'
levels(train$PoolQC)


train$Fence = factor(train$Fence, levels=c(levels(train$Fence), 'No'))
train$Fence[is.na(train$Fence)] = 'No'
levels(train$Fence)


train$MiscFeature = factor(train$MiscFeature, levels=c(levels(train$MiscFeature), 'None'))
train$MiscFeature[is.na(train$MiscFeature)] = 'None'
levels(train$MiscFeature)

#BsmtFinType1
#convert all NA's to No
train$BsmtFinType1 = factor(train$BsmtFinType1, levels=c(levels(train$BsmtFinType1), 'No'))
train$BsmtFinType1[is.na(train$BsmtFinType1)] = 'No'
levels(train$BsmtFinType1)


#BsmtFinType1
#convert all NA's to No
train$BsmtFinType2 = factor(train$BsmtFinType2, levels=c(levels(train$BsmtFinType2), 'No'))
train$BsmtFinType2[is.na(train$BsmtFinType2)] = 'No'
levels(train$BsmtFinType2)
train$BsmtFinType1


for(i in 1:length(train)){
  if(class(train[,i])=="integer"){
    c=c(c,i)
  }
}
c
length(c)
#a=cor(train[,c],train$SalePrice)

chk_missing_values=function(c){
  for(i in c){
    if(sum(is.na(train[,i])))
    {
      cat("Missing values in column",i,"\n")
      #train[[i]][is.na(train[[i]])]=round(mean(train[[i]],na.rm=TRUE))
    }
  }
}


chk_missing_values(c)
chk_missing_values(j)
#str(train[26])
names(train[26])
train[[3]][is.na(train[[3]])]=round(mean(train[[3]],na.rm=TRUE))
train[[26]][is.na(train[[26]])]=round(mean(train[[26]],na.rm=TRUE))
train[[59]][is.na(train[[59]])]=round(mean(train[[59]],na.rm=TRUE))


train[[42]][is.na(train[[42]])]='SBrkr'
train[[25]][is.na(train[[25]])]='None'

#Detecting and removing Outliers
z = target[!target %in% boxplot.stats(target)$out]
z1=log10(z)
par(mfrow = c(1, 2))
hist(z1)
hist(train$SalePrice)

index=which(!train$SalePrice %in% z)
length(index)
tr=train[-index,]
tr$SalePrice=log10(tr$SalePrice)
head(tr$SalePrice)
dim(tr)
dim(train)
tr$SalePrice
train = tr

antilog<-function(lx,base) 
{ 
  lbx<-lx/log(exp(1),base=base) 
  result<-exp(lbx) 
  result 
}

if(0){
cor_values=function(c){
  cat("Only variables with strong co-relation are considered for modelling:","\n")
  for(i in c){
    if(abs(cor(train$SalePrice,train[,i]))>=0.6){
      #numv=c(numv,names(train[i]))
      cat(names(train[i]),"\t",i,"\n")
    }
  }
}
}
for(i in j){
  train[,i]=as.numeric(train[,i])
}

#cor_values(c)
#cor_values(j)

set.seed(222)
ind=sample(2,nrow(train),replace = T, prob = c(0.7,0.3))
T1=train[ind==1,]
T2=train[ind==2,]


#---------------------------------------------------------------------------------------------------------
#Ridge
set.seed(5489)
lamdas = seq(.0001, 1, length = 5) #creating a sequence between .0001 to 1 of 5 no.
ridge = train(SalePrice ~ . , data = T1, method = 'glmnet', tuneGrid=expand.grid(alpha=0, lambda= lamdas))
summary(ridge)
ridge

SalePrice_ridge <- predict(ridge,
                     T2,
                     type = "raw")

res=antilog(T2$SalePrice,10)-antilog(SalePrice_ridge,10)
head(cbind(antilog(SalePrice_ridge,10), antilog(T2$SalePrice,10),res))
res

mad=mean(abs(res))
mse=mean(res^2)
rmse=sqrt(mse)#check the fit of the model. Lower the RMSE value better the model is
mape=mean(abs(res)/antilog(T2$SalePrice,10))
mad
mse
rmse
mape
#Plot Result
plot(ridge)
plot(ridge$finalModel,xvar= "lambda", label= T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))


#Rsquared = 83.02%
#rmse = 84260.55
#mape = 11.60
#--------------------------------------------------------------------------------------------------------------
#Lasso


lamdas = seq(.0001, 1, length = 5) #creating a sequence between .0001 to 1 of 5 no.
lasso = train(SalePrice ~ . , data = T1, method = 'glmnet', tuneGrid=expand.grid(alpha=1, lambda= lamdas))
summary(lasso)
lasso

SalePrice_lasso <- predict(lasso,
                           T2,
                           type = "raw")


res=antilog(T2$SalePrice,10)-antilog(SalePrice_lasso,10)
head(cbind(antilog(SalePrice_lasso,10), antilog(T2$SalePrice,10),res))
res

mad=mean(abs(res))
mse=mean(res^2)
rmse=sqrt(mse)#check the fit of the model. Lower the RMSE value better the model is
mape=mean(abs(res)/antilog(T2$SalePrice,10))

mad
mse
rmse
mape

#Rsquared = 82.42%
#rmse = 93178.95
#mape = 11.84%


#-------------------------------------------------------------------------------------------

#Elastic Net Regression

lamdas = seq(.0001, 1, length = 5) #creating a sequence between .0001 to 1 of 5 no.
alphas = seq(0.0001, 1, length = 5)
enr = train(SalePrice ~ . , data = T1, method = 'glmnet', tuneGrid=expand.grid(alpha=alphas, lambda= lamdas))
summary(enr)
enr
SalePrice_enr <- predict(enr,
                           T2,
                           type = "raw")
res=antilog(T2$SalePrice,10)-antilog(SalePrice_enr,10)
head(cbind(antilog(SalePrice_enr,10), antilog(T2$SalePrice,10),res))
res

mad=mean(abs(res))
mse=mean(res^2)
rmse=sqrt(mse)#check the fit of the model. Lower the RMSE value better the model is
mape=mean(abs(res)/antilog(T2$SalePrice,10))

mad
mse
rmse
mape

#Rsquared = 83.85 %
#rmse = 84258.15
#mape = 11.6%


#--------------------------------------------------------------------------------------

# Stepwise regression model
library(MASS)
full.model <- lm(SalePrice ~ ., data = T1)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
step.model

SalePrice_step <- predict(step.model,
                         T2)

res=antilog(T2$SalePrice,10)-antilog(SalePrice_step,10)
head(cbind(antilog(SalePrice_step,10), antilog(T2$SalePrice,10),res))
res

mad=mean(abs(res))
mse=mean(res^2)
rmse=sqrt(mse)#check the fit of the model. Lower the RMSE value better the model is
mape=mean(abs(res)/antilog(T2$SalePrice,10))

mad
mse
rmse
mape

#Rsquared = 87.93%
#rmse = 98601.56
#mape = 12.07%

#------------------------------------------------------------------------------------------------------------


# Gradient Boosting Machine 
fitControl <- trainControl( method = "repeatedcv", number = 4, repeats = 4)
fit <- train(SalePrice ~ ., data = T1, method = "gbm", trControl = fitControl,verbose = FALSE)
summary(fit)
fit

SalePrice_fit <- predict(fit,
                          T2)
res=antilog(T2$SalePrice,10)-antilog(SalePrice_fit,10)
head(cbind(antilog(SalePrice_fit,10), antilog(T2$SalePrice,10),res))
res

mad=mean(abs(res))
mse=mean(res^2)
rmse=sqrt(mse)#check the fit of the model. Lower the RMSE value better the model is
mape=mean(abs(res)/antilog(T2$SalePrice,10))

mad
mse
rmse
mape

#Rsquared = 88.24%
#rmse = 21675.27
#mape = 9.45%



#--------------------------------------------------------------------------------------
#XGBoost

TrainControl <- trainControl( method = "repeatedcv", number = 4, repeats = 3)
model<- train(SalePrice ~ ., data = T1, method = "xgbLinear", trControl = TrainControl,verbose = FALSE)
model
SalePrice_xg <- predict(model,
                          T2)
res=antilog(T2$SalePrice,10)-antilog(SalePrice_xg,10)
head(cbind(antilog(SalePrice_xg,10), antilog(T2$SalePrice,10),res))
res

mad=mean(abs(res))
mse=mean(res^2)
rmse=sqrt(mse)#check the fit of the model. Lower the RMSE value better the model is
mape=mean(abs(res)/antilog(T2$SalePrice,10))

mad
mse
rmse
mape

#Rsquared = 85.88%
#rmse = 22014.97
#mape = 10.15%


#----------------------------------------------------------------------------------------------------------------------
#Random Forest
rf_model_housing <-train(SalePrice ~., # Standard formula notation
                         data=T1,  # Excclude 'id'
                         method="rf",              # randomForest
                         nodesize= 12,              # 10 data-points/node. Speeds modeling
                         ntree =500,               # Default 500. Reduced to speed up modeling
                         trControl=trainControl(method="repeatedcv", number=2,repeats=1),  # cross-validation strategy
                         tuneGrid = expand.grid(mtry = c(123))
)


rf_model_housing
SalePrice <- predict(rf_model_housing,
                     T2,
                     type = "raw")
res=antilog(T2$SalePrice,10)-antilog(SalePrice,10)
head(cbind(antilog(SalePrice,10), antilog(T2$SalePrice,10),res))
res

mad=mean(abs(res))
mse=mean(res^2)
rmse=sqrt(mse)#check the fit of the model. Lower the RMSE value better the model is
mape=mean(abs(res)/antilog(T2$SalePrice,10))
mad
mse
rmse
mape

#Rsquared = 84.29%
#rmse = 24805.25
#mape = 10.66%


#--------------------------------------------------------------------------------------------------------

#In XGBoost we can see that we got the lowest RMSE = 391.0482 and mape = 1.803502% value.
#We got this value after running the model a couple of times and each time we ran it rmse value decreased.
#It takes lot of time to execute 


#We can see that Random Forest has RMSE = 1497.3 and Mape = 5.29%
#So Random Forestis the best model to use for this data.


#--------------------------------------------------------------------------------------------------------
rm(test)
#Predicting test values
test= read.csv(file.choose())
Id=test$Id
Id
test= test[,-1]
test$Alley = factor(test$Alley, levels=c(levels(test$Alley), 'No'))
test$Alley[is.na(test$Alley)] = 'No'
sum(is.na(test$Alley))
#convert all NA's to No
test$BsmtQual = factor(test$BsmtQual, levels=c(levels(test$BsmtQual), 'No'))
test$BsmtQual[is.na(test$BsmtQual)] = 'No'
sum(is.na(test$BsmtQual))
#convert all NA's to No
test$BsmtCond = factor(test$BsmtCond, levels=c(levels(test$BsmtCond), 'No'))
test$BsmtCond[is.na(test$BsmtCond)] = 'No'
levels(test$BsmtCond)
sum(is.na(test$BsmtCond))
#convert all NA's to No
test$BsmtExposure = factor(test$BsmtExposure, levels=c(levels(test$BsmtExposure), 'No Bsmt'))
test$BsmtExposure[is.na(test$BsmtExposure)] = 'No Bsmt'
test$BsmtExposure
levels(test$BsmtExposure)
sum(is.na(test$BsmtExposure))
#convert all NA's to No
test$FireplaceQu = factor(test$FireplaceQu, levels=c(levels(test$FireplaceQu), 'No'))
test$FireplaceQu[is.na(test$FireplaceQu)] = 'No'
levels(test$FireplaceQu)
sum(is.na(test$FireplaceQu))

#convert all NA's to No
test$GarageType = factor(test$GarageType, levels=c(levels(test$GarageType), 'No'))
test$GarageType[is.na(test$GarageType)] = 'No'
levels(test$GarageType)
sum(is.na(test$GarageType))

#convert all NA's to No
test$GarageFinish = factor(test$GarageFinish, levels=c(levels(test$GarageFinish), 'No'))
test$GarageFinish[is.na(test$GarageFinish)] = 'No'
levels(test$GarageFinish)
sum(is.na(test$GarageFinish))
#convert all NA's to No
test$GarageQual = factor(test$GarageQual, levels=c(levels(test$GarageQual), 'No'))
test$GarageQual[is.na(test$GarageQual)] = 'No'
levels(test$GarageQual)
sum(is.na(test$GarageQual))

test$GarageCond = factor(test$GarageCond, levels=c(levels(test$GarageCond), 'No'))
test$GarageCond[is.na(test$GarageCond)] = 'No'
levels(test$GarageCond)

test$PoolQC = factor(test$PoolQC, levels=c(levels(test$PoolQC), 'No'))
test$PoolQC[is.na(test$PoolQC)] = 'No'
levels(test$PoolQC)


test$Fence = factor(test$Fence, levels=c(levels(test$Fence), 'No'))
test$Fence[is.na(test$Fence)] = 'No'
levels(test$Fence)


test$MiscFeature = factor(test$MiscFeature, levels=c(levels(test$MiscFeature), 'None'))
test$MiscFeature[is.na(test$MiscFeature)] = 'None'
levels(test$MiscFeature)

#BsmtFinType1
#convert all NA's to No
test$BsmtFinType1 = factor(test$BsmtFinType1, levels=c(levels(test$BsmtFinType1), 'No'))
test$BsmtFinType1[is.na(test$BsmtFinType1)] = 'No'
levels(test$BsmtFinType1)


#BsmtFinType1
#convert all NA's to No
test$BsmtFinType2 = factor(test$BsmtFinType2, levels=c(levels(test$BsmtFinType2), 'No'))
test$BsmtFinType2[is.na(test$BsmtFinType2)] = 'No'
levels(test$BsmtFinType2)
test$BsmtFinType1

c1=c()
j1=c()
for(i in 1:length(test)){
  if(class(test[,i])=="integer"){
    c1=c(c1,i)
  }
}

for(i in 1:length(test)){
  if(class(test[,i])=="factor"){
    j1=c(j1,i)
  }
}
length(j1)

length(c1)

#a=cor(test[,c],test$SalePrice)

chk_missing_values=function(c){
  for(i in c){
    if(sum(is.na(test[,i])))
    {
      cat("Missing values in column",i,"\n")
      #test[[i]][is.na(test[[i]])]=round(mean(test[[i]],na.rm=TRUE))
    }
  }
}


chk_missing_values(c1)
chk_missing_values(j1)

test[[3]][is.na(test[[3]])]=round(mean(test[[3]],na.rm=TRUE))
test[[26]][is.na(test[[26]])]=round(mean(test[[26]],na.rm=TRUE))
test[[34]][is.na(test[[34]])]=round(mean(test[[34]],na.rm=TRUE))
test[[36]][is.na(test[[36]])]=round(mean(test[[36]],na.rm=TRUE))
test[[37]][is.na(test[[37]])]=round(mean(test[[37]],na.rm=TRUE))
test[[38]][is.na(test[[38]])]=round(mean(test[[38]],na.rm=TRUE))
test[[47]][is.na(test[[47]])]=round(mean(test[[47]],na.rm=TRUE))
test[[48]][is.na(test[[48]])]=round(mean(test[[48]],na.rm=TRUE))
test[[59]][is.na(test[[59]])]=round(mean(test[[59]],na.rm=TRUE))
test[[61]][is.na(test[[61]])]=round(mean(test[[61]],na.rm=TRUE))
test[[62]][is.na(test[[62]])]=round(mean(test[[62]],na.rm=TRUE))

names(test[73])
test[[42]]
summary(test[[73]])
class(test[[2]])
test[[2]][is.na(test[[2]])]='RL'
test[[9]][is.na(test[[9]])]='AllPub'
test[[25]][is.na(test[[25]])]='VinylSd'
test[[23]][is.na(test[[23]])]='VinylSd'
test[[24]][is.na(test[[24]])]='None'
test[[53]][is.na(test[[53]])]='TA'
test[[55]][is.na(test[[55]])]='Typ'
#test[[73]][is.na(test[[73]])]='WD'
test[[78]][is.na(test[[78]])]='WD'

for(i in j1){
  test[,i]=as.numeric(test[,i])
}

SalePrice_test <- predict(rf_model_housing,
                     test,
                     type = "raw")


#SalePrice_test1 <- predict(rf_model_housing,
#                          test,
#                          type = "raw")

head(cbind(Id, antilog(SalePrice_test,10)))
cbind(Id, antilog(SalePrice_test,10))
#head(cbind(test$Id, antilog(SalePrice_test1,10)))

