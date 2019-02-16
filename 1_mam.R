#SESSION 1
#To find correlation
data = read.csv("Correlation.csv", header = TRUE)
data
Temp = data$Temperature
pressure = data$Vapor.Pressure

plot(Temp,pressure)
cor(Temp,pressure)


#Regression
#strength
data1 = read.csv("Strength.csv", header = TRUE)
data1 = data1[,2:3]
week = data1$Weeks
strength = data1$Strength
plot(week,strength)
cor(data1)
#strength is dependent
mymodel = lm(strength~week)
summary(mymodel)
anova(mymodel)

pred = predict(mymodel)
res = residuals(mymodel)
cbind(data1,pred,res)

qqnorm(res)
qqline(res)
shapiro.test(res)

mad = mean(abs(res))
mse = mean(res^2)
rmse = sqrt(mse)
mape = mean(abs(res)/strength)
#Mape is easy to measure and compare

#Outlier test
library(car)
outlierTest(mymodel)
#p <  0.05  -- potential outlier
#------------------------------------------

#softdrink
data2 = read.csv("Softdrinks.csv", header = TRUE)
data2
data2 = data2[,2:3]
sales = data2$Sales
exp = data2$Expenditures
cor(data2)

mymodel = lm(sales~exp)
summary(mymodel)
anova(mymodel)
pred = predict(mymodel)
res = residuals(mymodel)
cbind(data2,pred,res)
#-----------------------------------------
#Pharma product file
data3 = read.csv(file.choose())
data3
data3 = data3[1:120,1:2]
sales = ts(data3$Sales)
plot(sales, type = "b")


#Dairy Product
data4 = read.csv(file.choose())
data4
production = ts(data4$Production, frequency = 1, start = c(1960))
production
plot(production, type = "b")


#Shipment

data5 = read.csv(file.choose())
data5
daa = ts(data5$Shipments)
plot(daa, type = "b")


#ADF - 
#H0 : Data not stationary 
library(tseries)
adf.test(daa)
#Data is  stationary at 10%
#Data is not stationary at 5%


#KPSS
#H0 : Data is stationary 
kpss.test(daa)

#Even if 1 test says that it is stationary then say it is stationary 
##GDP, Pharma_product, dairyproduct -- check if stationary


#--------Session 2 -----------------------------------------------------------------------------------
#GDP
data6 = read.csv(file.choose())
d6 = ts(data6$GDP)
plot(d6, type="b")
kpss.test(d6)  #Data is not stationary as p = 0.045 < 0.05

library(forecast)
ndiffs(d6)  #How many differncing u need to do

#differencing required is 1
#Mehtod for making data stationary 
mydiffdata = diff(d6, differences = 1)
plot(mydiffdata, type="b")
adf.test(mydiffdata)
kpss.test(mydiffdata)  #Now the data is stationary 


#Decomposition : seperating and estimating the 3 components 
#Accidents
data7 = read.csv(file.choose())
accidents = ts(data7$Number_per_month, frequency = 12, start= c(1986,1))
accidents
plot(accidents, type = "b")
mumodel = decompose(accidents)
plot(mumodel)

seasonal = mumodel$seasonal
trend = mumodel$trend
random = mumodel$random

seasonal
trend
random
cbind(accidents, seasonal, trend, random)


#Exopnential Smoothing 

#used for time sereis with  no trend or seaasonality 
#Most recent months have higher weigtage


#Amount 
data8 = read.csv(file.choose())
am = ts(data8)
plot(am, type = "b")
adf.test(am)  # data not stationary 
kpss.test(am) #data is stationary 
#Data is stationary 

mymodel = HoltWinters(am, beta = FALSE, gamma = FALSE)
mymodel
#alpha = 0.1285 
plot(mymodel)
#Read line soomths the randomness

#computing predicted and residuals 

pred = fitted(mymodel)
res = residuals(mymodel)
op = cbind(am, pred[,1], res)
op


#Model validation 

mad = mean(abs(res))
mse = mean(res^2)
rmse = sqrt(mse)
mape = mean(abs(res)/am)

mad
mse
rmse
mape

#Forecast 
library(forecast)
forecast = forecast(mymodel, 1)  #1 is number of periods to be forecasted 
forecast
plot(forecast)



#ARIMA
#Deterministic - function of time   and   stochastic- random noise
#takes corelation into account

#Part corr
data9 = read.csv(file.choose())

mymodel1 = lm(data9$Yield~data9$Temperature)
mymodel2 = lm(data9$Time~data9$Temperature)
summary(mymodel1)
summary(mymodel2)
anova(mymodel1)
anova(mymodel2)

res1 = residuals(mymodel1)
res2 = residuals(mymodel2)
res1
res2

#Partial corelation , keeping 1 factor constant find impact of other factor
cor(res1,res2)
#Here time was constant 


#Partial auto correlation 
#H0: There is no auto corelation 
#Here lag is calculated ... 

#rulers data
data10 = read.csv(file.choose())
age = ts(data10$Age)
plot(age,type = "b")

library(tseries)
adf.test(age)
kpss.test(age)
#Not stationary 

library(forecast)
ndiffs(age)
diffage = diff(age, differences = 1)
adf.test(diffage)
kpss.test(diffage)
#Data is stationary 


acf(diffage)  #Here lag1 is going above the blue line, consider the longest line that is crossing
pacf(diffage) #Lag 1 is significant 

mymodel = auto.arima(age)
summary(mymodel)
#Here arima is 0 1 1
#Figuure out where AIC is lowest -- AIC is estimate how closely  model is fitting 
#Auto Arima is the best 


#Timepass -- to search model 
arima(age, c(2,1,0))
#See aic value


#Auto correlation among residuals -- skipped

#Forecasating for next 3 periods

forecast = forecast(mymodel, h=3)
forecast



#Intervention model 
#Data cereals 

data11 = read.csv(file.choose())
mydata = ts(data11$Sales)
indicator = data11$Indicator
plot(mydata, type = "b")

mymodel = auto.arima(mydata, allowdrift = TRUE, xreg = indicator)
summary(mymodel)

fore = forecast(mymodel, h = 3)
fore




#------------ Session Absent -----

#Multiregression
library(caret)
#Boston Housing
data=read.csv(file.choose())
data("BostonHousing")
library(glmnet)
library(mlbench)
data = BostonHousing
data
library(psych)
str(data)
mymodel= lm(data$medv~data$crim+data$zn+data$indus+data$chas+data$nox+data$rm+data$age+data$dis+data$rad+data$tax+data$ptratio+data$b+data$lstat)
mymodel
summary(mymodel)
plot(lm$finalModel)
set.seed(222)
ind= sample(2, nrow(data), replace=T, prob= c(0.7,0.3))
train = data[ind==1,]
test= data[ind==2,]
train
xtab
#table= xtabs(data$medv~data$crim+data$zn+data$indus+data$chas+data$nox+data$rm+data$age+data$dis+data$rad+data$tax+data$ptratio+data$b+data$lstat,data)
data=read.csv("Cardiotocographic.csv", header = TRUE)#Cardiotographic
data
str(data)
data$NSP= as.factor(data$NSP)
head(data)
library(nnet)
#Multiple logitic regression
data$out= relevel(data$NSP, ref="1")
mymodel = multinom(out~LB+AC+FM, data=data)
summary(mymodel)

pred1=predict(mymodel,data)
tab1 = table(pred1, data$out)
tab1
1-sum(diag(tab1))/sum(tab1)


set.seed(222)
ind= sample(2, nrow(data), replace=T, prob= c(0.7,0.3))
train = data[ind==1,]
test= data[ind==2,]
train$out= relevel(train$NSP, ref="1")
mymodel = multinom(out~LB+AC+FM, data=train)
summary(mymodel)

pred1=predict(mymodel,train)
tab1 = table(pred1, train$out)
tab1
1-sum(diag(tab1))/sum(tab1)


test$out= relevel(test$NSP, ref="1")
mymodel = multinom(out~LB+AC+FM, data=test)
summary(mymodel)

pred1=predict(mymodel,test)
tab1 = table(pred1, test$out)
tab1
1-sum(diag(tab1))/sum(tab1)

#2-tailed z test
z= summary(mymodel)$coefficients/summary(mymodel)
p= (1- pnorm(abs(z),0,1))*2
p

#Decision Tree
#Advantage: it works well with contionus and discreat data
library(party)
mytree= ctree(NSP~LB+AC+FM, data, controls=ctree_control(mincriterion = 0.9, minsplit = 50))
print(mytree)
plot(mytree,type='simple')
#Misclassification erroe
tab= table(predict(mytree), data$NSP)
print(tab)
1-sum(diag(tab))/sum(tab)
#decision tress have more accuracy than logistic regression
mytree1= ctree(NSP~LB+AC+FM, train, controls=ctree_control(mincriterion = 0.9, minsplit = 50))
print(mytree1)
plot(mytree1,type='simple')
tab= table(predict(mytree1), train$NSP)#test$NSP
print(tab)
1-sum(diag(tab))/sum(tab)
mytree2= ctree(NSP~LB+AC+FM, test, controls=ctree_control(mincriterion = 0.9, minsplit = 50))
print(mytree2)
plot(mytree2,type='simple')
tab= table(predict(mytree2), test$NSP)
print(tab)
1-sum(diag(tab))/sum(tab)

#Train,TEst,Validate



#-----------------------Session -------------------------------------------------------

#Ridge Regression
#If there is high colinearity

library(mlbench)
library(caret)
library(glmnet)
library(psych)
library(party)
library(rlang)
data("BostonHousing")
data = BostonHousing

pairs.panels(data)

set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7,0.3))
train = data[ind == 1,]
test = data[ind == 2,]

lambdas = seq(0.0001,1,length = 5)
ridge <- caret::train(medv~., data = train, method = 'glmnet', tuneGrid = expand.grid(alpha = 0, lambda = lambdas))
#Alpha = 0  - ridge 
#aplha =1 - lasso



summary(ridge)
ridge 

#Plot
plot(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = "dev", label = T)
plot(varImp(ridge, scale = T))
#USE RMSE to find which is the best model, lower is better 


#LASSO
lasso <- caret::train(medv~., data = train, method = 'glmnet', tuneGrid = expand.grid(alpha = 1, lambda = lambdas))
summary(lasso)
lasso


#Elastic Net Regression

alphas = seq(0.0001, 1, length = 5)
en <- caret::train(medv~., data = train, method = 'glmnet', tuneGrid = expand.grid(alpha = alphas, lambda = lambdas))
summary(en)
en



#------------sesssion

library(caret)
library(pROC)
library(mlbench)

#Student classification -- File Admission  EXAMPLE 1
data = read.csv("binary.csv", header = TRUE)
str(data)
data$admit[data$admit == 0] = "No"
data$admit[data$admit ==  1] = "Yes"
data$admit = factor(data$admit)

#data Partition
set.seed(1234)
ind = sample(2,nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training = data[ind == 1,]
test = data[ind ==2,]

#KNN Model
#Make 10 models and repete it 3 times and find best model 
trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)


set.seed(222)
fit = train(admit~., data = training, method = 'knn', tuneLength = 20, trControl = trControl, preProc = c("center", "scale")) 

#Voxcoxc transformation is to transform data from normality 

#Model Peformance
fit
plot(fit)
varImp(fit)
pred = predict(fit, newdata = test)
confusionMatrix(pred, test$admit)



#Boston housind Regressio     #Example
data("BostonHousing")
data = BostonHousing
str(data)



#data Partition
set.seed(1234)
ind = sample(2,nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training = data[ind == 1,]
test = data[ind ==2,]

#KNN Model
#Make 10 models and repete it 3 times and find best model 
trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(333)
fit = train(medv~., data=training, tuneGrid = expand.grid(k=1:70), method = 'knn', metric = 'Rsquared', trControl = trControl, preProc = c('center', 'scale'))
fit

plot(fit)
varImp(fit)
pred = predict(fit, newdata =test)
RMSE(pred, test$medv)
plot(pred~test$medv)

#NAIVEBAYES
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

data = read.csv("binary.csv", header = TRUE)
str(data)

xtabs(~admit+rank, data = data)
data$rank = as.factor(data$rank)
data$admit = as.factor(data$admit)
#Visualization
pairs.panels(data[-1])

#Data Partition
set.seed(1234)
ind = sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))

train = data[ind ==1,]
test = data[ind == 2,]

#Naive Bayes Model
model = naivebayes::naive_bayes(admit ~ ., data = train, usekernel = TRUE)
model

train %>% 
        filter(admit == "1") %>%
        summarise(mean(gre), sd(gre))

plot(model)

#Predict 

p = predict(model, train, type = 'prob')
head(cbind(p, train))


#------------------- Session ---------------------
#Natural Language Processing 



data=read.csv("oneplus.csv", header = TRUE)
#octoparse scraper or instand data scrapper
head(data)
str(data)

#Build corpse

library(tm)
corpus = Corpus(VectorSource(data$Reviews))
inspect(corpus[1:5])
#inspect is similar to head just faster


#Clean text
corpus = tm_map(corpus, tolower)  #Lowercase
inspect(corpus[1:5])

corpus = tm_map()

cleanset = tm_map(cleanset, removeWords, c('phone', '6t', '6T'))

corpus = tm_map(corpus, removePunctuation)  #remove Punctuation
inspect(corpus[1:5])

corpus = tm_map(corpus, removeNumbers)  #remove Numbers
inspect(corpus[1:5])

cleanset = tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#cleanset = tm_map(cleanset, removeWords, c('phone', '6t'))

incleanset = tm_map(cleanset, gsub, pattern = 'indians', replacement = 'india') 

cleanset = tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])

#Term document matrix

tdm = TermDocumentMatrix(cleanset)
tdm
tdm = as.matrix(tdm)
tdm[1:10, 1:20]


distMatrix = dist(tdm, method = 'euclidean')  #Eucledian is best , others have pre and post conditions to be met
head(distMatrix)

dendo = hclust(distMatrix, method = "ward.D")
plot(dendo)

#----------------------------------


mydata = read.csv("binary.csv", header = TRUE)  #Admission 
str(mydata)
pairs(mydata)

#Scatter
plot(mydata$gre~ mydata$gpa, data = mydata)

#Calculate distance matrix (default is Euclidean distance)
distance = dist(mydata)

#Hirearchical agglomerative clustering using default complete linkage
mydata.hclust = hclust(distance)
plot(mydata.hclust,)
plot(mydata.hclust, labels = mydata$admit, main = 'Default from hclust')
plot(mydata.hclust, hang = 1)
mydata.hclust
mydata.hclust$order
mydata.hclust$labels


#Hirearchical agglomerative clustering using average linkage
mydata.hclust = hclust(distance, method = "average")
plot(mydata.hclust, hang = 1)



#Cluster membership
member = cutree(mydata.hclust, 3)
table(member)

#Characterising Clusters
aggregate(mydata, list(member), mean)


#K-means clustering
kc = kmeans(mydata, 3)
kc


#Unsupervised Self-Organizing Maps
library(kohonen)

data = read.csv("binary.csv", header = TRUE)
x = scale(data[,-1])  #Normalize data
summary(x)


#SOM
set.seed(222)
g = somgrid(xdim = 4, ydim = 4, topo = "rectangular")

map = som(x, grid = g, alpha = c(0.05,0.01), radius =1)

plot(map)
map
map$unit.classif
map$codes
plot(map, type = 'count') #Whcih grp less number or higher numbers of items present
plot(map, type = 'dist.neighbours')  #Distance between the clusters

#Session --------------------------------------------------------------------------------

#--------------------------

library(neuralnet)

data = read.csv("binary.csv", header = TRUE)
str(data)

#Min - Max Normalization
data$gre = (data$gre - min(data$gre)) / (max(data$gre) - min(data$gre))
data$gpa = (data$gpa - min(data$gpa)) / (max(data$gpa) - min(data$gpa))
data$rank = (data$rank - min(data$rank)) / (max(data$rank) - min(data$rank))

#Data Partition 

library(psych)
set.seed(222)
ind = sample(2, nrow(data), replace = TRUE  , prob = c(0.7,0.3))
training = data[ind ==1, ]
testing = data[ind == 2,]

#Neural Networks

library(neuralnet)
set.seed(333)
n = neuralnet(admit~gre+gpa+rank, data=training, hidden = 1, err.fct = "sse", linear.output = FALSE)

#n = neuralnet(admit~gre+gpa+rank, data=training, hidden = c(2,1), err.fct = "sse", linear.output = FALSE)  #try this
#try hidden = c(2,1)
plot(n)

#Prediction
output = compute(n, training[, -1])
head(output$net.result)
head(training[1,])

#Node output calculations with sigmoid activation function
in4 = 0.0455 + (0.82344*0.7586206897) + (1.35186*0.8103448276) + (-0.87435*0.6666666667)   #0.0455 is the blue value ---(plot value * training[1])
out4 = 1 / (1 + exp(-in4))
in5 = -7.06125 + (8.5741*out4)
out5 = 1/(1+exp(-in5))


#Confusion Matrix & Misclassification Error - training data 
output = compute(n, training[,-1])
p1 = output$net.result
pred1 = ifelse(p1>0.5, 1, 0)
tab1 = table(pred1, training$admit)
tab1
1-sum(diag(tab1)) / sum(tab1)


#Session --------------------------


#--------------------------------------------------------------
#XGBOOST
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)

data = read.csv("binary.csv", header = T)
str(data)
data$rank = as.factor(data$rank)


#partition data 
set.seed(1234)
ind = sample(2, nrow(data), replace = TRUE  , prob = c(0.8,0.2))
train = data[ind ==1, ]
test = data[ind == 2,]


#create matrix - One-Hot Encoding for Factor variable 
trainm = sparse.model.matrix(admit~. , -1, data=train)
head(trainm)

train_label = train[,"admit"]
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label = train_label)
testm = sparse.model.matrix(admit~., -1, data=test)
test_label = test[,"admit"]
test_matrix = xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Parameters
nc = length(unique(train_label))
xgb_params = list("objective" = "multi:softprob", 
                  "eval_metric" = "mlogloss",
                  "num_class" = nc)
wtlist = list(train = train_matrix, test = test_matrix)

#extreme Gradient Boosting Model
bst_model = xgb.train(params = xgb_params, data = train_matrix, nrounds = 100, watchlist = wtlist )

#Training and test error plot
e = data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
min(e$test_mlogloss)
e[e$test_mlogloss == 0.604529,]


#------change eta-------------
#extreme Gradient Boosting Model
bst_model = xgb.train(params = xgb_params, data = train_matrix, nrounds = 100, watchlist = wtlist, eta = 0.3) #reduce to 0.001 in steps
#Change eta value, it should be deceraed ... 
#Training and test error plot
e = data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
min(e$test_mlogloss)  #eta decreases ..this value increases ...it should increase 
e[e$test_mlogloss == 0.604529,]
#------------------------------

#max.depth = number of trees, default 6
#gamma = higher value > less overfitting
#subsample = percentage sampling
#missing = NA, for handling missing values
#seed = for randomization control 


##########################################################

#Freature importance 
imp = xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)
# Prediction and confusion matrix-  test data
p = predict(bst_model, newdata = test_matrix)
pred = matrix(p, nrow = nc, ncol = length(p) / nc) %>%
       t() %>%
       data.frame() %>%
       mutate(label = test_label, max_prob = max.col(., "last") - 1)

table(Prediction = pred$max_prob, Actual = pred$label)




#SESSION ----------------------------------
library(keras)
install_keras()

data = read.csv("Cardiotocographic.csv",header = TRUE)
str(data)

#Change to matrox
data = as.matrix(data)
dimnames(data) = NULL

#Normalize
data[, 1:21] = normalize(data[,1:21])
data[,22] = as.numeric(data[,22]) -1
summary(data)

#data Partition
set.seed(1234)
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))

training = data[ind == 1, 1:21]
test = data[ind == 2, 1:21]

trainingtarget = data[ind == 1, 22]
testtarget = data[ind ==2, 22]

#One Hot Encoding
trainLabels = to_categorical(trainingtarget)

testLabels = to_categorical(testtarget)

print(testLabels)


#Create Sequential model 
model = keras_model_sequential()
model %>%
          layer_dense(units=8, activation = 'relu', input_shape = c(21)) %>%
          
  
#H20AI ---company ---data free
  



