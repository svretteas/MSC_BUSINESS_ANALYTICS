# MSc IN BUSINESS ANALYTICS - STATISTICS II - PROJECT II  


# packages
library(glmnet)
library(dplyr)
library(operators)
library(caret)
library(pscl)
library(car)
library(rpart)
library(rpart.plot)
library(class)
library(MASS)
library(caret)
library(rattle)
library(class)


# read the finaldata file from previous project - basic data cleaning operations have been performed 

dictionary <- read.csv(file.choose(), header = T, sep =";")
data<- read.csv(file.choose(), header = T, sep = ",")
#counties_df<- cbind.data.frame(data$fips,data$state_abbreviation,data$County,data$Response)
data<- data[,-c(1:9)]

# transform all  variables to numeric 
str(data)
head(data)
table(data$Response)

# Split dataframe 

trainIndex <- createDataPartition(data$Response, p=0.70, list=FALSE)
train_data <- data[ trainIndex,]
test_data <- data[-trainIndex,]

set.seed(1)

#Perform 10 fold cross validation 20 times  - will be used in the below techniques

trainControl <- trainControl(method="repeatedcv", number=10, repeats=20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# LOGISTIC REGRESSION / CLASSIFICATION

# glm full model

full_model <- glm(Response~., data=data, family = binomial())  
summary(full_model) 


# stepwise AIC - method "both" 
aic_model <- step(full_model, direction = "both")

summary(aic_model) 

aic_model1<- glm(Response ~ +PST045214+PST040210+PST120214+AGE135214+AGE295214+AGE775214+SEX255214+RHI325214+RHI525214
                 +RHI625214+RHI625214+RHI725214+RHI825214+POP645213+EDU635213+EDU685213+EDU685213+VET605213
                 +HSG445213+HSD410213+INC910213+BZA010213+BZA110213+NES010213+SBO515207+POP060210, data = data, family = "binomial")

summary(aic_model1) 

# likelihood ratio test 
with(aic_model1, null.deviance - deviance) # 1525.267

with(aic_model1, df.null - df.residual) # 15 

with(aic_model1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # p-value 1.815751e-315

# exponentiate the coefficients and interpret them as odds-ratios
exp(coef(aic_model1))
exp(cbind(OR = coef(aic_model1), confint(aic_model1)))

# pseudoR2 
pR2(aic_model1)

# logistic model train 

model_log <- train(Response ~  +PST045214+PST040210+PST120214+AGE135214+AGE295214+AGE775214+SEX255214+RHI325214+RHI525214
                   +RHI625214+RHI625214+RHI725214+RHI825214+POP645213+EDU635213+EDU685213+EDU685213+VET605213
                   +HSG445213+HSD410213+INC910213+BZA010213+BZA110213+NES010213+SBO515207+POP060210, 
                     data=train_data, method="glm",  family = "binomial", trControl = trainControl)
model_log$results

print(model_log)

# Assessing the predictive ability of the model
fitted.results <- predict(model_log, test_data ,type='raw')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

preds_log <- fitted.results
preds_log

accuracy <- 1-mean(fitted.results != test_data$Response)
accuracy

# Estimate probabilites 
probabilities <- plogis(predict(model_log))
predicted.classes <- ifelse(probabilities > 0.5, 0, 1)
predicted.classes

# Accuracy 

accuracy_log <- confusionMatrix(as.factor(preds_log),as.factor(test_data$Response))
accuracy_log # 78.67%

# ROC LOGISTIC
library(ROCR)


pred1 <- prediction(preds_log,test_data$Response)
rocs1 <- performance(pred1, "tpr","fpr")

plot(rocs1, col = "black", main = "ROC Curves - Logistic Regression")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# DESICION  TREE / CLASSIFICATION   

tree <- rpart(Response~., data = train_data, method = 'class')

fancyRpartPlot(tree, main = "Method 2 - Decision Tree", sub = " ", type=2)

printcp(tree)

rpart.rules(tree, style = "tall")

tree$variable.importance

tree$cptable

# predict the tree 
preds_tree <-predict(tree, test_data, type = 'class')

# confusionMatrix 

accuracy_tree <- confusionMatrix(preds_tree,as.factor(test_data$Response))
accuracy_tree


# check table
table_tree <- table(test_data$Response, preds_tree)
table_tree

# check 
accuracy_tree1 <- sum(diag(table_tree)) / sum(table_tree)
accuracy_tree1 # 0.76



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SUPPORT VECTOR MACHINES / CLASSIFICATIONS  

# check dimensions 

dim(train_data); 
dim(test_data);

train_scale <- scale(train_data)

svm_linear <- train(as.factor(Response) ~., 
                    data = train_data,
                    method = "svmLinear",
                    trControl=trainControl)

svm_linear$call

preds_svm <- predict(svm_linear, test_data)

preds_svm

# confusionMatrix 
accuracy_svm_linear <- confusionMatrix(preds_svm,as.factor(test_data$Response))
accuracy_svm_linear # 0.80

# check table

table_svm <- table(test_data$Response, preds_svm)
table_svm

# check  
sum(diag(table_svm)) / sum(table_svm)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# MODEL COMPARISON 

# ROC 
library(ROCR)

preds_list<- list(preds_log,preds_svm,preds_tree)
m <- length(preds_list)
actual_list<- rep(list(test_data$Response), m)

pred <- prediction(preds_list,actual_list)
rocs <- performance(pred, "tpr","fpr")

plot(rocs, col = as.list(1:m), main = "ROC Curves - Comparison")
legend(x = "bottomright", legend = c("SVM","Logistic","DT"), fill = 1:m)



######################################################################################################################################################
# CLUSTERING 
######################################################################################################################################################

library(corrgram)
library(HDclassif)
library(cluster)
library(mclust)
library(FactMixtAnalysis)
library(nnet)
library(class)
library(tree)
library(factoextra)

# Split the dataframe 

not_economic_names <- list("PST045214", "PST040210", "PST120214", "POP010210",
                     
                         "AGE135214", "AGE295214", "AGE775214", "SEX255214",
                     
                          "RHI125214", "RHI225214", "RHI325214", "RHI425214",
                     
                         "RHI525214", "RHI625214", "RHI725214", "RHI825214",
                     
                          "POP715213", "POP645213", "POP815213", "EDU635213",
                     
                          "EDU685213", "VET605213")

excluded <- names(data) %in% not_economic_names

demographic <- data[!excluded]

demographic <- demographic[,-1]


# scale data 

scaled_data<-scale(demographic)

scaled_data = as.matrix(scaled_data)



#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(scaled_data, 
                                 k, 
                                 nstart=50,
                                 iter.max = 15 )$tot.withinss})
wss

# plot elbow 

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# K - MEANS 
set.seed(123)

kmm4 <- kmeans(scaled_data, centers = 4, nstart = 25)
print(kmm4)
str(kmm4)

# aggregate the mean 
dd_mean4 <- aggregate(scaled_data, by=list(cluster=kmm4$cluster), mean)

dd4<- cbind(scaled_data, cluster = kmm4$cluster)

#View(dd4)

# plot kmm4
# plot kmm5
clusplot(scaled_data,kmm4$cluster, 
         color=F, 
         shade=F,
         labels=0,
         lines=0, 
         main='k-Means Cluster Analysis')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



  

