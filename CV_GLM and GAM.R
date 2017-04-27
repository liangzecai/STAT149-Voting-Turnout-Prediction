library(mgcv); library(randomForest)

setwd("~/Desktop/Harvard/Spring 2017/Stat 149/Project")
source("myStep.R")
source("getCV.R")

#### Function to Calculate Discrepency Score ####
# Input: 
# y.true: factor vector of 'Y' and 'N'
# y.predict: numeric vector of probabilities

# Output: disprepancy score

model_score = function( y.predict, y.true){
        
        ## convert y.true to binary of 0/1
        #y.true = as.numeric(y.true) - 1
        
        ## score
        score = -mean(y.true * log(y.predict + 10^(-23)) + 
                              (1 - y.true) * log(1 - y.predict + 10^(-23)))
        
        return(score)
}


#read in the data
train <- read.csv("train.csv")
test <- read.csv("test.csv")


### need to specify the factor/ binary variables first
train$cd = as.factor(train$cd)
train$hd = as.factor(train$hd)
test$cd = as.factor(test$cd)
test$hd = as.factor(test$hd)


#specify model with all main effects
#bench.glm = glm(voted ~ gender + cd + hd + age + party + racename + hsonly + mrrg + 
                        chldprsnt + cath+ evang + nonchrst + otherchrst + days.since.reg, 
                family=binomial, data=train)


#data cleaning
# Remove all rows with at least one "NA" values
train=train[complete.cases(train[1:nrow(train),]),]

# count number of remaining observations
nrow(train)

#####################################
#Variable Selection (by LRT) for glm
####################################
#out = myStep(null.glm,data=train,K=14,fast=TRUE,test="Chisq",direction="forward")

#formula for the final model
#formula(out$fit)

#####################################
#Variable Selection (by LRT) for gam
#####################################
#library(mgcv)

#auto-generate gam formula (adding s(.); and putting max df if necessary 
#formula = getGAMformula(voted~., data=train)   
#formula 


#fit = gam(formula, data=train, family=binomial)
#out = myStep(fit,data=train,test="Chisq",direction="backward")

#Final Model: gam(formula = voted ~ s(dbdistance) + s(mrrg) + s(evang) + hd + 
#party + otherchrst + days.since.reg, family = binomial, data = train)


#################
#cross validation

#function for binaryPrediction (1/0)
binaryPrediction = function(data.train=NA,data.test,fit){ 
        ### get binary 0/1 prediction for methods lm/glm/gam/randomForest/rpart
        if((class(fit)[1])%in%c("lm","glm","gam")){
                out = c(predict(fit,data.test,type="response")>0.5)+0 
        }
        if((class(fit)[1])%in%c("svm.formula","svm","nnet","nnet.formula")){
                out = c(predict(fit,data.test)>0.5)+0 
        }
        if((class(fit)[1])%in%c("randomForest.formula","randomForest","rpart")){
                out = c(predict(fit,data.test,type="prob")[,"1"]>0.5)+0
        }
        out
}

#function for probability Prediction (between 0 and 1)
prPrediction = function(data.train=NA,data.test,fit){ 
        ### get binary 0/1 prediction for methods lm/glm/gam/randomForest/rpart
        if((class(fit)[1])%in%c("lm","glm","gam")){
                out = c(predict(fit,data.test,type="response"))+0 
        }
        if((class(fit)[1])%in%c("svm.formula","svm","nnet","nnet.formula")){
                out = c(predict(fit,data.test))+0 
        }
        if((class(fit)[1])%in%c("randomForest.formula","randomForest","rpart")){
                out = c(predict(fit,data.test,type="prob")[,"1"])+0
        }
        out
}



## convert response var. to binary of 0/1
train$voted = as.numeric(train$voted) - 1

#define the methods
method = c("glm(F)","gam(R)","RF(-hd)","RF(R)")
fHat = rep(list(NA),length(method))
names(fHat) = method
fit=fHat

#need to re-level the hd variable, so I used a simple glm here
fit[[1]] = function(data.train) glm(voted~., data=data.train, family=binomial)
#I used the gam model after stepwise model selection (reduced)
fit[[2]] = function(data.train) gam(voted ~ s(dbdistance) + s(mrrg) + s(evang) + hd + 
        party + otherchrst + days.since.reg, family = binomial, data = data.train)
#I used the random forest w/ all predictors excluding hd 
fit[[3]] = function(data.train) randomForest(as.factor(voted)~.-hd, data = data.train)
#I used the random forest w/ final model from gam(reduced model)
fit[[4]] = function(data.train) randomForest(as.factor(voted) ~ dbdistance + mrrg + evang+
                                                     I(dbdistance^2) + I(mrrg^2) + I(evang^2) +  
                                        party + otherchrst + days.since.reg, data = data.train)


#if don't want to convert to 0 and 1, remove the >0.5+0 at the end, and set classfication to F
#for(i in 1:length(method)){
#        fHat[[i]] = eval(substitute(function(...){prPrediction(fit=fit[[iLocal]](data.train), ...)}), list(iLocal=i))
#}
fHat[[1]] = function(data.train,data.test) prPrediction(fit=fit[[1]](data.train),data.train=data.train,data.test=data.test)
fHat[[2]] = function(data.train,data.test) prPrediction(fit=fit[[2]](data.train),data.train=data.train,data.test=data.test)
fHat[[3]] = function(data.train,data.test) prPrediction(fit=fit[[3]](data.train),data.train=data.train,data.test=data.test)
fHat[[4]] = function(data.train,data.test) prPrediction(fit=fit[[4]](data.train),data.train=data.train,data.test=data.test)

# fHat[[1]](data.train=NA,data.test=train[1:10,])

#10 fold cross validation
out = getCV(voted~.,data.train=train,fHat=fHat,K=10,loss=model_score,classification=F)
out 

#prediction accuracy comparison
#rowSums((out[[3]]/1000)[,2:3])