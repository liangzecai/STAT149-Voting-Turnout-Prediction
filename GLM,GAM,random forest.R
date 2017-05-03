library(e1071); library(mgcv); library(nnet); library(randomForest)
setwd("~/Desktop/Harvard/Spring 2017/Stat 149/Project")


#1)
#read in the raw data (ignoring missing values)
train <- read.csv("train.csv")
test <- read.csv("test.csv")


### need to specify the factor/ binary variables first
train$cd = as.factor(train$cd)
train$hd = as.factor(train$hd)

test$cd = as.factor(test$cd)
test$hd = as.factor(test$hd)


#data cleaning
# Remove all rows with at least one "NA" values
train=train[complete.cases(train[1:nrow(train),]),]

# count number of training obs.
nrow(train)

## convert response var. to binary of 0/1
train$voted = as.numeric(train$voted) - 1

dd =train
names(dd)[names(dd)=="days.since.reg"] = "daysSinceReg"

######################
#2)
#read in imputed data
train.full <- read.csv("train_full.csv")[1:5000,]
test.full <- read.csv("test_full.csv")

### need to specify the factor/ binary variables first
train.full$cd = as.factor(train.full$cd)
train.full$hd = as.factor(train.full$hd)
train.full$hd.city = as.factor(train.full$hd.city)
test.full$cd = as.factor(test.full$cd)
test.full$hd = as.factor(test.full$hd)
test.full$hd.city = as.factor(test.full$hd.city)

## convert response var. to binary of 0/1
train.full$voted = as.numeric(train.full$voted) - 1



######################
#3)
#read in hd-relevel data
train.full.relevel <- read.csv("relevel-train.full.csv")[1:5500,]
test.full.relevel <- read.csv("relevel-test.full.csv")

### need to specify the factor/ binary variables first
train.full.relevel$cd = as.factor(train.full.relevel$cd)
train.full.relevel$hd = as.factor(train.full.relevel$hd)
#train.full.relevel$city = as.factor(train.full.relevel$city)
test.full.relevel$cd = as.factor(test.full.relevel$cd)
test.full.relevel$hd = as.factor(test.full.relevel$hd)
#test.full.relevel$city = as.factor(test.full.relevel$city)

## convert response var. to binary of 0/1
train.full.relevel$voted = as.numeric(train.full.relevel$voted) - 1


dd =train.full.relevel
names(dd)[names(dd)=="days.since.reg"] = "daysSinceReg"


setwd("~/Desktop/Harvard/Spring 2017/Stat 149/Project")
source("myStep.R")
source("getCV.R")

#define loss function
loss1 = function(yHat,y){
        e = 1e-5
        -mean(y*log(pmax(pmin(yHat,1-e),e))+(1-y)*log(pmax(pmin(1-yHat,1-e),e)))
}

#cp optimizing function 
get.cp.opt = function(fit,method=c("oneSD","min","both")){ 
        ### get optimal cp for classification tree  
        m = (method[1]=="oneSD")+(method[1]=="min")*2
        if(method[1]=="both") m = 1:2
        threshold = sum(fit$cptable[which.min(fit$cptable[,"xerror"]),c("xerror","xstd")])
        I = fit$cptable[,"xerror"]<threshold 
        out1 = max(fit$cptable[I,"CP"])
        out2 = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"] 
        c("cp.opt(oneSD)"=out1,"cp.opt(minXerror)"=out2)[m]
}

#mtry optimizing function
get.mtry.opt = function(formula,data,print=TRUE,...){
        ### get optimal m.try for random forest 
        yName = all.vars(formula)[1]
        if(length(unique(data[,yName]))<=5) data[,yName] = as.factor(data[,yName])
        fit = randomForest(formula, data=data)
        xName = rownames(attr(fit$terms,"factors"))[-1]
        temp = rfcv(data[,xName],data[,yName],...)
        result = data.frame(cbind(nvars=temp$n.var, error.rate=temp$error.cv, opt=NA)[length(temp$n.var):1,])
        result[,3] = ""
        result[which.min(result[,2]),3] = "***"
        if(print){
                print(result)
        }
        result[which.min(result[,"error.rate"]),"nvars"]
}


#probability Prediction function 
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



### define lists for storing fitted models and prediction functions
###--------------------------------------------------------------------------------------------------------
method = c("gam(F)", "gam(-hd)", "gam(R-F)", "gam(R-hd)")
        #"rf(Def)",   "rf(Opt)")
        
        #"glm(F)", "glm(-hd)", "glm(R-F)", "glm(R-hd)", "glm(I)")
          # , , "svm(Main)")
fit    = rep(list(NA),length(method))
names(fit) = method
fHat = fit


### define fitted models (as functions of data.train)
# varSelection = function(data.train) myStep(fit[[1]](data.train), test="Chisq",direction="backward")
# varSelection2 = function(data.train) myStep(fit[[2]](data.train), test="Chisq",direction="backward")

varSelection = function(data.train) {f = glm(voted~., data=data.train, family=binomial);
              myStep(f, test="Chisq",direction="backward")}
varSelection2 = function(data.train) {f = glm(voted~.-hd, data=data.train, family=binomial);
       myStep(f, test="Chisq",direction="backward")}
#fit.opt = out$fit

#glm
# fit[[1]]  = function(data.train) glm(voted~., data=data.train, family=binomial) 
# fit[[2]]  = function(data.train) glm(voted~.-hd, data=data.train, family=binomial) 
# fit[[3]]  = function(data.train){ f = formula(varSelection(data.train)$fit); 
#                                 glm(f, data=data.train, family=binomial)}
# fit[[4]]  = function(data.train){ f = formula(varSelection2(data.train)$fit); 
#                                 glm(f, data=data.train, family=binomial)}
# fit[[5]] = function(data.train) { null = glm(voted~1, data=data.train, family=binomial);
#         f = myStep(null, K=2, test="Chisq",direction="forward", fast=TRUE, alpha=.05);
#         glm(formula(f$fit), data=data.train, family=binomial)}

#gam
fit[[1]]  = function(data.train) gam(getGAMformula(voted~., data.train), data=data.train, family=binomial)
fit[[2]]  = function(data.train) gam(getGAMformula(voted~.-hd, data.train), data=data.train, family=binomial)
fit[[3]]  = function(data.train) { f = getGAMformula(varSelection(data.train)$fit); gam(f, data=data.train, family=binomial) }
fit[[4]]  = function(data.train) { f = getGAMformula(varSelection2(data.train)$fit); gam(f, data=data.train, family=binomial) }


#fit[[1]]  = function(data.train) randomForest(as.factor(voted)~.-hd, data=data.train)
#fit[[2]]  = function(data.train) randomForest(as.factor(voted)~.-hd, data=data.train, m.try=get.mtry.opt(voted~.-hd,data.train,step=.9))
#fit[[7]] = function(data.train) svm(voted~.-hd, data=data.train)


### define fHat[[1]] to fHat[[7]]
###--------------------------------------------------------------------------------------------------------
for(i in 1:length(method)){
        fHat[[i]] = eval(substitute(function(data.train,...){prPrediction(fit=fit[[iLocal]](data.train), data.train,...)}), list(iLocal=i))
}
# fit[[1]] = function(data.train){ print("enter stupid function"); out = gam(getGAMformula(voted~.-hd, data.train), data=data.train, family=binomial) ; print("leave"); out}
# 
# function(data.train) gam(voted ~ s(dbdistance) + s(mrrg) + s(evang) + hd + 
#                                  party + otherchrst + days.since.reg, family = binomial, data = data.train)


### cross validation 
###--------------------------------------------------------------------------------------------------------
set.seed(1)
#outCV = getCV(voted~.,data.train=train, K=3,fHat=fHat[c(1,3)],loss=model_score, classification=F)
#outCV = getCV(voted~.,data.train=train, K=10,fHat=fHat,loss=loss1, classification=F)
#outCV
outCV = getCV(voted~.,data.train=train, K=10,fHat=fHat,loss=loss1, classification=F)
outCV
#pCorrectCV = rowSums(outCV[[3]][,c(1,4)])/rowSums(outCV[[3]])
#cbind(outCV[[3]],"%(correct)"=round(100*pCorrectCV,1))


#summary of best gam model
vote.gam.reduced = fit[[3]](dd)
summary(vote.gam.reduced)
plot(vote.gam.reduced, residuals = TRUE, shade = TRUE, se = TRUE, 
        shade.col = 2, pages = 3,pch = '.')


#1) GAM 2.0
### prediction
predict.gam = predict(fit[[3]](train.full.relevel), newdata = test.full, type="response")
predict.gam = data.frame(Id = 1:length(predict.gam),voted = predict.gam)


