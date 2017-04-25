#our loss function
loss1 = function(yHat,y){
        e = 1e-5
        -(y*log(pmax(pmin(1-yHat,1-e),e))+(1-y)*log(pmax(pmin(yHat,1-e),e)))
}

#set.seed(1)
expit = function(t) 1/(1+exp(-t))
n = 2000
x1 = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
x4 = rnorm(n)
eta0 = 1-exp(sin(x1))+cos(x2)*(x2/5)^3+exp(x3-2)^(1/3)-(x4/4)^2*cos(x4) #+ x1*x2 - x2*x3 + 3*x1*x4 #(-100+x1^3*cos(x1*pi)+x2^4+2*(x3-5)^3+x4)/250 #
mu = expit(eta0)
y = rbinom(n,1,mu)
data = data.frame(cbind(y=y,x1=x1,x2=x2,x3=x3,x4=x4))

fit1 = function(data.train) glm(y~., data=data.train, family=binomial)
fit2 = function(data.train) glm(y~.*., data=data.train, family=binomial)
fit3 = function(data.train) gam(y~s(x1)+s(x2)+s(x3)+s(x4), data=data.train, family=binomial)
fit4 = function(data.train) randomForest(as.factor(y)~., data=data.train)
#take a look at the estimated value and the confidence interval
par(mfrow=c(2,2)); plot(fit3(data),scale=0,shade=TRUE)

#if don't want to convert to 0 and 1, remove the >0.5+0 at the end, and set classfication to F
fHat1 = function(data.train,data.test){c(predict(fit1(data.train),data.test,type="response")>.5)+0}
fHat2 = function(data.train,data.test){c(predict(fit2(data.train),data.test,type="response")>.5)+0}
fHat3 = function(data.train,data.test){c(predict(fit3(data.train),data.test,type="response")>.5)+0}
fHat4 = function(data.train,data.test){c(predict(fit4(data.train),data.test))-1}

fHat = list(fHat1=fHat1, fHat2=fHat2, fHat3=fHat3, fHat4=fHat4)

out = getCV(y~.,data,fHat,K=5,loss=loss1,classification=T)
out 

#prediction accuracy comparison
rowSums((out[[3]]/1000)[,2:3])




getCV = function(formula, data.train, fHat, para=NA, K=10, data.test=NULL, loss=NULL, classification=FALSE, plot=TRUE, print=TRUE, log=FALSE, ...){
	t0 = Sys.time()
	###--------------
	varName = all.vars(formula)
	yName = varName[1]
	if(varName[2]=="."){  
		xName = colnames(data.train)[colnames(data.train)!=yName]  
	}else{
		xName = varName[-1]
	}
	###--------------
	if(is.null(loss)){  loss=function(fHat,f){(fHat-f)^2}  }
	if(is.null(data.test)){
		n = nrow(data.train)
		I = matrix(c(sample(1:n,n,replace=TRUE),rep(NA,ceiling(n/K)*K-n)),nc=K)
		main = paste0(K,"-fold Cross Validation")
	}else{
		main = paste0("Out-sample Testing")
	}
	if(is.na(para[1])){
		if(!is.list(fHat)){
			fHat = list(fHat)
		}
		M = length(fHat)
		type = 2
	}else{
		M = length(para)
		type = 1
	}
	mspe = rep(0,M)
	if(classification){
		temp1 = unique(data.train[,yName])
		if(!is.null(data.test)){
			temp2 = unique(data.test[,yName])	
		}else{
			temp2 = NULL
		}
		temp12 = unique(c(temp1,temp2))
		classError = array(0,dim=c(M,length(temp12)^2))
		nameError = outer(paste0("Pred=",temp12), paste0("True=",temp12),paste,sep="/")
		colnames(classError) = c(nameError)
	}
	###--------------
	count = 0
	for(m in 1:M){
		if(!is.null(data.test)){
			if(type==1){
				if(sum(formalArgs(fHat)=="formula")==0){
					yPred = fHat(data.train=data.train,data.test=data.test[,xName],para=para[m],...)
				}else{
					yPred = fHat(formula=formula,data.train=data.train,data.test=data.test[,xName],para=para[m],...)
				}
			}
			if(type==2){
				if(sum(formalArgs(fHat[[m]])=="formula")==0){
					yPred = fHat[[m]](data.train=data.train,data.test=data.test[,xName],...)	
				}else{
					yPred = fHat[[m]](formula=formula,data.train=data.train,data.test=data.test[,xName],...)	
				}
			}
			mspe[m] = mean(loss(yPred, data.test[,yName]))
			if(classification){
				temp = table(yPred+0, data.test[,yName]+0)
				nameError = c(outer(paste0("Pred=",rownames(temp)), paste0("True=",colnames(temp)),paste,sep="/"))
				classError[m,nameError] = c(temp)
			}
		}else{
			for(k in 1:K){
				ITest = c(na.omit(I[,k]))
				xTest = data.train[ITest,xName]
				yTest = data.train[ITest,yName]
				data.train.CV = data.train[-ITest,]
				if(type==1){
					if(sum(formalArgs(fHat)=="formula")==0){
						yPred = fHat(data.train=data.train.CV,data.test=xTest,para=para[m],...)
					}else{
						yPred = fHat(formula=formula,data.train=data.train.CV,data.test=xTest,para=para[m],...)
					}
				}
				if(type==2){
					if(sum(formalArgs(fHat[[m]])=="formula")==0){
						yPred = fHat[[m]](data.train=data.train.CV,data.test=xTest,...)	
					}else{
						yPred = fHat[[m]](formula=formula,data.train=data.train.CV,data.test=xTest,...)	
					}
				}
				mspe[m] = mspe[m] + mean(loss(yPred, yTest))/K
				if(classification){
					temp = table(yPred+0, yTest+0)
					nameError = c(outer(paste0("Pred=",rownames(temp)), paste0("True=",colnames(temp)),paste,sep="/"))
					classError[m,nameError] = classError[m,nameError] + c(temp)/K
				}
			}
		}
		if(m==1){
			cat("/// Progress ///\n");cat(paste0(paste0(paste(rep("-",9),collapse=""),c(1:9,"|")),collapse=""));cat("\n")
		}else{
			add = floor(m/M*100)-count
			count = floor(m/M*100)
			cat(paste0(rep(">",add),collapse=""))
		}
	}
	cat("\n\n")
	I = which(mspe==min(mspe))
	if(type==1){ 
		out = cbind(para=para,mspe=mspe) 
		rownames(out) = paste0("para=",para)
		opt = para[I]
	}
	if(type==2){ 
		if(is.null(names(fHat))){
			names(fHat) = paste0("fHat",1:M)
		}
		out = cbind(method=1:M,mspe=mspe)
		rownames(out) = paste0("method=",names(fHat))
		opt = names(fHat)[I]
	}
	if(classification){
		rownames(classError) = rownames(out)
	}
	###--------------
	if(plot){
		par(mar=c(4,5,2,2),cex=1.2,cex.main=1.2,cex.axis=1.2,cex.lab=1.2)
		if(log){
			yPlot = log(mspe)
			ylab = "log(Estimated mspe)"
		}else{
			yPlot = mspe
			ylab = "Estimated mspe"
		}
		
		if(type==1){
			plot(out[,1], yPlot, type="l", xlab="Tuning Parameter", ylab=ylab, main=main,lwd=2)
			abline(v=seq(from=range(out[,1])[1],to=range(out[,1])[2],length=10),
				h=seq(from=range(yPlot)[1],to=range(yPlot)[2],length=10),lwd=0.1)
			abline(v=opt,col="red",lty=2)
		}
		if(type==2){
			ylim = log(range(mspe)*c(1,1.0))*log+range(mspe)*c(1,1.0)*(1-log)
			plot(yPlot, type="h", xlab="Method", ylab=ylab, main=main,lwd=2,xaxt="n",ylim=ylim)
			abline(h=seq(from=range(yPlot)[1],to=range(yPlot)[2],length=10),lwd=0.1)
			axis(1,at=1:M,names(fHat),las=1)
			points(I,yPlot[I],col="red",pch=1,cex=2,lwd=3)
			text(1:m,yPlot,rank(mspe),pos=4,col="red")
		}
	}
	if(print){
		cat(paste0("Call: \n",main,"\n\n"))
		if(type==1){
			cat(paste0("Est. Opt. Para:   ",paste(round(opt,3),collapse=", "),"\n"))
		}
		if(type==2){
			cat(paste0("Est. Opt. Method: ",paste(opt,collapse=", "),"\n"))	
		}
		cat(paste0("Est. mspe:        ",round(mspe[I[1]],3),"\n"))
		cat(paste0("Time Spent:       ",round(as.numeric(Sys.time()-t0,units="mins"),3)," mins",sep=""))
	}
	###--------------
	if(classification){
		OUT = list(mspe=out, opt=opt, classError=classError)
	}else{
		OUT = list(mspe=out, opt=opt)
	}
	OUT 
}
