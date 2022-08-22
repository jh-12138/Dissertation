## Logistic 
library(logistf)
library(faux)

simulation_logit<-function(n,beta){ ##function that takes n and beta as input and output the data. 
    
    ## simulate the random numbers
  
  dat1 <- rnorm_multi(n, 
                      mu = c(0, 0, 0),
                      sd = c(1, 1, 1),
                      r = c(-0.25, 0.35, 0.55), 
                      varnames = c("z1", "z3", "z5"),                  
                      empirical = FALSE)
  
  dat2<- rnorm_multi(n, 
                     mu = c(0, 0, 0),
                     sd = c(1, 1, 1),
                     r = c(-0.5, 0.2, 0.35), 
                     varnames = c("z2", "z4", "z6"),                  
                     empirical = FALSE)
  
  dat<-cbind(dat1,dat2)
  dat<-as.matrix(dat)
  
  ## reordering to get the names in a correct order. 
  col.order <- c("z1","z2","z3","z4","z5","z6")
  dat<-dat[,col.order]
  
  
  ## binary
  dat[,"z1"]<-ifelse(dat[,"z1"]>1.8,1,0) ## around 3% of 1s.  mean 0.03333333
  dat[,"z3"]<-ifelse(dat[,"z3"]>1,1,0) ## 13 % of 1s. mean 0.1277778
  dat[,"z5"]<-ifelse(dat[,"z5"]>1,1,0) ## 15% of 1 s. mean  0.15
  
  ## ordinal 
  
  ## z2
  z2copy1<-z2copy2<-dat[,"z2"]
  z2copy1<-ifelse(z2copy1>0.8,1,0)
  z2copy2<-ifelse(z2copy2>1.2,1,0)
  z2<-z2copy1+z2copy2 ## 18% of non-zeros. mean 0.2777778
  
  
  
  ## z4
  
  z4copy1<-z4copy2<-z2copy2<-dat[,"z4"]
  z4copy1<-ifelse(z4copy1>0.8,1,0)
  z4copy2<-ifelse(z4copy2>1,1,0)
  z4<-z4copy1+z4copy2 ## 18% of non-zeros. mean 0.3277778
  
  ## z6
  
  z6copy1<-z6copy2<-dat[,"z6"]
  z6copy1<-ifelse(z6copy1>1,1,0)
  z6copy2<-ifelse(z6copy2>1.8,1,0)
  z6<-z6copy1+z6copy2 ## 13% of non-zeros mean 0.15
  
  ## sub them back to dat. 
  dat[,'z2']<-z2
  dat[,'z4']<-z4
  dat[,'z6']<-z6
  
  colnames(dat)<-c("x1","x2","x3","x4","x5","x6")
  
  ## model specification 
  
  ## arbitrary choosing x1,x3,x4
  z=log(0.1)+beta*dat[,"x1"]-0.69*dat[,"x3"]+0.69*dat[,"x4"]
  
  y_logit<-rbinom(n,1,1/(1+exp(-z)))
  
  logit_data<-cbind(y_logit,dat)
  
}


rep<-1000 ## 1000 data 
n<-600


## will try log(2),log(4),-log(2),-log(4)
beta<- log(8)

data.sim.logit<-list()
set.seed(10)

for(i in 1:rep){ ## simulation of the data 
  data.sim.logit[[i]]<-simulation_logit(n,beta)
}


## arbitrary choosing x1,x3,x4

name<-c("y_logit","x1","x3","x4")

newdata<-list()

for(i in 1:rep){
  newdata[[i]]<-data.sim.logit[[i]][,name]
}


## initialisations 

fit<-list()
se<-list()

for (i in 1:rep){
  
  ## regressions
  
  fit[[i]]<-logistf(y_logit~.,data =
                      as.data.frame(newdata[[i]]),pl = T,firth = T)
  
  se[[i]]<-sqrt(diag(vcov(fit[[i]])))["x1"]
}

## count the number of separation 
separation<-length(which(as.numeric(se)>200))

## coverage 

lower<-list()
upper<-list()

count<-0


coverage_fn<-function(se,rep,lower,upper, beta){
  
  se<-as.numeric(se)
  
  for (i in 1:rep){
    upper[i]<-fit[[i]]$ci.upper["x1"]
    
    lower[i]<-fit[[i]]$ci.lower["x1"]
    
    if ((lower[i] <= beta & beta <= upper[i])){
      count <- count + 1
      
    }
    
  }
  
  coverage <- count / rep
  
  return(coverage)
}



coverage<-coverage_fn(se,rep,lower,upper,beta)


##RMSE
estimate<-list()

RMSE_fn<-function(estimate,fit){ ## the full data
  
  for (i in 1:rep){
    
    estimate[i]<-fit[[i]]$coefficients["x1"]
  }
  
  estimate<-as.numeric(estimate)
  RMSE<-sqrt((sum((estimate-beta)^2)/rep))
  
  return(RMSE)
  
}

RMSE<-RMSE_fn(estimate,fit)



## predictions

pred<-list()
diff<-list()
RMSE.pred<-list()


RMSE.pred_fn<-function(pred,diff,RMSE.pred,fit){ ## the full data
  
  for (i in 1:rep){
    
    pred[[i]]<-fit[[i]]$predict
    diff[[i]]<-pred[[i]]-newdata[[i]][,"y_logit"]
    RMSE.pred[[i]]<-sum((diff[[i]])^2)/n
  }
  
  RMSE.pred<-sqrt(sum(unlist(RMSE.pred))/rep)
  
  return(RMSE.pred)
  
}

RMSE.pred<-RMSE.pred_fn(pred,diff,RMSE.pred,fit)














