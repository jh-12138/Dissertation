library(faux)
library(brglm2)
library(actuar)
library(MASS)


simulation_log<-function(n,beta){ 
  
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
  dat[,"z1"]<-ifelse(dat[,"z1"]>1.6,1,0) ## around 3% of 1s.  mean 0.07777778
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
  
  ## offsets
  psi<-rztpois(n, 1.6)
  
  
  z=log(0.1)+beta*dat[,"x1"]+0.69*dat[,"x2"]-0.69*dat[,"x3"]+0.69*dat[,"x4"]+
    0.35*dat[,"x5"]-0.35*dat[,"x6"]
  
  y_log<-rpois(n, exp(z)*psi)
  
  log_data<-cbind(y_log,dat)
  
  
}



rep<-1000 ## 1000 data 
n<-600


beta<-log(8)

## create a list to store the data
data.sim.log<-list()
set.seed(10)

for(i in 1:rep){
  data.sim.log[[i]]<-simulation_log(n,beta)
}

## lists for regression outputs and se. 


fit<-list()
se<-list()

for (i in 1:rep){
  
  fit[[i]]<-glm(y_log~.,data =
                  as.data.frame(data.sim.log[[i]]),
                family = poisson(link="log"),method="brglmFit")
  
  se[[i]]<-summary(fit[[i]])$coefficients[, 2]['x1']
  
}

## count the number of separation 
separation<-length(which(as.numeric(se)>200))


confidence<-list()
lower<-list()
upper<-list()

count<-0


coverage_fn<-function(se,rep,confidence,lower,upper, beta){
  
    se<-as.numeric(se)
    
    for (i in 1:rep){
      
      confidence[[i]]<-confint(fit[[i]],'x1')
      lower[i]<-confidence[[i]][1]
      upper[i]<-confidence[[i]][2]
      
      
      if ((lower[i] <= beta & beta <= upper[i])){
        count <- count + 1
      }
      
      
      
    }
    
    coverage <- count / rep
    
    return(coverage)
  }
  
 

coverage.firth<-coverage_fn(se,rep,confidence,lower,upper,beta)


estimate<-list()

RMSE.firth_fn<-function(estimate,fit){ 
  
  for (i in 1:rep){
    
    estimate[i]<-summary(fit[[i]])$coefficients[, 1]['x1']
  }
  
  estimate<-as.numeric(estimate)
  RMSE<-sqrt((sum((estimate-beta)^2)/rep))
  
  return(RMSE)
  
}

RMSE.firth<-RMSE.firth_fn(estimate,fit)


## predictions


pred<-list()
diff<-list()
RMSE.pred<-list()


RMSE.firth.pred_fn<-function(pred,diff,RMSE.pred,fit){ ## the full data
  
  for (i in 1:rep){
    
    pred[[i]]<-fit[[i]]$fitted
    diff[[i]]<-pred[[i]]-data.sim.log[[i]][,"y_log"]
    RMSE.pred[[i]]<-sum((diff[[i]])^2)/n
  }
  
  RMSE.pred<-sqrt(sum(unlist(RMSE.pred))/rep)
  
  return(RMSE.pred)
  
}

RMSE.firth.pred<-RMSE.firth.pred_fn(pred,diff,RMSE.pred,fit)



