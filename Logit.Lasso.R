## Logistic 
library(faux)
library(glmnet)

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
  
  z=log(0.1)+beta*dat[,"x1"]+0.69*dat[,"x2"]-0.69*dat[,"x3"]+0.69*dat[,"x4"]+
    0.35*dat[,"x5"]-0.35*dat[,"x6"]
  
  y_logit<-rbinom(n,1,1/(1+exp(-z)))
  
  logit_data<-cbind(y_logit,dat)
  
}



rep<-1000 
n<-600


beta<- log(8)


data.sim.logit<-list()
set.seed(10)

for(i in 1:rep){ ## simulation of the data 
  data.sim.logit[[i]]<-simulation_logit(n,beta)
}


## initialisations 

lasso<-list()
lasso.coef.logit<-list()


for (i in 1:rep){
  
  ## regressions 
  
  lasso[[i]] <- cv.glmnet(data.sim.logit[[i]][,2:7], data.sim.logit[[i]][,"y_logit"],
                          alpha = 1, family="binomial",nfolds = 10)
  
  lasso.coef.logit[i]<-as.numeric(coef(lasso[[i]])["x1",])
  
}

## RMSE

RMSE<-sqrt((sum((unlist(lasso.coef.logit)-beta)^2)/rep))


## predictions 

pred<-list()
RMSE.pred<-list()

for (i in 1:rep){
  pred[[i]] <- predict(lasso[[i]], s = lasso[[i]]$lambda.min, type="response", 
                       newx = data.sim.logit[[i]][,2:7])
  RMSE.pred[i]<-sum(((data.sim.logit[[i]][,"y_logit"]-pred[[i]])^2))/n
  
}


RMSE.pred<-sqrt(sum(unlist(RMSE.pred))/rep)



