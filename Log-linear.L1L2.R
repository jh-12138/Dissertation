library(glmnet)
library(actuar)
library(faux)



simulation_log<-function(n,beta){ ##function that takes n and beta as input and output the data. 
    
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



rep<-200 ## 200 data sets
n<-600



beta<- log(8)


data.sim.log<-list()
set.seed(10)

for(i in 1:rep){ ## simulation of the data 
  data.sim.log[[i]]<-simulation_log(n,beta)
}


## initialise 

lasso<-list()
lasso.coef.log<-list()


for (i in 1:rep){
  
    ## regressions
    
  lasso[[i]] <- cv.glmnet(data.sim.log[[i]][,2:7], data.sim.log[[i]][,"y_log"],
                          alpha = 1, family=poisson(link="log"),nfolds = 3)
  
  lasso.coef.log[i]<-as.numeric(coef(lasso[[i]])["x1",])
  
}


## RMSE

RMSE<-sqrt((sum((unlist(lasso.coef.log)-beta)^2)/rep))



## prediction 

pred<-list()
RMSE.pred<-list()

for (i in 1:rep){
  pred[[i]] <- predict(lasso[[i]], s = lasso[[i]]$lambda.min, type="response", 
                       newx = data.sim.log[[i]][,2:7])
  RMSE.pred[i]<-sum(((data.sim.log[[i]][,"y_log"]-pred[[i]])^2))/n
  
}


RMSE.pred<-sqrt(sum(unlist(RMSE.pred))/rep)



## ridge


rep<-200 ## 200 data sets
n<-600



beta<- log(8)


data.sim.log<-list()
set.seed(10)

for(i in 1:rep){
  data.sim.log[[i]]<-simulation_log(n,beta)
}



ridge<-list()
ridge.coef.log<-list()


for (i in 1:rep){
  
  ridge[[i]] <- cv.glmnet(data.sim.log[[i]][,2:7], data.sim.log[[i]][,"y_log"],
                          alpha = 0, family=poisson(link="log"),nfolds = 3)
  
  ridge.coef.log[i]<-as.numeric(coef(ridge[[i]])["x1",])
  
}


RMSE<-sqrt((sum((unlist(ridge.coef.log)-beta)^2)/rep))


pred<-list()
RMSE.pred<-list()

for (i in 1:rep){
  pred[[i]] <- predict(ridge[[i]], s = ridge[[i]]$lambda.min, type="response", 
                       newx = data.sim.log[[i]][,2:7])
  RMSE.pred[i]<-sum(((data.sim.log[[i]][,"y_log"]-pred[[i]])^2))/n
  
}


RMSE.pred<-sqrt(sum(unlist(RMSE.pred))/rep)



