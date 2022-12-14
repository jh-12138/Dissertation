
## load the required data sets. 

library(brglm2)
library(logistf)
library(glmnet)


data<-endometrial ## get the data 

## for illustration purpose: contingency table 

xtabs(~NV + factor(HG == 0), data = data)
xtabs(~NV+factor(HG==1), data=data)



## logit model

logit<-glm(HG~., data=data, family="binomial")
summary(logit)


## move on to the log-linear 

## for this examplle, i use the lizards data


data.2<-lizards
data.2<-data.2[,-1] ## remove the first column 


## fit the regression 

log<-glm(opalinus~.,data=data.2, family = poisson(link="log")) 
summary(log)

## get a contigency table 


xtabs(~height+diameter+opalinus, data = data.2) ## contingency table 2


## data modifications
data.2$opalinus<-ifelse(data.2$height==">=5ft",0,data.2$opalinus[data.2$height=="<5ft"])

## re-fit the data

log.2<-glm(opalinus~.,data=data.2, family = poisson(link="log")) 
summary(log.2)


## firth

firth_log<-glm(opalinus~.,data=data.2, family = poisson(link="log"),method = "brglmFit") ## log-linear via Firth

summary(firth_log)


firth_logit<-logistf(HG~.,data =
                  data,firth = T) ## logistic Firth

summary(firth_logit)


## lasso logit


## initialise the data 

x_vars <- model.matrix(HG~., data)[,-1]
y_var <- data$HG

set.seed(10)
train = sample(1:nrow(x_vars), nrow(x_vars)/2) ## train the data
x_test = (-train)
y_test = y_var[x_test]


## get the optimal lambda for the logistic model 

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, 
                       nfolds = 10, family="binomial")

best_lam <- cv_output$lambda.min ## use the best lambda to fit the data

lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam, family = "binomial")

coef(lasso_best) ## get the coefficients


## ridge logit

x_vars <- model.matrix(HG~., data)[,-1]
y_var <- data$HG

set.seed(10)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 0, 
                       nfolds = 10, family="binomial")

best_lam <- cv_output$lambda.min

ridge_best <- glmnet(x_vars[train,], y_var[train], alpha = 0, lambda = best_lam, family = "binomial")

coef(ridge_best)


## lasso log-linear 

x_vars <- model.matrix(opalinus~., data.2)[,-1]
y_var <- data.2$opalinus

set.seed(10)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, 
                       nfolds = 10, family=poisson(link="log"))

best_lam <- cv_output$lambda.min

lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam, family = poisson(link="log"))

coef(lasso_best)



## ridge log-linear 

set.seed(10)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 0, 
                       nfolds = 10, family=poisson(link="log"))

best_lam <- cv_output$lambda.min

ridge_best <- glmnet(x_vars[train,], y_var[train], alpha = 0, lambda = best_lam, family = poisson(link="log"))

coef(ridge_best)


