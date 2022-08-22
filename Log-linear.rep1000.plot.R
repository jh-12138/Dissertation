## RMSE

par(mfrow=c(2,3)) ## adjust display formats 

op <- par(cex = 0.85)  
## 6 covariates 

## ML with n=180
ML.180<-c(11.26042,10.69251,9.759554,6.90347, 4.054887,1.627273,0.6872059,0.3893261,0.3625224)

ML.180<-ML.180*sqrt(180)
x<-c(-log(8),-log(6),-log(4),-log(2),0,log(2),log(4),log(6),log(8))

plot(x, ML.180,xaxt = 'n',
     type = "b", pch = 18,
    ylab = expression(paste("RMSE (", beta, ") *sqrt (n)")), xlab = "True beta", ylim=c(0,40),
    main="Log-Linear, k=6, n=180",col="red")

axis(side = 1, at = round(x,2),labels = T, las=2)

## Firth

Firth.180<-c(0.8953194,0.8372296,0.7966378,0.7406176,0.6485129,0.5027674,0.4305752,0.3771745,0.3557636)
Firth.180<-Firth.180*sqrt(180)

lines(x,Firth.180, type="b", col="blue" )

## add legend 
legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)



## ML with n=300

ML.300<-c(9.062883,8.111555,6.361278,3.519977, 1.239312,0.382787,0.2969301,0.5655647,0.2747214)

ML.300<-ML.300*sqrt(300)

plot(x, ML.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", beta, ") *sqrt (n)")), xlab = "True beta", ylim=c(0,40),
     main="Log-Linear, k=6, n=300",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

## Firth

Firth.300<-c(0.778771,0.7553186,0.7127694,0.6130188,0.4692529,0.3639272,0.2903573,0.2832081,0.2721151)
Firth.300<-Firth.300*sqrt(300)

lines(x,Firth.300, type="b", col="blue" )

## add legend 
legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)


## ML with n=600

ML.600<-c(5.9503,4.719247,2.916442,1.084925, 0.3537501,0.2544937,0.2156793,0.1911213,0.1871873)

ML.600<-ML.600*sqrt(600)

plot(x, ML.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", beta, ") *sqrt (n)")), xlab = "True beta", ylim=c(0,40),
     main="Log-Linear, k=6, n=600",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

## Firth 
Firth.600<-c(0.6931238,0.6642977,0.5783691,0.4405575,0.3359496,0.2491283,0.2138554,0.1898441,0.1862777)
Firth.600<-Firth.600*sqrt(600)

## plot 
lines(x,Firth.600, type="b", col="blue" )

## add legend 
legend("topright", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)



## 3 covariates

## ML with n=180
ML.3.180<-c(11.82893,11.43457,10.7132,8.469157,5.201952,2.962143,1.151386,0.3874012,0.3826154)
ML.3.180<-ML.3.180*sqrt(180)

## Firth
Firth.3.180<-c(1.016191,0.9026242,0.7994791,0.7497876,0.671862,0.569284,0.4475833,0.3674788,0.3650542)
Firth.3.180<-Firth.3.180*sqrt(180)

plot(x, ML.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", beta, ") *sqrt (n)")), xlab = "True beta", ylim=c(0,40),
     main="Log-Linear, k=3, n=180",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,Firth.3.180, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)



## n=300

ML.3.300<-c(10.49863,9.738971,8.420758,5.410978,2.567752,0.6726375,0.3153071,0.2906472,0.2730214)
ML.3.300<-ML.3.300*sqrt(300)

Firth.3.300<-c(0.8084011,0.7736345,0.758334,0.6971584,0.5504275,0.4177537,0.3032491,0.2839245,0.268523)
Firth.3.300<-Firth.3.300*sqrt(300)

plot(x, ML.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", beta, ") *sqrt (n)")), xlab = "True beta", ylim=c(0,40),
     main="Log-Linear, k=3, n=300",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,Firth.3.300, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)



##n=600

ML.3.600<-c(7.890365,6.539455,4.703183,1.468701,0.3999099,0.2896918,0.216653,0.199752,0.1898368)
ML.3.600<-ML.3.600*sqrt(600)

Firth.3.600<-c(0.7394814,0.7190317,0.6666786,0.4909899,0.369255,0.2803586,0.2138053,0.1977511,0.1885221)
Firth.3.600<-Firth.3.600*sqrt(600)

plot(x, ML.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", beta, ") *sqrt (n)")), xlab = "True beta", ylim=c(0,40),
     main="Log-Linear, k=3, n=600", col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,Firth.3.600, type="b", col="blue" )

legend("topright", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)





## Coverage 

par(mfrow=c(2,3))
par(op)  
op <- par(cex = 0.85)  


## n=180

cov.180<-c(0.952,0.947,0.951,0.963,0.954,0.941,0.892,0.887,0.865)
cov.180<-cov.180*100

plot(x, cov.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=6, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)

cov.180.firth<-c(0.894,0.902,0.908, 0.934,0.937,0.931,0.891,0.883,0.865)

cov.180.firth<-cov.180.firth*100

lines(x,cov.180.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.8)


## n=300

cov.300<-c(0.956,0.961,0.963,0.955,0.948,0.916,0.907,0.881,0.844)
cov.300<-cov.300*100

plot(x, cov.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)

cov.300.firth<-c(0.911,0.915,0.931,0.932 ,0.936,0.916,0.905,0.884,0.842)

cov.300.firth<-cov.300.firth*100

lines(x,cov.300.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.8)



## n=600

cov.600<-c(0.957,0.962,0.962,0.958,0.931,0.923,0.873,0.885,0.834)

cov.600<-cov.600*100

plot(x, cov.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=6, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)

cov.600.firth<-c(0.928,0.931,0.947, 0.942,0.926,0.921,0.876,0.885,0.834)
cov.600.firth<-cov.600.firth*100

lines(x,cov.600.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.8)



## 3 cov

## n=180
cov.3.180<-c(0.954,0.951,0.942,0.957,0.957,0.948,0.908,0.903,0.895)

cov.3.180<-cov.3.180*100

cov.3.firth.180<-c(0.881,0.895,0.896,0.917,0.934,0.939,0.912,0.907,0.898)

cov.3.firth.180<-cov.3.firth.180*100

plot(x, cov.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,cov.3.firth.180, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.8)




## n=300

cov.3.300<-c(0.949,0.942,0.944,0.954,0.952,0.93,0.93,0.904,0.863)

cov.3.300<-cov.3.300*100

cov.3.firth.300<-c(0.892,0.902,0.911,0.933,0.943,0.921,0.925,0.905,0.865)

cov.3.firth.300<-cov.3.firth.300*100


plot(x, cov.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)



lines(x,cov.3.firth.300, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth_Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.8)

## n=600

cov.3.600<-c(0.959,0.967,0.967,0.965,0.947,0.939,0.914,0.884,0.866)

cov.3.600<-cov.3.600*100



cov.3.firth.600<-c(0.93,0.931,0.935,0.952,0.935,0.933,0.913,0.884,0.866)

cov.3.firth.600<-cov.3.firth.600*100


plot(x, cov.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,cov.3.firth.600, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.8)




## Separation

par(mfrow=c(2,3))
par(op)  
op <- par(cex = 1)  

## n=180
sep.180<-c(616,535,424,196,63,9,1,0,0)
sep.180<-sep.180/1000

plot(x, sep.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-linear, k=6, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)

sep.firth<-rep(0,9)
lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)

## n=300

sep.300<-c(397,306,179, 50 ,5,0,0,1,0)
sep.300<-sep.300/1000

plot(x, sep.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)

## n=600

sep.600<-c(173,104,37,4,0,0,0,0,0)
sep.600<-sep.600/1000

plot(x, sep.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=6, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)


## 3 cov

## n=180
sep.3.180<-c(697,627,524,303,107,32,4,0,0)
sep.3.180<-sep.3.180/1000

plot(x, sep.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)


## n=300
  
sep.3.300<-c(549,455,323,123,25,1,0,0,0)
sep.3.300<-sep.3.300/1000

plot(x, sep.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)


## n=600

sep.3.600<-c(311,205,100,8,0,0,0,0,0)
sep.3.600<-sep.3.600/1000


plot(x, sep.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)




## prediction
par(mfrow=c(2,3))
par(op)  
op <- par(cex = 1)  

#k=6, n=180
pred.180<-c(0.6111489,0.6119364,0.6133202,0.6176015,0.6270537,0.6474972,0.6910128,0.7409832,0.7891857)
pred.180<-pred.180*sqrt(180)


pred.180.firth<-c(0.6118864,0.6126766,0.6140563,0.6183476,0.6278051,0.6481581,0.691613,0.7412917,0.7896578)
pred.180.firth<-pred.180.firth*sqrt(180)


plot(x, pred.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta", 
     ylim=c(8,11.5),main="Log-Linear, k=6, n=180",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,pred.180.firth, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)

## k=6, n=300

pred.300<-c(0.6213075,0.6221495,0.6237817,0.6287398,0.6396482,0.6600747,0.7172127, 0.7647993,0.8279969)
pred.300<-pred.300*sqrt(300)


pred.300.firth<-c(0.6215397,0.6223809,0.6240194,0.6289746,0.6398756,0.6603219,0.7173815,0.7649012,0.8282236)
pred.300.firth<-pred.300.firth*sqrt(300)



plot(x, pred.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta", 
     ylim=c(10.5,15.5),main="Log-Linear, k=6, n=300",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,pred.300.firth, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)


## k=6, n=600


pred.600<-c(0.6228792,0.6236518,0.625173,0.6301728,0.6412779,0.6641776,0.7277267,0.7880793,0.8705302)
pred.600<-pred.600*sqrt(600)


pred.600.firth<-c(0.6229439,0.6237158,0.6252396,0.6302411,0.64134,0.6642443,0.727743,0.7881003,0.8705313)
pred.600.firth<-pred.600.firth*sqrt(600)

plot(x, pred.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(15,22.5),main="Log-Linear, k=6, n=600",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,pred.600.firth, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)




#k=3, n=180

pred.3.180<-c(0.5433186,0.5438397,0.5448475,0.5481186,0.5556635,0.5712985,0.6083053,0.657812,0.7038346)

pred.3.180<-pred.3.180*sqrt(180)

pred.3.180.firth<-c(0.543674,0.5441977,0.5452011,0.5484697,0.5560388,0.5716693,0.6086856,0.658125,0.70420477)

pred.3.180.firth<-pred.3.180.firth*sqrt(180)


plot(x, pred.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta", 
     ylim=c(7,9.5),main="Log-Linear, k=3, n=180",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,pred.3.180.firth, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)





## k=3, n=300


pred.3.300<-c(0.5489439,0.5495565,0.550883,0.5548659,0.5629573,0.5812669,0.6200721,0.6720047, 0.7255504)

pred.3.300<-pred.3.300*sqrt(300)

pred.3.300.firth<-c(0.5490748,0.5496841,0.5510103,0.5550062,0.5630842,0.5813823,0.6202109,0.6721281,0.7256928)

pred.3.300.firth<-pred.3.300.firth*sqrt(300)


plot(x, pred.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta", 
     ylim=c(9,13),main="Log-Linear, k=3, n=300",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,pred.3.300.firth, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)



## k=3, n=600



pred.3.600<-c(0.5469601,0.5475896,0.5488409,0.5527974,0.5610122,0.5795522,0.6262497,0.6775101,0.7353209)

pred.3.600<-pred.3.600*sqrt(600)


pred.3.600.firth<-c(0.5469937,0.5476247,0.5488782,0.5528373,0.5610521,0.5795921,0.6262839,0.6775357,0.7353029)

pred.3.600.firth<-pred.3.600.firth*sqrt(600)


plot(x, pred.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta", 
     ylim=c(13,18.5),main="Log-Linear, k=3, n=600",col="red")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,pred.3.600.firth, type="b", col="blue" )

legend("topleft", legend=c("ML", "Firth"),
       col=c("red", "blue"), lty=1, cex=0.8)





