## 6 covariates 

## RMSE of beta 

par(mfrow=c(2,3))
op <- par(cex = 1)  



x<-c(-log(8),-log(6),-log(4),-log(2),0,log(2),log(4),log(6),log(8))

## n=180
lasso.180<-c(1.951521,1.688144,1.308786,0.6700999,0.1291079,0.5775901,0.7772036,0.7792473,0.6247081)
lasso.180<-lasso.180*sqrt(180)

ridge.180<-c(1.818753,1.558985,1.190942,0.598318,0.2098724,0.5248908,0.8113468,0.8777425,0.816463)
ridge.180<-ridge.180*sqrt(180)

ml.180<-c(10.94889,10.37132,9.805062,6.520302,3.651975,1.315265,0.4392661,0.3906754,0.3748616)
ml.180<-ml.180*sqrt(180)

firth.180<-c(0.9260858,0.862477,0.7914552,0.7234598,0.6185161,0.5013887,0.4079031,0.3814964,0.3680849)

firth.180<-firth.180*sqrt(180)



plot(x, lasso.180,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",ylim=c(0,40),
     main="Log-Linear, k=6, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.180, type="b", col="green",pch=18)
lines(x,ml.180, type="b", col="red",pch=18)
lines(x,firth.180, type="b", col="blue")



legend("topright", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)


## n=300

lasso.300<-c(1.840597,1.59505,1.256677,0.6542296,0.1258523,0.5067989,0.4927103,0.4226177,0.3196468)
lasso.300<-lasso.300*sqrt(300)

ridge.300<-c(1.652267,1.40442,1.071028,0.5515193,0.2188498,0.4260091,0.5599841,0.5872857,0.5001401)
ridge.300<-ridge.300*sqrt(300)

ml.300<-c(9.355085,8.234705,7.04526,4.051569,0.5027828,0.3816401,0.280859,0.2785617,0.2472425)
ml.300<-ml.300*sqrt(300)


firth.300<-c(0.7891212,0.7677962,0.773164,0.6825324,0.4484726,0.3611451,0.2764824,0.2747708,0.2447382)

firth.300<-firth.300*sqrt(300)




plot(x, lasso.300,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",ylim=c(0,40),
     main="Log-Linear, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.300, type="b", col="green",pch=18)
lines(x,ml.300, type="b", col="red",pch=18)
lines(x,firth.300, type="b", col="blue")

legend("topright", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)



## n=300
lasso.600<-c(1.639666,1.442931,1.147951,0.6314358,0.08026556,0.4108446,0.2787678,0.2287978,0.20841)
lasso.600<-lasso.600*sqrt(600)


ridge.600<-c(1.430515,1.205621,0.8889029,0.4347767,0.1967256,0.2858243,0.3374195,0.3558793,0.3309832)
ridge.600<-ridge.600*sqrt(600)

ml.600<-c(5.708305,4.493684,3.355053,1.204839,0.3545048,0.2478855,0.2105742,0.1837783,0.1867422)
ml.600<-ml.600*sqrt(600)


firth.600<-c(0.6703805,0.6343368,0.5850425,0.4541827,0.3355766,0.2437896,0.2088764,0.1823625,0.1857431)

firth.600<-firth.600*sqrt(600)



plot(x, lasso.600,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",ylim=c(0,40),
     main="Log-Linear, k=6, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.600, type="b", col="green",pch=18)
lines(x,ml.600, type="b", col="red",pch=18)
lines(x,firth.600, type="b", col="blue")

legend("topright", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




## 3 cov

## n=180

lasso.3.180<-c(1.98606,1.717983,1.336171,0.6816057,0.07216356,0.6552978,0.9354728,0.9576737,0.8140383)
lasso.3.180<-lasso.3.180*sqrt(180)

ridge.3.180<-c(1.927907,1.650442,1.336171,0.6218575,0.1296607,0.5995575,0.9756769,1.100987,1.039788)
ridge.3.180<-ridge.3.180*sqrt(180)

ml.3.180<-c(11.54456,11.08543,10.60345,8.150372,5.214474,2.855684,1.74537,0.4018163,0.360323)

ml.3.180<-ml.3.180*sqrt(180)

firth.3.180<-c(1.038052,0.9161264,0.7452166,0.6964287,0.6279525,0.5215047,0.4413911,0.3824285,0.3469679)

firth.3.180<-firth.3.180*sqrt(180)

plot(x, lasso.3.180,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",ylim=c(0,40),
     main="Log-Linear, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.180, type="b", col="green",pch=18)
lines(x,ml.3.180, type="b", col="red",pch=18)
lines(x,firth.3.180, type="b", col="blue")

legend(x=-0.7,y=35, legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)






## n=300

lasso.3.300<-c(1.94478,1.677406,1.298329,0.6900919,0.07503481,0.6064059,0.7267909,0.6263405,0.5074553)

lasso.3.300<-lasso.3.300*sqrt(300)

ridge.3.300<-c(1.812329,1.54632,1.176331,0.5967442,0.1627048,0.51084,0.7530357,0.8124947,0.7844622)
ridge.3.300<-ridge.3.300*sqrt(300)

ml.3.300<-c(10.63361,9.938893,8.644796,5.50937,2.524626,0.4177478,0.3176088,0.300336,0.2808343)
ml.3.300<-ml.3.300*sqrt(300)  

firth.3.300<-c(0.7897217,0.7673883,0.7895687,0.7415265,0.5270728,0.3850911,0.3052473,0.2945172,0.2754725)

firth.3.300<-firth.3.300*sqrt(300)

plot(x, lasso.3.300,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",ylim=c(0,40),
     main="Log-Linear, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.300, type="b", col="green",pch=18)
lines(x,ml.3.300, type="b", col="red",pch=18)
lines(x,firth.3.300, type="b", col="blue")

legend("topright", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)



## n=600

lasso.3.600<-c(1.885783,1.659992,1.297151,0.6845391,0.05340675,0.6009676,0.5502975,0.3805167,0.3417115)

lasso.3.600<-lasso.3.600*sqrt(600)

ridge.3.600<-c(1.691383,1.443162,1.087376,0.5296213,0.1432006,0.4212436,0.5651538,0.5225532,0.5190805)
ridge.3.600<-ridge.3.600*sqrt(600)

ml.3.600<-c(7.607963,6.191601,5.229121,1.929038,0.4255186,0.3080084,0.2359469,0.1951264,0.1898434)
ml.3.600<-ml.3.600*sqrt(600)

firth.3.600<-c(0.7183853,0.684366,0.6731771,0.4945807,0.3864485,0.2948048,0.232039,0.1932599,0.1885388)

firth.3.600<-firth.3.600*sqrt(600)

plot(x, lasso.3.600,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",ylim=c(0,40),
     main="Log-Linear, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.600, type="b", col="green",pch=18)
lines(x,ml.3.600, type="b", col="red",pch=18)
lines(x,firth.3.600, type="b", col="blue")

legend("topright", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)





## prediction 

par(mfrow=c(2,3))
op <- par(cex = 1)  

## k=6, n=180

lasso.pred.180<-c(0.6167655,0.6180108,0.6197322,0.6253342,0.6364042,0.6552919,0.6915597,0.7421625,0.8141259)
lasso.pred.180<-lasso.pred.180*sqrt(180)

ridge.pred.180<-c(0.6165037,0.6174653,0.6190255,0.624207,0.6344532,0.654388,0.6943075,0.749159,0.8297187)

ridge.pred.180<-ridge.pred.180*sqrt(180)


ml.180.pred<-c(0.6105137,0.6113088,0.6125199,0.6166977,0.6264176,0.6453339,0.683846,0.7339486,0.8010129)

ml.180.pred<-ml.180.pred*sqrt(180)



firth.180.pred<-c(0.6112119,0.6119953,0.6131922,0.6173503,0.6270744,0.6458077,0.684537,0.7339578,0.8015367)

firth.180.pred<-firth.180.pred*sqrt(180)


plot(x, lasso.pred.180,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta",ylim=c(8,11.5),
     main="Log-Linear, k=6, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.pred.180, type="b", col="green",pch=18)
lines(x,ml.180.pred, type="b", col="red",pch=18)
lines(x,firth.180.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




## n=300

lasso.pred.300<-c(0.6276931,0.6288101,0.630871,0.635966,0.6465364,0.6690924,0.7120395,0.7812715,0.8501484)
lasso.pred.300<-lasso.pred.300*sqrt(300)


ridge.300.pred<-c(0.6279504,0.6289258,0.6308899,0.6357701,0.6464081,0.6696472,0.7141291,0.7854641,0.8592142)
ridge.300.pred<-ridge.300.pred*sqrt(300)

ml.300.pred<-c(0.6254824,0.6263054,0.6280543,0.6328221,0.6432391,0.6664448,0.7094467,0.777982,0.8459875)
ml.300.pred<-ml.300.pred*sqrt(300)

firth.300.pred<-c(0.6257503,0.6265733,0.6283323,0.633094,0.6435211,0.6667279,0.7096567,0.7780466,0.8461794)

firth.300.pred<-firth.300.pred*sqrt(300)


plot(x, lasso.pred.300,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta",ylim=c(10.5,15.5),
     main="Log-Linear, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.300.pred, type="b", col="green",pch=18)
lines(x,ml.300.pred, type="b", col="red",pch=18)
lines(x,firth.300.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)



## n=600

lasso.pred.600<-c(0.6256692,0.6265166,0.6279266,0.6330774,0.6440701,0.6656779,0.7262896,0.7860189,0.8746135)
lasso.pred.600<-lasso.pred.600*sqrt(600)

ridge.600.pred<-c(0.62592,0.6267761,0.6281663,0.6331483,0.6439189,0.6660156,0.7282591,0.7912135,0.8859656)
ridge.600.pred<-ridge.600.pred*sqrt(600)

ml.600.pred<-c(0.6251527,0.6259881,0.6273379,0.632222,0.6430647,0.6649467,0.7260063,0.7857332,0.8723547)
ml.600.pred<-ml.600.pred*sqrt(600)

firth.600.pred<-c(0.6252237,0.6260574,0.6274077,0.6322883,0.643124,0.6650046,0.7260281,0.785756,0.8723685)

firth.600.pred<-firth.600.pred*sqrt(600)


plot(x, lasso.pred.600,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta",ylim=c(15,22.5),
     main="Log-Linear, k=6, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.600.pred, type="b", col="green",pch=18)
lines(x,ml.600.pred, type="b", col="red",pch=18)
lines(x,firth.600.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)


## 3 cov

## n=180 

lasso.3.180.pred<-c(0.5439532,0.5447126,0.5456482,0.5502619,0.5586602,0.5752378,0.6124174,0.6436472,0.6942317)
lasso.3.180.pred<-lasso.3.180.pred*sqrt(180)

ridge.3.180.pred<-c(0.5395017,0.5403556,0.5412608,0.5456192,0.5542744,0.5698122,0.6039592,0.6339982,0.6719321)
ridge.3.180.pred<-ridge.3.180.pred*sqrt(180)

ml.3.180.pred<-c(0.5424975,0.5431018,0.5437988,0.5474632,0.5555349,0.5718242,0.6107776,0.6429272,0.6929786)

ml.3.180.pred<-ml.3.180.pred*sqrt(180)

firth.3.180.pred<-c(0.542827,0.5434256,0.5440961,0.5477421,0.555802,0.5720601,0.6109335,0.6434584,0.6932382)

firth.3.180.pred<-firth.3.180.pred*sqrt(180)


plot(x, lasso.3.180.pred,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta",ylim=c(7,9.5),
     main="Log-Linear, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.180.pred, type="b", col="green",pch=18)
lines(x,ml.3.180.pred, type="b", col="red",pch=18)
lines(x,firth.3.180.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




## n=300


lasso.3.300.pred<-c(0.5488095,0.5496099,0.5511314,0.5560729,0.5636432,0.5816168,0.6219746,0.6634612,0.724856)
lasso.3.300.pred<-lasso.3.300.pred*sqrt(300)

ridge.3.300.pred<-c(0.5461107,0.5469395,0.548444,0.5531773,0.5608797,0.578844,0.6178932,0.655098,0.7074968)
ridge.3.300.pred<-ridge.3.300.pred*sqrt(300)

ml.3.300.pred<-c(0.5483285,0.5489678,0.5502457,0.5544384,0.5618659,0.5801367,0.6212131,0.6632897,0.7247126)
ml.3.300.pred<-ml.3.300.pred*sqrt(300)

firth.3.300.pred<-c(0.5484496,0.5490901,0.5503751,0.5545798,0.5620078,0.5802509,0.6213297,0.6633732,0.7247565)

firth.3.300.pred<-firth.3.300.pred*sqrt(300)

plot(x, lasso.3.300.pred,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta",ylim=c(9,13),
     main="Log-Linear, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.300.pred, type="b", col="green",pch=18)
lines(x,ml.3.300.pred, type="b", col="red",pch=18)
lines(x,firth.3.300.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




## n=600

lasso.3.600.pred<-c(0.5496023,0.5503662,0.5516439,0.5557748,0.5642962,0.5828889,0.6258584,0.6817309,0.7326397)
lasso.3.600.pred<-lasso.3.600.pred*sqrt(600)


ridge.3.600.pred<-c(0.5479561,0.5487726,0.5501178,0.5542606,0.5625228,0.5809841,0.6218456,0.6744877,0.7232791)
ridge.3.600.pred<-ridge.3.600.pred*sqrt(600)


ml.3.600.pred<-c(0.5493957,0.5501149,0.5512878,0.5552263,0.5635837,0.5826341,0.6257916,0.6816724,0.7320053)

ml.3.600.pred<-ml.3.600.pred*sqrt(600)

firth.3.600.pred<-c(0.5494307,0.5501505,0.5513237,0.5552595,0.563618,0.5826648,0.6258366,0.6816882,0.7320031)
firth.3.600.pred<-firth.3.600.pred*sqrt(600)

plot(x, lasso.3.600.pred,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", mu, ") * sqrt (n)")), xlab = "True beta",ylim=c(13,18.5),
     main="Log-Linear, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.600.pred, type="b", col="green",pch=18)
lines(x,ml.3.600.pred, type="b", col="red",pch=18)
lines(x,firth.3.600.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)



## rep 200 separation


par(mfrow=c(2,3))
op <- par(cex = 1) 

## n=180
sep.180<-c(117,101,86,35,10,1,0,0,0)
sep.180<-sep.180/200

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
sep.300<-c(83,62,43,13,0,0,0,0,0)
sep.300<-sep.300/200

plot(x, sep.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)

## n=600
sep.600<-c(32,19,10,1,0,0,0,0,0)
sep.600<-sep.600/200

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

sep.3.180<-c(134,119,104,57,22,6,2,0,0)
sep.3.180<-sep.3.180/200

plot(x, sep.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)


## n=300

sep.3.300<-c(112,94,67,25,5,0,0,0,0)
sep.3.300<-sep.3.300/200

plot(x, sep.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)


## n=600

sep.3.600<-c(58,37,25,3,0,0,0,0,0)
sep.3.600<-sep.3.600/200


plot(x, sep.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", ylim = c(0,1),
     main="Log-Linear, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.8)




## Coverage 

par(mfrow=c(2,3))
op <- par(cex = 1) 

## n=180

cov.180<-c(0.935,0.95,0.95,0.98,0.965,0.94,0.915,0.875,0.835)
cov.180<-cov.180*100

plot(x, cov.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=6, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


cov.180.firth<-c(0.89,0.875,0.915,0.93,0.935,0.91,0.915,0.875,0.84)

cov.180.firth<-cov.180.firth*100

lines(x,cov.180.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)


## n=300

cov.300<-c(0.96,0.955,0.94,0.94,0.95,0.915,0.93,0.89,0.9)
cov.300<-cov.300*100

plot(x, cov.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)

cov.300.firth<-c(0.905,0.92,0.905,0.91,0.925,0.915,0.935,0.89,0.905)

cov.300.firth<-cov.300.firth*100


lines(x,cov.300.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)



## n=600

cov.600<-c(0.965,0.97,0.985,0.95,0.95,0.905,0.865,0.92,0.845)

cov.600<-cov.600*100

plot(x, cov.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=6, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)

cov.600.firth<-c(0.925,0.935,0.985,0.95,0.94,0.91,0.88,0.925,0.85)
cov.600.firth<-cov.600.firth*100

lines(x,cov.600.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)



## 3 cov

## n=180

cov.3.180<-c(0.955,0.96,0.94,0.97,0.98,0.975,0.95,0.905,0.89)

cov.3.180<-cov.3.180*100


cov.3.firth.180<-c(0.88,0.88,0.915,0.925,0.975,0.965,0.95,0.895,0.9)

cov.3.firth.180<-cov.3.firth.180*100

plot(x, cov.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,cov.3.firth.180, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)




## n=300

cov.3.300<-c(0.93,0.945,0.945,0.945,0.95,0.935,0.915,0.915,0.845)

cov.3.300<-cov.3.300*100

cov.3.firth.300<-c(0.9,0.895,0.905,0.935,0.935,0.93,0.915,0.915,0.84)

cov.3.firth.300<-cov.3.firth.300*100


plot(x, cov.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)



lines(x,cov.3.firth.300, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth_Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)

## n=600

cov.3.600<-c(0.965,0.98,0.99,0.97,0.925,0.935,0.905,0.9,0.885)

cov.3.600<-cov.3.600*100



cov.3.firth.600<-c(0.94,0.94,0.96,0.965,0.91,0.93,0.905,0.9,0.885)

cov.3.firth.600<-cov.3.firth.600*100


plot(x, cov.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(75,100),
     main="Log-Linear, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,cov.3.firth.600, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)






