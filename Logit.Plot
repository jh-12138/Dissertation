
par(mfrow=c(2,3))
op <- par(cex = 1)  

x<-c(-log(8),-log(6),-log(4),-log(2),0,log(2),log(4),log(6),log(8))


lasso.180<-c(2.077186,1.790454,1.385071,0.6922847,0.04992328,0.6903551,1.341244,1.688884,1.93081)
lasso.180<-lasso.180*sqrt(180)

ridge.180<-c(2.063918,1.777325,1.373833,0.6854426,0.07120031,0.6880767,1.34598,1.714659,1.97347)
ridge.180<-ridge.180*sqrt(180)

ml.180<-c(12.62796,12.60027,12.29354,11.15492,9.518647,6.998525,5.4337,5.061832,5.012078)
ml.180<-ml.180*sqrt(180)

firth.180<-c(1.505178,1.322496,1.151707,0.9804684,0.9850847,0.9788669,0.9767536,0.9918935,0.9705761)

firth.180<-firth.180*sqrt(180)



plot(x, lasso.180,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",ylim=c(0,40),
     main="Logit, k=6, n=180",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.180, type="b", col="green")
lines(x,ml.180, type="b", col="red",pch=18)
lines(x,firth.180, type="b", col="blue")



legend(-0.69, 39, legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




##

lasso.300<-c(2.076648,1.789012,1.384967,0.6929979,0.0364076,0.687092,1.308429,1.599801,1.789169)
lasso.300<-lasso.300*sqrt(300)

ridge.300<-c(2.040939,1.755879,1.357136,0.3315793,0.07703287,0.6734762,1.306803,1.641732,1.871689)
ridge.300<-ridge.300*sqrt(300)

ml.300<-c(11.84202,11.65878,11.04605,9.373045,6.514057,3.853654,2.376289,2.097283,1.558966)
ml.300<-ml.300*sqrt(300)


firth.300<-c(1.111695,0.9892471,0.8905782,0.882405,0.8419836,0.7695517,0.7430916,0.7502088,0.6990281)

firth.300<-firth.300*sqrt(300)




plot(x, lasso.300,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(0,40),main="Logit, k=6, n=300",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.300, type="b", col="green")
lines(x,ml.300, type="b", col="red",pch=18)
lines(x,firth.300, type="b", col="blue")

legend(-0.69, 39, legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)



##
lasso.600<-c(2.069606,1.783678,1.380135,0.6915546,0.03657904,0.6716196,1.176877,1.307866,1.370111)
lasso.600<-lasso.600*sqrt(600)


ridge.600<-c(1.991408,1.71205,1.32357,0.6649763,0.08236586,0.6282895,1.185525,1.435651,1.583874)
ridge.600<-ridge.600*sqrt(600)

ml.600<-c(10.37991,9.731129,8.341024,5.388903,2.843546,1.036428,0.5461673,0.5512439,0.5346909)
ml.600<-ml.600*sqrt(600)


firth.600<-c(0.8436553,0.806696,0.8106243,0.7608723,0.6687922,0.5431841,0.5115046,0.5191098,0.5009582)

firth.600<-firth.600*sqrt(600)



plot(x, lasso.600,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(0,40),main="Logit, k=6, n=600",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.600, type="b", col="green")
lines(x,ml.600, type="b", col="red",pch=18)
lines(x,firth.600, type="b", col="blue")

legend("bottomleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)


## k=3


lasso.3.180<-c(2.079442,1.791759,1.386294,0.6931472,0.02337064,0.6914293,1.362095,1.719362,1.954658)
lasso.3.180<-lasso.3.180*sqrt(180)

ridge.3.180<-c(2.067052,1.77998,1.37538,0.6847569,0.05262185,0.6900569,1.359861,1.733551,1.986791)
ridge.3.180<-ridge.3.180*sqrt(180)

ml.3.180<-c(12.77102,12.80872,12.64697,12.01849,10.43179,8.123062,5.840258,5.432856,4.837461)

ml.3.180<-ml.3.180*sqrt(180)

firth.3.180<-c(1.698101,1.488999,1.257054,0.9658015,0.8975817,0.9186079,0.9218844,0.9213478,0.8945372)

firth.3.180<-firth.3.180*sqrt(180)

plot(x, lasso.3.180,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(0,40),main="Logit, k=3, n=180",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.180, type="b", col="green")
lines(x,ml.3.180, type="b", col="red",pch=18)
lines(x,firth.3.180, type="b", col="blue")

legend(-0.69, 39, legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)






##
lasso.3.300<-c(2.078567,1.790904,1.385749,0.6931472,0.0304408,0.6903384,1.347706,1.684798,1.888906)

lasso.3.300<-lasso.3.300*sqrt(300)

ridge.3.300<-c(2.058152,1.771421,1.369829,0.684329,0.06465594,0.689321,1.33965,1.694985,1.926257)
ridge.3.300<-ridge.3.300*sqrt(300)

ml.3.300<-c(12.12881,12.04161,11.72078,10.09742,7.897382,5.609669,2.625756,1.721766,1.830187)
ml.3.300<-ml.3.300*sqrt(300)  

firth.3.300<-c(1.293491,1.118441,0.9339789,0.8475307,0.8362621,0.8330443,0.7265585,0.6991012,0.6818751)

firth.3.300<-firth.3.300*sqrt(300)

plot(x, lasso.3.300,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(0,40),main="Logit, k=3, n=300",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.300, type="b", col="green")
lines(x,ml.3.300, type="b", col="red",pch=18)
lines(x,firth.3.300, type="b", col="blue")

legend(-0.69, 39, legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)



##
lasso.3.600<-c(2.079442,1.791759,1.386294,0.6931472,0,0.6896438,1.31046,1.546914,1.658564)

lasso.3.600<-lasso.3.600*sqrt(600)

ridge.3.600<-c(2.037821,1.75392,1.354189,0.6775782,0.05702534,0.665263,1.273511,1.566162,1.740651)
ridge.3.600<-ridge.3.600*sqrt(600)

ml.3.600<-c(10.9514,10.5617,9.684016,7.004083,4.053246,1.602386,0.7494279,0.7317507,0.5106381)
ml.3.600<-ml.3.600*sqrt(600)

firth.3.600<-c(0.9081527,0.8380468,0.8060486,0.7857509,0.727076,0.5849193,0.5212662,0.4963994,0.484886)

firth.3.600<-firth.3.600*sqrt(600)

plot(x, lasso.3.600,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", beta, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(0,40),main="Logit, k=3, n=600",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.600, type="b", col="green")
lines(x,ml.3.600, type="b", col="red",pch=18)
lines(x,firth.3.600, type="b", col="blue")

legend("bottomleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)





## Prediction

par(mfrow=c(2,3))
op <- par(cex = 1)  
## k=6

lasso.pred.180<-c(0.3302627,0.3305856,0.3313422,0.3331876,0.3356109,0.3385602,0.340108,0.3403269,0.3402194)
lasso.pred.180<-lasso.pred.180*sqrt(180)

ridge.pred.180<-c(0.3300142,0.3303363,0.3311028,0.3329733,0.3354374,0.3382908,0.3399992,0.3401821,0.3399246)

ridge.pred.180<-ridge.pred.180*sqrt(180)


ml.180.pred<-c(0.3268751,0.327128,0.3277354,0.3292631,0.3313337,0.3338943,0.335816,0.3361799,0.3360656)

ml.180.pred<-ml.180.pred*sqrt(180)



firth.180.pred<-c(0.3274511,0.327694,0.3282725,0.3297386,0.3317525,0.3342382,0.3361107,0.3364639,0.3363613)

firth.180.pred<-firth.180.pred*sqrt(180)


plot(x, lasso.pred.180,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", pi, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(4.37,4.65),main="Logit, k=6, n=180",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.pred.180, type="b", col="green")
lines(x,ml.180.pred, type="b", col="red",pch=18)
lines(x,firth.180.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




##

lasso.pred.300<-c(0.3304751,0.3308678,0.3316898,0.3335459,0.3361222,0.3384709,0.3401119,0.3403104,0.3402059)
lasso.pred.300<-lasso.pred.300*sqrt(300)


ridge.300.pred<-c(0.3304641,0.3308119,0.3315793,0.3334029,0.3359462,0.3383624,0.340146,0.3404151,0.3403771)
ridge.300.pred<-ridge.300.pred*sqrt(300)

ml.300.pred<-c(0.3292019,0.3294965,0.3301667,0.3317888,0.3341253,0.3365523,0.3384895,0.3388789,0.3388839)
ml.300.pred<-ml.300.pred*sqrt(300)

firth.300.pred<-c(0.3294261,0.3297133,0.3303753,0.3319693,0.3342718,0.3366645,0.3385835,0.3389708,0.3389732)

firth.300.pred<-firth.300.pred*sqrt(300)


plot(x, lasso.pred.300,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", pi, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(5.7,5.95),main="Logit, k=6, n=300",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.300.pred, type="b", col="green")
lines(x,ml.300.pred, type="b", col="red",pch=18)
lines(x,firth.300.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)



##

lasso.pred.600<-c(0.3321319,0.3325236,0.3333366,0.3352159,0.3376698,0.340179,0.3419993,0.3424041,0.3422912)
lasso.pred.600<-lasso.pred.600*sqrt(600)

ridge.600.pred<-c(0.3321296,0.332511,0.3332908,0.335129,0.3375743,0.3402031,0.3420989,0.342514,0.3423964)
ridge.600.pred<-ridge.600.pred*sqrt(600)

ml.600.pred<-c(0.331817,0.3321651,0.3328922,0.3346653,0.3371207,0.3397895,0.3417125,0.342136,0.3420244)
ml.600.pred<-ml.600.pred*sqrt(600)

firth.600.pred<-c(0.3318741,0.33222,0.3329428,0.3347065,0.3371501,0.3398134,0.3417338,0.3421564,0.342046)

firth.600.pred<-firth.600.pred*sqrt(600)


plot(x, lasso.pred.600,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", pi, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(8.1,8.5),main="Logit, k=6, n=600",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.600.pred, type="b", col="green")
lines(x,ml.600.pred, type="b", col="red",pch=18)
lines(x,firth.600.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




## 3 cov


lasso.3.180.pred<-c(0.3068329,0.3071007,0.3077247,0.3090709,0.3114492,0.3144251,0.3169158,0.3178908,0.3181646)
lasso.3.180.pred<-lasso.3.180.pred*sqrt(180)

ridge.3.180.pred<-c(0.3069138,0.3071916,0.3077944,0.3091613,0.3115496,0.3145338,0.3171964,0.3181734,0.3184775)
ridge.3.180.pred<-ridge.3.180.pred*sqrt(180)

ml.3.180.pred<-c(0.306052,0.3062881,0.3068153,0.308002,0.3101183,0.3128612,0.3154153,0.316484,0.3168142)

ml.3.180.pred<-ml.3.180.pred*sqrt(180)

firth.3.180.pred<-c(0.3064675,0.3066956,0.3072025,0.3083508,0.3104104,0.3130902,0.315584,0.3166328,0.3169486)

firth.3.180.pred<-firth.3.180.pred*sqrt(180)


plot(x, lasso.3.180.pred,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", pi, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(4.1,4.3),main="Logit, k=3, n=180",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.180.pred, type="b", col="green")
lines(x,ml.3.180.pred, type="b", col="red",pch=18)
lines(x,firth.3.180.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




##


lasso.3.300.pred<-c(0.3061968,0.3064972,0.3070637,0.3088062,0.3111336,0.3136414,0.3163545,0.3173098,0.3176329)
lasso.3.300.pred<-lasso.3.300.pred*sqrt(300)

ridge.3.300.pred<-c(0.3062759,0.3065736,0.307126,0.308844,0.3111416,0.3137153,0.316494,0.3175262,0.3178481)
ridge.3.300.pred<-ridge.3.300.pred*sqrt(300)

ml.3.300.pred<-c(0.3058716,0.3061381,0.3066344,0.3082023,0.3103647,0.3128959,0.3157305,0.3168229,0.3171722)
ml.3.300.pred<-ml.3.300.pred*sqrt(300)

firth.3.300.pred<-c(0.3060489,0.3063103,0.306801,0.3083432,0.3104792,0.3129827,0.3157921,0.3168747,0.3172243)

firth.3.300.pred<-firth.3.300.pred*sqrt(300)

plot(x, lasso.3.300.pred,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", pi, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(5.28,5.55),main="Logit, k=3, n=300",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.300.pred, type="b", col="green")
lines(x,ml.3.300.pred, type="b", col="red",pch=18)
lines(x,firth.3.300.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




##
lasso.3.600.pred<-c(0.3071387,0.3074549,0.3080376,0.3096701,0.311966,0.3149265,0.317615,0.3186723,0.318978)
lasso.3.600.pred<-lasso.3.600.pred*sqrt(600)


ridge.3.600.pred<-c(0.3071561,0.3074594,0.3080193,0.3096305,0.3119075,0.3149307,0.3176858,0.3187569,0.319063)
ridge.3.600.pred<-ridge.3.600.pred*sqrt(600)


ml.3.600.pred<-c(0.3070088,0.307293,0.3078254,0.3093801,0.3116458,0.3147089,0.3175045,0.3185809,0.3188875)

ml.3.600.pred<-ml.3.600.pred*sqrt(600)

firth.3.600.pred<-c(0.3070524,0.3073347,0.3078645,0.309412,0.3116695,0.3147255,0.3175146,0.3185899,0.3188959)
firth.3.600.pred<-firth.3.600.pred*sqrt(600)

plot(x, lasso.3.600.pred,xaxt = 'n',
     type = "b",
     ylab = expression(paste("RMSE (", pi, ") * sqrt (n)")), xlab = "True beta",
     ylim=c(7.5,7.85),main="Logit, k=3, n=600",pch=18)
axis(side = 1, at = round(x,2),labels = T, las=2)

lines(x,ridge.3.600.pred, type="b", col="green")
lines(x,ml.3.600.pred, type="b", col="red",pch=18)
lines(x,firth.3.600.pred, type="b", col="blue")

legend("topleft", legend=c("Lasso", "Ridge","ML","Firth"),
       col=c("black", "green","red","blue"), lty=1, cex=0.6)




## Separation


par(mfrow=c(2,3))
op <- par(cex = 1) 

sep.180<-c(868,831,749,567,381,191,107,87,90)
sep.180<-sep.180/1000

plot(x, sep.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", 
     ylim = c(0,1),
     main="Logit, k=6, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)

sep.firth<-rep(0,9)
lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.7)

##
sep.300<-c(767,714,608,402,179,58,19,14,7)
sep.300<-sep.300/1000

plot(x, sep.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", 
     ylim = c(0,1),
     main="Logit, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.7)

##
sep.600<-c(585,494,343,131,33,3,0,0,0)
sep.600<-sep.600/1000

plot(x, sep.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", 
     ylim = c(0,1),
     main="Logit, k=6, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.7)


## 3 cov

sep.3.180<-c(908,877,808,669,467,263,126,102,80)
sep.3.180<-sep.3.180/1000

plot(x, sep.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", 
     ylim = c(0,1),
     main="Logit, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.7)


##

sep.3.300<-c(828,783,703,478,269,124,24,9,11)
sep.3.300<-sep.3.300/1000

plot(x, sep.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", 
     ylim = c(0,1),
     main="Logit, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.7)


##

sep.3.600<-c(674,601,477,228,69,9,1,1,0)
sep.3.600<-sep.3.600/1000


plot(x, sep.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Separation % (", beta, ")")), xlab = "True beta", 
     ylim = c(0,1),
     main="Logit, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,sep.firth, type="b", col="green" )
legend("topright", legend=c("ML", "Firth"),
       col=c("black", "green"), lty=1, cex=0.7)


## Coverage


par(mfrow=c(2,3))
op <- par(cex = 1) 

cov.180<-c(0.974,0.971,0.975,0.978,0.977,0.988,0.98,0.981,0.982)
cov.180<-cov.180*100

plot(x, cov.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(85,100),
     main="Logit, k=6, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


cov.180.firth<-c(0.956,0.965,0.968,0.976,0.973,0.975,0.957,0.948,0.959)

cov.180.firth<-cov.180.firth*100

lines(x,cov.180.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)


##

cov.300<-c(0.966,0.964,0.967,0.96,0.972,0.981,0.974,0.967,0.977)
cov.300<-cov.300*100

plot(x, cov.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(85,100),
     main="Logit, k=6, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)

cov.300.firth<-c(0.957,0.956,0.962,0.959,0.969,0.966,0.954,0.954,0.956)

cov.300.firth<-cov.300.firth*100


lines(x,cov.300.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)



##

cov.600<-c(0.959,0.963,0.965,0.969,0.969,0.955,0.949,0.939,0.947)

cov.600<-cov.600*100

plot(x, cov.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(85,100),
     main="Logit, k=6, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)

cov.600.firth<-c(0.955,0.955,0.964,0.966,0.953,0.953,0.947,0.934,0.946)
cov.600.firth<-cov.600.firth*100

lines(x,cov.600.firth, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)



## 3 cov

cov.3.180<-c(0.953,0.968,0.976,0.978,0.979,0.984,0.984,0.991,0.99)

cov.3.180<-cov.3.180*100


cov.3.firth.180<-c(0.94,0.946,0.964,0.972,0.976,0.976,0.961,0.968,0.964)

cov.3.firth.180<-cov.3.firth.180*100

plot(x, cov.3.180,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(85,100),
     main="Logit, k=3, n=180")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,cov.3.firth.180, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)




##

cov.3.300<-c(0.97,0.969,0.966,0.957,0.966,0.977,0.984,0.973,0.975)

cov.3.300<-cov.3.300*100

cov.3.firth.300<-c(0.95,0.963,0.965,0.954,0.964,0.965,0.967,0.955,0.958)

cov.3.firth.300<-cov.3.firth.300*100


plot(x, cov.3.300,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(85,100),
     main="Logit, k=3, n=300")
axis(side = 1, at = round(x,2),labels = T, las=2)



lines(x,cov.3.firth.300, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)

##

cov.3.600<-c(0.957,0.956,0.958,0.961,0.967,0.965,0.952,0.957,0.95)

cov.3.600<-cov.3.600*100



cov.3.firth.600<-c(0.954,0.953,0.956,0.959,0.957,0.952,0.947,0.951,0.946)

cov.3.firth.600<-cov.3.firth.600*100


plot(x, cov.3.600,xaxt = 'n',
     type = "b", pch = 18,
     ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta",ylim = c(85,100),
     main="Logit, k=3, n=600")
axis(side = 1, at = round(x,2),labels = T, las=2)


lines(x,cov.3.firth.600, type="b", col="green",lty=2)
abline(h = 95, lty=3)
legend("bottomleft", legend=c("ML-Wald", "Firth-Profile","95% Coverage"),
       col=c("black", "green"), lty=1:3, cex=0.6)





