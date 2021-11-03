## The I was change into D, G into V in the plot parts after data analysis to suit the paper word. 

setwd("e:/my documents/R/data/Nitraria Paper14")
data=read.csv(file.choose(),header=T)   # choose the file"DATA-meristem.style2.Final.7NP.csv"
data

#dataNP<-data[c(1:297),c(39:50)]

dataN0<-data[c(1:27),c(39:44)]
dataN10<-data[c(28:54),c(39:44)]
dataN20<-data[c(55:81),c(39:44)]
dataC<-data[c(82:108),c(39:44)]
dataP0<-data[c(109:135),c(39:44)]
dataP6<-data[c(136:162),c(39:44)]
dataP12<-data[c(163:189),c(39:44)]

#dataN<-data[c(1:135),c(1:53)]
#dataN
#dataC<-data[c(136:162),c(1:53)]
#dataC
#dataP<-data[c(163:297),c(1:53)]
#dataP

#xn_gr<-dataN[,48]
#xn_gr
#yn_i<-dataN[,45]
#yn_i

xn1_gr<-dataN0[c(1:27),4]
yn1_i<-dataN0[c(1:27),1]
xn3_gr<-dataN10[c(1:27),4]
yn3_i<-dataN10[c(1:27),1]
xn5_gr<-dataN20[c(1:27),4]
yn5_i<-dataN20[c(1:27),1]
xc_gr<-dataC[c(1:27),4]
yc_i<-dataC[c(1:27),1]
xp1_gr<-dataP0[c(1:27),4]
yp1_i<-dataP0[c(1:27),1]
xp3_gr<-dataP6[c(1:27),4]
yp3_i<-dataP6[c(1:27),1]
xp5_gr<-dataP12[c(1:27),4]
yp5_i<-dataP12[c(1:27),1]

lmN11<-lm(yn1_i~xn1_gr,data=dataN0)
summary(lmN11)
lmN31<-lm(yn3_i~xn3_gr,data=dataN10)
summary(lmN31)
lmN51<-lm(yn5_i~xn5_gr,data=dataN20)
summary(lmN51)
lmc1<-lm(yc_i~xc_gr,data=dataC)
summary(lmc1)
lmP11<-lm(yp1_i~xp1_gr,data=dataP0)
summary(lmP11)
lmP31<-lm(yp3_i~xp3_gr,data=dataP6)
summary(lmP31)
lmP51<-lm(yp5_i~xp5_gr,data=dataP12)
summary(lmP51)

library(lmodel2)
rma1N11<-lmodel2(yn1_i~xn1_gr,data=dataN0,"relative","relative",99)
rma1N11
rma1N31<-lmodel2(yn3_i~xn3_gr,data=dataN10,"relative","relative",99)
rma1N31
rma1N51<-lmodel2(yn5_i~xn5_gr,data=dataN20,"relative","relative",99)
rma1N51
rma1C1<-lmodel2(yc_i~xc_gr,data=dataC,"relative","relative",99)
rma1C1
rma1P11<-lmodel2(yp1_i~xp1_gr,data=dataP0,"relative","relative",99)
rma1P11
rma1P31<-lmodel2(yp3_i~xp3_gr,data=dataP6,"relative","relative",99)
rma1P31
rma1P51<-lmodel2(yp5_i~xp5_gr,data=dataP12,"relative","relative",99)
rma1P51
##############################################
xn1_ir<-dataN0[c(1:27),5]
yn1_g<-dataN0[c(1:27),2]
xn3_ir<-dataN10[c(1:27),5]
yn3_g<-dataN10[c(1:27),2]
xn5_ir<-dataN20[c(1:27),5]
yn5_g<-dataN20[c(1:27),2]
xc_ir<-dataC[c(1:27),5]
yc_g<-dataC[c(1:27),2]
xp1_ir<-dataP0[c(1:27),5]
yp1_g<-dataP0[c(1:27),2]
xp3_ir<-dataP6[c(1:27),5]
yp3_g<-dataP6[c(1:27),2]
xp5_ir<-dataP12[c(1:27),5]
yp5_g<-dataP12[c(1:27),2]

lmN12<-lm(yn1_g~xn1_ir,data=dataN0)
summary(lmN12)
lmN32<-lm(yn3_g~xn3_ir,data=dataN10)
summary(lmN32)
lmN52<-lm(yn5_g~xn5_ir,data=dataN20)
summary(lmN52)
lmc2<-lm(yc_g~xc_ir,data=dataC)
summary(lmc2)
lmP12<-lm(yp1_g~xp1_ir,data=dataP0)
summary(lmP12)
lmP32<-lm(yp3_g~xp3_ir,data=dataP6)
summary(lmP32)
lmP52<-lm(yp5_g~xp5_ir,data=dataP12)
summary(lmP52)

library(lmodel2)
rma1N12<-lmodel2(yn1_g~xn1_ir,data=dataN0,"relative","relative",99)
rma1N12
rma1N32<-lmodel2(yn3_g~xn3_ir,data=dataN10,"relative","relative",99)
rma1N32
rma1N52<-lmodel2(yn5_g~xn5_ir,data=dataN20,"relative","relative",99)
rma1N52
rma1C2<-lmodel2(yc_g~xc_ir,data=dataC,"relative","relative",99)
rma1C2
rma1P12<-lmodel2(yp1_g~xp1_ir,data=dataP0,"relative","relative",99)
rma1P12
rma1P32<-lmodel2(yp3_g~xp3_ir,data=dataP6,"relative","relative",99)
rma1P32
rma1P52<-lmodel2(yp5_g~xp5_ir,data=dataP12,"relative","relative",99)
rma1P52
###################################################
xn1_ig<-dataN0[c(1:27),6]
yn1_r<-dataN0[c(1:27),3]
xn3_ig<-dataN10[c(1:27),6]
yn3_r<-dataN10[c(1:27),3]
xn5_ig<-dataN20[c(1:27),6]
yn5_r<-dataN20[c(1:27),3]
xc_ig<-dataC[c(1:27),6]
yc_r<-dataC[c(1:27),3]
xp1_ig<-dataP0[c(1:27),6]
yp1_r<-dataP0[c(1:27),3]
xp3_ig<-dataP6[c(1:27),6]
yp3_r<-dataP6[c(1:27),3]
xp5_ig<-dataP12[c(1:27),6]
yp5_r<-dataP12[c(1:27),3]

lmN13<-lm(yn1_r~xn1_ig,data=dataN0)
summary(lmN13)
lmN33<-lm(yn3_r~xn3_ig,data=dataN10)
summary(lmN33)
lmN53<-lm(yn5_r~xn5_ig,data=dataN20)
summary(lmN53)
lmc3<-lm(yc_r~xc_ig,data=dataC)
summary(lmc3)
lmP13<-lm(yp1_r~xp1_ig,data=dataP0)
summary(lmP13)
lmP33<-lm(yp3_r~xp3_ig,data=dataP6)
summary(lmP33)
lmP53<-lm(yp5_r~xp5_ig,data=dataP12)
summary(lmP53)

library(lmodel2)
rma1N13<-lmodel2(yn1_r~xn1_ig,data=dataN0,"relative","relative",99)
rma1N13
rma1N33<-lmodel2(yn3_r~xn3_ig,data=dataN10,"relative","relative",99)
rma1N33
rma1N53<-lmodel2(yn5_r~xn5_ig,data=dataN20,"relative","relative",99)
rma1N53
rma1C3<-lmodel2(yc_r~xc_ig,data=dataC,"relative","relative",99)
rma1C3
rma1P13<-lmodel2(yp1_r~xp1_ig,data=dataP0,"relative","relative",99)
rma1P13
rma1P33<-lmodel2(yp3_r~xp3_ig,data=dataP6,"relative","relative",99)
rma1P33
rma1P53<-lmodel2(yp5_r~xp5_ig,data=dataP12,"relative","relative",99)
rma1P53
#####################################################
par(mfcol=c(7,3))
par(pin=c(1,1))
opar <- par(no.readonly=TRUE)
#par(mar=c(1,4,1,2)+0.1)
#par(mfcol=c(3,11))


#par(mar=c(0.4,3,0,0.2)+0.1)##组合图形中各个图的边界大???
#layout(matrix(c(1:21),7,1,byrow=TRUE))##还需要这个，以调???7副图高度以适应最小面一副图与其他图大小相当???

#picturelayout<-(matrix(c(1:21),7,3,byrow=FALSE),widths=c(4,2),heights=c(7,2),respect=TRUE)
#picturelayout
#drawlayout<-layout(picturelayout,widths=c(4,2),heights=c(7,2),respect=TRUE)
#layout.show(drawlayout)

#layout(matrix(c(1:21),7,3,byrow=FALSE),respect=FALSE)


#par(cex=0.5)
par(fig=c(0.09,0.34,0.8645,0.9995))
plot(xn1_gr,yn1_i,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE)
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N11$regression.results[3,2], rma1N11$regression.results[3,3],lty=2)
x11<-c(0,1,2,3)
y11<-c(0,1,2,3)
lm101<-lm(y11~x11)
abline(lm101,lwd=1.5,lty=1)
text(0.1,2.8,"A-1",cex=0.9,pos=4)

par(fig=c(0.09,0.34,0.7295,0.8645),new=TRUE)
plot(xn3_gr,yn3_i,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N31$regression.results[3,2], rma1N31$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"A-2",cex=0.9,pos=4)

par(fig=c(0.09,0.34,0.5945,0.7295),new=TRUE)
plot(xn5_gr,yn5_i,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N51$regression.results[3,2], rma1N51$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"A-3",cex=0.9,pos=4)

#########################################################################
par(fig=c(0.09,0.34,0.4595,0.5945),new=TRUE)
plot(xc_gr,yc_i,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1C1$regression.results[3,2], rma1C1$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"A-4",cex=0.9,pos=4)

#axis(2,labels=TRUE,pos=-0.3,tck=-0.02,outer=TRUE,mgp=c(1,0.5,0))
#title(ylab="logI",mgp=c(1.9,0.5,0))
#mtext("logI",side =2, las=0,cex=0.8)

par(fig=c(0.09,0.095,0.4595,0.5945),new=TRUE)
axis(2,labels=TRUE,pos=-0.15,tck=-0.02,outer=TRUE,mgp=c(3,0.3,0))
par(fig=c(0.152,0.155,0.4595,0.5945),new=TRUE)
mtext("logD",side=2, las=0, cex=0.8)
#########################################################################

par(fig=c(0.09,0.34,0.3245,0.4595),new=TRUE)
plot(xp1_gr,yp1_i,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P11$regression.results[3,2], rma1P11$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"A-5",cex=0.9,pos=4)

par(fig=c(0.09,0.34,0.1895,0.3245),new=TRUE)
plot(xp3_gr,yp3_i,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P31$regression.results[3,2], rma1P31$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"A-6",cex=0.9,pos=4)
#########################################################################
#par(mar=c(6,3,0,0.2)+0.1,NEW=TRUE)
par(fig=c(0.09,0.34,0.0545,0.1895),new=TRUE)
plot(xp5_gr,yp5_i,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P51$regression.results[3,2], rma1P51$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"A-7",cex=0.9,pos=4)

axis(1,labels=TRUE,pos=-0.3,tck=-0.02,outer=TRUE,mgp=c(1,0.5,0))
par(fig=c(0.09,0.34,0.025,0.15))
#title(xlab="log(G+R)",mgp=c(1.9,0.5,0))
mtext("log(V+R)",side =1,las=0,cex=0.8)
##########################################################################

##Start SECOND COLUMN
par(fig=c(0.42,0.67,0.8645,0.9995),new=TRUE)
plot(xn1_ir,yn1_g,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N12$regression.results[3,2], rma1N12$regression.results[3,3],lty=2)
x11<-c(0,1,2)
y11<-c(0,1,2)
lm101<-lm(y11~x11)
abline(lm101,lwd=1.5)
text(0.1,2.8,"B-1",cex=0.9,pos=4)

par(fig=c(0.42,0.67,0.7295,0.8645),new=TRUE)
plot(xn3_ir,yn3_g,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N32$regression.results[3,2], rma1N32$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"B-2",cex=0.9,pos=4)

par(fig=c(0.42,0.67,0.5945,0.7295),new=TRUE)
plot(xn5_ir,yn5_g,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N52$regression.results[3,2], rma1N52$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"B-3",cex=0.9,pos=4)

#########################################################################
par(fig=c(0.42,0.67,0.4595,0.5945),new=TRUE)
plot(xc_ir,yc_g,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1C2$regression.results[3,2], rma1C2$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"B-4",cex=0.9,pos=4)
#axis(2,labels=TRUE,pos=-0.3,tck=-0.02,outer=TRUE,mgp=c(1,0.5,0))
#title(ylab="logG",mgp=c(1.9,0.5,0))
#mtext("logG",side =2, las=0,cex=0.8)

par(fig=c(0.418,0.421,0.4595,0.5945),new=TRUE)
axis(2,labels=TRUE,pos=-0.15,tck=-0.02,outer=TRUE,mgp=c(3,0.3,0),cex=0.8)
par(fig=c(0.483,0.488,0.4595,0.5945),new=TRUE)
mtext("logV",side =2, las=0,cex=0.8)
#########################################################################

par(fig=c(0.42,0.67,0.3245,0.4595),new=TRUE)
plot(xp1_ir,yp1_g,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P12$regression.results[3,2], rma1P12$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"B-5",cex=0.9,pos=4)

par(fig=c(0.42,0.67,0.1895,0.3245),new=TRUE)
plot(xp3_ir,yp3_g,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P32$regression.results[3,2], rma1P32$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"B-6",cex=0.9,pos=4)

#########################################################################
#par(mar=c(6,3,0,0.2)+0.1,NEW=TRUE)
par(fig=c(0.42,0.67,0.0545,0.1895),new=TRUE)
plot(xp5_ir,yp5_g,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(6,1.5,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P52$regression.results[3,2], rma1P52$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"B-7",cex=0.9,pos=4)

axis(1,labels=TRUE,pos=-0.3,tck=-0.02,outer=TRUE,mgp=c(1,0.5,0))
#title(xlab="log(I+R)",mgp=c(1.9,0.5,0))
par(fig=c(0.42,0.67,0.025,0.15))
mtext("log(D+R)",side =1,las=0,cex=0.8)
##########################################################################

#Start Third COLUMN
par(fig=c(0.75,1,0.8645,0.9995),new=TRUE)
plot(xn1_ig,yn1_r,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N13$regression.results[3,2], rma1N13$regression.results[3,3],lty=2)
x11<-c(0,1,2)
y11<-c(0,1,2)
lm101<-lm(y11~x11)
abline(lm101,lwd=1.5)
text(0.1,2.8,"C-1",cex=0.9,pos=4)

par(fig=c(0.75,1,0.7295,0.8645),new=TRUE)
plot(xn3_ig,yn3_r,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N33$regression.results[3,2], rma1N33$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"C-2",cex=0.9,pos=4)

par(fig=c(0.75,1,0.5945,0.7295),new=TRUE)
plot(xn5_ig,yn5_r,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1N53$regression.results[3,2], rma1N53$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"C-3",cex=0.9,pos=4)

#########################################################################
par(fig=c(0.75,1,0.4595,0.5945),new=TRUE)
plot(xc_ig,yc_r,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1C3$regression.results[3,2], rma1C3$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"C-4",cex=0.9,pos=4)

par(fig=c(0.748,0.751,0.4595,0.5945),new=TRUE)
axis(2,labels=TRUE,pos=-0.15,tck=-0.02,outer=TRUE,mgp=c(3,0.3,0),cex=0.8)
par(fig=c(0.813,0.818,0.4595,0.5945),new=TRUE)
#title(ylab="logR",cex=0.5)
mtext("logR",side =2, las=0,cex=0.8)

#########################################################################

par(fig=c(0.75,1,0.3245,0.4595),new=TRUE)
plot(xp1_ig,yp1_r,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P13$regression.results[3,2], rma1P13$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"C-5",cex=0.9,pos=4)

par(fig=c(0.75,1,0.1895,0.3245),new=TRUE)
plot(xp3_ig,yp3_r,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P33$regression.results[3,2], rma1P33$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"C-6",cex=0.9,pos=4)

#########################################################################
#par(mar=c(6,3,0,0.2)+0.1,NEW=TRUE)

par(fig=c(0.75,1,0.0545,0.1895),new=TRUE)
plot(xp5_ig,yp5_r,pch=5,cex=0.8,xlim=c(0,3.0),ylim=c(0,3.0),axes=FALSE,ann=FALSE,mar=c(0,0,0,0))
axis(1,labels=FALSE,tck=0.02)
axis(2,labels=FALSE,tck=0.02)
abline(rma1P53$regression.results[3,2], rma1P53$regression.results[3,3],lty=2)
abline(lm101,lwd=1.5)
text(0.1,2.8,"C-7",cex=0.9,pos=4)

axis(1,labels=TRUE,pos=-0.3,tck=-0.02,outer=TRUE,mgp=c(1,0.5,0))
#title(xlab="log(I+G)",line=6)
#title(xlab="log(I+R)",mgp=c(1.9,0.5,0))
par(fig=c(0.75,1,0.025,0.15))
mtext("log(D+V)",side =1,las=0,cex=0.8)

##########################################################################

par(opar)


