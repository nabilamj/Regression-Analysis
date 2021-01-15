data=read.csv("coba.csv",header=T)
x0=as.matrix(data[,-7])
u=matrix(c(1),nrow(data),1)
x=cbind(u,x0)
y=data[,7]

x1=cbind(u,x0[,1])
x2=cbind(u,x0[,2])
x3=cbind(u,x0[,3])
x4=cbind(u,x0[,4])
x5=cbind(u,x0[,5])
x6=cbind(u,x0[,6])
alpha=0.05

backward=function(x,y,alpha=0.05){
  n=nrow(x)
  p=ncol(x)
  b=(solve(t(x)%*%x))%*%t(x)%*%y
  H=x%*%(solve(t(x)%*%x))%*%t(x)
  I=diag(c(1),nrow=n,ncol=n)
  e=(I-H)%*%y
  MSE=as.numeric(t(e)%*%e/(n-p))
  SE=sqrt((MSE)*diag(solve(t(x)%*%x)))
  tk=b/SE
  pval.t=2*(1-pt(abs(tk),n-p))
  pval=pval.t[-1,]
  pval.maks=pval[which(pval==max(pval))]
  kes=ifelse(pval.maks>alpha,"Keluarkan","STOP")
  print(list(pval.maks,kes))
}

backward(x,y)
step1=x[,-4]
backward(step1,y)
step2=x[,-c(4,7)]
backward(step2,y)
step3=x[,-c(4,6,7)]
backward(step3,y)
step4=x[,-c(4,5,6,7)]
backward(step4,y)
##Model akhir: Y=b0+b1X1+b2X2+e


forward=function(x,y){
  n=nrow(x)
  p=ncol(x)
  b=(solve(t(x)%*%x))%*%t(x)%*%y
  H=x%*%(solve(t(x)%*%x))%*%t(x)
  I=diag(c(1),nrow=n,ncol=n)
  e=(I-H)%*%y
  MSE=as.numeric(t(e)%*%e/(n-p))
  SE=sqrt((MSE)*diag(solve(t(x)%*%x)))
  tk=b/SE
  pval.t=2*(1-pt(abs(tk),n-p))
  m=nrow(pval.t)
  pval=pval.t[m,]
  print(pval)
}

fit1=forward(x1,y)
fit2=forward(x2,y)
fit3=forward(x3,y)
fit4=forward(x4,y)
fit5=forward(x5,y)
fit6=forward(x6,y)
pval=data.frame(fit1,fit2,fit3,fit4,fit5,fit6)
pval.min=pval[which(pval==min(pval))]
ifelse(pval.min<=alpha,"Masukkan","STOP")
##Model: Y=b0+b1X2+e

x2=x2[,-1]
fit1=forward(cbind(x2,x1),y)
fit3=forward(cbind(x2,x3),y)
fit4=forward(cbind(x2,x4),y)
fit5=forward(cbind(x2,x5),y)
fit6=forward(cbind(x2,x6),y)
pval=data.frame(fit1,fit3,fit4,fit5,fit6)
pval.min=pval[which(pval==min(pval))]
ifelse(pval.min<=alpha,"Masukkan","STOP")
##Model: Y=b0+b1X2+b2X1+e

x1=x1[,-1]
fit3=forward(cbind(x2,x1,x3),y)
fit4=forward(cbind(x2,x1,x4),y)
fit5=forward(cbind(x2,x1,x5),y)
fit6=forward(cbind(x2,x1,x6),y)
pval=data.frame(fit3,fit4,fit5,fit6)
pval.min=pval[which(pval==min(pval))]
ifelse(pval.min<=alpha,"Masukkan","STOP")
##Model akhir: Y=b0+b1X2+b2X1+e


x1=data[,1]
x2=data[,2]
model=lm(y~x1+x2,data=data)
summary(model)
