data=read.csv("coba.csv",header=T)
x=as.matrix(data[,1])
y=as.matrix(data[,7])
scatter.smooth(x,y)
library(ggplot2)
  
simple.reg=function(x,y,alpha=0.05){
  n=nrow(x)
  p=2 #karena beta ada 2
  xbar=mean(x)
  ybar=mean(y)
  
  #Taksiran beta
  b1=(n*sum(x*y)-sum(x)*sum(y))/(n*sum(x^2)-sum(x)^2)
  b0=ybar-b1*xbar
  b=matrix(c(b0,b1),2,1)
  
  ytop=b0+b1*x
  e=y-ytop
  MSE=sum(e^2)/(n-p)
  SE.Model=sqrt(MSE)
  SE0=sqrt(MSE*((1/n)+(xbar^2/sum((x-xbar)^2))))
  SE1=sqrt(MSE/sum((x-xbar)^2))
  SE=matrix(c(SE0,SE1),2,1)
  
  #Uji t (hipotesis)
  bk0=0
  tk=(b-bk0)/SE
  pval.t=2*(1-pt(abs(tk),n-p)) #2 pihak
  Kes.t=ifelse(pval.t<=alpha,"H0 ditolak","H0 diterima")
  
  taksiran=data.frame(b,SE,tk,pval.t,Kes.t)
  
  #ANAVA
  SSR=sum((ytop-ybar)^2)
  SSE=sum(e^2)
  SSTO=SSR+SSE
  dfr=p-1
  dfe=n-p
  dfto=n-1
  MSR=SSR/dfr
  MSE=SSE/dfe
  F=MSR/MSE
  pval.F=1-pf(F,dfr,dfe)
  Kes.F=ifelse(pval.F<=alpha,"H0 ditolak","H0 diterima")
  anava=data.frame(F,pval.F,Kes.F)
  
  tabel=matrix(c(SSR,SSE,SSTO,dfr,dfe,dfto,MSR,MSE,0),3,3)
  dimnames(tabel)=list(c("Regresi","Error","Total"),c("SS","df","MS"))
  
  dat=data.frame(y,ytop,e)
  
  #Koef Determinasi
  R2=SSR/SSTO
  
  #Confidence Interval
  CIb1=b1+c(-1,1)*abs(qt(alpha/2,n-p))*SE1
  CIb0=b0+c(-1,1)*abs(qt(alpha/2,n-p))*SE0
  CIMSE=c((MSE*(n-p)/qchisq(1-alpha/2,n-p)),(MSE*(n-p)/qchisq(alpha/2,n-p)))
  
  #Confidence Limit
  sc=sqrt(MSE*((1/n)+((x-xbar)^2/sum((x-xbar)^2))))
  CLlow=ytop-1*abs(qt(alpha/2,n-p))*sc
  CLup=ytop+1*abs(qt(alpha/2,n-p))*sc
  
  #Prediction Limit
  sp=sqrt(MSE*(1+((1/n)+((x-xbar)^2/sum((x-xbar)^2)))))
  PLlow=ytop-1*abs(qt(alpha/2,n-p))*sp
  PLup=ytop+1*abs(qt(alpha/2,n-p))*sp
  
  CB=data.frame(ytop,CLlow,CLup,PLlow,PLup)
  
  plot=ggplot(CB,aes(x)) +
    geom_line(aes(y=ytop),colour="red") +
    geom_line(aes(y=CLlow),colour="purple") +
    geom_line(aes(y=CLup),colour="purple") +
    geom_line(aes(y=PLlow),colour="green") +
    geom_line(aes(y=PLup),colour="green") +
    ylab("Y") + xlab("X")
  
  hasil=list("Taksiran Beta, SE, dan Uji t"=taksiran,"MSE"=MSE, "CI Beta 0"=CIb0,
             "CI Beta 1"=CIb1, "CI MSE"=CIMSE, "Tabel ANAVA"=tabel, "Uji ANAVA"=anava,
             "Standard Error Model"=SE.Model, "Koef Determinasi"=R2, "Y, Y topi, Residual"=dat,
             "Confidence dan Prediction Limit"=CB, "Plot Confidence dan Prediction Limit"=plot)
  print(hasil)
}
  
simple.reg(x,y,alpha=0.05)
