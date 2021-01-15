data=read.csv("cobarto.csv",header=T)
x=as.matrix(data[,2])
y=as.matrix(data[,1])
scatter.smooth(x,y)
library(ggplot2)

origin.reg=function(x,y,alpha=0.05){
  n=nrow(x)
  p=1 #karena beta ada 1
  xbar=mean(x)
  ybar=mean(y)
  
  #Taksiran beta
  b1=sum(x*y)/sum(x^2)
  
  ytop=b1*x
  e=y-ytop
  MSE=sum(e^2)/(n-p)
  SE.Model=sqrt(MSE)
  SE1=sqrt(MSE/sum(x^2))
  
  #Uji t (hipotesis)
  bk0=0
  tk=(b1-bk0)/SE1
  pval.t=2*(1-pt(abs(tk),n-p)) #2 pihak
  Kes.t=ifelse(pval.t<=alpha,"H0 ditolak","H0 diterima")
  
  taksiran=data.frame(b1,SE1,tk,pval.t,Kes.t)
  
  #ANAVA
  SSR=sum(ytop^2)
  SSE=sum(e^2)
  SSTO=sum(y^2)
  dfr=p
  dfe=n-p
  dfto=n
  MSR=SSR/dfr
  MSE=SSE/dfe
  F=MSR/MSE
  pval.F=1-pf(F,dfr,dfe)
  Kes.F=ifelse(pval.F<=alpha,"H0 ditolak","H0 diterima")
  anava=data.frame(F,pval.F,Kes.F)
  
  tabel=matrix(c(SSR,SSE,SSTO,dfr,dfe,dfto,MSR,MSE,0),3,3)
  dimnames(tabel)=list(c("Regresi","Error","Total"),c("SS","df","MS"))
  
  #Koef Determinasi
  R2=sum(ytop^2)/sum(y^2)
  
  dat=data.frame(y,ytop,e)
  
  #Confidence Interval
  CIb1=b1+c(-1,1)*abs(qt(alpha/2,n-p))*SE1
  CIMSE=c((MSE*(n-p)/qchisq(1-alpha/2,n-p)),(MSE*(n-p)/qchisq(alpha/2,n-p)))
  
  #Confidence Limit
  sc=sqrt((x^2*MSE)/sum(x^2))
  CLlow=ytop-1*abs(qt(alpha/2,n-p))*sc
  CLup=ytop+1*abs(qt(alpha/2,n-p))*sc
  
  #Prediction Limit
  sp=sqrt(MSE*(1+(x^2/sum(x^2))))
  PLlow=ytop+1*abs(qt(alpha/2,n-p))*sp
  PLup=ytop-1*abs(qt(alpha/2,n-p))*sp
  
  CB=data.frame(ytop,CLlow,CLup,PLlow,PLup)
  
  plot=ggplot(CB,aes(x)) +
    geom_line(aes(y=ytop),colour="red") +
    geom_line(aes(y=CLlow),colour="purple") +
    geom_line(aes(y=CLup),colour="purple") +
    geom_line(aes(y=PLlow),colour="green") +
    geom_line(aes(y=PLup),colour="green") +
    ylab("Y") + xlab("X")
  
  hasil=list("Taksiran Beta, SE, dan Uji t"=taksiran,"MSE"=MSE, "CI Beta 1"=CIb1, 
             "CI MSE"=CIMSE,"Tabel ANAVA"=tabel, "Uji ANAVA"=anava,
             "Standard Error Model"=SE.Model, "Koef Determinasi"=R2, 
             "Y, Y topi, Residual"=dat, "CB"=CB, 
             "Plot Confidence dan Prediction Limit"=plot)
  print(hasil)
}

origin.reg(x,y,alpha=0.05)

summary(lm(y~x))
