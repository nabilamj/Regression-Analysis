data=read.csv("coba.csv",header=T)
data
x=as.matrix(data[,1])
y=as.matrix(data[,7])
scatter.smooth(x,y)
library(ggplot2)

simple.reg=function(x,y,alpha=0.05){
  
  n=nrow(x)
  p=ncol(x)+1
  
  model=lm(y~x)
  ytop=fitted.values(model)
  e=residuals(model)
  MSE=sum(e^2)/(n-p)
  
  #Tabel ANAVA
  anava=anova(model)
  SSR=sum((ytop-mean(y))^2)
  SSE=sum(e^2)
  SSTO=SSR+SSE
  dfr=p-1
  dfe=n-p
  dfto=n-1
  MSR=SSR/dfr
  MSE=SSE/dfe
  tabel=matrix(c(SSR,SSE,SSTO,dfr,dfe,dfto,MSR,MSE,0),3,3)
  dimnames(tabel)=list(c("Regresi","Error","Total"),c("SS","df","MS"))
  
  dat=data.frame(y,ytop,e)
  
  #Confidence Interval Beta dan MSE
  CIb=confint(model,level=1-alpha)
  CIMSE=c((MSE*(n-p)/qchisq(1-alpha/2,n-p)),(MSE*(n-p)/qchisq(alpha/2,n-p)))
  
  #Confidence Limit
  CL=predict.lm(model,interval="confidence",level=1-alpha,type="response")
  CLlow=CL[,2]
  CLup=CL[,3]
  
  #Prediction Limit
  PL=predict.lm(model,interval="prediction",level=1-alpha,type="response")
  PLlow=PL[,2]
  PLup=PL[,3]
  
  CB=data.frame(ytop,CLlow,CLup,PLlow,PLup)
  
  plot=ggplot(CB,aes(x)) +
    geom_line(aes(y=ytop),colour="red") +
    geom_line(aes(y=CLlow),colour="purple") +
    geom_line(aes(y=CLup),colour="purple") +
    geom_line(aes(y=PLlow),colour="green") +
    geom_line(aes(y=PLup),colour="green") +
    ylab("Y") + xlab("X")
  
  hasil=list("Taksiran Beta"=summary(model), "MSE"=MSE, "CI Beta"=CIb, 
             "CI MSE"=CIMSE, "Uji ANAVA"=anava, "Tabel ANAVA"=tabel,
             "Y, Y topi, Residual"=dat, "Confidence dan Prediction Limit"=CB,
             "Plot Confidence dan Prediction Limit"=plot)
  print(hasil)
}

simple.reg(x,y,alpha=0.05)
