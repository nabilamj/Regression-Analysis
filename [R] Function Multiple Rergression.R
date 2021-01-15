data=read.csv("coba.csv",header=TRUE)

x0=as.matrix(data[,-7])
u=matrix(c(1),nrow(data),1)
x=cbind(u,x0)
y=data[,7]

multiple.reg=function(x,y,alpha=0.05){
  
  n=nrow(x)
  p=ncol(x)
  
  #Taksiran beta topi (b)
  b=(solve(t(x)%*%x))%*%t(x)%*%y
  
  #Taksiran H
  H=x%*%(solve(t(x)%*%x))%*%t(x)
  
  #Mencari SE
  I=diag(c(1),nrow=n,ncol=n)
  e=(I-H)%*%y
  MSE=as.numeric(t(e)%*%e/(n-p))
  SE=sqrt((MSE)*diag(solve(t(x)%*%x)))
  
  taksiran=data.frame(b,SE)
  
  #Mencari y topi
  ytop=matrix(0,n,1)
  for (i in (1:n)){
    for (j in (1:p)){
    ytop[i]=b[1]+b[i]*x[j] 
    }
  }
  
  #ANAVA
  J=matrix(c(1),nrow=n,ncol=n)
  SSTO=t(y)%*%y-((1/n)%*%(t(y)%*%J%*%y))
  SSE=t(e)%*%e
  SSR=SSTO-SSE
  dfto=n-1
  dfe=n-p
  dfr=p-1
  MSR=SSR/dfr
  MSE=SSE/dfe
  F=MSR/MSE
  pval.F=1-pf(F,dfr,dfe)
  Kes.F=ifelse(pval.F<=alpha,"H0 ditolak","H0 diterima")
  
  tabel=matrix(c(SSR,SSE,SSTO,dfr,dfe,dfto,MSR,MSE,0),3,3)
  dimnames(tabel)=list(c("Regresi","Error","Total"),c("SS","df","MS"))
  
  uji.F=data.frame(F,pval.F,Kes.F)
  
  #Koef Determinasi
  R2=SSR/SSTO
  
  #Uji Beta k (parsial) (Uji t)
  bk0=0
  tk=(b-bk0)/SE
  pval.t=2*(1-pt(abs(tk),n-p)) #2 pihak
  Kes.t=ifelse(pval.t<=alpha,"H0 ditolak","H0 diterima")
  uji.t=data.frame(tk,pval.t,Kes.t)
  
  #Confidence Interval
  kanan=b+abs(qt(alpha/2,n-p))*SE
  kiri=b-abs(qt(alpha/2,n-p))*SE
  CIb=data.frame(kiri,kanan)
  CIMSE=c((MSE*(n-p)/qchisq(1-alpha/2,n-p)),(MSE*(n-p)/qchisq(alpha/2,n-p)))
  
  
  hasil=list("Taksiran Beta dan SE"=taksiran, "Y topi"=ytop, "Residual"=e,
             "Tabel ANAVA"=tabel, "Uji ANAVA"=uji.F, "Koef Determinasi"=R2, 
             "Uji t Parsial"=uji.t, "CI Beta"=CIb, "CIMSE"=CIMSE)
  print(hasil)
}

multiple.reg(x,y,alpha=0.05)