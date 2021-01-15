prediksi.mean=function(x,y,alpha=0.05,xh=0){
  n=nrow(x)
  p=ncol(x)+1
  xbar=mean(x)
  model=lm(y~x)
  b=coefficients(model)
  xx=c(1,xh)
  yh=sum(b*xx)
  e=residuals(model)
  MSE=sum(e^2)/(n-p)
  s=sqrt(MSE*((1/n)+((xh-xbar)^2/sum((x-xbar)^2))))
  pred=yh+c(-1,1)*abs(qt(alpha/2,n-p))*s
  hasil=list("Yh topi"=yh, "Interval Estimation of Mean Response [E(yh)]"=pred)
  print(hasil)
}

prediksi.mean(x,y,alpha=0.05,xh=160)
