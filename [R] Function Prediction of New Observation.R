prediksi.baru=function(x,y,alpha=0.05,xh=0){
  n=nrow(x)
  p=ncol(x)+1
  xbar=mean(x)
  model=lm(y~x)
  b=coefficients(model)
  xx=c(1,xh)
  yh=sum(b*xx)
  e=residuals(model)
  MSE=sum(e^2)/(n-p)
  s=sqrt(MSE*(1+((1/n)+((xh-xbar)^2/sum((x-xbar)^2)))))
  pred=yh+c(-1,1)*abs(qt(alpha/2,n-p))*s
  hasil=list("Yh baru"=yh, "Prediction of New Observation"=pred)
  print(hasil)
}

prediksi.baru(x,y,alpha=0.05,xh=190)
