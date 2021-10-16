#####fonction de détermination de sigB et sigW a partir d'un data frame x=ppm et worker=character

var.comp <-function(dat)
{
  
  #dat : data.frame avec une colonne x de mesures et une colonne worker d'identifiants de travailleurs
  
  #des facteurs pour dat$worker
  
  dat$worker <-as.factor(dat$worker)
  
  #nombre de travailleur
  k <-length(table(dat$worker))
  
  #nombre de mesures par travailleur
  n <-table(dat$worker)[1]
  
  significant <-T
  
  #analyse de variance matricielle
  
  X <-model.matrix(log(x)~worker,data=dat)
  
  Y <-log(dat$x)
  
  I <-diag(rep(1,length(Y)))
  
  H <-X%*%solve(t(X)%*%X)%*%t(X)  
  
  J <-matrix(rep(1,(k*n)^2),nrow=k*n)
  
  ssr <-t(Y)%*%(H-J/(k*n))%*%Y
  
  sse<-t(Y)%*%(I-H)%*%Y
  
  f <-(ssr/(k-1))/(sse/(k*n-k))
  
  if ((ssr/(k-1))/(sse/(k*n-k))<qf(0.95, k-1, k*n-k)) significant <-F
  
  #variance loggee residuelle
  sigW <-sqrt(sse/(k*n-k))
  
  #si l'estimation de la variance 'between' loggee est <0, on la met a 0
  if ((ssr/(k-1)-sse/(k*n-k))>0) sigB <-sqrt((ssr/(k-1)-sse/(k*n-k))/n)
  
  else sigB<-0	
  
  #liste avec variance loggee intra (sigW), et variance loggee inter(sigW)
  
  return(list(sigW=sigW,sigB=sigB,rho=as.numeric(sigB^2/(sigB^2+sigW^2)),significant=significant))
  
}