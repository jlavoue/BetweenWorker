######################################
#
#  simlating the null hypothesis of no between worker differences
#
######################################

data <- read.csv('Raw data/Kromout93Table/kromhout.gsd.database.csv',stringsAsFactors=F)

fun.sim <- function( n , k , s) { 
  
  mydat <- data.frame( x = exp( rnorm ( n*k , log(100) , s  ) ) )
  
  # creating the worker column : balanced n/k rounded + random assignemt
  
  rep.balanced <- floor( n / k )
  
  workerbalanced <- rep( paste( "W" , 1:k , sep="") , rep( rep.balanced , k) )
  
  workertotal <- c( workerbalanced , sample( paste( "W" , 1:k , sep="") , size = n-rep.balanced*k , replace = TRUE ))


mydat$worker <- workertotal

return( mydat)

}
