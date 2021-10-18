######################################
#
#  simlating the null hypothesis of no between worker differences
#
######################################

data <- read.csv('Raw data/Kromout93Table/kromhout.gsd.database.csv',stringsAsFactors=F)



######### format du résultat : une matrice pour chaque paramètre n.obs *  n.sim 

n.sim <- 100

data <- read.csv('Raw data/Kromout93Table/kromhout.gsd.database.csv',stringsAsFactors=F)


data <-data[ 99 , ]

test <- mapply( fun.one.obs , n.sim = rep( n.sim , length(data[,1]) ),
                              n = data$n ,
                              k = data$k ,
                              s = ( data$sw^2 + data$sb^2 ) ,
                              SIMPLIFY = FALSE)

fullmat <- array( dim = c( length(data[,1])  , 3 , n.sim ) )

for ( i in 1:10) fullmat[ i , , ] <- test[[i]]

####### fonction pour 1 combinaison n , k , s

fun.one.obs <- function( n.sim , n , k , s ) {
  
  myres <- sapply( 1:n.sim , function(x) {
    
    mydat <- fun.sim( n , k , s)
    
    res <- var.comp.aov( mydat )
    
    sw <- res$sigW
    
    sb <- res$sigB
    
    rho <- res$rho
    
    return( c( sw , sb , rho ) )
    
  })
  
  return( myres )
  
  
  }
  
  

mydat <- fun.sim( 77 , 12 , 0.5)

###################  simulation of a worker group exposure dataset

fun.sim <- function( n , k , s) { 
  
  mydat <- data.frame( x = exp( rnorm ( n , log(100) , s  ) ) )
  
  # creating the worker column : balanced n/k rounded + random assignemt
  
  rep.balanced <- floor( n / k )
  
  workerbalanced <- rep( paste( "W" , 1:k , sep="") , rep( rep.balanced , k) )
  
  workertotal <- c( workerbalanced , sample( paste( "W" , 1:k , sep="") , size = n-rep.balanced*k , replace = TRUE ))

  mydat$worker <- workertotal
  
return( mydat)

}
