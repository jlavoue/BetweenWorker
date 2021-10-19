######################################
#
#  simlating the null hypothesis of no between worker differences
#
######################################

# libraries

library(reshape2)


# loading data

    data <- read.csv('Raw data/Kromout93Table/kromhout.gsd.database.csv',stringsAsFactors=F)
    
    source('scripts/functions/anovabetween.R')
    
    
# functions
    
    ####### fonction de simulation pour 1 combinaison n , k , s
    
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
    
    fun.one.obs.lme <- function( n.sim , n , k , s ) {
      
      myres <- sapply( 1:n.sim , function(x) {
        
        mydat <- fun.sim( n , k , s)
        
        res <- var.comp.lme( mydat )
        
        sw <- res$sigW
        
        sb <- res$sigB
        
        rho <- res$rho
        
        return( c( sw , sb , rho ) )
        
      })
      
      return( myres )
      
      
    }
    ###################  simulation of a worker group exposure dataset from total N(n) and number of workers (k)
    
    fun.sim <- function( n , k , s) { 
      
      mydat <- data.frame( x = exp( rnorm ( n , log(100) , s  ) ) )
      
      # creating the worker column : balanced n/k rounded + random assignemt
      
      rep.balanced <- floor( n / k )
      
      workerbalanced <- rep( paste( "W" , 1:k , sep="") , rep( rep.balanced , k) )
      
      workertotal <- c( workerbalanced , sample( paste( "W" , 1:k , sep="") , size = n-rep.balanced*k , replace = TRUE ))
      
      mydat$worker <- workertotal
      
      return( mydat)
      
    }
    
    
# résults
    
######### result format  : an array  n.obs *n.sim 

n.sim <- 500

## for tests on subsets
data1 <- data[  , ]

# test
#myworkerdata <- fun.sim( n = data1$n[1] , k = data1$k[1] , 
#                         s = sqrt( data1$sw[1]^2 + data$sb[1]^2 ))

# test
#mysimtest <-fun.one.obs( n.sim = 5 , n = data1$n[1] , k = data1$k[1] , 
#                         s = sqrt( data1$sw[1]^2 + data$sb[1]^2 ) )


#### applying the analysis to the whole dataset ( approx 7 min. calculation)
myres <- mapply( fun.one.obs , n.sim = rep( n.sim , length(data1[,1]) ),
                              n = data1$n ,
                              k = data1$k ,
                              s = sqrt( data1$sw^2 + data$sb^2 ) ,
                              SIMPLIFY = FALSE)
#### transforming the results into an array
fullmat <- array( dim = c( length(data[,1])  , 3 , n.sim ) )

for ( i in 1:length(data[,1]) ) fullmat[ i , , ] <- myres[[i]]

### saving the results

saveRDS( fullmat , 'results/project_NullHypothesis/sumulation_array.RDS')


####### using the lme estimation method

myres.lme <- mapply( fun.one.obs.lme , n.sim = rep( n.sim , length(data1[,1]) ),
                 n = data1$n ,
                 k = data1$k ,
                 s = sqrt( data1$sw^2 + data$sb^2 ) ,
                 SIMPLIFY = FALSE)

#convergence issues


 