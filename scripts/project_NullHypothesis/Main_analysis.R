#' ---
#' title: "Testing the null hypothesis that the Kromhout et al. observations were due to chance"
#' author: "Jérôme Lavoué"
#' date: "October 19, 2021"
#' output: github_document
#' ---
#' 
#' 

#+ r setup, include=FALSE, cache = FALSE
    require("knitr")
    ## setting working directory
    opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

#+ libraries, include = FALSE

    library(ggplot2)
    library(ggthemes)
    
#+ data loading, include = FALSE

    fullmat <- readRDS( 'results/project_NullHypothesis/sumulation_array.RDS')
    
    data <- read.csv('Raw data/Kromout93Table/kromhout.gsd.database.csv',stringsAsFactors=F)
    

#+ Graph preparation, include = F
   
    ## data.prep
    
    data$rho.obs <- data$sb^2 / ( data$sw^2 + data$sb^2 )
    
    data$rho.min <- apply( fullmat[ , 3 , ] , 1 , function(x) { quantile(x,0) } )
    
    data$rho.q5 <- apply( fullmat[ , 3 , ] , 1 , function(x) { quantile(x,0.05) } )
    
    data$rho.q25 <- apply( fullmat[ , 3 , ] , 1 , function(x) { quantile(x,0.25) } )
    
    data$rho.q50 <- apply( fullmat[ , 3 , ] , 1 , function(x) { quantile(x,0.5) } )
    
    data$rho.q75 <- apply( fullmat[ , 3 , ] , 1 , function(x) { quantile(x,0.75) } )
    
    data$rho.q95 <- apply( fullmat[ , 3 , ] , 1 , function(x) { quantile(x,0.95) } )
    
    data$rho.max <- apply( fullmat[ , 3 , ] , 1 , function(x) { quantile(x,1) } )
    
    data <- data[ order(data$rho.obs , decreasing = FALSE) , ]
    
    ###### sequential plot
    
    
    p1 <- ggplot( data = data , aes( x = 1:length(data[,1])) )
    
    p1 <- p1 + geom_point( aes( y = rho.obs ))
    
    p1 <- p1 + geom_point( aes( y = rho.q50) , color = "red")
    
    p1 <- p1 + geom_segment( aes( x = 1:length(data[,1]) , xend = 1:length(data[,1]),
                                y = rho.q5 , yend = rho.q95) , color="orange")
    
    p1 <- p1 + theme_calc()
    
    
    ##### ECDF 
    
    n <- 500
    
    df <- vector( length = n , mode = "list")
    
    for (i in 1:n) df[[i]] <- data.frame(rho = fullmat[ , 3 , i ] )
    
    p2 <- ggplot( data, aes(x = rho.obs))  
    
    p2 <- p2 + stat_ecdf(geom = "point") + labs(title="Empirical Cumulative Density Function",y = "Cumulative probability", x="Within worker correlation")
    
    for ( i in 1:n) p2 <- p2 + stat_ecdf(data=df[[i]], aes(x = rho) , geom = "point" , color = "lightgrey")
    
    p2 <- p2 + theme_calc()
    
    p2 <- p2 + scale_x_continuous( expand = c(0, 0)  ) 
    
    p2 <- p2 + scale_y_continuous( expand = c(0, 0)  )
    

#'  For each line in the public [Kromhout et al's](https://pubmed.ncbi.nlm.nih.gov/8346874/) table (A1), which had total n, number of workers(k), and a calculable value of rho, 500 random samples with the same n and k but rho=0 were generated.
    
#' So for each rho observed and reported in A1, I obtained 500 values of rho simulated under the null hypothesis of rho=0.
#'   
#'   
#' I used the calculations presented by [NVVa/BOHS](https://www.arbeidshygiene.nl/-uploads/files/insite/2011-12-bohs-nvva-sampling-strategy-guidance.pdf) and taken from several papers by Rappaport/Lyles/Kromhout. One tricky aspect is that for unbalanced data (most of times, n is not a multiple of k in the table), the traditional ANOVA formulae have to be adjusted for the calculation of between worker variance, with the calculation of a kind of "mean" number of repeats per worker (n0). There are warnings all over the internet that unbalanced ANOVA is not very good. To be reassured, I estimated rho using this approach and also using a random effect model fit through restricted maximum likelihood. The results were not equal but similar enough for this exercise ; I used the ANOVA formulae for the main analysis for calculation speed (comparred to the optimisation used in REML estimation for the random effect model)
#' 
#'     
#' Below are the results of this effort : the black ECDF is the observed rho, the grey part is made from the 500 ECDFs obtained from the simulation procedure.
#' 
#' 
#+ graph, echo = FALSE

    p2
   