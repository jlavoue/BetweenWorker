#' ---
#' title: "Short validation of the ANOVA procedures"
#' author: "Jérôme Lavoué"
#' date: "October 18, 2021"
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
    library(nlme)
    library(usethis)
    library(here)
    library(httr)
    library(magrittr)
    library(devtools)
    
    GET( "https://raw.githubusercontent.com/webexpo/scripts/master/chemin.R",
         add_headers(Authorization = "token b17593fd0c3381bb8dfd4f377cf6666053690acb")
    ) %>% content %>% str2expression %>% eval(., envir = .GlobalEnv)
    
    chemin(
      fileName = "webexpo.between.randomgeneration.R",
      relPath = c("RANDOM SAMPLE GENERATION"),
      githubCredentials = list(userName = "webexpo", repoName = "webexpo_r_lib")
    )

    source('scripts/functions/anovabetween.R')

   # Simplified function for random generation
   # 100 workers unbalanced with a max of 10 repeats : c(1,10,5,3,6,2,1,1,8,10)
   # total var : sigma=1
   # rho : from 0 to 0.8 by 0.05

#+ Function definiton, include = F
   
   # generating an unblanced dataset       
   myfun.gen <- function( rho ) {
     
    res <-  webexpo.between.gener.LN(     
                n.worker = 10*10 ,
                n.days = rep( c(1,10,5,3,6,2,1,1,8,10) , 10 ), 
                no.censoring = TRUE,
                gsd = exp(1),
                rho = rho)
    
     res$x <- as.numeric(res$x)
     
     return(res)
   }  
   
   # generating an balanced dataset 
   myfun.gen2 <- function( rho ) {
     
     res <-  webexpo.between.gener.LN(     
       n.worker = 100 ,
       n.days = rep(10,100), 
       no.censoring = TRUE,
       gsd = exp(1),
       rho = rho)
     
     res$x <- as.numeric(res$x)
     
     return(res)
   }  
   
   ## for a genrated dataset, calculation of rho using aov and lme methods
   myfun.calc <- function( myworkerdata ) {
     
       res.aov <- var.comp.aov( myworkerdata )
       
       res.lme <- var.comp.lme( myworkerdata )
       
       return( c( res.aov$rho , res.lme$rho ) )
       
   }
   
   ## for a value of rho, simulation of dataset and calculation
   myfun.tot <- function( rho ) {
     
     myworkerdata <- myfun.gen( rho )

     return( myfun.calc( myworkerdata ) )
     
   }
   

#+ Analysis, include=FALSE

   # initialization of sequence of rho values
   myres <- data.frame( rho = seq( from = 0 , to = 0.8 , by = 0.05))
   
   # calculation
   test <- mapply( myfun.tot , rho = myres$rho ,
                   SIMPLIFY = TRUE)
   
   # plotting the results
   myres$aov <- test[ 1 , ]
   
   myres$lme <- test[ 2 , ]
   
   toplot <- data.frame( rho = c( myres$rho , myres$rho) ,
                         est = c( myres$aov , myres$lme) ,
                         method = c( rep("aov",length(myres[,1])),
                                     rep("lme",length(myres[,1]))))
   
   # Initiate a ggplot
   p <- ggplot( toplot, aes(x = rho, y = est , color = method) )
   
   # Basic scatter plot
   p  <- p + geom_point() + geom_abline( intercept = 0 , slope = 1)

#'  For rho values between 0 and 0.8, one sample was simulated (100 workers with 1 to 10 repeats).
#'  rho was estimated using unbalanced ANOVA or LME
#' 
#' Plot below shows the results
#' 

#+ results, echo=FALSE
   p
   
############################## tests
   
   
   #myworkerdata <- myfun.gen( 0.5 )
   
   #myfun.calc( myworkerdata )
   
   #myworkerdata <- myfun.gen2( 0.5 )
   
   #myfun.calc( myworkerdata )
   