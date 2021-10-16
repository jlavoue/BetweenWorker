#' ---
#' title: "Descriptive analysis of the Kromhout et al 1993 database"
#' author: "Jérôme Lavoué"
#' date: "October 15, 2021"
#' output: github_document
#' ---
#' 
#' 

library(ggplot2)

data <- read.csv('Raw data/Kromout93Table/kromhout.gsd.database.csv',stringsAsFactors=F)

data$rho <- data$sb^2 / (data$sb^2 + data$sw^2)

#' The ECDF of the 165 within worker correlation values in Kromhout et al 1993

ggplot( data , aes(rho) ) + stat_ecdf(geom = "point") + labs(title="Empirical Cumulative Density Function",y = "Cumulative probability", x="Within worker correlation")
