---
title: "about"
author: "Harry Tattan-Birch"
date: "03/02/2021"
output: html_document
runtime: shiny
---


#### R code for calculator

The following R code was used for this calculator. It was written by Bence Palfi and Zoltan Dienes.

```{r setup}

Bf<-function(sd, obtained, dfdata = 1, likelihood = c("normal", "t"), modeloftheory= c("normal","t","cauchy", "uniform") ,lower =0, upper=1, modeoftheory = 0, scaleoftheory = 1, dftheory = 1, tail = 2)
{
  if(likelihood=="normal"){
    dfdata=10^10
  }
  if(modeloftheory=="normal"){
    dftheory = 10^10
  } else if(modeloftheory=="cauchy"){
    dftheory = 1
  }
  area <- 0
  normarea <- 0
  if(modeloftheory=="uniform"){
    theta <- lower
    range <- upper - lower
    incr <- range / 2000
    for (A in -1000:1000){
      theta <- theta + incr
      dist_theta <- 1 / range
      height <- dist_theta * dt((obtained-theta)/sd, df=dfdata)
      area <- area + height * incr
    }
    LikelihoodTheory <- area
  }else{
    theta <- modeoftheory - 8 * scaleoftheory
    incr <- scaleoftheory/200
    for (A in -1600:1600){
      theta <- theta + incr
      dist_theta <- dt((theta-modeoftheory)/scaleoftheory, df=dftheory)
      
      if(tail==1){
        if (theta <= modeoftheory){
          dist_theta <- 0
        } else {
          dist_theta <- dist_theta * 2
        }
      }
      height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
      area <- area + height * incr
      normarea <- normarea + dist_theta*incr
    }
    LikelihoodTheory <- area/normarea
  }
  LikelihoodNull <- dt(obtained/sd, df = dfdata)
  BayesFactor <- LikelihoodTheory/LikelihoodNull
  
  BayesFactor
  
}

```