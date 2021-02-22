---
title: "about"
author: "Harry Tattan-Birch"
date: "12/02/2021"
output: html_document
runtime: shiny
---

#### About Bayes factors

Bayes factors are being increasingly used to assess how far a given set of data support one hypothesis over another. This online tool allows users to calculate Bayes factors from information available from standard statistical packages. It calculates Bayes factors in the case when users want to assess whether they have an effect within a specified range versus no effect.

A common use of Bayes factors is to distinguish between evidence for no effect and lack of clear evidence for an effect. Classical statistics does not allow such a distinction but it is very common for researchers to claim 'no effect' or 'no evidence for an effect' based on not finding a statistically significant difference; this is wrong and should never be done. But you can calculate a Bayes factor to assess the level of support for the 'no effect' claim.

***

#### Creators
This web app was created by Harry Tattan-Birch, with input from Jamie Brown, Robert West and Zoltan Dienes. Special thanks to Bence Palfi and Zoltan Dienes, who created the function used here to calculate Bayes factors (details below).

***

#### Bugs
Please report any bugs on the [Github issues page](https://github.com/HTattanBirch/bayes-factor-calculator/issues). Alternatively, you can email me at [h.tattan-birch@ucl.ac.uk](h.tattan-birch@ucl.ac.uk) if you'd like to suggest specific improvements to this web app. 

***

#### Open source
All the code used to generate this web app is freely available on [Github](https://github.com/HTattanBirch/bayes-factor-calculator).

***

#### R code for calculator

The following R code was used to calculate Bayes factors. It was created by Bence Palfi and Zoltan Dienes.


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