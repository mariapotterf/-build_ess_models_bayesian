
# -----------------------------------
# Learn to build Bayesian models
# ----------------------------------
# https://medium.com/@ODSC/building-your-first-bayesian-model-in-r-7aa5dd304800

# Need to reproduduce Bayesian models to predict ESS based on Tord's Snall article
# as Tord can not provide them

# To learn: 
# how to create bayesian models? 
# do I have all the predictors and estimates to recreates it from teh paper???
# What predictors are needed??

# Bayes theorem: descrivbes teh conditional probability of an event 

# bayesian" making probabilistic predictions 
#while including proior information into analysis,
# estimate missing values
# R: build first model: https://medium.com/@ODSC/building-your-first-bayesian-model-in-r-7aa5dd304800

# -------------------------

# predict a price of harvware: new cellphone

# simulated data: predict the price as exponetial decay equation -
# price decrease over time

# Pt = price at time t
# b0 = starting price at time 0
# Z - rate of decline

Pt = 3000*e^-(Zt) + b0


# Build a dataset
# -----------------

############## Build Dataset #################################
# Lets model the decline of say solar panel prices
# add variation in data

time<-seq(1,5*365)

z = 0.003 # this is the true rate of decline, a rate we will later try to estimate
b = 500  # absolute minimum price likely in near future, asymptote


price = list()
for (i in 1:length(time)){
  price[i]<-3000*exp(-z*time[i])+b+rnorm(1,0,200)
}


jpeg("Simulated Price.jpg",width = 2000, height=2000,res = 300)
plot(unlist(price), xlab='Time (Days)', 
     ylab='Price', 
     cex.axis=1.2, cex.lab=1.2)
dev.off()

df = data.frame(Time=unlist(time),Price=unlist(price))
write.csv(df, "Bayes_dat.csv")


# ---------------------
# Start modelling:
# ---------------------

library(R2OpenBUGS)
library(rjags)     # Just another Gibbs Sampler - support Bayesian modelling
library(coda)
library(MCMCvis)   # Markov Chain Monte Carlo

# inspect the data
df <- read.csv("Bayes_dat.csv", stringsAsFactors = F)
head(df)
length(df$Time)


# Start with model: we are going to estimate b0 (asymptote) and Z (rate of decline)
# by making a model
# as Bayesian model, we need to to come up with priors independent of teh data
# as we do not have any prior knowledge, we will use vaque priors:
# get normal disyribution with a very large standsrd deviation:
# this is in R2OpenBUGS::dnorm(mean, precision); precisions = 1/variance

# Setup Model: two parts: priors and likelihood 
# of getting a data given a certain parameter value
# priors are the guessed distributions of 
# distributions of 

mod = function(){
  #priors
  b0 ~ dnorm(0,.001)  # guessed values, very large as we currently do not have any knowledge
  Z  ~ dnorm(0,.001)   # normal distribution
  
  sigma~dunif(0,100)
  tau<-1/(sigma*sigma)
  
  
  #likelihood
  for(i in 1:1825){
    mu[i]<-b0+3000*exp(-Z*Time[i]) # population mean
    Price[i]~dnorm(mu[i], tau)
    Price_pred[i]~dnorm(mu[i], tau)
  }
  
}

