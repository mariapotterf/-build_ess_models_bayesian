### Nate Jermain
### 5/27/19
### Basic Bayesian Model for ODSC article



############## Build Dataset #################################
# Lets model the decline of say solar panel prices

time<-seq(1,5*365)

z = .003 # this is the true rate of decline, a rate we will later try to estimate
b = 500 # absolute minimum price likely in near future


price = list()
for (i in 1:length(time)){
  price[i]<-3000*exp(-z*time[i])+b+rnorm(1,0,200)
}


jpeg("Simulated Price.jpg",width = 2000, height=2000,res = 300)
plot(unlist(price), xlab='Time (Days)', ylab='Price', cex.axis=1.2, cex.lab=1.2)
dev.off()

df = data.frame(Time=unlist(time),Price=unlist(price))
write.csv(df, "Bayes_dat.csv")


######### Modeling ######################################




library(R2OpenBUGS)
library(rjags)
library(coda)
library(MCMCvis)

# inspect the data
df <- read.csv("Bayes_dat.csv", stringsAsFactors = F)
head(df)
length(df$Time)


# Now, we are going to estimate b0 (the asymptote) and
# Z (the rate of decline) by setting up a model called 'mod'.

# as it is bayesian model, we need to have some priors independent of the data


# Setup Model: two parts: priors and likelihood 
# of getting a data given a certain parameter value

mod = function(){
  #priors
  b0 ~ dnorm(0,.001)  # guessed values, very large as we currently do not have any knowledge
  Z  ~ dnorm(0,.001)   # normal distribution

  sigma~dunif(0,100)
  tau<-1/(sigma*sigma)
  
  
  #likelihood function
  for(i in 1:1825){
    mu[i]<-b0+3000*exp(-Z*Time[i]) # population mean
    Price[i]~dnorm(mu[i], tau)
    Price_pred[i]~dnorm(mu[i], tau)
  }

}

# write model
model.file="model.txt"
write.model(mod,model.file)

# no initial values
inits<-NULL

# what parameters we want to track
params = c("tau","Z", "b0", "Price_pred")

## hyperparameters
# number of iterations
ni = 10000
# burn in interval
nb = 1000
# thinning interval
nt = 1
# number of chains
nc = 3


# compile model
jmod = jags.model(file = model.file, data = df, n.chains = nc, inits = inits, n.adapt = 1000)

# iterate through jmod for the extent of the burn-in
update(jmod, n.iter=nb, by=1)

# draw samples from the posterior for params, given MCMC hyperparameters
post = coda.samples(jmod, params, n.iter = ni, thin = nt)

# diagnostic evaluation of posterior samples
MCMCtrace(post, params = c('b0','Z'), pdf=F)

# objectively assess convergence with gelmans diagnostic
gelman.diag(post)

# get summary of posterior samples for two parameters
MCMCsummary(post, params = c('b0','Z'), digits=2)

# get samples from posteriors
samples = jags.samples(jmod,c('b0', 'Z', 'Price_pred'), length(df$Time))

# take the mean of each group of samples
posterior_means = apply(samples$Price_pred,1,mean)

# plot posterior means versus observed values
plot(df$Price, posterior_means, ylab='Predicted Price', xlab='Observed Price')
lines(seq(1,3500),seq(1,3500), col='red', lwd=2, cex=3)




