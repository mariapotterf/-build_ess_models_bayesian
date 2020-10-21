library(car);library(MASS);library(lattice);library(sciplot);library(rethinking)
setwd("    ")
data<-read.csv("AFB-CFU-all.csv")
#fob
data$FOB2<-data$FOB*2
#AFB
data$AFB_orig<-data$AFB
data$AFB<-(data$AFB_orig-mean(data$AFB_orig))/sd(data$AFB_orig)
#treat
data$treat2<-as.numeric(data$treat)
#FOB
data$FOB_orig<-data$FOB
data$FOB<-(data$FOB_orig-mean(data$FOB_orig))/sd(data$FOB_orig)
head(data)

#test standardized values
#fig 1
afb_brood_day<-data$AFB_orig/data$Brood/data$NrDay_orig
# x11()
# plot(data$CFU_orig,afb_brood_day)
#fig 2
cfu_fob_day<-data$CFU_orig/data$FOB_orig/data$NrDay_orig
#plot(data$AFB_orig,cfu_fob_day)
#fig 3
cfu_fob_afb<-data$CFU_orig/data$FOB_orig/data$AFB_orig
#plot(data$NrDay_orig,cfu_fob_day)
###############################################################################
################################################################################
#STEP 1: how many random effects?
################################################################################
################################################################################
################################################################################


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 000: #glimmer(CFU_orig~AFB*NrDay*FOB+(1|treat)+(1|CoID)+(1|Obs),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.CFUb.000<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       b_AFB_X_NrDay_X_FOB*AFB*NrDay*FOB +
#       v_treat_Intercept[treat] +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB,b_NrDay_X_FOB,
#       b_AFB_X_NrDay_X_FOB) ~ dnorm(0,1),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
# control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
#saveRDS(m.CFUb.000,"m.CFUb.000.rds")
m.CFUb.000<-readRDS("m.CFUb.000.rds")
##gelman
precis(m.CFU.000)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 00: glimmer(CFU_orig~AFB*NrDay*FOB+(1|CoID)+(1|Obs),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.CFUb.00<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       b_AFB_X_NrDay_X_FOB*AFB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB,b_NrDay_X_FOB,
#       b_AFB_X_NrDay_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
#saveRDS(m.CFUb.00,"m.CFUb.00.rds")
m.CFUb.00<-readRDS("m.CFUb.00.rds")
##gelman
#precis(m.CFU.00)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 0: #glimmer(CFU_orig~AFB*NrDay+(1|CoID),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.CFUb.0<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       b_AFB_X_NrDay_X_FOB*AFB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#      c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB,b_NrDay_X_FOB,
#       b_AFB_X_NrDay_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     sigma_CoID ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
#saveRDS(m.CFUb.0,"m.CFUb.0.rds")
m.CFUb.0<-readRDS("m.CFUb.0.rds")## was arror, but not needed anyway
##gelman
#precis(m.CFU.0,dept=2)

#comparing models
(m.compare<-compare(m.CFUb.00,m.CFUb.000))
#               WAIC pWAIC dWAIC weight    SE  dSE
# m.CFUb.00  2332.4 118.7   0.0   0.98 59.23   NA  #no treat random needed
# m.CFUb.000 2340.4 122.0   7.9   0.02 59.15 3.42  

################################################################################
################################################################################
################################################################################
#STEP 2: interactions important?
################################################################################
################################################################################
################################################################################

##### AFB*NrDay*FOB +++++++++++++
#m.CFUb.ANF<-m.CFUb.00
#save model
#saveRDS(m.CFUb.ANF,"m.CFUb.ANF.rds")
m.CFUb.ANF<-readRDS("m.CFUb.ANF.rds")
#precis(m.CFUb.ANF)

##### (AFB+NrDay+FOB)^2 +++++++++++++
# m.CFUb.ANF2<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB,b_NrDay_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
#save model
#saveRDS(m.CFUb.ANF2,"m.CFUb.ANF2.rds")
m.CFUb.ANF2<-readRDS("m.CFUb.ANF2.rds")
#precis(m.CFUb.ANF2)

##### (AFB+NrDay+FOB)^2 +++++++++++++
# m.CFUb.ANF2<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB,b_NrDay_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
#save model
#saveRDS(m.CFUb.ANF2,"m.CFUb.ANF2.rds")
m.CFUb.ANF2<-readRDS("m.CFUb.ANF2.rds")
#precis(m.CFUb.ANF2)

##### AFB+NrDay+FOB+ AN:AF +++++++++++++
# m.CFUb.AN.AF<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.CFUb.AN.AF,"m.CFUb.AN.AF.rds")
m.CFUb.AN.AF<-readRDS("m.CFUb.AN.AF.rds")
#precis(m.CFUb.AN.AF)

##### AFB+NrDay+FOB+ AN:NF +++++++++++++
# m.CFUb.AN.NF<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_NrDay_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.CFUb.AN.NF,"m.CFUb.AN.NF.rds")
m.CFUb.AN.NF<-readRDS("m.CFUb.AN.NF.rds")
#precis(m.CFUb.AN.NF)

##### AFB+NrDay+FOB+ AF NF +++++++++++++
# m.CFUb.AF.NF<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_FOB,b_NrDay_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.CFUb.AF.NF,"m.CFUb.AF.NF.rds")
m.CFUb.AF.NF<-readRDS("m.CFUb.AF.NF.rds")
#precis(m.CFUb.AF.NF)

##### AFB*NrDay+FOB +++++++++++++
# m.CFUb.AN<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
#save model
#saveRDS(m.CFUb.AN,"m.CFUb.AN.rds")
m.CFUb.AN<-readRDS("m.CFUb.AN.rds")
#precis(m.CFUb.AN)
##### AFB+NrDay*FOB +++++++++++++
# m.CFUb.NF<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_NrDay_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
#save model
#saveRDS(m.CFUb.NF,"m.CFUb.NF.rds")
m.CFUb.NF<-readRDS("m.CFUb.NF.rds")
precis(m.CFUb.NF)
##### AFB*FOB+NrDay +++++++++++++
# m.CFUb.AF<-map2stan(
#   alist(
#     CFU_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_FOB*AFB*FOB +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_FOB) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
#save model
#saveRDS(m.CFUb.AF,"m.CFUb.AF.rds")
m.CFUb.AF<-readRDS("m.CFUb.AF.rds")
precis(m.CFUb.AF)
#comparing models
(comp.all<-compare(m.CFUb.ANF,m.CFUb.ANF2,
               m.CFUb.AF.NF,m.CFUb.AN.NF,m.CFUb.AN.AF,
               m.CFUb.AN,m.CFUb.NF,m.CFUb.AF))
#                WAIC pWAIC dWAIC weight    SE  dSE
# m.CFUb.AN    2330.6 118.4   0.0   0.41 59.81   NA
# m.CFUb.ANF   2332.4 118.7   1.8   0.16 59.23 3.92
# m.CFUb.NF    2332.7 118.6   2.1   0.15 59.56 3.75
# m.CFUb.AF.NF 2333.4 119.3   2.8   0.10 59.51 3.91
# m.CFUb.AF    2333.5 119.5   2.9   0.09 59.60 3.43
# m.CFUb.AN.NF 2334.6 120.2   4.0   0.06 59.27 3.77
# m.CFUb.AN.AF 2336.7 121.0   6.1   0.02 59.58 3.90
# m.CFUb.ANF2  2340.0 121.8   9.4   0.00 59.82 3.63
(comp<-compare(m.CFUb.AN,m.CFUb.ANF,m.CFUb.NF,m.CFUb.AF.NF))
#                WAIC pWAIC dWAIC weight    SE  dSE
# m.CFUb.AN    2330.6 118.4   0.0   0.50 59.81   NA
# m.CFUb.ANF   2332.4 118.7   1.8   0.20 59.23 3.92
# m.CFUb.NF    2332.7 118.6   2.1   0.18 59.56 3.75
# m.CFUb.AF.NF 2333.4 119.3   2.8   0.12 59.51 3.91

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model validation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# #gelman
# precis(m.CFUb.ANF)#,dept=2)
# #check chains
# x11(8,8)
# plot(m.CFUb.ANF)#,pars=c("Intercept","b_CFU")
# #plot model to see correlations
# x11()
# pairs(m.CFUb.ANF,pars=c("Intercept","b_AFB","b_NrDay","b_FOB"))
# pairs(m.CFUb.ANF,pars=c("b_AFB_X_NrDay","b_AFB_X_FOB","b_NrDay_X_FOB",
#                         "b_AFB_X_NrDay_X_FOB"))
# #predictions original data
# x11()
# postcheck(m.CFUb.ANF)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# posterior
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pos.CFUb.AN<-extract.samples(m.CFUb.AN)
pos.CFUb.ANF<-extract.samples(m.CFUb.ANF)
pos.CFUb.NF<-extract.samples(m.CFUb.NF)
pos.CFUb.AF.NF<-extract.samples(m.CFUb.AF.NF)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# plotting predictions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### sequences
AFB.seq<-seq(min(data$AFB),max(data$AFB),length.out=nrow(data))
NrDay.seq<-seq(min(data$NrDay),max(data$NrDay),length.out=nrow(data))
FOB.seq<-seq(min(data$FOB),max(data$FOB),length.out=nrow(data))
#### unscale
AFB.seq.un<-seq(min(data$AFB_orig),max(data$AFB_orig),
                length.out=nrow(data))
NrDay.seq.un<-seq(min(data$NrDay_orig),max(data$NrDay_orig),
                  length.out=nrow(data))
FOB.seq.un<-seq(min(data$FOB_orig),max(data$FOB_orig),
                  length.out=nrow(data))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: AFB*(F_6+F_9+F_12)*(N_20+N_50+N_80)

summary(data$FOB_orig)## will use 1stQ, mean, and 3rdQ (approximately)
summary(data$NrDay_orig)
F_6<-approx(data$FOB_orig,data$FOB,6)$y
F_9<-approx(data$FOB_orig,data$FOB,9)$y
F_12<-approx(data$FOB_orig,data$FOB,12)$y
F_all<-c(F_6,F_9,F_12)
N_20<-approx(data$NrDay_orig,data$NrDay,20)$y
N_50<-approx(data$NrDay_orig,data$NrDay,50)$y
N_80<-approx(data$NrDay_orig,data$NrDay,80)$y

#posterior day 20
pred.ANF.N_20<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_20[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*AFB.seq[i]+
            pos.CFUb.AN$b_NrDay*N_20+
            pos.CFUb.AN$b_FOB*F_all[j]+
            pos.CFUb.AN$b_AFB_X_NrDay*AFB.seq[i]*N_20)+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*AFB.seq[i]+
            pos.CFUb.ANF$b_NrDay*N_20+
            pos.CFUb.ANF$b_FOB*F_all[j]+
            pos.CFUb.ANF$b_NrDay_X_FOB*N_20*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_FOB*AFB.seq[i]*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_NrDay*AFB.seq[i]*N_20+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*AFB.seq[i]*N_20*F_all[j])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*AFB.seq[i]+
            pos.CFUb.NF$b_NrDay*N_20+
            pos.CFUb.NF$b_FOB*F_all[j]+
            pos.CFUb.NF$b_NrDay_X_FOB*N_20*F_all[j])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*AFB.seq[i]+
            pos.CFUb.AF.NF$b_NrDay*N_20+
            pos.CFUb.AF.NF$b_FOB*F_all[j]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*N_20*F_all[j]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}
#posterior day 50
pred.ANF.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_50[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*AFB.seq[i]+
            pos.CFUb.AN$b_NrDay*N_50+
            pos.CFUb.AN$b_FOB*F_all[j]+
            pos.CFUb.AN$b_AFB_X_NrDay*AFB.seq[i]*N_50)+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*AFB.seq[i]+
            pos.CFUb.ANF$b_NrDay*N_50+
            pos.CFUb.ANF$b_FOB*F_all[j]+
            pos.CFUb.ANF$b_NrDay_X_FOB*N_50*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_FOB*AFB.seq[i]*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_NrDay*AFB.seq[i]*N_50+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*AFB.seq[i]*N_50*F_all[j])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*AFB.seq[i]+
            pos.CFUb.NF$b_NrDay*N_50+
            pos.CFUb.NF$b_FOB*F_all[j]+
            pos.CFUb.NF$b_NrDay_X_FOB*N_50*F_all[j])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*AFB.seq[i]+
            pos.CFUb.AF.NF$b_NrDay*N_50+
            pos.CFUb.AF.NF$b_FOB*F_all[j]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*N_50*F_all[j]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}
#posterior day 80
pred.ANF.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_80[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*AFB.seq[i]+
            pos.CFUb.AN$b_NrDay*N_80+
            pos.CFUb.AN$b_FOB*F_all[j]+
            pos.CFUb.AN$b_AFB_X_NrDay*AFB.seq[i]*N_80)+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*AFB.seq[i]+
            pos.CFUb.ANF$b_NrDay*N_80+
            pos.CFUb.ANF$b_FOB*F_all[j]+
            pos.CFUb.ANF$b_NrDay_X_FOB*N_80*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_FOB*AFB.seq[i]*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_NrDay*AFB.seq[i]*N_80+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*AFB.seq[i]*N_80*F_all[j])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*AFB.seq[i]+
            pos.CFUb.NF$b_NrDay*N_80+
            pos.CFUb.NF$b_FOB*F_all[j]+
            pos.CFUb.NF$b_NrDay_X_FOB*N_80*F_all[j])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*AFB.seq[i]+
            pos.CFUb.AF.NF$b_NrDay*N_80+
            pos.CFUb.AF.NF$b_FOB*F_all[j]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*N_80*F_all[j]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}
##########
##########
# Figure 2
##########
##########
XAXT=c("s","n","n")
Y1=3000;Y2=3500
#x11(12,12)
tiff("afb-cfu.Fig.2_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,15),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1,Y1*.33,Y1*.66),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_20[[i]],2,median))
  rect(-.1,Y1,17,Y2,col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1,Y1*.33,Y1*.66),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_50[[i]],2,median))
  rect(-.1,Y1,17,Y2,col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1,Y1*.33,Y1*.66),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_80[[i]],2,median))
  rect(-.1,Y1,17,Y2,col="white",border=NA)}
mtext(c("[AFB score/colony]","Symptoms"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Spores","[CFU/Bee]"),
      2,c(4.1,2.5),cex=c(1.5,1.2),outer=T,las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Bees 6","Bees 9","Bees 12"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########

##########
##########
# Figure s4 (full range)
##########
##########
XAXT=c("s","n","n")
Y1=7e+5;Y2=8e+5
#x11(12,12)
tiff("afb-cfu.Fig.S4_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,15),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1,Y1*.33,Y1*.66),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_20[[i]],2,median))
  rect(-.1,Y1,17,Y2,col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1,Y1*.33,Y1*.66),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_50[[i]],2,median))
  rect(-.1,Y1,17,Y2,col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1,Y1*.33,Y1*.66),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_80[[i]],2,median))
  rect(-.1,Y1,17,Y2,col="white",border=NA)}
mtext(c("[AFB score/colony]","Symptoms"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Spores","[CFU/Bee]"),
      2,c(4.1,2.5),cex=c(1.5,1.2),outer=T,las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Bees 6","Bees 9","Bees 12"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: day*(A_me+A_2+A_10)*(F_6+F_9+F_12)
summary(data$AFB_orig)
summary(data$NrDay_orig)
# x11()
# hist(data$AFB_orig,breaks=15)
A_me<-approx(data$AFB_orig,data$AFB,1.5)$y#
A_5<-approx(data$AFB_orig,data$AFB,5)$y
A_10<-approx(data$AFB_orig,data$AFB,10)$y
F_6<-approx(data$FOB_orig,data$FOB,6)$y
F_9<-approx(data$FOB_orig,data$FOB,9)$y
F_12<-approx(data$FOB_orig,data$FOB,12)$y
F_all<-c(F_6,F_9,F_12)
#posterior A 1.5
pred.ANF.A_1.5<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                   matrix(NA,nrow=1000,ncol=nrow(data)),
                   matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.A_1.5[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*A_me+
            pos.CFUb.AN$b_NrDay*NrDay.seq[i]+
            pos.CFUb.AN$b_FOB*F_all[j]+
            pos.CFUb.AN$b_AFB_X_NrDay*A_me*NrDay.seq[i])+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*A_me+
            pos.CFUb.ANF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.ANF$b_FOB*F_all[j]+
            pos.CFUb.ANF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_FOB*A_me*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_NrDay*A_me*NrDay.seq[i]+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*A_me*NrDay.seq[i]*F_all[j])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*A_me+
            pos.CFUb.NF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.NF$b_FOB*F_all[j]+
            pos.CFUb.NF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*A_me+
            pos.CFUb.AF.NF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.AF.NF$b_FOB*F_all[j]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*A_me*F_all[j])}}
#posterior A 5
pred.ANF.A_5<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                   matrix(NA,nrow=1000,ncol=nrow(data)),
                   matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.A_5[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*A_5+
            pos.CFUb.AN$b_NrDay*NrDay.seq[i]+
            pos.CFUb.AN$b_FOB*F_all[j]+
            pos.CFUb.AN$b_AFB_X_NrDay*A_5*NrDay.seq[i])+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*A_5+
            pos.CFUb.ANF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.ANF$b_FOB*F_all[j]+
            pos.CFUb.ANF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_FOB*A_5*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_NrDay*A_5*NrDay.seq[i]+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*A_5*NrDay.seq[i]*F_all[j])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*A_5+
            pos.CFUb.NF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.NF$b_FOB*F_all[j]+
            pos.CFUb.NF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*A_5+
            pos.CFUb.AF.NF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.AF.NF$b_FOB*F_all[j]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*A_5*F_all[j])}}
#posterior A 10
pred.ANF.A_10<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.A_10[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*A_10+
            pos.CFUb.AN$b_NrDay*NrDay.seq[i]+
            pos.CFUb.AN$b_FOB*F_9+
            pos.CFUb.AN$b_AFB_X_NrDay*A_10*NrDay.seq[i])+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*A_10+
            pos.CFUb.ANF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.ANF$b_FOB*F_all[j]+
            pos.CFUb.ANF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_FOB*A_10*F_all[j]+
            pos.CFUb.ANF$b_AFB_X_NrDay*A_10*NrDay.seq[i]+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*A_10*NrDay.seq[i]*F_all[j])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*A_10+
            pos.CFUb.NF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.NF$b_FOB*F_all[j]+
            pos.CFUb.NF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*A_10+
            pos.CFUb.AF.NF$b_NrDay*NrDay.seq[i]+
            pos.CFUb.AF.NF$b_FOB*F_all[j]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*NrDay.seq[i]*F_all[j]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*A_10*F_all[j])}}
##########
##########
# Figure S5
##########
##########
XAXT=c("s","n","n")
X1=120;X2=c(X1*1.2)
#x11(12,12)
tiff("afb-cfu.Fig.S5_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# A 1.5
Y1=3000;Y2=c(Y1*1.3)
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,X1),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.A_1.5[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.ANF.A_1.5[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.ANF.A_1.5[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(v=c(0,X1,X1*.33,X1*.66),h=c(0,Y1,Y1*.33,Y1*.66),
         col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.ANF.A_1.5[[i]],2,median))
  rect(-1,Y1,X2,Y2[i],col="white",border=NA)}
# A 5
Y1=3000;Y2=c(Y1*1.3)
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,X1),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.A_5[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.ANF.A_5[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.ANF.A_5[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(v=c(0,X1,X1*.33,X1*.66),h=c(0,Y1,Y1*.33,Y1*.66),
         col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.ANF.A_5[[i]],2,median))
  rect(-1,Y1,X2,Y2[i],col="white",border=NA)}
# A 10
Y1=3000;Y2=c(Y1*1.3)
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1),xlim=c(0,X1),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.A_10[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.ANF.A_10[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.ANF.A_10[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(v=c(0,X1,X1*.33,X1*.66),h=c(0,Y1,Y1*.33,Y1*.66),
         col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.ANF.A_10[[i]],2,median))
  rect(-1,Y1,X2,Y2[i],col="white",border=NA)}
mtext(c("[Days]","Time"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Spores","[CFU/Bee]"),
      2,c(4.1,2.5),cex=c(1.5,1.2),outer=T,las=0)
mtext(c("Bees 6","Bees 9","Bees 12"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
mtext(c("Symptoms 1.5","Symptoms 5","Symptoms 10"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
par(op);dev.off()
##########
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: FOB*(A_me+A_2+A_10)*(N_20+N_50+N_80)
summary(data$AFB_orig)
summary(data$NrDay_orig)
# x11()
# hist(data$AFB_orig,breaks=15)
A_me<-approx(data$AFB_orig,data$AFB,1.5)$y#
A_5<-approx(data$AFB_orig,data$AFB,5)$y
A_10<-approx(data$AFB_orig,data$AFB,10)$y
A_all<-c(A_me,A_5,A_10)
N_20<-approx(data$NrDay_orig,data$NrDay,20)$y
N_50<-approx(data$NrDay_orig,data$NrDay,50)$y
N_80<-approx(data$NrDay_orig,data$NrDay,80)$y

#posterior day 20
pred.ANF.N_20<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(A_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_20[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*A_all[j]+
            pos.CFUb.AN$b_NrDay*N_20+
            pos.CFUb.AN$b_FOB*FOB.seq[i]+
            pos.CFUb.AN$b_AFB_X_NrDay*A_all[j]*N_20)+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*A_all[j]+
            pos.CFUb.ANF$b_NrDay*N_20+
            pos.CFUb.ANF$b_FOB*FOB.seq[i]+
            pos.CFUb.ANF$b_NrDay_X_FOB*N_20*FOB.seq[i]+
            pos.CFUb.ANF$b_AFB_X_FOB*A_all[j]*FOB.seq[i]+
            pos.CFUb.ANF$b_AFB_X_NrDay*A_all[j]*N_20+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*A_all[j]*N_20*FOB.seq[i])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*A_all[j]+
            pos.CFUb.NF$b_NrDay*N_20+
            pos.CFUb.NF$b_FOB*FOB.seq[i]+
            pos.CFUb.NF$b_NrDay_X_FOB*N_20*FOB.seq[i])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*A_all[j]+
            pos.CFUb.AF.NF$b_NrDay*N_20+
            pos.CFUb.AF.NF$b_FOB*FOB.seq[i]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*N_20*FOB.seq[i]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*A_all[j]*FOB.seq[i])}}
#posterior day 50
pred.ANF.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(A_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_50[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*A_all[j]+
            pos.CFUb.AN$b_NrDay*N_50+
            pos.CFUb.AN$b_FOB*FOB.seq[i]+
            pos.CFUb.AN$b_AFB_X_NrDay*A_all[j]*N_50)+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*A_all[j]+
            pos.CFUb.ANF$b_NrDay*N_50+
            pos.CFUb.ANF$b_FOB*FOB.seq[i]+
            pos.CFUb.ANF$b_NrDay_X_FOB*N_50*FOB.seq[i]+
            pos.CFUb.ANF$b_AFB_X_FOB*A_all[j]*FOB.seq[i]+
            pos.CFUb.ANF$b_AFB_X_NrDay*A_all[j]*N_50+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*A_all[j]*N_50*FOB.seq[i])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*A_all[j]+
            pos.CFUb.NF$b_NrDay*N_50+
            pos.CFUb.NF$b_FOB*FOB.seq[i]+
            pos.CFUb.NF$b_NrDay_X_FOB*N_50*FOB.seq[i])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*A_all[j]+
            pos.CFUb.AF.NF$b_NrDay*N_50+
            pos.CFUb.AF.NF$b_FOB*FOB.seq[i]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*N_50*FOB.seq[i]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*A_all[j]*FOB.seq[i])}}
#posterior day 80
pred.ANF.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(A_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_80[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.CFUb.AN$Intercept+
            pos.CFUb.AN$b_AFB*A_all[j]+
            pos.CFUb.AN$b_NrDay*N_80+
            pos.CFUb.AN$b_FOB*FOB.seq[i]+
            pos.CFUb.AN$b_AFB_X_NrDay*A_all[j]*N_80)+
      comp@output$weight[2]*
      exp(pos.CFUb.ANF$Intercept+
            pos.CFUb.ANF$b_AFB*A_all[j]+
            pos.CFUb.ANF$b_NrDay*N_80+
            pos.CFUb.ANF$b_FOB*FOB.seq[i]+
            pos.CFUb.ANF$b_NrDay_X_FOB*N_80*FOB.seq[i]+
            pos.CFUb.ANF$b_AFB_X_FOB*A_all[j]*FOB.seq[i]+
            pos.CFUb.ANF$b_AFB_X_NrDay*A_all[j]*N_80+
            pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*A_all[j]*N_80*FOB.seq[i])+
      comp@output$weight[3]*
      exp(pos.CFUb.NF$Intercept+
            pos.CFUb.NF$b_AFB*A_all[j]+
            pos.CFUb.NF$b_NrDay*N_80+
            pos.CFUb.NF$b_FOB*FOB.seq[i]+
            pos.CFUb.NF$b_NrDay_X_FOB*N_80*FOB.seq[i])+
      comp@output$weight[4]*
      exp(pos.CFUb.AF.NF$Intercept+
            pos.CFUb.AF.NF$b_AFB*A_all[j]+
            pos.CFUb.AF.NF$b_NrDay*N_80+
            pos.CFUb.AF.NF$b_FOB*FOB.seq[i]+
            pos.CFUb.AF.NF$b_NrDay_X_FOB*N_80*FOB.seq[i]+
            pos.CFUb.AF.NF$b_AFB_X_FOB*A_all[j]*FOB.seq[i])}}
##########
##########
# Figure S6
##########
##########
XAXT=c("s","n","n")
Y1=c(1e+3,.5e+4,10e+4);Y2=c(Y1*1.3)
#Y1=rep(30000,3);Y2=c(Y1*1.3)
#x11(12,12)
tiff("afb-cfu.Fig.S6_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1[i]),xlim=c(0,20),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.97),FOB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.89),FOB.seq.un)
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.67),FOB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1[i],Y1[i]*.33,Y1[i]*.66),col="grey85",xpd=F)
  lines(FOB.seq.un,apply(pred.ANF.N_20[[i]],2,median))
  rect(-1,Y1[i],23,Y2[i],col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1[i]),xlim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.97),FOB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.89),FOB.seq.un)
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.67),FOB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1[i],Y1[i]*.33,Y1[i]*.66),col="grey85",xpd=F)
  lines(FOB.seq.un,apply(pred.ANF.N_50[[i]],2,median))
  rect(-1,Y1[i],23,Y2[i],col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,Y1[i]),xlim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.97),FOB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.89),FOB.seq.un)
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.67),FOB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,Y1[i],Y1[i]*.33,Y1[i]*.66),col="grey85",xpd=F)
  lines(FOB.seq.un,apply(pred.ANF.N_80[[i]],2,median))
  rect(-1,Y1[i],23,Y2[i],col="white",border=NA)}
mtext(c("[Frames of bees]","Bees"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Spores","[CFU/Bee]"),
      2,c(4.1,2.5),cex=c(1.5,1.2),outer=T,las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Symptoms 1.5","Symptoms 5","Symptoms 10"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# calculate stuff
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#--------------------------------------------------
#the overall effect 
#--------------------------------------------------
#(keeping other predictors at 0 (meaning interactions
# are 0 also))
names(pos.CFUb.ANF)
#Intercept--------------------------------------------------
Intercept.post<-
  exp(pos.CFUb.AN$Intercept)   *comp@output$weight[1]+
  exp(pos.CFUb.ANF$Intercept)  *comp@output$weight[2]+
  exp(pos.CFUb.NF$Intercept)   *comp@output$weight[3]+
  exp(pos.CFUb.AF.NF$Intercept)*comp@output$weight[4]
x11()
hist(Intercept.post,breaks=100)
mean(Intercept.post);sd(Intercept.post);HPDI(Intercept.post,prob=0.97)
# [1] 286.0811
# [1] 65.6874
# |0.97    0.97| 
#   159.8305 432.0579 
1-ecdf(Intercept.post)(0) #1
#b_AFB--------------------------------------------------
b_AFB.post<-
  exp(pos.CFUb.AN$b_AFB)   *comp@output$weight[1]+
  exp(pos.CFUb.ANF$b_AFB)  *comp@output$weight[2]+
  exp(pos.CFUb.NF$b_AFB)   *comp@output$weight[3]+
  exp(pos.CFUb.AF.NF$b_AFB)*comp@output$weight[4]
x11()
hist(b_AFB.post,breaks=100)
mean(b_AFB.post);sd(b_AFB.post);HPDI(b_AFB.post,prob=0.97)
# [1] 3.512284
# [1] 0.6885862
# |0.97    0.97| 
#   2.204570 5.062249 
1-ecdf(b_AFB.post)(0) #1
#b_NrDay--------------------------------------------------
b_NrDay.post<-
  exp(pos.CFUb.AN$b_NrDay)   *comp@output$weight[1]+
  exp(pos.CFUb.ANF$b_NrDay)  *comp@output$weight[2]+
  exp(pos.CFUb.NF$b_NrDay)   *comp@output$weight[3]+
  exp(pos.CFUb.AF.NF$b_NrDay)*comp@output$weight[4]
x11()
hist(b_NrDay.post,breaks=100)
mean(b_NrDay.post);sd(b_NrDay.post);HPDI(b_NrDay.post,prob=0.97)
# [1] 0.6306862
# [1] 0.0805733
# |0.97     0.97| 
#   0.4668196 0.8175373
1-ecdf(b_NrDay.post)(0) #1
#b_FOB--------------------------------------------------
b_FOB.post<-
  exp(pos.CFUb.AN$b_FOB)   *comp@output$weight[1]+
  exp(pos.CFUb.ANF$b_FOB)  *comp@output$weight[2]+
  exp(pos.CFUb.NF$b_FOB)   *comp@output$weight[3]+
  exp(pos.CFUb.AF.NF$b_FOB)*comp@output$weight[4]
x11()
hist(b_FOB.post,breaks=100)
mean(b_FOB.post);sd(b_FOB.post);HPDI(b_FOB.post,prob=0.97)
# [1] 1.081176
# [1] 0.1550919
# |0.97     0.97| 
#   0.7833271 1.4633688 
1-ecdf(b_FOB.post)(0) #1

#--------------------------------------------------
#higher predictivness of predictor
#--------------------------------------------------

AFB.FOB.post<-b_AFB.post-b_FOB.post
x11()
hist(AFB.FOB.post,breaks=100)
mean(AFB.FOB.post);sd(AFB.FOB.post);HPDI(AFB.FOB.post,prob=0.97)
# [1] 2.431108
# [1] 0.7413716
# |0.97     0.97| 
#   0.9748559 4.1744673 
1-ecdf(AFB.FOB.post)(0) #100 %  #better to evaluate AFB

FOB.Day.post<-b_FOB.post-b_NrDay.post
x11()
hist(FOB.Day.post,breaks=100)
mean(FOB.Day.post);sd(FOB.Day.post);HPDI(FOB.Day.post,prob=0.97)
# [1] 0.4504901
# [1] 0.1906129
# |0.97      0.97| 
#   0.04578988 0.87609499 
1-ecdf(FOB.Day.post)(0) #100 %  #better to evaluate FOB

AFB.Day.post<-b_AFB.post-b_NrDay.post
x11()
hist(AFB.Day.post,breaks=100)
mean(AFB.Day.post);sd(AFB.Day.post);HPDI(AFB.Day.post,prob=0.97)
# [1] 2.881598
# [1] 0.7105817
# |0.97    0.97| 
#   1.577087 4.499911
1-ecdf(AFB.Day.post)(0) #100 %  #better to evaluate AFB

#--------------------------------------------------
####function for values
#--------------------------------------------------
pred.function<-function(A_x,N_x,F_x)  comp@output$weight[1]*
  exp(pos.CFUb.AN$Intercept+
        pos.CFUb.AN$b_AFB*A_x+
        pos.CFUb.AN$b_NrDay*N_x+
        pos.CFUb.AN$b_FOB*F_x+
        pos.CFUb.AN$b_AFB_X_NrDay*A_x*N_x)+
  comp@output$weight[2]*
  exp(pos.CFUb.ANF$Intercept+
        pos.CFUb.ANF$b_AFB*A_x+
        pos.CFUb.ANF$b_NrDay*N_x+
        pos.CFUb.ANF$b_FOB*F_x+
        pos.CFUb.ANF$b_NrDay_X_FOB*N_x*F_x+
        pos.CFUb.ANF$b_AFB_X_FOB*A_x*F_x+
        pos.CFUb.ANF$b_AFB_X_NrDay*A_x*N_x+
        pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*A_x*N_x*F_x)+
  comp@output$weight[3]*
  exp(pos.CFUb.NF$Intercept+
        pos.CFUb.NF$b_AFB*A_x+
        pos.CFUb.NF$b_NrDay*N_x+
        pos.CFUb.NF$b_FOB*F_x+
        pos.CFUb.NF$b_NrDay_X_FOB*N_x*F_x)+
  comp@output$weight[4]*
  exp(pos.CFUb.AF.NF$Intercept+
        pos.CFUb.AF.NF$b_AFB*A_x+
        pos.CFUb.AF.NF$b_NrDay*N_x+
        pos.CFUb.AF.NF$b_FOB*F_x+
        pos.CFUb.AF.NF$b_NrDay_X_FOB*N_x*F_x+
        pos.CFUb.AF.NF$b_AFB_X_FOB*A_x*F_x)
#--------------------------------------------------
#### spores at zero afb score
#--------------------------------------------------
pos.CFUb.zero<-
  pred.function(approx(data$AFB_orig,data$AFB,0)$y,
                approx(data$NrDay_orig,data$NrDay,50)$y,
                approx(data$FOB_orig,data$FOB,9)$y)
x11()
hist(pos.CFUb.zero,breaks=100,freq=F)
mean(pos.CFUb.zero);sd(pos.CFUb.zero);HPDI(pos.CFUb.zero,prob=0.97)
# [1] 158.2685
# [1] 36.588
# |0.97     0.97| 
# 92.63743 241.76841
1-ecdf(pos.CFUb.zero)(0) #1
#--------------------------------------------------
#### what spore if scored as diseased colony
#--------------------------------------------------
pos.CFUb.one<-
  pred.function(approx(data$AFB_orig,data$AFB,1)$y,
                approx(data$NrDay_orig,data$NrDay,50)$y,
                approx(data$FOB_orig,data$FOB,9)$y)
x11()
hist(pos.CFUb.one,breaks=100,freq=F)
mean(pos.CFUb.one);sd(pos.CFUb.one);HPDI(pos.CFUb.one,prob=0.97)
# [1] 228.1369
# [1] 53.04218
# |0.97    0.97| 
#   123.5497 349.1203
1-ecdf(pos.CFUb.one)(0) #1

#--------------------------------------------------
#### worst case of dilution effect 
#--------------------------------------------------
#fob .5
pos.CFUb.FOB..5<-
  pred.function(approx(data$AFB_orig,data$AFB,1)$y,
                approx(data$NrDay_orig,data$NrDay,105)$y,
                approx(data$FOB_orig,data$FOB,.5)$y)
#fob 19
pos.CFUb.FOB.19<-
  pred.function(approx(data$AFB_orig,data$AFB,1)$y,
                approx(data$NrDay_orig,data$NrDay,105)$y,
                approx(data$FOB_orig,data$FOB,19)$y)
##diff
pos.CFUb.FOB.dif<-pos.CFUb.FOB..5-pos.CFUb.FOB.19
x11()
hist(pos.CFUb.FOB.dif,breaks=100,freq=F)
mean(pos.CFUb.FOB.dif);sd(pos.CFUb.FOB.dif);HPDI(pos.CFUb.FOB.dif,prob=0.97)
(1-ecdf(pos.CFUb.FOB.dif)(0))*100 
# [1] 625.8424
# [1] 555.1116
# |0.97     0.97| 
#   -65.4731 1835.5399 
#99.5

#--------------------------------------------------
#### likely case of dilution effect 
#--------------------------------------------------

###Likely one
#fob 5
pos.CFUb.FOB.5<-
  pred.function(approx(data$AFB_orig,data$AFB,2)$y,
                approx(data$NrDay_orig,data$NrDay,50)$y,
                approx(data$FOB_orig,data$FOB,5)$y)
#fob 10
pos.CFUb.FOB.10<-
  pred.function(approx(data$AFB_orig,data$AFB,2)$y,
                approx(data$NrDay_orig,data$NrDay,50)$y,
                approx(data$FOB_orig,data$FOB,10)$y)
pos.CFUb.FOB.dif<-pos.CFUb.FOB.5-pos.CFUb.FOB.10
x11()
hist(pos.CFUb.FOB.dif,breaks=100,freq=F)
mean(pos.CFUb.FOB.dif);sd(pos.CFUb.FOB.dif);HPDI(pos.CFUb.FOB.dif,prob=0.97)
(1-ecdf(pos.CFUb.FOB.dif)(0))*100 
# [1] 16.33546
# [1] 73.38767
# |0.97     0.97| 
#   -116.4440  224.0764
#53.3

###Likely two
#fob 5
pos.CFUb.FOB.5<-
  pred.function(approx(data$AFB_orig,data$AFB,4)$y,
                approx(data$NrDay_orig,data$NrDay,40)$y,
                approx(data$FOB_orig,data$FOB,4)$y)
#fob 10
pos.CFUb.FOB.10<-
  pred.function(approx(data$AFB_orig,data$AFB,4)$y,
                approx(data$NrDay_orig,data$NrDay,40)$y,
                approx(data$FOB_orig,data$FOB,13)$y)
pos.CFUb.FOB.dif<-pos.CFUb.FOB.5-pos.CFUb.FOB.10
x11()
hist(pos.CFUb.FOB.dif,breaks=100,freq=F)
mean(pos.CFUb.FOB.dif);sd(pos.CFUb.FOB.dif);HPDI(pos.CFUb.FOB.dif,prob=0.97)
(1-ecdf(pos.CFUb.FOB.dif)(0))*100 
# [1] 210.0211
# [1] 660.7832
# |0.97     0.97| 
#   -905.4369 1640.0083 
# [1] 55.9








