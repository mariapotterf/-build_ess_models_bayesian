library(car);library(MASS);library(lattice);library(sciplot);library(rethinking)
setwd("    ")
data<-read.csv("AFB-CFU-all.csv")
#AFB
data$AFB_orig<-data$AFB
data$AFB<-(data$AFB_orig-mean(data$AFB_orig))/sd(data$AFB_orig)
#FOB2
data$FOB2<-data$FOB*2
#Brood
data$Brood_orig<-data$Brood
data$Brood<-(data$Brood_orig-mean(data$Brood_orig))/sd(data$Brood_orig)
#FOB
data$FOB_orig<-data$FOB
data$FOB<-(data$FOB_orig-mean(data$FOB_orig))/sd(data$FOB_orig)
#FOB2
data$FOB2_orig<-data$FOB2
data$FOB2<-(data$FOB2_orig-mean(data$FOB2_orig))/sd(data$FOB2_orig)
head(data)

# source("panel.cor.r")
# x11(12,12)
# pairs(~AFB_orig+CFU_orig+NrDay_orig+FOB_orig+Brood_orig+Brood_orig2,
#       data=data,upper.panel=panel.smooth,lower.panel=panel.cor)
###############################################################################
################################################################################
#STEP 1: how many random effects?
################################################################################
################################################################################
################################################################################


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 000: #glimmer(FOB2_orig~CFU*NrDay*Brood+(1|treat)+(1|CoID)+(1|Obs),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.FOB2b.000<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_CFU_X_Brood*CFU*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       b_CFU_X_NrDay_X_Brood*CFU*NrDay*Brood +
#       v_treat_Intercept[treat] +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_CFU_X_Brood,b_NrDay_X_Brood,
#       b_CFU_X_NrDay_X_Brood) ~ dnorm(0,1),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
# control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
#saveRDS(m.FOB2b.000,"m.FOB2b.000.rds")
m.FOB2b.000<-readRDS("m.FOB2b.000.rds")
##gelman
precis(m.FOB2b.000)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 00: glimmer(FOB2_orig~CFU*NrDay*Brood+(1|CoID)+(1|Obs),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.FOB2b.00<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_CFU_X_Brood*CFU*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       b_CFU_X_NrDay_X_Brood*CFU*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_CFU_X_Brood,b_NrDay_X_Brood,
#       b_CFU_X_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
#saveRDS(m.FOB2b.000,"m.FOB2b.00.rds")
m.FOB2b.000<-readRDS("m.FOB2b.00.rds")
##gelman
precis(m.FOB2b.00)

#comparing models
(m.compare<-compare(m.FOB2b.00,m.FOB2b.000))
#             WAIC pWAIC dWAIC weight    SE  dSE
# m.FOB2b.00  1453.5  78.2   0.0   0.79 30.69   NA  treat not necessary
# m.FOB2b.000 1456.2  78.6   2.7   0.21 31.59 1.98

################################################################################
################################################################################
################################################################################
#STEP 2: interactions important?
################################################################################
################################################################################
################################################################################

##### CFU*NrDay*Brood +++++++++++++
#m.FOB2b.CNB<-m.FOB2b.00
#save model
#saveRDS(m.FOB2b.CNB,"m.FOB2b.CNB.rds")
m.FOB2b.CNB<-readRDS("m.FOB2b.CNB.rds")

##### CFU+NrDay+Brood+ CFU:NrDay+CFU:Brood+NrDay:Brood +++++++++++++
# m.FOB2b.CNB2<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_CFU_X_Brood*CFU*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_CFU_X_Brood,b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.FOB2b.CNB2,"m.FOB2b.CNB2.rds")
m.FOB2b.CNB2<-readRDS("m.FOB2b.CNB2.rds")
##gelman
precis(m.FOB2b.CNB2)

##### CFU+NrDay+Brood+ CFU:NrDay+NrDay:Brood +++++++++++++
# m.FOB2b.CN.NB<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.FOB2b.CN.NB,"m.FOB2b.CN.NB.rds")
m.FOB2b.CN.NB<-readRDS("m.FOB2b.CN.NB.rds")
##gelman
precis(m.FOB2b.CN.NB)

##### CFU+NrDay+Brood+ CFU:NrDay+CFU:Brood +++++++++++++
# m.FOB2b.CN.CB<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_CFU_X_Brood*CFU*Brood +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_CFU_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.FOB2b.CN.CB,"m.FOB2b.CN.CB.rds")
m.FOB2b.CN.CB<-readRDS("m.FOB2b.CN.CB.rds")
##gelman
precis(m.FOB2b.CN.CB)

##### CFU+NrDay+Brood+ CFU:Brood+NrDay:Brood +++++++++++++
# m.FOB2b.CB.NB<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_Brood*CFU*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#      b_CFU_X_Brood,b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.FOB2b.CB.NB,"m.FOB2b.CB.NB.rds")
m.FOB2b.CB.NB<-readRDS("m.FOB2b.CB.NB.rds")
##gelman
precis(m.FOB2b.CB.NB)

##### CFU+NrDay+Brood+ CFU:NrDay +++++++++++++
# m.FOB2b.CN<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.FOB2b.CN,"m.FOB2b.CN.rds")
m.FOB2b.CN<-readRDS("m.FOB2b.CN.rds")
##gelman
precis(m.FOB2b.CN)

##### CFU+NrDay*Brood +++++++++++++
# m.FOB2b.NB<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#        b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.FOB2b.NB,"m.FOB2b.NB.rds")
m.FOB2b.NB<-readRDS("m.FOB2b.NB.rds")
##gelman
precis(m.FOB2b.NB)

##### CFU*Brood+NrDay +++++++++++++
# m.FOB2b.CB<-map2stan(
#   alist(
#     FOB2_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_Brood*CFU*Brood +
#       v_CoID_Intercept[CoID] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,b_CFU_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.FOB2b.CB,"m.FOB2b.CB.rds")
m.FOB2b.CB<-readRDS("m.FOB2b.CB.rds")
##gelman
precis(m.FOB2b.CB)

#comparing models
(comp.all<-compare(m.FOB2b.CNB,m.FOB2b.CNB2,
               m.FOB2b.CN.NB,m.FOB2b.CN.CB,m.FOB2b.CB.NB,
               m.FOB2b.CN,m.FOB2b.NB,m.FOB2b.CB))
#                 WAIC pWAIC dWAIC weight    SE  dSE
# m.FOB2b.CNB   1453.5  78.2   0.0   0.47 30.69   NA
# m.FOB2b.CN.NB 1454.6  81.0   1.1   0.27 28.60 5.24
# m.FOB2b.CB    1457.2  80.3   3.8   0.07 29.16 7.14
# m.FOB2b.CN.CB 1457.4  78.4   3.9   0.07 30.95 5.50
# m.FOB2b.CNB2  1457.9  80.8   4.4   0.05 30.30 4.72
# m.FOB2b.CB.NB 1459.0  81.9   5.6   0.03 28.17 6.79
# m.FOB2b.CN    1459.3  81.4   5.8   0.03 30.86 5.60
# m.FOB2b.NB    1459.5  82.0   6.0   0.02 28.48 6.71
(comp<-compare(m.FOB2b.CNB,m.FOB2b.CN.NB,m.FOB2b.CB,m.FOB2b.CN.CB))
#                 WAIC pWAIC dWAIC weight    SE  dSE
# m.FOB2b.CNB   1453.5  78.2   0.0   0.54 30.69   NA
# m.FOB2b.CN.NB 1454.6  81.0   1.1   0.30 28.60 5.24
# m.FOB2b.CB    1457.2  80.3   3.8   0.08 29.16 7.14
# m.FOB2b.CN.CB 1457.4  78.4   3.9   0.08 30.95 5.50

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model validation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# to do
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# posterior
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pos.FOB2b.CNB<-extract.samples(m.FOB2b.CNB)
pos.FOB2b.CN.NB<-extract.samples(m.FOB2b.CN.NB)
pos.FOB2b.CB<-extract.samples(m.FOB2b.CB)
pos.FOB2b.CN.CB<-extract.samples(m.FOB2b.CN.CB)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# plotting predictions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### sequences
CFU.seq<-seq(min(data$CFU),max(data$CFU),length.out=nrow(data))
NrDay.seq<-seq(min(data$NrDay),max(data$NrDay),length.out=nrow(data))
Brood.seq<-seq(min(data$Brood),max(data$Brood),length.out=nrow(data))
#### unscale
CFU.seq.un<-seq(min(data$CFU_orig),max(data$CFU_orig),
                length.out=nrow(data))
NrDay.seq.un<-seq(min(data$NrDay_orig),max(data$NrDay_orig),
                  length.out=nrow(data))
Brood.seq.un<-seq(min(data$Brood_orig),max(data$Brood_orig),
                  length.out=nrow(data))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: CFU*(B_80+B_130+B_200)*(N_20+N_50+N_80)

summary(data$Brood_orig)## will use 1stQ, mean, and 3rdQ (approximately)
summary(data$NrDay_orig)
B_80<-approx(data$Brood_orig,data$Brood,80)$y
B_130<-approx(data$Brood_orig,data$Brood,130)$y
B_200<-approx(data$Brood_orig,data$Brood,200)$y
B_all<-c(B_80,B_130,B_200)
N_20<-approx(data$NrDay_orig,data$NrDay,20)$y
N_50<-approx(data$NrDay_orig,data$NrDay,50)$y
N_80<-approx(data$NrDay_orig,data$NrDay,80)$y
names(pos.FOB2b.CNB)
#posterior day 20
pred.CNB.N_20<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_20[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CNB$b_NrDay*N_20+
            pos.FOB2b.CNB$b_Brood*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*CFU.seq[i]*N_20+
            pos.FOB2b.CNB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*N_20*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*CFU.seq[i]*N_20*B_all[j])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CN.NB$b_NrDay*N_20+
            pos.FOB2b.CN.NB$b_Brood*B_all[j]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*CFU.seq[i]*N_20+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*N_20*B_all[j])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CB$b_NrDay*N_20+
            pos.FOB2b.CB$b_Brood*B_all[j]+
            pos.FOB2b.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CN.CB$b_NrDay*N_20+
            pos.FOB2b.CN.CB$b_Brood*B_all[j]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*CFU.seq[i]*N_20+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])}}
#posterior day 50
pred.CNB.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_50[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CNB$b_NrDay*N_50+
            pos.FOB2b.CNB$b_Brood*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*CFU.seq[i]*N_50+
            pos.FOB2b.CNB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*N_50*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*CFU.seq[i]*N_50*B_all[j])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CN.NB$b_NrDay*N_50+
            pos.FOB2b.CN.NB$b_Brood*B_all[j]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*CFU.seq[i]*N_50+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*N_50*B_all[j])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CB$b_NrDay*N_50+
            pos.FOB2b.CB$b_Brood*B_all[j]+
            pos.FOB2b.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CN.CB$b_NrDay*N_50+
            pos.FOB2b.CN.CB$b_Brood*B_all[j]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*CFU.seq[i]*N_50+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])}}
#posterior day 80
pred.CNB.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_80[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CNB$b_NrDay*N_80+
            pos.FOB2b.CNB$b_Brood*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*CFU.seq[i]*N_80+
            pos.FOB2b.CNB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*N_80*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*CFU.seq[i]*N_80*B_all[j])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CN.NB$b_NrDay*N_80+
            pos.FOB2b.CN.NB$b_Brood*B_all[j]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*CFU.seq[i]*N_80+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*N_80*B_all[j])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CB$b_NrDay*N_80+
            pos.FOB2b.CB$b_Brood*B_all[j]+
            pos.FOB2b.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*CFU.seq[i]+
            pos.FOB2b.CN.CB$b_NrDay*N_80+
            pos.FOB2b.CN.CB$b_Brood*B_all[j]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*CFU.seq[i]*N_80+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])}}

##########
##########
# Figure 3
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.3_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,5,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,7e+5),ylim=c(0,20),bty="n",xaxt=XAXT[i])
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.97)/2,CFU.seq.un,col="yellow")
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.89)/2,CFU.seq.un)
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.67)/2,CFU.seq.un)
  abline(h=c(0,5,10,15,20),v=c(0,2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(CFU.seq.un,apply(pred.CNB.N_20[[i]]/2,2,median))
  rect(0,20.1,7e+5,23,col="white",border=NA)  }
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,7e+5),ylim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.97)/2,CFU.seq.un,col="yellow")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.89)/2,CFU.seq.un)
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.67)/2,CFU.seq.un)
  abline(h=c(0,5,10,15,20),v=c(0,2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(CFU.seq.un,apply(pred.CNB.N_50[[i]]/2,2,median))
  rect(0,20.1,7e+5,23,col="white",border=NA)  }
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,7e+5),ylim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.97)/2,CFU.seq.un,col="yellow")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.89)/2,CFU.seq.un)
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.67)/2,CFU.seq.un)
  abline(h=c(0,5,10,15,20),v=c(0,2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(CFU.seq.un,apply(pred.CNB.N_80[[i]]/2,2,median))
  rect(0,20.1,7e+5,23,col="white",border=NA) }
mtext(c("Spores","[CFU/Bee]"),
      1,c(1.5,3.1),cex=c(1.5,1.2),outer=T)
mtext(c("[Frames of bees]","Bees"),
      2,c(1.5,3.1),cex=c(1.2,1.5),outer=T,las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Brood 80","Brood 130","Brood 200"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: NrDay*(B_80+B_130+B_200)*(C_40+C_800+C_15000)

summary(data$Brood_orig)## will use 1stQ, mean, and 3rdQ (approximately)
summary(data$CFU_orig)
B_80<-approx(data$Brood_orig,data$Brood,80)$y
B_130<-approx(data$Brood_orig,data$Brood,130)$y
B_200<-approx(data$Brood_orig,data$Brood,200)$y
B_all<-c(B_80,B_130,B_200)
C_md<-approx(data$CFU_orig,data$CFU,850)$y#
C_3e<-approx(data$CFU_orig,data$CFU,50000)$y
C_6e<-approx(data$CFU_orig,data$CFU,200000)$y
names(pos.FOB2b.CNB)
#posterior CFU 850
pred.CNB.C_md<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.C_md[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*C_md+
            pos.FOB2b.CNB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CNB$b_Brood*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*C_md*NrDay.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_Brood*C_md*B_all[j]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*C_md*NrDay.seq[i]*B_all[j])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*C_md+
            pos.FOB2b.CN.NB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CN.NB$b_Brood*B_all[j]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*C_md*NrDay.seq[i]+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*C_md+
            pos.FOB2b.CB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CB$b_Brood*B_all[j]+
            pos.FOB2b.CB$b_CFU_X_Brood*C_md*B_all[j])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*C_md+
            pos.FOB2b.CN.CB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CN.CB$b_Brood*B_all[j]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*C_md*NrDay.seq[i]+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*C_md*B_all[j])}}
#posterior CFU 50000
pred.CNB.C_3e<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.C_3e[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*C_3e+
            pos.FOB2b.CNB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CNB$b_Brood*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*C_3e*NrDay.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_Brood*C_3e*B_all[j]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*C_3e*NrDay.seq[i]*B_all[j])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*C_3e+
            pos.FOB2b.CN.NB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CN.NB$b_Brood*B_all[j]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*C_3e*NrDay.seq[i]+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*C_3e+
            pos.FOB2b.CB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CB$b_Brood*B_all[j]+
            pos.FOB2b.CB$b_CFU_X_Brood*C_3e*B_all[j])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*C_3e+
            pos.FOB2b.CN.CB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CN.CB$b_Brood*B_all[j]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*C_3e*NrDay.seq[i]+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*C_3e*B_all[j])}}
#posterior CFU 200000
pred.CNB.C_6e<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.C_6e[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*C_6e+
            pos.FOB2b.CNB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CNB$b_Brood*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*C_6e*NrDay.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_Brood*C_6e*B_all[j]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*C_6e*NrDay.seq[i]*B_all[j])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*C_6e+
            pos.FOB2b.CN.NB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CN.NB$b_Brood*B_all[j]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*C_6e*NrDay.seq[i]+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*C_6e+
            pos.FOB2b.CB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CB$b_Brood*B_all[j]+
            pos.FOB2b.CB$b_CFU_X_Brood*C_6e*B_all[j])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*C_6e+
            pos.FOB2b.CN.CB$b_NrDay*NrDay.seq[i]+
            pos.FOB2b.CN.CB$b_Brood*B_all[j]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*C_6e*NrDay.seq[i]+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*C_6e*B_all[j])}}

##########
##########
# Figure s7
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.S7_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,5,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# CFU 40
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,120),ylim=c(0,20),bty="n",xaxt=XAXT[i])
  shade(apply(pred.CNB.C_md[[i]],2,PI,prob=0.97)/2,NrDay.seq.un,col="yellow")
  shade(apply(pred.CNB.C_md[[i]],2,PI,prob=0.89)/2,NrDay.seq.un)
  shade(apply(pred.CNB.C_md[[i]],2,PI,prob=0.67)/2,NrDay.seq.un)
  abline(h=c(0,7.5,15),v=c(0,40,80,120),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.CNB.C_md[[i]]/2,2,median))
  rect(0,20.1,120,23,col="white",border=NA)  }
# CFU 800
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,120),ylim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.C_3e[[i]],2,PI,prob=0.97)/2,NrDay.seq.un,col="yellow")
  shade(apply(pred.CNB.C_3e[[i]],2,PI,prob=0.89)/2,NrDay.seq.un)
  shade(apply(pred.CNB.C_3e[[i]],2,PI,prob=0.67)/2,NrDay.seq.un)
  abline(h=c(0,7.5,15),v=c(0,40,80,120),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.CNB.C_3e[[i]]/2,2,median))
  rect(0,20.1,120,23,col="white",border=NA)  }
# CFU 15000
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,120),ylim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.C_6e[[i]],2,PI,prob=0.97)/2,NrDay.seq.un,col="yellow")
  shade(apply(pred.CNB.C_6e[[i]],2,PI,prob=0.89)/2,NrDay.seq.un)
  shade(apply(pred.CNB.C_6e[[i]],2,PI,prob=0.67)/2,NrDay.seq.un)
  abline(h=c(0,7.5,15),v=c(0,40,80,120),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.CNB.C_6e[[i]]/2,2,median))
  rect(0,20.1,120,23,col="white",border=NA) }
mtext(c("Time","[Days]"),
      1,c(1.5,3.1),cex=c(1.5,1.2),outer=T)
mtext(c("[Frames of bees]","Bees"),
      2,c(1.5,3.1),cex=c(1.2,1.5),outer=T,las=0)
mtext(c("Spores 850","Spores 5e+4","Spores 2e+5"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Brood 80","Brood 130","Brood 200"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: Brood*(N_20+N_50+n_80)*(C_md+C_3e+C_6e)

summary(data$Brood_orig)## will use 1stQ, mean, and 3rdQ (approximately)
summary(data$CFU_orig)
N_20<-approx(data$NrDay_orig,data$NrDay,20)$y
N_50<-approx(data$NrDay_orig,data$NrDay,50)$y
N_80<-approx(data$NrDay_orig,data$NrDay,80)$y
C_md<-approx(data$CFU_orig,data$CFU,850)$y#
C_3e<-approx(data$CFU_orig,data$CFU,50000)$y
C_6e<-approx(data$CFU_orig,data$CFU,200000)$y
C_all<-c(C_md,C_3e,C_6e)
names(pos.FOB2b.CNB)
#posterior day 20
pred.CNB.N_20<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(C_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_20[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*C_all[j]+
            pos.FOB2b.CNB$b_NrDay*N_20+
            pos.FOB2b.CNB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*C_all[j]*N_20+
            pos.FOB2b.CNB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*N_20*Brood.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*C_all[j]*N_20*Brood.seq[i])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*C_all[j]+
            pos.FOB2b.CN.NB$b_NrDay*N_20+
            pos.FOB2b.CN.NB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*C_all[j]*N_20+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*N_20*Brood.seq[i])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*C_all[j]+
            pos.FOB2b.CB$b_NrDay*N_20+
            pos.FOB2b.CB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*C_all[j]+
            pos.FOB2b.CN.CB$b_NrDay*N_20+
            pos.FOB2b.CN.CB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*C_all[j]*N_20+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])}}
#posterior day 50
pred.CNB.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(C_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_50[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*C_all[j]+
            pos.FOB2b.CNB$b_NrDay*N_50+
            pos.FOB2b.CNB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*C_all[j]*N_50+
            pos.FOB2b.CNB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*N_50*Brood.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*C_all[j]*N_50*Brood.seq[i])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*C_all[j]+
            pos.FOB2b.CN.NB$b_NrDay*N_50+
            pos.FOB2b.CN.NB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*C_all[j]*N_50+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*N_50*Brood.seq[i])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*C_all[j]+
            pos.FOB2b.CB$b_NrDay*N_50+
            pos.FOB2b.CB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*C_all[j]+
            pos.FOB2b.CN.CB$b_NrDay*N_50+
            pos.FOB2b.CN.CB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*C_all[j]*N_50+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])}}
#posterior day 80
pred.CNB.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(C_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_80[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.FOB2b.CNB$Intercept+
            pos.FOB2b.CNB$b_CFU*C_all[j]+
            pos.FOB2b.CNB$b_NrDay*N_80+
            pos.FOB2b.CNB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_NrDay*C_all[j]*N_80+
            pos.FOB2b.CNB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.FOB2b.CNB$b_NrDay_X_Brood*N_80*Brood.seq[i]+
            pos.FOB2b.CNB$b_CFU_X_NrDay_X_Brood*C_all[j]*N_80*Brood.seq[i])+
      comp@output$weight[2]*
      exp(pos.FOB2b.CN.NB$Intercept+
            pos.FOB2b.CN.NB$b_CFU*C_all[j]+
            pos.FOB2b.CN.NB$b_NrDay*N_80+
            pos.FOB2b.CN.NB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CN.NB$b_CFU_X_NrDay*C_all[j]*N_80+
            pos.FOB2b.CN.NB$b_NrDay_X_Brood*N_80*Brood.seq[i])+
      comp@output$weight[3]*
      exp(pos.FOB2b.CB$Intercept+
            pos.FOB2b.CB$b_CFU*C_all[j]+
            pos.FOB2b.CB$b_NrDay*N_80+
            pos.FOB2b.CB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])+
      comp@output$weight[4]*
      exp(pos.FOB2b.CN.CB$Intercept+
            pos.FOB2b.CN.CB$b_CFU*C_all[j]+
            pos.FOB2b.CN.CB$b_NrDay*N_80+
            pos.FOB2b.CN.CB$b_Brood*Brood.seq[i]+
            pos.FOB2b.CN.CB$b_CFU_X_NrDay*C_all[j]*N_80+
            pos.FOB2b.CN.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])}}

##########
##########
# Figure s8
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.S8_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,5,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,350),ylim=c(0,20),bty="n",xaxt=XAXT[i])
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.97)/2,Brood.seq.un,col="yellow")
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.89)/2,Brood.seq.un)
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.67)/2,Brood.seq.un)
  abline(h=c(0,5,10,15),v=c(0,100,200,300),col="grey85",xpd=F)
  lines(Brood.seq.un,apply(pred.CNB.N_20[[i]]/2,2,median))
  rect(-3,19.1,350,24,col="white",border=NA)  }
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,350),ylim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.97)/2,Brood.seq.un,col="yellow")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.89)/2,Brood.seq.un)
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.67)/2,Brood.seq.un)
  abline(h=c(0,5,10,15),v=c(0,100,200,300),col="grey85",xpd=F)
  lines(Brood.seq.un,apply(pred.CNB.N_50[[i]]/2,2,median))
  rect(-3,19.1,350,24,col="white",border=NA)  }
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,350),ylim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.97)/2,Brood.seq.un,col="yellow")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.89)/2,Brood.seq.un)
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.67)/2,Brood.seq.un)
  abline(h=c(0,5,10,15),v=c(0,100,200,300),col="grey85",xpd=F)
  lines(Brood.seq.un,apply(pred.CNB.N_80[[i]]/2,2,median))
  rect(-3,19.1,350,24,col="white",border=NA)  }
mtext(c("Brood","[Grids occupied]"),
      1,c(1.5,3.1),cex=c(1.5,1.2),outer=T)
mtext(c("[Frames of bees]","Bees"),
      2,c(1.5,3.1),cex=c(1.2,1.5),outer=T,las=0)
mtext(c("Spores 850","Spores 5e+4","Spores 2e+5"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
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
names(pos.AFBb.CNB)
#Intercept--------------------------------------------------
Intercept.post<-
  exp(pos.FOB2b.CNB$Intercept)  *comp@output$weight[1]+
  exp(pos.FOB2b.CN.NB$Intercept)*comp@output$weight[2]+
  exp(pos.FOB2b.CB$Intercept)   *comp@output$weight[3]+
  exp(pos.FOB2b.CN.CB$Intercept)*comp@output$weight[4]
x11()
hist(Intercept.post,breaks=100)
mean(Intercept.post);sd(Intercept.post);HPDI(Intercept.post,prob=0.97)
# [1] 17.27781
# [1] 0.4005812
# |0.97    0.97| 
#   16.41076 18.08099 
#Effect probability
1-ecdf(Intercept.post)(0) #1
#b_CFU--------------------------------------------------
b_CFU.post<-
  exp(pos.FOB2b.CNB$b_CFU)  *comp@output$weight[1]+
  exp(pos.FOB2b.CN.NB$b_CFU)*comp@output$weight[2]+
  exp(pos.FOB2b.CB$b_CFU)   *comp@output$weight[3]+
  exp(pos.FOB2b.CN.CB$b_CFU)*comp@output$weight[4]
x11()
hist(b_CFU.post,breaks=100)
mean(b_CFU.post);sd(b_CFU.post);HPDI(b_CFU.post,prob=0.97)
# [1] 1.042685
# [1] 0.02483521
# |0.97    0.97| 
#   0.990745 1.101167 
#Effect probability
1-ecdf(b_CFU.post)(0) #1
#b_NrDay--------------------------------------------------
b_NrDay.post<-
  exp(pos.FOB2b.CNB$b_NrDay)  *comp@output$weight[1]+
  exp(pos.FOB2b.CN.NB$b_NrDay)*comp@output$weight[2]+
  exp(pos.FOB2b.CB$b_NrDay)   *comp@output$weight[3]+
  exp(pos.FOB2b.CN.CB$b_NrDay)*comp@output$weight[4]
x11()
hist(b_NrDay.post,breaks=100)
mean(b_NrDay.post);sd(b_NrDay.post);HPDI(b_NrDay.post,prob=0.97)
# [1] 1.137424
# [1] 0.01620855
# |0.97    0.97| 
#   1.101371 1.170406 
#Effect probability
1-ecdf(b_NrDay.post)(0) #1
#b_Brood--------------------------------------------------
b_Brood.post<-
  exp(pos.FOB2b.CNB$b_Brood)  *comp@output$weight[1]+
  exp(pos.FOB2b.CN.NB$b_Brood)*comp@output$weight[2]+
  exp(pos.FOB2b.CB$b_Brood)   *comp@output$weight[3]+
  exp(pos.FOB2b.CN.CB$b_Brood)*comp@output$weight[4]
x11()
hist(b_Brood.post,breaks=100)
mean(b_Brood.post);sd(b_Brood.post);HPDI(b_Brood.post,prob=0.97)
# [1] 1.252251
# [1] 0.0192344
# |0.97    0.97| 
#   1.210566 1.293357 
#Effect probability
1-ecdf(b_Brood.post)(0) #1

#--------------------------------------------------
#higher predictivness of predictor
#--------------------------------------------------

CFU.Brood.post<-b_Brood.post-b_CFU.post
x11()
hist(CFU.Brood.post,breaks=100)
mean(CFU.Brood.post);sd(CFU.Brood.post);HPDI(CFU.Brood.post,prob=0.97)
# [1] 0.2095661
# [1] 0.03373408
# |0.97     0.97| 
#   0.1375429 0.2900131
1-ecdf(CFU.Brood.post)(0) #100 %  #better to evaluate brood

Brood.NrDay.post<-b_Brood.post-b_NrDay.post
x11()
hist(Brood.NrDay.post,breaks=100)
mean(Brood.NrDay.post);sd(Brood.NrDay.post);HPDI(Brood.NrDay.post,prob=0.97)
# [1] 0.1148267
# [1] 0.02166307
# |0.97      0.97| 
#   0.06783632 0.15638140 
1-ecdf(Brood.NrDay.post)(0) #100 %  better to evaluate brood

CFU.NrDay.post<-b_NrDay.post-b_CFU.post
x11()
hist(CFU.NrDay.post,breaks=100)
mean(CFU.NrDay.post);sd(CFU.NrDay.post);HPDI(CFU.NrDay.post,prob=0.97)
# [1] 0.09473942
# [1] 0.03244954
# |0.97      0.97| 
#   0.02152531 0.16642560 
1-ecdf(CFU.NrDay.post)(0) #99.7 %  #better to evaluate cfu

