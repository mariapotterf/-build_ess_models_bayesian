library(car);library(MASS);library(lattice);library(sciplot);library(rethinking)
setwd("    ")
data<-read.csv("AFB-CFU-all.csv")
# Re-center data at mean, to reduce autocorrelation in MCMC sampling.
# Divide by SD to make prior specification generic.
#AFB
data$AFB_orig<-data$AFB
data$AFB<-(data$AFB_orig-mean(data$AFB_orig))/sd(data$AFB_orig)
#Brood
data$Brood_orig<-data$Brood
data$Brood<-(data$Brood_orig-mean(data$Brood_orig))/sd(data$Brood_orig)
#FOB
data$FOB_orig<-data$FOB
data$FOB<-(data$FOB_orig-mean(data$FOB_orig))/sd(data$FOB_orig)
#Brood polynomial
# data$Brood_orig2<-data$Brood_orig^2
data$Brood2<-data$Brood^2
head(data)


#checking variance in orig----------------
x11()
dens(data$FOB_orig)
x11()
dens(data$Brood_orig)
#calculate coefficient of variance
sd(data$FOB_orig)/mean(data$FOB_orig)
sd(data$Brood_orig)/mean(data$Brood_orig)

#more summary-------------------------
glimpse(data)
showset<-data[,c("AFB_orig","CFU_orig",
                     "Brood_orig","NrDay_orig","FOB_orig","treat")]
TS1<-psych::describeBy(showset,showset$treat,mat = T) #%>% view()
TS1[,c("mean","sd","se")]<-signif(TS1[,c("mean","sd","se")],2)
TS1<-TS1[,c("group1","n","mean","sd","median","min","max","se" )]
openxlsx::write.xlsx(TS1,file = "TS1.xlsx",rowNames=T)

#
 # source("panel.cor.r")
 # x11(12,12)
 # pairs(~AFB_orig+CFU_orig+FOB_orig+Brood_orig+NrDay_orig,
 #       data=data,upper.panel=panel.smooth,lower.panel=panel.cor)
 # x11(12,12)
 # pairs(~AFB+CFU+FOB+Brood+NrDay,
 #       data=data,upper.panel=panel.smooth,lower.panel=panel.cor)
###############################################################################
################################################################################
#STEP 1: how many random effects?
################################################################################
################################################################################
################################################################################


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 000: #glimmer(AFB_orig~CFU*NrDay*Brood+(1|treat)+(1|CoID)+(1|Obs),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.AFBb.000<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
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
# # #save model
# saveRDS(m.AFBb.000,"m.AFBb.000.rds")
m.AFBb.000<-readRDS("m.AFBb.000.rds")
##gelman
precis(m.AFBb.000)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 00: glimmer(AFB_orig~CFU*NrDay*Brood+(1|CoID)+(1|Obs),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.AFBb.00<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
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
# # #save model
# saveRDS(m.AFBb.00,"m.AFBb.00.rds")
m.AFBb.00<-readRDS("m.AFBb.00.rds")
##gelman
precis(m.AFBb.00)

#comparing models
(m.compare<-compare(m.AFBb.00,m.AFBb.000))
#             WAIC pWAIC dWAIC weight    SE  dSE
# m.AFBb.000 481.6  62.6   0.0   0.78 34.12   NA
# m.AFBb.00  484.2  63.5   2.6   0.22 34.36 2.39

################################################################################
################################################################################
################################################################################
#STEP 2: interactions important?
################################################################################
################################################################################
################################################################################

##### CFU*NrDay*Brood +++++++++++++
#m.AFBb.CNF<-m.AFBb.000
#save model
#saveRDS(m.AFBb.CNF,"m.AFBb.CNF.rds")
m.AFBb.CNB<-readRDS("m.AFBb.CNF.rds")

##### (CFU+NrDay+Brood)^2 +++++++++++++
# m.AFBb.CNF2<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_CFU_X_Brood*CFU*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_CFU_X_Brood,b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
#save model
#saveRDS(m.AFBb.CNF2,"m.AFBb.CNF2.rds")
m.AFBb.CNB2<-readRDS("m.AFBb.CNF2.rds")
precis(m.AFBb.CNB2)

##### CFU+NrDay+Brood+ CFU:NrDay+NrDay:Brood +++++++++++++
# m.AFBb.CN.NB<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# # # #save model
# saveRDS(m.AFBb.CN.NB,"m.AFBb.CN.NB.rds")
m.AFBb.CN.NB<-readRDS("m.AFBb.CN.NB.rds")
precis(m.AFBb.CN.NB)

##### CFU+NrDay+Brood+ CFU:NrDay+CFU:Brood +++++++++++++
# m.AFBb.CN.CB<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       b_CFU_X_Brood*CFU*Brood +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay,b_CFU_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.AFBb.CN.CB,"m.AFBb.CN.CB.rds")
m.AFBb.CN.CB<-readRDS("m.AFBb.CN.CB.rds")
precis(m.AFBb.CN.CB)

##### CFU+NrDay+Brood+ CFU:Brood+NrDay:Brood +++++++++++++
# m.AFBb.CB.NB<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_Brood*CFU*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_Brood,b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.AFBb.CB.NB,"m.AFBb.CB.NB.rds")
m.AFBb.CB.NB<-readRDS("m.AFBb.CB.NB.rds")
precis(m.AFBb.CB.NB)

##### CFU*NrDay+Brood +++++++++++++
# m.AFBb.CN<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_NrDay*CFU*NrDay +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_NrDay) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# # #save model
# saveRDS(m.AFBb.CN,"m.AFBb.CN.rds")
m.AFBb.CN<-readRDS("m.AFBb.CN.rds")

##### CFU+NrDay*Brood +++++++++++++
# m.AFBb.NF<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_NrDay_X_Brood*NrDay*Brood +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_NrDay_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.AFBb.NF,"m.AFBb.NF.rds")
m.AFBb.NB<-readRDS("m.AFBb.NF.rds")
##### CFU*Brood+NrDay +++++++++++++
# m.AFBb.CF<-map2stan(
#   alist(
#     AFB_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_CFU*CFU +
#       b_NrDay*NrDay +
#       b_Brood*Brood +
#       b_CFU_X_Brood*CFU*Brood +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_CFU,b_NrDay,b_Brood,
#       b_CFU_X_Brood) ~ dnorm(0,1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.AFBb.CF,"m.AFBb.CF.rds")
m.AFBb.CB<-readRDS("m.AFBb.CF.rds")

#comparing models
(comp.all<-compare(m.AFBb.CNB,m.AFBb.CNB2,
               m.AFBb.CN.NB,m.AFBb.CN.CB,m.AFBb.CB.NB,
               m.AFBb.CN,m.AFBb.NB,m.AFBb.CB))
#               WAIC pWAIC dWAIC weight    SE  dSE
# m.AFBb.CNB   481.6  62.6   0.0   0.49 34.12   NA
# m.AFBb.CB    483.3  62.4   1.6   0.22 34.22 2.75
# m.AFBb.CB.NB 483.8  63.1   2.1   0.17 34.18 2.52
# m.AFBb.CNB2  485.4  63.3   3.7   0.08 34.42 2.60
# m.AFBb.CN.CB 488.3  64.4   6.7   0.02 34.41 2.71
# m.AFBb.CN    489.0  64.9   7.4   0.01 34.50 2.98
# m.AFBb.CN.NB 489.4  64.9   7.7   0.01 34.60 3.22
# m.AFBb.NB    491.6  65.2  10.0   0.00 34.52 3.18

(comp<-compare(m.AFBb.CNB,m.AFBb.CB,m.AFBb.CB.NB,m.AFBb.CNB2))
#               WAIC pWAIC dWAIC weight    SE  dSE
# m.AFBb.CNB   481.6  62.6   0.0   0.51 34.12   NA
# m.AFBb.CB    483.3  62.4   1.6   0.23 34.22 2.75
# m.AFBb.CB.NB 483.8  63.1   2.1   0.18 34.18 2.52
# m.AFBb.CNB2  485.4  63.3   3.7   0.08 34.42 2.60
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model validation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# names(m.AFBb.CNB)
# ##gelman
# precis(m.AFBb.CNB)#,dept=2)
# #check chains
# x11(8,8)
# plot(m.AFBb.CNB)#,pars=c("Intercept","b_CFU")
# #plot model to see correlations
# x11()
# pairs(m.AFBb.CNB,pars=c("Intercept","b_CFU","b_NrDay","b_Brood"))
# pairs(m.AFBb.CNB,pars=c("b_CFU_X_NrDay","b_CFU_X_Brood","b_NrDay_X_Brood",
#                         "b_CFU_X_NrDay_X_Brood"))
# #predictions original data
# x11()
# postcheck(m.AFBb.CNB)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #posterior
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pos.AFBb.CNB<-extract.samples(m.AFBb.CNB)
pos.AFBb.CB<-extract.samples(m.AFBb.CB)
pos.AFBb.CB.NB<-extract.samples(m.AFBb.CB.NB)
pos.AFBb.CNB2<-extract.samples(m.AFBb.CNB2)

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

#posterior day 20
pred.CNB.N_20<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_20[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*CFU.seq[i]+
            pos.AFBb.CNB$b_NrDay*N_20+
            pos.AFBb.CNB$b_Brood*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay*CFU.seq[i]*N_20+
            pos.AFBb.CNB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CNB$b_NrDay_X_Brood*N_20*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*CFU.seq[i]*N_20*B_all[j])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*CFU.seq[i]+
            pos.AFBb.CB$b_NrDay*N_20+
            pos.AFBb.CB$b_Brood*B_all[j]+
            pos.AFBb.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*CFU.seq[i]+
            pos.AFBb.CB.NB$b_NrDay*N_20+
            pos.AFBb.CB.NB$b_Brood*B_all[j]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*N_20*B_all[j])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*CFU.seq[i]+
            pos.AFBb.CNB2$b_NrDay*N_20+
            pos.AFBb.CNB2$b_Brood*B_all[j]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*CFU.seq[i]*N_20+
            pos.AFBb.CNB2$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*N_20*B_all[j])}}
#posterior day 50
pred.CNB.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_50[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*CFU.seq[i]+
            pos.AFBb.CNB$b_NrDay*N_50+
            pos.AFBb.CNB$b_Brood*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay*CFU.seq[i]*N_50+
            pos.AFBb.CNB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CNB$b_NrDay_X_Brood*N_50*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*CFU.seq[i]*N_50*B_all[j])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*CFU.seq[i]+
            pos.AFBb.CB$b_NrDay*N_50+
            pos.AFBb.CB$b_Brood*B_all[j]+
            pos.AFBb.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*CFU.seq[i]+
            pos.AFBb.CB.NB$b_NrDay*N_50+
            pos.AFBb.CB.NB$b_Brood*B_all[j]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*N_50*B_all[j])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*CFU.seq[i]+
            pos.AFBb.CNB2$b_NrDay*N_50+
            pos.AFBb.CNB2$b_Brood*B_all[j]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*CFU.seq[i]*N_50+
            pos.AFBb.CNB2$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*N_50*B_all[j])}}
#save for graphic of eva
#saveRDS(pred.CNB.N_50,"pred.CNB.N_50.rds")

#posterior day 80
pred.CNB.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_80[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*CFU.seq[i]+
            pos.AFBb.CNB$b_NrDay*N_80+
            pos.AFBb.CNB$b_Brood*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay*CFU.seq[i]*N_80+
            pos.AFBb.CNB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CNB$b_NrDay_X_Brood*N_80*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*CFU.seq[i]*N_80*B_all[j])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*CFU.seq[i]+
            pos.AFBb.CB$b_NrDay*N_80+
            pos.AFBb.CB$b_Brood*B_all[j]+
            pos.AFBb.CB$b_CFU_X_Brood*CFU.seq[i]*B_all[j])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*CFU.seq[i]+
            pos.AFBb.CB.NB$b_NrDay*N_80+
            pos.AFBb.CB.NB$b_Brood*B_all[j]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*N_80*B_all[j])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*CFU.seq[i]+
            pos.AFBb.CNB2$b_NrDay*N_80+
            pos.AFBb.CNB2$b_Brood*B_all[j]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*CFU.seq[i]*N_80+
            pos.AFBb.CNB2$b_CFU_X_Brood*CFU.seq[i]*B_all[j]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*N_80*B_all[j])}}
##########
##########
# Figure 1
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.1_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,5,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,7e+5),ylim=c(0,15),bty="n",xaxt=XAXT[i])
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.97),CFU.seq.un,col="yellow")
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.89),CFU.seq.un)
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.67),CFU.seq.un)
  abline(h=c(0,5,10,15),v=c(0,2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(CFU.seq.un,apply(pred.CNB.N_20[[i]],2,median))
  rect(0,15.1,7e+5,17,col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,7e+5),ylim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.97),CFU.seq.un,col="yellow")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.89),CFU.seq.un)
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.67),CFU.seq.un)
  abline(h=c(0,5,10,15),v=c(0,2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(CFU.seq.un,apply(pred.CNB.N_50[[i]],2,median))
  rect(0,15.1,7e+5,17,col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,7e+5),ylim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.97),CFU.seq.un,col="yellow")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.89),CFU.seq.un)
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.67),CFU.seq.un)
  abline(h=c(0,5,10,15),v=c(0,2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(CFU.seq.un,apply(pred.CNB.N_80[[i]],2,median))
  rect(0,15.1,7e+5,17,col="white",border=NA)}
mtext(c("Spores","[CFU/Bee]"),
      1,c(1.5,3.1),cex=c(1.5,1.2),outer=T)
mtext(c("[AFB score/colony]","Symptoms"),
      2,c(1.5,3.1),cex=c(1.2,1.5),outer=T,las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Brood 80","Brood 130","Brood 200"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: Day*(B_80+B_130+B_200)*(C_md+C_3e+C_6e)
summary(data$Brood_orig)
summary(data$CFU_orig)
B_80<-approx(data$Brood_orig,data$Brood,80)$y
B_130<-approx(data$Brood_orig,data$Brood,130)$y
B_200<-approx(data$Brood_orig,data$Brood,200)$y
B_all<-c(B_80,B_130,B_200)
C_md<-approx(data$CFU_orig,data$CFU,850)$y#
C_3e<-approx(data$CFU_orig,data$CFU,50000)$y
C_6e<-approx(data$CFU_orig,data$CFU,200000)$y

#posterior C_md
pred.CNB.C_md<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.C_md[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*C_md+
            pos.AFBb.CNB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CNB$b_Brood*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay*C_md*NrDay.seq[i]+
            pos.AFBb.CNB$b_CFU_X_Brood*C_md*B_all[j]+
            pos.AFBb.CNB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_md*NrDay.seq[i]*B_all[j])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*C_md+
            pos.AFBb.CB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CB$b_Brood*B_all[j]+
            pos.AFBb.CB$b_CFU_X_Brood*C_md*B_all[j])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*C_md+
            pos.AFBb.CB.NB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CB.NB$b_Brood*B_all[j]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*C_md*B_all[j]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*C_md+
            pos.AFBb.CNB2$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CNB2$b_Brood*B_all[j]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*C_md*NrDay.seq[i]+
            pos.AFBb.CNB2$b_CFU_X_Brood*C_md*B_all[j]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])}}
#posterior C_3e
pred.CNB.C_3e<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.C_3e[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*C_3e+
            pos.AFBb.CNB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CNB$b_Brood*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay*C_3e*NrDay.seq[i]+
            pos.AFBb.CNB$b_CFU_X_Brood*C_3e*B_all[j]+
            pos.AFBb.CNB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_3e*NrDay.seq[i]*B_all[j])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*C_3e+
            pos.AFBb.CB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CB$b_Brood*B_all[j]+
            pos.AFBb.CB$b_CFU_X_Brood*C_3e*B_all[j])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*C_3e+
            pos.AFBb.CB.NB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CB.NB$b_Brood*B_all[j]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*C_3e*B_all[j]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*C_3e+
            pos.AFBb.CNB2$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CNB2$b_Brood*B_all[j]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*C_3e*NrDay.seq[i]+
            pos.AFBb.CNB2$b_CFU_X_Brood*C_3e*B_all[j]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])}}
#posterior C_6e
pred.CNB.C_6e<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.C_6e[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*C_6e+
            pos.AFBb.CNB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CNB$b_Brood*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay*C_6e*NrDay.seq[i]+
            pos.AFBb.CNB$b_CFU_X_Brood*C_6e*B_all[j]+
            pos.AFBb.CNB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_6e*NrDay.seq[i]*B_all[j])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*C_6e+
            pos.AFBb.CB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CB$b_Brood*B_all[j]+
            pos.AFBb.CB$b_CFU_X_Brood*C_6e*B_all[j])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*C_6e+
            pos.AFBb.CB.NB$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CB.NB$b_Brood*B_all[j]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*C_6e*B_all[j]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*C_6e+
            pos.AFBb.CNB2$b_NrDay*NrDay.seq[i]+
            pos.AFBb.CNB2$b_Brood*B_all[j]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*C_6e*NrDay.seq[i]+
            pos.AFBb.CNB2$b_CFU_X_Brood*C_6e*B_all[j]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*NrDay.seq[i]*B_all[j])}}
##########
##########
# Figure S2
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.S2_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,5,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# C_md
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,120),ylim=c(0,15),bty="n",xaxt=XAXT[i])
  shade(apply(pred.CNB.C_md[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.CNB.C_md[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.CNB.C_md[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(h=c(0,5,10,15),v=c(0,30,60,90),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.CNB.C_md[[i]],2,median))
  rect(-3,15.1,120,17,col="white",border=NA)}
# C_3e
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,120),ylim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.C_3e[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.CNB.C_3e[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.CNB.C_3e[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(h=c(0,5,10,15),v=c(0,30,60,90),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.CNB.C_3e[[i]],2,median))
  rect(-3,15.1,120,17,col="white",border=NA)}
# C_6e
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,120),ylim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.C_6e[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.CNB.C_6e[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.CNB.C_6e[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(h=c(0,5,10,15),v=c(0,30,60,90),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.CNB.C_6e[[i]],2,median))
  rect(-3,15.1,120,17,col="white",border=NA)}
mtext(c("Time","[Days]"),
      1,c(1.5,3.1),cex=c(1.5,1.2),outer=T)
mtext(c("[AFB score/colony]","Symptoms"),
      2,c(1.5,3.1),cex=c(1.2,1.5),outer=T,las=0)
mtext(c("Spores 850","Spores 5e+4","Spores 2e+5"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Brood 80","Brood 130","Brood 200"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: CFU*(C_md+C_3e+C_6e)*(N_20+N_50+N_80)

summary(data$CFU_orig)
summary(data$NrDay_orig)
C_md<-approx(data$CFU_orig,data$CFU,850)$y#
C_3e<-approx(data$CFU_orig,data$CFU,50000)$y
C_6e<-approx(data$CFU_orig,data$CFU,200000)$y
C_all<-c(C_md,C_3e,C_6e)
N_20<-approx(data$NrDay_orig,data$NrDay,20)$y
N_50<-approx(data$NrDay_orig,data$NrDay,50)$y
N_80<-approx(data$NrDay_orig,data$NrDay,80)$y

#posterior day 20
pred.CNB.N_20<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_20[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*C_all[j]+
            pos.AFBb.CNB$b_NrDay*N_20+
            pos.AFBb.CNB$b_Brood*Brood.seq[i]+
            pos.AFBb.CNB$b_CFU_X_NrDay*C_all[j]*N_20+
            pos.AFBb.CNB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CNB$b_NrDay_X_Brood*N_20*Brood.seq[i]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_all[j]*N_20*Brood.seq[i])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*C_all[j]+
            pos.AFBb.CB$b_NrDay*N_20+
            pos.AFBb.CB$b_Brood*Brood.seq[i]+
            pos.AFBb.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*C_all[j]+
            pos.AFBb.CB.NB$b_NrDay*N_20+
            pos.AFBb.CB.NB$b_Brood*Brood.seq[i]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*N_20*Brood.seq[i])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*C_all[j]+
            pos.AFBb.CNB2$b_NrDay*N_20+
            pos.AFBb.CNB2$b_Brood*Brood.seq[i]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*C_all[j]*N_20+
            pos.AFBb.CNB2$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*N_20*Brood.seq[i])}}
#posterior day 50
pred.CNB.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_50[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*C_all[j]+
            pos.AFBb.CNB$b_NrDay*N_50+
            pos.AFBb.CNB$b_Brood*Brood.seq[i]+
            pos.AFBb.CNB$b_CFU_X_NrDay*C_all[j]*N_50+
            pos.AFBb.CNB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CNB$b_NrDay_X_Brood*N_50*Brood.seq[i]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_all[j]*N_50*Brood.seq[i])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*C_all[j]+
            pos.AFBb.CB$b_NrDay*N_50+
            pos.AFBb.CB$b_Brood*Brood.seq[i]+
            pos.AFBb.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*C_all[j]+
            pos.AFBb.CB.NB$b_NrDay*N_50+
            pos.AFBb.CB.NB$b_Brood*Brood.seq[i]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*N_50*Brood.seq[i])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*C_all[j]+
            pos.AFBb.CNB2$b_NrDay*N_50+
            pos.AFBb.CNB2$b_Brood*Brood.seq[i]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*C_all[j]*N_50+
            pos.AFBb.CNB2$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*N_50*Brood.seq[i])}}
#posterior day 80
pred.CNB.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(B_all)){ 
  for (i in 1:nrow(data)){ 
    pred.CNB.N_80[[j]][,i]<-
      comp@output$weight[1]*
      exp(pos.AFBb.CNB$Intercept+
            pos.AFBb.CNB$b_CFU*C_all[j]+
            pos.AFBb.CNB$b_NrDay*N_80+
            pos.AFBb.CNB$b_Brood*Brood.seq[i]+
            pos.AFBb.CNB$b_CFU_X_NrDay*C_all[j]*N_80+
            pos.AFBb.CNB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CNB$b_NrDay_X_Brood*N_80*Brood.seq[i]+
            pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_all[j]*N_80*Brood.seq[i])+
      comp@output$weight[2]*
      exp(pos.AFBb.CB$Intercept+
            pos.AFBb.CB$b_CFU*C_all[j]+
            pos.AFBb.CB$b_NrDay*N_80+
            pos.AFBb.CB$b_Brood*Brood.seq[i]+
            pos.AFBb.CB$b_CFU_X_Brood*C_all[j]*Brood.seq[i])+
      comp@output$weight[3]*
      exp(pos.AFBb.CB.NB$Intercept+
            pos.AFBb.CB.NB$b_CFU*C_all[j]+
            pos.AFBb.CB.NB$b_NrDay*N_80+
            pos.AFBb.CB.NB$b_Brood*Brood.seq[i]+
            pos.AFBb.CB.NB$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CB.NB$b_NrDay_X_Brood*N_80*Brood.seq[i])+
      comp@output$weight[4]*
      exp(pos.AFBb.CNB2$Intercept+
            pos.AFBb.CNB2$b_CFU*C_all[j]+
            pos.AFBb.CNB2$b_NrDay*N_80+
            pos.AFBb.CNB2$b_Brood*Brood.seq[i]+
            pos.AFBb.CNB2$b_CFU_X_NrDay*C_all[j]*N_80+
            pos.AFBb.CNB2$b_CFU_X_Brood*C_all[j]*Brood.seq[i]+
            pos.AFBb.CNB2$b_NrDay_X_Brood*N_80*Brood.seq[i])}}
##########
##########
# Figure S3
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.S3_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,5,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,350),ylim=c(0,15),bty="n",xaxt=XAXT[i])
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.97),Brood.seq.un,col="yellow")
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.89),Brood.seq.un)
  shade(apply(pred.CNB.N_20[[i]],2,PI,prob=0.67),Brood.seq.un)
  abline(h=c(0,5,10,15),v=c(0,100,200,300),col="grey85",xpd=F)
  lines(Brood.seq.un,apply(pred.CNB.N_20[[i]],2,median))
  rect(-3,15.1,350,17,col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,350),ylim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.97),Brood.seq.un,col="yellow")
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.89),Brood.seq.un)
  shade(apply(pred.CNB.N_50[[i]],2,PI,prob=0.67),Brood.seq.un)
  abline(h=c(0,5,10,15),v=c(0,100,200,300),col="grey85",xpd=F)
  lines(Brood.seq.un,apply(pred.CNB.N_50[[i]],2,median))
  rect(-3,15.1,350,17,col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",xlim=c(0,350),ylim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.97),Brood.seq.un,col="yellow")
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.89),Brood.seq.un)
  shade(apply(pred.CNB.N_80[[i]],2,PI,prob=0.67),Brood.seq.un)
  abline(h=c(0,5,10,15),v=c(0,100,200,300),col="grey85",xpd=F)
  lines(Brood.seq.un,apply(pred.CNB.N_80[[i]],2,median))
  rect(-3,15.1,350,17,col="white",border=NA)}
mtext(c("Brood","[Grids occupied]"),
      1,c(1.5,3.1),cex=c(1.5,1.2),outer=T)
mtext(c("[AFB score/colony]","Symptoms"),
      2,c(1.5,3.1),cex=c(1.2,1.5),outer=T,las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Spores 850","Spores 5e+4","Spores 2e+5"),
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
names(pos.AFBb.CNB)
#Intercept--------------------------------------------------
Intercept.post<-
  exp(pos.AFBb.CNB$Intercept)  *comp@output$weight[1]+
  exp(pos.AFBb.CB$Intercept)   *comp@output$weight[2]+
  exp(pos.AFBb.CB.NB$Intercept)*comp@output$weight[3]+
  exp(pos.AFBb.CNB2$Intercept) *comp@output$weight[4]
#save intercept for eva graph
saveRDS(Intercept.post,"Intercept.post.rds")
x11()
hist(Intercept.post,breaks=100)
mean(Intercept.post);sd(Intercept.post);HPDI(Intercept.post,prob=0.97)
# [1] 0.2960992
# [1] 0.06556747
# |0.97     0.97| 
#   0.1638529 0.43654

#also get slope to use it as prior (ignoring interactions)
Slope.post<-
  exp(pos.AFBb.CNB$b_CFU)  *comp@output$weight[1]+
  exp(pos.AFBb.CB$b_CFU)   *comp@output$weight[2]+
  exp(pos.AFBb.CB.NB$b_CFU)*comp@output$weight[3]+
  exp(pos.AFBb.CNB2$b_CFU) *comp@output$weight[4]
mean(Slope.post);sd(Slope.post)
#save slope for eva graph
saveRDS(Slope.post,"Slope.post.rds")

#Effect probability
1-ecdf(Intercept.post)(0) #1
#b_CFU--------------------------------------------------
b_CFU.post<-
  exp(pos.AFBb.CNB$b_CFU)  *comp@output$weight[1]+
  exp(pos.AFBb.CB$b_CFU)   *comp@output$weight[2]+
  exp(pos.AFBb.CB.NB$b_CFU)*comp@output$weight[3]+
  exp(pos.AFBb.CNB2$b_CFU) *comp@output$weight[4]
x11()
hist(b_CFU.post,breaks=100)
mean(b_CFU.post);sd(b_CFU.post);HPDI(b_CFU.post,prob=0.97)
# [1] 2.138699
# [1] 0.3344624
# |0.97    0.97| 
#   1.479641 2.914876 
#Effect probability
1-ecdf(b_CFU.post)(0) #1
#b_NrDay--------------------------------------------------
b_NrDay.post<-
  exp(pos.AFBb.CNB$b_NrDay)  *comp@output$weight[1]+
  exp(pos.AFBb.CB$b_NrDay)   *comp@output$weight[2]+
  exp(pos.AFBb.CB.NB$b_NrDay)*comp@output$weight[3]+
  exp(pos.AFBb.CNB2$b_NrDay) *comp@output$weight[4]
x11()
hist(b_NrDay.post,breaks=100)
mean(b_NrDay.post);sd(b_NrDay.post);HPDI(b_NrDay.post,prob=0.97)
# [1] 1.581991
# [1] 0.1575221
# |0.97    0.97| 
#   1.269249 1.914525 
#Effect probability
1-ecdf(b_NrDay.post)(0) #1
#b_Brood--------------------------------------------------
b_Brood.post<-
  exp(pos.AFBb.CNB$b_Brood)  *comp@output$weight[1]+
  exp(pos.AFBb.CB$b_Brood)   *comp@output$weight[2]+
  exp(pos.AFBb.CB.NB$b_Brood)*comp@output$weight[3]+
  exp(pos.AFBb.CNB2$b_Brood) *comp@output$weight[4]
x11()
hist(b_Brood.post,breaks=100)
mean(b_Brood.post);sd(b_Brood.post);HPDI(b_Brood.post,prob=0.97)
# [1] 1.619074
# [1] 0.1925801
# |0.97    0.97| 
#   1.288503 2.109504
#Effect probability
1-ecdf(b_Brood.post)(0) #1
# #b_CFU_X_NrDay--------------------------------------------------
# b_CFU_X_NrDay.post<-
#   exp(pos.AFBb.CNB$b_CFU_X_NrDay)  *comp@output$weight[1]+
#   exp(pos.AFBb.CNB2$b_CFU_X_NrDay) *comp@output$weight[4]
# x11()
# hist(b_CFU_X_NrDay.post,breaks=100)
# #Effect probability
# 1-ecdf(b_CFU_X_NrDay.post)(0) #1


#--------------------------------------------------
#higher predictivness of predictor
#--------------------------------------------------

CFU.Brood.post<-b_CFU.post-b_Brood.post
1-ecdf(CFU.Brood.post)(0) #91.3 %  #better to evaluate cfu
x11()
hist(CFU.Brood.post,breaks=100)
mean(CFU.Brood.post);sd(CFU.Brood.post);HPDI(CFU.Brood.post,prob=0.97)
# [1] 0.5196245
# [1] 0.4018709
# |0.97      0.97| 
#   -0.2674582  1.4592045

Brood.NrDay.post<-b_Brood.post-b_NrDay.post
1-ecdf(Brood.NrDay.post)(0) #55.6 %  #both are equaly important
x11()
hist(Brood.NrDay.post,breaks=100)
mean(Brood.NrDay.post);sd(Brood.NrDay.post);HPDI(Brood.NrDay.post,prob=0.97)
# [1] 0.03708291
# [1] 0.2286255
# |0.97      0.97| 
#   -0.4600749  0.5241362 

CFU.NrDay.post<-b_CFU.post-b_NrDay.post
1-ecdf(CFU.NrDay.post)(0) #94.3 %  #better to evaluate cfu
x11()
hist(CFU.NrDay.post,breaks=100)
mean(CFU.NrDay.post);sd(CFU.NrDay.post);HPDI(CFU.NrDay.post,prob=0.97)
# [1] 0.5567074
# [1] 0.3915192
# |0.97      0.97| 
#   -0.2498891  1.4958568 


#--------------------------------------------------
#### symptoms at zero spores
#--------------------------------------------------
C_x<-approx(data$CFU_orig,data$CFU,0)$y
pos.AFBb.zero<-
comp@output$weight[1]*
  exp(pos.AFBb.CNB$Intercept+
        pos.AFBb.CNB$b_CFU*C_x+
        pos.AFBb.CNB$b_NrDay*N_50+
        pos.AFBb.CNB$b_Brood*B_130+
        pos.AFBb.CNB$b_CFU_X_NrDay*C_x*N_50+
        pos.AFBb.CNB$b_CFU_X_Brood*C_x*B_130+
        pos.AFBb.CNB$b_NrDay_X_Brood*N_50*B_130+
        pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_x*N_50*B_130)+
  comp@output$weight[2]*
  exp(pos.AFBb.CB$Intercept+
        pos.AFBb.CB$b_CFU*C_x+
        pos.AFBb.CB$b_NrDay*N_50+
        pos.AFBb.CB$b_Brood*B_130+
        pos.AFBb.CB$b_CFU_X_Brood*C_x*B_130)+
  comp@output$weight[3]*
  exp(pos.AFBb.CB.NB$Intercept+
        pos.AFBb.CB.NB$b_CFU*C_x+
        pos.AFBb.CB.NB$b_NrDay*N_50+
        pos.AFBb.CB.NB$b_Brood*B_130+
        pos.AFBb.CB.NB$b_CFU_X_Brood*C_x*B_130+
        pos.AFBb.CB.NB$b_NrDay_X_Brood*N_50*B_130)+
  comp@output$weight[4]*
  exp(pos.AFBb.CNB2$Intercept+
        pos.AFBb.CNB2$b_CFU*C_x+
        pos.AFBb.CNB2$b_NrDay*N_50+
        pos.AFBb.CNB2$b_Brood*B_130+
        pos.AFBb.CNB2$b_CFU_X_NrDay*C_x*N_50+
        pos.AFBb.CNB2$b_CFU_X_Brood*C_x*B_130+
        pos.AFBb.CNB2$b_NrDay_X_Brood*N_50*B_130)
x11()
hist(pos.AFBb.zero,breaks=100,freq=F)
mean(pos.AFBb.zero);sd(pos.AFBb.zero);HPDI(pos.AFBb.zero,prob=0.97)
# [1] 0.2273951
# [1] 0.05281229
# |0.97     0.97| 
#   0.1280087 0.3424385 
1-ecdf(pos.CFUb.zero)(0) #1

##### when afb 1 included
C_x<-approx(data$CFU_orig,data$CFU,60000)$y
pos.AFBb.one<-
  comp@output$weight[1]*
  exp(pos.AFBb.CNB$Intercept+
        pos.AFBb.CNB$b_CFU*C_x+
        pos.AFBb.CNB$b_NrDay*N_50+
        pos.AFBb.CNB$b_Brood*B_130+
        pos.AFBb.CNB$b_CFU_X_NrDay*C_x*N_50+
        pos.AFBb.CNB$b_CFU_X_Brood*C_x*B_130+
        pos.AFBb.CNB$b_NrDay_X_Brood*N_50*B_130+
        pos.AFBb.CNB$b_CFU_X_NrDay_X_Brood*C_x*N_50*B_130)+
  comp@output$weight[2]*
  exp(pos.AFBb.CB$Intercept+
        pos.AFBb.CB$b_CFU*C_x+
        pos.AFBb.CB$b_NrDay*N_50+
        pos.AFBb.CB$b_Brood*B_130+
        pos.AFBb.CB$b_CFU_X_Brood*C_x*B_130)+
  comp@output$weight[3]*
  exp(pos.AFBb.CB.NB$Intercept+
        pos.AFBb.CB.NB$b_CFU*C_x+
        pos.AFBb.CB.NB$b_NrDay*N_50+
        pos.AFBb.CB.NB$b_Brood*B_130+
        pos.AFBb.CB.NB$b_CFU_X_Brood*C_x*B_130+
        pos.AFBb.CB.NB$b_NrDay_X_Brood*N_50*B_130)+
  comp@output$weight[4]*
  exp(pos.AFBb.CNB2$Intercept+
        pos.AFBb.CNB2$b_CFU*C_x+
        pos.AFBb.CNB2$b_NrDay*N_50+
        pos.AFBb.CNB2$b_Brood*B_130+
        pos.AFBb.CNB2$b_CFU_X_NrDay*C_x*N_50+
        pos.AFBb.CNB2$b_CFU_X_Brood*C_x*B_130+
        pos.AFBb.CNB2$b_NrDay_X_Brood*N_50*B_130)
x11()
hist(pos.AFBb.one,breaks=100,freq=F)
mean(pos.AFBb.one);sd(pos.AFBb.one);HPDI(pos.AFBb.one,prob=1)
# at 60000 spores afb score is 1



