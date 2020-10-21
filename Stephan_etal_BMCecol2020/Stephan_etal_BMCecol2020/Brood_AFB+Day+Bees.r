library(car);library(MASS);library(lattice);library(sciplot);library(rethinking)
setwd("C:/MyTemp/myGitLab/bayesianESS/build_ess_models_bayesian/Stephan_etal_BMCecol2020/Stephan_etal_BMCecol2020")
data<-read.csv("AFB-CFU-all.csv")
#AFB
data$AFB_orig<-data$AFB
data$AFB<-(data$AFB_orig-mean(data$AFB_orig))/sd(data$AFB_orig)
#FOB
data$FOB_orig<-data$FOB
data$FOB<-(data$FOB_orig-mean(data$FOB_orig))/sd(data$FOB_orig)
#Brood
data$Brood_orig<-data$Brood
data$Brood<-(data$Brood_orig-mean(data$Brood_orig))/sd(data$Brood_orig)
head(data)

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

# m.Brood.000<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
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
#       b_AFB_X_NrDay_X_FOB) ~ dnorm(0,.1),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
# control = list(max_treedepth = 15,adapt_delta = 0.999))
# # #save model
#saveRDS(m.Brood.000,"m.Brood.000.rds")
m.Brood.000<-readRDS("m.Brood.000.rds")
##gelman
precis(m.Brood.000)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model 00: glimmer(Brood_orig~AFB*NrDay*FOB+(1|CoID)+(1|Obs),data,poisson)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# m.Brood.00<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
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
#       b_AFB_X_NrDay_X_FOB) ~ dnorm(0,.1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# # #save model
# saveRDS(m.CFUb.00,"m.CFUb.00.rds")
m.CFUb.00<-readRDS("m.CFUb.00.rds")
##gelman
precis(m.Brood.00)

#comparing models
(m.compare<-compare(m.Brood.00,m.Brood.000))
#               WAIC pWAIC dWAIC weight    SE  dSE
# m.Brood.000 1948.4 141.2   0.0   0.64 16.98   NA
# m.Brood.00  1949.6 141.7   1.1   0.36 17.10 3.68

################################################################################
################################################################################
################################################################################
#STEP 2: interactions important?
################################################################################
################################################################################
################################################################################

##### AFB*NrDay*FOB +++++++++++++
#m.Brood.ANF<-m.Brood.000
#save model
#saveRDS(m.Brood.ANF,"m.Brood.ANF.rds")
m.Brood.ANF<-readRDS("m.Brood.ANF.rds")
#precis(m.Brood.ANF)
##### AFB+NrDay+FOB+  AFB:NrDay+NrDay:FOB+AFB:FOB  ++++++++++++++
# m.Brood.ANF2<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB,b_NrDay_X_FOB) ~ dnorm(0,.1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.Brood.ANF2,"m.Brood.ANF2.rds")
m.Brood.ANF2<-readRDS("m.Brood.ANF2.rds")
#precis(m.Brood.ANF2)

##### AFB+NrDay+FOB+  AFB:NrDay+NrDay:FOB  ++++++++++++++
# m.Brood.AN.NF<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_NrDay_X_FOB) ~ dnorm(0,.1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.Brood.AN.NF,"m.Brood.AN.NF.rds")
m.Brood.AN.NF<-readRDS("m.Brood.AN.NF.rds")
#precis(m.Brood.AN.NF)

##### AFB+NrDay+FOB+  AFB:NrDay+AFB:FOB  ++++++++++++++
# m.Brood.AN.AF<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       b_AFB_X_FOB*AFB*FOB +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay,b_AFB_X_FOB) ~ dnorm(0,.1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.Brood.AN.AF,"m.Brood.AN.AF.rds")
m.Brood.AN.AF<-readRDS("m.Brood.AN.AF.rds")
#precis(m.Brood.AN.AF)

##### AFB+NrDay+FOB+  NrDay:FOB+AFB:FOB  ++++++++++++++
# m.Brood.NF.AF<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_FOB*AFB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_FOB,b_NrDay_X_FOB) ~ dnorm(0,.1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.Brood.NF.AF,"m.Brood.NF.AF.rds")
m.Brood.NF.AF<-readRDS("m.Brood.NF.AF.rds")
#precis(m.Brood.NF.AF)

##### AFB*NrDay+FOB +++++++++++++
# m.Brood.AN<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_NrDay*AFB*NrDay +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_NrDay) ~ dnorm(0,.1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.Brood.AN,"m.Brood.AN.rds")
m.Brood.AN<-readRDS("m.Brood.AN.rds")
#precis(m.Brood.AN)

##### AFB+NrDay*FOB +++++++++++++
# m.Brood.NF<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_NrDay_X_FOB*NrDay*FOB +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_NrDay_X_FOB) ~ dnorm(0,.1),
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
# saveRDS(m.Brood.NF,"m.Brood.NF.rds")
m.Brood.NF<-readRDS("m.Brood.NF.rds")
#precis(m.Brood.NF)

##### AFB*FOB+NrDay +++++++++++++
# m.Brood.AF<-map2stan(
#   alist(
#     Brood_orig ~ dpois( lambda ),
#     log(lambda) <- Intercept +
#       b_AFB*AFB +
#       b_NrDay*NrDay +
#       b_FOB*FOB +
#       b_AFB_X_FOB*AFB*FOB +
#       v_CoID_Intercept[CoID] +
#       v_treat_Intercept[treat] +
#       v_Obs_Intercept[Obs],
#     c(Intercept,b_AFB,b_NrDay,b_FOB,
#       b_AFB_X_FOB) ~ dnorm(0,10),                       ##larger sigma here
#     v_CoID_Intercept[CoID] ~ dnorm(0,sigma_CoID),
#     v_treat_Intercept[treat] ~ dnorm(0,sigma_treat),
#     v_Obs_Intercept[Obs] ~ dnorm(0,sigma_Obs),
#     c(sigma_treat,sigma_CoID,sigma_Obs) ~ dexp(1)),data=data,
#   iter=4000,warmup=3000,                                ## longer warm up here
#   control = list(max_treedepth = 15,adapt_delta = 0.999))
# #save model
#saveRDS(m.Brood.AF,"m.Brood.AF.rds")
m.Brood.AF<-readRDS("m.Brood.AF.rds")
#precis(m.Brood.AF)

#comparing models
(comp.all<-compare(m.Brood.ANF,m.Brood.ANF2,
               m.Brood.AN.NF,m.Brood.NF.AF,m.Brood.AN.AF,
               m.Brood.AN,m.Brood.NF,m.Brood.AF))
#                 WAIC pWAIC dWAIC weight    SE  dSE
# m.Brood.AF    1950.5 141.6   0.0      1 15.27   NA
# m.Brood.AN    1962.8 144.8  12.3      0 12.60 5.96
# m.Brood.AN.AF 1965.3 146.2  14.8      0 12.34 6.03
# m.Brood.AN.NF 1966.0 146.6  15.4      0 13.00 5.30
# m.Brood.ANF2  1966.3 146.2  15.7      0 13.39 5.39
# m.Brood.ANF   1967.7 147.2  17.2      0 14.02 5.30
# m.Brood.NF.AF 1970.8 148.2  20.2      0 13.51 5.33
# m.Brood.NF    1971.5 148.8  21.0      0 13.00 5.95
(comp<-compare(m.Brood.AF,m.Brood.AN,m.Brood.AN.AF,m.Brood.AN.NF))
# m.Brood.AF    1950.5 141.6   0.0      1 15.27   NA
# m.Brood.AN    1962.8 144.8  12.3      0 12.60 5.96
# m.Brood.AN.AF 1965.3 146.2  14.8      0 12.34 6.03
# m.Brood.AN.NF 1966.0 146.6  15.4      0 13.00 5.30
comp@output$weight
#0.9967913207 0.0021455008 0.0006121837 0.0004509948

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# model validation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
models<-c(m.Brood.ANF,m.Brood.ANF2,
          m.Brood.AN.NF,m.Brood.NF.AF,m.Brood.AN.AF,
          m.Brood.AN,m.Brood.NF,m.Brood.AF)
mod.names<-c("m.Brood.ANF","m.Brood.ANF2",
             "m.Brood.AN.NF","m.Brood.NF.AF","m.Brood.AN.AF",
             "m.Brood.AN","m.Brood.NF","m.Brood.AF")
##gelman
precis(m.Brood.AF)
#all models
for (i in 1:length(mod.names)){
  print(mod.names[i])
  print(precis(models[[i]],depth=1))}
#check chains

para<-rownames(precis(models[[1]],dept=2)@output)
para.h1<-cbind(seq(1:length(para)),sort(rep(1:20,16)))[1:length(para),]
para.d<-data.frame(x=para.h1[,1],y=para.h1[,2])


for (i in 1:max(para.d$y)){
x11(10,6)
traceplot(eval(parse(text=mod.names[unique(para.d[para.d$y==i,]$y)]))@stanfit,
  pars=c(c(rownames(precis(models[[unique(para.d[para.d$y==i,]$y)]],dept=2)@output)[para.d[para.d$y==i,]$x])))
}

para.d[para.d$y==i,]$y
i=10
str(m.Brood.AF)


##predictions original data
x11()
postcheck(m.Brood.AF)
#plot model to see correlations
x11()
pairs(m.Brood.AF,pars=c("Intercept","b_AFB","b_NrDay"))
x11()
pairs(m.Brood.AF,pars=c("b_FOB","b_AFB_X_FOB"))

#all models
for (i in 1:length(mod.names)){
x11(5,5)
pairs(models[[i]],main=mod.names[i],
      pars=c("Intercept","b_AFB","b_NrDay"))}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#posterior
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pos.Brood.AF<-extract.samples(m.Brood.AF)
# pos.Brood.NF<-extract.samples(m.Brood.ANF)
# pos.Brood.ANF2<-extract.samples(m.Brood.ANF2)
# pos.Brood.NF.AF<-extract.samples(m.Brood.NF.AF)

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
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*AFB.seq[i]+
            pos.Brood.AF$b_NrDay*N_20+
            pos.Brood.AF$b_FOB*F_all[j]+
            pos.Brood.AF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}
#posterior day 50
pred.ANF.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_50[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*AFB.seq[i]+
            pos.Brood.AF$b_NrDay*N_50+
            pos.Brood.AF$b_FOB*F_all[j]+
            pos.Brood.AF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}
#posterior day 80
pred.ANF.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_80[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*AFB.seq[i]+
            pos.Brood.AF$b_NrDay*N_80+
            pos.Brood.AF$b_FOB*F_all[j]+
            pos.Brood.AF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}

##########
##########
# Figure 4
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.4_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,15),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_20[[i]],2,median))
  rect(-.1,350,17,400,col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_50[[i]],2,median))
  rect(-.1,350,17,400,col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_80[[i]],2,median))
  rect(-.1,350,17,400,col="white",border=NA)}
mtext(c("[AFB score/colony]","Symptoms"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Brood","[Grids occupied]"),
      2,c(4.1,2.5),cex=c(1.5,1.2),outer=T,las=0)
mtext(c("Time 20","Time 50","Time 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Bees 6","Bees 9","Bees 12"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: Day*(F_6+F_9+F_12)*(A_1.5+A_5+A_10)

summary(data$FOB_orig)## will use 1stQ, mean, and 3rdQ (approximately)
summary(data$NrDay_orig)
F_6<-approx(data$FOB_orig,data$FOB,6)$y
F_9<-approx(data$FOB_orig,data$FOB,9)$y
F_12<-approx(data$FOB_orig,data$FOB,12)$y
F_all<-c(F_6,F_9,F_12)
A_me<-approx(data$AFB_orig,data$AFB,1.5)$y#
A_5<-approx(data$AFB_orig,data$AFB,5)$y
A_10<-approx(data$AFB_orig,data$AFB,10)$y

#posterior A 1.5
pred.ANF.A_me<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.A_me[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*A_me+
            pos.Brood.AF$b_NrDay*NrDay.seq[i]+
            pos.Brood.AF$b_FOB*F_all[j]+
            pos.Brood.AF$b_AFB_X_FOB*A_me*F_all[j])}}
#posterior day 50
pred.ANF.A_5<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.A_5[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*A_5+
            pos.Brood.AF$b_NrDay*NrDay.seq[i]+
            pos.Brood.AF$b_FOB*F_all[j]+
            pos.Brood.AF$b_AFB_X_FOB*A_5*F_all[j])}}
#posterior day 80
pred.ANF.A_10<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.A_10[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*A_10+
            pos.Brood.AF$b_NrDay*NrDay.seq[i]+
            pos.Brood.AF$b_FOB*F_all[j]+
            pos.Brood.AF$b_AFB_X_FOB*A_10*F_all[j])}}

##########
##########
# Figure S9
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.S9_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# A_me
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,120),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.A_me[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.ANF.A_me[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.ANF.A_me[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(v=c(0,40,80,120),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.ANF.A_me[[i]],2,median))
  rect(-.1,350,120,400,col="white",border=NA)}
# A_5
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,120),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.A_5[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.ANF.A_5[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.ANF.A_5[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(v=c(0,40,80,120),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.ANF.A_5[[i]],2,median))
  rect(-.1,350,120,400,col="white",border=NA)}
# A_10
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,120),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.A_10[[i]],2,PI,prob=0.97),NrDay.seq.un,col="yellow")
  shade(apply(pred.ANF.A_10[[i]],2,PI,prob=0.89),NrDay.seq.un)
  shade(apply(pred.ANF.A_10[[i]],2,PI,prob=0.67),NrDay.seq.un)
  abline(v=c(0,40,80,120),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(NrDay.seq.un,apply(pred.ANF.A_10[[i]],2,median))
  rect(-.1,350,120,400,col="white",border=NA)}
mtext(c("[Days]","Time"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Brood","[Grids occupied]"),
      2,c(4.1,2.5),cex=c(1.5,1.2),outer=T,las=0)
mtext(c("Symptoms 1.5","Symptoms 5","Symptoms 10"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("Bees 6","Bees 9","Bees 12"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: FOB*(A_1.5+A_5+A_10)*(N_20+N_50+N_80)

summary(data$FOB_orig)## will use 1stQ, mean, and 3rdQ (approximately)
summary(data$NrDay_orig)
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
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_20[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*A_all[j]+
            pos.Brood.AF$b_NrDay*N_20+
            pos.Brood.AF$b_FOB*FOB.seq[i]+
            pos.Brood.AF$b_AFB_X_FOB*A_all[j]*FOB.seq[i])}}
#posterior day 50
pred.ANF.N_50<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_50[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*A_all[j]+
            pos.Brood.AF$b_NrDay*N_50+
            pos.Brood.AF$b_FOB*FOB.seq[i]+
            pos.Brood.AF$b_AFB_X_FOB*A_all[j]*FOB.seq[i])}}
#posterior day 80
pred.ANF.N_80<-list(matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)),
                    matrix(NA,nrow=1000,ncol=nrow(data)))
for (j in 1:length(F_all)){ 
  for (i in 1:nrow(data)){ 
    pred.ANF.N_80[[j]][,i]<-
      exp(pos.Brood.AF$Intercept+
            pos.Brood.AF$b_AFB*A_all[j]+
            pos.Brood.AF$b_NrDay*N_80+
            pos.Brood.AF$b_FOB*FOB.seq[i]+
            pos.Brood.AF$b_AFB_X_FOB*A_all[j]*FOB.seq[i])}}

##########
##########
# Figure S10
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-cfu.Fig.S10_final.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,20),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.97),FOB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.89),FOB.seq.un)
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.67),FOB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(FOB.seq.un,apply(pred.ANF.N_20[[i]],2,median))
  rect(-.1,350,22,400,col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.97),FOB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.89),FOB.seq.un)
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.67),FOB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(FOB.seq.un,apply(pred.ANF.N_50[[i]],2,median))
  rect(-.1,350,22,400,col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,350),xlim=c(0,20),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.97),FOB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.89),FOB.seq.un)
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.67),FOB.seq.un)
  abline(v=c(0,5,10,15),h=c(0,100,200,300),col="grey85",xpd=F)
  lines(FOB.seq.un,apply(pred.ANF.N_80[[i]],2,median))
  rect(-.1,350,22,400,col="white",border=NA)}
mtext(c("[Frames of bees]","Bees"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Brood","[Grids occupied]"),
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
names(pos.Brood.ANF)
#Intercept--------------------------------------------------
Intercept.post<-exp(pos.Brood.AF$Intercept)
x11()
hist(Intercept.post,breaks=100)
mean(Intercept.post);sd(Intercept.post);HPDI(Intercept.post,prob=0.97)
# [1] 88.11617
# [1] 17.36389
# |0.97     0.97| 
#   58.69496 127.88715 
1-ecdf(Intercept.post)(0) #1
#b_AFB--------------------------------------------------
b_AFB.post<-exp(pos.Brood.AF$b_AFB)
x11()
hist(b_AFB.post,breaks=100)
mean(b_AFB.post);sd(b_AFB.post);HPDI(b_AFB.post,prob=0.97)
# [1] 1.309128
# [1] 0.1070902
# |0.97    0.97| 
#   1.109329 1.591704 
1-ecdf(b_AFB.post)(0) #1
#b_NrDay--------------------------------------------------
b_NrDay.post<-exp(pos.Brood.AF$b_NrDay)
x11()
hist(b_NrDay.post,breaks=100,freq=F)
mean(b_NrDay.post);sd(b_NrDay.post);HPDI(b_NrDay.post,prob=0.97)
# [1] 0.5273461
# [1] 0.03877901
# |0.97     0.97| 
#   0.4477055 0.6128513 
1-ecdf(b_NrDay.post)(0) #1
#b_FOB--------------------------------------------------
b_FOB.post<-exp(pos.Brood.AF$b_FOB)
x11()
hist(b_FOB.post,breaks=100)
mean(b_FOB.post);sd(b_FOB.post);HPDI(b_FOB.post,prob=0.97)
# [1] 1.897841
# [1] 0.152156
# |0.97    0.97| 
#   1.606668 2.290417
1-ecdf(b_FOB.post)(0) #1

#--------------------------------------------------
#higher predictivness of predictor
#--------------------------------------------------

AFB.FOB.post<-b_FOB.post-b_AFB.post
x11()
hist(AFB.FOB.post,breaks=100)
mean(AFB.FOB.post);sd(AFB.FOB.post);HPDI(AFB.FOB.post,prob=0.97)
# [1] 0.5887136
# [1] 0.1942882
# |0.97     0.97| 
#   0.1635975 1.0267060
(1-ecdf(AFB.FOB.post)(0))*100 #99.9 %  #better to evaluate AFB

FOB.Day.post<-b_FOB.post-b_NrDay.post
x11()
hist(FOB.Day.post,breaks=100)
mean(FOB.Day.post);sd(FOB.Day.post);HPDI(FOB.Day.post,prob=0.97)
# [1] 1.370495
# [1] 0.165708
# |0.97     0.97| 
#   0.9756299 1.7188552 
1-ecdf(FOB.Day.post)(0) #100 %  #better to evaluate FOB

AFB.Day.post<-b_AFB.post-b_NrDay.post
x11()
hist(AFB.Day.post,breaks=100)
mean(AFB.Day.post);sd(AFB.Day.post);HPDI(AFB.Day.post,prob=0.97)
# [1] 0.7817816
# [1] 0.1246369
# |0.97     0.97| 
#   0.5304877 1.0779336 
1-ecdf(AFB.Day.post)(0) #100 %  #better to evaluate AFB








#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++to predict df: AFB

#p.m.list<-list(pos.CFUb.AN,pos.CFUb.AF,pos.CFUb.NF,pos.CFUb.AF)
#p.m.list[[2]]$b_NrDay[1:10]
#precis(m.CFUb.AN)

#posterior for each model
pred.ANF<-matrix(NA,nrow=1000,ncol=nrow(data))
for (i in 1:nrow(data)){ 
  pred.ANF[,i]<-exp(
    pos.CFUb.ANF$Intercept+
      pos.CFUb.ANF$b_AFB*AFB.seq[i]+
      pos.CFUb.ANF$b_NrDay*mean(data$NrDay)+
      pos.CFUb.ANF$b_FOB*mean(data$FOB)+
      pos.CFUb.ANF$b_AFB_X_NrDay*AFB.seq[i]*mean(data$NrDay)+
      pos.CFUb.ANF$b_AFB_X_FOB*AFB.seq[i]*mean(data$FOB)+
      pos.CFUb.ANF$b_NrDay_X_FOB*mean(data$NrDay)*mean(data$FOB)+
      pos.CFUb.ANF$b_AFB_X_NrDay_X_FOB*AFB.seq[i]*mean(data$NrDay)*mean(data$FOB))}
pred.AN<-matrix(NA,nrow=1000,ncol=nrow(data))
for (i in 1:nrow(data)){ 
  pred.AN[,i]<-exp(
      pos.CFUb.AN$Intercept+
      pos.CFUb.AN$b_AFB*AFB.seq[i]+
      pos.CFUb.AN$b_NrDay*mean(data$NrDay)+
      pos.CFUb.AN$b_FOB*mean(data$FOB)+
      pos.CFUb.AN$b_AFB_X_NrDay*AFB.seq[i]*mean(data$NrDay))}
pred.AF<-matrix(NA,nrow=1000,ncol=nrow(data))
for (i in 1:nrow(data)){ 
  pred.AF[,i]<-exp(
      pos.CFUb.AF$Intercept+
      pos.CFUb.AF$b_AFB*AFB.seq[i]+
      pos.CFUb.AF$b_NrDay*mean(data$NrDay)+
      pos.CFUb.AF$b_FOB*mean(data$FOB)+
      pos.CFUb.AF$b_AFB_X_FOB*AFB.seq[i]*mean(data$FOB))}
pred.NF<-matrix(NA,nrow=1000,ncol=nrow(data))
for (i in 1:nrow(data)){ 
  pred.NF[,i]<-exp(
      pos.CFUb.NF$Intercept+
      pos.CFUb.NF$b_AFB*AFB.seq[i]+
      pos.CFUb.NF$b_NrDay*mean(data$NrDay)+
      pos.CFUb.NF$b_FOB*mean(data$FOB)+
      pos.CFUb.NF$b_NrDay_X_FOB*mean(data$NrDay)*mean(data$FOB))}
#weight
AFB.pred<-pred.AN *comp@output$weight[1]+
          pred.ANF*comp@output$weight[2]+
          pred.NF *comp@output$weight[3]+
          pred.AF *comp@output$weight[4]
# #OR   (random effects still missing; similate? not similate intercepts?)
# d.predict<-list( CFU_orig = rep(0,length(AFB.seq)), # empty outcome 
#                  AFB = AFB.seq,     
#                  NrDay = rep(mean(data$NrDay),length(AFB.seq)))#average
# test<-ensemble(m.CFU.000i,m.CFU.000,data=d.predict)

##
x11(5,5)
plot(jitter(data$AFB_orig),jitter(data$CFU_orig),col=rangi2,pch=16,
     xlab="AFB",ylab="CFU")
lines(AFB.seq.un,apply(AFB.pred,2,median))
shade(apply(AFB.pred,2,PI,prob=0.97),AFB.seq.un)
shade(apply(AFB.pred,2,PI,prob=0.89),AFB.seq.un)
shade(apply(AFB.pred,2,PI,prob=0.67),AFB.seq.un)
###

##shorter x
x11(5,5)
plot(jitter(data$AFB_orig),jitter(data$CFU_orig),col=rangi2,pch=16,
     xlab="AFB",ylab="CFU",ylim=c(0,1200))
lines(AFB.seq.un,apply(AFB.pred,2,median))
shade(apply(AFB.pred,2,PI,prob=0.97),AFB.seq.un)
shade(apply(AFB.pred,2,PI,prob=0.89),AFB.seq.un)
shade(apply(AFB.pred,2,PI,prob=0.67),AFB.seq.un)
abline(v=1,lty=2)
###
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
      exp(pos.CFUb.AF$Intercept+
            pos.CFUb.AF$b_AFB*AFB.seq[i]+
            pos.CFUb.AF$b_NrDay*N_20+
            pos.CFUb.AF$b_FOB*F_all[j]+
            pos.CFUb.AF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}
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
      exp(pos.CFUb.AF$Intercept+
            pos.CFUb.AF$b_AFB*AFB.seq[i]+
            pos.CFUb.AF$b_NrDay*N_50+
            pos.CFUb.AF$b_FOB*F_all[j]+
            pos.CFUb.AF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}
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
      exp(pos.CFUb.AF$Intercept+
            pos.CFUb.AF$b_AFB*AFB.seq[i]+
            pos.CFUb.AF$b_NrDay*N_80+
            pos.CFUb.AF$b_FOB*F_all[j]+
            pos.CFUb.AF$b_AFB_X_FOB*AFB.seq[i]*F_all[j])}}


##########
##########
# Figure 1
##########
##########
XAXT=c("s","n","n")
#x11(12,12)
tiff("afb-AFB.Fig.3.tiff",3200,3200,res=600)
op<-par(mai=c(0.1,0.1,0.1,0.1),oma=c(5,6,2,2),
        xpd=T,las=1,pch=1,mgp=c(3,0.5,0))
layout(matrix(c(1:9),3))
# day 20
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,7e+5),xlim=c(0,15),bty="n",xaxt=XAXT[i])
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_20[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_20[[i]],2,median))
  rect(-.1,7e+5,17,8e+5,col="white",border=NA)}
# day 50
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,7e+5),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_50[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_50[[i]],2,median))
  rect(-.1,7e+5,17,8e+5,col="white",border=NA)}
# day 80
for (i in 3:1){
  plot(NA,xlab="",ylab="",ylim=c(0,7e+5),xlim=c(0,15),bty="n",xaxt=XAXT[i],yaxt="n")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.97),AFB.seq.un,col="yellow")
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.89),AFB.seq.un)
  shade(apply(pred.ANF.N_80[[i]],2,PI,prob=0.67),AFB.seq.un)
  abline(v=c(0,5,10,15),h=c(2e+5,4e+5,6e+5),col="grey85",xpd=F)
  lines(AFB.seq.un,apply(pred.ANF.N_80[[i]],2,median))
  rect(-.1,7e+5,17,8e+5,col="white",border=NA)}
mtext(c("[AFB score/colony]","Clinical symptoms"),
      1,c(3.4,1.9),cex=c(1.2,1.5),outer=T)
mtext(c("Spore count","[CFU/Bee]"),
      2,c(4.1,2.5),cex=c(1.5,1.2),outer=T,las=0)
mtext(c("Day 20","Day 50","Day 80"),
      3,0,cex=1.3,outer=T,at=c(.2,.525,.85))
mtext(c("FOB 6","FOB 9","FOB 12"),
      4,0,cex=1.3,outer=T,at=c(.2,.525,.85),las=0)
par(op);dev.off()
##########





