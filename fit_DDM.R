######################################################
#FIt DDM
######################################################

# NOTE: if you want to use more than one factor to model a parameter,
# keep in mind to write them in p.map and factors in the same order!
# AND S must always be the first!!!

#######################################################

# CLEAR WORKSPACE

rm(list=ls())

######################################################

#### PRE-REQ

# Import Packages & functions
source("essential_functions_libraries.R")
source("ddm_function.R")
library(plyr) # prob need to find better function than revalue

# Load my.data (if it is not already present)
if (!exists("my.data")){
  load("confidence_attention.RData")
  print("loading my.data")
}

# DOWNLOAD THE DDM model 
#get dropbox folder and DDM data directory on dropbox
db.folder<-get.dropbox.folder()
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\DMC_160825\\",sep=""))

# change the wd to the DDM dropbox folder, download model and utils, then change back

#!!! need a nicer way but different versions kept crashing at load_model

wd <- getwd()

setwd(data.folder)
source("tutorial\\file_utils.R")
load_model ("ddm.R")
setwd(wd)
######################################################

## PREALOCATION

# models and priors
model <- list()
p.prior <- list()

# Participants parameters
participants <- list()

# Information criterion and weights
WAIC<-list()
winner<-data.frame(stringsAsFactors = F)
tll<-list()

# Posterior-predictive
pp<-list() 
gelman<-list()
ES<-list()
######################################################

## DEFINE MODELs

# Model flags
constants <- c(st0=0,d=0,sz=0.11) #sz=0.11 || 0.0
match.map <- list(M=list(s1="r1", s2="r2"))
responses <- c("r1","r2")
type <- "rd"

# parameter type: z = starting point ; v = drift rate
# model: SP = StartingPoint ; DR = DriftRate ; SPr = StartingPointReduced ; DR = DriftRateReduced ; RE = Reduced; SPDR = Complete model

#### SP - starting point varies with SI
p.map <- list(a="1",v="1",z="Q_red",d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"),Q_red=c("Rght", "Left", "Incg"))

model$SP <- model.dmc(p.map,factors,responses,match.map,constants,type)

# priors
p1 <- c(a=1,v=0,z.Rght = 2, z.Left = 2, z.Incg = 2, sv=1,t0=1)                           
p2 <- c(a=1,v=2,z.Rght = 2, z.Left = 2, z.Incg = 2, sv=1,t0=1)
lower <- c(0,-5,NA,NA,NA,0,NA)
upper <- c(2, 5,NA,NA,NA,2,NA)
dists <- c("tnorm","tnorm","beta","beta","beta","tnorm","beta")

p.prior$SP <- prior.p.dmc(p1, p2, lower, upper, dists)

#### SPDR - starting point AND drift rate varies with SI
p.map <- list(a="1",v="Q_red",z="Q_red",d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"),Q_red=c("Rght", "Left", "Incg"))

model$SPDR <- model.dmc(p.map,factors,responses,match.map,constants,type)

# priors
p1 <- c(a=1,v.Rght = 0, v.Left = 0, v.Incg = 0, z.Rght = 2,z.Left = 2,z.Incg = 2, sv=1,t0=1)                           
p2 <- c(a=1,v.Rght = 2, v.Left = 2, v.Incg = 2, z.Rght = 2,z.Left = 2,z.Incg = 2, sv=1,t0=1)
lower <- c(0,-5,-5,-5,NA,NA,NA,0,NA)
upper <- c(2, 5, 5, 5,NA,NA,NA,2,NA)
dists <- c("tnorm","tnorm","tnorm","tnorm","beta","beta","beta","tnorm","beta")

p.prior$SPDR <- prior.p.dmc(p1, p2, lower, upper, dists)

#### RE - only varies with stimulus (single drift rate)
p.map <- list(a="1",v="1",z="1",d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"))

model$RE <- model.dmc(p.map,factors,responses,match.map,constants,type)

###priors
p1 <- c(a=1,v=0,z = 2, sv=1,t0=1)                          
p2 <- c(a=1,v=2,z = 2, sv=1,t0=1)
lower <- c(0,-5,NA,0,NA)
upper <- c(2, 5,NA,2,NA)
dists <- c("tnorm","tnorm","beta","tnorm","beta")

p.prior$RE <- prior.p.dmc(p1, p2, lower, upper, dists)
#####################################################

## SET-UP DATA FILE (my.data.ddm)
# This file contains all the information necessary for the ddm analysis of the various models

# REMOVE trials with no ANSWER (check with ULF if that is correct)
f.data <- my.data[!is.na(my.data$key_resp_direction.rt)]

#RT
RT <- f.data$key_resp_direction.rt

# RESPONSE (also ddm boundary)
R <- f.data$key_resp_direction.keys
R <- revalue(R, c("left" = "r1","right" = "r2")) #sets lower boundary to left and upper to right
#R <- ifelse(f.data$key_resp_direction.keys=="left","r1","r2")

# STIMULUS - (or correct response)
S <- f.data$correct
S <- revalue(S, c("left" = "s1", "right" = "s2"))  
#R <- ifelse(f.data$correct=="left","s1","s2")

# Q_red - SOCIAL CUES
f.data$Q_red<-ifelse(f.data$op_square_right==1&f.data$op_square_left==1,"Incg",ifelse(f.data$op_square_right==1,"Rght","Left"))
Q_red <- factor(f.data$Q_red)

# participant ID
id <- f.data$participant

# CREATE my.data.ddm file
my.data.ddm <- data.frame(S, Q_red, R, RT, id)

# delete rows with NA (if any)
my.data.ddm  <- na.omit(my.data.ddm)
my.data.ddm$R <- droplevels(my.data.ddm$R)


#####################################################################################################

## RUN THE ANALYSIS

select<-unique(my.data.ddm$id); # Folco had here sample(unique(...)) do I need it?

for(i in 1:length(select)){
  
  data2 <- subset(my.data.ddm,id==select[i])[1:4]
  row.names(data2) <- NULL
  
  #### SP model ####
  
  # final dataset for model fitting
  data  <- data.model.dmc(data2,model$SP)
  
  # first round with migration
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$SP,data), report = 25, cores=4,p.migrate=.05)
  
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  # second round
  #samples2 <- run.converge.dmc(samples.dmc(nmc=50,samples=samples), cut = 1.08,cores=4,nmc=50,report=10, max.try = 60, verbose = T)
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  
  ## save values SP model ##
  participants$SP[[i]] <- summary.dmc(samples2)$statistics
  
  ## model comparison using WAIC and DIC 
  tll$SP[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$SP[i] <- as.numeric(tll$SP[[i]][3])
  
  gelman$SP[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$SP[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  
  ## PP probability
  pp$SP[[i]] <- post.predict.dmc(samples2)
  
 
  
  
  #### SPDR model ####
  data = data.model.dmc(data2, model$SPDR)
  
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$SPDR,data), report = 25, cores=4,p.migrate=.05)
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  #samples2 <- run.converge.dmc(samples.dmc(nmc=50,samples=samples), cut = 1.13,cores=4,nmc=50,report=10, max.try = 60, verbose = T)
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  participants$SPDR[[i]] <- summary.dmc(samples2)$statistics
  
  tll$SPDR[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$SPDR[i] <- as.numeric(tll$SPDR[[i]][3])
  
  gelman$SPDR[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$SPDR[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  pp$SPDR[[i]] <- post.predict.dmc(samples2)
  
  
  #### RE - Reduced model ####
  data  <- data.model.dmc(data2,model$RE)
  
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$RE,data), report = 25, cores=4,p.migrate=.05)
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  #samples2 <- run.converge.dmc(samples.dmc(nmc=50,samples=samples), cut = 1.05,cores=4,nmc=50,report=10, max.try = 60, verbose = T)
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  participants$RE[[i]] <- summary.dmc(samples2)$statistics
  
  tll$RE[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$RE[i] <- as.numeric(tll$RE[[i]][3])
  
  gelman$RE[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$RE[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  pp$RE[[i]] <- post.predict.dmc(samples2)
  
  #### Weights ####    
  models<-list(SP=tll$SP[[i]],SPDR=tll$SPDR[[i]],RE=tll$RE[[i]])
  WAIC$weights[[i]] <- loocompare.dmc(models,digits=3)
  
  ### count cases in which ci of WAIC differences includes 0
  win <- which.min(WAIC$weights[[i]][1,])
  winner[i,names(models[win])]<-"wins"
  
  junk<-capture.output(
    for (m in attributes(models[-win])$names) ifelse(diff(loocompare.dmc(models[[win]], models[[m]]))>0, winner[i,m]<-"ci",winner[i,m]<-NA) 
  )
  
  
}

save(file='DDM_select.RData', list = ls())

###########################################################################

## plot Z/V
m.SP <- matrix(NA,nrow=10,ncol=4)
colnames(m.SP)<-c("v","z.Rght","z.Left","Z.Incg")
m.RE <- matrix(NA,nrow=10,ncol=2)
colnames(m.RE)<-c("v","z")
m.SPDR <- matrix(NA,nrow=10,ncol=6)
colnames(m.SPDR)<-c("v.Rght","v.Left","v.Incg","z.Rght","z.Left","Z.Incg")


for (ipart in 1:7){
  m.SP[ipart,1:4] <-participant1$SP[[ipart]][2:5,1]
  m.RE[ipart,1:2] <-participant1$RE[[ipart]][2:3,1]
  m.SPDR[ipart,1:6] <-participant1$SPDR[[ipart]][2:7,1]
}
for (ipart in 1:3){
  m.SP[ipart+7,1:4] <-participant2$SP[[ipart]][2:5,1]
  m.RE[ipart+7,1:2] <-participant2$RE[[ipart]][2:3,1]
  m.SPDR[ipart+7,1:6] <-participant2$SPDR[[ipart]][2:7,1]
}

m.SP<-data.frame(m.SP)
m.RE<-data.frame(m.RE)
m.SPDR <- data.frame(m.SPDR)

m.SP$part<-c(1811,1821,1831,1842,1851,3341,3352,31861,31871,31891)
m.RE$part<-c(1811,1821,1831,1842,1851,3341,3352,31861,31871,31891)
m.SPDR$part<-c(1811,1821,1831,1842,1851,3341,3352,31861,31871,31891)

m.SP.p<-melt(m.SP,id.vars="part")
m.SPDR.p<-melt(m.SPDR,id.vars="part")
m.RE.p<-melt(m.RE,id.vars="part")

# PLOT
p.SP = ggplot(aes(x=variable, y=value, fill=variable),data=m.SP.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("") + 
  geom_abline(intercept = 0, slope = 0)

f.SP <- group_by(m.SP.p,part) %>%
  do(SP = p.SP %+% ., 
     SP_F = p.SP + facet_wrap(~ part))

p.SPDR = ggplot(aes(x=variable, y=value, fill=variable),data=m.SPDR.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("") + 
  geom_abline(intercept = 0, slope = 0)

f.SPDR <- group_by(m.SPDR.p,part) %>%
  do(SPDR = p.SPDR %+% ., 
     SPDR_F = p.SPDR + facet_wrap(~ part))

p.RE = ggplot(aes(x=variable, y=value, fill=variable),data=m.RE.p) + 
  geom_bar(stat = "identity") + 
  xlab("") + ylab("") + 
  geom_abline(intercept = 0, slope = 0)

f.RE <- group_by(m.RE.p,part) %>%
  do(RE = p.RE %+% ., 
     RE_F = p.RE + facet_wrap(~ part))

f.all <- list(f.SP,f.SPDR,f.RE)
DDM_plots <- Reduce(function(...) merge(...,by = "part", all = TRUE), f.all)

save(DDM_plots,file="DDM_plots.RData")

# # DO SOME PLOTS
# 
# PLOT WEIGHTS
m.WAIC <- matrix(NA,nrow=3,ncol=3)

for (ipart in 1:3){
  m.WAIC[ipart,1:3] <-WAIC$weights[[ipart]][1,1:3]
}

colnames(m.w)<- c("SP","SPDR","RE")

m.w <- matrix(NA,nrow=3,ncol=3)

for (ipart in 1:3){
  m.w[ipart,1:3] <-WAIC$weights[[ipart]][2,1:3]
}

colnames(m.w)<- c("SP","SPDR","RE")

m.w<-data.frame(m.w)
m.WAIC<-data.frame(m.WAIC)

m.w$part <- select
m.WAIC$part <-select

m.w.p <- melt(m.w, id.vars="part")
m.WAIC.p <- melt(m.WAIC, id.vars="part")

p.WAIC = ggplot(aes(x=variable, y=value),data=m.WAIC.p) + geom_bar(stat = "identity") + xlab("model")
p.w = ggplot(aes(x=variable, y=value),data=m.w.p) + geom_bar(stat = "identity")+ xlab("model")

# do the graph for all participants and save it into a list
fig.WAIC.p <- group_by(m.WAIC.p,part) %>%
  do(plots = p.WAIC %+% .)

fig.w.p <- group_by(m.w.p,part) %>%
  do(plots = p.w %+% . )

grid.arrange(fig.w.p$plots[[1]],fig.w.p$plots[[2]],fig.w.p$plots[[3]],fig.w.p$plots[[4]],fig.w.p$plots[[5]],fig.w.p$plots[[6]],fig.w.p$plots[[7]], ncol=4, nrow=2)

