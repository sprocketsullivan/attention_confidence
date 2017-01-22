lba_calc<-function(help.data)
{
  #S#R#RT
  S<-ifelse(help.data$correct=="left","s1","s2")
  R<-ifelse(help.data$key_resp_direction.keys=="left","r1","r2")
  RT<-help.data$key_resp_direction.rt
  data.model<-data.frame(S=S,R=R,RT=RT)
  rm(S,R,RT)
  #load_data ("dmc_3_3.RData")
  # NB: For the LBA one of B, mean_v or sd_v must be fixed in at least one
  #     cell of the design. In this case we fix sd_v for both cells
  p.map <- list(A="1",B="1",mean_v="M",sd_v="1",t0="1",st0="1")
  const <- c(st0=0,sd_v=1)
  # Same simple design as for previous LNR examples
  model <- model.dmc(p.map,constants=const,match.map=list(M=list(s1="r1",s2="r2")),
                     factors=list(S=c("s1","s2")),responses=c("r1","r2"),type="norm")
  # Simulate some data, with around 65% accuracy
  # p.vector  <- c(A=.25,B=.35,mean_v.true=1,mean_v.false=.25,t0=.2)
  # data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
  data.model <- data.model.dmc(data.model,model)
  # Data distributions similar to LNR model
  #par(mfrow=c(1,2))
  #plot.cell.density(data.cell=data.model[data.model$S=="s1",],C="r1",xlim=c(0,2))
  #plot.cell.density(data.cell=data.model[data.model$S=="s2",],C="r2",xlim=c(0,2))
  # Slow errors
  crct <- (data.model$S=="s1" & data.model$R=="r1") |
    (data.model$S=="s2" & data.model$R=="r2")
  round(tapply(data.model$RT,list(data.model$S,C=crct),mean),2)
  # Give t0 a uniform prior from 0.1-1s, other priors normal, truncated
  # below for A and B as they must be positive, unbounded for the v
  p.prior <- prior.p.dmc(
    dists = c("tnorm","tnorm","tnorm","tnorm","beta"),
    p1=c(A=.3,B=.3,mean_v.true=1,mean_v.false=0,t0=1),                           
    p2=c(1,1,3,3,1),lower=c(0,0,NA,NA,.1),upper=c(NA,NA,NA,NA,1)
  )
  #par(mfcol=c(2,3)); for (i in names(p.prior)) plot.prior(i,p.prior)
  # Parameters of the LBA are more strongly correlated than those of the LNR
  # hence longer burnin and more stuck chains are to be expected, so try a longer
  # burnin run than for LNR.
  samples <- samples.dmc(nmc=400,p.prior,data.model)
  samples <- run.dmc(samples, report = 25, cores=4,p.migrate=.05)
  #plot.dmc(samples,layout=c(3,4))
  #plot.dmc(samples,layout=c(3,4),start=300,smooth=FALSE)
  # Looks like burnt in, so get a longer run without migration
  samples1 <- run.dmc(samples.dmc(nmc=1000,samples=samples), 
                      cores=4,report=25)
  return(samples1)
}
# #plot.dmc(samples1,layout=c(3,4),smooth=FALSE)
# 
# # R-hat shows whole series is close to converged
# #gelman.diag(theta.as.mcmc.list(samples1),transform=TRUE)
# 
# # Add 500 more to see if that settles things down (longer series often help)
# # samples2 <- run.dmc(samples.dmc(nmc=500,samples=samples1,add=TRUE), 
# #                     cores=4,report=50)
# # plot.dmc(samples2,layout=c(3,4),smooth=FALSE)
# 
# # Now reports converged
# gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
# 
# # Now have over 500 of each type
# effectiveSize(theta.as.mcmc.list(samples1))
# 
# # Looking at autocorrelations
# acf.dmc(samples,chain=1,par="A")
# 
# # Confirm that no chains are stuck
# pick.stuck.dmc(samples1,cut=10,verbose=TRUE)
# 
# 
# # Tabled estimates confirm some inaccuracy in estimates of A, B, and 
# # mean_v.false with mean_v and t0 parameters relatively well recovered.
# summary.dmc(samples1)
