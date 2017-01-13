check.migration.dmc<-function(samples,cut=30, verbose=FALSE){
  # check if at least one chain has not converged with the others
  report<-FALSE
  for (i in 1:samples$n.pars){
    dist<-diff(sort(as.array(window(theta.as.mcmc.list(samples)[,i],start=samples$nmc))))
    test<-max(dist)/median(dist)
    if(test>cut) {report<-TRUE
    if(verbose)     cat(samples$p.names[i], 'parameter chains have not converged. Index value:',test,'\n')
    }
  }
  report
}