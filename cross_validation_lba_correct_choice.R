#predict correct/incorrect from eye tracking
source("lba_calc.R")
res.validation<-NULL
for (id in unique(my.data$participant)){
  #select particpant
  help.data<-subset(my.data,participant==id)
  #select only incongruent trials
  help.data<-subset(help.data,social3=="none")
  #particion vector for 10 fold cross validation
  ident.vec<-rep(seq(1,nrow(help.data)/10),10)
  for (i in 1:(nrow(help.data)/10)){
    #get training data
    training.data<-help.data[ident.vec!=i,]
    lba.training<-lba_calc(training.data)
    summary.lba<-summary.dmc(lba.training)
    v.true<-summary.lba$statistics[3,1]
    v.false<-summary.lba$statistics[4,1]
    #get validation data
    validation.data<-help.data[ident.vec==i,]
    validation.data$dwell_chosen<-0
    validation.data$dwell_chosen<-ifelse(validation.data$key_resp_direction.keys=="left",validation.data$duration_left,validation.data$duration_right)
    validation.data$dwell_unchosen<-ifelse(validation.data$key_resp_direction.keys=="left",validation.data$duration_right,validation.data$duration_left)
    validation.data<-
      validation.data %>% 
      mutate(ratio=ifelse(correct==key_resp_direction.keys,dwell_chosen*v.true/(dwell_unchosen*v.false+dwell_chosen*v.true),dwell_chosen*v.false/(dwell_unchosen*v.true+dwell_chosen*v.false)))
    correct<-as.numeric(ifelse(validation.data$ratio<0.5,0,1)==validation.data$key_resp_direction.corr)
    res.validation<-rbind(res.validation,cbind(correct,i,id))
  }
}



