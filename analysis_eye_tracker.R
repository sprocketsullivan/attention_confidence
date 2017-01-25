#dwell time difference correct-incorrect
my.data$dt_diff<-0
my.data$dt_diff<-ifelse(my.data$correct=="left",my.data$duration_left-my.data$duration_right,my.data$duration_right-my.data$duration_left)
my.data$dwell_chosen<-0
my.data$dwell_chosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_unchosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_right,my.data$duration_left)
my.data<-
  my.data %>% 
  mutate(ratio3=ifelse(correct==key_resp_direction.keys,dwell_chosen*2.1/(dwell_unchosen*0.75+dwell_chosen*2.1),dwell_chosen*0.75/(dwell_unchosen*2.1+dwell_chosen*.75)))
my.data$duration_last_fix<-as.numeric(my.data$duration_last_fix)

ggplot(aes(y=zConf,x=I(key_resp_direction.rt),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=scale((dt_diff))),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((1-ratio3)),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((ratio_inc)),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"&ratio_inc<1&ratio_inc>-1))+geom_point()+facet_wrap(~participant,scales = "free_x")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((ratio4)/(key_resp_direction.rt)),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I(ratio3),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=scale(dwell_chosen),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
summary(with(subset(my.data,participant==unique(my.data$participant)[3]&social3=="none"&abs(dt_diff<1000000)),lm(zConf~ratio3+key_resp_direction.rt)))




#calculate gain from last fixation
my.data$dwell_chosen_wolast<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_unchosen_wolast<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_right,my.data$duration_left)
my.data$dwell_chosen_wolast<-ifelse((my.data$key_resp_direction.keys=="left"&my.data$last_fix=="left")|(my.data$key_resp_direction.keys=="right"&my.data$last_fix=="right"),my.data$dwell_chosen_wolast-my.data$duration_last_fix,my.data$dwell_chosen_wolast)
my.data$dwell_chosen_wolast<-ifelse((my.data$key_resp_direction.keys=="left"&my.data$last_fix=="right")|(my.data$key_resp_direction.keys=="right"&my.data$last_fix=="left"),my.data$dwell_unchosen_wolast-my.data$duration_last_fix,my.data$dwell_chosen_wolast)

my.data<-
  my.data %>% 
  mutate(ratio_wo=ifelse(correct==key_resp_direction.keys,dwell_chosen_wolast*2.1/(dwell_unchosen_wolast*0.75+dwell_chosen_wolast*2.1),dwell_chosen_wolast*0.75/(dwell_unchosen_wolast*2.1+dwell_chosen_wolast*.75)))
my.data$ratio4<-ifelse(my.data$ratio3>0.5,my.data$ratio3-0.5,0.5-my.data$ratio3)
my.data$ratio_inc<-(my.data$ratio3-my.data$ratio_wo)

ggplot(aes(x=ratio4,col=),data=my.data)+geom_histogram()+facet_wrap(~participant)


