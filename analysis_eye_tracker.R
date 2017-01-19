#predicting confidence and choice
#1. choice
my.data$last_is_choice<-ifelse(my.data$key_resp_direction.keys==my.data$last_fix,1,0)
my.data$first_is_choice<-ifelse(my.data$key_resp_direction.keys==my.data$first_fix,1,0)
my.data$ratio<-my.data$duration_right/(my.data$duration_right+my.data$duration_left)

choices_left<-
  my.data %>%
  filter(social3=="none") %>% 
  group_by(participant) %>% 
  summarise(number_left=sum(key_resp_direction.keys=="left"),N=length(key_resp_direction.keys))

fix_left<-
  my.data %>%
  filter(social3=="none") %>% 
  group_by(participant) %>% 
  summarise(number_left=sum(first_fix=="left"),N=length(key_resp_direction.keys))



last_choice_ana<-
  my.data %>% 
  filter(social3=="none") %>% 
  group_by(participant) %>% 
  summarise(mean_score=mean(last_is_choice,na.rm=T),sd_score=sd(last_is_choice,na.rm=T))


my.data$ratio_choice<-ifelse((my.data$ratio>0.5&my.data$key_resp_direction.keys=="right")|(my.data$ratio<0.5&my.data$key_resp_direction.keys=="left"),1,0)
ratio_choice_ana<-
  my.data %>% 
  filter(social3=="none") %>% 
  group_by(participant) %>% 
  summarise(mean_score=mean(ratio_choice,na.rm=T),sd_score=sd(ratio_choice,na.rm=T))

ratio_corr_ana<-
  my.data %>% 
  filter(social3=="none"&key_resp_direction.keys!="None") %>% 
  group_by(participant,correct,key_resp_direction.keys) %>% 
  summarise(mean_score=mean(ratio,na.rm=T),sd_score=sd(ratio,na.rm=T)) %>% 
  mutate(condition=paste(correct,key_resp_direction.keys))

ggplot(aes(y=mean_score,x=factor(participant)),data=ratio_corr_ana)+
  geom_bar(stat="identity")+
  facet_wrap(~condition)+
  theme_classic()+
  geom_abline(aes(intercept=0.5,slope=0),col="red")
  


first_choice_ana<-
  my.data %>% 
  filter(social3=="none") %>% 
  group_by(participant) %>% 
  summarise(mean_score=mean(first_is_choice,na.rm=T),sd_score=sd(first_is_choice,na.rm=T))
change_ana<-
  my.data %>% 
  filter(social3=="none") %>% 
  group_by(participant,key_resp_direction.corr) %>%
  summarise(mean_score=mean(changes,na.rm=T),sd_score=sd(changes,na.rm=T))
ggplot(aes(y=mean_score,x=factor(key_resp_direction.corr)),data=change_ana)+geom_bar(stat="identity")+facet_wrap(~participant)


my.data$ratio2<-ifelse(my.data$ratio<0.5,1-my.data$ratio,my.data$ratio)
my.data$first_fix_corr<-ifelse(my.data$first_fix==my.data$correct,1,0)
ratio2_ana<-
  my.data %>% 
  filter(social3=="none") %>% 
  group_by(participant,first_fix,key_resp_direction.corr) %>% 
  summarise(mean_score=mean(ratio3,na.rm=T))

ggplot(aes(y=mean_score,x=key_resp_direction.corr),data=ratio2_ana)+geom_bar(stat="identity")+facet_grid(first_fix_corr~participant)

ggplot(aes(x=as.integer(V1/1000),y=V2),data=subset(eye.data.list[[4]],trial<=100&trial>80))+geom_line()+facet_wrap(~trial,scales="free_x")

#dwell time difference correct-incorrect
my.data$dt_diff<-0
my.data$dt_diff<-ifelse(my.data$correct=="left",my.data$duration_left-my.data$duration_right,my.data$duration_right-my.data$duration_left)
ggplot(aes(y=zConf,x=I(key_resp_direction.rt),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=scale((dt_diff))),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=ratio3,col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=scale(dwell_chosen),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
summary(with(subset(my.data,participant==unique(my.data$participant)[3]&social3=="none"&abs(dt_diff<1000000)),lm(zConf~ratio3+key_resp_direction.rt)))


my.data$ratio3<-my.data$ratio2*my.data$key_resp_direction.rt

######
#new ratio/ chosen vs unchosen
my.data$dwell_chosen<-0
my.data$dwell_chosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_unchosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_right,my.data$duration_left)
my.data<-
  my.data %>% 
  mutate(ratio3=ifelse(correct==key_resp_direction.keys,dwell_chosen*2.1/(dwell_unchosen*0.75+dwell_chosen*2.1),dwell_chosen*0.75/(dwell_unchosen*2.1+dwell_chosen*.75)))

my.data<-
  my.data %>% 
  mutate(dt_diff=ifelse(correct==key_resp_direction.keys,dwell_chosen*2.25-dwell_unchosen*1.25,dwell_chosen*1.25-dwell_unchosen*2.25))

my.data$dwell_correct<-ifelse(my.data$correct=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_incorrect<-ifelse(my.data$correct=="left",my.data$duration_right,my.data$duration_left)
my.data<-
  my.data %>% 
  mutate(ratio4=dwell_correct/(dwell_incorrect+dwell_correct))






