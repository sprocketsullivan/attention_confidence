#analysis of tracks that yield high or low confidence
unique(my.data$participant)
c.id<-1831

eye.p<-my.eye#subset(my.eye,participant==c.id)
export.data<-subset(my.data,social3=="none")
with(subset(eye.p,fixNum==1&change==1),aggregate(trial,list(participant),length))
eye.p$trial
speeder<-ifelse(my.data$correct=="right",-1,1)
eye.p$correct<-""
kicker<-rep(0,nrow(eye.p))
for(i in unique(my.data$participant))
  for(j in 1:max(my.data$trial))
    kicker[eye.p$trial==j&eye.p$participant==i]<-speeder[my.data$trial==j&my.data$participant==i]
eye.p$correct<-ifelse(kicker==-1,"right","left")

speeder<-NULL
speeder<-ifelse(my.data$key_resp_direction.keys=="right",-1,1)
kicker<-rep(0,nrow(eye.p))
for(i in unique(my.data$participant))
  for(j in 1:max(my.data$trial))
    kicker[eye.p$trial==j&eye.p$participant==i]<-speeder[my.data$trial==j&my.data$participant==i]
eye.p$choice<-ifelse(kicker==-1,"right","left")
eye.p$c_choice<-eye.p$choice==eye.p$correct

speeder<-NULL
speeder<-my.data$zConf
kicker<-rep(0,nrow(eye.p))
for(i in unique(my.data$participant))
  for(j in 1:max(my.data$trial))
    kicker[eye.p$trial==j&eye.p$participant==i]<-speeder[my.data$trial==j&my.data$participant==i]
eye.p$zConf<-kicker




eye.p2<-NULL
for (i in unique(my.data$participant)){
  eye.p2<-rbind(eye.p2,eye.p[which(eye.p$participant==i)[which(eye.p$trial[eye.p$participant==i]%in%(export.data$trial[export.data$participant==i]))],])
}


eye.p2$counter<-0
eye.p2<-
  eye.p2 %>% 
  group_by(participant,trial) %>% 
  mutate(count=seq(1,n()))


eye.p3<-
  eye.p2 %>% 
  filter(fix%in%c("left","right")&change==1) %>% 
  group_by(participant,trial) %>% 
  mutate(fix_lr=seq(1,n()),len_tria=max(fix_lr))
eye.p3$first_fix_correct<-FALSE

eye.p3$trial_new<-0
eye.p3$trial_new<-ifelse(eye.p3$fix_lr==1,1,0)


eye.p3<-
  eye.p3 %>% 
  group_by(participant) %>% 
  mutate(trial_new=cumsum(trial_new)) %>% 
  group_by(participant) %>% 
  mutate(ranked_conf=bin(zConf))




for(i in unique(eye.p3$participant))
  for(j in unique(eye.p3$trial[(eye.p3$participant)==i])){
    if(sum(eye.p3$trial==j&eye.p3$participant==i&eye.p3$fixPos=="firstFix")>0){
      
    eye.p3$first_fix_correct[eye.p3$trial==j&eye.p3$participant==i]<-eye.p3$fix[eye.p3$trial==j&eye.p3$participant==i&eye.p3$fixPos=="firstFix"]==eye.p3$correct[eye.p3$trial==j&eye.p3$participant==i&eye.p3$fixPos=="firstFix"]
    
    }
  }



eye.p4<-
  eye.p3 %>% 
  mutate(ia=interaction(first_fix_correct,c_choice)) %>% 
  group_by(participant,first_fix_correct,c_choice,fix_lr) %>% 
  summarise(mean_time=mean(time))
ggplot(aes(y=-mean_time,x=fix_lr,shape=first_fix_correct,col=c_choice),data=subset(eye.p4,fix_lr<6))+geom_point()+geom_line()+geom_line()+facet_wrap(~participant,scales="free_y")

ggplot(aes(y=-time,x=fix_lr,col=c_choice,alpha=zConf,shape=first_fix_correct),data=subset(eye.p3,ranked_conf>60))+geom_point()+geom_line()+facet_grid(participant~ranked_conf,scales="free_y")
hist(eye.p3$len_tria)
eye.p5<-
  eye.p3 %>% 
  filter(len_tria>1)

ggplot(aes(x=len_tria),data=subset(eye.p3,fix_lr==1))+geom_histogram(binwidth = 1)+facet_wrap(~participant)


summary(lm(I(-eye.p5$time[eye.p5$fix_lr==2]) ~ I(-eye.p5$time[eye.p5$fix_lr==1])))


c.trial<-which(export.data$key_resp_direction.corr==1&export.data$zConf<0.5&export.data$zConf>-0.5)
c.trial<-which(export.data$key_resp_direction.corr==1&export.data$zConf< -1)
ggplot(aes(y=V2,x=count),data=subset(eye.p,trial%in%c.trial))+geom_line()+facet_wrap(~trial)

#average times of fixation left and right 
eye.p<-
  eye.p %>% 
  group_by(trial,fixNum) %>% 
  mutate(t_fix=min(time))
eye.p.red<-
  eye.p %>% 
  filter(change==1) %>% 
  group_by(trial,fix) %>% 
  summarise(avg_fix=mean(t_fix),sum_fix=sum(t_fix)) %>% 
  filter(fix=="left"|fix=="right") %>% 
  group_by(trial) %>% 
  summarise(avg_time=mean(avg_fix),sum_time=sum(sum_fix))
eye.p.red$zConf<-export.data$zConf[export.data$trial%in%eye.p.red$trial]
eye.p.red$correct<-export.data$c_choice[export.data$trial%in%eye.p.red$trial]
ggplot(aes(y=avg_time,x=zConf,col=correct),data=subset(eye.p.red,zConf>-2))+geom_point()+geom_smooth(method="lm")
