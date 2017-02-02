#dwell time difference correct-incorrect
my.data$dt_diff<-0
my.data$dt_diff<-ifelse(my.data$correct=="left",my.data$duration_left-my.data$duration_right,my.data$duration_right-my.data$duration_left)
my.data$dwell_chosen<-0
my.data$dwell_chosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_unchosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_right,my.data$duration_left)
my.data$lba_v_cor<-0
my.data$lba_v_incor<-0
m.fitty<-list()
#get values for each player
for (i in 1:length(unique(my.data$participant))){
  fit.data<-subset(my.data,participant==unique(my.data$participant)[i]&social3=="none")
  m.fit<-lba_calc(fit.data)
  m.fit.sum<-summary.dmc(m.fit)
  my.data$lba_v_cor[my.data$participant==unique(my.data$participant)[i]]<-m.fit.sum$statistics[3,1]
  my.data$lba_v_incor[my.data$participant==unique(my.data$participant)[i]]<-m.fit.sum$statistics[4,1]
  m.fitty[[i]]<-m.fit
}
summary.dmc(m.fitty[[1]])
my.data<-
  my.data %>% 
  mutate(ratio3=ifelse(correct==key_resp_direction.keys,dwell_chosen*lba_v_cor/(dwell_unchosen*lba_v_incor+dwell_chosen*lba_v_cor),dwell_chosen*lba_v_incor/(dwell_unchosen*lba_v_cor+dwell_chosen*lba_v_incor)))
my.data<-
  my.data %>% 
  mutate(ratio4=ifelse(correct==key_resp_direction.keys,dwell_chosen*lba_v_cor-dwell_unchosen*lba_v_incor,dwell_chosen*lba_v_incor-dwell_unchosen*lba_v_cor))

my.data<-
  my.data %>% 
  mutate(weight_conf=ifelse(correct==key_resp_direction.keys,dwell_chosen*lba_v_cor+dwell_unchosen*lba_v_incor,dwell_chosen*lba_v_incor+dwell_unchosen*lba_v_cor)) %>% 
  mutate(strength_conf=ifelse(correct==key_resp_direction.keys,(dwell_chosen*lba_v_cor-dwell_unchosen*lba_v_incor)/(dwell_chosen*lba_v_cor+dwell_unchosen*lba_v_incor),(dwell_chosen*lba_v_incor-dwell_unchosen*lba_v_cor)/(dwell_chosen*lba_v_incor+dwell_unchosen*lba_v_cor))) %>% 
  mutate(p_cor=ifelse(correct==key_resp_direction.keys,dwell_chosen*lba_v_cor-dwell_unchosen*lba_v_incor,dwell_chosen*lba_v_incor-dwell_unchosen*lba_v_cor))

m.meta
m.diff
my.data$duration_last_fix<-as.numeric(my.data$duration_last_fix)
my.data$fix_correct_first<-ifelse(my.data$first_fix==my.data$correct,1,0)
my.data$fix_correct_last<-ifelse(my.data$last_fix==my.data$correct,1,0)
ggplot(aes(y=zConf,x=I(key_resp_direction.rt),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant,scale="free")+geom_smooth(method="lm")
ggplot(aes(y=conf,x=I(((weight_conf*strength_conf))),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=conf,x=I(((weight_conf*strength_conf))),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+geom_smooth()
ggplot(aes(y=zConf,x=I((dwell_chosen-dwell_unchosen)*p_cor),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I(dwell_chosen+dwell_unchosen),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((changes)),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<1.5))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
my.data$conf
ggplot(aes(y=zConf,x=I((strength_conf)),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((weight_conf)),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=scale(dt_diff),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((1-ratio3)),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((ratio_inc)),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"&ratio_inc<1&ratio_inc>-1))+geom_point()+facet_wrap(~participant,scales = "free_x")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((ratio4)),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&zConf<4&social3=="none"))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((ratio3)/(dwell_chosen+dwell_unchosen)),col=factor(paste(key_resp_direction.corr))),data=subset(my.data,abs(dt_diff)<1000000&zConf<4&social3=="none"))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I(ratio3),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&zConf<4))+geom_point()+facet_wrap(~participant)+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I(changes),col=factor(key_resp_direction.corr)),data=subset(my.data,zConf<4&social3=="none"))+geom_point()+facet_wrap(~participant,scale="free")+geom_smooth()+theme(legend.title=element_blank())

ggplot(aes(y=zConf,x=time_diff_w,col=factor(paste(key_resp_direction.corr,fix_correct_first))),data=subset(my.data,abs(dt_diff)<1000000&zConf<4&social3=="none"))+geom_point()+facet_wrap(~participant,scale="free")+geom_smooth(method="lm")+theme(legend.title=element_blank())
ggplot(aes(y=zConf,x=dt_diff,col=factor(paste(key_resp_direction.corr))),data=subset(my.data,abs(dt_diff)<1000000&zConf<4&social3=="none"))+geom_point()+facet_wrap(~participant,scale="free")+geom_smooth(method="lm")+theme(legend.title=element_blank())

ggplot(aes(y=zConf,x=,col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant,scale="free_x")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=scale(dwell_chosen),col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant,scale="free_x")+geom_smooth(method="lm")

ggplot(aes(y=pupil_lf,x=ratio3,col=factor(key_resp_direction.corr)),data=subset(my.data,abs(dt_diff)<1000000&social3=="none"))+geom_point()+facet_wrap(~participant,scale="free")+geom_smooth(method="lm")
summary(with(subset(my.data,participant==unique(my.data$participant)[3]&social3=="none"&abs(dt_diff<1000000)),lm(zConf~ratio3+key_resp_direction.rt)))


#calculate gain from last fixation
my.data$dwell_chosen_wolast<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_unchosen_wolast<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_right,my.data$duration_left)
my.data$dwell_chosen_wolast<-ifelse((my.data$key_resp_direction.keys=="left"&my.data$last_fix=="left")|(my.data$key_resp_direction.keys=="right"&my.data$last_fix=="right"),my.data$dwell_chosen_wolast-my.data$duration_last_fix,my.data$dwell_chosen_wolast)
my.data$dwell_chosen_wolast<-ifelse((my.data$key_resp_direction.keys=="left"&my.data$last_fix=="right")|(my.data$key_resp_direction.keys=="right"&my.data$last_fix=="left"),my.data$dwell_unchosen_wolast-my.data$duration_last_fix,my.data$dwell_chosen_wolast)
m.meta



my.data<-
  my.data %>% 
  mutate(ratio_wo=ifelse(correct==key_resp_direction.keys,dwell_chosen_wolast*2.1/(dwell_unchosen_wolast*0.75+dwell_chosen_wolast*2.1),dwell_chosen_wolast*0.75/(dwell_unchosen_wolast*2.1+dwell_chosen_wolast*.75)))
my.data<-
  my.data %>% 
  mutate(time_diff_w=ifelse(correct==key_resp_direction.keys,dwell_chosen*2.1-dwell_unchosen*0.75,dwell_chosen*0.75-dwell_unchosen*2.1))


my.data$ratio4<-ifelse(my.data$ratio3>0.5,my.data$ratio3-0.5,0.5-my.data$ratio3)
my.data$ratio_inc<-(my.data$ratio3-my.data$ratio_wo)

ggplot(aes(x=ratio4,col=),data=my.data)+geom_histogram()+facet_wrap(~participant)



fit.data<-subset(my.data,participant==unique(my.data$participant)[4]&social3=="none")
m.1<-nls(zConf~a*strength_conf*weight_conf+b,data=fit.data)
summary(m.1)
hist(my.data$weight_conf)


ggplot(aes(y=zConf,x=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&zConf<3))+geom_boxplot()+facet_wrap(~participant)

gggplot(aes(y=zConf,x=I(((strength_conf)*(weight_conf))),col=key_resp_direction.rt),data=subset(my.data,social3=="none"&participant==6261&zConf<5&key_resp_direction.corr==1))+geom_point()+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I(key_resp_direction.rt),col=key_resp_direction.rt),data=subset(my.data,social3=="none"&participant==6261&zConf<5&key_resp_direction.corr==1))+geom_point()+geom_smooth(method="lm")
unique(my.data$participant)
fit.data<-subset(my.data,participant==1831&social3=="none"&key_resp_direction.corr==1)
fit.data$conf_cat<- ifelse(fit.data$zConf<0.5,-1,1)
ggplot(aes(y=fix_correct_first,x=factor(conf_cat)),data=fit.data)+geom_bar(stat="summary",fun.y="mean")
m.1<-lm(zConf~fix_correct_first+changes+fix_correct_last+I(duration_none+duration_fix)+zRT+duration_last_fix+pupil_all+pupil_lf+I(strength_conf*weight_conf),data=fit.data)
m.1<-lm(zConf~fix_correct_last+I(strength_conf*weight_conf),data=fit.data)
summary(m.1)
ggplot(aes(y=zConf,x=I((strength_conf*(weight_conf))),col=win_evidence/key_resp_direction.rt),data=fit.data)+geom_point()
fit.data$new<-abs(1-fit.data$duration_right/fit.data$duration_left)

#evidence of winning accumulator compared to maximum evidence
my.data$win_evidence<-ifelse(my.data$key_resp_direction.corr==1,my.data$dwell_chosen*my.data$lba_v_cor,my.data$dwell_chosen*my.data$lba_v_incor)
max(my.data$win_evidence,na.rm=T)
my.data[,max_evidence:=max(win_evidence,na.rm=T),by=participant]
my.data$delta_bound<-my.data$win_evidence-my.data$max_evidence






