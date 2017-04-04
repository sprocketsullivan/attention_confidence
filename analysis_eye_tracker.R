#dwell time difference correct-incorrect
source("lba_calc.R")
my.data$dt_diff<-0
my.data$dt_diff<-ifelse(my.data$correct=="left",my.data$duration_left-my.data$duration_right,my.data$duration_right-my.data$duration_left)

my.data$dwell_correct<-0
my.data$dwell_incorrect<-0
my.data$dwell_correct<-ifelse(my.data$correct=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_incorrect<-ifelse(my.data$correct=="left",my.data$duration_right,my.data$duration_left)

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
}
summary.dmc(m.fitty[[10]])
my.data<-
  my.data %>% 
  mutate(ratio3=ifelse(correct==key_resp_direction.keys,dwell_chosen*lba_v_cor/(dwell_unchosen*lba_v_incor+dwell_chosen*lba_v_cor),dwell_chosen*lba_v_incor/(dwell_unchosen*lba_v_cor+dwell_chosen*lba_v_incor)))
my.data<-
  my.data %>% 
  mutate(ratio4=ifelse(correct==key_resp_direction.keys,dwell_chosen*lba_v_cor-dwell_unchosen*lba_v_incor,dwell_chosen*lba_v_incor-dwell_unchosen*lba_v_cor))

my.data<-
  my.data %>% 
  mutate(weight_conf=dwell_correct+dwell_incorrect) %>% 
  mutate(strength_conf=(dwell_correct-dwell_incorrect)/(dwell_correct+dwell_incorrect)) %>% 
  mutate(strength_conf2=dwell_correct-dwell_incorrect) %>%
  mutate(strength_conf3=(dwell_correct-dwell_incorrect)/(dwell_correct+dwell_incorrect)) %>%
  mutate(ratio_weighted_evidence2=dwell_chosen/dwell_unchosen) %>% 
  mutate(ratio_weighted_evidence=dwell_correct/dwell_incorrect) %>% 
  mutate(bin_strength = ntile(strength_conf, 6)) %>% 
  mutate(bin_weight = ntile(weight_conf, 6)) 

c_a_per_strength<-
  my.data %>% 
  group_by(participant,bin_strength) %>% 
  filter(social3=="none") %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            sd_cor=sd(key_resp_direction.corr),
            N=n(),
            se_cor= sd(key_resp_direction.corr)/ sqrt(n()),
            ymin=mean(key_resp_direction.corr)-sd(key_resp_direction.corr)/ sqrt(n()),
            ymax=mean(key_resp_direction.corr)+sd(key_resp_direction.corr)/ sqrt(n()))
ggplot(aes(y=mean_cor,x=bin_strength),data=c_a_per_strength)+geom_point()+facet_wrap(~participant)+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)
c_a_per_weight<-
  my.data %>% 
  group_by(participant,bin_weight) %>% 
  filter(social3=="none") %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            sd=sd(key_resp_direction.corr),
            N=n(),
            se_cor= sd(key_resp_direction.corr)/ sqrt(n()),
            ymin=mean(key_resp_direction.corr)-sd(key_resp_direction.corr)/ sqrt(n()),
            ymax=mean(key_resp_direction.corr)+sd(key_resp_direction.corr)/ sqrt(n()))
ggplot(aes(y=mean_cor,x=bin_weight),data=c_a_per_weight)+geom_point()+facet_wrap(~participant,scales="free_y")+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)


c_a_per_weight<-
  my.data %>% 
  group_by(participant,bin_weight) %>% 
  filter(social3=="none") %>% 
  summarise(mean_cor=mean(zConf),
            sd=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))
ggplot(aes(y=mean_cor,x=bin_weight),data=c_a_per_weight)+geom_point()+facet_wrap(~participant,scales="free_y")+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)
c_a_per_strength<-
  my.data %>% 
  group_by(participant,bin_strength) %>% 
  filter(social3=="none") %>% 
  summarise(mean_cor=mean(zConf),
            sd_cor=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))
ggplot(aes(y=mean_cor,x=bin_strength),data=c_a_per_strength)+geom_point()+facet_wrap(~participant,scales="free_y")+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)


m.meta



ggplot(aes(y=zConf,x=weight_conf),data=subset(my.data,social3=="none"&key_resp_direction.rt<3&ratio_weighted_evidence<3&zConf<4))+geom_point(aes(col=factor(key_resp_direction.corr)))+geom_smooth()+facet_wrap(~participant,scales="free")+theme_classic()+ylab("")+theme(legend.title = element_blank())
ggplot(aes(y=zConf,x=strength_conf),data=subset(my.data,social3=="none"&key_resp_direction.rt<3&ratio_weighted_evidence<3&zConf<4))+geom_point(aes(col=factor(key_resp_direction.corr)))+geom_smooth()+facet_wrap(~participant,scales="free")+theme_classic()+ylab("")+theme(legend.title = element_blank())






my.data$key_resp_direction.corr

hist(my.data$dwell_incorrect)
#my.data$dwell_chosen<-my.data$dwell_chosen/1000000
#my.data$dwell_unchosen<-my.data$dwell_unchosen/1000000
#my.data$dwell_correct<-my.data$dwell_correct/1000000
#my.data$dwell_incorrect<-my.data$dwell_incorrect/1000000

my.data$duration_last_fix<-as.numeric(my.data$duration_last_fix)
my.data$fix_correct_first<-ifelse(my.data$first_fix==my.data$correct,1,0)
my.data$fix_correct_last<-ifelse(my.data$last_fix==my.data$correct,1,0)

lba_help_cor<-2.3
lba_help_incor<-1
ggplot(aes(y=zConf,x=I(ifelse(correct==key_resp_direction.keys,(dwell_chosen*lba_help_cor-dwell_unchosen*lba_help_incor)/(dwell_chosen*lba_help_cor+dwell_unchosen*lba_help_incor),(dwell_chosen*lba_help_incor-dwell_unchosen*lba_help_cor)/(dwell_chosen*lba_help_incor+dwell_unchosen*lba_help_cor))),col=factor(c_choice)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+facet_wrap(~participant,scales="free")+ylim(-2,2)+theme_classic()


ggplot(aes(y=zConf,x=I((((weight_conf)*strength_conf))),col=c_choice),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+geom_smooth(method="lm")+facet_wrap(~participant,scales="free")+ylim(-2,2)+xlab("Evidence (strength*weight)")+theme_classic()+ylab("z transformed confidence")+theme(legend.title = element_blank())
ggplot(aes(y=zConf,x=I(((weight_conf))),col=c_choice),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+geom_smooth(method="lm")+facet_wrap(~participant,scales="free")+ylim(-2,2)+xlab("Evidence (strength*weight)")+theme_classic()+ylab("z transformed confidence")+theme(legend.title = element_blank())
ggplot(aes(y=zConf,x=I(((ratio_weighted_evidence))),col=c_choice),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+geom_smooth(method="lm")+facet_wrap(~participant,scales="free")+ylim(-2,2)+xlab("Evidence (strength*weight)")+theme_classic()+ylab("z transformed confidence")+theme(legend.title = element_blank())
ggplot(aes(y=zConf,x=I((((weight_conf)))),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3&zConf<2))+geom_point()+geom_smooth(method="lm")+facet_wrap(~participant,scales="free")
ggplot(aes(y=zConf,x=I((((strength_conf)))),col=(dwell_correct)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+geom_smooth(method="lm")+facet_wrap(~participant,scales="free")
ggplot(aes(y=zConf,x=I(((dwell_correct*lba_v_cor-(dwell_incorrect*lba_v_incor)))),col=c_choice),data=subset(my.data,social3=="none"&key_resp_direction.rt<3&fix_correct_last==0))+geom_point()+geom_smooth(method="lm")+facet_wrap(~participant,scales="free")+xlab("Evidence (strength*weight)")+theme_classic()+ylab("z transformed confidence")+theme(legend.title = element_blank())
ggplot(aes(y=I((dwell_correct)-(dwell_incorrect)),x=c_choice),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_boxplot()+facet_wrap(~participant)
ggplot(aes(y=I(ratio_weighted_evidence2),x=c_choice),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_boxplot()+facet_wrap(~participant)+ylim(0,3)





my.data$fix_correct_first

my.data$dwell_correct
my.data$changes
ggplot(aes(y=zConf,x=I(duration_last_fix/1000000),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3&(zConf>-1)))+geom_point()+geom_smooth(method="lm")+facet_wrap(~participant,scales="free")+ylim(-2,2)

selector<-seq(1,nrow(my.data))[-which(my.data$conf<0.52&my.data$conf>0.48)]

ggplot(aes(y=key_resp_direction.rt,x=c_choice),data=my.data)+geom_boxplot()+facet_wrap(~participant)
my.data$key_resp_direction.corr

m.d
m.meta

ggplot(aes(y=zConf,x=I(((dwell_correct-dwell_incorrect)/(dwell_correct+dwell_incorrect))),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I(((dwell_correct-dwell_incorrect)/(dwell_correct+dwell_incorrect))*strength_conf),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I(dwell_chosen),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<3))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
ggplot(aes(y=zConf,x=I((changes)),col=factor(key_resp_direction.corr)),data=subset(my.data,social3=="none"&key_resp_direction.rt<1.5))+geom_point()+facet_wrap(~participant,scales="free")+geom_smooth(method="lm")
hist(my.data$weight_conf)
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



save.image("intermediate.Rdata")
load("intermediate.Rdata")




library(scatterplot3d)
scatterplot3d(my.data$weight_conf, my.data$strength_conf, my.data$zConf)
library(rgl)
k.data<-subset(my.data,participant==unique(my.data$participant)[4]&social3=="none")
plot3d(k.data$weight_conf, k.data$strength_conf, k.data$zConf, type='s', size=0.5,col=(k.data$key_resp_direction.corr+1))




library(mgcv)
m.conf<-gam(zConf~s(weight_conf),data=k.data)
plot(k.data$zConf~k.data$strength_conf)
summary(m.conf)
plot(m.conf)
plot(k.data$zConf,fitted(m.conf))




m.prop.corr<-list()
my.data$glm.fit<-0
#get values for each player
for (i in 1:length(unique(my.data$participant))){
  fit.data<-subset(my.data,participant==unique(my.data$participant)[i]&social3=="none"&!is.na(dwell_correct)&!is.na(dwell_incorrect))
  m.fit<-glm(key_resp_direction.corr~dwell_correct-dwell_incorrect,data=fit.data,family=binomial)
  my.data$glm.fit[my.data$participant==unique(my.data$participant)[i]&my.data$social3=="none"&!is.na(my.data$dwell_correct)&!is.na(my.data$dwell_incorrect)]<-inv.logit(as.numeric(predict(m.fit)))
}


