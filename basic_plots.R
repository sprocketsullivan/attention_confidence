################################################################
# BASIC GRAPHS
################################################################

# This script outputs the following graphs for each participant:
#   accuracy for invalid/inc/valid
#   mean RT for invalid/inc/valid AND correct/incorrect responses
#   mean RT for ... compared with SI
#   distribution of RT times for ...
#   mean confidence for ...
#   distribution of confidence for ...
#   mean confidence for ... compared with SI

# Outputs a data frame for each condition
# AND one compiled data frame with all graphs

##################################################################
# clear the workspace
#rm(list=ls())
################################################################

#### PRE-REQ

# Import Packages 
source("essential_functions_libraries.R")

# Load my.data (if it is not already present)
if (!exists("my.data")){
  load("confidence_attention.RData")
  print("loading my.data")
}

# Graphs settings
dodge <- position_dodge(.5)
fig<- list() #container for figures
colorScheme<-c("#F5A503","#56D9CD", "#3AA1BF") #yellow, l.blue, d.blue

####################################################################

### ACCURACY
fig.accuracy <- list()

s.accuracy<-
  my.data %>% 
  group_by(participant,social3) %>%
  summarise(mean_corr=mean(key_resp_direction.corr*100),N=length(key_resp_direction.corr))

p.accuracy <-
  ggplot(aes(y=mean_corr,x=social3,fill=social3),data=s.accuracy) +
  geom_bar(stat='identity',position="dodge") +
  ylab("Proportion correct") + xlab("") + theme_classic()+
  geom_hline(yintercept=50,linetype="dashed") +
  scale_fill_manual(values=colorScheme)

f.accuracy <- group_by(s.accuracy,participant) %>%
  do(ACC = p.accuracy %+% ., 
     ACC_F = p.accuracy + facet_wrap(~ participant))
f.accuracy$ACC_F[[1]]
### RT - MEAN
s.RT.M<-
  group_by(my.data,participant,c_choice,social3)%>%
  summarise(meanCorr=mean(key_resp_direction.rt,na.rm=T))

p.RT.M <- 
  ggplot(aes(y=meanCorr,x=social3,fill=social3),data=s.RT.M) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("RT") + xlab("")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c(colorScheme,"grey"))+
  facet_wrap(~c_choice)

f.RT.M <- group_by(s.RT.M,participant) %>%
  do(RT.M = p.RT.M %+% .,
     RT.M_F = p.RT.M + facet_grid(c_choice~participant))
f.RT.M$RT.M_F[[1]]
### RT - SI COMPARED TO no SI
s.RT.SI<-
  group_by(my.data,participant,c_choice,social3)%>%
  summarise(meanCorr=mean(key_resp_direction.rt,na.rm=T))%>%
  spread(social3,meanCorr)%>%
  mutate(Invalid=invalid-none,Valid=valid-none)%>%
  gather(socialInfo,meanCorr,Invalid,Valid)

p.RT.SI <- 
  ggplot(aes(y=meanCorr,x=socialInfo,fill=socialInfo),data=s.RT.SI) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("RT") + xlab("compared to no SI")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c(colorScheme,"grey"))+
  facet_wrap(~c_choice)

f.RT.SI <- group_by(s.RT.SI,participant) %>%
  do(RT.SI = p.RT.SI %+% .,
     RT.SI_F = p.RT.SI + facet_grid(c_choice~participant))

### RT - DISTRIBUTION
p.RT.D <- 
  ggplot(aes(x=zRT,fill=c_choice),data=my.data) + 
  geom_density(alpha=0.2) +
  facet_wrap(~ social3)

f.RT.D <- group_by(my.data,participant)%>%
  do(RT.D = p.RT.D %+% .,
     RT.D_F = p.RT.D + facet_grid(social3~participant))
f.RT.D$RT.D_F[[1]]
### CONFIDENCE - MEAN
s.conf.M<-
  group_by(my.data,participant,c_choice,social3)%>%
  summarise(meanCorr=mean(zConf,na.rm=T))

p.conf.M <- 
  ggplot(aes(y=meanCorr,x=social3,fill=social3),data=s.conf.M) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("Confidence") + xlab("")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c(colorScheme,"grey"))+
  facet_wrap(~c_choice)

f.conf.M <- group_by(s.conf.M,participant) %>%
  do(conf.M = p.conf.M %+% .,
     conf.M_F = p.conf.M + facet_grid(c_choice~participant))
f.conf.M$conf.M_F[1]
### CONFIDENCE - DISTRIBUTION

p.conf.D <- 
  ggplot(aes(x=conf,fill=c_choice),data=subset(my.data,social3=="none")) + 
  geom_histogram(position='dodge',bins=10) +
  facet_wrap(~ social3,scales="free_y")

f.conf.D <- 
  group_by(my.data,participant)%>%
  filter(social3=="none") %>% 
  do(conf.D = p.conf.D %+% .,
     conf.D_F = p.conf.D + facet_wrap(~participant))
f.conf.D$conf.D_F[1]

### CONFIDENCE - SI COMPARED TO no SI
s.conf.SI<-
  group_by(my.data,participant,c_choice,social3)%>%
  summarise(meanCorr=mean(zConf,na.rm=T))%>%
  spread(social3,meanCorr)%>%
  mutate(Invalid=invalid-none,Valid=valid-none)%>%
  gather(socialInfo,meanCorr,Invalid,Valid)

p.conf.SI <- 
  ggplot(aes(y=meanCorr,x=socialInfo,fill=socialInfo),data=s.conf.SI) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("Confidence") + xlab("compared to no SI")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c(colorScheme,"grey"))+
  facet_wrap(~c_choice)

f.conf.SI <- group_by(s.RT.SI,participant) %>%
  do(conf.SI = p.conf.SI %+% .,
     conf.SI_F = p.conf.SI + facet_grid(c_choice~participant))

f.conf.SI$conf.SI_F[1]
### PUT ALL PLOTS INTO A SINGLE TABLE

# create a list with all data.frames to be merged
f.all <- list(f.accuracy,f.conf.D,f.conf.M,f.conf.SI,f.RT.D,f.RT.M,f.RT.SI)

# use reduce to call function merge (merges two data.frames)
basic_plots <- Reduce(function(...) merge(...,by = "participant", all = TRUE), f.all)

# save it
save(basic_plots,file='basic_plots.RData')




