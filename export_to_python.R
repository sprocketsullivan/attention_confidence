#export a sample file to python
export.data<-
  my.data %>% 
  filter(participant==31871&social3=="none") %>% 
  select(key_resp_direction.corr,key_resp_direction.rt,dwell_correct,dwell_incorrect,zConf) %>% 
  filter(!is.na(dwell_correct)&!is.na(key_resp_direction.rt))
names(export.data)<-c("response","rt","dwell_chosen","dwell_unchosen","conf")


export.data<-
  export.data %>% 
  mutate(dwell_chosen=(dwell_chosen/1000000)) %>% 
  mutate(dwell_unchosen=(dwell_unchosen/1000000)) %>% 
  mutate(strength=dwell_unchosen+dwell_chosen,weight=(dwell_unchosen-dwell_chosen)/(dwell_unchosen+dwell_chosen))
write.csv(export.data,"ddm_test.csv",row.names = F)
hist(export.data$dwell_chosen)


export.data$ddm<-2.36*export.data$rt-1.6*export.data$strength+9.5*export.data$weight-6.5*export.data$strength*export.data$weight
export.data$ddm2<-2.9*export.data$rt-1.7*export.data$dwell_chosen-1.1*export.data$dwell_unchosen
export.data$ddm2<-2.35*export.data$rt-2.1*export.data$strength
ggplot(aes(y=conf,x=ddm),data=export.data)+geom_point()+geom_smooth(method="lm")

export.data$weight
unique(my.data$participant)

