#eye tracker functions

clean_eye_tracker_data<-function(id){
  
  #first read in whole text file
  text_file<-scan(files.eye[id],character(0),sep="\n")
  
  #########
  #search file for some expressions for the start and end of trial
  regexp<-"Trial Started"
  regexp2<-"Trial Ended"
  regexp3<-"^[[:digit:]]{10,11}"
  
  #where are trial start commands?
  start_trial_lines<-which(str_detect(text_file,regexp))
  end_trial_lines<-which(str_detect(text_file,regexp2))
  
  #when did trials start?
  trial_start_times<<-as.numeric(str_extract(text_file[start_trial_lines],pattern=regexp3))
  trial_end_times<<-as.numeric(str_extract(text_file[end_trial_lines],pattern=regexp3))
  
  #save remaining data
  cat(text_file[-c(start_trial_lines,end_trial_lines)],file="file.txt",sep="\n")
  rm(text_file)
}

create_eye_track_data<-function(){
  #read in cleaned file
  eye.data<-fread("file.txt")
  
  #insert trial numbers
  eye.data$trial<-0
  helper2<-eye.data$trial
  helper<-eye.data$V1
  for (i in 1:length(trial_start_times))
    helper2[helper>=trial_start_times[i]&helper<=trial_end_times[i]]<-i
  eye.data$trial<-helper2
  unique(helper2)
  rm(helper,helper2)
  
  #remove all eye tracking not belonging to particular trials
  eye.data<-
    eye.data %>% 
    filter(trial>0) %>% 
    filter(V2>0)
  
  ##########
  #plot eyetracking of first 20 trials
  #omit y axis as eyes are mainly in one plane (see second histogram)
  #########
  # ggplot(aes(x=as.integer(V1/1000),y=V2),data=subset(eye.data,trial<=20))+geom_line()+facet_wrap(~trial,scales="free_x")
  # hist(eye.data$V2,breaks=seq(0,1500,10))
  # hist(eye.data$V3,breaks=seq(0,1500,10))
  
  #########
  #identify fixation points
  # THere are positioned at X=420px and Y=525px and X=1240px and Y= 525.
  # THe fixation cross is at X=840 and Y= 525.
  # Gratings are 420px horizontally and 262,5px vertically
  # meaning everything from 210 to 630 is left and everything from 1030 to 1450
  # we assume that everything from 700 to 900 is fixation cross
  #create vector
  
  left.bounds<-c(210,630)
  right.bounds<-c(1030,1450)
  fixation.bounds<-c(700,900)
  eye.data$fix<-"none"
  eye.data$fix[(eye.data$V2-left.bounds[1])*(eye.data$V2-left.bounds[2])<0]<-c("left")
  eye.data$fix[(eye.data$V2-right.bounds[1])*(eye.data$V2-right.bounds[2])<0]<-c("right")
  eye.data$fix[(eye.data$V2-fixation.bounds[1])*(eye.data$V2-fixation.bounds[2])<0]<-c("fix")
  
  #see if this calculation is correct
  #ggplot(aes(x=V2,fill=fix),data=subset(eye.data))+geom_histogram()
  
  #calculate the time spent in the left/right/fixed area per trial
  #indicate changes
  eye.data$change<-0
  helper<-c("start",eye.data$fix[1:nrow(eye.data)-1])
  helper2<-c(eye.data$fix[2:nrow(eye.data)],NA)
  eye.data$change<-ifelse(eye.data$fix==helper,0,1)
  eye.data$change<-ifelse(eye.data$fix==helper2,eye.data$change,2)
  #cleanup
  rm(helper,helper2)
  #exclude tracks with length 1 (eye.data$change==2)
  helper<-c(NA,eye.data$change[1:nrow(eye.data)-1])
  eye.data<-eye.data[!(eye.data$change==helper&eye.data$change==2),]
  rm(helper)
  #last cell must be 2
  eye.data$change[nrow(eye.data)]<-2
  #check
  #ggplot(aes(x=as.integer(V1/1000),y=V2),data=subset(eye.data,trial<=20))+geom_line()+facet_wrap(~trial,scales="free_x")+geom_line(aes(y=change*1000,x=as.integer(V1/1000)))
  return(eye.data)
}

calc_eye_tracker_values<-function(eye.data){
  #extract times
  eye.change.data<-
    eye.data %>% 
    select(V1,fix,change,trial) %>% 
    filter(change>0)
  ec.data<-eye.change.data[eye.change.data$change==1,]
  ec.data$time.end<-eye.change.data$V1[eye.change.data$change==2]
  ec.data$duration<-ec.data$time.end-ec.data$V1
  ec.final<-
    ec.data %>% 
    select(fix,trial,duration) %>% 
    dcast(fix+trial~.,fun=sum) 
  names(ec.final)<-c("fix","trial","duration")
  ec.final$duration2<-as.integer(ec.final$duration)
  ec.final<-
    ec.final %>% 
    filter(duration<2e+06)
  #ggplot(aes(x=(duration2/360000)),data=ec.final)+geom_histogram()+facet_wrap(~fix)+xlab("time[s]")
  
  #calculate right of total left+right
  ec.wide<-
    ec.final %>% 
    select(fix,trial,duration2) %>% 
    spread(fix,duration2) %>% 
    mutate(ratio=right/(left+right))
  
  #fixate at which side first in each trial
  fix.first<-
    eye.data %>%
    select(fix,trial,change) %>% 
    filter(fix%in%c("left","right")&change==1) %>% 
    group_by(trial) %>% 
    summarise(first_fix=head(fix,n=1),
              last_fix=tail(fix,n=1),
              changes=length(fix))
  fix.first$duration_left<-ec.wide$left
  fix.first$duration_right<-ec.wide$right
  fix.first$duration_none<-ec.wide$none
  fix.first$duration_fix<-ec.wide$fix
  return(fix.first)
}
