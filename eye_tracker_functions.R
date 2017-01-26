###########################################
#EYE TRACKER FUNCTIONS
###########################################

# SAVE TRIAL START/END TIMES AND REMOVE THOSE LINES FROM THE FILE
clean_eye_tracker_data<-function(id){
  
  # First read in whole text file
  text_file<-scan(files.eye[id],character(0),sep="\n")
  
  # Expressions to look for - trial start, trial end 
  # and timestamp number with 10 to 11 digits at the start of each line
  regexp<-"Trial Started"
  regexp2<-"Trial Ended"
  regexp3<-"^[[:digit:]]{10,11}" 
  
  # Where does the trial start/end?
  start_trial_lines<-which(str_detect(text_file,regexp))
  end_trial_lines<-which(str_detect(text_file,regexp2))
  
  # when did trials start?
  trial_start_times<<-as.numeric(str_extract(text_file[start_trial_lines],pattern=regexp3))
  trial_end_times<<-as.numeric(str_extract(text_file[end_trial_lines],pattern=regexp3))
  
  # Save the cleaned data
  cat(text_file[-c(start_trial_lines,end_trial_lines)],file="file.txt",sep="\n")
  rm(text_file)
}

### INSERT TRIAL NUMBERS AND THE START/END OF EYE FIXATIONS

create_eye_track_data<-function(){
 
  # Read in cleaned file (from the previous function)
  eye.data<-fread("file.txt")
  
  ## INSERT TRIAL NUMBERS
  eye.data$trial<-0
  helper2<-eye.data$trial
  helper<-eye.data$V1
  
  # look up the data points that occured between each start and end of the trial
  # and give them a trial number starting with #1 
  for (i in 1:length(trial_start_times)){
    helper2[helper>=trial_start_times[i]&helper<=trial_end_times[i]]<-i}
  
  eye.data$trial<-helper2
 
  #check that all 240 trials have been assigned and remove the helpers
  #unique(helper2)
  rm(helper,helper2)
  
  #remove all eye tracking not belonging to particular trials
  # and data points from when the participant was not looking at the screen
  eye.data<-
    eye.data %>% 
    filter(trial>0) %>% 
    filter(V2>0)
  
  ##########
  #plot eyetracking of first 20 trials
  #omit y axis as eyes are mainly in one plane (see second histogram)
  
  # ggplot(aes(x=as.integer(V1/1000),y=V2),data=subset(eye.data,trial<=20))+geom_line()+facet_wrap(~trial,scales="free_x")
  # hist(eye.data$V2,breaks=seq(0,1500,10))
  # hist(eye.data$V3,breaks=seq(0,1500,10))
  #########
  
  
  # IDENTIFY FIXATION POINTS
    # THere are positioned at X=420px and Y=525px and X=1260px and Y= 525.
    # THe fixation cross is at X=840 and Y= 525.
    # Gratings are 420px horizontally and 262,5px vertically
    # meaning everything from 210 to 630 is left and everything from 1030 to 1450
    # we assume that everything from 740 to 940 is fixation cross

  # create vector with the x-axis boundaries for each position
  left.bounds<-c(210,630)
  right.bounds<-c(1050,1470)
  fixation.bounds<-c(740,940)
  
  eye.data$fix<-"none"
  eye.data$fix[(eye.data$V2-left.bounds[1])*(eye.data$V2-left.bounds[2])<0]<-c("left")
  eye.data$fix[(eye.data$V2-right.bounds[1])*(eye.data$V2-right.bounds[2])<0]<-c("right")
  eye.data$fix[(eye.data$V2-fixation.bounds[1])*(eye.data$V2-fixation.bounds[2])<0]<-c("fix")
  
  #see if this calculation is correct
  #ggplot(aes(x=V2,fill=fix),data=subset(eye.data))+geom_histogram()
  
  #calculate the time spent in the left/right/fixed area per trial
  
  #indicate changes - NEEDS TO BE DONE PER TRIAL
  eye.data$change<-0
  
  helper<-c("start",eye.data$fix[1:nrow(eye.data)-1])
  helper2<-c(eye.data$fix[2:nrow(eye.data)],NA)
  
  # mark the start of fixation as 1 - the current data point is different from the previous one
  eye.data$change<-ifelse(eye.data$fix==helper,0,1)
  
  # mark the end of fixation as 2 - the current data point is different from the following one
  eye.data$change<-ifelse(eye.data$fix==helper2,eye.data$change,2)
  
  # cleanup
  rm(helper,helper2)
  
  #exclude tracks with length 1, those trials have eye.data$change==2 that 
  # is immediately preceeded by another 2 (two ends of fixation in the row)
  # if that happens exclude the second one (as it is not preceded by a start of fixation)
  
  helper<-c(NA,eye.data$change[1:nrow(eye.data)-1])
  eye.data<-eye.data[!(eye.data$change==helper&eye.data$change==2),]
  rm(helper)
  
  #last cell must be 2
  eye.data$change[nrow(eye.data)]<-2
  
  
  ###### CHECK THAT EVERY TRIAL STARTS WITH change==1 AND ENDS WITH change==2
  
  helper2<-eye.data$change
  helper<-eye.data$V1
  
  for(itrial in unique(eye.data$trial)){
    trial.data<-
      eye.data %>%
      select(V1,trial,change) %>%
      filter(trial==itrial)
    start.trial = min(trial.data$V1)
    end.trial = max(trial.data$V1)
    
    helper2[helper==start.trial]<-1
    helper2[helper==end.trial]<-2
  }
  
  # add to the main file
  eye.data$change<-helper2
  
  # clear up  
  rm(helper,helper2)
  
  ### AND REMOVE TRACKS WITH LENGTH 1 (that pop up again),
  
  # if two 2s are next to each other - delete the second
  helper<-c(NA,eye.data$change[1:nrow(eye.data)-1])
  eye.data<-eye.data[!(eye.data$change==helper&eye.data$change==2),]
  rm(helper)
  
  # if two 1s are next to each otehr - delete the first
  helper<-c(eye.data$change[2:nrow(eye.data)],NA)
  eye.data<-eye.data[!(eye.data$change==helper&eye.data$change==1),]
  rm(helper)
  
  #last cell must be 2(end of fixation) - should be true anyway but better safe than sorry
  eye.data$change[nrow(eye.data)]<-2
 
  
   #check
  #ggplot(aes(x=as.integer(V1/1000),y=V2),data=subset(eye.data,trial<=20))+geom_line()+facet_wrap(~trial,scales="free_x")+geom_line(aes(y=change*1000,x=as.integer(V1/1000)))
  return(eye.data)
}


#### CALCULATE THE NUMBER OF FIXATIONS, THEIR POSITION & DURATION + THE DIAMETER OF PUPILS

calc_eye_tracker_values<-function(eye.data){
  
  ### CALCULTE LENGTH OF FIXATION
  # Extract the start/end of each fixation
  eye.change.data<-
    eye.data %>% 
    select(V1,fix,change,trial) %>% 
    filter(change>0)
  
  # put in the same row data for start of fixation(V1) with the timestamp of its end (time.end)
  # and calculate their difference to get the fixation time
  ec.data<-eye.change.data[eye.change.data$change==1,]
  ec.data$time.end<-eye.change.data$V1[eye.change.data$change==2]
  ec.data$duration<-ec.data$time.end-ec.data$V1
  
  # for each combination of fix & trial, sum up all values of duration
   ec.final<-
    ec.data %>% 
    select(fix,trial,duration) %>% 
    dcast(fix+trial~.,fun=sum) 
  
   names(ec.final)<-c("fix","trial","duration")
   ec.final$duration2<-as.integer(ec.final$duration)
  
  #now get last fixation durations
  ec.fl<-
    ec.data %>% 
    select(fix,trial,duration) %>%
    dcast(fix+trial~.,fun=function(x) tail(x,n=1),value.var="duration",fill=0) 
  names(ec.fl)<-c("fix","trial","last_fix")
  ec.fl$last_fix<-as.integer(ec.fl$last_fix)
  
  #and first fixation duration
  ec.ff<-
    ec.data %>% 
    select(fix,trial,duration) %>%
    dcast(fix+trial~.,fun=function(x) head(x,n=1),value.var="duration",fill=0) 
    names(ec.ff)<-c("fix","trial","first_fix")
    ec.ff$first_fix<-as.integer(ec.ff$first_fix)
  
  #ec.final<-
   # ec.final %>% 
    #filter(duration<2e+06)
  #ggplot(aes(x=(duration2/360000)),data=ec.final)+geom_histogram()+facet_wrap(~fix)+xlab("time[s]")
  
  #calculate right of total time for left and right fixation and also their ratio
  ec.wide<-
    ec.final %>% 
    select(fix,trial,duration2) %>% 
    spread(fix,duration2) %>% 
    mutate(ratio=right/(left+right))
 
  #make transformation for last and first fixation durations
  ec.wide.last.dur<-
    ec.fl %>% 
    select(fix,trial,last_fix) %>% 
    spread(fix,last_fix)
  ec.wide.first.dur<-
    ec.ff %>% 
    select(fix,trial,first_fix) %>% 
    spread(fix,first_fix)
  
  ### DETERMINE FIRST AND LAST FIXATION IN THE TRIAL
  fix.first<-
    eye.data %>%
    select(fix,trial,change) %>% 
    filter(fix%in%c("left","right")&change==1) %>% 
    group_by(trial) %>% 
    summarise(first_fix=head(fix,n=1), # returns the first data.point in the fix column
              last_fix=tail(fix,n=1), # returns the last data.point in the fix column
              changes=length(fix))
  
  
  # sometimes fix first looses a trial
  # this adds the lost trials to the data frame with NA as values
  a<-seq(1,240)
  add<-a[!a%in%fix.first$trial]
  if (length(add)>0)
  for (i in add){
    k<-fix.first[1,]
    k[1,]<-NA
    k$trial<-i
    fix.first<-bind_rows(fix.first,k)
    fix.first<-fix.first[order(fix.first$trial),]
  }
  
  # ADD FIXATION TIMES and FIRST/LAST FIXATION TOGETHER
  fix.first$duration_left<-ec.wide$left
  fix.first$duration_right<-ec.wide$right
  fix.first$duration_none<-ec.wide$none
  fix.first$duration_fix<-ec.wide$fix
  fix.first$duration_first_fix<-ifelse(fix.first$first_fix=="left",ec.wide.first.dur$left,ec.wide.first.dur$right)
  fix.first$duration_last_fix<-ifelse(fix.first$first_fix=="left",ec.wide.last.dur$left,ec.wide.last.dur$right)
  
  
  ### PUPIL DIAMETER
 
  # labels the first and last fixation of the trial
  eye.data$fixPos<-"none"
  
  helper2<-eye.data$fixPos
  helper<-eye.data$V1
  
  for(itrial in unique(eye.data$trial)){
    trial.data<-
      eye.data %>%
      select(V1,V4,trial,change,fix) %>%
      filter(trial==itrial)
    start.fix = trial.data$V1[which(trial.data$change==1)]
    end.fix = trial.data$V1[which(trial.data$change==2)]
    
    helper2[helper>=start.fix[1]&helper<=end.fix[1]]<-"firstFix"
    helper2[helper>=start.fix[length(start.fix)]&helper<=end.fix[length(start.fix)]]<-"lastFix"
    }
  
  # add to the main file
  eye.data$fixPos<-helper2
  
  # clear up  
  rm(helper,helper2)
  
  # calculate the mean pupil diameter for the whole duration of the trial
  pupil_all<-
    eye.data %>%
    select(V4,trial,fix) %>% 
    group_by(trial) %>% 
    summarise(pupil_all = mean(V4))
  
  # calculate the mean pupil diameter for the fist and last fixation
  pupil_fix<-
    eye.data %>%
    select(V4,trial,fixPos) %>% 
    filter(fixPos%in%c("firstFix","lastFix")) %>% 
    group_by(trial,fixPos) %>% 
    summarise(pupil=mean(V4,na.rm=T))%>% 
    spread(fixPos,pupil)
  colnames(pupil_fix)<-c("trial","pupil_ff","pupil_lf")
  
  # merges those together 
  pupil <- merge(x=pupil_all,y=pupil_fix,by="trial")
  
  # ADD PUPIL DATA TO THE FIX.FIRST
  eye.values <- merge(x=fix.first,y=pupil,by="trial")
  
  
  
  return(eye.values)
}
