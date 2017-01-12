################################################################
# FIT META D' TO DATA
################################################################

# clear the workspace
rm(list=ls())

################################################################

#### PRE-REQ

# Import Packages 
source("essential_functions_libraries.R")
source("meta_dprime_functions.R")

# Load my.data (if it is not already present)
if (!exists("my.data")){
  load("confidence_attention.RData")
  print("loading my.data")
}

####################################################################

### PRE-ALLOCATION
# load participants' IDs
pID <- unique(my.data$participant)

# names of columns
names <-c("Invalid", "Incon", "Valid")

# meta d'
m.meta <- matrix(NA, ncol = 3, nrow = length(pID))
colnames(m.meta)<-names

# d' 
m.d <- matrix(NA, ncol = 3, nrow = length(pID))
colnames(m.d)<-names

# graphs
fig.p <- list()

#################################################################

### CALCULALTE META-D' and D':

SI<- c(-1,0,1)
for (iSI in 1:3){
  run.v <- c(1:length(pID))
  for (iparticipant in run.v){
    f.data <- 
      filter(my.data,(!is.na(zConf)))%>%
      filter(social==SI[iSI]&participant==pID[iparticipant])
    model <-DataMetaD(f.data)%>%
      FitMetaD()
    m.meta[iparticipant,iSI] <- mean(as.numeric(model$meta_d))
    m.d[iparticipant,iSI] <- mean(as.numeric(model$d1))
  }
}


### CONVERT THE MATRIX INTO A DATA FRAME ANd CALCULATE D'DIFF

#convert matrices into data.frames (for ggplot, and easier handling)
m.meta <- data.frame(m.meta)
m.d <- data.frame(m.d)

# create a new matrix with metacognitive efficiency (meta-d' - d')
m.diff <- m.meta - m.d

# add a column with participant info
m.meta$participant<-pID
m.d$participant<-pID
m.diff$participant<-pID

### CREATE THE PLOTS

# PLOT by participant
# change the structure of the d.frame so that I can plot multiple columns
m.meta.p<-melt(m.meta,id.vars="participant")
m.d.p <-melt(m.d,id.vars="participant")
m.diff.p <-melt(m.diff,id.vars="participant")

# META-D'
p.meta = ggplot(aes(x=variable, y=value, fill=variable),data=m.meta.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("meta-d") + 
  scale_fill_manual(values=c("#F5A503","#56D9CD", "#3AA1BF")) +
  geom_abline(intercept = 0, slope = 0)

f.meta <- group_by(m.meta.p,participant) %>%
  do(meta = p.meta %+% ., 
     meta_F = p.meta + facet_wrap(~ participant))

# D'
p.d = ggplot(aes(x=variable, y=value, fill=variable),data=m.d.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("dPrime") + 
  scale_fill_manual(values=c("#F5A503","#56D9CD", "#3AA1BF")) +
  geom_abline(intercept = 0, slope = 0)

f.d <- group_by(m.d.p,participant) %>%
  do(d = p.d %+% .,
     d_F = p.d + facet_wrap(~ participant))

# D'-DIFF - metacognitive efficiency (meta-d' - d')
p.diff = ggplot(aes(x=variable, y=value, fill=variable),data=m.diff.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("metaD-dPrime")+ 
  scale_fill_manual(values=c("#F5A503","#56D9CD", "#3AA1BF")) +
  geom_abline(intercept = 0, slope = 0)

f.diff <- group_by(m.diff.p,participant) %>%
  do(diff = p.diff %+% ., 
     diff_F = p.diff + facet_wrap(~ participant))

### PUT ALL PLOTS INTO A SINGLE TABLE

# create a list with all data.frames to be merged
f.all <- list(f.meta,f.d,f.diff)

# use reduce to call function merge (merges two data.frames)
meta_plots <- Reduce(function(...) merge(...,by = "participant", all = TRUE), f.all)

# save it
save(meta_plots,file='meta_plots.RData')

### SAVE THE DATA FILES
m.meta$type<-"meta"
m.d$type<-"dprime"
m.diff$type<-"diff"

m.combined <-rbind(m.meta,m.d,m.diff)

save(m.combined,file='meta_values.RData') 

# Put meta and dPrime together 

#for(i in 1:length(unique(m.meta.p$participant))){
  # D', metaD' and D'diff in one graph
  #fig.p[[i]]<- grid.arrange(fig.meta.p$plots[[i]],fig.d.p$plots[[i]], fig.diff.p$plots[[i]],ncol=1, nrow =3, top=as.character(unique(m.meta.p$participant)[i]))

  # D', metaD' and D'diff in one graph
 # fig.p[[i]]<- grid.arrange(fig.meta.p$plots[[i]],fig.d.p$plots[[i]],ncol=1, nrow =2, top=as.character(unique(m.meta.p$participant)[i]))
#}

# Plot D' and metaD' together by participant
#grid.arrange(fig.p[[1]],fig.p[[2]],fig.p[[3]],fig.p[[4]],fig.p[[5]],fig.p[[6]],fig.p[[7]], ncol=4, nrow=2)

