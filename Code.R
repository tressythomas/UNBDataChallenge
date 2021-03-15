options(pen=999)
library(survey)
library(rpms)
library(jtools)
library(dplyr)
library(ggplot2)
library(data.table)


raw=read.csv("C:\\Users\\Tress\\OneDrive\\Desktop\\Learn\\Data Challenge\\Rawds.csv",header=TRUE,sep=',',na.strings=c("","N/A"," ","NA",'NULL'))
head(raw)
sum(is.na(raw))





#Overview 

proportions <- function(check_val='Infosources'){
  print(check_val)
  switch(check_val,
         Infosources = {
           analysis_data_infosource=raw %>%
             select(pers_wgt,fc_05a,	fc_05b,	fc_05c,	fc_05d,	fc_05e,	fc_05f,	fc_05g,	fc_05h,	fc_05i,	fc_05j,	fc_05k,	fc_05l) 
           col_names=c("pers_wgt",
                       "Social media posts from users/influencers",
                       "Social media posts from news orgs, magazines",
                       "Online newspapers or news sites",
                       "Online magazine on current events",
                       "Online forums",
                       "Online encyclopedia or repository",
                       "Blogs",
                       "Podcasts",
                       "Online video sharing platforms",
                       "Email from a friend or family",
                       "Other",
                       "Did not use Internet to find info on COVID-19")
           rel_fields=c(1)
           
         },
         Accuracy_check = {
           analysis_data_infosource=raw %>%
             select(pers_wgt,fc_15a,	fc_15b,	fc_15c,	fc_15d,	fc_15e,	fc_15f,	fc_15g,	fc_15h)
           col_names=c("pers_wgt",
                       "Searched author/source for credibility",
                       "Consulted other sources",
                       "Clicked on the link to read the entire news article",
                       "Verified URL for credibility",
                       "Verified the date of the information",
                       "Read comments to see discussion on the topic/source",
                       "Consulted friends, family, online network",
                       "Other"
           )
           rel_fields=c(1)
         },
         Precautions = {
           analysis_data_infosource=raw %>%
             select(pers_wgt,bh_20a,	bh_20b,	bh_20c,	bh_20d,	bh_20e,	bh_20f,	bh_20g,	bh_20h,	bh_20i,	bh_20j,	bh_20k,	bh_20l,	bh_20m,	bh_20n,	bh_20o)
           col_names=c("pers_wgt",
                       "Stocked up on essentials",
                       "Filled prescriptions",
                       "Made a plan for sick hhld members",
                       "Made a plan other non-hhld memb",
                       "Made a plan communicate",
                       "Avoided leaving the house",
                       "Used physical distancing in public",
                       "Avoided crowds and large gathering",
                       "Washed your hands more regularly",
                       "Avoided touching your face",
                       "Cancelled travel",
                       "Worked from home",
                       "Wore mask/other p.p.e",
                       "Other",
                       "None"
                       
           )
           rel_fields=c(1)
         },
         Concern = {
           analysis_data_infosource=raw %>%
             select(pers_wgt,bh_55a,	bh_55b,	bh_55c,	bh_55d,	bh_55e,	bh_55f,	bh_55g,	bh_55h,	bh_55i,	bh_55j,	bh_55k,	bh_55l)
           col_names=c("pers_wgt",
                       "My own health",
                       "Member of household’s health",
                       "Vulnerable people’s health",
                       "Canadian population’s health",
                       "World population’s health",
                       "Overloading the health system",
                       "Civil disorder",
                       "Maintaining social ties",
                       "Ability to support in crisis",
                       "Ability to support post-crisis",
                       "Family stress from confinement",
                       "Violence in the home")
           rel_fields=c(2,3,4)
         },
         Activities= {
           analysis_data_infosource=raw %>%
             select(pers_wgt,bh_35a,	bh_35b,	bh_35c,	bh_35d,	bh_35e)
           col_names=c("pers_wgt",
                       "Communication with friends and family",
                       "Meditation",
                       "Exercise outdoors",
                       "Exercise indoors",
                       "Changing my food choices")
           rel_fields=c(1,2,3)
         },
         Habits = {
           analysis_data_infosource=raw %>%
             select(pers_wgt,bh_40a,bh_40b,bh_40c,	bh_40d,	bh_40e,	bh_40f,	bh_40g,	bh_40h)
           col_names=c("pers_wgt",
                       "Consuming alcohol",
                       "Using tobacco products",
                       "Consuming cannabis",
                       "Eating junk food or sweets",
                       "Watching television",
                       "Spending time on the internet",
                       "Playing video games",
                       "Playing board games")
           rel_fields=c(1)
         },
         Lastweek= {
           analysis_data_infosource=raw %>%
             select(pers_wgt,bh_60a,bh_60b,	bh_60c)
           col_names=c("pers_wgt",
                       "Went shopping at the grocery store or drugstore",
                       "Used delivery service for groceries or drugstore",
                       "Used a food delivery service for prepared food"
           )
           rel_fields=c(1,2,3)
         }
         
         
  )
  colnames(analysis_data_infosource)=col_names
  tot_pers_wgt=sum(analysis_data_infosource$pers_wgt)
  prop=apply(analysis_data_infosource,2,function(x) round(100*sum(analysis_data_infosource$pers_wgt[x%in%rel_fields])/tot_pers_wgt,1))
  #n1=apply(analysis_data_infosource,2,function(x) nrow(analysis_data_infosource[x==1,]))
  #nall=nrow(analysis_data_infosource)
  print(prop)
  proportion=t(t(prop))
  tmp=data.frame(proportion,Indicator=rownames(proportion),row.names=NULL)[-1,] 
  tmp$Indicator=factor(tmp$Indicator,levels=tmp$Indicator)
  tmp=tmp%>% arrange(desc(proportion))
  print(tmp)
  ggplot(data=tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
    geom_bar(stat="identity")+
    scale_y_discrete(limits = rev(levels(tmp$Indicator)))+
    scale_x_continuous(breaks=seq(0,100,10))+
    geom_bar(stat="identity")+
    ggtitle(check_val) +
    ylab(check_val)+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
  
}


proportions('Infosources')
proportions('Accuracy_check')
proportions('Precautions')
proportions('Concern')
proportions('Activities')
proportions('Habits')
proportions('Lastweek')

#Analysis on the use of information source and precautions taken.
 
analysis_prec_info=raw %>%
  select(pumfid,
         pers_wgt,
         bh_05c, #main info source
         bh_20f, #Avoided leaving the house
         bh_20g, #Used physical distancing in public
         bh_20h, #Avoided crowds and large gathering
         bh_20i, #Washed your hands more regularly
         bh_20j, #Avoided touching your face
         bh_20k, #Cancelled travel
         bh_20l, #Worked from home
         bh_20o, #mask use
         sex,
         agegrp)  



#Avoided leaving the house
temp=analysis_prec_info[,c(1,2,3,4)] %>%
  group_by(bh_05c,bh_20f) %>%
  summarise(perc=sum(pers_wgt))
leave_house_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20f=bh_20f,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>% 
  arrange(bh_20f,percx)
leave_house_stat

plot_leave_house=leave_house_stat[leave_house_stat$bh_05c<13 & leave_house_stat$bh_20f==1, ]%>%arrange(desc(percx))
plot_leave_house$bh_05c=factor(plot_leave_house$bh_05c, levels =plot_leave_house$bh_05c)
ggplot(data=plot_leave_house,aes(y=bh_05c,x=as.numeric(substr(percx,1,3)),fill=as.factor(bh_20f)))+
  scale_fill_manual(values=c("#E69F00","#999999",  "#56B4E9"))+
  scale_y_discrete(limits = rev(levels(plot_leave_house$bh_05c)),
                   breaks=y_axis_breaks,
                   labels=y_axis_labels)+
  scale_x_continuous(breaks=seq(0,100,10))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ggtitle("Main info source and avoided leaving house ") +
  ylab("Info source")+xlab("% of respondants avoided leaving house ") + theme(legend.position="none") +theme_classic()


summary(analysis_prec_info)
table(analysis_prec_info$bh_05c)
table(analysis_prec_info$bh_20f)
analysis_prec_info$lh_ind=cut(analysis_prec_info$bh_20f,c(0,1,Inf), c(1,0))
analysis_prec_info$pd_ind=cut(analysis_prec_info$bh_20g,c(0,1,Inf), c(1,0))
analysis_prec_info$ac_ind=cut(analysis_prec_info$bh_20h,c(0,1,Inf), c(1,0))
analysis_prec_info$wh_ind=cut(analysis_prec_info$bh_20i,c(0,1,Inf), c(1,0))
analysis_prec_info$tf_ind=cut(analysis_prec_info$bh_20j,c(0,1,Inf), c(1,0))
analysis_prec_info$ct_ind=cut(analysis_prec_info$bh_20k,c(0,1,Inf), c(1,0))
analysis_prec_info$wfh_ind=cut(analysis_prec_info$bh_20l,c(0,1,Inf), c(1,0))
analysis_prec_info$wm_ind=cut(analysis_prec_info$bh_20o,c(0,1,Inf), c(1,0))
svy_data_all = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=analysis_prec_info)
svy_data=subset(svy_data_all,bh_05c <13)
svychisq(~bh_05c+wm_ind,design =svy_data,statistic="adjWald")

main_source=svymean(~factor(bh_05c),design =svy_data)
par(las=2) # make label text perpendicular to axis
par(mar=c(5,15,4,2)) # increase y-axis margin.
barplot(main_source,beside=TRUE,legend.text = F, horiz=T,width=c(0.05),las=2,
        #args.legend = list(x = "topright", bty = "n", inset=c(-0.1, -.1)),
        col=c('darkblue','pink'),
        xlab="Proportion",xlim=c(0,1),names.arg=y_axis_labels,cex.names=0.8)

#Sex and use of main info source
ms_sex=svyby(~factor(sex),~factor(bh_05c),design =svy_data, na = TRUE, svymean)
barplot(ms_sex,beside=TRUE,legend.text = F, horiz=TRUE,width=c(0.05),las=2,
        #args.legend = list(x = "topright", bty = "n", inset=c(-0.1, -.1)),
        col=c('darkblue','pink'),
        xlab="Proportion",xlim=c(0,1),names.arg=y_axis_labels,cex.names=0.8)


svytable(~lh_ind+bh_05c,design =svy_data)
leavehome_mean=svyby(~lh_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(leavehome_mean)
phydist_mean=svyby(~pd_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(phydist_mean)
avdcrwd_mean=svyby(~ac_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(avdcrwd_mean)
washands_mean=svyby(~wh_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(washands_mean)
touchface_mean=svyby(~wh_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(touchface_mean)
cantrvl_mean=svyby(~ct_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(cantrvl_mean)
wfh_mean=svyby(~wfh_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(wfh_mean)
mask_mean=svyby(~wm_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
estimate_table(mask_mean)

estimate_table <-function(survey_mean) {
  p_mean=survey_mean
  p_estimates=data.frame(confint(p_mean)[c(1:12),])
  rownames(p_estimates)=y_axis_labels
  p_estimates$Mean=p_mean[,2]
  print(p_estimates[order(p_estimates$Mean,decreasing = TRUE),])
  
}






svy_data_all = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=analysis_prec_info)
svy_data=subset(svy_data_all,bh_05c <13)

p_mean=svyby(~lh_ind,~bh_05c,design =svy_data, na = TRUE, svymean)
p_estimates=data.frame(confint(p_mean)[c(1:12),])
rownames(p_estimates)=y_axis_labels
p_estimates$Mean=p_mean$lh_ind1
p_estimates[order(p_estimates$Mean,decreasing = TRUE),]



#Used physical distancing in public
temp=analysis_prec_info[,c(1,2,3,5)] %>%
  group_by(bh_05c,bh_20g) %>%
  summarise(perc=sum(pers_wgt))
phy_dist_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20g=bh_20g,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>%
  arrange(bh_20g,percx)
phy_dist_stat

plot_phy_dist=phy_dist_stat[phy_dist_stat$bh_05c<13 & phy_dist_stat$bh_20g==1, ]%>%arrange(desc(percx))
plot_phy_dist$bh_05c=factor(plot_phy_dist$bh_05c, levels =plot_phy_dist$bh_05c)
ggplot(data=plot_phy_dist,aes(y=bh_05c,x=as.numeric(substr(percx,1,3)),fill=as.factor(bh_20g)))+
  scale_fill_manual(values=c("#E69F00","#999999",  "#56B4E9"))+
  scale_y_discrete(limits = rev(levels(plot_phy_dist$bh_05c)),
                   breaks=y_axis_breaks,
                   labels=y_axis_labels)+
  scale_x_continuous(breaks=seq(0,100,10))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ggtitle("Main info source and physical distancing ") +
  ylab("Info source")+xlab("% of respondants adhered to physical distancing ") + theme(legend.position="none") +theme_classic()

#Avoided crowds and large gathering
temp=analysis_prec_info[,c(1,2,3,6)] %>%
  group_by(bh_05c,bh_20h) %>%
  summarise(perc=sum(pers_wgt))
avoid_crowds_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20h=bh_20h,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>%
  arrange(bh_20h,percx)

plot_avoid_crowds=avoid_crowds_stat[avoid_crowds_stat$bh_05c<13 & avoid_crowds_stat$bh_20h==1, ]%>%arrange(desc(percx))
plot_avoid_crowds$bh_05c=factor(plot_avoid_crowds$bh_05c, levels =plot_avoid_crowds$bh_05c)
ggplot(data=plot_avoid_crowds,aes(y=bh_05c,x=as.numeric(substr(percx,1,3)),fill=as.factor(bh_20h)))+
  scale_fill_manual(values=c("#E69F00","#999999",  "#56B4E9"))+
  scale_y_discrete(limits = rev(levels(plot_avoid_crowds$bh_05c)),
                   breaks=y_axis_breaks,
                   labels=y_axis_labels)+
  scale_x_continuous(breaks=seq(0,100,10))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ggtitle("Main info source and avoided crowds ") +
  ylab("Info source")+xlab("% of respondants avoided crowds ") + theme(legend.position="none") +theme_classic()

#Washed your hands more regularly
temp=analysis_prec_info[,c(1,2,3,7)] %>%
  group_by(bh_05c,bh_20i) %>%
  summarise(perc=sum(pers_wgt))
washed_hands_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20i=bh_20i,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>%
  arrange(bh_20i,percx)

plot_washed_hands=washed_hands_stat[washed_hands_stat$bh_05c<13 & washed_hands_stat$bh_20i==1, ]%>%arrange(desc(percx))
plot_washed_hands$bh_05c=factor(plot_washed_hands$bh_05c, levels =plot_washed_hands$bh_05c)
ggplot(data=plot_washed_hands,aes(y=bh_05c,x=as.numeric(substr(percx,1,3)),fill=as.factor(bh_20i)))+
  scale_fill_manual(values=c("#E69F00","#999999",  "#56B4E9"))+
  scale_y_discrete(limits = rev(levels(plot_washed_hands$bh_05c)),
                   breaks=y_axis_breaks,
                   labels=y_axis_labels)+
  scale_x_continuous(breaks=seq(0,100,10))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ggtitle("Main info source and precaution taken to wash hands ") +
  ylab("Info source")+xlab("% of respondants that washed hands ") + theme(legend.position="none") +theme_classic()

#Avoided touching your face
temp=analysis_prec_info[,c(1,2,3,8)] %>%
  group_by(bh_05c,bh_20j) %>%
  summarise(perc=sum(pers_wgt))
touch_face_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20j=bh_20j,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>%
  arrange(bh_20j,percx)
touch_face_stat


#Cancelled travel
temp=analysis_prec_info[,c(1,2,3,9)] %>%
  group_by(bh_05c,bh_20k) %>%
  summarise(perc=sum(pers_wgt))
cancelled_travel_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20k=bh_20k,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>%
  arrange(bh_20k,percx)
cancelled_travel_stat

plot_cancelled_travel=cancelled_travel_stat[cancelled_travel_stat$bh_05c<13 & cancelled_travel_stat$bh_20k==1, ]%>%arrange(desc(percx))
plot_cancelled_travel$bh_05c=factor(plot_cancelled_travel$bh_05c, levels =plot_cancelled_travel$bh_05c)
ggplot(data=plot_cancelled_travel,aes(y=bh_05c,x=as.numeric(substr(percx,1,3)),fill=as.factor(bh_20k)))+
  scale_fill_manual(values=c("#E69F00","#999999",  "#56B4E9"))+
  scale_y_discrete(limits = rev(levels(plot_cancelled_travel$bh_05c)),
                   breaks=y_axis_breaks,
                   labels=y_axis_labels)+
  scale_x_continuous(breaks=seq(0,100,10))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ggtitle("Main info source and cancelled travel ") +
  ylab("Info source")+xlab("% of respondants that Cancelled travel ") + theme(legend.position="none") +theme_classic()

#Worked from home
temp=analysis_prec_info[,c(1,2,3,10)] %>%
  group_by(bh_05c,bh_20l) %>%
  summarise(perc=sum(pers_wgt))
wfh_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20l=bh_20l,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>%
  arrange(bh_20l,percx)
wfh_stat

plot_wfh_stat=wfh_stat[wfh_stat$bh_05c<13 & wfh_stat$bh_20l==1, ]%>%arrange(desc(percx))
plot_wfh_stat$bh_05c=factor(plot_wfh_stat$bh_05c, levels =plot_wfh_stat$bh_05c)
ggplot(data=plot_wfh_stat,aes(y=bh_05c,x=as.numeric(substr(percx,1,3)),fill=as.factor(bh_20l)))+
  scale_fill_manual(values=c("#E69F00","#999999",  "#56B4E9"))+
  scale_y_discrete(limits = rev(levels(plot_wfh_stat$bh_05c)),
                   breaks=y_axis_breaks,
                   labels=y_axis_labels)+
  scale_x_continuous(breaks=seq(0,100,10))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ggtitle("Main info source and Work from home ") +
  ylab("Info source")+xlab("% of respondants that Work from home ") + theme(legend.position="none") +theme_classic()

#Mask use
temp=analysis_prec_info[,c(1,2,3,11)] %>%
  group_by(bh_05c,bh_20o) %>%
  summarise(perc=sum(pers_wgt))
mask_use_stat  = temp%>%
  group_by(bh_05c)%>%
  summarise(bh_05c=bh_05c,bh_20o=bh_20o,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))%>%
  arrange(bh_20o,desc(percx))

mask_use_stat #People who use federal sources as main information source are more likely to wear a mask
y_axis_breaks=c("1","2","3","4","5","6","7","8","9","10","11","12")
y_axis_labels=c("News outlets","Federal health agency",
                "Provincial or territorial health agency",
                "Municipal health agency ",
                "Federal daily announcements",
                "Provincial daily announcements",
                "Social media",
                "Family, friends or colleagues",
                "Health professionals",
                "Place of employment",
                "Other",
                "I do not look for information about this")


plot_mask=mask_use_stat[mask_use_stat$bh_05c<13 & mask_use_stat$bh_20o==1,]%>%arrange(desc(percx))
plot_mask$bh_05c=factor(plot_mask$bh_05c, levels =plot_mask$bh_05c)
ggplot(data=plot_mask,aes(y=bh_05c,x=as.numeric(substr(percx,1,3)),fill=as.factor(bh_20o)))+
  scale_fill_manual(values=c("#E69F00","#999999",  "#56B4E9"))+
  scale_y_discrete(limits = rev(levels(plot_mask$bh_05c)),
                   breaks=y_axis_breaks,
                   labels=y_axis_labels)+
  scale_x_continuous(breaks=seq(0,100,10))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ggtitle("Main info source and use of mask") +
  ylab("Info source")+xlab("% of respondants that used mask") +theme_classic()
  #theme(axis.text.x = element_text(angle=45))

res.aov <- aov(as.numeric(substr(percx,1,3)) ~ bh_05c, data = mask_use_stat)
summary(res.aov)
res.aov <- aov(as.numeric(substr(percx,1,3)) ~ bh_05c, data = leave_house_stat)
summary(res.aov)
res.aov <- aov(as.numeric(substr(percx,1,3)) ~ bh_05c, data = cancelled_travel_stat)
summary(res.aov)
res.aov <- aov(as.numeric(substr(percx,1,3)) ~ bh_05c, data = touch_face_stat)
summary(res.aov)
res.aov <- aov(as.numeric(substr(percx,1,3)) ~ bh_05c, data = washed_hands_stat)
summary(res.aov)
res.aov <- aov(as.numeric(substr(percx,1,3)) ~ bh_05c, data = avoid_crowds_stat)
summary(res.aov)
res.aov <- aov(as.numeric(substr(percx,1,3)) ~ bh_05c, data = phy_dist_stat)
summary(res.aov)


#...accuracy check
analysis_accuracy=raw %>%
  select(pumfid,
         pers_wgt,
         agegrp,
         fc_10)

summary(analysis_accuracy)
#Filter records that has a valid employment status. Take sub population-Ignore  records with 'Not stated' code 9
table(analysis_accuracy$fc_10)
#Create Employment indicator
analysis_accuracy$acc_ind=cut(analysis_accuracy$fc_10,c(0,3,Inf), c(1,0))

svy_data_all = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=analysis_accuracy)
svytable(~acc_ind,design =svy_data_all)
total_acc=svymean(~acc_ind,design =svy_data_all)
names(total_emp)=c("Employed", "Unemployed", "Not Available")

barplot(total_acc,beside=TRUE,legend.text = F, 
        #args.legend = list(x = "topright", bty = "n", inset=c(-0.1, -.1)),
        xlab="Employment Status", col=c("darkblue","red","white"),
        ylab="Proportion",ylim=c(0,.8))
#names.arg=c("Employed", "Unemployed", "Not Available"))
confint(total_emp)
###########################################################################
Trustedsource=raw %>%
    select(pumfid,pers_wgt,fc_05a,	fc_05b,	fc_05c,	fc_05d,	fc_05e,	fc_05f,	fc_05g,	fc_05h,	fc_05i,	fc_05j,	fc_05k,	fc_05l,fc_20g,fc_10) 
  col_names=c("pers_wgt",
              "Social media posts from users/influencers",
              "Social media posts from news orgs, magazines",
              "Online newspapers or news sites",
              "Online magazine on current events",
              "Online forums",
              "Online encyclopedia or repository",
              "Blogs",
              "Podcasts",
              "Online video sharing platforms",
              "Email from a friend or family",
              "Other",
              "Did not use Internet to find info on COVID-19")

Trustedsource$trust_ind=cut(Trustedsource$fc_20g,c(0,1,Inf), c(1,0))
Trustedsource$nock_ind=cut(Trustedsource$fc_10,c(0,3,5,Inf), c(0,1,0))
  
svy_data_all = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=Trustedsource)
svy_data=subset(svy_data_all,nock_ind==1&fc_05a==1)
r1=svytable(~factor(fc_05a)+trust_ind,design =svy_data)
 

svy_data=subset(svy_data_all,nock_ind==1&fc_05b==1)
r2=svytable(~factor(fc_05b)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05c==1)
r3=svytable(~factor(fc_05c)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05d==1)
r4=svytable(~factor(fc_05d)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05e==1)
r5=svytable(~factor(fc_05e)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05f==1)
r6=svytable(~factor(fc_05f)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05g==1)
r7=svytable(~factor(fc_05g)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05h==1)
r8=svytable(~factor(fc_05h)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05i==1)
r9=svytable(~factor(fc_05i)+trust_ind,design =svy_data)


svy_data=subset(svy_data_all,nock_ind==1&fc_05j==1)
r10=svytable(~factor(fc_05j)+trust_ind,design =svy_data)

svy_data=subset(svy_data_all,nock_ind==1&fc_05k==1)
r11=svytable(~factor(fc_05k)+trust_ind,design =svy_data)



ts_cmpr=ts_cmpr[order(ts_cmpr$trust_pc,decreasing = TRUE),]
ts_cmpr$Source=factor(ts_cmpr$Source, levels =ts_cmpr$Source)
ggplot(data=ts_cmpr,aes(y=Source,x=round(trust_pc*100,3),fill=-trust_pc))+
  scale_y_discrete(limits = rev(levels(ts_cmpr$Source)))+
                     scale_x_continuous(breaks=seq(0,100,10))+
                     geom_bar(stat="identity",show.legend = FALSE,col="blue")+
  ggtitle("Trustworthy sources ") +
  ylab("Info source")+xlab("% of respondants trusted ") + theme(legend.position="none") +theme_classic()
  

#1.	What is the impact of education level on keeping the Canadians employed during pandemic?

#Employment status based on SEC

analysis_employment=raw %>%
  select(pumfid,
         pers_wgt,
         agegrp,
         sex,
         rururb,
         immigrnc,
         marstatc,
         hhldsizc,
         peduc_lc,
         pempstc)

summary(analysis_employment)
#Filter records that has a valid employment status. Take sub population-Ignore  records with 'Not stated' code 9
analysis_employment_data=analysis_employment 
table(analysis_employment_data$pempstc)
#Create Employment indicator
analysis_employment_data$emp_ind=cut(analysis_employment_data$pempstc,c(0,3,4,Inf), c(1,0,9))

svy_data_all = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=analysis_employment_data)
svytable(~emp_ind,design =svy_data_all)
total_emp=svymean(~emp_ind,design =svy_data_all)
names(total_emp)=c("Employed", "Unemployed", "Not Available")

barplot(total_emp,beside=TRUE,legend.text = F, 
        #args.legend = list(x = "topright", bty = "n", inset=c(-0.1, -.1)),
        xlab="Employment Status", col=c("darkblue","red","white"),
        ylab="Proportion",ylim=c(0,.8))
        #names.arg=c("Employed", "Unemployed", "Not Available"))
confint(total_emp)

total_edu=svymean(~factor(peduc_lc),design =svy_data_all)
names(total_edu)=c("Less than HS", "HS diploma","Cert/Dip","Some College","Uni Cert/Dip",
                   "Bach. deg","above Bach.")

barplot(total_edu,beside=TRUE,legend.text = F, 
        #args.legend = list(x = "topright", bty = "n", inset=c(-0.1, -.1)),
        xlab="Employment Status",col=c("darkblue"),
        ylab="Proportion",ylim=c(0,1))
#names.arg=c("Employed", "Unemployed", "Not Available"))
confint(total_edu)

confint(total_edu)


svy_data = subset(svy_data_all ,emp_ind%in%c(1,0))
summary(svy_data)

# Bivariate analysis:0. Education and employment 
svytable(~peduc_lc+emp_ind,design =svy_data)
svychisq(~peduc_lc+emp_ind,design =svy_data,statistic="adjWald") #p-value = 2.186e-06 Dependent
barplt<-svyby(~emp_ind,~peduc_lc,design =svy_data, na = TRUE, svymean)
confint(barplt)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.01, -.1)),
        xlab="Education Level", col=c("darkblue","red","white"),ylim=c(0,1),
        names.arg=c("Less than HS", "HS diploma","Cer/Dip","Some College","Uni Cert/Dip",
                    "Bach degree","above Bach"))
EMP_EDU <- (svyglm(emp_ind~factor(peduc_lc), family=quasibinomial,design=svy_data))
summary(EMP_EDU)
psrsq(EMP_EDU,method = c("Nagelkerke"))
summ(EMP_EDU)

age=svymean(~factor(agegrp),design =svy_data_all)
barplot(age,beside=TRUE,legend.text = F, 
        #args.legend = list(x = "topright", bty = "n", inset=c(-0.1, -.1)),
        xlab="Employment Status", 
        ylab="Proportion",ylim=c(0,1))

# Bivariate analysis:1. Age group and employment
svytable(~agegrp+emp_ind,design =svy_data)
svychisq(~agegrp+emp_ind,design =svy_data,statistic="adjWald") #p-value < 2.2e-16 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~emp_ind,~agegrp,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Age Group", col=c("darkblue","red","white"),
        names.arg=c("15 to 24yrs", "25 to 34 yrs", "35 to 44 yrs",'45 to 54 yrs','55 to 64 yrs','65 to 74 yrs','75+'))
 


# Bivariate analysis:2. Sex and employment
svytable(~sex+emp_ind,design =svy_data)
svychisq(~sex+emp_ind,design =svy_data,statistic="adjWald") #p-value = 0.0005213
barplt<-svyby(~emp_ind,~sex,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Sex", col=c("darkblue","red","white"),
        names.arg=c("Male", "Female"))


# Bivariate analysis:3. Rural and Urban 
svytable(~rururb+emp_ind,design =svy_data)
svychisq(~rururb+emp_ind,design =svy_data,statistic="adjWald") #p-value = 0.4335 . Not dependent
barplt<-svyby(~emp_ind,~rururb,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Rural and Urban", col=c("darkblue","red","white"),
        names.arg=c("Rural", "Urban"))

# Bivariate analysis:4. Immigration status and employment 
svytable(~immigrnc+emp_ind,design =svy_data)
svychisq(~immigrnc+emp_ind,design =svy_data,statistic="adjWald") #p-value =  0.7757 . Not dependent
barplt<-svyby(~emp_ind,~immigrnc,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Immigration status", col=c("darkblue","red","white"),
        names.arg=c("Canadian", "Immigrant"))

# Bivariate analysis:5. Householdsize and employment 
svytable(~hhldsizc+emp_ind,design =svy_data)
svychisq(~hhldsizc+emp_ind,design =svy_data,statistic="adjWald") #p-value =  0.1694 . Not dependent
barplt<-svyby(~emp_ind,~hhldsizc,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Householdsize", col=c("darkblue","red","white"),
        names.arg=c("1", "2","3","4","5 +"))

# Bivariate analysis:6. Martial Status and employment 
svytable(~marstatc+emp_ind,design =svy_data)
svychisq(~marstatc+emp_ind,design =svy_data,statistic="adjWald") #p-value = 4.603e-08 . Dependent
barplt<-svyby(~emp_ind,~marstatc,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.01, -.1)),
        xlab="Martial status", col=c("darkblue","red","white"),
        names.arg=c("Married", "Living common-law","Widowed/Separated/Divorced","Single, never married"))

# Bivariate analysis:7. Education and employment 
svytable(~peduc_lc+emp_ind,design =svy_data)
svychisq(~peduc_lc+emp_ind,design =svy_data,statistic="adjWald") #p-value = 2.186e-06 Dependent
barplt<-svyby(~emp_ind,~peduc_lc,design =svy_data, na = TRUE, svymean)
confint(barplt)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.01, -.1)),
        xlab="Education Level", col=c("darkblue","red","white"),
        names.arg=c("Less than HS", "HS diploma","Cer/Dip","Some College","Uni Cert/Dip",
                    "Bach degree","above Bach"))
EMP_EDU <- (svyglm(factor(emp_ind)~peduc_lc, family=quasibinomial, design=svy_data))
summary(EMP_EDU)
psrsq(EMP_EDU,method = c("Nagelkerke"))
summary(EMP_EDU)
 
logit1 <- (svyglm(factor(emp_ind)~factor(agegrp)+
                    factor(sex)+
                    #factor(rururb)+
                    factor(immigrnc)+
                    factor(marstatc)+
                    #factor(hhldsizc)+
                    factor(peduc_lc), family=quasibinomial, design=svy_data))
summary(logit1)
psrsq(logit1,method = c("Nagelkerke")) 
summ(logit1)

logit2 <- (svyglm(factor(emp_ind)~factor(agegrp)*factor(peduc_lc)+
                    factor(sex)+
                    factor(marstatc)*factor(hhldsizc)+
                    factor(immigrnc)
                    #factor(rururb)*
                    
                    #factor(hhldsizc)+
                    , family=quasibinomial, design=svy_data))
summary(logit2)
psrsq(logit2,method = c("Nagelkerke")) 
summ(logit2)
 

# Mental health based on SEC

analysis_mentalhealth_data=raw %>%
  select(pumfid,
         pers_wgt,
         agegrp,
         sex,
         rururb,
         immigrnc,
         marstatc,
         hhldsizc,
         peduc_lc,
         pempstc,
         mh_30)

summary(analysis_mentalhealth_data)
table(analysis_mentalhealth_data$mh_30)
#Create mental health indicator . Good, Very good, Excellent --> 1
analysis_mentalhealth_data$mh_ind=cut(analysis_mentalhealth_data$mh_30,c(0,3,5,Inf), c(1,0,9))

svy_data_all = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=analysis_mentalhealth_data)
svy_data = subset(svy_data_all ,mh_ind%in%c(1,0))
summary(svy_data)


# Univariate Analysis
mental_health=svymean(~mh_ind,design =svy_data_all)
barplot(mental_health,beside=TRUE,legend.text = F, 
        xlab="Age Group", col=c("orange","darkred","white"),ylim=c(0,1),
        names.arg=c("Excellent~Good",'Fair/Poor','Other'))
confint(mental_health)

# Bivariate analysis:1. Age group and Mental Health
svytable(~agegrp+mh_ind,design =svy_data)
svychisq(~agegrp+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~mh_ind,~agegrp,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Age Group", col=c("orange","darkred","white"),ylim=c(0,1),
        names.arg=c("15 to 24yrs", "25 to 34 yrs", "35 to 44 yrs",'45 to 54 yrs','55 to 64 yrs','65 to 74 yrs','75+'))

mh_age= (svyglm(factor(mh_ind)~factor(agegrp), family=quasibinomial, design=svy_data))

summary(mh_age)
res.aov <- svyranktest(factor(mh_ind)~factor(agegrp),design=svy_data,test=("KruskalWallis"))
res.aov
# Bivariate analysis:2. Sex and Mental Health
svytable(~sex+mh_ind,design =svy_data)
svychisq(~sex+mh_ind,design =svy_data,statistic="adjWald") #p-value = 0.01699
barplt<-svyby(~mh_ind,~sex,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Sex", col=c("orange","darkred","white"),ylim=c(0,1),
        names.arg=c("Male", "Female"))
res.aov <- svyranktest(factor(mh_ind)~factor(sex),design=svy_data,test=("KruskalWallis"))
res.aov <- svyranktest(factor(mh_ind)~factor(sex),design=svy_data,test= c("wilcoxon"))
res.aov

# Bivariate analysis:3. Rural aand Mental Health
svytable(~rururb+mh_ind,design =svy_data)
svychisq(~rururb+mh_ind,design =svy_data,statistic="adjWald") #p-value = 0.3914. Not dependent
barplt<-svyby(~mh_ind,~rururb,design =svy_data, na = TRUE, svymean)
barplot(barplt,legend.text = TRUE, beside=TRUE,
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Rural and Urban", col=c("orange","darkred","white"),
        names.arg=c("Rural", "Urban"))

# Bivariate analysis:4. Immigration status and Mental Health 75% are canadians, 25% are immigrants
svymean(~factor(immigrnc),design =svy_data)
svytable(~immigrnc+mh_ind,design =svy_data)
svychisq(~immigrnc+mh_ind,design =svy_data,statistic="adjWald") #p-value = 0.001227 . dependent
barplt<-svyby(~mh_ind,~immigrnc,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Immigration status", col=c("orange","darkred","white"),
        names.arg=c("Canadian", "Immigrant"))
res.aov <- svyranktest(factor(mh_ind)~factor(immigrnc),design=svy_data,test=("KruskalWallis"))
res.aov
# Bivariate analysis:5. Householdsize and Mental Health
svytable(~hhldsizc+mh_ind,design =svy_data)
svychisq(~hhldsizc+mh_ind,design =svy_data,statistic="adjWald") #p-value = 0.06285 . Not dependent
barplt<-svyby(~mh_ind,~hhldsizc,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Householdsize", col=c("orange","darkred","white"),
        names.arg=c("1", "2","3","4","5 +"))

# Bivariate analysis:6. Martial Status and Mental Health
svytable(~marstatc+mh_ind,design =svy_data)
svychisq(~marstatc+mh_ind,design =svy_data,statistic="adjWald") #p-value = 4.603e-08 . Dependent
barplt<-svyby(~mh_ind,~marstatc,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.01, -.1)),
        xlab="Martial status", col=c("orange","darkred","white"),
        names.arg=c("Married", "Living common-law","Widowed/Separated/Divorced","Single, never married"))

mh_ms <- (svyglm(factor(mh_ind)~factor(marstatc),family=quasibinomial, design=svy_data))
summary(mh_ms)
# Bivariate analysis:7. Education and Mental Health
svymean(~factor(peduc_lc),design =svy_data)
svytable(~peduc_lc+mh_ind,design =svy_data)
svychisq(~peduc_lc+mh_ind,design =svy_data,statistic="adjWald") #p-value = 0.05357 Not Dependent
barplt<-svyby(~mh_ind,~peduc_lc,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.01, -.1)),
        xlab="Education Level", col=c("orange","darkred","white"),
        names.arg=c("Less than HS", "HS diploma","Cer/Dip","Some College","Uni Cert/Dip",
                    "Bach degree","above Bach"))

# Bivariate analysis:8. Mental health and employment
svymean(~factor(pempstc),design =svy_data)
svytable(~pempstc+mh_ind,design =svy_data)
svychisq(~pempstc+mh_ind,design =svy_data,statistic="adjWald") #p-value = 0.8782 Not Dependent
barplt<-svyby(~mh_ind,~pempstc,design =svy_data, na = TRUE, svymean)
confint(barplt)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.01, -.1)),
        xlab="Employment Status", col=c("orange","darkred","white")) 
        #names.arg=c("Less than HS", "HS diploma","Cer/Dip","Some College","Uni Cert/Dip",
                   # "Bach degree","above Bach"))


mhlogit1 <- (svyglm(factor(mh_ind)~factor(agegrp)+
                    factor(sex)+
                    factor(rururb)+
                    factor(immigrnc)+
                    factor(marstatc)+
                    factor(hhldsizc)
                     ,family=quasibinomial, design=svy_data))
summary(mhlogit1)
mhlogit2 <- (svyglm(mh_ind~factor(agegrp)*factor(marstatc)*
                      factor(sex)+factor(immigrnc)
                      , family=quasibinomial, design=svy_data))
summary(mhlogit2)


psrsq(mhlogit1,method = c("Nagelkerke"))
psrsq(mhlogit2,method = c("Nagelkerke"))



# mental health and Habits

mh_habit=raw%>%select(pumfid,mh_30,pers_wgt,bh_40a,bh_40b,bh_40c,	bh_40d,	bh_40e,	bh_40f,	bh_40g,	bh_40h)
col_names=c("pumfid",
            "mental_health",
            "pers_wgt",
            "Consuming_alcohol",
            "Using_tobacco_products",
            "Consuming_cannabis",
            "Eating_junk_food_or_sweets",
            "Watching_television",
            "Spending_time_on_the_internet",
            "Playing_video_games",
            "Playing_board_games")

colnames(mh_habit)=col_names
mh_habit$mh_ind=cut(mh_habit$mental_health,c(0,3,5,Inf), c(1,0,9))
svy_data_all = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=mh_habit)
svy_data = subset(svy_data_all ,mh_ind%in%c(1,0))
summary(svy_data)

svytable(~mh_ind,design =svy_data)
a=svymean(~mh_ind,design =svy_data)
svytotal(~mh_ind,design =svy_data)
confint(a,level = 0.95)


# Bivariate analysis:1. Consuming_alcohol and Mental Health
svytable(~Consuming_alcohol+mh_ind,design =svy_data)
svychisq(~Consuming_alcohol+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~mh_ind,~Consuming_alcohol,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Consuming alcohol") 
         
# Bivariate analysis:2. Using_tobacco_products and Mental Health
svytable(~Using_tobacco_products+mh_ind,design =svy_data)
svychisq(~Using_tobacco_products+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~mh_ind,~Using_tobacco_products,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Consuming alcohol") 

# Bivariate analysis:3. Consuming_cannabis and Mental Health
svytable(~Consuming_cannabis+mh_ind,design =svy_data)
svychisq(~Consuming_cannabis+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~mh_ind,~Consuming_cannabis,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Consuming cannabis") 

# Bivariate analysis:4. Eating_junk_food_or_sweets and Mental Health
svytable(~Eating_junk_food_or_sweets+mh_ind,design =svy_data)
svychisq(~Eating_junk_food_or_sweets+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~mh_ind,~Eating_junk_food_or_sweets,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Eating_junk_food_or_sweets") 

# Bivariate analysis:5. Consuming_alcohol and Mental Health
svytable(~Consuming_alcohol+mh_ind,design =svy_data)
svychisq(~Consuming_alcohol+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~mh_ind,~Consuming_alcohol,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Consuming alcohol") 

# Bivariate analysis:6. Watching_television and Mental Health
svytable(~Watching_television+mh_ind,design =svy_data)
svychisq(~Watching_television+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent

barplt<-svyby(~mh_ind,~Watching_television,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Watching_television") 


# Bivariate analysis:7. Spending_time_on_the_internet and Mental Health
svytable(~Spending_time_on_the_internet+mh_ind,design =svy_data)
svychisq(~Spending_time_on_the_internet+mh_ind,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~mh_ind,~Spending_time_on_the_internet,design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = TRUE, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        xlab="Consuming alcohol") 
##############################################################################################
w_habit=raw%>%select(pumfid,
                     pers_wgt,
                     agegrp,
                     sex,
                     rururb,
                     immigrnc,
                     bh_40a,bh_40b,bh_40c,	bh_40d,	bh_40e,	bh_40f,	bh_40g,	bh_40h)
col_names=c("pumfid",
            "pers_wgt",
            "Agegrp",
            "Sex",
            "Rural/Urban",
            "Immigration Stat",
            "Consuming_alcohol",
            "Using_tobacco_products",
            "Consuming_cannabis",
            "Eating_junk_food_or_sweets",
            "Watching_television",
            "Spending_time_on_the_internet",
            "Playing_video_games",
            "Playing_board_games")

colnames(w_habit)=col_names
svy_data  = svydesign(id=~pumfid, weights=~pers_wgt, survey.lonely.psu = "adjust", data=w_habit)
#svy_data = subset(svy_data_all ,mh_ind%in%c(1,0))
summary(svy_data)

svytable(~Spending_time_on_the_internet+Agegrp,design =svy_data)
svychisq(~Spending_time_on_the_internet+Agegrp,design =svy_data,statistic="adjWald") #p-value <2.047e-11 dependent
#svyhist(~agegrp,design =svy_data)
barplt<-svyby(~factor(Spending_time_on_the_internet),~factor(Agegrp),design =svy_data, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend.text = F, 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.1, -.1)),
        ylab="% Spending_time_on_the_internet") 


##############################################################################################
analysis_data_contact=raw %>%
  select(pumfid,
         pers_wgt,
         agegrp,
         pbh_115)

head(analysis_data_contact)
temp=analysis_data_contact %>%
  group_by(agegrp,pbh_115) %>%
  summarise(perc=sum(pers_wgt))
temp
contact_stat = temp%>%
  group_by(agegrp)%>%
  summarise(pbh_115=pbh_115,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))
contact_stat

analysis_contact_sex=raw %>%
  select(pumfid,
         pers_wgt,
         sex,
         pbh_115)
temp=analysis_contact_sex %>%
  group_by(sex,pbh_115) %>%
  summarise(perc=sum(pers_wgt))
temp
contact_stat_sex = temp%>%
  group_by(sex)%>%
  summarise(pbh_115=pbh_115,sex=sex,perc=perc,percx=paste0(round(perc*100/sum(perc),2),'%'))

contact_stat_sex



