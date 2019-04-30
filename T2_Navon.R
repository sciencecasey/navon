#All Data DF
N_time1all=bind_rows(t1s1N, t1s2N, t1s3N, t1s4N, t1s5N)
N_time1all$Session=replace_na(N_time1all$Session, 1)
N_T2T1_all=bind_rows(N_time1all, t2s1N, t2s2N, t2s3N, t2s4N, t2s5N)
#remove practice block
N_T2T1_all=N_T2T1_all[!N_T2T1_all$Block=="P",]
N_T2T1_all$Block=factor(N_T2T1_all$Block)
levels(N_T2T1_all$Block) #check that it worked
#rename some incorrect subject names
N_T2T1_all$Subject[N_T2T1_all$Subject=="5031"]="50312"
N_T2T1_all$Subject[N_T2T1_all$Subject=="5020"]="50202"
N_T2T1_all$Subject[N_T2T1_all$Subject=="501421"]="50142"
N_T2T1_all$Subject
#Variables as Factors for grouping
F_N_T2T1_all=N_T2T1_all
F_N_T2T1_all$Session=factor(F_N_T2T1_all$Session)
F_N_T2T1_all$Subject=factor(F_N_T2T1_all$Subject)
F_N_T2T1_all$Configuration=factor(F_N_T2T1_all$Configuration)
F_N_T2T1_all$TargetLetter=factor(F_N_T2T1_all$TargetLetter)
F_N_T2T1_all$TargetLocation=factor(F_N_T2T1_all$TargetLocation)
str(F_N_T2T1_all)

#Remove irrelevant variables 
F_N_T2T1_all=subset(F_N_T2T1_all, select= -c(XCoo, YCoo, Block))
#Recode nonresponse as NA for RT and ACC
is.na(F_N_T2T1_all$Stimuli.RT)=F_N_T2T1_all$Stimuli.RT==0
is.na(F_N_T2T1_all$Stimuli.RT) #just as a check
is.na(F_N_T2T1_all$Stimuli.ACC)=is.na(F_N_T2T1_all$Stimuli.RT)
#Recode RT as NA for inaccurate responses
is.na(F_N_T2T1_all$Stimuli.RT)=F_N_T2T1_all$Stimuli.ACC==0
#make switch/nonswitch condition
F_N_T2T1_all$Switch=ifelse(F_N_T2T1_all$Configuration=="GLS" | F_N_T2T1_all$Configuration=="LGS", "switch", "nonswitch")
F_N_T2T1_all$Switch=factor(F_N_T2T1_all$Switch)
#Group for Configuration stats All
Config_T2T1=subset(F_N_T2T1_all, select= c(Subject, Configuration, Session, Stimuli.ACC, Stimuli.RT, Switch))
Config_T2T1=group_by(Config_T2T1, Subject, Session, Switch)
Config_T2T1_stats_bysubject=summarise(Config_T2T1, AvgAcc= mean(Stimuli.ACC, na.rm=TRUE), AvgRT= mean(Stimuli.RT, na.rm=TRUE))
Config_T2T1_simple_stats3=subset(Config_T2T1, select= -Subject)  
Config_T2T1_simple_stats2=group_by(Config_T2T1_simple_stats3, Switch, Session) 
Config_T2T1_simple_stats=summarise(Config_T2T1_simple_stats2, AvgAcc= mean(Stimuli.ACC, na.rm=TRUE), AvgRT= mean(Stimuli.RT, na.rm=TRUE))
rm(Config_T2T1_simple_stats2, Config_T2T1_simple_stats3)

#grouped by subject & config only stats (both time 1 and 2 together)
navon_stats_bysub_bydirection=Config_T2T1 %>%
  group_by(Subject, Switch) %>%
  summarise_at(c("Stimuli.RT", "Stimuli.ACC"), funs(mean, sd), na.rm=TRUE)

#grouped by subject, direction, and session stats
navon_stats_bysub_bydirection_bytime=Config_T2T1 %>%
  group_by(Subject, Session, Switch) %>%
  summarise_at(c("Stimuli.RT", "Stimuli.ACC"), funs(mean, sd), na.rm=TRUE)

#overall time 2 and 1 by direction and Session
navon_stats_bydirection_bysession=Config_T2T1 %>%
  group_by(Switch, Session) %>%
  summarise_at(c("Stimuli.RT", "Stimuli.ACC"), funs(mean, sd), na.rm=TRUE)

#put overall and by subjects into one df
navon_stats_withoverall=bind_rows(navon_stats_bysub_bydirection_bytime, navon_stats_bydirection_bysession) #bysub&direction was a df, bydirection was a tibble
navon_stats_withoverall$Subject=as.numeric(navon_stats_withoverall$Subject)
navon_stats_withoverall$Subject=replace_na(navon_stats_withoverall$Subject, "Overall")
navon_stats_withoverall

#RT by configuration Analysis
navon_stats=navon_stats_bysub_bydirection_bytime
str(navon_stats)
library(plyr)
ddply(navon_stats, ~Switch*Session, summarise, mean.RT=mean(Stimuli.RT_mean), sd.RT=sd(Stimuli.RT_mean))
hist(navon_stats[navon_stats$Switch=="switch" & 
                   navon_stats$Session=="1",]$Stimuli.RT_mean, xlab="Switch, Sess1 RT")
hist(navon_stats[navon_stats$Switch=="nonswitch" & 
                   navon_stats$Session=="1",]$Stimuli.RT_mean, xlab="Nonswitch, Sess1 RT")
hist(navon_stats[navon_stats$Switch=="switch" & 
                   navon_stats$Session=="2",]$Stimuli.RT_mean, xlab="Switch, Sess2 RT")
hist(navon_stats[navon_stats$Switch=="nonswitch" & 
                   navon_stats$Session=="2",]$Stimuli.RT_mean, xlab="Nonswitch, Sess2 RT")
boxplot(Stimuli.RT_mean~Switch*Session, data=navon_stats,
        xlab="Switch+Session", ylab="RT")
with(navon_stats, interaction.plot(Switch, Session, Stimuli.RT_mean))

#2x2 repeated factorial anova
#test normality assumption
shapiro.test(navon_stats[navon_stats$Switch=="switch",]$Stimuli.RT_mean)
shapiro.test(navon_stats[navon_stats$Switch=="nonswitch",]$Stimuli.RT_mean)
shapiro.test(navon_stats[navon_stats$Session=="1",]$Stimuli.RT_mean)
shapiro.test(navon_stats[navon_stats$Session=="2",]$Stimuli.RT_mean)
#all p values insig.
#test residuals
m=aov(Stimuli.RT_mean~Switch*Session+Error(Subject/(Switch*Session)), data=navon_stats)
shapiro.test(residuals(m$Subject))
qqnorm(residuals(m$Subject));qqline(residuals(m$Subject))
shapiro.test(residuals(m$"Subject:Switch"))
qqnorm(residuals(m$"Subject:Switch"));qqline(residuals(m$"Subject:Switch"))
shapiro.test(residuals(m$"Subject:Session"))
qqnorm(residuals(m$"Subject:Session"));qqline(residuals(m$"Subject:Session"))
#all resids are normal

library(ez)
m=ezANOVA(dv=Stimuli.RT_mean, within=c(Switch, Session), wid=Subject, data=navon_stats)
m$MAUCHLY
m$ANOVA

#post hoc t tests on sig main effects of Switch and Session
library(reshape2)
navon_stats_wide=dcast(navon_stats, Subject +Switch~Session, value.var = "Stimuli.RT_mean")
(ttestSwitch=t.test(navon_stats$Stimuli.RT_mean~navon_stats$Switch, data=navon_stats, paired=TRUE))
(ttestSession=t.test(navon_stats$Stimuli.RT_mean~navon_stats$Session, data=navon_stats, paired=TRUE))
p.adjust(c(ttestSession$p.value, ttestSwitch$p.value), method="holm")
#both are highly significant

#LMM RT
str(F_N_T2T1_all)

#add nested variable "Trial #)
length(F_N_T2T1_all$Stimuli.ACC)
length(t2s1N$Stimuli.ACC)
F_N_T2T1_all$Trial=rep(seq(1,128), 10)
F_N_T2T1_all$Trial=factor(F_N_T2T1_all$Trial)
levels(F_N_T2T1_all$Trial)
#exploratory plots
ggpairs(F_N_T2T1_all[,c("Session", "Switch", "Stimuli.ACC", "Stimuli.RT")], na.rm=TRUE)

#mixed effects logistic regression estimate
RTComp1N=lmer(Stimuli.RT~Switch*Session+(Switch*Session|Subject), data=F_N_T2T1_all, 
       control=lmerControl(optimizer = "bobyqa"))
RTComp2N=lmer(Stimuli.RT~Switch*Session+(Session|Subject), data=F_N_T2T1_all)
RTfit1N=lmer(Stimuli.RT~Switch*Session+(1|Subject), data=F_N_T2T1_all)
RTfit2N=lmer(Stimuli.RT~Switch+Session+(1|Subject), data=F_N_T2T1_all)
(RTSummarN=anova(RTComp1N, RTComp2N, RTfit1N, RTfit2N))
emmip(RTComp1N, Switch~Session, CIs=TRUE, ylab="Response Time Prediction")
emmip(RTComp2N, Switch~Session, CIs=TRUE, ylab="Response Time Prediction")
emmip(RTfit1N, Switch~Session, CIs=TRUE, ylab="Response Time Prediction")
emmip(RTfit2N, Switch~Session, CIs=TRUE, ylab="Response Time Prediction")

#ACC by switch Analysis
ddply(navon_stats, ~Switch*Session, summarise, mean.ACC=mean(Stimuli.ACC_mean), sd.ACC=sd(Stimuli.ACC_mean))
hist(navon_stats[navon_stats$Switch=="switch" & 
                   navon_stats$Session=="1",]$Stimuli.ACC_mean, xlab="Switch, Sess1 ACC")
hist(navon_stats[navon_stats$Switch=="nonswitch" & 
                   navon_stats$Session=="1",]$Stimuli.ACC_mean, xlab="Nonswitch, Sess1 ACC")
hist(navon_stats[navon_stats$Switch=="switch" & 
                   navon_stats$Session=="2",]$Stimuli.ACC_mean, xlab="Switch, Sess2 ACC")
hist(navon_stats[navon_stats$Switch=="nonswitch" & 
                   navon_stats$Session=="2",]$Stimuli.ACC_mean, xlab="Nonswitch, Sess2 ACC")
boxplot(Stimuli.ACC_mean~Switch*Session, data=navon_stats,
        xlab="Switch+Session", ylab="ACC")
with(navon_stats, interaction.plot(Switch, Session, Stimuli.ACC_mean))

#2x2 repeated factorial anova
#test normality assumption
shapiro.test(navon_stats[navon_stats$Switch=="switch",]$Stimuli.ACC_mean)
shapiro.test(navon_stats[navon_stats$Switch=="nonswitch",]$Stimuli.ACC_mean)
shapiro.test(navon_stats[navon_stats$Session=="1",]$Stimuli.ACC_mean)
shapiro.test(navon_stats[navon_stats$Session=="2",]$Stimuli.ACC_mean)
#sig p values for switch and nonswitch
#test residuals
m=aov(Stimuli.ACC_mean~Switch*Session+Error(Subject/(Switch*Session)), data=navon_stats)
shapiro.test(residuals(m$Subject))
qqnorm(residuals(m$Subject));qqline(residuals(m$Subject))
shapiro.test(residuals(m$"Subject:Switch"))
qqnorm(residuals(m$"Subject:Switch"));qqline(residuals(m$"Subject:Switch"))
shapiro.test(residuals(m$"Subject:Session"))
qqnorm(residuals(m$"Subject:Session"));qqline(residuals(m$"Subject:Session"))
#subjectSwitch was significant:: not a normal residual dist. 

library(ez)
m=ezANOVA(dv=Stimuli.ACC_mean, within=c(Switch, Session), wid=Subject, data=navon_stats)
m$MAUCHLY
m$ANOVA

#post hoc t tests on sig main effects of Switch and Session
(ttestSwitch=t.test(navon_stats$Stimuli.ACC_mean~navon_stats$Switch, data=navon_stats, paired=TRUE))
(ttestSession=t.test(navon_stats$Stimuli.ACC_mean~navon_stats$Session, data=navon_stats, paired=TRUE))
p.adjust(c(ttestSession$p.value, ttestSwitch$p.value), method="holm")
#neither sig. after adjustment

#LMM ACC
#mixed effects logistic regression estimate
ACCComp1N=lmer(Stimuli.ACC~Switch*Session+(Switch*Session|Subject), data=F_N_T2T1_all, 
              control=lmerControl(optimizer = "bobyqa"))
ACCComp2N=lmer(Stimuli.ACC~Switch*Session+(Session|Subject), data=F_N_T2T1_all)
ACCfit1N=lmer(Stimuli.ACC~Switch*Session+(1|Subject), data=F_N_T2T1_all)
ACCfit2N=lmer(Stimuli.ACC~Switch+Session+(1|Subject), data=F_N_T2T1_all)
(ACCSummarN=anova(ACCComp1N, ACCComp2N, ACCfit1N, ACCfit2N))
emmip(ACCComp1N, Switch~Session, CIs=TRUE, ylab="Accuracy Prediction")
emmip(ACCComp2N, Switch~Session, CIs=TRUE, ylab="Accuracy Prediction")
emmip(ACCfit1N, Switch~Session, CIs=TRUE, ylab="Accuracy Prediction")
emmip(ACCfit2N, Switch~Session, CIs=TRUE, ylab="Accuracy Prediction")
summary(ACCComp1)#not sig
summary(ACCComp2) #sig at Direction
summary(ACCfit1) #sig at direction
summary(ACCfit2N)#not sig

#Group for Global/Local stats Configuration Time 1
TargetLocation_T2T1=subset(F_N_T2T1_all, select= c(Subject, Session, TargetLocation, Stimuli.ACC, Stimuli.RT))
TargetLocation_T2T1=group_by(TargetLocation_T2T1, Subject, Session, TargetLocation)
TargetLocation_T2T1_stats_bysubject=summarise(TargetLocation_T2T1, AvgAcc=mean(Stimuli.ACC, na.rm=TRUE), AvgRT=mean(Stimuli.RT, na.rm=TRUE))
TargetLocation_T2T1_simple_stats3=subset(TargetLocation_T2T1, select= -Subject)
TargetLocation_T2T1_simple_stats2=group_by(TargetLocation_T2T1_simple_stats3, TargetLocation, Session)
TargetLocation_T2T1_simple_stats=summarise(TargetLocation_T2T1_simple_stats2, AvgAcc=mean(Stimuli.ACC, na.rm=TRUE), AvgRT=mean(Stimuli.RT, na.rm=TRUE))
rm(TargetLocation_T2T1_simple_stats2, TargetLocation_T2T1_simple_stats3)


#Print stats Summaries
write_csv(TargetLocation_T2T1_stats_bysubject, "./TargetLocation_T2T1_stats_bysubject.csv")
write_csv(TargetLocation_T2T1_simple_stats, "./TargetLocation_T2T1_simple_stats.csv")
write_csv(Config_T2T1_stats_bysubject, "./Config_T2T1_stats_bysubject.csv")
write_csv(Config_T2T1_simple_stats, "./Config_T2T1_simple_stats.csv")


##simple attempt to combine and mutate Df
NT2DF=rbind(t2s1N, t2s2N, t2s3N, t2s4N, t2s5N)
colnames(NT2DF)
NT2DF=select(NT2DF, Stimuli.ACC, Stimuli.RT)
attempts=cbind(N_time1all, NT2DF)
attempts=subset(attempts, select= -c(XCoo, YCoo, Block))
attempts=rename_at(attempts, 2, ~"Stimuli.AccT1")
attempts=rename_at(attempts, 3, ~"Stimuli.RTT1")
F_attempts=attemtps%>% 
F_attempts$Session=factor(c(attempts$Session, attempts$Subject, attempts$Configuration, attempts$TargetLocation, attempts$TargetLetter))
F_N_T2T1_all$Subject=factor(F_N_T2T1_all$Subject)
F_N_T2T1_all$Configuration=factor(F_N_T2T1_all$Configuration)
F_N_T2T1_all$TargetLetter=factor(F_N_T2T1_all$TargetLetter)
F_N_T2T1_all$TargetLocation=factor(F_N_T2T1_all$TargetLocation)
#attempts1=subset(attempts, Session==1)
#attempts2= subset(attempts, Session==2)
#attempts=cbind(attempts1, attempts2)
#colnames(attempts)=c("TargetLocation", "Session1", "AvgAccT1", "AvgRTT1", "Removethis", "Session2", "AvgAccT2", "AvgRTT2")
#rm(attempts1, attempts2)
#attempts=subset(attempts, select= - c(Removethis, Session1, Session2))
#attempting2=data.frame(attempts, "DifferenceRT"=(attempts$AvgRTT2 - attempts$AvgRTT1), "DifferenceACC"=(attempts$AvgRTT2 -attempts$AvgRTT1))
