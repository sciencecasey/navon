t1s1N=read_csv("./Navon-50262-1_truncated2.csv")
t2s1N=read_csv("./Navon-50262-2_truncated2.csv")
t1s2N=read_csv("./Navon-50202-1_truncated2.csv")
t2s2N=read_csv("./Navon-50202-2_truncated2.csv")
t1s3N=read_csv("./Navon-50192-1_truncated2.csv")
t2s3N=read_csv("./Navon-50192-2_truncated2.csv")
t1s4N=read_csv("./Navon-50312-1_truncated2.csv")
t2s4N=read_csv("./Navon-5031-2_truncated2.csv")
t1s5N=read_csv("./Navon-501421-1_truncated2.csv")
t2s5N=read_csv("./Navon-50142-2_truncated2.csv")
#Create DF
N_time1all=bind_rows(t1s1N, t1s2N, t1s3N, t1s4N, t1s5N)
N_time1all$Session=replace_na(N_time1all$Session, 1)
N_time1all$Session
N_T2T1_all=bind_rows(N_time1all, t2s1N, t2s2N, t2s3N, t2s4N, t2s5N)
str(N_T2T1_all)
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
sum(is.na(F_N_T2T1_all$Stimuli.ACC))
sum(is.na(F_N_T2T1_all$Stimuli.RT))
#make a column for logTime
F_N_T2T1_all$logRT=log(F_N_T2T1_all$Stimuli.RT)
#make switch/nonswitch condition
F_N_T2T1_all$Switch=ifelse(F_N_T2T1_all$Configuration=="GLS" | F_N_T2T1_all$Configuration=="LGS", "switch", "nonswitch")
F_N_T2T1_all$Switch=factor(F_N_T2T1_all$Switch)
str(F_N_T2T1_all)


#All RT for answered questions (inacc and inac)
#check that is what we're looking at
sum(F_N_T2T1_all$Stimuli.ACC==0, na.rm = TRUE) #25
sum(F_N_T2T1_all$Stimuli.ACC==1, na.rm=TRUE) #1227
sum(is.na(F_N_T2T1_all$Stimuli.ACC)) #28
sum(is.na(F_N_T2T1_all$logRT)) #28

N_switch=subset(F_N_T2T1_all, select=c(Stimuli.ACC, logRT, Subject, Session, Switch, TargetLocation))

#Switch grouped stats by sub and direction
N_switch_stats_bysub=N_switch %>%
  group_by(Subject, Switch) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#Inv grouped stats by sub and direction and time
N_switch_stats_bysub_bysession=N_switch %>%
  group_by(Subject, Switch, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#overall time by direction by session
N_switch_stats_bysession=N_switch %>%
  group_by(Switch, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#put overall and by subj into a df
N_switch_stats=bind_rows(N_switch_stats_bysub_bysession, N_switch_stats_bysession) #bysub&direction was a df, bydirection was a tibble
(N_switch_stats$Subject=ifelse(is.na(N_switch_stats$Subject), "Overall", N_switch_stats$Subject))

#Switch Trials Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="switch", N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Switch Response Time", auto.key = TRUE)
#Non-Switch Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="nonswitch", N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Non-switch Response Time", auto.key = TRUE)
#By session
xyplot(logRT_mean~Switch|Session, groups=Subject, N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Response Time by Session", auto.key = TRUE)
#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Switch), N_switch_stats_bysub_bysession, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time", auto.key = TRUE)
#plot Subject 1
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50142", N_switch_stats_bysub_bysession, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, main="Mean RT Subject 1", auto.key = TRUE)
#plot Sub 2
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50192", N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Mean RT Subject 2", auto.key = TRUE)
#plot Sub 3
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50202", N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Mean RT Subject 3", auto.key = TRUE)
#Plot Sub 4
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50262", N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Mean RT Subject 4", auto.key = TRUE)
#Plot Sub 5
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50312", N_switch_stats_bysub_bysession, type=c('p', 'l'), par.settings=ggplot2like(),axis=axis.grid, main="Mean RT Subject 5", auto.key = TRUE)
#Plot Overall Spaghetti
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="Overall", N_switch_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Overall RT", auto.key = TRUE) 

xyplot(logRT_mean~Session, groups=Switch, N_switch_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall RT", main="Overall", auto.key = list(space="right"))

#Inv_stats_bysub_bydirection_bytime$tmpSubject=as.numeric(Inv_stats_bysub_bydirection_bytime$Subject)
#sidebyside=equal.count(Inv_stats_bysub_bydirection_bytime$tmpSubject, number=5, overlap=0)
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
#xyplot(logRT_mean~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key = list(space="top"))

colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubjectN=factor(N_switch_stats$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubjectN, groups=Switch, data=N_switch_stats, h=N_switch_stats$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Switch v NonSwitch", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key=keylist)


#Repeated Measures Anova on Means log RT
N_switch_RT=ezANOVA(dv=logRT_mean, within=c(Switch, Session), wid=Subject, data=N_switch_stats_bysub_bysession, detailed=TRUE)
(N_switch_RTAnova=N_switch_RT$ANOVA) #switch and Session Sig. (and intercept)
#sig effect of Direction and Intercept

#GLMM RT 
#add Trial factor
str(F_N_T2T1_all)
length(F_N_T2T1_all$Stimuli.ACC)
F_N_T2T1_all$Trial=factor(rep(seq(1, 128), 10))
str(F_N_T2T1_all)

Nswitch_RTComp1=lmer(logRT~1+Switch+Session+Switch:Session+(1+Session|Subject), data=F_N_T2T1_all)
summary(Nswitch_RTComp1) 
#main effect of Switch, Session, and Intercept (not interaction)
anova(Nswitch_RTComp1) #switch and session, not interaction

#plot each person's RT Over time
NT1Plots=F_N_T2T1_all %>%
  subset(Session=="1")
NT2Plots=F_N_T2T1_all %>%
  subset(Session=="2")
str(NT1Plots)
str(NT2Plots)
sum(NT1Plots$Session==1)
sum(NT1Plots$Session==2)
sum(NT2Plots$Session==1)
sum(NT2Plots$Session==2) #just double checking
#count non-responses
sum(is.na(NT2Plots$logRT)) #13
sum(is.na(NT1Plots$logRT)) #15

#Time 1
ggplot(data=F_N_T2T1_all, subset=c(Subject=="50142" & Session=="1", aes(logRT~Trial) +geom_line()))
levels(NT1Plots$Subject)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50142", data=NT1Plots, type='l', ylim = (5.5:8.5), main="Subject1, Session 1 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50192", data=NT1Plots, type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50202", data=NT1Plots, type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50262", data=NT1Plots, type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50312", data=NT1Plots, type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Subject, data=NT1Plots, type="a", ylim=(5.5:8.5), main="Session 1 Response Times by Subject", auto.key = list(space='top'), xlim=(1:130), x.scales=10)


#Time 2
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50142", data=NT2Plots, type='l', ylim = (3:10), main="Subject1, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50192", data=NT2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50202", data=NT2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50262", data=NT2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50312", data=NT2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Subject, data=NT1Plots, type="a", ylim=(3:10), main="Session 2 Response Times by Subject", auto.key = list(space='top'), xlim=(1:130), x.scales=10)

#################################################################################################

#Recode RT to include only Accurate responses
FN2=F_N_T2T1_all
str(FN2)
is.na(FN2$logRT)=FN2$Stimuli.ACC=="0"
sum(is.na(FN2$logRT))#53 total
sum(is.na(FN2$Stimuli.ACC))#28
sum(FN2$Stimuli.ACC=="0", na.rm = TRUE) #25

#Switch grouped stats by sub and direction
N2_switch_stats_bysub=FN2 %>%
  group_by(Subject, Switch) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#Inv grouped stats by sub and direction and time
N2_switch_stats_bysub_bysession=FN2 %>%
  group_by(Subject, Switch, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#overall time by direction by session
N2_switch_stats_bysession=FN2 %>%
  group_by(Switch, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#put overall and by subj into a df
N2_switch_stats=bind_rows(N2_switch_stats_bysub_bysession, N2_switch_stats_bysession) #bysub&direction was a df, bydirection was a tibble
(N2_switch_stats$Subject=ifelse(is.na(N2_switch_stats$Subject), "Overall", N2_switch_stats$Subject))

#Switch Trials Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="switch", N2_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Switch Response Time", auto.key = TRUE)
#Non-Switch Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="nonswitch", N2_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Non-switch Response Time", auto.key = TRUE)
#By session
xyplot(logRT_mean~Switch|Session, groups=Subject, N2_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Response Time by Session", auto.key = TRUE, alternate=FALSE)
#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Switch), N2_switch_stats_bysub_bysession, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time", auto.key = TRUE)

#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Switch), N2_switch_stats_bysub_bysession, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time", auto.key = TRUE)


#Group for Global/Local stats Configuration, RT