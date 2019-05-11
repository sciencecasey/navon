summary(F_N_T2T1_all)
#made sure this is the DF to use with unanswered labeled as NA acc
summary(N_switch_stats_bysub_bysession)

#Switch Trials Spaghetti Plot
xyplot(Stimuli.ACC_mean~Session, groups=Subject, subset=Switch=="switch", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Switch ACC", auto.key = TRUE)
#Non-Switch Spaghetti Plot
xyplot(Stimuli.ACC_mean~Session, groups=Subject, subset=Switch=="nonswitch", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Non-switch ACC", 
       auto.key = TRUE)
#By session
xyplot(Stimuli.ACC_mean~Switch|Session, groups=Subject, N_switch_stats_bysub_bysession, 
       type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, main="ACC by Session", 
       auto.key = TRUE)
#Spaghetti plot line for each subject and Direction
xyplot(Stimuli.ACC_mean~Session, groups=c(Subject, Switch), N_switch_stats_bysub_bysession, 
       type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="ACC", auto.key = TRUE)
#plot Subject 1
xyplot(Stimuli.ACC_mean~Session, groups=Switch, subset=Subject=="50142", 
       N_switch_stats_bysub_bysession, type=c('p','l'), par.settings=ggplot2like(),
       axis=axis.grid, main="Mean ACC Subject 1", auto.key = TRUE)
#plot Sub 2
xyplot(Stimuli.ACC_mean~Session, groups=Switch, subset=Subject=="50192", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Mean ACC Subject 2", auto.key = TRUE)
#plot Sub 3
xyplot(Stimuli.ACC_mean~Session, groups=Switch, subset=Subject=="50202", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Mean ACC Subject 3", auto.key = TRUE)
#Plot Sub 4
xyplot(Stimuli.ACC_mean~Session, groups=Switch, subset=Subject=="50262", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Mean ACC Subject 4", auto.key = TRUE)
#Plot Sub 5
xyplot(Stimuli.ACC_mean~Session, groups=Switch, subset=Subject=="50312", 
       N_switch_stats_bysub_bysession, type=c('p', 'l'), 
       par.settings=ggplot2like(),axis=axis.grid, main="Mean RT Subject 5", auto.key = TRUE)
#Plot Overall Spaghetti
xyplot(Stimuli.ACC_mean~Session, groups=Switch, subset=Subject=="Overall", 
       N_switch_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, main="Overall ACC", auto.key = TRUE) 

xyplot(Stimuli.ACC_mean~Session, groups=c(Subject, Switch), N_switch_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall ACC", main="Overall", 
       auto.key = list(space="right"))

#Inv_stats_bysub_bydirection_bytime$tmpSubject=as.numeric(Inv_stats_bysub_bydirection_bytime$Subject)
#sidebyside=equal.count(Inv_stats_bysub_bydirection_bytime$tmpSubject, number=5, overlap=0)
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
#xyplot(logRT_mean~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$Stimuli.ACC_mean, layout=c(5,1), aspect=1.5, main="Subject ACC Inv v Up", xlab="ACC", ylab="Session", panel=mypanel, auto.key = list(space="top"))

colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubjectN=factor(N_switch_stats$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(Stimuli.ACC_mean~Session|bysubjectN, groups=Switch, data=N_switch_stats, 
       h=N_switch_stats$Stimuli.ACC_mean, layout=c(5,1), aspect=1.5, 
       main="Subject ACC Switch v NonSwitch", xlab="Session", ylab="ACC", panel=mypanel, 
       auto.key=keylist)


#Repeated Measures Anova on Means log RT
N_switch_ACC=ezANOVA(dv=Stimuli.ACC_mean, within=c(Switch, Session), wid=Subject, 
                     data=N_switch_stats_bysub_bysession, detailed=TRUE)
(N_switch_ACCAnova=N_switch_ACC$ANOVA) #switch and Session Sig. (and intercept)
#No sig effect (only at intercept)

#GLMM RT 
Nswitch_ACComp1=lmer(Stimuli.ACC~1+Switch+Session+Switch:Session+(1+Session|Subject), 
                     data=F_N_T2T1_all)
summary(Nswitch_ACComp1) 
#no sig
anova(Nswitch_ACComp1) #no sig

#plot each person's RT Over time
NT1PlotsACC=F_N_T2T1_all %>%
  subset(Session=="1")
NT2PlotsACC=F_N_T2T1_all %>%
  subset(Session=="2")
str(NT1PlotsACC)
str(NT2PlotsACC)
sum(NT1PlotsACC$Session==1)
sum(NT1PlotsACC$Session==2)
sum(NT2PlotsACC$Session==1)
sum(NT2PlotsACC$Session==2) #just double checking
#count non-responses
sum(is.na(NT2PlotsACC$logRT)) #13
sum(is.na(NT1PlotsACC$logRT)) #15

#Time 1
#ggplot(data=F_N_T2T1_all, subset=c(Subject=="50142" & Session=="1", aes(Stimuli.ACC~Trial) +geom_line()))
levels(NT1PlotsACC$Subject)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50142", data=NT1Plots, 
       type='l', main="Subject1, Session 1 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50192", data=NT1Plots, 
       type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50202", data=NT1Plots, 
       type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50262", data=NT1Plots, 
       type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50312", data=NT1Plots, 
       type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Subject, data=NT1Plots, type="a", 
       main="Session 1 ACC by Subject",xlim=(1:130), auto.key = list(space='top'), x.scales=10)


#Time 2
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50142", data=NT2Plots, 
       type='l', main="Subject1, Session 2 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50192", data=NT2Plots, 
       type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50202", data=NT2Plots, 
       type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50262", data=NT2Plots, 
       type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Switch, subset=Subject=="50312", data=NT2Plots, 
       type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"), 
       xlim=(1:130), x.scales=10)
xyplot(Stimuli.ACC~Trial, groups=Subject, data=NT1Plots, type="a", 
       main="Session 2 ACC by Subject", auto.key = list(space='top'), xlim=(1:130), x.scales=10)

#Local v Global Grouping ACC
###############################################Group for Global/Local stats Configuration, RT
########################using all responses, acc and inacc
str(F_N_T2T1_all)
sum(is.na(F_N_T2T1_all$Stimuli.ACC)) #28 nonresponse
sum(FN2$Stimuli.ACC=="0", na.rm = TRUE) #25 inaccurate
sum(is.na(F_N_T2T1_all$logRT)) #only exclude the 28 nonresponse

#Repeated Measures Anova on Means ACC
N_tloc_ACC=ezANOVA(dv=Stimuli.ACC_mean, within=c(TargetLocation, Session), 
                   wid=Subject, data=fn_tlocation_bysubj_bytime, detailed=TRUE)
(N_tloc_ACCAnova=N_tloc_ACC$ANOVA) #None Sig.

#GLMM RT 
Ntloc_ACCComp1=lmer(Stimuli.ACC~1+TargetLocation+Session+TargetLocation:Session+(1+Session|Subject), 
                    data=F_N_T2T1_all)
summary(Ntloc_ACCComp1) #none sig.
anova(Ntloc_ACCComp1) #none sig.

#Plot Target Location by Session
(ses1ACC=xyplot(Stimuli.ACC_mean~TargetLocation, data=fn_tlocation_bysubj_bytime, 
                groups=Subject, subset=Session=="1", auto.key = list(space="right"), 
                type=c("p","l"),main="Session 1 RT by Location"))
(ses2ACC=xyplot(Stimuli.ACC_mean~TargetLocation, data=fn_tlocation_bysubj_bytime, 
                groups=Subject, subset=Session=="2", auto.key=list(space="right"), 
                type=c("p", "l"), pch=24, main="Session 2 RT by Location"))
#key=list(space="right", column=2, points=list(pch=c(21, 24), col=c("blue", "pink", "green", "red", "yellow")), text=c(levels(fn_tlocation_bysubj_bytime$Subject), levels(fn_tlocation_bysubj_bytime$Session)) )
plot(ses2ACC+ses1ACC) #I want to learn to add symbols showing the triangles are session 2 and circles session 1 if desired
(globACC=xyplot(Stimuli.ACC_mean~Session, data=fn_tlocation_bysubj_bytime, 
                groups=Subject, subset=TargetLocation=="G", auto.key = list(space="right"), 
                type=c("p","l"),main="RT, Global"))
(locACC=xyplot(Stimuli.ACC_mean~Session, data=fn_tlocation_bysubj_bytime, 
               groups=Subject, subset=TargetLocation=="L", auto.key=list(space="right"), 
               type=c("p", "l"), pch=24, main="RT, Local"))
plot(globACC+locACC, main="Global and Local Response Times by Session") #key not functioning correctly nor is the title

#plot side by side session 1 and 2 glocal and local
xyplot(Stimuli.ACC_mean~Session|TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, 
       auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")
xyplot(Stimuli.ACC_mean~TargetLocation|Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
       auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")

#in panels by subj with regression and mean lines
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
Tlockeylist
bysubjectGlobal=factor(fn_tlocation_stats$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(Stimuli.ACC_mean~Session|bysubjectGlobal, groups=TargetLocation, data=fn_tlocation_stats, 
       h=fn_tlocation_stats$Stimuli.ACC_mean, layout=c(5,1), aspect=1.5, 
       main="Subject ACC Global v Local", xlab="Session", ylab="ACC", panel=mypanel, 
       auto.key=Tlockeylist)
