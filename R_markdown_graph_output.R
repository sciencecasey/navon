#Markdown Graphs:
#Inverted Faces RT for all answered questions (accurate and inaccurate)
#Upright Spaghetti Plot
require("ggplot2")
require(GGally)
require(reshape2)
require(lme4)
#require(compiler)
#require(parallel)
#require(boot)
require(lattice)
require(dplyr)
require(tidyverse)
require("haven")
require(lsr)
require(plyr)
require(car)
require(fitdistrplus)
require(multcomp)
require(emmeans)
library(lmerTest)
library(latticeExtra)  
library(ez)
library(markdown)
re=xyplot(logRT_mean~Session, groups=Subject, subset=Direction=="Up", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
          par.settings=ggplot2like(),axis=axis.grid, ylab="Upright Response Time", auto.key = TRUE)
#Inverted Spaghetti Plot
ra=xyplot(logRT_mean~Session, groups=Subject, subset=Direction=="Inv", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
          par.settings=ggplot2like(),axis=axis.grid, ylab="Inverted Response Time", auto.key = TRUE, pch=24)
re
ra
plot(ra+re, yaxp=c(6.4,7.4, 10))
#Spaghetti plot line for each subject and Direction in panels
xyplot(logRT_mean~Session|Direction, groups=Subject, Inv_stats_bysub_bydirection_bytime, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time", auto.key = list(space="right"), layout=c(2,1), main="Response Time by Subject")
#try Spaghetti plot line each sub in same panel
try.pch=c(22, 16)
try.fill=c("orange", "skyblue", "lightgreen", "purple", "red")
(tryplot=with(Inv_stats_bysub_bydirection_bytime,
              xyplot(logRT_mean~Session,
                     panel=function(x,y,...,subscripts) {
                       pch=try.pch[Direction[subscripts]]
                       fill=try.fill[Subject[subscripts]]
                       panel.xyplot(x,y,pch=pch, fill=fill, col=fill, type ='p', cex=1.25)
                     },
                     key=list(space="right", text=list(levels(Direction)), points=list(pch=try.pch, col="black", cex=1.5), 
                              text=list(levels(Subject)), points=list(pch=16, col=try.fill),
                              rep=FALSE), axis=axis.grid, ylab="Response Time", main="Response Time by Subject")))
#plot Subject 1
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="1", Inv_stats_bysub_bydirection_bytime, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 1")
#plot Sub 2
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="2", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 2")
#plot Sub 3
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="3", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 3")
#Plot Sub 4
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="4", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 4")
#Plot Sub 5
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="5", Inv_stats_bysub_bydirection_bytime, type=c('p', 'l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 5")
#Plot Overall Spaghetti
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="Overall", Inv_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall RT") 

xyplot(logRT_mean~Session, groups=c(Direction), Inv_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall RT", main="Subject 5 & Overall", auto.key = list(space="right", title="Key", cex.title=1.5))

#Inv_stats_bysub_bydirection_bytime$tmpSubject=as.numeric(Inv_stats_bysub_bydirection_bytime$Subject)
#sidebyside=equal.count(Inv_stats_bysub_bydirection_bytime$tmpSubject, number=5, overlap=0)
mypanel=function(x,y,h, k){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
#xyplot(logRT_mean~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key = list(space="top"))

colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubject=factor(Inv_stats_bysub_bydirection_bytime$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubject, groups=Direction, data=Inv_stats_bysub_bydirection_bytime, h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key=keylist)

#plot each person's RT Over time
T1Plots=Invtime1_time2_LONG %>%
  subset(Invtime1_time2_LONG, Session=="1")
T2Plots=Invtime1_time2_LONG %>%
  subset(Session=="2")
str(T1Plots)
str(T2Plots)
sum(T1Plots$Session==1)
sum(T1Plots$Session==2)
sum(T2Plots$Session==1)
sum(T2Plots$Session==2) #just double checking

#recode trials to be over time
length(T1Plots$Trial)
T1Plots$Trial=seq(1,280)
T1Plots$Trial
T2Plots$Trial=seq(1,280)
T2Plots$Trial
sum(is.na(T2Plots$logRT)) #11
sum(is.na(T1Plots$logRT)) #9

#Time 1
ggplot(data=Invtime1_time2_LONG, subset=c(Subject=="50142" & Session=="1", aes(logRT~Trial) +geom_line()))
levels(T1Plots$Subject)
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50142", data=T1Plots, type='l', ylim = (3:10), main="Subject1, Session 1 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50192", data=T1Plots, type='l', ylim = (3:10), main="Subject2, Session 1 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50202", data=T1Plots, type='l', ylim = (3:10), main="Subject2, Session 1 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50262", data=T1Plots, type='l', ylim = (3:10), main="Subject2, Session 1 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50312", data=T1Plots, type='l', ylim = (3:10), main="Subject2, Session 1 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Subject, data=T1Plots, type="a", ylim=(3:10), main="Session 1 Response Times by Subject", auto.key = list(space='top'))
#i don't understand why the lines don't overlap

#Time 2
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50142", data=T2Plots, type='l', ylim = (3:10), main="Subject1, Session 2 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50192", data=T2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50202", data=T2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50262", data=T2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Direction, subset=Subject=="50312", data=T2Plots, type='l', ylim = (3:10), main="Subject2, Session 2 RT", auto.key = list(space="top"))
xyplot(logRT~Trial, groups=Subject, data=T1Plots, type="a", ylim=(3:10), main="Session 2 Response Times by Subject", auto.key = list(space='top'))

#RT Inverted Faces Only the Accurate Responses
#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Direction), Inv_2_sub_dir_time, type='l',
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time")
xyplot(logRT~Session, group=Subject, subset=Comparison.ACC==0, Invtime1_time2_LONG, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time, Incorrect Responses")
xyplot(logRT~Session, group=Comparison.ACC, Invtime1_time2_LONG, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time")


#ACC Inverted Faces
#Upright Spaghetti Plot
xyplot(Comparison.ACC_mean~Session, groups=Subject, subset=Direction=="Up", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Upright Accuracy")
#Inverted Spaghetti Plot
xyplot(Comparison.ACC_mean~Session, groups=Subject, subset=Direction=="Inv", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Inverted Accuracy")
#Spaghetti plot line for each subject and Direction
xyplot(Comparison.ACC_mean~Session, groups=c(Subject, Direction), Inv_stats_bysub_bydirection_bytime, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Accuracy")
#plot Subject 1
xyplot(Comparison.ACC_mean~Session, groups=Direction, subset=Subject=="1", Inv_stats_bysub_bydirection_bytime, type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Mean ACC Subject 1")
#plot Sub 2
xyplot(Comparison.ACC_mean~Session, groups=Direction, subset=Subject=="2", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean ACC Subject 2")
#plot Sub 3
xyplot(Comparison.ACC_mean~Session, groups=Direction, subset=Subject=="3", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean ACC Subject 3")
#Plot Sub 4
xyplot(Comparison.ACC_mean~Session, groups=Direction, subset=Subject=="4", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean ACC Subject 4")
#Plot Sub 5
xyplot(Comparison.ACC_mean~Session, groups=Direction, subset=Subject=="5", Inv_stats_bysub_bydirection_bytime, type=c('p', 'l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 5")
#Plot Overall Spaghetti
xyplot(Comparison.ACC_mean~Session, groups=Direction, subset=Subject=="Overall", Inv_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall ACC") 

xyplot(Comparison.ACC_mean~Session, groups=c(Direction), Inv_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall ACC", main="Subject 5 & Overall", auto.key = list(space="right", title="Key", cex.title=1.5))

#Inv_stats_bysub_bydirection_bytime$tmpSubject=as.numeric(Inv_stats_bysub_bydirection_bytime$Subject)
#sidebyside=equal.count(Inv_stats_bysub_bydirection_bytime$tmpSubject, number=5, overlap=0)
mypanel=function(x,y,h, k){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
#xyplot(Comparison.ACC~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject ACC Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key = list(space="top"))

colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubject=factor(Inv_stats_bysub_bydirection_bytime$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(Comparison.ACC_mean~Session|bysubject, groups=Direction, data=Inv_stats_bysub_bydirection_bytime, h=Inv_stats_bysub_bydirection_bytime$Comparison.ACC_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key=keylist)
#plot each person's ACC Over time
#Time 1
ggplot(data=T1Plots, subset=Subject=="50142", aes(Comparison.ACC~Trial) +geom_line())
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50142", data=T1Plots, type='l', main="Subject1, Session 1 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50192", data=T1Plots, type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50202", data=T1Plots, type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50262", data=T1Plots, type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50312", data=T1Plots, type='l', main="Subject2, Session 1 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Subject, data=T1Plots, type="a", main="Session 1 Accuracy by Subject", auto.key = list(space='top'))

#Time 2
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50142", data=T2Plots, type='l', main="Subject1, Session 2 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50192", data=T2Plots, type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50202", data=T2Plots, type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50262", data=T2Plots, type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Direction, subset=Subject=="50312", data=T2Plots, type='l', main="Subject2, Session 2 ACC", auto.key = list(space="top"))
xyplot(Comparison.ACC~Trial, groups=Subject, data=T1Plots, type="a", main="Session 2 Accuracy by Subject", auto.key = list(space='top'))

##############################################Navon
#RT Navon Analysis

#Switch/NonSwitch
#All answers, acc and inacc
#Switch Trials Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="switch", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Switch Response Time", auto.key = TRUE)
#Non-Switch Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="nonswitch", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Non-switch Response Time", auto.key = TRUE)
#By session
xyplot(logRT_mean~Switch|Session, groups=Subject, 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Response Time by Session", auto.key = TRUE)
#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Switch), N_switch_stats_bysub_bysession, 
       type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, 
       ylab="Response Time", auto.key = TRUE) 
#the lines intersect creating a third line that is meaningless

#plot Subject 1
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50142", 
       N_switch_stats_bysub_bysession, type=c('p','l'), 
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Mean RT Subject 1", auto.key = TRUE)
#plot Sub 2
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50192", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Mean RT Subject 2", auto.key = TRUE)
#plot Sub 3
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50202", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Mean RT Subject 3", auto.key = TRUE)
#Plot Sub 4
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50262", 
       N_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Mean RT Subject 4", auto.key = TRUE)
#Plot Sub 5
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="50312", 
       N_switch_stats_bysub_bysession, type=c('p', 'l'), 
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Mean RT Subject 5", auto.key = TRUE)
#Plot Overall Spaghetti
xyplot(logRT_mean~Session, groups=Switch, subset=Subject=="Overall", 
       N_switch_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Overall RT", auto.key = TRUE) 

#xyplot(logRT_mean~Session, groups=Switch, N_switch_stats, type=c('p','l'),
#     par.settings=ggplot2like(),axis=axis.grid, ylab="Overall RT", main="Overall", 
#     auto.key = list(space="right"))

#Inv_stats_bysub_bydirection_bytime$tmpSubject=as.numeric(Inv_stats_bysub_bydirection_bytime$Subject)
#sidebyside=equal.count(Inv_stats_bysub_bydirection_bytime$tmpSubject, number=5, overlap=0)
#xyplot(logRT_mean~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key = list(space="top"))

mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), 
             columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubjectN=factor(N_switch_stats$Subject, levels = c(1,2,3,4,5), 
                  labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubjectN, groups=Switch, data=N_switch_stats, 
       h=N_switch_stats$logRT_mean, layout=c(5,1), aspect=1.5, 
       main="Subject Response Times Switch v NonSwitch", xlab="RT (ms)", 
       ylab="Session", panel=mypanel, auto.key=keylist)
#plot each person's RT Over time as a check for lag/fatigue
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
#ggplot(data=F_N_T2T1_all, subset=c(Subject=="50142" & Session=="1", aes(logRT~Trial) +geom_line()))
levels(NT1Plots$Subject)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50142", data=NT1Plots, 
       type='l', ylim = (5.5:8.5), main="Subject1, Session 1 RT", 
       auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50192", data=NT1Plots, 
       type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", 
       auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50202", data=NT1Plots, 
       type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", 
       auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50262", data=NT1Plots, 
       type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", 
       auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50312", data=NT1Plots, 
       type='l', ylim = (5.5:8.5), main="Subject2, Session 1 RT", 
       auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Subject, data=NT1Plots, type="a", ylim=(5.5:8.5), 
       main="Session 1 Response Times by Subject", 
       auto.key = list(space='top'), xlim=(1:130), x.scales=10)


#Time 2
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50142", data=NT2Plots, type='l', ylim = (3:10), 
       main="Subject1, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50192", data=NT2Plots, type='l', ylim = (3:10), 
       main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50202", data=NT2Plots, type='l', ylim = (3:10), 
       main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50262", data=NT2Plots, type='l', ylim = (3:10), 
       main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Switch, subset=Subject=="50312", data=NT2Plots, type='l', ylim = (3:10), 
       main="Subject2, Session 2 RT", auto.key = list(space="top"), xlim=(1:130), x.scales=10)
xyplot(logRT~Trial, groups=Subject, data=NT1Plots, type="a", ylim=(3:10), main="Session 2 Response Times by Subject", auto.key = list(space='top'), xlim=(1:130), x.scales=10)

#Inv/Upright only Accurate responses; Switch/Nonswitch
#Switch Trials Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="switch", 
       N2_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Switch Response Time", auto.key = TRUE)
#Non-Switch Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Switch=="nonswitch", 
       N2_switch_stats_bysub_bysession, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, 
       main="Non-switch Response Time", auto.key = TRUE)
#By session
xyplot(logRT_mean~Switch|Session, groups=Subject, N2_switch_stats_bysub_bysession, 
       type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, 
       main="Response Time by Session", auto.key = TRUE, alternate=FALSE)
#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Switch), N2_switch_stats_bysub_bysession, 
       type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time", auto.key = TRUE)

#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Switch), N2_switch_stats_bysub_bysession, 
       type=c('p','l'), par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time", auto.key = TRUE)

#with regression and mean line as panels
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), 
             columns=1, text=c("Inv", "Up", "Mean", "Regression"), points=FALSE)
bysubjectN2=factor(N2_switch_stats$Subject, levels = c(1,2,3,4,5), 
                   labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubjectN2, groups=Switch, data=N2_switch_stats, 
       h=N2_switch_stats$logRT_mean, layout=c(5,1), aspect=1.5, 
       main="Subject Response Times Switch v NonSwitch", xlab="RT (ms)", 
       ylab="Session", panel=mypanel, auto.key=keylist)
#still having trouble getting Up lines the correct color and not having the connecting line


###############################################Group for Global/Local stats Configuration, RT
########################using all responses, acc and inacc
#Plot Target Location by Session
(ses1=xyplot(logRT_mean~TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, 
             subset=Session=="1", auto.key = list(space="right", columns=2), 
             type=c("p","l"), ylim = c(6,7),main="Session 1 RT by Location"))
(ses2=xyplot(logRT_mean~TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, 
             subset=Session=="2", auto.key=list(space="right"), type=c("p", "l"),
             ylim = c(6,7), pch=24, main="Session 2 RT by Location"))
#key=list(space="right", column=2, 
#         points=list(pch=c(21, 24), col=c("blue", "pink", "green", "red", "yellow")), 
#         text=c(levels(fn_tlocation_bysubj_bytime$Subject), 
#                levels(fn_tlocation_bysubj_bytime$Session)))
plot(ses2+ses1) 
#I want to learn to add symbols showing the triangles are session 2 and circles session 1 if desired
(glob=xyplot(logRT_mean~Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
             subset=TargetLocation=="G", auto.key = list(space="right"), 
             type=c("p","l"), ylim = c(6,7),main="RT, Global"))
(loc=xyplot(logRT_mean~Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
            subset=TargetLocation=="L", auto.key=list(space="right"), type=c("p", "l"),
            ylim = c(6,7), pch=24, main="RT, Local"))
#mycols=c("pink", "blue", "yellow", "brown", "light green")
plot(glob+loc, cols=cols, main="Global and Local Response Times by Session")# %>%
#  legend("right", legend = 
#         paste(c("1 Global", "2 Global","3 Global", "4 Global", "5 Global"), 
#               c("1 Local", "2 Local", "3 Local", "4 Local", "5 Local"), sep=";"), 
#       col=rep(cols, times=2), pch=rep(c(16, 24), each=5), bty="n", 
#       ncol=2, cex=.7, pt.cex = .7)
#key not functioning correctly nor is the title

#plot side by side session 1 and 2 glocal and local
xyplot(logRT_mean~Session|TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")
xyplot(logRT_mean~TargetLocation|Session, data=fn_tlocation_bysubj_bytime, groups=Subject, auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")

#panels by subject with mean and regression
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
Tlockeylist=list(space="top", col=c("blue", "green", "red", "purple"), 
                 columns=1, text=c("Global", "Local", "Mean", "Regression"), points=FALSE)
bysubjectTloc=factor(fn_tlocation_stats$Subject, levels = c(1,2,3,4,5), 
                     labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubjectTloc, groups=TargetLocation, data=fn_tlocation_stats, 
       h=fn_tlocation_stats$logRT_mean, layout=c(5,1), aspect=1.5, 
       main="Subject Response Times Target Location", xlab="RT (ms)", 
       ylab="Session", panel=mypanel, auto.key=Tlockeylist)


###################using RT that are accurate ONLY
#Plot Target Location by Session
(ses1_2=xyplot(logRT_mean~TargetLocation, data=fn2_tlocaiton_bysubj_bytime, groups=Subject, 
               subset=Session=="1", auto.key = list(space="right"), 
               type=c("p","l"), ylim = c(6,7),main="Session 1 RT by Location"))
(ses2_2=xyplot(logRT_mean~TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, 
               subset=Session=="2", auto.key=list(space="right"), 
               type=c("p", "l"), ylim = c(6,7), pch=24, main="Session 2 RT by Location"))
#key=list(space="right", column=2, points=list(pch=c(21, 24), col=c("blue", "pink", "green", "red", "yellow")), text=c(levels(fn_tlocation_bysubj_bytime$Subject), levels(fn_tlocation_bysubj_bytime$Session)) )
plot(ses2_2+ses1_2) 
#I want to learn to add symbols showing the triangles are session 2 and circles session 1 if desired
(glob_2=xyplot(logRT_mean~Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
               subset=TargetLocation=="G", auto.key = list(space="right"), 
               type=c("p","l"), ylim = c(6,7),main="RT, Global"))
(loc_2=xyplot(logRT_mean~Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
              subset=TargetLocation=="L", auto.key=list(space="right"), 
              type=c("p", "l"),ylim = c(6,7), pch=24, main="RT, Local"))
plot(glob_2+loc_2, main="Global and Local Response Times by Session") 
#key not functioning correctly nor is the title

#plot side by side session 1 and 2 glocal and local
xyplot(logRT_mean~Session|TargetLocation, data=fn2_tlocaiton_bysubj_bytime, groups=Subject, 
       auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")
xyplot(logRT_mean~TargetLocation|Session, data=fn2_tlocaiton_bysubj_bytime, groups=Subject, 
       auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")

#In panels by subj with mean and regression
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
Tlockeylist=list(space="top", col=c("blue", "green", "red", "purple"), 
                 columns=1, text=c("Global", "Local", "Mean", "Regression"), points=FALSE)
bysubjectTloc=factor(fn_tlocation_stats$Subject, levels = c(1,2,3,4,5), 
                     labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubjectTloc, groups=TargetLocation, data=fn2_stats_tlocation, 
       h=fn2_stats_tlocation$logRT_mean, layout=c(5,1), aspect=1.5, 
       main="Subject Response Times Target Location", xlab="RT (ms)", 
       ylab="Session", panel=mypanel, auto.key=Tlockeylist)


###Navon ACC
#Switch/Nonswitch
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

#plot each person's RT Over time
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
