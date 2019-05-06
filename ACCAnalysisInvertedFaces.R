#All responses as proportion of ACC (unanswered removed)
summary(Invtime1_time2_LONG)
str(Invtime1_time2_LONG)

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


#Repeated Measures Anova on Means ACC
InvACC=ezANOVA(dv=Comparison.ACC_mean, within=c(Direction, Session), wid=Subject, data=Inv_stats_bysub_bydirection_bytime, detailed=TRUE)
(InvACCAnova=InvACC$ANOVA) #Direction and Session sig, not sig interaction
#sig effect of Direction and Intercept

#GLMM ACC
str(Invtime1_time2_LONG)
Inv_ACCComp1=lmer(Comparison.ACC~1+Direction+Session+Direction:Session+(1+Session|Subject), data=Invtime1_time2_LONG)
summary(Inv_ACCComp1) #direction significant
#main effect of Direction
anova(Inv_ACCComp1) #direction significant

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



