#Pull together all the data TIME ONE ONLY
#after importing subjects, create long df/tibble with all the subjects
Invtime1all=rbind(t1s1, t1s2, t1s3, t1s4, t1s5)
Invtime1all
#add time 2
Invtime1_time2_all=rbind(Invtime1all, t2s1, t2s2, t2s3, t2s4, t2s5)
#some of the data is saved as Inv some and Inverse, change all to Inv
Invtime1_time2_all$Direction[Invtime1_time2_all$Direction=="Inverse"]="Inv"
#some subjects saved without 2 at the end for time 2
Invtime1_time2_all$Subject[Invtime1_time2_all$Subject=="5031"]="50312"
Invtime1_time2_all$Subject[Invtime1_time2_all$Subject=="5020"]="50202"
#subset only the LONG trials
Invtime1all[["Time"]] #checking the data only
Invtime1_time2_LONG=subset(Invtime1_time2_all, Time=="l")
#set Subject as Factor to double check
Invtime1_time2_LONG$Subject=factor(Invtime1_time2_LONG$Subject)
levels(Invtime1_time2_LONG$Subject) #check 5 subjects
#explore the amount of incorrect 
sum(Invtime1_time2_LONG$Comparison.ACC=="0")
#75 total inaccurate
sum(is.na(Invtime1_time2_LONG$Comparison.RESP))
#20 non-response
sum(Invtime1_time2_LONG$Comparison.RT=="0")
#20 listed as RT=0, these were non-response
#Recode RT saved as 0 for nonresponse to NA 
is.na(Invtime1_time2_LONG$Comparison.RT)=is.na(Invtime1_time2_LONG$Comparison.RESP)
sum(Invtime1_time2_LONG$Comparison.RT==0) 
sum(is.na(Invtime1_time2_LONG$Comparison.RT))#check function recoded 
#remove ACC values for the NA RT as these shouldn't be coded
sum(is.na(Invtime1_time2_LONG$Comparison.ACC)) #no n/as for ACC
is.na(Invtime1_time2_LONG$Comparison.ACC)=is.na(Invtime1_time2_LONG$Comparison.RESP)
sum(is.na(Invtime1_time2_LONG$Comparison.ACC))
sum(is.na(Invtime1_time2_LONG$Comparison.RESP)) #check that these are the same
sum(Invtime1_time2_LONG$Comparison.ACC==0, na.rm=TRUE) # new total incorrect responses (should be lower, not counting non-response)
sum(!is.na(Invtime1_time2_LONG$Comparison.RT)) #540 responses
sum(!Invtime1_time2_LONG$Comparison.ACC==0, na.rm=TRUE) #485 accurate trials
#540-480=60 inaccurate responses

#RT all responses 
#add column for logtime
Invtime1_time2_LONG$logRT=log(Invtime1_time2_LONG$Comparison.RT)
str(Invtime1_time2_LONG)
sum(is.na(Invtime1_time2_LONG$logRT)) #checking the calculation used NAs from RESP and RT

#Inv grouped stats by sub and direction
Inv_stats_bysub_bydirection=Invtime1_time2_LONG %>%
  group_by(Subject, Direction) %>%
  summarise_at(c("Comparison.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#Inv grouped stats by sub and direction and time
Inv_stats_bysub_bydirection_bytime=Invtime1_time2_LONG %>%
  group_by(Subject, Direction, Session) %>%
  summarise_at(c("Comparison.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#overall time by direction by session
Inv_stats_bydirection_bytime=Invtime1_time2_LONG %>%
  group_by(Direction, Session) %>%
  summarise_at(c("logRT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#put overall and by subj into a df
Inv_stats=bind_rows(Inv_stats_bysub_bydirection_bytime, Inv_stats_bydirection_bytime) #bysub&direction was a df, bydirection was a tibble
(Inv_stats$Subject=ifelse(is.na(Inv_stats$Subject), "Overall", Inv_stats$Subject))

str(Inv_stats_bysub_bydirection_bytime)
Inv_stats_bysub_bydirection_bytime=within(Inv_stats_bysub_bydirection_bytime, {
  Direction=factor(Direction)
  Session=factor(Session)
  Subject=factor(Subject)
})
str(Inv_stats_bysub_bydirection_bytime)
InvRT= ggplot(data=Inv_stats_bysub_bydirection_bytime,aes(x=Session, y=logRT_mean, linetype=Subject, color=Direction),+geom_point())
InvRT
##can't get ggplot to work modified many times

#Upright Spaghetti Plot
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

         
#Repeated Measures Anova on Means log RT
InvRT=ezANOVA(dv=logRT_mean, within=c(Direction, Session), wid=Subject, data=Inv_stats_bysub_bydirection_bytime, detailed=TRUE)
(InvRTAnova=InvRT$ANOVA)
#sig effect of Direction and Intercept

#GLMM RT 
#as factors
str(Invtime1_time2_LONG)
Invtime1_time2_LONG=within(Invtime1_time2_LONG, {
  Subject=factor(Subject)
  Session=factor(Session)
  Direction=factor(Direction)
  Trial=factor(Trial)
})
str(Invtime1_time2_LONG)

Inv_RTComp1=lmer(logRT~1+Direction+Session+Direction:Session+(1+Session|Subject), data=Invtime1_time2_LONG)
summary(Inv_RTComp1)
#main effect of Direction
anova(Inv_RTComp1)

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



#################################################################################################

#Recode RT to include only Accurate responses
#Recode RT saved as 0 for nonresponse to NA Time 1
Inv_2=Invtime1_time2_LONG
Inv_2
str(Inv_2)
sum(is.na(Inv_2$logRT)) #20 total
sum(is.na(Inv_2$Comparison.ACC)) #20 total
sum(Inv_2$Comparison.ACC=="0", na.rm=TRUE) #55 total
#should be 75 total NA RT for all the NA ACC+ AC=0 (inaccurate) responses
is.na(Inv_2$logRT)=Inv_2$Comparison.ACC=="0"
sum(is.na(Inv_2$logRT)) #now 75 total 
sum(!is.na(Inv_2$logRT)) #480 RTs responses
sum(!Inv_2$Comparison.ACC==0, na.rm=TRUE)

#grouped by subject & direction only stats (both time 1 and 2 together)
Inv_2_sub_dir=Inv_2 %>%
  group_by(Subject, Direction) %>%
  summarise_at(c("logRT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#grouped by subject, direction, and session stats
Inv_2_sub_dir_time=Inv_2 %>%
  group_by(Subject, Direction, Session) %>%
  summarise_at(c("logRT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#overall time 2 and 1 by direction and Session
Inv_2_dir_time=Inv_2 %>%
  group_by(Direction, Session) %>%
  summarise_at(c("logRT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#put overall and by subjects into one df
Inv_2_stats=bind_rows(Inv_2_sub_dir_time, Inv_2_dir_time) #bysub&direction was a df, bydirection was a tibble
Inv_2_stats$Subject=as.numeric(Inv_2_stats$Subject)
Inv_2_stats$Subject=replace_na(Inv_2_stats$Subject, "Overall")

#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Direction), Inv_2_sub_dir_time, type='l',
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time")

#Repeated Measures Anova on Means log RT (ACC and inacc, without nonresponse)
InvRT2=ezANOVA(dv=logRT_mean, within=c(Direction, Session), wid=Subject, data=Inv_2_sub_dir_time, detailed=TRUE)
(InvRTAnova2=InvRT2$ANOVA)
#sig effect of Direction and Intercept

#GLMM RT ACC and Inacc (without non-responses)
Inv_RTComp2=lmer(logRT~1+Direction+Session+Direction:Session+(1+Session|Subject), data=Inv_2)
summary(Inv_RTComp2)
#main effect of Direction

#adjust trials to compare accurate RT to inacc RT

#Spaghetti plot line for each subject, Inaccurate RT
xyplot(logRT~Session, group=Subject, subset=Comparison.ACC==0, Invtime1_time2_LONG, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time, Incorrect Responses")
xyplot(logRT~Session, group=Comparison.ACC, Invtime1_time2_LONG, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time")



