#Pull together all the data TIME ONE ONLY
#after importing subjects, create long df/tibble with all the subjects
Invtime1all=bind_rows(t1s1, t1s2, t1s3, t1s4, t1s5)
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


#RT ACC responses only
#add column for logtime
Invtime1_time2_LONG$logRT=log(Invtime1_time2_LONG$Comparison.RT)
str(Invtime1_time2_LONG)
sum(is.na(Invtime1_time2_LONG$logRT)) #checking the calculation used NAs from RESP and RT

#Inv grouped stats by sub and direction, accurate RT only
Inv_stats_bysub_bydirection=Invtime1_time2_LONG %>%
  group_by(Subject, Direction) %>%
  summarise_at(c("Comparison.ACC", "logRT"), funs(mean, sd), na.rm=TRUE)

#Inv grouped stats by sub and direction and time, accurate RT only
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
##InvRT= ggplot(data=Inv_time2_stats_bysub_bydirection_bytime,aes(x=Session, y=logRT_mean, linetype=Subject, color=Direction),+geom_line())
##InvRT
##can't get ggplot to work modified many times

#Upright Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Direction=="Up", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Upright Response Time")
#Inverted Spaghetti Plot
xyplot(logRT_mean~Session, groups=Subject, subset=Direction=="Inv", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Inverted Response Time")
#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Direction), Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time")
#plot Subject 1
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="50262", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 1")
#plot Sub 2
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="50202", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 2")
#plot Sub 3
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="50192", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 3")
#Plot Sub 4
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="50312", Inv_stats_bysub_bydirection_bytime, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 4")
#Plot Sub 5
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="50142", Inv_stats_bysub_bydirection_bytime, type=c('p', 'l'), panel.average(),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Mean RT Subject 5")
#Plot Overall Spaghetti
xyplot(logRT_mean~Session, groups=Direction, subset=Subject=="Overall", Inv_stats, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall RT") 

xyplot(logRT_mean~Session, groups=c(Direction), Inv_stats, type=c('p','l'),
                       par.settings=ggplot2like(),axis=axis.grid, ylab="Overall RT", main="Subject 5 & Overall", auto.key = list(space="right", title="Key", cex.title=1.5))


#Inv_stats_bysub_bydirection_bytime$tmpSubject=as.numeric(Inv_stats_bysub_bydirection_bytime$Subject)
#sidebyside=equal.count(Inv_stats_bysub_bydirection_bytime$tmpSubject, number=5, overlap=0)
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'), col.line = c("blue", "green"))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
}
#xyplot(logRT_mean~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key = list(space="top"))

bysubject=factor(Inv_stats_bysub_bydirection_bytime$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubject, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Inv", "Up", "Mean", "Regression")))

         

#Repeated Measures Anova on Means log RT (ACC only)
InvRT=ezANOVA(dv=logRT_mean, within=c(Direction, Session), wid=Subject, data=Inv_time2_stats_bysub_bydirection_bytime, detailed=TRUE)
(InvRTAnova=InvRT$ANOVA)
#sig effect of Direction and Intercept

#GLMM RT (only Acc responses)
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

#################################################################################################

#Recode RT to include all of them (Accurate and Inaccurate)
#Recode RT saved as 0 for nonresponse to NA Time 1
Invtime1all_2=bind_rows(t1s1, t1s2, t1s3, t1s4, t1s5)
Invtime1all_2
#some of the data is saved as Inv some and Inverse, change all to Inv
Invtime1all_2$Direction[Invtime1all_2$Direction=="Inverse"]="Inv"
#separate by time (l=long, s=short, p=practice) and create a subset DF/tibble
Invtime1all_2[["Time"]] #checking the data only
Invtime1LONGall_2=subset(Invtime1all_2, subset=Time=="l")
Invtime1LONGall_2[["Time"]] #recheck to make sure worked
#add time 2
Invtime1_time2_all_2=rbind(Invtime1all_2, t2s1, t2s2, t2s3, t2s4, t2s5)
#some of the data is saved as Inv some and Inverse, change all to Inv
Invtime1_time2_all_2$Direction[Invtime1_time2_all_2$Direction=="Inverse"]="Inv"
#some subjects saved without 2 at the end for time 2
Invtime1_time2_all_2$Subject[Invtime1_time2_all_2$Subject=="5031"]="50312"
Invtime1_time2_all_2$Subject[Invtime1_time2_all_2$Subject=="5020"]="50202"
#subset only the LONG trials
Invtime1_time2_LONG_2=subset(Invtime1_time2_all_2, Time=="l")
#recode non-responses as NA
sum(Invtime1_time2_LONG_2$Comparison.RT==0) #check original format saved as 0
sum(is.na(Invtime1_time2_LONG_2$Comparison.RESP))
is.na(Invtime1_time2_LONG_2$Comparison.RT)=is.na(Invtime1_time2_LONG_2$Comparison.RESP)
sum(Invtime1_time2_LONG_2$Comparison.RT==0) #check function
sum(Invtime1_time2_LONG_2$Comparison.ACC==0, na.rm=TRUE) #check that more inaccurate responses than na RT (getting RT for the reponded ACC responses)

#add logtime
Invtime1_time2_LONG_2$logRT=log(Invtime1_time2_LONG_2$Comparison.RT)
str(Invtime1_time2_LONG_2)
sum(is.na(Invtime1_time2_LONG_2$Comparison.RT))
sum(is.na(Invtime1_time2_LONG_2$logRT))
sum(Invtime1_time2_LONG_2$Comparison.ACC==0) #check that more inaccurate responses than total nonresponse

#grouped by subject & direction only stats (both time 1 and 2 together)
Inv_alltimes_stats_bysub_bydirection_2=Invtime1_time2_LONG_2 %>%
  group_by(Subject, Direction) %>%
  summarise_at(c("logRT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#grouped by subject, direction, and session stats
Inv_time2_stats_bysub_bydirection_bytime_2=Invtime1_time2_LONG_2 %>%
  group_by(Subject, Direction, Session) %>%
  summarise_at(c("logRT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#overall time 2 and 1 by direction and Session
Inv_time2_stats_bydirection_bysession_2=Invtime1_time2_LONG_2 %>%
  group_by(Direction, Session) %>%
  summarise_at(c("logRT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#put overall and by subjects into one df
Inv_time2_stats_2=bind_rows(Inv_time2_stats_bysub_bydirection_bytime_2, Inv_time2_stats_bydirection_bysession_2) #bysub&direction was a df, bydirection was a tibble
Inv_time2_stats_2$Subject=as.numeric(Inv_time2_stats_2$Subject)
Inv_time2_stats_2$Subject=replace_na(Inv_time2_stats_2$Subject, "Overall")

#Spaghetti plot line for each subject and Direction
xyplot(logRT_mean~Session, groups=c(Subject, Direction), Inv_time2_stats_bysub_bydirection_bytime_2, type='l',
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time")

#GLMM RT ACC and Inacc (without non-responses)
#as factors
Invtime1_time2_LONG_2=within(Invtime1_time2_LONG_2, {
  Subject=factor(Subject)
  Session=factor(Session)
  Direction=factor(Direction)
  Trial=factor(Trial)
})
str(Invtime1_time2_LONG_2)

#Repeated Measures Anova on Means log RT (ACC and inacc, without nonresponse)
InvRT2=ezANOVA(dv=logRT_mean, within=c(Direction, Session), wid=Subject, data=Inv_time2_stats_bysub_bydirection_bytime_2, detailed=TRUE)
(InvRTAnova2=InvRT2$ANOVA)
#sig effect of Direction and Intercept

Inv_RTComp2=lmer(logRT~1+Direction+Session+Direction:Session+(1+Session|Subject), data=Invtime1_time2_LONG_2)
summary(Inv_RTComp2)
#main effect of Direction

#adjust trials to compare accurate RT to inacc RT

#Spaghetti plot line for each subject, Inaccurate RT
xyplot(logRT~Session, group=Subject, subset=Comparison.ACC==0, Invtime1_time2_LONG_2, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, ylab="Response Time, Incorrect Responses")
sum(Invtime1_time2_LONG_2$Comparison.ACC==0) 
#75 incorrect observations total [some will be NA]
sum(is.na(Invtime1_time2_LONG_2$Comparison.RT))
#20 are non-responses (coded as NA)