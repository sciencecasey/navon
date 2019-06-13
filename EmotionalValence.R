t1s1E=read.csv("50262_emovalence_1.csv")
t2s1E=read.csv("50262_emovalence_2.csv")
t1s2E=read.csv("50202_emovalence_1.csv")
t2s2E=read.csv("50202_emovalence_2.csv")
t1s3E=read.csv("50192_emovalence_1.csv")
t2s3E=read.csv("50192_emovalence_2.csv")
t1s4E=read.csv("50132_emovalence_1.csv")
t2s4E=read.csv("50132_emovalence_2.csv")
t1s5E=read.csv("50142_emovalence_1.csv")
t2s5E=read.csv("50142_emovalence_2.csv")


#add participant and trial
length(t1s1E$Video.Time)
t1s1E$Subject=rep(50262, times=745)
t1s1E$Session=rep(1, times=745)
str(t1s1E)
t1s1E$Valence=as.numeric(as.character(t1s1E$Valence))
str(t1s1E)
sum(is.na(t1s1E$Valence)) #only 3
length(t2s1E$Video.Time)
t2s1E$Subject=rep(50262, times=795)
t2s1E$Session=rep(2, times=795)
str(t2s1E)
length(t1s2E$Video.Time)
t1s2E$Subject=rep(50202, 836)
t1s2E$Session=rep(1, 836)
str(t1s2E)
t1s2E$Valence=as.numeric(as.character((t1s2E$Valence)))
sum(is.na(t1s2E$Valence)) #only 2
str(t1s2E)
length(t2s2E$Video.Time)
t2s2E$Subject=rep(50202, 780)
t2s2E$Session=rep(2, 780)
str(t2s2E)
length(t1s3E$Video.Time)
t1s3E$Subject=rep(50192, 768)
t1s3E$Session=rep(1, 768)
str(t1s3E)
length(t2s3E$Video.Time)
t2s3E$Subject=rep(50192, 725)
t2s3E$Session=rep(2, 725)
str(t2s3E)
length(t1s4E$Video.Time)
t1s4E$Subject=rep(50132, 711)
t1s4E$Session=rep(1, 711)
str(t1s4E)
length(t2s4E$Video.Time)
t2s4E$Subject=rep(50132, 711)
t2s4E$Session=rep(2, 711)
str(t2s4E)
length(t1s5E$Video.Time)
t1s5E$Subject=rep(50142, 866)
t1s5E$Session=rep(1, 866)
str(t1s5E)
length(t2s5E$Video.Time)
t2s5E$Subject=rep(50142, 728)
t2s5E$Session=rep(2, 728)
str(t2s5E)

#combine particpant DFs
EAll=bind_rows(t1s1E, t2s1E, t1s2E, t2s2E, t1s3E, t2s3E, t1s4E, t2s4E, t1s5E, t2s5E)
warnings()
str(EAll)
EAll=within(EAll, {Subject=factor(Subject)
   Session=factor(Session)
})
str(EAll)

#group statistics by subject and session
EAll_bysub_bysession=EAll %>%
  group_by(Subject, Session) %>%
  summarise_at("Valence", funs(mean, sd), na.rm=TRUE)
EAll_bysub_bysession

#group stats by Session
EALL_bysession=EAll %>%
  group_by(Session) %>%
  summarise_at("Valence", funs(mean, sd), na.rm=TRUE)
EALL_bysession

#add overall to df by sub
EAll_stats=bind_rows(EAll_bysub_bysession, EALL_bysession)
EAll_stats$Subject=ifelse(is.na(EAll_stats$Subject), "Overall", EAll_stats$Subject)

#t test average valence with a one tailed Cohen's D effect size
#t.test(Valence~Session, alternative = "less", data=EAll, na.rm=TRUE) #this had too mult comparisons
t.test(mean~Session, alternative="less", data=EAll_bysub_bysession, na.rm=TRUE, paired=TRUE)
cohensD(mean~Session, data=EAll_bysub_bysession)

#plot for viewing
xyplot(mean~Session, EAll_bysub_bysession, groups = Subject, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, auto.key = TRUE, main="Average Valence by Subject", ylab="Average Valence") 

#GLMM
str(EAll)
EV_lmer=lmer(Valence~(Session|Subject), data=EAll)
summary(EV_lmer)
#EV_lmer=lmer(Valence~Session+(Session|Subject), data=EAll)   Same formula; correlated random intercept and slope


#reprocess only looking at natural faces (no cross hair or scrambled)
#make DF
EAll=rbind(t1s1E, t2s1E, t1s2E, t2s2E, t1s3E, t2s3E, t1s4E, t2s4E, t1s5E, t2s5E)
str(EAll)
EAll=within(EAll, {Subject=factor(Subject)
Session=factor(Session) 
})
str(EAll)
#test how to change Video Time to a numeric
EAllFilt=EAll
class(EAllFilt$Video.Time)
EAllFilt$Video.Time=as.character(EAllFilt$Video.Time)
str(EAllFilt)
head(EAllFilt$Video.Time)
tail(EAllFilt$Video.Time)
options(digits.secs=2)
test=as.POSIXct(as.character(EAllFilt$Video.Time))
str(test)
class(test)
head(test)
tail(test)
test1=strftime(test1, format='%M:%OS2')
class(test1)
tail(test1)
test2=hms(EAllFilt$Video.Time)
class(test2)
str(test2)
head(test2)
tail(test2)
test3=as.duration(test2, dminutes=test2[2],dseconds=test2[4])
class(test3)
str(test3)
tail(test3)
test4=strptime(EAllFilt$Video.Time, '%M:%SO2')
class(test4)
str(test4)
head(test4)
tail(test4)
test5=as.chron(times(EAllFilt$Video.Time, format="m:s"))
test6=period_to_seconds(hms(EAllFilt$Video.Time)) #this works but saves as milliseconds, not seconds
class(test6)
head(test6)
tail(test6) #this one is best!

#bind the MS data to the DF
str(EAll)
EAllFilt=data.frame(cbind("Subject"= EAll$Subject, "Session"= EAll$Session, "TimeMS"=test6, "Valence"=EAll$Valence))
str(EAllFilt)
EAllFilt=within(EAllFilt, {
  Subject=factor(Subject)
  Session=factor(Session)
})
str(EAllFilt) #7665 rows total
#Select only the times of interest
#31.0-48.1 #1860-2886ms
#77.8-95.0 #4668-5700ms
#125.0-142.0 # 7500-8250ms
#we only use 2.5 minutes of data
#2.5min *60*60=9000 ms
EAllFilt=filter(EAllFilt, TimeMS<=8250) #only 8250ms
str(EAllFilt) #now only has 6990 rows
EAllFilt=filter(EAllFilt, (TimeMS>=1860 & TimeMS<=2886) | (TimeMS>=4668 & TimeMS<=5700) | (TimeMS>=7500 & TimeMS<=8250))
str(EAllFilt) #now has 2400 obs
head(EAllFilt)
tail(EAllFilt)

#group statistics by subject and session
EFilt_stats_bysub_bysession= EAllFilt %>%
  group_by(Subject, Session) %>%
  summarise_at("Valence", funs(mean, sd), na.rm=TRUE)
EFilt_stats_bysub_bysession

#group stats by Session
EFilt_stats_bysession= EAllFilt %>%
  group_by(Session) %>%
  summarise_at("Valence", funs(mean,sd), na.rm=TRUE)
EFilt_stats_bysession

#add overall to df by sub
EFilt_stats=bind_rows(EFilt_stats_bysub_bysession, EFilt_stats_bysession)
EFilt_stats$Subject=ifelse(is.na(EFilt_stats$Subject), "Overall", EFilt_stats$Subject)
EFilt_stats

#plot for viewing
xyplot(mean~Session, EFilt_stats_bysub_bysession, groups = Subject, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, auto.key = TRUE, main="Average Valence by Subject", ylab="Average Valence") 


#t test average valence with a one tailed Cohen's D effect size
EFilt_stats_bysub_bysessionWIDE=dcast(EFilt_stats_bysub_bysession, Subject~Session, value.var = "mean")
EFilt_stats_bysub_bysessionWIDE$Sess1=EFilt_stats_bysub_bysessionWIDE$`1`
EFilt_stats_bysub_bysessionWIDE$Sess2=EFilt_stats_bysub_bysessionWIDE$`2`
t.test(EFilt_stats_bysub_bysessionWIDE$Sess1, EFilt_stats_bysub_bysessionWIDE$Sess2, paired=TRUE, alternative="less")
cohensD(EFilt_stats_bysub_bysessionWIDE$Sess1, EFilt_stats_bysub_bysessionWIDE$Sess2)
#check above
EFilt_stats_bysub_bysessionWIDE$Difference=(EFilt_stats_bysub_bysessionWIDE$Sess2)-(EFilt_stats_bysub_bysessionWIDE$Sess1) 
t.test(EFilt_stats_bysub_bysessionWIDE$Difference, alternative = "less")
#GLMM
(EFilt_glmm=lmer(Valence~Session+(1+Session|Subject), data=EAllFilt))
summary(EFilt_glmm)

