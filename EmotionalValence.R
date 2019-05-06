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
length(t2s1E$Video.Time)
t2s1E$Subject=rep(50262, times=795)
t2s1E$Session=rep(2, times=795)
str(t2s1E)
length(t1s2E$Video.Time)
t1s2E$Subject=rep(50202, 836)
t1s2E$Session=rep(1, 836)
str(t1s2E)
t1s2E$Valence=as.numeric(t1s2E$Valence)
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
EAll=rbind(t1s1E, t2s1E, t1s2E, t2s2E, t1s3E, t2s3E, t1s4E, t2s4E, t1s5E, t2s5E)
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

#group stats by 
EALL_bysession=EAll %>%
  group_by(Session) %>%
  summarise_at("Valence", funs(mean, sd), na.rm=TRUE)
EALL_bysession

#add overall to df by sub
EAll_stats=bind_rows(EAll_bysub_bysession, EALL_bysession)
EAll_stats$Subject=ifelse(is.na(EAll_stats$Subject), "Overall", EAll_stats$Subject)

#t test average valence with a one tailed Cohen's D effect size
t.test(Valence~Session, alternative = "less", data=EAll)
cohensD(Valence~Session, data=EAll)

#GLMM
str(EAll)
EV_lmer=lmer(Valence~Session+(1+Session|Subject), data=EAll)
summary(EV_lmer)
#EV_lmer=lmer(Valence~Session+(Session|Subject), data=EAll)   Same formula; correlated random intercept and slope


#reprocess only looking at natural faces (no cross hair or scrambled)

