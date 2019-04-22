#All Data DF
N_T2T1_all=bind_rows(N_time1all, t2s1N, t2s2N, t2s3N, t2s4N, t2s5N)

#Variables as Factors for grouping
F_N_T2T1_all=N_T2T1_all
F_N_T2T1_all$Session=factor(F_N_T2T1_all$Session)
F_N_T2T1_all$Subject=factor(F_N_T2T1_all$Subject)
F_N_T2T1_all$Configuration=factor(F_N_T2T1_all$Configuration)
F_N_T2T1_all$TargetLetter=factor(F_N_T2T1_all$TargetLetter)
F_N_T2T1_all$TargetLocation=factor(F_N_T2T1_all$TargetLocation)
str(F_N_T2T1_all)
#remove practice task
F_N_T2T1_all=F_N_T2T1_all[-(1:10),]
#Remove irrelevant variables 
F_N_T2T1_all=subset(F_N_T2T1_all, select= -c(XCoo, YCoo, Block))
#Recode nonresponse as NA for RT and ACC
is.na(F_N_T2T1_all$Stimuli.RT)=F_N_T2T1_all$Stimuli.RT==0
is.na(F_N_T2T1_all$Stimuli.RT) #just as a check
is.na(F_N_T2T1_all$Stimuli.ACC)=is.na(F_N_T2T1_all$Stimuli.RT)
#Recode RT as NA for inaccurate responses
is.na(F_N_T2T1_all$Stimuli.RT)=F_N_T2T1_all$Stimuli.ACC==0
#Group for Configuration stats All
Config_T2T1=subset(F_N_T2T1_all, select= c(Subject, Configuration, Session, Stimuli.ACC, Stimuli.RT))
Config_T2T1=group_by(Config_T2T1, Subject, Session, Configuration)
Config_T2T1_stats_bysubject=summarise(Config_T2T1, AvgAcc= mean(Stimuli.ACC, na.rm=TRUE), AvgRT= mean(Stimuli.RT, na.rm=TRUE))
Config_T2T1_simple_stats3=subset(Config_T2T1, select= -Subject)  
Config_T2T1_simple_stats2=group_by(Config_T2T1_simple_stats3, Configuration, Session) 
Config_T2T1_simple_stats=summarise(Config_T2T1_simple_stats2, AvgAcc= mean(Stimuli.ACC, na.rm=TRUE), AvgRT= mean(Stimuli.RT, na.rm=TRUE))
rm(Config_T2T1_simple_stats2, Config_T2T1_simple_stats3)

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
