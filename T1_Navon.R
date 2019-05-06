#Modeled after Kerwin Analysis but only switch/nonswitch 
#import datasets
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


#Pull together all the data TIME ONE ONLY
#after importing subjects, create long df/tibble with all the subjects
N_time1all=bind_rows(t1s1N, t1s2N, t1s3N, t1s4N, t1s5N)
N_time1all
#Replace null values within $Session
N_time1all$Session=replace_na(N_time1all$Session, 1)

#set relevant variables as factors
names=c('Session', 'Subject', 'Configuration', 
        'TargetLetter', 'TargetLocation')
N_time1all[,names]=lapply(N_time1all[,names], factor)
str(N_time1all)
F_N_time1all=N_time1all

#Removeirrelevant variables 
F_N_time1all=subset(F_N_time1all, select= -c(XCoo, YCoo, Block, Session))
#Check for nonresponse in RT and recode as NA
F_N_time1all$Stimuli.RT==0
F_N_time1all=mutate(F_N_time1all, Stimuli.RT=ifelse(Stimuli.RT==0, NA, Stimuli.RT))
#recode the nonresponse ACC values as NA
is.na(F_N_time1all$Stimuli.ACC)=is.na(F_N_time1all$Stimuli.RT)

#Group for Configuration stats Time 1
Config_Time1=subset(F_N_time1all, select= c(Subject, Configuration, Stimuli.ACC, Stimuli.RT))
Config_Time1$Subject=as.character(Config_Time1$Subject)
Config_Time1$Subject=as_factor(Config_Time1$Subject)
Config_Time1=group_by(Config_Time1, Subject, Configuration)
#make switch/nonswitch condition
Config_Time1$Switch=ifelse(Config_Time1$Configuration=="GLS" | Config_Time1$Configuration=="LGS", "switch", "nonswitch")
Config_Time1$Switch=factor(Config_Time1$Switch)
(Config_Time1_overallstats=summarise(Config_Time1, AvgAcc= mean(Stimuli.ACC, na.rm = TRUE), AvgRT= mean(Stimuli.RT, na.rm = TRUE)))
(Config_Time1_simple_stats3=subset(Config_Time1, select= -Subject))  


#Group for Global/Local stats Configuration Time 1
TargetLocation_Time1=subset(F_N_time1all, select= c(Subject, TargetLocation, Stimuli.ACC, Stimuli.RT))
TargetLocation_Time1$Subject=factor(TargetLocation_Time1$Subject)
TargetLocation_Time1=group_by(TargetLocation_Time1, Subject, TargetLocation)
TargetLocation_Time1_stats_bysubject=summarise(TargetLocation_Time1, AvgAcc=mean(Stimuli.ACC, na.rm=TRUE), AvgRT=mean(Stimuli.RT, na.rm=TRUE))
TargetLocation_Time1_simple_stats3=subset(TargetLocation_Time1, select= -Subject)
TargetLocation_Time1_simple_stats2=group_by(TargetLocation_Time1_simple_stats3, TargetLocation)
TargetLocation_Time1_simple_stats=summarise(TargetLocation_Time1_simple_stats2, AvgAcc=mean(Stimuli.ACC, na.rm=TRUE), AvgRT=mean(Stimuli.RT, na.rm=TRUE))
rm(TargetLocation_Time1_simple_stats2, TargetLocation_Time1_simple_stats3)

#Print the tables for Simple Stats
write_csv(TargetLocation_Time1_simple_stats, "./TargetLocation_Time1_simple_stats_CORRECTED.csv")
write_csv(TargetLocation_Time1_stats_bysubject, "./TargetLocation_Time1_stats_bysubject_CORRECTED.csv")
write_csv(Config_Time1_simple_stats, "./Config_Time1_simple_stats_CORRECTED.csv")
write_csv(Config_Time1_stats_bysubject, "./Config_Time1_stats_bysubject_CORRECTED.csv")

#Check Stats for index, middle finger bias
#TargetLetter_Time1

