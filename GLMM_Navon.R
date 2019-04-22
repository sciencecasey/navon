#learned new regression scripts
#trying something different
require(tidyverse)
require("haven")
require(lsr)
require(dplyr)
install.packages("car")
install.packages("alr3")
install.packages("faraway")
library("car")
library("faraway")
library("alr3")

#cleanup variables from old data
rm(attempting2, attempts, Config_Time1, Config_Time1_simple_stats, Config_Time1_stats_bysubject, etasq_lmTime1ACCbyDirection_Subj, 
   etasq_lmTime1RTbyDirection_Subj, F_N_T2T1_all, F_N_time1all, fit1, fit2, GLMBothSessionsRTbyDirectionbySubjectbySession, 
   GLMMBothSessionsRTwithComplexModel, GLMSummaryBothSesions, grouped_bySubject_direction, lmBothSessionsRTbyDirectionbySubjbySession, 
   lmTime1ACCAveragebyDirection, lmTime1ACCbyDirection, lmTime1ACCbyDirection_Sub, lmTime1RTbyDirection_Subj, N_time1all, 
   select_data_stats, summary_stats, summary_stats_comparison, summary_stats_comparison2, TableSummaryTime1_Subj_Direction, TargetLocation_T2T1, 
   TargetLocation_T2T1_simple_stats, TargetLocation_T2T1_stats_bysubject, time1_time2_all, time1_time2_LONG, time1_time2_LONG2, time1all,
   time1AllACCavg, Time1GroupedSubjDir, time1LONGall, time1RTbysubj, ttestTime1ACCbyDirection, ttestTime1RTbyDirection, numcol, time1LONGall2, TargetLocation_Time1,
   TargetLocation_Time1_simple_stats, TargetLocation_Time1_stats_bysubject, RTT2T1_UP, factored_time1_time2_LONG)


#separating by switch or nonswitch
switch_all=mutate(Config_T2T1, Switch= ifelse(Configuration=="GLS" | Configuration=="LGS", "Switch", "NotSwitch"))
switch_all$Switch=as.factor(switch_all$Switch)


###ACC by switch and non-switch, not accounting for T1 T2
#lm for above


###RT by switch and non-swtich, not accounting for T1 T2
#lm for above


###ACC by switch and non-switch accounting for T2 and T1
#lm for above


###RT by switch and non-swtich accounting for T1 T2
#lm for above

