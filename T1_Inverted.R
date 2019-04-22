#Pull together all the data TIME ONE ONLY
#after importing subjects, create long df/tibble with all the subjects
time1all=bind_rows(t1s1, t1s2, t1s3, t1s4, t1s5)
time1all
#some of the data is saved as Inv some and Inverse, change all to Inv
time1all$Direction[time1all$Direction=="Inverse"]="Inv"
#separate by time (l=long, s=short, p=practice) and create a subset DF/tibble
time1all[["Time"]] #checking the data only
time1LONGall=subset(time1all, subset=Time=="l")
time1LONGall[["Time"]] #recheck to make sure worked
#Recode RT saved as 0 for nonresponse to NA
time1LONGall$Comparison.RT==0 #check original format saved as 0
is.na(time1LONGall$Comparison.RT)=is.na(time1LONGall$Comparison.RESP)
time1LONGall$Comparison.RT==0 #check function recoded 
#remove ACC values for the NA RT as these shouldn't be coded
is.na(time1LONGall$Comparison.ACC)=is.na(time1LONGall$Comparison.RESP)

#Time 1 grouped stats by sub and direction
time1_stats_bysub_bydirection=time1LONGall %>%
  group_by(Subject, Direction) %>%
  summarise_at(c("Comparison.RT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#overall time 1 by direction
time1_stats_bydirection=time1LONGall %>%
  group_by(Direction) %>%
  summarise_at(c("Comparison.RT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#put overall and by subj into a df
time1_stats=bind_rows(time1_stats_bysub_bydirection, time1_stats_bydirection) #bysub&direction was a df, bydirection was a tibble
time1_stats$Subject=replace_na(time1_stats$Subject, "Overall")

#GLMMRegression Model Anova for Accuracy

#GLMMRegression Model Anova for RT

#T Test for Accuracy

#Check the Accuracy (ttest) Effect size with 

#T Test for RT

#Other Tables
#Group all data by Subject, then Direction (for Teena)
#Time1GroupedSubjDir=group_by(time1LONGall, Subject, Direction)
#Summary statistics with mean RT and porportion ACC for the grouped data (for Teena)
#TableSummaryTime1_Subj_Direction=summarize(Time1GroupedSubjDir, mean(Comparison.RT, na.rm = TRUE), mean(Comparison.ACC, na.rm=TRUE))

#Export all the data above so I can send it to colleagues
write_csv(time1_stats, "./All_Summary_Stats_T1.csv")
write_csv(time1LONGall, "./time1LONGall_CORRECTED.csv")
