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


#explore data
names(time1LONGall)
summary(time1LONGall)
time1LONGall$Trial=factor(time1LONGall$Trial)
time1LONGall$Subject=factor(time1LONGall$Subject)
time1LONGall$Direction=factor(time1LONGall$Direction)
str(time1LONGall)
ddply(time1LONGall, ~Direction, function(data)summary(data$Comparison.RT))
hist(time1LONGall[time1LONGall$Direction=="Up",]$Comparison.RT)
hist(time1LONGall[time1LONGall$Direction=="Inv",]$Comparison.RT)
plot(data=time1LONGall, Comparison.RT~Direction, xlab="Direction", ylab="Comparison.RT")

#test normality assumption
shapiro.test(time1LONGall[time1LONGall$Direction=="Up",]$Comparison.RT)
shapiro.test(time1LONGall[time1LONGall$Direction=="Inv",]$Comparison.RT)
#p value is sig in both suggesting sig different than normal

#fit the model to lognormal distr
library(goft)
lnorm_test(time1LONGall[time1LONGall$Direction=="Up",]$Comparison.RT)
lnorm_test(time1LONGall[time1LONGall$Direction=="Inv",]$Comparison.RT)
#p value not sig different than lognorm

#compute logcolumn and retest with the correlation error of subjects as well
time1LONGall$log.RT=log(time1LONGall$Comparison.RT)
shapiro.test(time1LONGall[time1LONGall$Direction=="Up",]$log.RT)
shapiro.test(time1LONGall[time1LONGall$Direction=="Inv",]$log.RT)
m=aov(log.RT~Direction+Error(Subject/Direction), data=time1LONGall) #fit the model
shapiro.test(residuals(m$Subject))
qqnorm(residuals(m$"Subject:Direction"));qqline(residuals(m$"Subject:Direction"))
#the log Time residuals not sig different than normal
#shows this is a lognormal function

#test homoscedasticity
library(car)
leveneTest(log.RT~Direction, data=time1LONGall, center=median)
#####################################how can I tell if this is valid???

#reshape to a wide order dt
library(reshape2)
wide.time1LONGall=dcast(time1LONGall, Subject~Trial*Direction, value.var="Comparison.RT")
wide.time1log=dcast(time1LONGall, Subject~Trial*Direction, value.var = "log.RT")
str(wide.time1log)
str(wide.time1LONGall)

##############################repeated measures T Test for time 1 RT using Logtime or time?
#I think because multiple comparison need LMM
#LMMRegression Model Anova for RT
#set sum to zero contrasts for Anova call
contrasts(time1LONGall$Direction)="contr.sum"
contrasts(time1LONGall$Trial)="contr.sum"

#LMM main effect for RT with Trial nested fixed effect and Subject random effect
n=lmer(data=time1LONGall, log.RT~Direction/Trial+(1|Subject))
m=lmer(data=time1LONGall, Comparison.RT~Direction/Trial +(1|Subject))
Anova(m, type=3, test.statistic = "F")
emmip(m, ~Direction, CIs=TRUE)
library(multcomp)
summary(glht(m, lsm(pairwise~Direction/(1|Trial)), test-adjusted(type="holm")))
Anova(n, type=3, test.statistic = "F")
emmip(n, ~Direction, CIs=TRUE)
#seems to be significantly different by direction and direction*Trial interaction (which we don't want, right?)
#check the effect with random effect trial instead of nested effect
m=lmer(data=time1LONGall, Comparison.RT~Direction/(1|Trial) +(1|Subject))
Anova(m, type=3, test.statistic = "F")
################the DF looks too small for this to be correct

#trying again
#none of this works
m=lmer(data=time1LONGall, Comparison.RT~Direction/Trial +(1|Subject))
o=lm(data=time1LONGall, Comparison.RT~Direction/Trial +(1|Subject))
emmeans(o, Comparison.RT~Direction/Trial, contr=c("Up", "Inv"))
emtrends(time1LONGall, pairwise~Direciton/Trial, var=time1LONGall$Comparison.RT)



#GLMMRegression Model Anova for Accuracy




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
