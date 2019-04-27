time1_time2_all=rbind(time1all, t2s1, t2s2, t2s3, t2s4, t2s5)
#some of the data is saved as Inv some and Inverse, change all to Inv
time1_time2_all$Direction[time1_time2_all$Direction=="Inverse"]="Inv"
#some subjects saved without 2 at the end for time 2
time1_time2_all$Subject[time1_time2_all$Subject=="5031"]="50312"
time1_time2_all$Subject[time1_time2_all$Subject=="5020"]="50202"
time1_time2_LONG=subset(time1_time2_all, Time=="l")
#Check for lack of response (coded as zero) in RT
time1_time2_LONG$Comparison.RT==0
#Recode as NA so not used in calculations
is.na(time1_time2_LONG$Comparison.RT)=is.na(time1_time2_LONG$Comparison.RESP)
##time1_time2_LONG= mutate(time1_time2_LONG, Comparison.RT=(ifelse(Comparison.RT==0, NA, Comparison.RT)))
time1_time2_LONG$Comparison.RT==0
#Recode ACC values for nonresponse as NA
is.na(time1_time2_LONG$Comparison.ACC)=is.na(time1_time2_LONG$Comparison.RESP)
is.na(time1_time2_LONG$Comparison.ACC)

#grouped by subject & direction only stats (both time 1 and 2 together)
alltimes_stats_bysub_bydirection=time1_time2_LONG %>%
  group_by(Subject, Direction) %>%
  summarise_at(c("Comparison.RT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#grouped by subject, direction, and session stats
time2_stats_bysub_bydirection_bytime=time1_time2_LONG %>%
  group_by(Subject, Direction, Session) %>%
  summarise_at(c("Comparison.RT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#overall time 2 and 1 by direction and Session
time2_stats_bydirection_bysession=time1_time2_LONG %>%
  group_by(Direction, Session) %>%
  summarise_at(c("Comparison.RT", "Comparison.ACC"), funs(mean, sd), na.rm=TRUE)

#put overall and by subjects into one df
time2_stats=bind_rows(time2_stats_bysub_bydirection_bytime, time2_stats_bydirection_bysession) #bysub&direction was a df, bydirection was a tibble
time2_stats$Subject=as.numeric(time2_stats$Subject)
time2_stats$Subject=replace_na(time2_stats$Subject, "Overall")


#explore RT data
str(time2_stats_bysub_bydirection_bytime)
library(plyr)
ddply(time2_stats_bysub_bydirection_bytime, ~Direction*Session, function(data) summary(data$Comparison.RT_mean))
ddply(time2_stats_bysub_bydirection_bytime, ~Direction*Session, summarise, mean.RT=mean(Comparison.RT_mean), sd.RT=sd(Comparison.RT_mean))
hist(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Direction=="Up" & 
                                            time2_stats_bysub_bydirection_bytime$Session=="1",]$Comparison.RT_mean)
hist(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Direction=="Up" & 
                                            time2_stats_bysub_bydirection_bytime$Session=="2",]$Comparison.RT_mean)
hist(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Direction=="Inv" & 
                                            time2_stats_bysub_bydirection_bytime$Session=="1",]$Comparison.RT_mean)
hist(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Direction=="Inv" & 
                                            time2_stats_bysub_bydirection_bytime$Session=="2",]$Comparison.RT_mean)
boxplot(Comparison.RT_mean~Direction*Session, data=time2_stats_bysub_bydirection_bytime, 
        xlab="Direction+Session", ylab="Response Time")
with(time2_stats_bysub_bydirection_bytime, interaction.plot(Direction, Session, Comparison.RT_mean))

#2x2 Repeated Measures Factorial Anova
#test normality
shapiro.test(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Direction=="Up",]$Comparison.RT_mean)
shapiro.test(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Direction=="Inv",]$Comparison.RT_mean)
shapiro.test(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Session=="1",]$Comparison.RT_mean)
shapiro.test(time2_stats_bysub_bydirection_bytime[time2_stats_bysub_bydirection_bytime$Session=="2",]$Comparison.RT_mean)
#pvalue for session 2 was sig!! What should be done?
#test residual normality
m = aov(Comparison.RT_mean~Direction*Session+Error(Subject/(Direction*Session)), data=time2_stats_bysub_bydirection_bytime)
shapiro.test(residuals(m$Subject))
qqnorm(residuals(m$Subject));qqline(residuals(m$Subject))
shapiro.test(residuals(m$"Subject:Direction"))
qqnorm(residuals(m$"Subject:Direction"));qqline(residuals(m$"Subject:Direction"))
shapiro.test(residuals(m$"Subject:Session"))
qqnorm(residuals(m$"Subject:Session"));qqline(residuals(m$"Subject:Session"))
#residuals are all norm

library(ez)
m=ezANOVA(dv=Comparison.RT_mean, within=c(Direction, Session), wid=Subject, data=time2_stats_bysub_bydirection_bytime, detailed=TRUE)
m$Mauchly #answer was null for sphericity
m$ANOVA
#this includes the generalized eta squared effect size

#sphericity corrections
pos=match(m$"Sphericity Correction"$Effect, m$ANOVA$Effect)
m$Sphericity$GGe.DFn=m$Sphericity$GGe * m$ANOVA$DFn[pos] #numerator DF Greenhouse-Geisser
m$Sphericity$GGe.DFd=m$Sphericity$GGe * m$ANOVA$DFd[pos] #denominator DF
m$Sphericity$GGe.DFn=m$Sphericity$GGe * m$ANOVA$DFn[pos] #numerator DF Huynh-Fedlt
m$Sphericity$GGe.DFd=m$Sphericity$GGe * m$ANOVA$DFd[pos] #demoniator DF
m$Sphericity

m$ANOVA

#post hoc t test on sig main effect of Direction
(ttestDirection=t.test(time2_stats_bysub_bydirection_bytime$Comparison.RT_mean~time2_stats_bysub_bydirection_bytime$Direction, data=time2_stats_bysub_bydirection_bytime))
emmip(m, Direction~Session, CIs=TRUE)

#GLMM RT
#as factors
time1_time2_LONG=within(time1_time2_LONG, {
  Subject=factor(Subject)
  Session=factor(Session)
  Direction=factor(Direction)
  Trial=factor(Trial)
})

str(time1_time2_LONG)
#exploratory plots
library(ggplot2)
ggpairs(time1_time2_LONG[,c("Session", "Direction", "Comparison.ACC", "Comparison.RT")], lower=list(combo=wrap("facethist", binwidth=300)), na.rm=TRUE)

tmp<-melt(time1_time2_LONG[c("Session", "Comparison.RT", "Direction")], na.rm = TRUE)
ggplot(tmp, aes(x=tmp$Direction, y=value)) +
  geom_jitter(alpha=.1) +
  geom_violin(alpha=.75)

tmp<-melt(time1_time2_LONG[,c("Comparison.ACC", "Comparison.RT", "Session", "Direction")],
          id.vars="Comparison.ACC", na.rm = TRUE)
ggplot(tmp, aes(factor(Comparison.ACC), y=value, fill=factor(Comparison.ACC))) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free_y")

#mixed effects logistic regression estimate
m<-glmer(Comparison.ACC~Direction*Session+(Direction*Session|Subject), data=time1_time2_LONG,
         family = binomial, control=glmerControl(optimizer = "bobyqa"))
n<-lmer(Comparison.RT~Direction*Session+(Direction*Session|Subject), data=time1_time2_LONG,
        control=lmerControl(optimizer="bobyqa"))

#calculate confidenceIntervals
mci=sqrt(diag(vcov(m)))
(mtab=cbind(Est=fixef(m), LL=fixef(m)-1.96* mci, UL=fixef(m)+1.96*mci))
nci=sqrt(diag(vcov(n)))
(ntab=cbind(Est=fixef(n), LL=fixef(n)-1.96*nci, UL=fixef(n)+1.96*nci))
m
n
#nlme or lme for linear mixed model analysis: use nlme fuction
#with Gerhard did the following-- review later
#install.packages("lme4")
require(lme4)
RTDirSess=lmer(Comparison.RT~Direction*Session+(Direction*Session|Subject), data=time1_time2_LONG)
RTSess=lmer(Comparison.RT~Direction*Session+(Session|Subject), data=time1_time2_LONG)
RT=lmer(Comparison.RT~Direction*Session+(1|Subject), data=time1_time2_LONG)
summary(RTDirSess)
summary(RTSess)
summary(RT)
fit1=lmer(Comparison.RT~Direction*Session+(1|Subject), data=time1_time2_LONG)
fit2=lmer(Comparison.RT~Direction+Session+(1|Subject), data=time1_time2_LONG)
anova(fit1,fit2)
?anova

#Export all the data above so I can send it to colleagues
write_csv(time2_stats, "./time2_allstats.csv")
write_csv(time2_stats_bysub_bydirection_bytime, "./time2_stats_bysub_bydirection_bytime.csv")

                                   