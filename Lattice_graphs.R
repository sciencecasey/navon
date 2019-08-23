#Original Graphs Below:

#Navon ACC
#Switch/Nonswitch
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
#xyplot(logRT_mean~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$Stimuli.ACC_mean, layout=c(5,1), aspect=1.5, main="Subject ACC Inv v Up", xlab="ACC", ylab="Session", panel=mypanel, auto.key = list(space="top"))

colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubjectN=factor(N_switch_stats$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(Stimuli.ACC_mean~Session|bysubjectN, groups=Switch, data=N_switch_stats, 
       h=N_switch_stats$Stimuli.ACC_mean, layout=c(5,1), aspect=1.5, 
       main="Subject ACC Switch v NonSwitch", xlab="Session", ylab="ACC", panel=mypanel, 
       auto.key=keylist)

##Local/Global
xyplot(Stimuli.ACC_mean~Session|TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, 
       auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")

xyplot(Stimuli.ACC_mean~TargetLocation|Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
       auto.key =list(space="right"), type=c("p", "l"), main="Global and Local Processing by Session")

#in panels by subj with regression and mean lines
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
Tlockeylist
bysubjectGlobal=factor(fn_tlocation_stats$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(Stimuli.ACC_mean~Session|bysubjectGlobal, groups=TargetLocation, data=fn_tlocation_stats, 
       h=fn_tlocation_stats$Stimuli.ACC_mean, layout=c(5,1), aspect=1.5, 
       main="Subject ACC Global v Local", xlab="Session", ylab="ACC", panel=mypanel, 
       auto.key=Tlockeylist)


#ACC Inv. Faces
mypanel=function(x,y,h, k){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubject=factor(Inv_stats$Subject, levels = c(1,2,3,4,5), labels = c("1", "2", "3", "4", "5"))
xyplot(Comparison.ACC_mean~Session|bysubject, groups=Direction, data=Inv_stats,
       h=Inv_stats$Comparison.ACC_mean, layout=c(5,1), aspect=1.5, 
       main="Subject Accuracy Inv v Up", xlab="Session", 
       ylab="Proportion Accurate Responses", panel=mypanel, auto.key=keylist)


#RT Inv Faces
Inv_stats$Session=as.factor(Inv_stats$Session)
mypanel=function(x,y,h){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), 
             columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubjectN=factor(N_switch_stats$Subject, levels = c(1,2,3,4,5), 
                  labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubjectN, groups=Switch, data=N_switch_stats, 
       h=N_switch_stats$logRT_mean, layout=c(5,1), aspect=1.5, 
       main="Subject Response Times Switch v NonSwitch", xlab="RT (ms)", 
       ylab="Session", panel=mypanel, auto.key=keylist)













#########################################Another Option!!



#Navon RT
xyplot(logRT_mean~Session|bysubject, 
       groups=Direction, 
       data=Inv_stats_bysub_bydirection_bytime, 
       type=c('p','l'), 
       h=N_switch_stats$logRT_mean,
       layout=c(5,1), aspect=1.5,
       par.settings=ggplot2like(),
       axis=axis.grid, 
       ylab="Response Time", 
       auto.key = list(space="right"),
       main="Response Time by Subject",
       scales=list(alternating=FALSE))

#trying to add regression line and mean line to above
xyplot(logRT_mean~Session|bysubject, 
       groups=Direction, 
       data=Inv_stats_bysub_bydirection_bytime, 
       type='l', #this isn't working
       h=N_switch_stats$logRT_mean,
       #layout=c(5,1), aspect=1.5,
       par.settings=ggplot2like(),
       axis=axis.grid, 
       ylab="Response Time", 
       #auto.key = list(space="right"),
       main="Response Time by Subject",
       scales=list(alternating=FALSE),
       auto.key=list(space="right", ),
       panel=function(x,y,h,...){
         panel.superpose(x,y,..., col = c("purple","blue", "green"))
         panel.lmline(x,y,..., col="green")
         #panel.ablineq(mean(h),..., lty=1, col.line="red")
       })
       panel = function(x, y, h, subscripts, groups) {
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=1, col="red")
         panel.superpose(x,y,subscripts=groups[subscripts], groups, lty=1, col=c("blue", "pink"))
         #llines(x, y, subscripts, groups, lty=1, col=c("blue", "pink"))
       })

xyplot(logRT_mean~Session|Subject, 
       data=Inv_stats_bysub_bydirection_bytime, 
       groups=Direction, 
       h=Inv_stats_bysub_bydirection_bytime$logRT_mean,
       auto.key =list(space="bottom"), #ltext(labels=c("Inverse", "Up", "Regression", "Average"))), 
       main="Response Times by Subject", 
       ylab = "Average RT",
       axis=axis.grid,
       layout=c(5,1),
       scales=list(alternating=FALSE),
       panel = function(x, y, h, subscripts, groups) {
         panel.superpose(x,y,subscripts=groups[subscripts], 
                         groups=groups[subscripts], lty=c(1,5), col.points =c("blue", "pink"))
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=2, col="red")
         #llines(x, y, subscripts, groups, col=c("blue", "pink"))
         #ltext(x = x, y = y,labels = groups, cex=1,
               #fontfamily = "HersheySans", type=c("p", "l"))
       })

xyplot(logRT_mean~Session|bysubject, 
       groups=Direction, data=Inv_stats_bysub_bydirection_bytime, 
       type=c('p','l'), 
       h=N_switch_stats$logRT_mean,
       layout=c(5,1), aspect=1.5,
       par.settings=ggplot2like(),
       axis=axis.grid, 
       ylab="Response Time", 
       auto.key = list(space="right"),
       panel = function(x, y, h, subscripts, groups) {
         panel.abline(mean(h), lty=2, col="red") #not working
         panel.superpose(x,y,subscripts=groups[subscripts], groups)
         ltext(x = x, y = y, h=mean, labels = groups[subscripts], cex=1,
               fontfamily = "HersheySans")
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
       },
       main="Response Time by Subject")


xyplot(logRT_mean~Session|bysubject, 
       groups=Direction, data=Inv_stats_bysub_bydirection_bytime, 
       type=c('p','l'), 
       h=N_switch_stats$logRT_mean,
       layout=c(5,1), aspect=1.5,
       par.settings=ggplot2like(),
       axis=axis.grid, 
       ylab="Response Time", 
       auto.key = list(space="right"),
       panel = function(x, y, h, subscripts, groups) {
         ltext(x = x, y = y, h=mean, labels = groups[subscripts], cex=1,
               fontfamily = "HersheySans")
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=2, col="red")
         panel.superpose(x,y,subscripts=groups[subscripts], groups)
       },
       main="Response Time by Subject")

## Works!! Getting dots and/or text
xyplot(Stimuli.ACC_mean~Session|TargetLocation, 
       data=fn_tlocation_bysubj_bytime, 
       groups=bysubjectGlobal, 
       h=fn_tlocation_bysubj_bytime$Stimuli.ACC_mean,
       auto.key =list(space="right"), 
       type=c("p", "l"), 
       main="Global v. Local Processing Accuracy", 
       ylab = "Average Accuracy",
       scales=list(alternating=FALSE),
       panel = function(x, y, h, subscripts, groups) {
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=2, col="red")
         panel.superpose(x,y,subscripts=groups[subscripts], groups, lty=1, col=c("blue", "pink", "green", "red", "yellow"))
         #llines(x, y, subscripts, groups, col=c("blue", "pink", "green", "red", "yellow"))
         ltext(x = x, y = y,labels = groups, cex=1,
               fontfamily = "HersheySans")
       })

#without text
xyplot(Stimuli.ACC_mean~Session|TargetLocation, 
       data=fn_tlocation_bysubj_bytime, 
       groups=bysubjectGlobal, 
       h=fn_tlocation_bysubj_bytime$Stimuli.ACC_mean,
       auto.key =list(space="right"), 
       type=c("p", "l"),
       main="Global v. Local Processing Accuracy", 
       ylab = "Average Accuracy",
       scales=list(alternating=FALSE),
       panel = function(x, y, h, subscripts, groups) {
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=1, col="red")
         panel.superpose(x,y,subscripts=groups[subscripts], groups, lty=1, col=c("blue", "pink", "green", "red", "yellow"))
         #llines(x, y, subscripts, groups, lty=1, col=c("blue", "pink", "green", "red", "yellow"))
       })


#Plot Target Location by Session
(ses1=xyplot(logRT_mean~TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, 
             subset=Session=="1",
             type=c("p","l"), ylim = c(6,7), main="RT by Location"))
(ses2=xyplot(logRT_mean~TargetLocation, data=fn_tlocation_bysubj_bytime, groups=Subject, 
             subset=Session=="2", auto.key=list(space="right"), type=c("p", "l"),
             ylim = c(6,7), pch=24, main="RT by Location"))
key=list(space="right", column=2, 
         points=list(pch=c(21, 24), col=c("blue", "pink", "green", "red", "yellow")), 
         text=c(levels(fn_tlocation_bysubj_bytime$Subject), 
                levels(fn_tlocation_bysubj_bytime$Session)))
plot(ses2+ses1) 
#I want to learn to add symbols showing the triangles are session 2 and circles session 1 if desired
(glob=xyplot(logRT_mean~Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
             subset=TargetLocation=="G", auto.key = list(space="right"), 
             type=c("p","l"), ylim = c(6,7),main="RT, Global"))
(loc=xyplot(logRT_mean~Session, data=fn_tlocation_bysubj_bytime, groups=Subject, 
            subset=TargetLocation=="L", auto.key=list(space="right"), type=c("p", "l"),
            ylim = c(6,7), pch=24, main="RT, Local"))
#mycols=c("pink", "blue", "yellow", "brown", "light green")
plot(glob+loc, cols=cols, main="Global and Local Response Times by Session")# %>%
#  legend("right", legend = 
#         paste(c("1 Global", "2 Global","3 Global", "4 Global", "5 Global"), 
#               c("1 Local", "2 Local", "3 Local", "4 Local", "5 Local"), sep=";"), 
#       col=rep(cols, times=2), pch=rep(c(16, 24), each=5), bty="n", 
#       ncol=2, cex=.7, pt.cex = .7)
#key not functioning correctly nor is the title




#Inverted Faces
colors=c("blue", "black") #not sure if this does anything
mypanel=function(x,y,h, subscripts){
  panel.xyplot(x, y, pch =1, col = "black") #want to separate into 2 colors#####
  panel.xyplot(Inv_stats$Session[subscripts], y, col="blue", pch=2)
  panel.lmline(x, y, lty=3, lwd=1, col="purple") #works as desired here and key
  panel.grid(h=-1, v=-1) #good
  panel.abline(mean(h), lty=2, col="red") #works well here and key
  #panel.superpose(x, y, panel.groups)
  #col.line=colors #didn't do anything
  #llines(x, y, col.line=colors) #only blue shows, not sure if this does anything
}
xyplot(logRT_mean~Session|sidebyside, groups=Direction, data=Inv_stats_bysub_bydirection_bytime,  h=Inv_stats_bysub_bydirection_bytime$logRT_mean, layout=c(5,1), aspect=1.5, main="Subject Response Times Inv v Up", xlab="RT (ms)", ylab="Session", panel=mypanel, auto.key = list(space="top"))


xyplot(logRT_mean~Session|bysubject, 
       groups=Direction, #didn't do anything....
       data=Inv_stats, 
       type='l',
       main="Subject Response Times Inv v Up", 
       ylab="RT (ms)", 
       xlab="Session",
       auto.key=list(space="top"),
       panel=mypaneltest(x))

mypaneltest=function(x,y){
  panel.lmline(lty=3, lwd=1, col="purple")
  panel.abline(mean(Inv_stats$logRT_mean), lty=2, col="red")
  print(x)
}




















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


keylist=list(space="right", 
             #col=c("blue", "green", "red", "purple"), 
             columns=1, 
             #p
             text=c("Inv", "Up", "Mean", "Regression"))
bysubject=factor(Inv_stats$Subject, 
                 levels = c(1,2,3,4,5), 
                 labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubject, 
       groups=Direction, #didn't do anything....
       #allow.multiple = TRUE #didn't change anything
       data=Inv_stats, 
       h=Inv_stats$logRT_mean, 
       layout=c(5,1), 
       aspect=1.5, 
       main="Subject Response Times Inv v Up", 
       ylab="RT (ms)", 
       xlab="Session", 
       panel=mypanel, 
       auto.key=keylist)

mypaneltest=function(x,y){
  panel.lmline(lty=3, lwd=1, col="purple")
  panel.abline(mean(Inv_stats$logRT_mean), lty=2, col="red")
  print(x)
}




mypanel=function(x,y,h,groups,subscripts){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.superpose(x,y,groups = groups, subsc)
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  panel.groups=group
  llines(x, y, col=c("blue", "green"))
}
colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), 
             columns=1, text=c("Inv", "Up", "Mean", "Regression"))
bysubjectN=factor(N_switch_stats$Subject, levels = c(1,2,3,4,5), 
                  labels = c("1", "2", "3", "4", "5"))
xyplot(logRT_mean~Session|bysubjectN, groups=Switch, data=N_switch_stats, 
       h=N_switch_stats$logRT_mean, layout=c(5,1), aspect=1.5, 
       main="Subject Response Times Switch v NonSwitch", xlab="RT (ms)", 
       ylab="Session", panel=mypanel, auto.key=keylist)

