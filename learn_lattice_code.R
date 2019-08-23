#from
#http://stat688.bio5.org/sites/default/files/fall2014/graphics-present.R
data(Cars93, package = "MASS") 
table(Cars93$Cylinders) 
sup.sym <- Rows(trellis.par.get("superpose.symbol"), 1:5) 
str(sup.sym) 
names(Cars93)
str(Cars93)
summary(Cars93)
## Figure 9.1 
xyplot(Price ~ EngineSize | reorder(AirBags, Price), data = Cars93, groups = Cylinders, 
       subset = Cylinders != "rotary", scales = list(y = list(log = 2, tick.number = 3)), 
       xlab = "Engine Size (litres)", ylab = "Average Price (1000 USD)", 
       key = list(text = list(levels(Cars93$Cylinders)[1:5]), points = sup.sym, space = "right")) 
## Figure 9.1 (alternative, using auto.key) 
xyplot(Price ~ EngineSize | reorder(AirBags, Price), 
       data = Cars93, groups = Cylinders, subset = Cylinders != "rotary", 
       scales = list(y = list(log = 2, tick.number = 3)), 
       xlab = "Engine Size (litres)", ylab = "Average Price (1000 USD)", 
       auto.key = list(text = levels(Cars93$Cylinders)[1:5], space = "right", points = TRUE)) 
## Figure 9.1 (yet another alternative, using drop=TRUE) 
xyplot(Price ~ EngineSize | reorder(AirBags, Price), data = subset(Cars93, Cylinders != "rotary"), 
       groups = Cylinders[, drop = TRUE], scales = list(y = list(log = 2, tick.number = 3)), 
       xlab = "Engine Size (litres)", ylab = "Average Price (1000 USD)", 
       auto.key = list(space = "right")) 

my.pch <- c(21:25, 20) 
my.fill <- c("transparent", "grey", "black") 

## Figure 9.2 
with(Cars93, 
     xyplot(Price ~ EngineSize, scales = list(y = list(log = 2, tick.number = 3)), 
                    panel = function(x, y, ..., subscripts) { 
                      pch <- my.pch[Cylinders[subscripts]] 
                      fill <- my.fill[AirBags[subscripts]]
                      panel.xyplot(x, y, pch = pch, fill = fill, col = "black")
                      }, 
            key = list(space = "right", text = list(levels(Cylinders)), points = list(pch = my.pch), 
                       text = list(levels(AirBags)), points = list(pch = 16, fill = my.fill),
                       rep = FALSE)))
                  
     
#color version, not shown in book
my.pch2 <- c(21:25, 20)
my.fill2 <- c("orange", "skyblue", "lightgreen")
     
(col.compkey <- 
  with(Cars93, xyplot(Price ~ EngineSize, scales = list(y = list(log = 2, tick.number = 3)),
                      panel = function(x, y, ..., subscripts) {
                        pch <- my.pch2[Cylinders[subscripts]]
                        fill <- my.fill2[AirBags[subscripts]]
                        panel.xyplot(x, y, pch = pch, fill = fill, col = fill)
                        },
                      key = list(space = "right", text = list(levels(Cylinders)), points = list(pch = my.pch2), 
                                 text = list(levels(AirBags)), points = list(pch = 16, col = my.fill2),
                                 rep = FALSE))))

## Figure 9.3 
hc1 <- hclust(dist(USArrests, method = "canberra"))
hc1 <- as.dendrogram(hc1)
ord.hc1 <- order.dendrogram(hc1)
hc2 <- reorder(hc1, state.region[ord.hc1])
ord.hc2 <- order.dendrogram(hc2)
library(latticeExtra)
region.colors <- trellis.par.get("superpose.polygon")$col
levelplot(t(scale(USArrests))[, ord.hc2], 
          scales = list(x = list(rot = 90)),
          colorkey = FALSE,
          legend =
            list(right =
                   list(fun = dendrogramGrob,
                        args =
                          list(x = hc2, ord = ord.hc2,
                               side = "right", size = 10, size.add = 0.5,
                               add = list(rect =
                                            list(col = "transparent",
                                                 fill = region.colors[state.region])),
                               type = "rectangle"))))



#Using Iris: 
#from https://stackoverflow.com/questions/14185600/superposed-xyplot-panels-with-grouped-regression-lines 
xyplot(
  Petal.Width  ~ Petal.Length | Species,
  data = iris,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(lm(y~x), col='#0080ff')
  },
  grid = TRUE
)

xyplot(
  Petal.Width ~ Petal.Length,
  data = iris,
  groups = Species,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(lm(y~x))
  },
  grid = TRUE,
  auto.key = list(title='Species', space='right')
)

#to overlay
#option 1
xyplot(Petal.Width  ~ Petal.Length, groups = Species, data = iris, 
       type = c('p','r','g'),  
       auto.key = list(title='Species', space='right'))

#option 2
xyplot(Petal.Width  ~ Petal.Length,
  groups = Species,
  data = iris,
  panel = function(x, y, ...) {
    panel.superpose(x, y, ...,
                    panel.groups = function(x,y, col, col.symbol, ...) {
                      panel.xyplot(x, y, col=col.symbol, ...)
                      panel.abline(lm(y~x), col.line=col.symbol)
                    }
    )
  },
  grid = TRUE,
  auto.key = list(title='Species', space='right')
)

#option 3
xyplot(
  Petal.Width  ~ Petal.Length,
  groups = Species,
  data = iris,
  panel = panel.superpose, # must for use of panel.groups
  panel.groups=function(x, y, col, col.symbol, ...) {
    panel.xyplot(x, y, col=col.symbol, ...)
    panel.lmline(x, y, col.line=col.symbol)
  },
  grid = TRUE,
  auto.key = list(title='Species', space='right')
)

#using Duncan Data
#from https://quantoid.net/files/rbe/lattice.pdf 
trellis.par.set(superpose.line=list(col=c("blue", "black", "red")), 
                superpose.symbol = list(col=c("blue", "black", "red"), 
                                        pch=1:3))
xyplot(prestige ~ income, data=Duncan, groups=Duncan$type,
       auto.key=list(text=c("Blue Collar", "Professional", "White Collar")),
       panel = function(...){
         panel.superpose(..., panel.groups="panel.xyplot")
         panel.superpose(..., panel.groups="panel.lmline")
         })


#From https://stackoverflow.com/questions/23804752/adding-several-loess-lines-to-each-panel-in-lattice-plot
#sample data

(data<-expand.grid(Months=1:13, Site=paste("Site", 1:3), Spe=labels))
(data$Total<-sort(runif(nrow(data), 100,10000))+rnorm(nrow(data),50, 20))

my.col1<- c("white", "darkgray", "black", "lightgray",  "ivory2")
my.col2<- c("white", "darkgray", "black", "lightgray",  "ivory2")
mlabels<- c("Jan-12", "Feb-12", "Mar-12", "Apr-12", "May-12", "Jun-12", 
            "Jul-12", "Aug-12", "Sep-12", "Oct-12", "Nov-12", "Dec-12", "Jan-13")
labels<- c("H", "A", "E", "Q", "T")

xyplot(Total~Months|Site,data=data, groups=Spe,  
       layout=c(3,1), index.cond=list(c(1,2,3)),
       par.settings = list(
         superpose.polygon = list(col=c(my.col1, my.col2))), 
       superpose.line=list(col=c(my.col1, my.col2)),
       ylab="Individuals", xlab="Months",
       scales=list(x=list(rot=90, alternating=1,labels=mlabels)),
       auto.key=list(space="top", columns=3, cex=.8,between.columns = 1,font=3,
                     rectangles=FALSE, points=TRUE, labels=labels),
       panel = panel.superpose,
       panel.groups = function(x,y,...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y, ...)
       }
)


#modeling my issue for Stack Exchange:

library(vegan)
library(dplyr)
data("ANT")
str(ANT)
ANT$block=factor(ANT$block)
#20 subs
#6 blocks
#2 directions
#RT

#add column for logtime
ANT$rtLOG=log(ANT$rt)
str(ANT)

#Inv grouped stats by sub and direction and time
ANT_stats_bysub_bydirection_bytime= ANT %>%
  group_by(subnum, direction, block) %>%
  summarise_at(("rtLOG"), funs(mean, sd), na.rm=TRUE)

str(ANT_stats_bysub_bydirection_bytime)

mypanel=function(x,y,h, k){
  panel.xyplot(x, y, lty=1, type=c('p', 'l'))
  panel.lmline(x, y, lty=3, lwd=1, col="purple")
  panel.grid(h=-1, v=-1)
  panel.abline(mean(h), lty=2, col="red")
  llines(x, y, col=c("blue", "green")) 
}

colors=c("blue", "green")
keylist=list(space="top", col=c("blue", "green", "red", "purple"), columns=1, text=c("Left", "Right", "Mean", "Regression"))
xyplot(mean~block|subnum, groups=direction, data=ANT_stats_bysub_bydirection_bytime, 
       h=ANT_stats$mean, layout=c(5,4), aspect=1.5, 
       main="Subject Response Time, by Direction and Session", xlab="Session", 
       ylab="Mean Response Time", panel=mypanel, auto.key=keylist)


##from https://learnr.wordpress.com/2009/07/20/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-part-6/
quakes$Magnitude <- equal.count(quakes$mag, 4)
pl=cloud(depth~lat*long | Magnitude, data=quakes,
         zlim=rev(range(quakes$depth)), screen=list(z=105, x=-70), panel.aspect = 0.75,
         xlab="longitude", ylab="Latitude", zlab="Depth")
print(pl)



##trying my own
xyplot(Stimuli.ACC_mean~Session|bysubjectGlobal, 
       data=fn_tlocation_bysubj_bytime, 
       groups=TargetLocation, 
       h=fn_tlocation_bysubj_bytime$Stimuli.ACC_mean,
       auto.key =list(space="right", col=c("blue", "pink")), 
       type=c("p", "l"), 
       main="Global v. Local Processing Accuracy", 
       ylab = "Average Accuracy",
       scales=list(alternating=FALSE),
       panel = function(x, y, h, subscripts, groups) {
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=1, col="red")
         #panel.superpose(x,y,subscripts=groups[subscripts], groups, lty=1, col=c("blue", "pink", "green", "red", "yellow"))
         llines(x=x, y=y, type='p', pch=c(23, 23, 21, 21), col=c("blue", "blue", "pink", "pink"),
                fill=c("blue", "blue", "pink", "pink"))
         #ltext(x = x, y = y,labels = c("G", "L", "G", "L"), cex=1,
               #fontfamily = "HersheySans", col=c("blue", "pink", "blue", "pink"))
       },
       layout=c(5,1), aspect=5,
       axis=axis.grid
       )
xyplot(logRT_mean~Session|bysubject, 
       data=Inv_stats_bysub_bydirection_bytime, 
       groups=Direction, 
       h=Inv_stats_bysub_bydirection_bytime$logRT_mean,
       auto.key =list(space="right", col=c("blue", "pink")), 
       main="Response Time by Subject", 
       ylab = "Response Time",
       scales=list(alternating=FALSE),
       panel = function(x, y, h, subscripts, groups) {
         panel.lmline(x, y, lty=3, lwd=1, col="purple")
         panel.abline(mean(h), lty=1, col="red")
         llines(x=x, y=y, type='p', pch=c(23, 23, 21, 21), col=c("blue", "blue", "pink", "pink"),
                fill=c("blue", "blue", "pink", "pink"))
         #ltext(x = x, y = y,labels = c("G", "L", "G", "L"), cex=1,
         #fontfamily = "HersheySans", col=c("blue", "pink", "blue", "pink"))
       },
       layout=c(5,1), aspect=5,
       axis=axis.grid
)
