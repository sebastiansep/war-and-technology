#############################################################################
#This project calculates stats on wars
#Written by Sebastian Sep 
#2 August 2015
#Power Index Data obtained from http://knoema.com/gxbljhd/global-firepower-man-power
#############################################################################
library('ProjectTemplate')
reload.project()
library(ggplot2)
library(survival)
library(muhaz)
library(sep)
library(rvest)


us.wars$duration = us.wars$End-us.wars$Start+1
us.wars$Conflict = factor(us.wars$Conflict, levels = rev(us.wars$Conflict), ordered=T)
us.wars$failed = ifelse(us.wars$Outcome == "Victory",0,1)
plot(survfit(Surv(us.wars$duration)~ 1, conf.type="none"))
p = ggplot(us.wars, aes(x=Conflict, y = duration, fill = Outcome)) + geom_bar(stat = "identity") + coord_flip()
p = p + theme_sep() 
p


mil.power$year = as.numeric(substr(mil.power$Date, 5,8))
mil.power = subset(mil.power, year == 2014)
mil.power$Value = min(mil.power$Value)/mil.power$Value

