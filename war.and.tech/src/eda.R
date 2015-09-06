#############################################################################
#This project calculates stats on wars
#Written by Sebastian Sep 
#2 August 2015
#Power Index Data obtained from http://knoema.com/gxbljhd/global-firepower-man-power
#############################################################################
library('ProjectTemplate')
reload.project()
transparency = 0.9
# temp = as.data.frame.table(prop.table(table(wars$advantage, wars$deaths), 1))
# names(temp) = c("Advantage", "Deaths", "Percentage")
# p <- ggplot(data = temp, aes(x=Deaths, y = Percentage))  + geom_bar(stat="identity", aes(fill=Advantage), alpha = transparency) + facet_wrap(~Advantage)
# p = p + theme_sep()
# p = p + xlab("") + ylab("") + scale_y_continuous(labels = percent) + ggtitle("Deaths by Military Superiority 1816-2010")
# p = p + theme(legend.position = "none", axis.text.x = element_text(size = 10), strip.text = element_text(size = 15))
# p = p + coord_flip()
# p

temp = wars %>% group_by(decade) %>% summarise(duration = mean(duration))
p = ggplot(temp, aes(x=decade, y = duration)) + geom_line(size = 3, col = "red", alpha = transparency)
p = p + theme_sep() + theme(axis.text.y=element_text(size = 20))
p = p + geom_smooth(col = "grey", size = 2, linetype ="dashed", fill = "grey80")
p = p + ggtitle("Average Duration of War Across Time")
p = p + ylim(c(0,4)) + ylab("Average Duration in Years") + xlim(c(min(wars$decade), max(wars$decade)))
p
sep.png(p, "wartime")

#Density plot, not that useful
# p <- ggplot(data = wars, aes(x = (approx.deaths-min(approx.deaths))/diff(range(approx.deaths)), category = advantage))  
# p = p + geom_density(aes(y=..density..,  fill=advantage), alpha = transparency) + facet_wrap(~advantage)
# p = p + theme_sep() + stat_ecdf()
# p = p + xlab("") + ylab("") + ggtitle("Deaths by Military Superiority 1816-2010")
# p = p + theme(legend.position = "none", axis.text.x = element_text(angle = 10, size = 12), strip.text = element_text(size = 15))
# p = p + xlim(0,1)
# p = p + scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),labels=levels(wars$deaths))
# p
# sep.png(p, "battledeaths")
cols = RColorBrewer::brewer.pal(3, "Set1")[2:1]
temp = as.data.frame.table(prop.table(table(wars$advantage,wars$aggregate.deaths),1))
p <- ggplot(data = temp, aes(x=Var2, y = Freq))  + facet_wrap(~Var1) + geom_bar(stat="identity", aes(fill=Var2), alpha = transparency)
p = p + theme_sep() + scale_fill_manual(values = cols)
p = p + xlab("") + ylab("Percentage") + scale_y_continuous(labels = percent) + ggtitle("Deaths by Military Superiority 1816-2010")
p = p + theme(legend.position = "none", axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))
p
sep.png(p, "aggregate-battledeaths")



p <- ggplot(data = wars, aes(x = duration))  + geom_density(aes(y=..density..,fill=advantage), alpha = transparency) + facet_wrap(~advantage)
p = p + theme_sep() + stat_ecdf()
p = p + xlab("Number of Years") + ylab("") + ggtitle("Length of Conflict by Military Superiority 1816-2010")
p = p + theme(legend.position = "none", axis.text.x = element_text(size = 10), strip.text = element_text(size = 15))
p
sep.png(p, "battle-length")

# temp = as.data.frame.table(prop.table(table(wars$advantage, wars$length), 1))
# names(temp) = c("Advantage", "Length", "Percentage")
# p <- ggplot(data = temp, aes(x=Length, y = Percentage))  + geom_bar(stat="identity", aes(fill=Advantage), alpha = transparency) + facet_wrap(~Advantage)
# p = p + theme_sep()
# p = p + xlab("") + ylab("") + scale_y_continuous(labels = percent) + ggtitle("Duration by Military Superiority 1816-2010")
# p = p + theme(legend.position = "none", axis.text.x = element_text(size = 10), strip.text = element_text(size = 15))
# p = p + coord_flip()
# p
##If you want to divide by military difference
# temp = as.data.frame.table(prop.table(table(wars$advantage,wars$superior.win),1))
# p <- ggplot(data = temp, aes(x=Var2, y = Freq))  + facet_wrap(~Var1) + geom_bar(stat="identity", aes(fill=Var2), alpha = transparency)
# p = p + theme_sep()
# p = p + xlab("") + ylab("Percentage") + scale_y_continuous(labels = percent) + ggtitle("Outcomes of Interstate Wars 1816-2010")
# p = p + theme(legend.position = "none", axis.title.y = element_text(size = 15), axis.text.x = element_text(angle = 10))
# p
temp = subset(wars, !grepl("Minimal", advantage))
temp = subset(temp, !grepl("Minor", advantage))
temp = as.data.frame.table(prop.table(table(temp$superior.win)))
temp = arrange(temp, desc(Freq))
temp$Var1 = factor(temp$Var1, levels = temp$Var1, ordered=T)
p <- ggplot(data = temp, aes(x=Var1, y = Freq))  + geom_bar(stat="identity", aes(fill=Var1), alpha = transparency)
p = p + theme_sep()
p = p + xlab("") + ylab("Percentage") + scale_y_continuous(labels = percent) + ggtitle("Outcomes of Interstate Wars 1816-2010")
p = p + theme(legend.position = "none", axis.title.y = element_text(size = 15))
p
sep.png(p, "battleoutcome")

temp = subset(wars, !grepl("Minimal", advantage))
temp = subset(temp, !grepl("Minor", advantage))
temp = temp %>% group_by(duration) %>% summarise(success = sum(superior.win == "Superior Force Won")/n())
p <- ggplot(data = temp, aes(x=duration, y = success))  + geom_bar(stat="identity", aes(fill = factor(duration)), alpha = transparency) 
p = p + theme_sep() + geom_smooth(size=3, col = "red", se = F, alpha = 0.5) 
p = p + xlab("Duration in Years") + ylab("Percentage of Outright Victories 1816-2010") + scale_y_continuous(labels = percent) 
p = p + ggtitle("Historical Success Rate of Large Superior Forces by Duration of Conflict")
p = p + theme(legend.position = "none", axis.title.y = element_text(size = 15))
p = p + scale_fill_brewer(palette = "Blues")
when = 1.5
rate = 0.75
ang = 60
p = p + geom_segment(x = when, y = 0, xend = when, yend = rate, linetype = "dashed", colour = "grey70") + annotate("text", x = when, y = rate + 0.03, label = "Iraq War I", col = "grey10", angle = ang )
when = 8
p = p + geom_segment(x = when, y = 0, xend = when, yend = rate, linetype = "dashed", colour = "grey70") + annotate("text", x = 8, y = rate + 0.03, label = "Vietnam", col = "grey10", angle = ang )
when = 14
p = p + geom_segment(x = when, y = 0, xend = when, yend = rate, linetype = "dashed", colour = "grey70") + annotate("text", x = when, y = rate + 0.03, label = "Afghanistan (ongoing)", col = "grey10", angle = ang )
when = 9
p = p + geom_segment(x = when, y = 0, xend = when, yend = rate, linetype = "dashed", colour = "grey70") + annotate("text", x = when, y = rate + 0.03, label = "Iraq War II", col = "grey10", angle = ang )
when = 6
p = p + geom_segment(x = when, y = 0, xend = when, yend = rate, linetype = "dashed", colour = "grey70") + annotate("text", x = when, y = rate + 0.03, label = "World War II", col = "grey10", angle = ang )
when = 4
p = p + geom_segment(x = when, y = 0, xend = when, yend = rate, linetype = "dashed", colour = "grey70") + annotate("text", x = when, y = rate + 0.03, label = "World War I", col = "grey10", angle = ang )
p
sep.png(p, "durationsucessrates")

