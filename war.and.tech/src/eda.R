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

p <- ggplot(data = wars, aes(x = (approx.deaths-min(approx.deaths))/diff(range(approx.deaths)), category = advantage))  + geom_density(aes(y=..density..,  fill=advantage), alpha = transparency) + facet_wrap(~advantage)
p = p + theme_sep() + stat_ecdf()
p = p + xlab("") + ylab("") + ggtitle("Deaths by Military Superiority 1816-2010")
p = p + theme(legend.position = "none", axis.text.x = element_text(angle = 10, size = 12), strip.text = element_text(size = 15))
p = p + xlim(0,1)
p = p + scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),labels=levels(wars$deaths))
p
sep.png(p, "battledeaths")


p <- ggplot(data = wars, aes(x = duration))  + geom_density(aes(y=..density..,fill=advantage), alpha = transparency) + facet_wrap(~advantage)
p = p + theme_sep() + stat_ecdf()
p = p + xlab("Number of Years") + ylab("") + ggtitle("Length of Conflict by Military Superiority 1816-2010")
p = p + theme(legend.position = "none", axis.text.x = element_text(size = 10), strip.text = element_text(size = 15))
p
sep.png(p, "battlelength")

# temp = as.data.frame.table(prop.table(table(wars$advantage, wars$length), 1))
# names(temp) = c("Advantage", "Length", "Percentage")
# p <- ggplot(data = temp, aes(x=Length, y = Percentage))  + geom_bar(stat="identity", aes(fill=Advantage), alpha = transparency) + facet_wrap(~Advantage)
# p = p + theme_sep()
# p = p + xlab("") + ylab("") + scale_y_continuous(labels = percent) + ggtitle("Duration by Military Superiority 1816-2010")
# p = p + theme(legend.position = "none", axis.text.x = element_text(size = 10), strip.text = element_text(size = 15))
# p = p + coord_flip()
# p

temp = as.data.frame.table(prop.table(table(wars$superior.win)))
temp = arrange(temp, desc(Freq))
temp$Var1 = factor(temp$Var1, levels = temp$Var1, ordered=T)
p <- ggplot(data = temp, aes(x=Var1, y = Freq))  + geom_bar(stat="identity", aes(fill=Var1), alpha = transparency)
p = p + theme_sep()
p = p + xlab("") + ylab("Percentage") + scale_y_continuous(labels = percent) + ggtitle("Outcomes of Interstate Wars 1816-2010")
p = p + theme(legend.position = "none", axis.title.y = element_text(size = 15))
p
sep.png(p, "battleoutcome")
