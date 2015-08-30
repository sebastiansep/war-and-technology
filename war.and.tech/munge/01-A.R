# Example preprocessing script.
library(ggplot2)
library(survival)
library(muhaz)
library(sep)
library(dplyr)
library(countrycode)
library(DataCombine)


#Normalise military power scale
mil.power$year = as.numeric(substr(mil.power$Date, 5,8))
mil.power = subset(mil.power, year == 2014)
mil.power$Value = min(mil.power$Value)/mil.power$Value
mil.power = rename(mil.power, power = Value)
mil.power$iso3c = countrycode(mil.power$location, "country.name", "iso3c")
mil.power$region = sep.region(mil.power$iso3c)
mil.power = mil.power %>% group_by(region) %>% mutate(regional.power = mean(power, na.rm=T))

#Get conflict data
wars = MID.level.MIDB.4.01
wars = subset(wars, HostLev >=4)
wars = merge(wars, select(MID.level.MIDA.4.01, DispNum3, MaxDur, Outcome))
wars = select(wars, DispNum3, StAbb, ccode, StYear, EndYear, SideA, Fatality, Outcome, MaxDur)
wars = subset(wars, Fatality > 1)


#Get isocodes and region
wars$country = countrycode(wars$ccode, "cown", "country.name")
wars$iso3c = sep.old.iso3c(wars$country)
wars$region = sep.region(wars$iso3c)
wars$region = gsub("Caribbean", "Central America", wars$region)
wars$region = gsub("Melanesia", "South-Eastern Asia", wars$region)
wars$region = gsub("Micronesia", "South-Eastern Asia", wars$region)

#Merge military power index
wars = merge(wars, select(mil.power, iso3c, power), all.x=T)
wars = merge(wars, unique(select(mil.power, region, regional.power)), all.x=T)
pos = is.na(wars$power)
wars$power[pos] = wars$regional.power[pos]
wars = subset(wars, !is.na(power))

#Calculate Duration
wars$duration = with(wars, EndYear-StYear+1)

#Merge outcomes
wars = subset(wars, Outcome < 9)
wars = merge(wars, cow.outcomes)
wars = select(wars, DispNum3, iso3c, country,  SideA, power, StYear, EndYear, duration, Fatality, outcome)

#Determine power of opposing force
wars = wars %>% group_by(DispNum3) %>% mutate(opposing.power = max(power[SideA!=1]))
wars = subset(wars, SideA == 1)
wars$power.balance = with(wars,abs((opposing.power - power)))
wars$superior = ifelse(wars$power > wars$opposing.power, "Side A", "Side B")
wars$superior.win = as.character(wars$outcome)
pos = apply(wars[,c("superior", "outcome")], 1, function(x) grepl(x[1], x[2]))
wars$superior.win[pos] = "Superior Force Won"
pos = grepl("Victory", wars$superior.win)
wars$superior.win[pos] = "Superior Force Lost"
pos = grepl("Yield", wars$superior.win)
wars$superior.win[pos] = "Superior Force Lost"
pos = wars$superior.win == "Released"
wars$superior.win[pos] = "Compromise"
wars$advantage = findInterval(wars$power.balance, seq(0,1, by = 0.25), all.inside = T)
mil.diffs = c("Minimal Military Superiority For Either Side", "Minor Military Superiority on One Side", "Large Military Superiority on One Side", "Dominant Military Superiority on One Side")
wars$advantage = mil.diffs[wars$advantage]
wars = wars %>% group_by(advantage) %>% mutate(n.advantage = n())
wars$advantage = paste(wars$advantage, " (n = ", wars$n.advantage, ")", sep = "")
wars = select(wars, -n.advantage)
temp = unique(wars$advantage)
key = grep(mil.diffs[1], temp)
key = c(key, grep(mil.diffs[2], temp))
key = c(key, grep(mil.diffs[3], temp))
key = c(key, grep(mil.diffs[4], temp))
wars$advantage =  factor(wars$advantage, levels = temp[key], ordered=T)

times = c("< 2 years", "2-5 years", "5-10 years", "> 10 years")
wars$length = ifelse(wars$duration <= 2, times[1], NA)
wars$length = ifelse(between(wars$duration,2.1, 5), times[2], wars$length)
wars$length = ifelse(between(wars$duration,5.1, 10), times[3], wars$length)
wars$length = ifelse(between(wars$duration,10.1, 100), times[4], wars$length)
wars$length = factor(wars$length, levels = times, ordered=T)
deaths = c("26-100 deaths", "101-250 deaths", "251-500 deaths", "501-999 deaths", "> 999 deaths")#c("None", "1-25 deaths", "26-100 deaths", "101-250 deaths", "251-500 deaths", "501-999 deaths", "> 999 deaths")
pos = wars$Fatality > -9
wars$deaths = wars$Fatality
approx.deaths = c(75, 175, 375, 750, 1000)
wars$approx.deaths[pos] = approx.deaths[wars$deaths[pos]-1 ]
wars$deaths[pos] = deaths[wars$deaths[pos]-1 ]
pos = wars$Fatality == -9
wars$deaths[pos] = "Unknown"
wars$deaths = factor(wars$deaths, levels = c( deaths), ordered=T)#factor(wars$deaths, levels = c("Unknown", deaths), ordered=T)
wars$annual.deaths = wars$approx.deaths/wars$duration

#decade
wars$decade = findInterval(wars$StYear, seq(1810, max(wars$StYear), by = 10), all.inside=T)
wars$decade = plyr::round_any(wars$StYear, accuracy=10, f = ceiling)
#clean environment
rmExcept("wars")




