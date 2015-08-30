############################
# Replication of the States of Fragility 2015 report by the OECD
# Available at http://www.oecd.org/publications/states-of-fragility-2015-9789264227699-en.htm
#
# By Thomas Leo Scherer
#
# Last Updated 5/6/2015
#
# Note: this script was run with R version 3.2.0.
# Runing an earlier version may change the results, due to issues with the countrycode package.
#
############################

us.wars$duration = us.wars$End-us.wars$Start+1
us.wars$Conflict = factor(us.wars$Conflict, levels = rev(us.wars$Conflict), ordered=T)
us.wars$failed = ifelse(us.wars$Outcome == "Victory",0,1)
plot(survfit(Surv(us.wars$duration)~ 1, conf.type="none"))
p = ggplot(us.wars, aes(x=Conflict, y = duration, fill = Outcome)) + geom_bar(stat = "identity") + coord_flip()
p = p + theme_sep() 
p
#### Load Packages ####
#install.packages("WDI")
#install.packages("ggplot2")
#install.packages("xlsx")
#install.packages("countrycode")
library(WDI)
library(xlsx)
library(ggplot2)
library(countrycode)
load(url("http://www.pcr.uu.se/digitalAssets/124/124920_1ucdp-prio-2015.rdata"))

#### BattleDeaths #####
load(url("http://www.pcr.uu.se/digitalAssets/124/124934_1ucdpbattle-relateddeathsdatasetv.5-2014dyadic.rdata"))
# set year 2012
BattleDeaths <- ucdpBRDDyadic[ucdpBRDDyadic$Year == 2012,]
# if Best estiamte is missing use low estimate
BattleDeaths$BdBest[BattleDeaths$BdBest<0] <- BattleDeaths$BdLow[BattleDeaths$BdBest<0] 
# Take care of countries with multiple locations 
BattleDeaths$CountryCount<-stringr::str_count(as.character(BattleDeaths$LocationInc),",")
ToSplit<-subset(BattleDeaths,CountryCount>0) 
Conflictsub<-subset(BattleDeaths,CountryCount==0) # no need to split
if(nrow(ToSplit)+nrow(Conflictsub)!=nrow(BattleDeaths)) warning("DANGER DANGER")
if(sum(BattleDeaths$CountryCount)>0){
  for(i in 1:nrow(ToSplit)){
    Splits<-ToSplit[rep(rownames(ToSplit[i,]),ToSplit$CountryCount[i]+1),]
    Splits$LocationInc<-stringr::str_split(ToSplit$LocationInc[i], ", ")[[1]]
    Conflictsub<-rbind(Conflictsub, Splits)
  }
}
BattleDeaths<-Conflictsub[,c("LocationInc", "BdBest")]
# aggregate by country
BattleDeaths <-aggregate(BattleDeaths[,c("BdBest")], by=list(BattleDeaths$LocationInc), FUN=sum, na.rm=TRUE)
# column names
names(BattleDeaths)<-c("LocationInc", "BattleDeaths")
# fix countries
BattleDeaths$Country <- countrycode(BattleDeaths$LocationInc, "country.name", "country.name")
if(sum(BattleDeaths$Country=="Sudan")>1) warning("Error, South Sudan turned to Sudan. You likely need to update R to latest version.")
BattleDeaths <- BattleDeaths[c("Country", "BattleDeaths")]
# had to change Yemen Arab Republic to Yemen
BattleDeaths$Country[BattleDeaths$Country=="Yemen Arab Republic"]<-"Yemen"



#### BattleDeaths2013 #####
load(url("http://www.pcr.uu.se/digitalAssets/124/124934_1ucdpbattle-relateddeathsdatasetv.5-2014dyadic.rdata"))
# set year 2013
BattleDeaths2013 <- ucdpBRDDyadic[ucdpBRDDyadic$Year == 2013,]
# if Best estiamte is missing use low estimate
BattleDeaths2013$BdBest[BattleDeaths2013$BdBest<0] <- BattleDeaths2013$BdLow[BattleDeaths2013$BdBest<0] 
# Take care of countries with multiple locations 
BattleDeaths2013$CountryCount<-stringr::str_count(as.character(BattleDeaths2013$LocationInc),",")
ToSplit<-subset(BattleDeaths2013,CountryCount>0) 
Conflictsub<-subset(BattleDeaths2013,CountryCount==0) # no need to split
if(nrow(ToSplit)+nrow(Conflictsub)!=nrow(BattleDeaths2013)) warning("DANGER DANGER")
if(sum(BattleDeaths2013$CountryCount)>0){
  for(i in 1:nrow(ToSplit)){
    Splits<-ToSplit[rep(rownames(ToSplit[i,]),ToSplit$CountryCount[i]+1),]
    Splits$LocationInc<-stringr::str_split(ToSplit$LocationInc[i], ", ")[[1]]
    Conflictsub<-rbind(Conflictsub, Splits)
  }
}
BattleDeaths2013<-Conflictsub[,c("LocationInc", "BdBest")]
# aggregate by country
BattleDeaths2013 <-aggregate(BattleDeaths2013[,c("BdBest")], by=list(BattleDeaths2013$LocationInc), FUN=sum, na.rm=TRUE)
# column names
names(BattleDeaths2013)<-c("LocationInc", "BattleDeaths")
# fix countries
BattleDeaths2013$Country <- countrycode(BattleDeaths2013$LocationInc, "country.name", "country.name")
if(sum(BattleDeaths2013$Country=="Sudan")>1) warning("Error, South Sudan turned to Sudan. You likely need to update R to latest version.")
BattleDeaths2013 <- BattleDeaths2013[c("Country", "BattleDeaths")]
# had to change Yemen Arab Republic to Yemen
BattleDeaths2013$Country[BattleDeaths2013$Country=="Yemen Arab Republic"]<-"Yemen"