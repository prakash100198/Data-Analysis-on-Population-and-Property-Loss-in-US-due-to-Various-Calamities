library(plyr)
library(dplyr)
library(ggplot2)


data<-read.csv("repdata_data_StormData.csv.bz2")
head(data)
str(data)
View(data)

colSums(is.na(data))
sum(data$CROPDMG)

#variables we need to solve this analysis
dframe<-data[,c("STATE__","STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
head(dframe)
dframe$POPU.DAMAGE<-dframe[,"FATALITIES"]+dframe[,"INJURIES"]
nrow(dframe)
dframe$TOTAL.PROP.DAMAGE<-rep(0,902297)
#dframe$PROP.DAMAGE<-rep(0,902297)
#dframe$CROP.DAMAGE<-rep(0,902297)

dframe$CROPDMGEXP<-as.character(dframe$CROPDMGEXP)
dframe$PROPDMGEXP<-as.character(dframe$PROPDMGEXP)
#since there are many levels i will take only k,K,m,M,b,B as denomination units
deno<-c("K","M","B")
df<-filter(dframe,PROPDMGEXP %in% deno & CROPDMGEXP %in% deno)
head(df)
nrow(df)
df<-df[,-13]


for(i in 1:nrow(df)){

     if(df$PROPDMGEXP[i] == "K"&&df$CROPDMGEXP[i] == "K")
        df$TOTAL.PROP.DAMAGE[i]<- df$PROPDMG[i]*1000 + df$CROPDMG[i]*1000
  
     else if(df$PROPDMGEXP[i]=="K"&&df$CROPDMGEXP[i] == "M")
        df$TOTAL.PROP.DAMAGE[i]<- df$PROPDMG[i]*1000 + df$CROPDMG[i]*1000000
  
     else if(df$PROPDMGEXP[i]=="K"&&df$CROPDMGEXP[i] == "B")
       df$TOTAL.PROP.DAMAGE[i]<- df$PROPDMG[i]*1000 + df$CROPDMG[i]*1000000000
     
     else if(df$PROPDMGEXP[i] == "M"&&df$CROPDMGEXP[i] == "K")
       df$TOTAL.PROP.DAMAGE[i]<- df$PROPDMG[i]*1000000 + df$CROPDMG[i]*1000
     
     else if(df$PROPDMGEXP[i]=="M"&&df$CROPDMGEXP[i] == "M")
       df$TOTAL.PROP.DAMAGE[i]<-  df$PROPDMG[i]*1000000 + df$CROPDMG[i]*1000000
     
     else if(df$PROPDMGEXP[i]=="M"&& df$CROPDMGEXP[i] == "B")
       df$TOTAL.PROP.DAMAGE[i]<-  df$PROPDMG[i]*1000000 + df$CROPDMG[i]*1000000000
     
     else if(df$PROPDMGEXP[i] == "B"&&df$CROPDMGEXP[i] == "K")
       df$TOTAL.PROP.DAMAGE[i]<- df$PROPDMG[i]*1000000000 + df$CROPDMG[i]*1000
     
     else if(df$PROPDMGEXP[i]=="B"&&df$CROPDMGEXP[i] == "M")
       df$TOTAL.PROP.DAMAGE[i]<-  df$PROPDMG[i]*1000000000 + df$CROPDMG[i]*1000000
     
     else if(df$PROPDMGEXP[i]=="B"&& df$CROPDMGEXP[i] == "B")
       df$TOTAL.PROP.DAMAGE[i]<-  df$PROPDMG[i]*1000000000 + df$CROPDMG[i]*1000000000
     
}

#we have set population damage and economic damage based on the original data

#now we will plot btw evtype and its consequences on population and economical damage 
df1<-df%>%group_by(EVTYPE)%>%summarise(TOT.POPU.DAMAGE.PER.EVTYPE=sum(POPU.DAMAGE,na.rm = T),TOT.ECONOMIC.DAMAGE.PER.EVTYPE=sum(TOTAL.PROP.DAMAGE,na.rm = T))
df1
View(df1)
summary(df1)
nrow(df1[which(df1$TOT.POPU.DAMAGE.PER.EVTYPE==0),])

#after seeing summary we see that max fatalities in US is 13000+ and still mean is just 200+ so we will divide this dataset into 2 parts one part with total fatalities >500 and second set with fatalaties<500
df2<-filter(df1,TOT.POPU.DAMAGE.PER.EVTYPE>=1000)
df2
nrow(df1[which(df1$TOT.POPU.DAMAGE.PER.EVTYPE>1000),])

# plot to see which calamity causes max popu damage
g1<-ggplot(df2,aes(EVTYPE,TOT.POPU.DAMAGE.PER.EVTYPE,fill=EVTYPE))
g1+geom_bar(stat = "identity",show.legend = F)+theme_bw() +geom_text(aes(label=TOT.POPU.DAMAGE.PER.EVTYPE),vjust=-0.3)+ggtitle("POPULATION DAMAGE(>1000)")+ylab("POPULATION DAMAGE PER EVTYPE")+ theme(axis.text.x = element_text(angle = 60, hjust = 1))

#here we see max damage is caused by tornado and is followed by flood and then ice storm
# plot to see which calamity causes max economic damage
summary(df1)
df3<-filter(df1,TOT.ECONOMIC.DAMAGE.PER.EVTYPE>2000000000)
df3

g2<-ggplot(df3,aes(EVTYPE,TOT.ECONOMIC.DAMAGE.PER.EVTYPE/1000000000,fill=EVTYPE))
g2+geom_bar(stat = "identity",show.legend = F)+theme_bw() +geom_text(aes(label=round(TOT.ECONOMIC.DAMAGE.PER.EVTYPE/1000000000,2)),vjust=-0.3)+ggtitle("ECONOMIC DAMAGE(>2B$)")+ylab("ECONOMIC DAMAGE PER EVTYPE(in Billion Dollars)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1))
#max economic destruction caused by flood with 138.007B$ followed by hurricane/typhoon with 29.34B$ loss  3rd place is occupied by tornado with 16.5B$ loss of property

