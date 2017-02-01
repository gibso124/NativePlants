##########################################################################
##########################################################################
#MEETING WITH CHRISTIIE
##########################################################################
library(ggplot2)

#If you want to use a subset or any other change, do that all outside ggplot2
#This way, the code for the plot itself can remain exactly the same.
standard2.barplot<-ggplot(summarized.data, aes(x=species), y=ne, fill=as.factor(species))+
  geom_bar(stat="identity", colour="black")+
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se))+
  theme(axis.text.x=element_text(angle=-45, size=10))+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("Mean natural enemies / sample")+
  xlab("\n Plant species\n")+
  facet_wrap(~site,ncol=1)#3 plots side by side (by site) in single object
standard2.barplot

#NMDs Code line s690-824 in Lampyrid code on GitHub

#Summaraizing the data. 
library(plyr)
summarized.data<-ddply(all.with.controls, c('site', 'species'),summarise,
                       ne=mean(ne_total),herb=mean(herb_total),
                       ne.se=sd(ne_total)/sqrt(length(ne_total)),
                       herb.se=sd(herb_total)/sqrt(length(herb_total)))#returns some 'NaN' b/c there is only one sample


#Creating mean phenology dates for each species at each site, for ordering charts
#This could be incorporated directly much earlier, such as when converting dates for the main dataset
library(plyr)
bloomday <- subset(all.nocontrols, select = c(1, 2, 3, 4, 6))#Smaller dataset to work with)
library(lubridate)
bloomday$DOY<-yday(bloomday$date.x) #Get day of year (1-365)
bloomday<-ddply(bloomday, c('site','species'),summarise,
                DOYphenol=mean(DOY))#average dates of all samples
summarized.data<-merge(summarized.data,bloomday,by=c('site','species'),all.x=TRUE) #Merge into summary data
summarized.data.ord<-summarized.data[order(summarized.data$DOYphenol, summarized.data$species),]#



#Code with controls normalization
# Summarized.data<-ddply(all.with.controls, c('site', 'species'),summarise,
#               norm_ne=mean(ne_total/control_ne),norm_herb=mean(herb_total/control_herb),
#               ne.se=sd(ne_total/control_ne)/sqrt(length(ne_total)),
#               .se=sd(herb_total/control_herb)/sqrt(length(herb_total)))

#This is the original from the Lampyrid code
# peaks.year<-ggplot(peaks, aes(x=as.factor(year), y=peak, fill=as.factor(year)))+
#   scale_fill_manual(values=pal)+ #'pal' is an object with CB's custom pallette
#   geom_bar(stat="identity", colour="black")+
#   geom_errorbar(aes(ymin=peak-peak.err, ymax=peak+peak.err))+
#   theme_bw(base_size = 20)+
#   guides(fill=FALSE)+
#   ylab("\nDD at peak emergence\n")+
#   xlab("\nYear\n")+
#   theme(axis.text.x=element_text(angle=90))
# peaks.year

##########################################################################

##########################################################################
# Code for pulling individual columns
########################################################################## 

#pulling out columns from a larger data frame
#both "dplyr" and "plyr" methods work.
library(dplyr)
plots2<- select(controls,week,ne_total)
library(plyr)
plots <- subset(controls, select = c(1, 2, 77))
