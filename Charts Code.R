##########################################################################
##########################################################################
#MEETING WITH CHRISTIIE
##########################################################################
library(ggplot2)

#If you want to use a subset or any other change, do that all outside ggplot2
#This way, the code for the plot itself can remain exactly the same.
barplot.1<-ggplot(summarized.data, aes(x=reorder(species,DOYphenol), y=ne, fill=as.factor(species)))+ 
  geom_bar(stat="identity", 
           colour="black", 
           size=0.25)+
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se), 
                     width=.4,
                     size=0.25)+
  theme_bw(base_size = 15)+
  guides(fill=FALSE)+
  labs(title='\nMean Natural Enemies - with controls overlaid')+
  theme(plot.title = element_text(face = 'bold',
                                  size = 20,
                                  hjust = 0.5),
        axis.title = element_text(face = 'plain',
                                  size = 15),
        axis.text.x=element_text(angle=55, 
                                 face='plain', #("plain", "italic", "bold", "bold.italic")
                                 size=7,    #(in pts)
                                 #vjust=0,   #(in [0, 1])
                                 hjust=1))+ #(in [0, 1])
  ylab("\nMean natural enemies / sample")+
  xlab("Plant species\n")+
#  geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
    geom_point(data=summarized.data, aes(x=as.numeric(reorder(species,DOYphenol)), y=ne_control, group=1, color=''))+
  facet_wrap(~site,ncol=1, scales = 'free_y')#3 plots side by side (by site) in single object
barplot.1



#NMDs Code line s690-824 in Lampyrid code on GitHub

#Summaraizing the data. 
library(plyr)
summarized.data<-ddply(all.with.controls, c('site', 'species'),summarise,
                       ne=mean(ne_total),herb=mean(herb_total),
                       ne.se=sd(ne_total)/sqrt(length(ne_total)),
                       herb.se=sd(herb_total)/sqrt(length(herb_total)),
                       ne_control=mean(control_ne),herb_control=mean(control_herb),
                       DOYphenol=mean(DOY))#average dates of all samples
                       #returns some 'NaN' b/c there is only one sample

#Adding Names
# Bring in Names file.
names<-read.csv('Names.csv',header=TRUE)
summarized.data<-merge(summarized.data,names,by=c('species'),all.x=TRUE)


#Order sites (NOTE: Add site num column earlier in other code)
summarized.data$sitenum<-summarized.data$site
summarized.data$sitenum<-gsub('NWMHRC','1',summarized.data$sitenum)
summarized.data$sitenum<-gsub('CRC','2',summarized.data$sitenum)
summarized.data$sitenum<-gsub('SWMREC','3',summarized.data$sitenum)
summarized.data<-summarized.data[order(summarized.data$sitenum, 
                                       summarized.data$DOYphenol, 
                                       summarized.data$species),]
summarized.data$site<-factor(summarized.data$site, levels=unique(summarized.data$site))

#Summarize: Individual Sites
SWMREC<-subset(summarized.data, site == "SWMREC")
CRC<-subset(summarized.data, site == "CRC")
NWMHRC<-subset(summarized.data, site == "NWMHRC")

#Summarize: combine sites
library(plyr)
summary.combined.sites<-ddply(all.with.controls, c('species'),summarise,
                       ne=mean(ne_total),herb=mean(herb_total),
                       ne.se=sd(ne_total)/sqrt(length(ne_total)),
                       herb.se=sd(herb_total)/sqrt(length(herb_total)),
                       DOYphenol=mean(DOY))#average dates of all samples
                        #returns some 'NaN' b/c there is only one sample for a species
#Summarize: controls
controls.summary$sitenum<-controls.summary$site
controls.summary$sitenum<-gsub('NWMHRC','1',controls.summary$sitenum)
controls.summary$sitenum<-gsub('CRC','2',controls.summary$sitenum)
controls.summary$sitenum<-gsub('SWMREC','3',controls.summary$sitenum)
controls.summary<-controls.summary[order(controls.summary$sitenum, 
                                        controls.summary$week),]
controls.summary$site<-factor(controls.summary$site, levels=unique(controls.summary$site))

#Summarizing TOTAL insect abundances (including controls)
library(plyr)
summary.all<-ddply(all.data, c('site', 'week'),summarise,
                       mean_all_arthropods=mean(ne_total+herb_total),
                        sum_all_arthropods=sum(ne_total+herb_total),
                       ne=mean(ne_total),herb=mean(herb_total),
                       ne.se=sd(ne_total)/sqrt(length(ne_total)),
                       herb.se=sd(herb_total)/sqrt(length(herb_total)),
                       DOYphenol=mean(DOY))#average dates of all samples
#returns some 'NaN' b/c there is only one sample

summary.all$sitenum<-summary.all$site
summary.all$sitenum<-gsub('NWMHRC','1',summary.all$sitenum)
summary.all$sitenum<-gsub('CRC','2',summary.all$sitenum)
summary.all$sitenum<-gsub('SWMREC','3',summary.all$sitenum)
summary.all<-summary.all[order(summary.all$sitenum, 
                               summary.all$week),]
summary.all$site<-factor(summary.all$site, levels=unique(summary.all$site))

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
