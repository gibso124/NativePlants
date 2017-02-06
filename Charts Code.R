##########################################################################-
#CHARTS
##########################################################################

#If you want to use a subset or any other change, do that all outside ggplot2
#This way, the code for the plot itself can remain exactly the same.library(ggplot2)

#### Original bar plot from the Lampyrid script ####
# library(ggplot2)
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


#### Standard Bar Plot ####
library(ggplot2)
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
  geom_point(data=summarized.data, aes(x=as.numeric(reorder(species,DOYphenol)), y=controls_ne, group=1))+
  facet_wrap(~site,ncol=1, scales = 'free_y')#3 plots side by side (by site) in single object
barplot.1

#### Standard Bar Plot - Norm ####
library(ggplot2)
barplot.2<-ggplot(norm, aes(x=reorder(species,DOYphenol), y=mnorm_ne, fill=as.factor(species)))+ 
  geom_bar(stat="identity", 
           colour="black", 
           size=0.25)+
  geom_errorbar(aes(ymin=mnorm_ne-ne.se, ymax=mnorm_ne+ne.se), 
                width=.4,
                size=0.25)+
  theme_bw(base_size = 15)+
  guides(fill=FALSE)+
  labs(title='\nMean Natural Enemies - Normalized by Mowed')+
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
  ylab("\nLabel?")+
  xlab("Plant species\n")+
  # geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  geom_hline(yintercept=1, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  # geom_point(data=summarized.data, aes(x=as.numeric(reorder(species,DOYphenol)), y=controls_ne, group=1))+
  facet_wrap(~site,ncol=1, scales = 'free_y')#3 plots side by side (by site) in single object
barplot.2

##### Stacked Bar Plot ####
library(ggplot2)
stacked.bar<- ggplot(mtaxa.ne, aes(reorder(species,DOYphenol), mean_ne, fill = order)) +
  geom_bar(position = "stack", stat = "identity")+
  theme(axis.title = element_text(face = 'plain',
                                  size = 10),
        axis.text.x=element_text(angle=55, 
                                 face='plain', #("plain", "italic", "bold", "bold.italic")
                                 size=4,    #(in pts)
                                 #vjust=0,   #(in [0, 1])
                                 hjust=1))+ #(in [0, 1])
  ylab("Mean natural enemies / sample")+
  xlab("Plant species")+
  facet_wrap(~ site,ncol=1)#3 plots side by side (by site) in single object
stacked.bar

##Reshape data for stacked plot
{#Extract columns
library(plyr)
taxa.ne <- subset(summarized.data, select = c(1, 2, 3,4,6,8,10,12,14))
DOY <- subset(summarized.data, select = c(1,2,29))

#melt to long form, rename columns
library("reshape")
mtaxa.ne<-melt(taxa.ne, id=c("site","species",'sitenum'))
names(mtaxa.ne)[names(mtaxa.ne) == 'variable']<-'order'
names(mtaxa.ne)[names(mtaxa.ne) == 'value']<-'mean_ne'
#Merge DOY into mtaxa.ne for ordering
mtaxa.ne<-merge(mtaxa.ne,DOY,by = c('site','species'),all = TRUE)
}


#### Mowed by Week Bar Plot ####
library(viridis)
library(ggplot2)
# barplot.1<-ggplot(controls.summary, aes(x=as.factor(week), y=mowed_ne, fill=as.factor(week)))+  #NE only
# barplot.1<-ggplot(controls.summary, aes(x=as.factor(week), y=mowed_herb, fill=as.factor(week)))+  #herb only
barplot.1<-ggplot(controls.summary, aes(x=as.factor(week), y=mowed_all, fill=as.factor(week)))+  #both ne and herb
  scale_fill_viridis(discrete=TRUE, option = 'plasma', direction = -1)+
  geom_bar(stat="identity", 
           colour="black", 
           size=0.25)+
  # geom_errorbar(aes(ymin=mowed_ne-mowed_ne.se, ymax=mowed_ne+mowed_ne.se), 
  # geom_errorbar(aes(ymin=mowed_herb-mowed_herb.se, ymax=mowed_herb+mowed_herb.se),
  geom_errorbar(aes(ymin=mowed_all-mowed_all.se, ymax=mowed_all+mowed_all.se),
                width=.4,
                size=0.25)+
  theme_bw(base_size = 15)+
  guides(fill=FALSE)+
  labs(title='\nMowed Control - NE +Herb')+
  theme(plot.title = element_text(face = 'bold',
                                  size = 20,
                                  hjust = 0.5),
        axis.title = element_text(face = 'plain',
                                  size = 15),
        axis.text.x=element_text(angle=1, 
                                 face='plain', #("plain", "italic", "bold", "bold.italic")
                                 size=10,    #(in pts)
                                 #vjust=0,   #(in [0, 1])
                                 hjust=0.5))+ #(in [0, 1])
  ylab("\nMean natural enemies / sample")+
  xlab("Week of year\n")+
  # geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  facet_wrap(~site,ncol=1)#3 plots side by side (by site) in single object
barplot.1

#### Bar Plots: Color by DOY - NE ####
#Fill by 'week' to get a color gradient that acurately displays time.  'Rank_DOY' distorted time
# by spreading/compressing slight/large intervals into whole number intervals.
library(viridis)
library(ggplot2)
barplot.2<-ggplot(nNWMHRC, aes(x=reorder(species,rank_DOY), y=mnorm_ne, fill=week))+
# barplot.2<-ggplot(nNWMHRC, aes(x=reorder(species,rank_ne), y=mnorm_ne, fill=week))+
# barplot.2<-ggplot(NWMHRC, aes(x=reorder(species,rank_DOY), y=ne, fill=week))+
# barplot.2<-ggplot(NWMHRC, aes(x=reorder(species,rank_ne), y=ne, fill=week))+
    scale_fill_viridis(option = 'plasma', direction = -1)+
  geom_bar(stat="identity", 
           colour="black", 
           size=0.25)+
    geom_errorbar(aes(ymin=mnorm_ne-mne.se, ymax=mnorm_ne+mne.se), #Normalized error
    # geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se), # RAW error
                width=.4,
                size=0.25)+
  theme_bw(base_size = 15)+
  guides(fill= 'colorbar')+
  theme(legend.position = c(0.1,.85))+
  # guides(fill = FALSE)+
  labs(title='\nMean Natural Enemies - Normalized by Mowed')+
  # labs(title='\nMean Natural Enemies')+
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
  ylab("\nMean natural enemies / sample + SE")+
  xlab("Plant species\n")+
  # geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  geom_hline(yintercept=1, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  # geom_point(data=NWMHRC, aes(x=reorder(species,rank_DOY), y=mowed_ne, group=1))+
  # geom_point(data=NWMHRC, aes(x=reorder(species,rank_ne), y=mowed_ne, group=1))+
    facet_wrap(~site,ncol=1, scales = 'free_x')#3 plots side by side (by site) in single object
barplot.2




#### Bar Plots: Color by DOY - HERB ####
#Fill by 'week' to get a color gradient that acurately displays time.  'Rank_DOY' distorted time
# by spreading/compressing slight/large intervals into whole number intervals.
library(viridis)
library(ggplot2)
# barplot.2<-ggplot(nNWMHRC, aes(x=reorder(species,rank_DOY), y=mnorm_herb, fill=week))+
# barplot.2<-ggplot(nNWMHRC, aes(x=reorder(species,rank_ne), y=mnorm_herb, fill=week))+
barplot.2<-ggplot(NWMHRC, aes(x=reorder(species,rank_DOY), y=herb, fill=week))+
# barplot.2<-ggplot(NWMHRC, aes(x=reorder(species,rank_ne), y=herb, fill=week))+
  scale_fill_viridis(option = 'plasma', direction = -1)+
  geom_bar(stat="identity", 
           colour="black", 
           size=0.25)+
  # geom_errorbar(aes(ymin=mnorm_herb-mherb.se, ymax=mnorm_herb+mherb.se), #Normalized error
  geom_errorbar(aes(ymin=herb-herb.se, ymax=herb+herb.se), # RAW error
                width=.4,
                size=0.25)+
  theme_bw(base_size = 15)+
  guides(fill= 'colorbar')+
  theme(legend.position = c(0.2,.85))+
  # guides(fill = FALSE)+
  # labs(title='\nMean Herbivores - Normalized by Mowed')+
  labs(title='\nMean Herbivores')+
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
  # ylab("\n(Control Mean herbivores / sample + SE) / Mowed Control")+
  ylab("\nMean herbivores / sample + SE")+
  xlab("Plant species\n")+
  # geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  # geom_hline(yintercept=1, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  geom_point(data=NWMHRC, aes(x=reorder(species,rank_DOY), y=mowed_herb, group=1))+
  # geom_point(data=NWMHRC, aes(x=reorder(species,rank_ne), y=mowed_herb, group=1))+
  facet_wrap(~site,ncol=1, scales = 'free_x')#3 plots side by side (by site) in single object
barplot.2





#NMDs Code line s690-824 in Lampyrid code on GitHub ####

#### Summarizing Data #####

#Summarize: Means and SE of taxa (orders) and controls by species
{library(plyr)
summarized.data<-ddply(all.with.controls, c('site', 'species','sitenum'),summarise,
                       arach=mean(arachnida), #NOTE: The destination var cannot have same name as source var.
                       arach.se=sd(arachnida)/sqrt(length(arachnida)),
                       
                       dipt_ne=mean(diptera_ne),
                       dipt_ne.se=sd(diptera_ne)/sqrt(length(diptera_ne)),
                       
                       neurop_ne=mean(neuroptera_ne),
                       neurop_ne.se=sd(neuroptera_ne)/sqrt(length(neuroptera_ne)),
                       
                       coleop_ne=mean(coleoptera_ne),
                       coleop_ne.se=sd(coleoptera_ne)/sqrt(length(coleoptera_ne)),
                       
                       hymenop_ne=mean(hymenoptera_ne),
                       hymenop_ne.se=sd(hymenoptera_ne)/sqrt(length(hymenoptera_ne)),
                       
                       hemip_ne=mean(hemiptera_ne),
                       hemip_ne.se=sd(hemiptera_ne)/sqrt(length(hemiptera_ne)),
                       
                       ne=mean(total_ne),
                       ne.se=sd(total_ne)/sqrt(length(total_ne)),                      
                      
                       herb=mean(total_herb),
                       herb.se=sd(total_herb)/sqrt(length(total_herb)),
                       
                       controls_ne=mean(controls_ne),
                       controls_herb=mean(controls_herb),
                       controls_all=mean(controls_all),
                       mowed_ne=mean(mowed_ne),
                       mowed_herb=mean(mowed_herb),
                       mowed_all=mean(mowed_all),
                       weedy_ne=mean(weedy_ne),
                       weedy_herb=mean(weedy_herb),
                       weedy_all=mean(weedy_all),
                       week=mean(week),
                       DOYphenol=mean(DOY))#average dates of all samples
                       #returns some 'NaN' b/c there is only one sample
}

#Order by site in summarized.data. 
{#Defines 'site' as factor whose order in df can determine chart order
summarized.data$site<-factor(summarized.data$site, levels=unique(summarized.data$site))
#Orders df as 1, 2, 3, (or NW, CRC, SW)
summarized.data<-summarized.data[order(summarized.data$sitenum, 
                                       summarized.data$DOYphenol, 
                                       summarized.data$species),]}

#Summarize: subset individual site data frames from summarized.data
{
  SWMREC<-subset(summarized.data, site == "SWMREC")
CRC<-subset(summarized.data, site == "CRC")
NWMHRC<-subset(summarized.data, site == "NWMHRC")
}

#Ranking
{library(dplyr)
#by Attractiveness
SWMREC$rank_ne <- rank(SWMREC$ne)
CRC$rank_ne <- rank(CRC$ne)
NWMHRC$rank_ne <- rank(NWMHRC$ne)
#By DOYPhneol
SWMREC$rank_DOY <- rank(SWMREC$DOYphenol)
CRC$rank_DOY <- rank(CRC$DOYphenol)
NWMHRC$rank_DOY <- rank(NWMHRC$DOYphenol)
#Merge back together
summarized.data<-rbind(SWMREC,CRC,NWMHRC)
}

#Summarize: combine data from sites (NE, Herb variables only).
{library(plyr)
summary.combined.sites<-ddply(all.with.controls, c('species'),summarise,
                       ne=mean(total_ne),herb=mean(total_herb),
                       ne.se=sd(total_ne)/sqrt(length(total_ne)),
                       herb.se=sd(total_herb)/sqrt(length(total_herb)),
                       DOYphenol=mean(DOY))#average dates of all samples
                        #returns some 'NaN' b/c there is only one sample for a species
}

#Summarize: TOTAL insect abundance by week (including controls)
{library(plyr)
summary.all<-ddply(all.data, c('site', 'week'),summarise,
                        mean_all_arthropods=mean(total_ne+total_herb),
                        sum_all_arthropods=sum(total_ne+total_herb),
                        ne=mean(total_ne),
                        herb=mean(total_herb),
                        ne.se=sd(total_ne)/sqrt(length(total_ne)),
                        herb.se=sd(total_herb)/sqrt(length(total_herb)),
                        DOYphenol=mean(DOY))#average dates of all samples
                        #returns some 'NaN' b/c there is only one sample
}

#Order by site in summary.all. 
{#Defines 'site' as factor whose order in df can determine chart order
summary.all$site<-factor(summary.all$site, levels=unique(summary.all$site))
#Orders df as 1, 2, 3, (or NW, CRC, SW)
summary.all<-summary.all[order(summary.all$sitenum, 
                               summary.all$week),]
}

#### Adding Full Names ####
# Bring in Names file.
names<-read.csv('Names.csv',header=TRUE)
# Merge into data frame
summarized.data<-merge(summarized.data,names,by=c('species'),all.x=TRUE)


#### Normalizing by Controls ####
library(plyr)
norm<-ddply(all.with.controls, c('site', 'species','sitenum'),summarise,
              # norm_ne=mean(total_ne/controls_ne),   #### Don't need to use the vars if not using weedy control. ###
              # ne.se=sd(total_ne/controls_ne)/sqrt(length(total_ne)),
              # 
              # norm_herb=mean(total_herb/controls_herb),
              # herb.se=sd(total_herb/controls_herb)/sqrt(length(total_herb)),
            
              mnorm_ne=mean(total_ne/mowed_ne),
              mne.se=sd(total_ne/mowed_ne)/sqrt(length(total_ne)),
              
              mnorm_herb=mean(total_herb/mowed_herb),
              mherb.se=sd(total_herb/mowed_herb)/sqrt(length(total_herb)),
              week=mean(week),
              DOYphenol=mean(DOY))#average dates of all samples,

#Order by site in norm. 
{#Defines 'site' as factor whose order in df can determine chart order
  norm$site<-factor(norm$site, levels=unique(norm$site))
  #Orders df as 1, 2, 3, (or NW, CRC, SW)
  norm<-norm[order(norm$sitenum, 
                   norm$DOYphenol, 
                   norm$species),]}

#Summarize: subset individual site data frames from norm
{
  nSWMREC<-subset(norm, site == "SWMREC")
  nCRC<-subset(norm, site == "CRC")
  nNWMHRC<-subset(norm, site == "NWMHRC")
}
#Ranking
library(dplyr)
#by Attractiveness
nSWMREC$rank_ne <- rank(nSWMREC$mnorm_ne)
nCRC$rank_ne <- rank(nCRC$mnorm_ne)
nNWMHRC$rank_ne <- rank(nNWMHRC$mnorm_ne)
#By DOYPhneol
nSWMREC$rank_DOY <- rank(nSWMREC$DOYphenol)
nCRC$rank_DOY <- rank(nCRC$DOYphenol)
nNWMHRC$rank_DOY <- rank(nNWMHRC$DOYphenol)
#Merge back together
norm2<-rbind(nSWMREC,nCRC,nNWMHRC)





#### General Code for subsetting columns from a df ####

#both "dplyr" and "plyr" methods work.
library(dplyr)
plots2<- select(controls,week,ne_total)
library(plyr)
plots <- subset(controls, select = c(1, 2, 77))
