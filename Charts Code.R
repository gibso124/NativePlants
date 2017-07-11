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
barplot.1<-ggplot(SWMREC, aes(x=reorder(scientific_name,DOYphenol), y=ne, fill=week))+
  geom_bar(stat="identity",
           colour="black",
           size=0.25)+
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
                width=.4,
                size=0.25)+
  theme_bw(base_size = 15)+
  theme(legend.position = c(0.2,.85))+
  guides(fill=FALSE)+
  labs(title='\nMean Natural Enemies')+
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
  geom_point(data=SWMREC, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))+
  facet_wrap(~site,ncol=1, scales = 'free')#3 plots side by side (by site) in single object
barplot.1

# for PP: Save as
#### Standard Bar Plot - Single site, Full ####
library(ggplot2)
fullsite<-ggplot(SWMREC, aes(x=reorder(scientific_name,DOYphenol), y=ne))+

  geom_bar(stat="identity",
           colour="black",
           size=0.25)+
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
                     width=.4,
                     size=0.25)+
  theme_bw(base_size = 15)+
  # guides(fill= 'colorbar')+
  # theme(legend.position = c(0.2,.85))+
  guides(fill=FALSE)+
  # labs(title='\nMean Natural Enemies')+
  theme(plot.title = element_text(face = 'bold',
                                  size = 20,
                                  hjust = 0.5),
        axis.title = element_text(face = 'plain',
                                  size = 20),
        axis.text.x=element_text(angle=55,
                                 face='plain', #("plain", "italic", "bold", "bold.italic")
                                 size=11,    #(in pts)
                                 #vjust=0,   #(in [0, 1])
                                 hjust=1))+ #(in [0, 1])
  ylab("\nMean natural enemies / sample")+
  xlab("Plant species\n")+
#  geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  geom_point(data=SWMREC, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))+
  facet_wrap(~site,ncol=1, scales = 'free_y')#3 plots side by side (by site) in single object
fullsite

# for PP: Save as PNG, 1800 x 1100 pixels for single site

#### Standard Bar Plot - Grouped Ne/herb for lab meeting ####
library(ggplot2)
barplot.1<-ggplot(N, aes(x=reorder(scientific_name,DOYphenol), y=stack, fill=class))+
  # scale_fill_viridis(option = 'plasma', direction = -1)+
  geom_bar(stat="identity",
           position = 'dodge',
           colour="black",
           size=0.25)+
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
                width=.4,
                size=0.25)+
  geom_errorbar(aes(ymin=-herb-herb.se, ymax=-herb+herb.se),
                width=.4,
                size=0.25)+
  theme_bw(base_size = 15)+
  # guides(fill= 'colorbar')+
  # theme(legend.position = c(0.2,.85))+
  guides(fill=FALSE)+
  # labs(title='\nMean Natural Enemies')+
  theme(plot.title = element_text(face = 'bold',
                                  size = 20,
                                  hjust = 0.5),
        axis.title = element_text(face = 'plain',
                                  size = 20),
        axis.text.x=element_text(angle=55,
                                 face='plain', #("plain", "italic", "bold", "bold.italic")
                                 size=11,    #(in pts)
                                 #vjust=0,   #(in [0, 1])
                                 hjust=1))+ #(in [0, 1])
  ylab("\nMean natural enemies / sample")+
  xlab("Plant species\n")+
  # ylim(-125, 100)+
  #  geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  geom_point(data=N, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))+
  geom_point(data=N, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=-mowed_herb, group=1, fill = 'cross'))+
  facet_wrap(~site,ncol=1, scales = 'free_y')#3 plots side by side (by site) in single object
barplot.1

# for PP: Save as PNG, 1800 x 1100 pixels for single, 
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
  facet_wrap(~ site,ncol=1)#Plots side by side (by site) in single object
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
barplot.1<-ggplot(controls.summary, aes(x=as.factor(week), y=mowed_ne, fill=as.factor(week)))+  #both ne and herb
  scale_fill_viridis(discrete=TRUE, option = 'plasma', direction = -1)+
  geom_bar(stat="identity", 
           colour="black", 
           size=0.25)+
  # geom_errorbar(aes(ymin=mowed_ne-mowed_ne.se, ymax=mowed_ne+mowed_ne.se), 
  # geom_errorbar(aes(ymin=mowed_herb-mowed_herb.se, ymax=mowed_herb+mowed_herb.se),
  geom_errorbar(aes(ymin=mowed_ne-mowed_ne.se, ymax=mowed_ne+mowed_ne.se),
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







#### Histograms ####
# Log10 transform total_ne
all.with.controls$log_total_ne<-log10((all.with.controls$total_ne)+1)

# Subset individual site data frames from all.with.controls
{
  SWMREC.raw<-subset(all.with.controls, site == "SWMREC")
  CRC.raw<-subset(all.with.controls, site == "CRC")
  NWMHRC.raw<-subset(all.with.controls, site == "NWMHRC")
}

library(ggplot2)
histogram<-ggplot(all.with.controls, aes(x=total_ne))+ 
  # geom_histogram(binwidth = 3)+
  geom_histogram(bins = 30)+
  facet_wrap(~site,ncol=1, scales = 'free')#3 plots side by side (by site) in single object
histogram

#### Bar Plots: # Observations by species ####
library(ggplot2)
barplot.1<-ggplot(summarized.data, aes(x=reorder(species,DOYphenol), y=freq, fill=as.factor(species)))+ 
  geom_bar(stat="identity", 
           colour="black", 
           size=0.25)+
  theme_bw(base_size = 15)+
  guides(fill=FALSE)+
  labs(title='\n# Observations per Species')+
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
  ylab("\n# Samples")+
  xlab("Plant species\n")+
  #  geom_vline(xintercept=6.5, color='black',size=.25,linetype='solid')+ #Adds line at location on x-axis. n.0 centers on column, n.5 is b/w columns
  facet_wrap(~site,ncol=1)#3 plots side by side (by site) in single object
barplot.1


#NMDs Code line s690-824 in Lampyrid code on GitHub ####
##########################################################################
# NEW CHARTS
##########################################################################-
#### NE: Full Site Plots ####
#Currently Optimized for 10 x 16 inch PDF
#theme 'bw' has gridlines, theme 'classic' does not
library(ggplot2)
fullsite<-ggplot(ALLsig, aes(x=reorder(scientific_name,DOYphenol), y=ne))+
  geom_bar(stat="identity", fill = 'tomato2', colour="black")+
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
                width=.4,
                size=0.5)+
  theme_classic(base_size = 23)+
  guides(fill=FALSE)+
  ylab("Mean natural enemies / sample ± SE\n")+
  xlab("Plant species")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))+

  # ylim(-14,80) + # Pie chart space: SW & CRC = (-20, 115)  NWMHRC = (-14, 80)
  
  # # Bloom Period Lines
  # geom_vline(xintercept=2.5, color='dark gray',size=0.5,linetype='dashed')+
  # geom_vline(xintercept=9.5, color='dark gray',size=0.5,linetype='dashed')+

  # # Controls
  # geom_point(data=SWsig, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))
  geom_point(data=ALLsig, size = 3, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))
fullsite

#save to pdf
pdf("Significant only - 2015 NE statewide means - red.pdf", height=10, width=16) # line widths and font size are optimized for this PDF size
fullsite
dev.off()

# Bloom period line  distances:
# SWMREC full site    16.5    35.5 
# CRC    full site    11.5    26.5
# NWMHRC full site    11.5    27.5 
# SWMREC Sig only     2.5     7.5
# CRC    Sig Only     2.5     7.5
# NWMHRC Sig Only     2.5     9.5


#### NE: Early, Mid, Late Plots ####
library(ggplot2)
bp<-ggplot(SWMREC.E, aes(x=reorder(sci_name,DOYphenol), y=ne))+
  geom_bar(stat="identity", colour="black")+
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
                width=.4,
                size=0.5)+
  theme_classic(base_size = 23)+
  guides(fill=FALSE)+
  ylab("Mean natural enemies / sample\n")+
  xlab("Plant species")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))+
  geom_point(data=SWMREC.E, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))
bp

#save to pdf
pdf("INTRO.PLOT.pdf", height=8, width=10) # line widths and font size are optimized for this PDF size
bp
dev.off()







#### NE & Herb: up/down Full Site Plots (offset bars) ####
#Currently Optimized for 10 x 16 inch PDF
#theme 'bw' has gridlines, theme 'classic' does not
library(ggplot2)
neherb<-ggplot(C.sig, aes(x=reorder(sci_name,DOYphenol), y=abundance, fill=class))+
  geom_bar(stat="identity",
           position = 'dodge',
           colour="black")+
  
  scale_fill_manual(values=c("lightgoldenrod3", "tomato2")) + #Colors to match pp headings
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
  # geom_errorbar(aes(ymin=ne-ne.se, ymax=ne),
                width=.2,
                size=0.5,
                position = position_nudge(x = 0.23))+         # CENTERS ERROR BARS
  geom_errorbar(aes(ymin=-herb-herb.se, ymax=-herb+herb.se),
  # geom_errorbar(aes(ymin=-herb, ymax=-herb+herb.se),
                width=.2,
                size=0.5,
                position = position_nudge(x = -0.23))+
  

  theme_classic(base_size = 23)+
  guides(fill=FALSE)+
  ylab("Mean arthropods / sample ± SE\n")+
  xlab("Plant species")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))+
  
  # ylim(-130, 115) + # tO ACCOUNT FOR SYSE2 OUTLIER AT CRC
  # expand_limits(y=-55) + # Pie space. SWMREC = -120    CRC = -150     NWMHRC = -55
  
  geom_vline(xintercept=2.5, color='dark gray',size=0.5,   linetype='dashed')+ 
  geom_vline(xintercept=9.5, color='dark gray',size=0.5,   linetype='dashed')+ 

  geom_point(data=C.sig, size = 3, aes(x=as.numeric(reorder(sci_name,DOYphenol)), 
                         y=mowed_ne, group=1), position = position_nudge(x = +0.23))+
  geom_point(data=C.sig, size = 3, aes(x=as.numeric(reorder(sci_name,DOYphenol)), 
                         y=-mowed_herb, group=1), position = position_nudge(x = -0.23))
neherb

#save to pdf
pdf("N stacked PP SIG ONLY .pdf", height=10, width=16) # line widths and font size are optimized for this PDF size
neherb
dev.off()

# Bloom period line  distances:
# SWMREC full site    16.5    35.5 
# CRC    full site    11.5    26.5
# NWMHRC full site    11.5    27.5 
# SWMREC Sig only     2.5     7.5
# CRC    Sig Only     2.5     7.5
# NWMHRC Sig Only     2.5     9.5

#### NE & Herb: up/down Full Site Plots (directly stacked bars) ####
#Currently Optimized for 10 x 16 inch PDF
#theme 'bw' has gridlines, theme 'classic' does not
library(ggplot2)
neherb<-ggplot(N, aes(x=reorder(sci_name,DOYphenol), y=abundance, fill=class))+
  geom_bar(stat="identity",
           colour="black")+
  
  scale_fill_manual(values=c("lightgoldenrod3", "tomato2")) + #Colors to match pp headings
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
                # geom_errorbar(aes(ymin=ne-ne.se, ymax=ne),
                width=.4,
                size=0.5)+        
  geom_errorbar(aes(ymin=-herb-herb.se, ymax=-herb+herb.se),
                # geom_errorbar(aes(ymin=-herb, ymax=-herb+herb.se),
                width=.4,
                size=0.5)+
  
  theme_classic(base_size = 23)+
  guides(fill=FALSE)+
  ylab("Mean arthropods / sample ± SE\n")+
  xlab("Plant species")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))+
  # ylim(-120, 100) + # tO ACCOUNT FOR SYSE2 OUTLIER AT CRC full site
  # expand_limits(y=-55) + # Pie space. SWMREC = -120    CRC = -150     NWMHRC = -55
  
  geom_vline(xintercept=16.5, color='dark gray',size=0.5,   linetype='dashed')+ 
  geom_vline(xintercept=35.5, color='dark gray',size=0.5,   linetype='dashed')+ 
  
  geom_point(data=N, size = 2, aes(x=as.numeric(reorder(sci_name,DOYphenol)), 
                                       y=mowed_ne, group=1))+
  geom_point(data=N, shape = 17, size = 2, aes(x=as.numeric(reorder(sci_name,DOYphenol)), 
                                       y=-mowed_herb, group=1))
neherb

#save to pdf
pdf("NWMHRC direct stacked.pdf", height=10, width=16) # line widths and font size are optimized for this PDF size
neherb
dev.off()

# Bloom period line  distances:
# SWMREC full site    16.5    35.5 
# CRC    full site    11.5    26.5
# NWMHRC full site    11.5    27.5 
# SWMREC Sig only     2.5     7.5
# CRC    Sig Only     2.5     7.5
# NWMHRC Sig Only     2.5     9.5

#### NE: Pie Charts  ####
#theme 'bw' has gridlines, theme 'classic' does not
library(RColorBrewer)
library(ggplot2)
# pie<-ggplot(mtaxa.ne,
pie<-ggplot(data = subset(mtaxa.ne, site == 'NWMHRC'),
            aes(x=factor(1), 
                y=abundance, 
                fill=factor(order)))+
  theme_classic()+
  scale_fill_brewer(type = 'qual', palette = 7, guide = 'legend') +
  geom_bar(stat = 'identity', width = 1, position = 'fill') + # Add colour = 'black' when faceting down to site*species
  xlab('')+
  ylab('')+
  scale_y_continuous(
    breaks=NULL,
    labels=NULL)+
  
  #  # Use this only when df in use is fully facetted.
  # scale_y_continuous( 
  #   breaks=cumsum(mtaxa.ne$abundance) - mtaxa.ne$abundance/2,
  #   labels=mtaxa.ne$order)+
  
  # facet_grid(facets = .(~site, ~species))+
  facet_wrap(~reorder(species, DOYphenol), ncol = 7)+
  coord_polar(theta = 'y')
pie

#save to pdf
pdf("NWMHRC Ne taxa - facetted by species.pdf", height=11, width=8.5) # single: 3x5 | 3 sites: 2x6 | all species: 11x8.5
pie
dev.off()



#### NE: Coxcomb Charts  ####
#theme 'bw' has gridlines, theme 'classic' does not
library(RColorBrewer)
library(ggplot2)
# coxcomb<-ggplot(data = subset(mtaxa.ne, site == 'NWMHRC'),
coxcomb<-ggplot(ne3,
            aes(x=factor(taxa), 
                y=count, 
                fill=factor(taxa)))+
  theme_classic()+
  scale_fill_brewer(type = 'qual', palette = 7, guide = 'legend') +
  geom_bar(stat = 'identity', width = 1) + # Add colour = 'black' when faceting down to site*species
  
  labs(title='\n Sum NEs 2015 - all sites')+
  
  xlab('')+
  ylab('')+
  theme(axis.text.x=element_blank())+
  # scale_y_continuous(
  #   breaks=NULL,
  #   labels=NULL)+

  #  # Use this only when df in use is fully facetted.
  # scale_y_continuous( 
  #   breaks=cumsum(mtaxa.ne$abundance) - mtaxa.ne$abundance/2,
  #   labels=mtaxa.ne$order)+
  
  # geom_vline(xintercept=1.5, color='gray',size=0.5,linetype='dashed')+ 
  
  # facet_grid(facets = .(~reorder(species, DOYphenol), ~site))+
  # facet_wrap(~reorder(species, DOYphenol), ncol = 7)+
  facet_wrap(~site, ncol = 3)+
  
  coord_polar()
coxcomb

#save to pdf
pdf("Coxcomb - Sum NEs facetted by site.pdf", height=4, width=6) # single: 3x5 | 3 sites: 2x6 | all species: 11x8.5
coxcomb
dev.off()


#### HERB: Preliminary viewing ####
library(ggplot2)
# herbs<-ggplot(mtaxa.herb, aes(x=reorder(species,DOYphenol), y=count))+
herbs<-ggplot(mtaxa.herb,
                aes(x=factor(taxa), 
                    y=count, 
                    fill=factor(taxa)))+
  geom_bar(stat="identity")+

  theme_classic(base_size = 10)+
  # guides(fill=FALSE)+
  labs(title='\n Total Herbivores 2015 - all sites')+
  ylab("Sum herbivores \n")+
  xlab("herbivore taxa")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))+
# facet_wrap(~reorder(species, DOY), ncol = 7, scales = 'free_y')
facet_wrap(~site, ncol = 3)

  
herbs

#save to pdf
pdf("total herbivores - facetted by site.pdf", height=8.5, width=11) # line widths and font size are optimized for this PDF size
herbs
dev.off()

# Stacked Graph
herbs2<-ggplot(mtaxa.herb,aes(x=site, 
    y=count, 
    fill=factor(taxa)))+
  geom_bar(stat = 'identity') +
  # guides(fill=FALSE)+
  theme_classic(base_size = 10)+
  labs(title='\n Total Herbivores 2015 - all sites')+
  ylab("Sum herbivores \n")+
  xlab("herbivore taxa")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))
  # facet_wrap(~reorder(species, DOY), ncol = 7, scales = 'free_y')
  # facet_wrap(~site, ncol = 3)


herbs2


#### HERB: Pie Charts (Experimental) ####

# df "mtaxa.herb" is not summarized, just stacked, so charts take a LOOOONG time to process
# and have unwanted lines because the individual samples have not been averaged or summed.
# Need to make a "summarized.data" type df for herbs, then melt that.

library(RColorBrewer)
library(ggplot2)
# pie2<-ggplot(mtaxa.herb,
pie2<-ggplot(data = subset(mtaxa.herb, site == 'NWMHRC'),
            aes(x=factor(1), 
                y=count, 
                fill=factor(taxa)))+
  theme_classic()+
  # scale_fill_brewer(type = 'qual', palette = 7, guide = 'legend') +
  geom_bar(stat = 'identity', width = 1, position = 'fill') + # Add colour = 'black' when faceting down to site*species
  xlab('')+
  ylab('')+
  scale_y_continuous(
    breaks=NULL,
    labels=NULL)+
  
  #  # Use this only when df in use is fully facetted.
  # scale_y_continuous( 
  #   breaks=cumsum(mtaxa.ne$abundance) - mtaxa.ne$abundance/2,
  #   labels=mtaxa.ne$order)+
  
  # facet_grid(facets = .(~site, ~species))+
  facet_wrap(~reorder(species, DOY), ncol = 7)+
  coord_polar(theta = 'y')
pie2

#save to pdf
pdf("NWMHRC Herb taxa - Pies facetted by species.pdf", height=11, width=8.5) # single: 3x5 | 3 sites: 2x6 | all species: 11x8.5
pie
dev.off()



##########################################################################
#### Summarizing Data (means) ##### ----

### Primary Summarizing ##
{
  #### Create df 'summarized.data':  Means and SE of taxa (orders) and controls by species and site from all.with.controls
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
                           
                           mowed_ne=mean(mowed_ne),
                           mowed_herb=mean(mowed_herb),
                           mowed_all=mean(mowed_all),

                           week=mean(week),
                           DOYphenol=mean(DOY))#average dates of all samples
    #returns some 'NaN' b/c there is only one sample
  }
  
  ## Add Full Names
  {# Bring in Names file.
    names<-read.csv('Names.csv',header=TRUE)
    # Merge into data frame
    summarized.data<-merge(summarized.data,names,by=c('species'),all.x=TRUE)
  }
  
  #Create 'freq' = # observations for species at a site  **This is broken***
  {
    #Create variable
    library(plyr)
    freq<-count(all.with.controls, c('site','species'), wt = NULL)
    #Merge into summarized.data
    summarized.data<-merge(summarized.data,freq,by = c('site','species'))
  }
  
  #Order by site. (Needed to get desired faceting order)
  {#Defines 'site' as factor whose order in df can determine chart order
    summarized.data$site<-factor(summarized.data$site, levels=unique(summarized.data$site))
    #Orders df as 1, 2, 3, (or NW, CRC, SW)
    summarized.data<-summarized.data[order(summarized.data$sitenum, 
                                           summarized.data$DOYphenol, 
                                           summarized.data$species),]}
  
  # subset summarized.data by individual site data frames. dfs = 'SWMREC', ... 'NWMHRC'
  {
    SWMREC<-subset(summarized.data, site == "SWMREC")
    CRC<-subset(summarized.data, site == "CRC")
    NWMHRC<-subset(summarized.data, site == "NWMHRC")
  }
  
  #Subset summarized.data by site AND Bloom Period. dfs = 'SMWREC.E', 'SWMREC.M', ... 'NWMHRC.L'
  {
    SWMREC.E<-subset(summarized.data, site == 'SWMREC' & DOYphenol < 200 )
    SWMREC.M<-subset(summarized.data, site == 'SWMREC' & DOYphenol > 200 & DOYphenol < 230 )
    SWMREC.L<-subset(summarized.data, site == 'SWMREC' & DOYphenol > 230 )
    
    CRC.E<-subset(summarized.data, site == 'CRC' & DOYphenol < 202 )
    CRC.M<-subset(summarized.data, site == 'CRC' & DOYphenol > 202 & DOYphenol < 230 )
    CRC.L<-subset(summarized.data, site == 'CRC' & DOYphenol > 230 )
    
    NWMHRC.E<-subset(summarized.data, site == 'NWMHRC' & DOYphenol < 200 )
    NWMHRC.M<-subset(summarized.data, site == 'NWMHRC' & DOYphenol > 200 & DOYphenol < 233 )
    NWMHRC.L<-subset(summarized.data, site == 'NWMHRC' & DOYphenol > 233 )
    
  }
  
  #### Create df 'stack':  Split summarized.data to stack NE and Herb
  # new vars = 'class' 'abundance'
  {
  # Create new vars 'class' and 'abundance'
  ne<-summarized.data
  ne$abundance<-ne$ne
  ne$class<-'ne'
  herb<-summarized.data
  herb$abundance<- -herb$herb
  herb$class<-'herb'
  #Merge back together
  stack<-rbind(ne, herb)
  stack$class<-as.factor(stack$class)
  }
  
  # Subset 'stack' by site. dfs = 'S' 'C' 'N'
  {
  S<-subset(stack, site == 'SWMREC')
  C<-subset(stack, site == 'CRC')
  N<-subset(stack, site == 'NWMHRC')
  }
  
  # Subset 'stack' by site AND Bloom Period. dfs = 'S.E', 'S.M', ... 'N.L'
  {
    S.E<-subset(stack, site == 'SWMREC' & DOYphenol < 200 )
    S.M<-subset(stack, site == 'SWMREC' & DOYphenol > 200 & DOYphenol < 230 )
    S.L<-subset(stack, site == 'SWMREC' & DOYphenol > 230 )
    
    C.E<-subset(stack, site == 'CRC' & DOYphenol < 202 )
    C.M<-subset(stack, site == 'CRC' & DOYphenol > 202 & DOYphenol < 230 )
    C.L<-subset(stack, site == 'CRC' & DOYphenol > 230 )
    
    N.E<-subset(stack, site == 'NWMHRC' & DOYphenol < 200 )
    N.M<-subset(stack, site == 'NWMHRC' & DOYphenol > 200 & DOYphenol < 233 )
    N.L<-subset(stack, site == 'NWMHRC' & DOYphenol > 233 )
  }

  
  ##Reshape data for ne taxa stacked plots. new df = 'mtaxa.ne'
  # ne vars = 'order' 'abundance'
  {#Extract columns
    library(plyr)
    taxa.ne <- subset(summarized.data, select = c(1, 2, 3,4,6,8,10,12,14))
    DOY <- subset(summarized.data, select = c(1,2,23,24,25,26,27))
    
    #melt to long form, rename columns
    library("reshape")
    mtaxa.ne<-melt(taxa.ne, id=c("site","species",'sitenum'))
    names(mtaxa.ne)[names(mtaxa.ne) == 'variable']<-'order'
    names(mtaxa.ne)[names(mtaxa.ne) == 'value']<-'abundance'
    #Merge DOY into mtaxa.ne for ordering
    mtaxa.ne<-merge(mtaxa.ne,DOY,by = c('site','species'),all = TRUE)
    
    #Defines 'site' as factor whose order in df can determine chart order
    mtaxa.ne$site<-factor(mtaxa.ne$site, levels=unique(mtaxa.ne$site))
    mtaxa.ne$sitenum<-as.character(mtaxa.ne$sitenum)
    #Orders df as 1, 2, 3, (or NW, CRC, SW)
    mtaxa.ne<-mtaxa.ne[order(mtaxa.ne$sitenum, 
                             mtaxa.ne$DOYphenol, 
                             mtaxa.ne$species),]
  }
}

#Subset of significant (T-TEST AGAINST MOWED) species from 'summarized.data'
{ #SWMREC
  SWsig<-summarized.data[which(summarized.data$site == 'SWMREC'),]
SWsig<-SWsig[which(SWsig$species == 'ACMI2' | SWsig$species == 'LOCO6' |
                        SWsig$species == 'MOFI' | SWsig$species == 'SONE' |
                        SWsig$species == 'PYVI' | SWsig$species == 'MOPU' |
                        SWsig$species == 'PYPI' | SWsig$species == 'HEOC2' |
                        SWsig$species == 'COTR4' | SWsig$species == 'OLRI' |
                        SWsig$species == 'HEST' | SWsig$species == 'SYSE2' |
                        SWsig$species == 'SYOO' | SWsig$species == 'SOSP2' ),]

#CRC
CRCsig<-summarized.data[which(summarized.data$site == 'CRC'),]
CRCsig<-CRCsig[which(CRCsig$species == 'ACMI2' | CRCsig$species == 'ASVE' |
                      CRCsig$species == 'CESTM' | CRCsig$species == 'COTR4' |
                      CRCsig$species == 'DAFR6' | CRCsig$species == 'HEOC2' |
                      CRCsig$species == 'HEST' | CRCsig$species == 'LOCO6' |
                      CRCsig$species == 'MOFI' | CRCsig$species == 'PYVI' |
                      CRCsig$species == 'OLRI' | CRCsig$species == 'SIIN2' |
                      CRCsig$species == 'SOJU' | CRCsig$species == 'SONE' ),]

#NWMHRC
NWsig<-summarized.data[which(summarized.data$site == 'NWMHRC'),]
NWsig<-NWsig[which(NWsig$species == 'ACMI2' | NWsig$species == 'ASVE' |
                     NWsig$species == 'CESTM' | NWsig$species == 'CHANA2' |
                     NWsig$species == 'COPA10' | NWsig$species == 'COTR4' |
                     NWsig$species == 'DAPU5' | NWsig$species == 'ECPU' |
                     NWsig$species == 'ERYU' | NWsig$species == 'HEOC2' |
                     NWsig$species == 'HEST' | NWsig$species == 'MOPU' |
                     NWsig$species == 'OLRI' | NWsig$species == 'PEHI' |
                     NWsig$species == 'PYPI' | NWsig$species == 'PYVI' |
                     NWsig$species == 'RAPI' | NWsig$species == 'RUHI2' |
                     NWsig$species == 'SIIN2' | NWsig$species == 'SOJU' |
                     NWsig$species == 'SONE' | NWsig$species == 'SOSP2' ),]


#### Create df 'ALLsig':  Means and SE of total ne by species from all.with.controls
## This is mean across sites.
{library(plyr)
  ALLsig<-ddply(all.with.controls, c('species'),summarise,
                         
                         ne=mean(total_ne),
                         ne.se=sd(total_ne)/sqrt(length(total_ne)),                      

                         
                         mowed_ne=mean(mowed_ne),
                         
                         week=mean(week),
                         DOYphenol=mean(DOY))#average dates of all samples
  #returns some 'NaN' b/c there is only one sample
}
## Add Full Names to 'ALLsig'
{# Bring in Names file.
  names<-read.csv('Names.csv',header=TRUE)
  # Merge into data frame
  ALLsig<-merge(ALLsig,names,by=c('species'),all.x=TRUE)
}
## Subset to only significant plants
{ALLsig<-ALLsig[which(ALLsig$species == 'PEHI' | ALLsig$species == 'ACMI2' |
                       ALLsig$species == 'LOCO6' | ALLsig$species == 'RUHI2' |
                       ALLsig$species == 'COPA10' | ALLsig$species == 'MOFI' |
                       ALLsig$species == 'SONE' | ALLsig$species == 'PYVI' |
                       ALLsig$species == 'DAFR6' | ALLsig$species == 'RAPI' |
                       ALLsig$species == 'CESTM' | ALLsig$species == 'CHANA2' |
                       ALLsig$species == 'ASVE' | ALLsig$species == 'MOPU' |
                       ALLsig$species == 'ERYU' | ALLsig$species == 'ECPU' |
                       ALLsig$species == 'SOJU' | ALLsig$species == 'PYPI' |
                       ALLsig$species == 'DAPU5' | ALLsig$species == 'HEOC2' |
                       ALLsig$species == 'COTR4' | ALLsig$species == 'SIIN2' |
                       ALLsig$species == 'OLRI' | ALLsig$species == 'HEST' |
                       ALLsig$species == 'SYSE2' | 
                       ALLsig$species == 'SYOO' | ALLsig$species == 'SOSP2' ),]
  }

}

#Subset of significant (T-TEST AGAINST MOWED) species from 'stack'
{ # SWMREC
  S.sig<-stack[which(stack$site == 'SWMREC'),]
  S.sig<-S.sig[which(S.sig$species == 'ACMI2' | S.sig$species == 'LOCO6' |
                       S.sig$species == 'MOFI' | S.sig$species == 'SONE' |
                       S.sig$species == 'PYVI' | S.sig$species == 'MOPU' |
                       S.sig$species == 'PYPI' | S.sig$species == 'HEOC2' |
                       S.sig$species == 'COTR4' | S.sig$species == 'OLRI' |
                       S.sig$species == 'HEST' | S.sig$species == 'SYSE2' |
                       S.sig$species == 'SYOO' | S.sig$species == 'SOSP2' ),]
  
  #CRC
  C.sig<-stack[which(stack$site == 'CRC'),]
  C.sig<-C.sig[which(C.sig$species == 'ACMI2' | C.sig$species == 'ASVE' |
                       C.sig$species == 'CESTM' | C.sig$species == 'COTR4' |
                       C.sig$species == 'DAFR6' | C.sig$species == 'HEOC2' |
                       C.sig$species == 'HEST' | C.sig$species == 'LOCO6' |
                       C.sig$species == 'MOFI' | C.sig$species == 'PYVI' |
                       C.sig$species == 'OLRI' | C.sig$species == 'SIIN2' |
                       C.sig$species == 'SOJU' | C.sig$species == 'SONE' ),]
  
  #NWMHRC
  N.sig<-stack[which(stack$site == 'NWMHRC'),]
  N.sig<-N.sig[which(N.sig$species == 'ACMI2' | N.sig$species == 'ASVE' |
                       N.sig$species == 'CESTM' | N.sig$species == 'CHANA2' |
                       N.sig$species == 'COPA10' | N.sig$species == 'COTR4' |
                       N.sig$species == 'DAPU5' | N.sig$species == 'ECPU' |
                       N.sig$species == 'ERYU' | N.sig$species == 'HEOC2' |
                       N.sig$species == 'HEST' | N.sig$species == 'MOPU' |
                       N.sig$species == 'OLRI' | N.sig$species == 'PEHI' |
                       N.sig$species == 'PYPI' | N.sig$species == 'PYVI' |
                       N.sig$species == 'RAPI' | N.sig$species == 'RUHI2' |
                       N.sig$species == 'SIIN2' | N.sig$species == 'SOJU' |
                       N.sig$species == 'SONE' | N.sig$species == 'SOSP2' ),]
  
}

### All Families Summary##
{
  #### Create df 'summarized.data.families':  Means and SE of taxa (original) and controls by species and site from all.with.controls
  {library(plyr)
    summarized.data.families<-ddply(all.with.controls, c('site', 'species','sitenum'),summarise,
                                    arach=mean(arachnida), #NOTE: The destination var cannot have same name as source var.
                                    arach.se=sd(arachnida)/sqrt(length(arachnida)),
                                    
                                    anthocorid=mean(anthocoridae),
                                    anthocorid.se=sd(anthocoridae)/sqrt(length(anthocoridae)),
                                    
                                    nabid=mean(nabidae),
                                    nabid.se=sd(nabidae)/sqrt(length(nabidae)),
                                    
                                    reduviid=mean(reduviidae),
                                    reduviid.se=sd(reduviidae)/sqrt(length(reduviidae)),
                                    
                                    aphid=mean(aphidae),
                                    aphid.se=sd(aphidae)/sqrt(length(aphidae)),
                                    
                                    cercopid=mean(cercopidae),
                                    cercopid.se=sd(cercopidae)/sqrt(length(cercopidae)),
                                    
                                    cicadellid=mean(cicadellidae),
                                    cicadellid.se=sd(cicadellidae)/sqrt(length(cicadellidae)),
                                    
                                    mirid=mean(herb_miridae),
                                    mirid.se=sd(herb_miridae)/sqrt(length(herb_miridae)),
                                    
                                    pentatomid=mean(pentatomidae),
                                    pentatomid.se=sd(pentatomidae)/sqrt(length(pentatomidae)),
                                    
                                    tingid=mean(tingidae),
                                    tingid.se=sd(tingidae)/sqrt(length(tingidae)),
                                    
                                    imm_homopt=mean(imm_homopteran),
                                    imm_homopt.se=sd(imm_homopteran)/sqrt(length(imm_homopteran)),
                                    
                                    hoppers=mean(cercopidae+cicadellidae+imm_homopteran),
                                    hoppers.se=sd(cercopidae+cicadellidae+imm_homopteran)/sqrt(length(cercopidae+cicadellidae+imm_homopteran)),
                                    
                                    cantharid=mean(cantharidae),
                                    cantharid.se=sd(cantharidae)/sqrt(length(cantharidae)),
                                    
                                    carabid=mean(carabidae),
                                    carabid.se=sd(carabidae)/sqrt(length(carabidae)),
                                    
                                    coccinellid=mean(coccinellidae),
                                    coccinellid.se=sd(coccinellidae)/sqrt(length(coccinellidae)),
                                    
                                    cerambicid=mean(cerambicidae),
                                    cerambicid.se=sd(cerambicidae)/sqrt(length(cerambicidae)),
                                    
                                    chrysomelid=mean(chrysomelidae),
                                    chrysomelid.se=sd(chrysomelidae)/sqrt(length(chrysomelidae)),
                                    
                                    curculionid=mean(curculionidae),
                                    curculionid.se=sd(curculionidae)/sqrt(length(curculionidae)),
                                    
                                    elaterid=mean(elateridae),
                                    elaterid.se=sd(elateridae)/sqrt(length(elateridae)),
                                    
                                    scarabid=mean(scarabidae),
                                    scarabid.se=sd(scarabidae)/sqrt(length(scarabidae)),
                                    
                                    p.japonica=mean(Jap_beetles),
                                    p.japonica.se=sd(Jap_beetles)/sqrt(length(Jap_beetles)),
                                    
                                    chrysopid=mean(chrysopidae),
                                    chrysopid.se=sd(chrysopidae)/sqrt(length(chrysopidae)),
                                    
                                    hemerobiid=mean(hemerobiidae),
                                    hemerobiid.se=sd(hemerobiidae)/sqrt(length(hemerobiidae)),
                                    
                                    parasitoids=mean(parasitoid_wasps),
                                    parasitoids.se=sd(parasitoid_wasps)/sqrt(length(parasitoid_wasps)),
                                    
                                    sphecid=mean(sphecidae),
                                    sphecid.se=sd(sphecidae)/sqrt(length(sphecidae)),
                                    
                                    tiphiid=mean(tiphiidae),
                                    tiphiid.se=sd(tiphiidae)/sqrt(length(tiphiidae)),
                                    
                                    vespid=mean(vespidae),
                                    vespid.se=sd(vespidae)/sqrt(length(vespidae)),
                                    
                                    bombyliid=mean(bombyliidae),
                                    bombyliid.se=sd(bombyliidae)/sqrt(length(bombyliidae)),
                                    
                                    syrphid=mean(syrphidae),
                                    syrphid.se=sd(syrphidae)/sqrt(length(syrphidae)),
                                    
                                    tachinid=mean(tachinidae),
                                    tachinid.se=sd(tachinidae)/sqrt(length(tachinidae)),
                                    
                                    POSS_TACH=mean(POSS_TACHINID),
                                    POSS_TACH.se=sd(POSS_TACHINID)/sqrt(length(POSS_TACHINID)),
                                    
                                    orthopt=mean(orthoptera),
                                    orthopt.se=sd(orthoptera)/sqrt(length(orthoptera)),
                                    
                                    lep_adults=mean(lep_adult),
                                    lep_adults.se=sd(lep_adult)/sqrt(length(lep_adult)),
                                    
                                    lep_cats=mean(lep_caterpillar),
                                    lep_cats.se=sd(lep_caterpillar)/sqrt(length(lep_caterpillar)),
                                    
                                    meloid=mean(meloidae),
                                    meloid.se=sd(meloidae)/sqrt(length(meloidae)),
                                    
                                    psyllid=mean(psyllidae),
                                    psyllid.se=sd(psyllidae)/sqrt(length(psyllidae)),
                                    
                                    unk_23=mean(unk_beetle_23),
                                    unk_23.se=sd(unk_beetle_23)/sqrt(length(unk_beetle_23)),
                                    
                                    ne=mean(total_ne),
                                    ne.se=sd(total_ne)/sqrt(length(total_ne)),                      
                                    
                                    herb=mean(total_herb),
                                    herb.se=sd(total_herb)/sqrt(length(total_herb)),
                                    
                                    mowed_ne=mean(mowed_ne),
                                    mowed_herb=mean(mowed_herb),
                                    mowed_all=mean(mowed_all),
                                    
                                    week=mean(week),
                                    DOYphenol=mean(DOY))#average dates of all samples
    #returns some 'NaN' b/c there is only one sample
  }
  
  ## Add Full Names
  {# Bring in Names file.
    names<-read.csv('Names.csv',header=TRUE)
    # Merge into data frame
    summarized.data.families<-merge(summarized.data.families,names,by=c('species'),all.x=TRUE)
  }
  
  ## Export
  write.csv(summarized.data.families, 'summarized.data.families--3.3.2017.csv')
  
}

####Herbivore taxa (sums) ##### -----

#subset data
library(dplyr)
taxa.herb<- select(all.nocontrols, DOY, week, sitenum,site,species,
            block,sample_ident_vac,aphidae,cercopidae,cicadellidae,herb_miridae,
             pentatomidae,tingidae,imm_homopteran,cerambicidae,
             chrysomelidae,curculionidae,elateridae,scarabidae,
             orthoptera,lep_adult,lep_caterpillar)



#melt the matrix, rename columns
library("reshape")
mtaxa.herb<-melt(taxa.herb, id=c('DOY', 'week', 'sitenum','site','species',
            'block','sample_ident_vac'))
names(mtaxa.herb)[names(mtaxa.herb) == 'variable']<-'taxa'
names(mtaxa.herb)[names(mtaxa.herb) == 'value']<-'count'

#Defines 'site' and 'sitenum' as factors whose order in df can determine chart order
mtaxa.herb$site<-factor(mtaxa.herb$site, levels=unique(mtaxa.herb$site))
mtaxa.herb$sitenum<- as.numeric(as.vector(mtaxa.herb$sitenum))
#Orders df as 1, 2, 3, (or NW, CRC, SW)
mtaxa.herb<-mtaxa.herb[order(mtaxa.herb$sitenum,
                             mtaxa.herb$DOY, 
                             mtaxa.herb$species),]


library(plyr)
  herbtaxa<-ddply(mtaxa.herb, c('site','sitenum','taxa'),summarise,
                         count=sum(count))
  
  
#### Natural Enemy taxa (sums) ##### ----
  
  #subset data
  library(dplyr)
  ne2<- select(all.nocontrols, DOY, week, sitenum,site,species,
                     block,sample_ident_vac, arachnida, diptera_ne,
                     neuroptera_ne,coleoptera_ne,hymenoptera_ne,hemiptera_ne)
  #melt the matrix, rename columns
  library("reshape")
  ne2<-melt(ne2, id=c('DOY', 'week', 'sitenum','site','species',
                                   'block','sample_ident_vac'))
  names(ne2)[names(ne2) == 'variable']<-'taxa'
  names(ne2)[names(ne2) == 'value']<-'count'
  
  #Defines 'site' and 'sitenum' as factors whose order in df can determine chart order
  ne2$site<-factor(ne2$site, levels=unique(ne2$site))
  ne2$sitenum<- as.numeric(as.vector(ne2$sitenum))
  #Orders df as 1, 2, 3, (or NW, CRC, SW)
  ne2<-ne2[order(ne2$sitenum,
                 ne2$DOY, 
                 ne2$species),]
  
  library(plyr)
  ne3<-ddply(ne2, c('site','sitenum','taxa'),summarise,
                  count=sum(count))
#####-

#Ranking by bloom time
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


#### NOTES from LOGAN 2-7-2017 ####

# My data looks like Poisson distribution overall. Check into this.
# Does the overall distribution matter or individual species?
# Logan's cut off for analyzing a species was that it have at least 6 observations,
# allowing him to remove the species that were distorting his model because they
# had too few data points.
# Binomial distribution is better for over-dispersed data, and is robust against differences 
# in samples sizes between treatments.

#Unknown package
#To Build negative binomial model.  
glm.Nb(x~y, ....)
#To conduct pairwise comparisons referencing the model above.
#Logan used Tukey (only because it is standard and known)
gLHT()

#Unkown package(s)
#Function could be glmmAbb or glmmbuild
#Model that is highly customizable and should fit the data well 
glmmabb(x~y,family = Poisson)


#### NE data subset for Logan for plant traits analysis ####
CRC.NE<-summarized.data
CRC.NE<-subset(CRC.NE, site == 'CRC')
CRC.NE<-subset(CRC.NE, select = c(23,24,3,2,1,16,17,18,19,25,26,27,4,5,6,7,8,9,10,11,12,13,14,15))
write.csv(CRC.NE, 'CRC NE data Summary 2.23.2017.csv')
