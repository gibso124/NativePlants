#### Reshape data for NE: Full Site Plots. new df = summarized.data ----
# Means and SE of taxa (orders) and controls by 
#  species and site from all.with.controls
{library(plyr)
  summarized.data<-ddply(all.with.controls, c('species', 'site','sitenum',
                                              'scientific_name','sci_name',
                                              'common_name'),summarise,
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
                         
                         # 
                         # 
                         # 
                         # 
                         # aphid=mean(aphidae),
                         # aphid.se=sd(aphidae)/sqrt(length(aphidae)),
                         # 
                         # cercopid=mean(cercopidae),
                         # cercopid.se=sd(cercopidae)/sqrt(length(cercopidae)),
                         # 
                         # cicadellid=mean(cicadellidae),
                         # cicadellid.se=sd(cicadellidae)/sqrt(length(cicadellidae)),
                         # 
                         # mirid=mean(herb_miridae),
                         # mirid.se=sd(herb_miridae)/sqrt(length(herb_miridae)),
                         # 
                         # pentatomid=mean(pentatomidae),
                         # pentatomid.se=sd(pentatomidae)/sqrt(length(pentatomidae)),
                         # 
                         # tingid=mean(tingidae),
                         # tingid.se=sd(tingidae)/sqrt(length(tingidae)),
                         # 
                         # imm_homopt=mean(imm_homopteran),
                         # imm_homopt.se=sd(imm_homopteran)/sqrt(length(imm_homopteran)),
                         # 
                         # hoppers=mean(cercopidae+cicadellidae+imm_homopteran),
                         # hoppers.se=sd(cercopidae+cicadellidae+imm_homopteran)/sqrt(length(cercopidae+cicadellidae+imm_homopteran)),
                         # 
                         # cantharid=mean(cantharidae),
                         # cantharid.se=sd(cantharidae)/sqrt(length(cantharidae)),
                         # 
                         # cerambicid=mean(cerambicidae),
                         # cerambicid.se=sd(cerambicidae)/sqrt(length(cerambicidae)),
                         # 
                         # chrysomelid=mean(chrysomelidae),
                         # chrysomelid.se=sd(chrysomelidae)/sqrt(length(chrysomelidae)),
                         # 
                         # curculionid=mean(curculionidae),
                         # curculionid.se=sd(curculionidae)/sqrt(length(curculionidae)),
                         # 
                         # elaterid=mean(elateridae),
                         # elaterid.se=sd(elateridae)/sqrt(length(elateridae)),
                         # 
                         # scarabid=mean(scarabidae),
                         # scarabid.se=sd(scarabidae)/sqrt(length(scarabidae)),
                         # 
                         # orthopt=mean(orthoptera),
                         # orthopt.se=sd(orthoptera)/sqrt(length(orthoptera)),
                         # 
                         # lep_adults=mean(lep_adult),
                         # lep_adults.se=sd(lep_adult)/sqrt(length(lep_adult)),
                         # 
                         # lep_cats=mean(lep_caterpillar),
                         # lep_cats.se=sd(lep_caterpillar)/sqrt(length(lep_caterpillar)),
                         # 
                         # meloid=mean(meloidae),
                         # meloid.se=sd(meloidae)/sqrt(length(meloidae)),
                         # 

                         week=mean(week),
                         DOYphenol=mean(DOY))#average dates of all samples
  #returns some 'NaN' b/c there is only one sample
  summarized.data$exotic<- ifelse(summarized.data$species == 'LOCO6' | summarized.data$species == 'CESTM', 'y', 'n')
  }

#Order by site. (Needed to get desired faceting order)
{#Defines 'site' as factor whose order in df can determine chart order
  summarized.data$site<-factor(summarized.data$site, levels=unique(summarized.data$site))
  #Orders df as 1, 2, 3, (or NW, CRC, SW)
  summarized.data<-summarized.data[order(summarized.data$DOYphenol, 
                                         summarized.data$species),]}

# Subset Top 20 species from 'summarized.data'

{
library(magrittr)
library(dplyr)
s20 <-
  summarized.data %>%
  filter(site == "SWMREC") %>%
  arrange(-ne) %>%
  slice(1:20) %>%
  arrange(DOYphenol)

c20 <-
  summarized.data %>%
  filter(site == "CRC") %>%
  arrange(-ne) %>%
  slice(1:20) %>%
  arrange(DOYphenol)

n20 <-
  summarized.data %>%
  filter(site == "NWMHRC") %>%
  arrange(-ne) %>%
  slice(1:20) %>%
  arrange(DOYphenol)
}

 #Sig only Subsetting (OLD)
{ 
  # #SWMREC
  # SWsig<-summarized.data[which(summarized.data$site == 'SWMREC'),]
  # SWsig<-SWsig[which(SWsig$species == 'ACMI2' | SWsig$species == 'LOCO6' |
  #                      SWsig$species == 'ASSY' | SWsig$species == 'MOFI' |
  #                      SWsig$species == 'ASVE' | SWsig$species == 'OLRI' |
  #                      SWsig$species == 'COTR4' | SWsig$species == 'PYPI' |
  #                      SWsig$species == 'DAFR6' | SWsig$species == 'PYVI' |
  #                      SWsig$species == 'HEOC2' | SWsig$species == 'SOJU' |
  #                      SWsig$species == 'HEST' | SWsig$species == 'SONE'  |
  #                      SWsig$species == 'SOSP2' | SWsig$species == 'SYSE2' ),]
  # 
  # #CRC
  # CRCsig<-summarized.data[which(summarized.data$site == 'CRC'),]
  # CRCsig<-CRCsig[which(CRCsig$species == 'ACMI2' | CRCsig$species == 'MOFI' |
  #                        CRCsig$species == 'ASSY' | CRCsig$species == 'MOPU' |
  #                        CRCsig$species == 'ASTU' | CRCsig$species == 'OLRI' |
  #                        CRCsig$species == 'ASVE' | CRCsig$species == 'POSI2' |
  #                        CRCsig$species == 'CESTM' | CRCsig$species == 'PYVI' |
  #                        CRCsig$species == 'COTR4' | CRCsig$species == 'RAPI' |
  #                        CRCsig$species == 'DAFR6' | CRCsig$species == 'RUHI2' |
  #                        CRCsig$species == 'ECPU' | CRCsig$species == 'SIIN2' |
  #                        CRCsig$species == 'ERYU' | CRCsig$species == 'SITE' |
  #                        CRCsig$species == 'HEOC2' | CRCsig$species == 'SONE' |
  #                        CRCsig$species == 'HEST' | CRCsig$species == 'SOSP2' |
  #                        CRCsig$species == 'HYPR' | CRCsig$species == 'SYOO' |
  #                        CRCsig$species == 'LOCO6' | CRCsig$species == 'SYSE2' |
  #                        CRCsig$species == 'SOJU' ),]
  # 
  # #NWMHRC
  # NWsig<-summarized.data[which(summarized.data$site == 'NWMHRC'),]
  # NWsig<-NWsig[which(NWsig$species == 'ACMI2' | NWsig$species == 'LOCO6' |
  #                      NWsig$species == 'ASSY' | NWsig$species == 'LUPE3' |
  #                      NWsig$species == 'ASTU' | NWsig$species == 'MOFI' |
  #                      NWsig$species == 'ASVE' | NWsig$species == 'MOPU' |
  #                      NWsig$species == 'CARO2' | NWsig$species == 'OEFR' |
  #                      NWsig$species == 'CEAM' | NWsig$species == 'OLRI' |
  #                      NWsig$species == 'CESTM' | NWsig$species == 'PEDI' |
  #                      NWsig$species == 'COLA5' | NWsig$species == 'PYPI' |
  #                      NWsig$species == 'COPA10' | NWsig$species == 'PYVI' |
  #                      NWsig$species == 'COTR4' | NWsig$species == 'RAPI' |
  #                      NWsig$species == 'DAFR6' | NWsig$species == 'ROCA4' |
  #                      NWsig$species == 'DAPU5' | NWsig$species == 'SIIN2' |
  #                      NWsig$species == 'ECPU' | NWsig$species == 'SITE' |
  #                      NWsig$species == 'ERYU' | NWsig$species == 'SONE' |
  #                      NWsig$species == 'HEOC2' | NWsig$species == 'SOSP2' |
  #                      NWsig$species == 'HEST' | NWsig$species == 'SYOO' |
  #                      NWsig$species == 'HYPR' | NWsig$species == 'SYSE2' |
  #                      NWsig$species == 'LIAS' | NWsig$species == 'TROH' |
  #                      NWsig$species == 'LICY' | NWsig$species == 'VEST' ),]
}


#### Top 20 Bar - SWMREC ####
#theme 'bw' has gridlines, theme 'classic' does not
library(ggplot2)
sbar<-ggplot(data = s20, 
             aes(
               x = reorder(sci_name,DOYphenol), 
               y = ne))+
  geom_bar(stat = "identity", 
           colour = "black",
           fill = ifelse(s20$exotic  == "y",
                         "gray", 
                         "tomato2"),
           size = 0.15)+
  geom_errorbar(aes(
                ymin = ne-ne.se, 
                ymax = ne),
                width = .4,
                size = 0.15)+
  theme_classic(base_size = 8)+
  guides(fill = FALSE)+  
  labs(title = '2016',
       y = expression(Mean~natural~enemies~"/"~m^{2}~"-"~SE),
       x = "Plant species")+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text.x = element_text(face = "italic",
                                 angle = 65,
                                 hjust = 1,
                                 vjust = 1,
                                 colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        axis.line = element_line(size = 0.15))+
  ylim(-15,100) + # Pie chart space 2016:  CRC = (-15, 115)  NWMHRC = (-4,35)
  # ylim(-15,100) + # Pie chart space 2015: SW & CRC = (-20, 115)  NWMHRC = (-14, 80)
  # # Bloom Period Lines
  # geom_vline(xintercept=2.5, color='dark gray',size=0.5,linetype='dashed')+
  # geom_vline(xintercept=9.5, color='dark gray',size=0.5,linetype='dashed')+
  geom_point(data = s20, 
             size = 0.7, 
             aes(x = as.numeric(reorder(sci_name,DOYphenol)), 
                 y = mowed_ne, 
                 group = 1))
sbar

#save to pdf
pdf("test.pdf", height=3.3, width=6) # line widths and font size are optimized for this PDF size
sbar
dev.off()

#### Top 20 Bar - CRC ####
library(ggplot2)
cbar<-ggplot(data = c20, 
             aes(
               x = reorder(sci_name,DOYphenol), 
               y = ne))+
  geom_bar(stat = "identity", 
           fill = 'tomato2', 
           colour = "black", 
           size = 0.25)+
  geom_errorbar(aes(
    ymin = ne-ne.se, 
    ymax = ne),
    width = .4,
    size = 0.25)+
  theme_classic(base_size = 12)+
  guides(fill = FALSE)+  
  labs(title = '\nCRC 2015 - Top 20')+
  ylab("Mean natural enemies / m2 - SE\n")+
  xlab("Plant species")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        axis.line = element_line(size = 0.25))+
  ylim(-15,115) + # 2016
  # ylim(-15,100) + # 2015
  # # Bloom Period Lines
  # geom_vline(xintercept=2.5, color='dark gray',size=0.5,linetype='dashed')+
  # geom_vline(xintercept=9.5, color='dark gray',size=0.5,linetype='dashed')+
  geom_point(data = c20, 
             size = 1, 
             aes(x = as.numeric(reorder(sci_name,DOYphenol)), 
                 y = mowed_ne, 
                 group = 1))
cbar

#save to pdf
pdf("2015 NE Bar CRC top20.pdf", height=5, width=9) # line widths and font size are optimized for this PDF size
cbar
dev.off()


#### Top 20 Bar - NWMHRC ####
library(ggplot2)
nbar<-ggplot(data = n20, 
             aes(
               x = reorder(sci_name,DOYphenol), 
               y = ne))+
  geom_bar(stat = "identity", 
           fill = 'tomato2', 
           colour = "black", 
           size = 0.25)+
  geom_errorbar(aes(
    ymin = ne-ne.se, 
    ymax = ne),
    width = .4,
    size = 0.25)+
  theme_classic(base_size = 12)+
  guides(fill = FALSE)+  
  labs(title = '\nNWMHRC 2015 - Top 20')+
  ylab("Mean natural enemies / m2 - SE\n")+
  xlab("Plant species")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        axis.line = element_line(size = 0.25))+
  # ylim(-5,35) + # 2016
  ylim(-8,60) + # 2015
  
  # # Bloom Period Lines
  # geom_vline(xintercept=2.5, color='dark gray',size=0.5,linetype='dashed')+
  # geom_vline(xintercept=9.5, color='dark gray',size=0.5,linetype='dashed')+
  
  geom_point(data = n20, 
             size = 1, 
             aes(x = as.numeric(reorder(sci_name,DOYphenol)), 
                 y = mowed_ne, 
                 group = 1))
nbar

#save to pdf
pdf("2015 NE Bar NWMHRC top20.pdf", height=5, width=9) # line widths and font size are optimized for this PDF size
nbar
dev.off()

#### NE PLOTS: Full Site Plots ####
#Currently Optimized for 10 x 16 inch PDF
#theme 'bw' has gridlines, theme 'classic' does not
library(ggplot2) #data = subset(summarized.data, site == 'SWMREC') #SWsig
fullsite<-ggplot(data = SWsig, aes(x=reorder(scientific_name,DOYphenol), y=ne))+
  geom_bar(stat="identity", fill = 'tomato2', colour="black")+
  # geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne),
                width=.4,
                size=0.5)+
  labs(title='\nNatural enemies - SWMREC 2016 (Sign. only)')+
  theme_classic(base_size = 20)+
  guides(fill=FALSE)+
  ylab("Mean natural enemies / sample ± SE\n")+
  xlab("Plant species")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))+
  
  # ylim(-14,80) + # Pie chart space 2015: SW & CRC = (-20, 115)  NWMHRC = (-14, 80)
  ylim(-15,100) + # Pie chart space 2016:  CRC = (-15, 115)  NWMHRC = (-4,35)
  
  
  # # Bloom Period Lines
  # geom_vline(xintercept=2.5, color='dark gray',size=0.5,linetype='dashed')+
  # geom_vline(xintercept=9.5, color='dark gray',size=0.5,linetype='dashed')+
  
  # # Controls
  # geom_point(data=SWsig, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))
  geom_point(data = SWsig, size = 3, 
             aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))
fullsite

#save to pdf
pdf("2016 NE Bar SWMREC sig2.pdf", height=10, width=16) # line widths and font size are optimized for this PDF size
# pdf("2016 NE Bar CRC sig.pdf", height=10, width=16) # line widths and font size are optimized for this PDF size
# pdf("2016 NE Bar NWMHRC sig.pdf", height=10, width=16) # line widths and font size are optimized for this PDF size
fullsite
dev.off()

# Bloom period line  distances 2015:
# SWMREC full site    16.5    35.5 
# CRC    full site    11.5    26.5
# NWMHRC full site    11.5    27.5 
# SWMREC Sig only     2.5     7.5
# CRC    Sig Only     2.5     7.5
# NWMHRC Sig Only     2.5     9.5





# ----
#### Reshape data for NE Pie Charts. new dfs = '[s/c/n]20pie' ----
# ne vars = 'order' 'abundance'
{
  s20pie <-
    summarized.data %>%
    filter(site == "SWMREC") %>%
    arrange(-ne) %>%
    slice(1:20) %>%
    select(c(1,2,3,4,5,6,26,27,7,9,11,13,15,17)) %>%
    melt(id=c("site","species",'sitenum','scientific_name',
              'sci_name','common_name','week','DOYphenol')) %>%
    rename(order = `variable`) %>%
    rename(abundance = `value`)
  
  c20pie <-
    summarized.data %>%
    filter(site == "CRC") %>%
    arrange(-ne) %>%
    slice(1:20) %>%
    select(c(1,2,3,4,5,6,26,27,7,9,11,13,15,17)) %>%
    melt(id=c("site","species",'sitenum','scientific_name',
              'sci_name','common_name','week','DOYphenol')) %>%
    rename(order = `variable`) %>%
    rename(abundance = `value`)
  
  n20pie <-
    summarized.data %>%
    filter(site == "NWMHRC") %>%
    arrange(-ne) %>%
    slice(1:20) %>%
    select(c(1,2,3,4,5,6,26,27,7,9,11,13,15,17)) %>%
    melt(id=c("site","species",'sitenum','scientific_name',
              'sci_name','common_name','week','DOYphenol')) %>%
    rename(order = `variable`) %>%
    rename(abundance = `value`)
  
}

# Temporary fix for 2015
{
  s20pie <-
    summarized.data %>%
    filter(site == "SWMREC") %>%
    arrange(-ne) %>%
    slice(1:20) %>%
    select(c(1,2,3,4,6,8,10,12,14,23,24,25,26,27)) %>%
    melt(id=c("site","species",'sitenum','scientific_name',
              'sci_name','common_name','week','DOYphenol')) %>%
    rename(order = `variable`) %>%
    rename(abundance = `value`)
  
  c20pie <-
    summarized.data %>%
    filter(site == "CRC") %>%
    arrange(-ne) %>%
    slice(1:20) %>%
    select(c(1,2,3,4,6,8,10,12,14,23,24,25,26,27)) %>%
    melt(id=c("site","species",'sitenum','scientific_name',
              'sci_name','common_name','week','DOYphenol')) %>%
    rename(order = `variable`) %>%
    rename(abundance = `value`)
  
  n20pie <-
    summarized.data %>%
    filter(site == "NWMHRC") %>%
    arrange(-ne) %>%
    slice(1:20) %>%
    select(c(1,2,3,4,6,8,10,12,14,23,24,25,26,27)) %>%
    melt(id=c("site","species",'sitenum','scientific_name',
              'sci_name','common_name','week','DOYphenol')) %>%
    rename(order = `variable`) %>%
    rename(abundance = `value`)
}
#### Top 20 Pie - SWMREC  ####
library(RColorBrewer)
library(ggplot2)
spie<-ggplot(s20pie,
             aes(x = factor(1), 
                 y = abundance, 
                 fill = factor(order)))+
  theme_classic(base_size = 4)+
  scale_fill_brewer(type = 'qual', 
                    palette = 7, 
                    guide = 'legend') +
  geom_bar(stat = 'identity', 
           width = 1, 
           position = 'fill') + 
  scale_y_continuous(
    breaks = NULL,
    expand = c(0,0))+
  guides(fill = FALSE)+
  theme(
    title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    # strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(-.3, 'lines'),
    plot.background = element_blank(),
    plot.margin = unit(c(0,-0.03,0,-0.03), "inch") #T,R,B,L
  )+
  facet_wrap(~reorder(sci_name, DOYphenol), 
             ncol = 20)+
  coord_polar(theta = 'y')
spie

#save to pdf
pdf("2015 NE Pie SWMREC top 20.pdf", height=2, width=8.075)
spie
dev.off()




#### Top 20 Pie - CRC ####
library(RColorBrewer)
library(ggplot2)
cpie<-ggplot(c20pie,
             aes(x = factor(1), 
                 y = abundance, 
                 fill = factor(order)))+
  theme_classic(base_size = 4)+
  scale_fill_brewer(type = 'qual', 
                    palette = 7, 
                    guide = 'legend') +
  geom_bar(stat = 'identity', 
           width = 1, 
           position = 'fill') + 
  scale_y_continuous(
    breaks = NULL,
    expand = c(0,0))+
  guides(fill = FALSE)+
  theme(
    title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    # strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(-.3, 'lines'),
    plot.background = element_blank(),
    plot.margin = unit(c(0,-0.03,0,-0.03), "inch") #T,R,B,L
  )+
  facet_wrap(~reorder(sci_name, DOYphenol), 
             ncol = 20)+
  coord_polar(theta = 'y')
cpie

#save to pdf
pdf("2015 NE Pie CRC top 20.pdf", height=2, width=8.075)
cpie
dev.off()

#### Top 20 Pie - NWMHRC ####
library(RColorBrewer)
library(ggplot2)
npie<-ggplot(n20pie,
             aes(x = factor(1), 
                 y = abundance, 
                 fill = factor(order)))+
  theme_classic(base_size = 4)+
  scale_fill_brewer(type = 'qual', 
                    palette = 7, 
                    guide = 'legend') +
  geom_bar(stat = 'identity', 
           width = 1, 
           position = 'fill') + 
  scale_y_continuous(
    breaks = NULL,
    expand = c(0,0))+
  guides(fill = FALSE)+
  theme(
    title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    # strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(-.3, 'lines'),
    plot.background = element_blank(),
    plot.margin = unit(c(0,-0.03,0,-0.03), "inch") #T,R,B,L
  )+
  facet_wrap(~reorder(sci_name, DOYphenol), 
             ncol = 20)+
  coord_polar(theta = 'y')
npie

#save to pdf
pdf("2015 NE Pie NWMHRC top 20.pdf", height=2, width=8.075)
npie
dev.off()
#### NE PLOTS: Pie Charts  ####
#theme 'bw' has gridlines, theme 'classic' does not
library(RColorBrewer)
library(ggplot2)
pie<-ggplot(s20pie,
            # pie<-ggplot(data = subset(mtaxa.ne, site == 'NWMHRC'),
            aes(x=factor(1), 
                y=abundance, 
                fill=factor(order)))+
  theme_classic(base_size = 40)+
  scale_fill_brewer(type = 'qual', palette = 7, guide = 'legend') +
  geom_bar(stat = 'identity', width = 1, position = 'fill') + # Add colour = 'black' when faceting down to site*species
  # xlab(NULL)+
  # ylab(NULL)+
  scale_y_continuous(
    breaks=NULL,
    expand = c(0,0))+
  guides(fill=FALSE)+
  theme(title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        strip.text = element_blank(),
        axis.title = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, 'lines'))+
  scale_x_discrete(expand = c(0,0)) + #Removes plot margins
  facet_wrap(~scientific_name, ncol = 38)+
  coord_polar(theta = 'y')
pie

#save to pdf
# pdf("2016 NE Pie Legend.pdf", height=5, width=15) # single: 3x5 | 3 sites: 2x6 | all species: 11x8.5
pdf("2016 NE Pie SWMREC Sig line2.pdf", height=2, width=8) # single: 3x5 | 3 sites: 2x6 | all species: 11x8.5
# pdf("2016 NE Pie CRC Sig line.pdf", height=1, width=14.7) # single: 3x5 | 3 sites: 2x6 | all species: 11x8.5
# pdf("2016 NE Pie NWMHRC Sig line.pdf", height=1, width=14.85) # single: 3x5 | 3 sites: 2x6 | all species: 11x8.5
pie
dev.off()



#### Reshape data for HERB: Pie Charts. new df = 'mtaxa.herb' ----
# herb vars = 'order' 'abundance'
{#Extract columns
  library(plyr)
  taxa.herb <- subset(summarized.data, select = c(1,2,3,4,5,6,26,27,7,9,11,13,15,17))
  # DOY <- subset(summarized.data, select = c(1,2,23,24,25,26,27))
  
  #melt to long form, rename columns
  library("reshape")
  mtaxa.herb<-melt(taxa.herb, id=c("site","species",'sitenum','scientific_name',
                                   'sci_name','common_name','week','DOYphenol'))
  names(mtaxa.herb)[names(mtaxa.herb) == 'variable']<-'order'
  names(mtaxa.herb)[names(mtaxa.herb) == 'value']<-'abundance'
  
  
  #Defines 'site' as factor whose order in df can determine chart order
  mtaxa.herb$site<-factor(mtaxa.herb$site, levels=unique(mtaxa.herb$site))
  mtaxa.herb$sitenum<-as.character(mtaxa.herb$sitenum)
  #Orders df as 1, 2, 3, (or NW, CRC, SW)
  mtaxa.herb<-mtaxa.herb[order(mtaxa.herb$sitenum, 
                               mtaxa.herb$DOYphenol, 
                               mtaxa.herb$species),]
  
}


# ----
#### Reshape data for NE+HERB PLOTS----
# Split summarized.datA into ne and herb, then stack
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

# Subset of significant (T-TEST AGAINST MOWED) species from 'stack'
{ #SWMREC
  S.sig<-stack[which(stack$site == 'SWMREC'),]
  S.sig<-S.sig[which(S.sig$species == 'ACMI2' | S.sig$species == 'LOCO6' |
                       S.sig$species == 'ASSY' | S.sig$species == 'MOFI' |
                       S.sig$species == 'ASVE' | S.sig$species == 'OLRI' |
                       S.sig$species == 'COTR4' | S.sig$species == 'PYPI' |
                       S.sig$species == 'DAFR6' | S.sig$species == 'PYVI' |
                       S.sig$species == 'HEOC2' | S.sig$species == 'SOJU' |
                       S.sig$species == 'HEST' | S.sig$species == 'SONE'  |
                       S.sig$species == 'SOSP2' | S.sig$species == 'SYSE2' ),]
  
  #CRC
  C.sig<-stack[which(stack$site == 'CRC'),]
  C.sig<-C.sig[which(C.sig$species == 'ACMI2' | C.sig$species == 'MOFI' |
                       C.sig$species == 'ASSY' | C.sig$species == 'MOPU' |
                       C.sig$species == 'ASTU' | C.sig$species == 'OLRI' |
                       C.sig$species == 'ASVE' | C.sig$species == 'POSI2' |
                       C.sig$species == 'CESTM' | C.sig$species == 'PYVI' |
                       C.sig$species == 'COTR4' | C.sig$species == 'RAPI' |
                       C.sig$species == 'DAFR6' | C.sig$species == 'RUHI2' |
                       C.sig$species == 'ECPU' | C.sig$species == 'SIIN2' |
                       C.sig$species == 'ERYU' | C.sig$species == 'SITE' |
                       C.sig$species == 'HEOC2' | C.sig$species == 'SONE' |
                       C.sig$species == 'HEST' | C.sig$species == 'SOSP2' |
                       C.sig$species == 'HYPR' | C.sig$species == 'SYOO' |
                       C.sig$species == 'LOCO6' | C.sig$species == 'SYSE2' ),]
  
  #NWMHRC
  N.sig<-stack[which(stack$site == 'NWMHRC'),]
  N.sig<-N.sig[which(N.sig$species == 'ACMI2' | N.sig$species == 'LOCO6' |
                       N.sig$species == 'ASSY' | N.sig$species == 'LUPE3' |
                       N.sig$species == 'ASTU' | N.sig$species == 'MOFI' |
                       N.sig$species == 'ASVE' | N.sig$species == 'MOPU' |
                       N.sig$species == 'CARO2' | N.sig$species == 'OEFR' |
                       N.sig$species == 'CEAM' | N.sig$species == 'OLRI' |
                       N.sig$species == 'CESTM' | N.sig$species == 'PEDI' |
                       N.sig$species == 'COLA5' | N.sig$species == 'PYPI' |
                       N.sig$species == 'COPA10' | N.sig$species == 'PYVI' |
                       N.sig$species == 'COTR4' | N.sig$species == 'RAPI' |
                       N.sig$species == 'DAFR6' | N.sig$species == 'ROCA4' |
                       N.sig$species == 'DAPU5' | N.sig$species == 'SIIN2' |
                       N.sig$species == 'ECPU' | N.sig$species == 'SITE' |
                       N.sig$species == 'ERYU' | N.sig$species == 'SONE' |
                       N.sig$species == 'HEOC2' | N.sig$species == 'SOSP2' |
                       N.sig$species == 'HEST' | N.sig$species == 'SYOO' |
                       N.sig$species == 'HYPR' | N.sig$species == 'SYSE2' |
                       N.sig$species == 'LIAS' | N.sig$species == 'TROH' |
                       N.sig$species == 'LICY' | N.sig$species == 'VEST' ),]
}

#### NE+HERB PLOTS: up/down Full Site Plots  ####
#Currently Optimized for 10 x 16 inch PDF
#theme 'bw' has gridlines, theme 'classic' does not
library(ggplot2)
neherb<-ggplot(N.sig, aes(x=reorder(sci_name,DOYphenol), y=abundance, fill=class))+
  geom_bar(stat="identity",
           colour="black")+
  
  scale_fill_manual(values=c("lightgoldenrod3", "tomato2")) + #Colors to match pp headings
  geom_errorbar(aes(ymin=ne-ne.se, ymax=ne+ne.se),
                geom_errorbar(aes(ymin=ne-ne.se, ymax=ne),
                width=.4,
                size=0.5)+        
  # geom_errorbar(aes(ymin=-herb-herb.se, ymax=-herb+herb.se),
                geom_errorbar(aes(ymin=-herb, ymax=-herb+herb.se),
                width=.4,
                size=0.5)+
  
  theme_classic(base_size = 20)+
  guides(fill=FALSE)+
  labs(title='\nNatural enemies & herbivores - SWMREC 2016 (Sign. only)')+
  ylab("Mean arthropods / sample ± SE\n")+
  xlab("Plant species")+
  theme(axis.text.x=element_text(angle=65,
                                 hjust=1,
                                 colour = 'black'),
        axis.text.y=element_text(colour = 'black'))+
  # ylim(-120, 100) + # tO ACCOUNT FOR SYSE2 OUTLIER AT CRC full site
  # expand_limits(y=-55) + # Pie space. SWMREC = -120    CRC = -150     NWMHRC = -55
  
  # geom_vline(xintercept=16.5, color='dark gray',size=0.5,   linetype='dashed')+ 
  # geom_vline(xintercept=35.5, color='dark gray',size=0.5,   linetype='dashed')+ 
  
  geom_point(data=N.sig, size = 2, aes(x=as.numeric(reorder(sci_name,DOYphenol)), 
                                   y=mowed_ne, group=1))+
  geom_point(data=N.sig, shape = 17, size = 2, aes(x=as.numeric(reorder(sci_name,DOYphenol)), 
                                               y=-mowed_herb, group=1))
neherb

#save to pdf
pdf("2016 NE+Herb OpposingBar NWMHRC sig only.pdf", height=10, width=16) # line widths and font size are optimized for this PDF size
neherb
dev.off()

# Bloom period line  distances:
# SWMREC full site    16.5    35.5 
# CRC    full site    11.5    26.5
# NWMHRC full site    11.5    27.5 
# SWMREC Sig only     2.5     7.5
# CRC    Sig Only     2.5     7.5
# NWMHRC Sig Only     2.5     9.5





