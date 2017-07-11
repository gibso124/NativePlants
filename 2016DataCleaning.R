#Bring in primary data
#*******************
# Must set Working Directory First!!

# Import floral data -----
library(readr)
floral_raw <-
  read_csv(
    "floral2016__6_27_2017.csv",
    col_types = cols(
      block = col_factor(levels = c("1",
                                    "2", "3", "4")),
      coverage = col_double(),
      date_floral = col_date(format = "%m/%d/%Y"),
      flowers = col_double(),
      mortality = col_double(),
      site = col_factor(levels = c("SWMREC",
                                   "CRC", "NWMHRC")),
      species = col_factor(
        levels = c(
          "ACMI2",
          "AMCA6",
          "ASSY",
          "ASTU",
          "ASVE",
          "BAALM",
          "CARO2",
          "CEAM",
          "CESTM",
          "CHANA2",
          "COLA5",
          "CONTROL",
          "COPA10",
          "COTR4",
          "DAFR6",
          "DAPU5",
          "ECPU",
          "ERYU",
          "HEOC2",
          "HERI",
          "HEST",
          "HIGR3",
          "HYPR",
          "LECA8",
          "LEHI2",
          "LIAS",
          "LICY",
          "LOCO6",
          "MOFI",
          "MOPU",
          "MOWED",
          "OEBI",
          "OEFR",
          "OLRI",
          "PEDI",
          "PEHI",
          "POAR7",
          "POSI2",
          "PYPI",
          "PYVI",
          "RAPI",
          "RHCO",
          "RUHI2",
          "SEOB2",
          "SIIN2",
          "SITE",
          "SITE(2)",
          "SOJU",
          "SONE",
          "SOSP2",
          "SYOO",
          "SYSE2",
          "TROH",
          "VEST",
          "SILA3",
          "ROCA4",
          "LUPE3"
        )
      ),
      tallest_flower = col_double(),
      total_flowers = col_double(),
      units = col_double(),
      vac_time = col_double()
    )
  )
View(floral_raw)
floral<-floral_raw

floral_problems<-problems(floral_raw)


#----
# Import Vac data ----
library(readr)
vac_raw <-
  read_csv(
    "vac2016__6_28_2017.csv",
    col_types = cols(
      block = col_factor(levels = c("1",
                                    "2", "3", "4")),
      date_vac = col_date(format = "%m/%d/%Y"),
      site = col_factor(levels = c("SWMREC",
                                   "CRC", "NWMHRC")),
      species = col_factor(
        levels = c(
          "ACMI2",
          "AMCA6",
          "ASSY",
          "ASTU",
          "ASVE",
          "BAALM",
          "CARO2",
          "CEAM",
          "CESTM",
          "CHANA2",
          "COLA5",
          "CONTROL",
          "COPA10",
          "COTR4",
          "DAFR6",
          "DAPU5",
          "ECPU",
          "ERYU",
          "HEOC2",
          "HERI",
          "HEST",
          "HIGR3",
          "HYPR",
          "LECA8",
          "LEHI2",
          "LIAS",
          "LICY",
          "LOCO6",
          "MOFI",
          "MOPU",
          "MOWED",
          "OEBI",
          "OEFR",
          "OLRI",
          "PEDI",
          "PEHI",
          "POAR7",
          "POSI2",
          "PYPI",
          "PYVI",
          "RAPI",
          "RHCO",
          "RUHI2",
          "SEOB2",
          "SIIN2",
          "SITE",
          "SITE(2)",
          "SOJU",
          "SONE",
          "SOSP2",
          "SYOO",
          "SYSE2",
          "TROH",
          "VEST",
          "SILA3",
          "ROCA4",
          "LUPE3"
        )
      )
    )
  )
View(vac_raw)
vac<-vac_raw
vac_problems<-problems(vac_raw)


# Bring in floral area reference ----
{
  #Bring in floral area
  area<-read.csv("area.csv",header=TRUE)
  
  #Check on data quality
  summary(area)
  
  #Calculate new mean area in 'area'
  area$avgarea<-rowMeans(area[,4:8])
  
  #Remove excess variables from area
  area$f1<-NULL
  area$f2<-NULL
  area$f3<-NULL
  area$f4<-NULL
  area$f5<-NULL
  
  
  
  #merge into floral
  floral<-merge(floral,area,by=c('species'),all.x=TRUE)
  
  #calculate total floral area
  floral$totalarea<- floral$total_flowers * floral$avgarea
}

# Import names ----
# Bring in Names file.
names<-read.csv('names.csv',header=TRUE)
# Merge into floral
floral<-merge(floral,names,by=c('species'),all.x=TRUE)

# Import floral color reference ----
library(readr)
color <-
  read_csv(
    "floral_color_4_21.csv",
    col_types = cols(
      `LM (R - G)` = col_skip(),
      `MS (Y-B)` = col_skip(),
      color = col_factor(levels = c(
        "white",
        "yellow", "orange", "pink", "purple"
      )),
      notes = col_skip(),
      species = col_factor(
        levels = c(
          "ACMI2",
          "AMCA6",
          "ASSY",
          "ASTU",
          "ASVE",
          "BAALM",
          "CARO2",
          "CEAM",
          "CESTM",
          "CHANA2",
          "COLA5",
          "CONTROL",
          "COPA10",
          "COTR4",
          "DAFR6",
          "DAPU5",
          "ECPU",
          "ERYU",
          "HEOC2",
          "HERI",
          "HEST",
          "HIGR3",
          "HYPR",
          "LECA8",
          "LEHI2",
          "LIAS",
          "LICY",
          "LOCO6",
          "LUPE3",
          "MOFI",
          "MOPU",
          "MOWED",
          "OEBI",
          "OEFR",
          "OLRI",
          "PEDI",
          "PEHI",
          "POAR7",
          "POSI2",
          "PYPI",
          "PYVI",
          "RAPI",
          "RHCO",
          "ROCA4",
          "RUHI2",
          "SEOB2",
          "SIIN2",
          "SILA3",
          "SITE",
          "SOJU",
          "SONE",
          "SOSP2",
          "SYOO",
          "SYSE2",
          "TROH",
          "VEST"
        )
      )
    )
  )
View(color)

# Merge into floral
floral<-merge(floral,color,by=c('species'),all.x=TRUE)




##########################################################################-
# Import pollen reference ----
# Bring in pollen file.
pollen<-read.csv('pollen.csv',header=TRUE)
# Merge into floral
floral<-merge(floral,pollen,by=c('species'),all.x=TRUE)
#calculate total floral pollen
floral$totalpollen<- floral$total_flowers * floral$poll_per_flower

# Add Week and sample Identifier ----
library(lubridate)#isoweek() fcn starts on first day of year
floral$DOY<-yday(floral$date_floral) 
library(ISOweek)#ISOweek() fcn starts on first Sunday of year
floral$week<-ISOweek(floral$date_floral)
vac$week<-ISOweek(vac$date_vac)

# Remove text string in Week variable
library(stringr)
floral$week<-str_sub(floral$week, start= -2)
vac$week<-str_sub(vac$week, start= -2)

floral$sample_ident_floral<-paste(floral$week,floral$site,floral$species,floral$block,sep='_')
vac$sample_ident_vac<-paste(vac$week,vac$site,vac$species,vac$block,sep='_')

#Create numerical site code to order data in charts
floral$sitenum<-floral$site
floral$sitenum<-gsub('NWMHRC','1',floral$sitenum)
floral$sitenum<-gsub('CRC','2',floral$sitenum)
floral$sitenum<-gsub('SWMREC','3',floral$sitenum)
#Defines 'site' and 'sitenum' as factors whose order in df can determine chart order
floral$site<-factor(floral$site, levels=unique(floral$site))
floral$sitenum<-factor(floral$sitenum, levels=unique(floral$sitenum))

# Import Reference Matrix ----




#Bring in reference matrix
peakref<-read.csv("peak_reference_matrix_2016.csv")


#melt the matrix, rename columns
library("reshape")
mpeakref<-melt(peakref, id=c("site","week"))
names(mpeakref)[names(mpeakref) == 'variable']<-'species'
names(mpeakref)[names(mpeakref) == 'value']<-'peak'


#Merge mpeakref into floral.
floral<-merge(floral,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
floral.peak<-subset(floral, peak == "y")#total=1956
#Outgroup of subset by peakbloom from mpeakref
floral.notpeak<-subset(floral, peak == "n")#Total=604

#Merge mpeakref into vac. Subset to only peak bloom observations.

#Merge mpeakref into vac. 
vac<-merge(vac,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
vac.peak<-subset(vac, peak == "y")#total=1849
#Outgroup of subset by peakbloom from mpeakref
vac.notpeak<-subset(vac, peak == "n")#total=75




#Merge. 
all.data<-merge(vac.peak,floral.peak,by=c('site','week','species','block'),all.x=TRUE)
all.data2<-merge(floral.peak,vac.peak,by=c('site','week','species','block'),all.x=TRUE)



#send to CSV
write.csv(all.data, 'all.data2016.csv')

#Calculate Summary Variables ----
#Calculate total NEs in new variable
all.data$total_ne<-rowSums(all.data[,c("anthocoridae","nabidae","reduviidae",
                                       "cantharidae","carabidae","coccinellidae","chrysopidae",
                                       "hemerobiidae","sphecidae","tiphiidae",
                                       "vespidae","bombyliidae","syrphidae","tachinidae",
                                       "geocoridae", "aeolothripidae", "phlaeothripidae", 
                                       "ichneumonidae", "braconidae", "chalcidoidea", "cynipoidea",
                                       "aranae", "opiliones")])
# all.data$total_ne<-NULL

#### SUMMARIZING CONTROLS BY WEEK #### ----
##########################################################################-
## Remove observations without vac data pair (REMOVE ONCE DATA IS BETTER CLEANED)
# all.data<-subset(all.data, !is.na(date_vac))
# all.data2<-subset(all.data2, !is.na(date_floral))

#Remove controls from data
all.nocontrols<-all.data[which(all.data$species !="MOWED" & all.data$species !="CONTROL"),]

#Create data frames for controls only
controls<-subset(all.data, species == "MOWED" | species == "CONTROL")
#Concatenate week_site
controls$week_site<-paste(controls$week,controls$site,sep='_')
mowed<-subset(controls,species == "MOWED")
# weedy<-subset(controls,species == "CONTROL")

#Summarise controls
#Summarise by week and site using ddply
library("plyr")
# controls.summary<-ddply(controls, c('site', 'week','sitenum'),summarise,
#                     controls_ne=mean(total_ne),
#                     controls_herb=mean(total_herb),
#                     controls_all=mean(total_ne+total_herb))
mowed.summary<-ddply(mowed, c('site', 'week','sitenum'),summarise,
                     mowed_ne=mean(total_ne),
                     mowed_ne.se=sd(total_ne)/sqrt(length(total_ne)),
                     
                     mowed_herb=mean(total_herb),
                     mowed_herb.se=sd(total_herb)/sqrt(length(total_herb)),
                     
                     mowed_all=mean(total_ne+total_herb),
                     mowed_all.se=sd(total_ne+total_herb)/sqrt(length(total_ne)))
# weedy.summary<-ddply(weedy, c('site', 'week','sitenum'),summarise,
#                     weedy_ne=mean(total_ne),
#                     weedy_herb=mean(total_herb),
#                     weedy_all=mean(total_ne+total_herb))
# controls.summary<-merge(controls.summary,mowed.summary,by=c('site','sitenum','week'),all.x=TRUE)
# controls.summary<-merge(controls.summary,weedy.summary,by=c('site','sitenum','week'),all.x=TRUE)

#Order sites. 
#Defines 'site' as factor whose order in df can determine chart order
mowed.summary$site<-factor(mowed.summary$site, levels=unique(mowed.summary$site))
#Orders df as 1, 2, 3, (or NW, CRC, SW)
mowed.summary<-mowed.summary[order(mowed.summary$sitenum, 
                                   mowed.summary$week),]


#Merge control summary into all.nocontrols 
all.with.controls<-merge(all.nocontrols,mowed.summary,by=c('site','week','sitenum'),all.x=TRUE)


##########################################################################-






#### Standard Bar Plot ####
library(ggplot2)
barplot.1<-ggplot(SWMREC, aes(x=reorder(species,DOY), y=ne, fill=week))+
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
  # geom_point(data=SWMREC, aes(x=as.numeric(reorder(scientific_name,DOYphenol)), y=mowed_ne, group=1))+
  facet_wrap(~site,ncol=1, scales = 'free')#3 plots side by side (by site) in single object
barplot.1
