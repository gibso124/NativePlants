##########################################################################-
#### BRING IN DATA #### ----
##########################################################################-

#Bring in floral area reference
{
#Bring in floral area
area<-read.csv("floral_area.csv",header=TRUE)

#Check on data quality
summary(area)

#Getting rid of the 'floral.area' mean area calculated in Excel
area$mean_area<-NULL

#Calculate new mean area in 'floral.area'
area$avgarea<-rowMeans(area[,5:9])

#Remove site from floral.area
area$site<-NULL
}

#Bring in primary data
#*******************

#Bring in floral data (complete version)
floral.RAW<-read.csv("2015_floral_Data_1.30.2017.csv",header=TRUE)
floral<-floral.RAW

#Bring in Vacuum data
vac.RAW<-read.csv("Vac_Data_2015_JK_03182016.csv",header=TRUE)
vac<-vac.RAW

##########################################################################-
#### CLEAN AND STANDARDIZE #### ----
##########################################################################-

#Sample variable in vac.data is reading as factor, which doesn't sort properly.  
#Change to numeric, use 9999 to indicate empty sample (no physical vial to number).
is.factor(vac$sample) #data type is factor
vac$sample<-gsub('--',9999,vac$sample) #Don't use quotes, b/c that means text string
vac$sample<-as.numeric(vac$sample)


#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days)
#extract month of year. Week is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
#Use Lubridate package to convert dates to week of year.
library(lubridate)
#format original dates in a new column in ISO format.
floral$date<-mdy(floral$date)
vac$date<-mdy(vac$date)
#specify origin of dates (useful after merging)
floral$floral_date<-floral$date
floral$date<-NULL
vac$vac_date<-vac$date
vac$date<-NULL
#Get day of year (1-365)
floral$DOY<-yday(floral$floral_date) 


#Lubridate weeks start on the first day of the year. 
#ISOweek week one starts on the first sunday of the year.
library(ISOweek)
floral$week<-isoweek(floral$floral_date)
vac$week<-isoweek(vac$vac_date)

#Create numerical site code to order data in charts
floral$sitenum<-floral$site
floral$sitenum<-gsub('NWMHRC','1',floral$sitenum)
floral$sitenum<-gsub('CRC','2',floral$sitenum)
floral$sitenum<-gsub('SWMREC','3',floral$sitenum)
#Defines 'site' as factor whose order in df can determine chart order
floral$site<-factor(floral$site, levels=unique(floral$site))

#Fixed CAPS inconsistency in the 'species' column.
#Changes column from factor to character, which causes problems.
#change it back with as.factor after each line.
floral$species<-gsub('control','CONTROL',floral$species)
as.factor(floral$species)
floral$species<-gsub('mowed','MOWED',floral$species)
as.factor(floral$species)
vac$species<-gsub('control','CONTROL',vac$species)
as.factor(vac$species)
vac$species<-gsub('mowed','MOWED',vac$species)
as.factor(vac$species)


#Concatenate week, site, block, species in both 'floral' and 'vac' to create a single variable that uniquely identifies each sample.

floral$sample_ident_floral<-paste(floral$week,floral$site,floral$species,floral$block,sep='_')
vac$sample_ident_vac<-paste(vac$week,vac$site,vac$species,vac$block,sep='_')


#There are multiple cases in which there are two different samples with the same identifier. I cannot verify which is correct or
#what the identity of the other is, so I must remove the whole pair.

#25SWMREC3CONTROL
#26SWMREC2COLA5
#30CRC2CONTROL
#37SWMREC4CONTROL
#38NWMHRC1HEST
#28_CRC_4_MOWED
vac<-vac[which(vac$sample !="127" & vac$sample !="105" &
                 vac$sample !="78" & vac$sample !="79" &
                 vac$sample !="768" & vac$sample !="773" &
                 vac$sample !="409" & vac$sample !="412" &
                 vac$sample !="1567" & vac$sample !="1569" &
                 vac$sample !="727" & vac$sample !="725"),]

#8/4/15 4 CESTM (32SWMREC4CESTM) was accidentally measured twice. Delete second measurement
floral<-floral[which(floral$record !="1139"),]


##########################################################################-
#### ADD PEAK BLOOM REFERENCE MATRIX ####
##########################################################################-

#Bring in reference matrix
peakref<-read.csv("peak_reference_matrix2015.csv")

#melt the matrix, rename columns
library("reshape")
mpeakref<-melt(peakref, id=c("site","week"))
names(mpeakref)[names(mpeakref) == 'variable']<-'species'
names(mpeakref)[names(mpeakref) == 'value']<-'peak'



#Merge mpeakref into floral. Subset to only peak bloom observations.

#Merge mpeakref into floral.
floral<-merge(floral,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
floral.peak<-subset(floral, peak == "y")#total=1673
#Outgroup of subset by peakbloom from mpeakref
floral.notpeak<-subset(floral, peak == "n")#Total=720

#Merge mpeakref into vac. Subset to only peak bloom observations.

#Merge mpeakref into vac. 
vac<-merge(vac,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
vac.peak<-subset(vac, peak == "y")#total=1595
#Outgroup of subset by peakbloom from mpeakref
vac.notpeak<-subset(vac, peak == "n")#total=45


#### MERGE VAC and FLORAL DATA FRAMES #### ---- 
#Failed merges useful for data cleaning

#Merge vac.peak into floral.peak
all.data<-merge(floral.peak,vac.peak,by=c('site','week','species','block'),all.x=TRUE)

#merge peak.floral.clean into vac.peak
all.data2<-merge(vac.peak,floral.peak,by=c('site','week','species','block'),all.x=TRUE)


#### CLEANING: Finding floral and vac merge problems #### ----

## Find lines that failed to merge ##
{
#subset NAs from all.data and all.data2 to for easier to deal with "error lists"
vac.missing<-subset(all.data, is.na(vac_date))
#Subset columns for simpler viewing
# library(dplyr)
# vac.missing<- select(vac.errors, record.x, floral_date, site, week, species, block, sample_ident_floral, sample_ident_vac)

floral.missing<-subset(all.data2, is.na(floral_date))
#Subset columns for simpler viewing
# library(dplyr)
# floral.missing<- select(floral.errors, record.x, vac_date, site, week, species, block, sample_ident_vac, sample_ident_floral)
}

## Identifying duplicates in VAC data ##
{
  #Find duplicates, identify in new column
  vac$duplicate<-duplicated(vac$sample_ident_vac)| duplicated(vac$sample_ident_vac, fromLast = TRUE)
  #create new file for entries where 'duplicate'=TRUE
  vac.duplicates<-vac[which(vac$duplicate==TRUE),]
}

## Identifying duplicates in FLORAL data ##
{
#Find duplicates, identify in new column
  #5 pairs of duplicates: 2 peak, 3 non-peak
  floral$duplicate<-duplicated(floral$sample_ident_floral)| duplicated(floral$sample_ident_floral, fromLast = TRUE)
  #create new file for entries where 'duplicate'=TRUE
  floral.duplicates<-floral[which(floral$duplicate==TRUE),]
}

## Save the evidence as CSV for more convenient sleuthing ##
{#Export Data
write.csv(vac.missing, 'vac.missing.csv')
write.csv(floral.missing, 'floral.missing.csv')
write.csv(vac.duplicates, 'vac.duplicates.csv')
write.csv(floral.duplicates, 'floral.duplicates.csv')
}

##########################################################################-
#### CREATE NE/HERBIVORE SUM VARIABLES #### ----
##########################################################################-
#A single NA each in the Bombyliidae, herb_mridiae, and curculionidae prevented NE variable from properly calculating
all.data$bombyliidae<-replace(all.data$bombyliidae,which(is.na(all.data$bombyliidae)),0)
all.data$curculionidae<-replace(all.data$curculionidae,which(is.na(all.data$curculionidae)),0)
all.data$herb_miridae<-replace(all.data$herb_miridae,which(is.na(all.data$herb_miridae)),0)


#Calculate total NEs in new variable
all.data$total_ne<-rowSums(all.data[,c("arachnida","anthocoridae","nabidae","reduviidae",
                                       "cantharidae","carabidae","coccinellidae","chrysopidae",
                                       "hemerobiidae","parasitoid_wasps","sphecidae","tiphiidae",
                                       "vespidae","bombyliidae","syrphidae","tachinidae")])

#Calculate total herbivores in new variable
all.data$total_herb<-rowSums(all.data[,c("aphidae","cercopidae","cicadellidae","herb_miridae",
                                         "pentatomidae","tingidae","imm_homopteran","cerambicidae",
                                         "chrysomelidae","curculionidae","elateridae","scarabidae",
                                         "orthoptera","lep_adult","lep_caterpillar")])

#Calculate total NE Diptera in new variable
all.data$diptera_ne<-rowSums(all.data[,c("bombyliidae","syrphidae","tachinidae")])

#Calculate total NE Lacewings in new variable
all.data$neuroptera_ne<-rowSums(all.data[,c("chrysopidae","hemerobiidae")])

#Calculate total NE Coleoptera in new variable
all.data$coleoptera_ne<-rowSums(all.data[,c("cantharidae","carabidae","coccinellidae")])

#Calculate total NE Hymenoptera in new variable
all.data$hymenoptera_ne<-rowSums(all.data[,c("parasitoid_wasps","sphecidae","tiphiidae",
                                       "vespidae")])

#Calculate total NE Hemiptera in new variable
all.data$hemiptera_ne<-rowSums(all.data[,c("anthocoridae","nabidae","reduviidae")])



##########################################################################-
#### SUMMARIZING CONTROLS BY WEEK #### ----
##########################################################################-
#Remove observations without vac data pair (REMOVE ONCE DATA IS BETTER CLEANED)
all.data<-subset(all.data, !is.na(vac_date))
# all.data2<-subset(all.data2, !is.na(floral_date))

#Remove controls from data
all.nocontrols<-all.data[which(all.data$species !="MOWED" & all.data$species !="CONTROL"),]

#Create data frames for controls only
controls<-subset(all.data, species == "MOWED" | species == "CONTROL")
#Concatenate week_site
controls$week_site<-paste(controls$week,controls$site,sep='_')
mowed<-subset(controls,species == "MOWED")
weedy<-subset(controls,species == "CONTROL")

#Summarise controls
#Summarise by week and site using ddply
library("plyr")
controls.summary<-ddply(controls, c('site', 'week','sitenum'),summarise,
                    controls_ne=mean(total_ne),
                    controls_herb=mean(total_herb),
                    controls_all=mean(total_ne+total_herb))
mowed.summary<-ddply(mowed, c('site', 'week','sitenum'),summarise,
                    mowed_ne=mean(total_ne),
                    mowed_ne.se=sd(total_ne)/sqrt(length(total_ne)),
                    
                    mowed_herb=mean(total_herb),
                    mowed_herb.se=sd(total_herb)/sqrt(length(total_herb)),
                    
                    mowed_all=mean(total_ne+total_herb),
                    mowed_all.se=sd(total_ne+total_herb)/sqrt(length(total_ne)))
weedy.summary<-ddply(weedy, c('site', 'week','sitenum'),summarise,
                    weedy_ne=mean(total_ne),
                    weedy_herb=mean(total_herb),
                    weedy_all=mean(total_ne+total_herb))
controls.summary<-merge(controls.summary,mowed.summary,by=c('site','sitenum','week'),all.x=TRUE)
controls.summary<-merge(controls.summary,weedy.summary,by=c('site','sitenum','week'),all.x=TRUE)

#Order sites. 
#Defines 'site' as factor whose order in df can determine chart order
controls.summary$site<-factor(controls.summary$site, levels=unique(controls.summary$site))
#Orders df as 1, 2, 3, (or NW, CRC, SW)
controls.summary<-controls.summary[order(controls.summary$sitenum, 
                                         controls.summary$week),]


#Merge control summary into all.nocontrols 
all.with.controls<-merge(all.nocontrols,controls.summary,by=c('site','week','sitenum'),all.x=TRUE)


##########################################################################-




#### Miscellaneous Reference code #### ----
##########################################################################-
#Export Data
write.csv(df, 'title.csv')

#Finding unmatched pairs in all.data
is.factor(all.data$sample)
as.factor(all.data$sample)

all.data.sample.NA2<-all.data[is.na(all.data$sample),]
write.csv(all.data.sample.NA2, 'all.data.sample.NA2.csv')

#Merge scientific names and common names into vac data file.
#Bring in Vacuum data
vac.data<-read.csv("Vac_Data_April8.csv",header=TRUE)

# Bring in Names file.
names.data<-read.csv('Names.csv',header=TRUE)

# Bring in Phenology file.
phenol.data<-read.csv('Phenology.csv',header=TRUE)


