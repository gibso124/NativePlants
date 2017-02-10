##########################################################################-
#### BRING IN DATA #### ----
##########################################################################-

# #Bring in floral area reference
# {
# #Bring in floral area
# area<-read.csv("floral_area.csv",header=TRUE)
# 
# #Check on data quality
# summary(area)
# 
# #Getting rid of the 'floral.area' mean area calculated in Excel
# area$mean_area<-NULL
# 
# #Calculate new mean area in 'floral.area'
# area$avgarea<-rowMeans(area[,5:9])
# 
# #Remove site from floral.area
# area$site<-NULL
# }

#Bring in primary data
#*******************

#Bring in floral data (complete version)
# floral.RAW<-read.csv("2015_floral_Data_1.30.2017.csv",header=TRUE)
# floral.RAW<-read.csv("2015_floral_Data_1.30.2017_Refined.csv",header=TRUE)
floral.RAW<-read.csv("2015_floral_Data_1.30.2017_Refined2.csv",header=TRUE)

floral<-floral.RAW

#Bring in Vacuum data
# vac.RAW<-read.csv("Vac_Data_2015_JK_03182016.csv",header=TRUE)
# vac.RAW<-read.csv("Vac_Data_2015_DG_2-7-2017 Refined.csv",header=TRUE)
vac.RAW<-read.csv("Vac_Data_2015_DG_2-7-2017 Refined2.csv",header=TRUE)

vac<-vac.RAW

##########################################################################-
#### CLEAN AND STANDARDIZE #### ----
##########################################################################-

#Sample variable in vac.data is reading as factor, which doesn't sort properly.  
is.numeric(vac$sample) #data type is numeric, so it should sort now


#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days)
#extract month of year. Week is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
#Use Lubridate package to convert dates to week of year.
library(lubridate)
# #format original dates in a new column in ISO format.
# floral$date<-mdy(floral$date)
# vac$date_vac<-mdy(vac$date_vac)
# #specify origin of dates (useful after merging)
# floral$floral_date<-floral$date
# floral$date<-NULL
#Get day of year (1-365)
floral$DOY<-yday(floral$date_floral) 


#Lubridate weeks start on the first day of the year. 
#ISOweek week one starts on the first sunday of the year.
library(ISOweek)
floral$week<-isoweek(floral$date_floral)
vac$week<-isoweek(vac$date_vac)

#Create numerical site code to order data in charts
floral$sitenum<-floral$site
floral$sitenum<-gsub('NWMHRC','1',floral$sitenum)
floral$sitenum<-gsub('CRC','2',floral$sitenum)
floral$sitenum<-gsub('SWMREC','3',floral$sitenum)
#Defines 'site' as factor whose order in df can determine chart order
floral$site<-factor(floral$site, levels=unique(floral$site))

# #Fixed CAPS inconsistency in the 'species' column.
# #Changes column from factor to character, which causes problems.
# #change it back with as.factor after each line.
# floral$species<-gsub('control','CONTROL',floral$species)
# as.factor(floral$species)
# floral$species<-gsub('mowed','MOWED',floral$species)
# as.factor(floral$species)



#Concatenate week, site, block, species in both 'floral' and 'vac' to create a single variable that uniquely identifies each sample.

floral$sample_ident_floral<-paste(floral$week,floral$site,floral$species,floral$block,sep='_')
vac$sample_ident_vac<-paste(vac$week,vac$site,vac$species,vac$block,sep='_')


#There are multiple cases in which there are two different samples with the same identifier. I cannot verify which is correct or
#what the identity of the other is, so I must remove the whole pair.

#25SWMREC3CONTROL
#26SWMREC2COLA5
#30CRC2CONTROL
#37SWMREC4CONTROL
#28_CRC_4_MOWED
vac<-vac[which(vac$sample !="127" & vac$sample !="105" &
                 vac$sample !="78" & vac$sample !="79" &
                 vac$sample !="768" & vac$sample !="773" &
                 vac$sample !="409" & vac$sample !="412" &
                 vac$sample !="727" & vac$sample !="725"),]

#7/3/15 4 CARO2 (27NWMHRC4CARO2) has two vac samples (#1175, #1673). 
#The second (#1673) is correct. Delete #1175.
#7/30/15 3 LICY (31SWMREC3LICY, #326) has no floral measurements. Delete it.
vac<-vac[which(vac$sample !="1175" & vac$sample !="326"),]

###### WARNING: PEHI and COLA5 samples from CRC 6-23-2015 have floral sample lines with vac times,
###### but no floral measurements were taken.  They will need to be removed when analyzing vac data
###### with plant traits. Easiest way is through changing 'peakref'. 
###### Change CRC week 26:  'y' --->> 'n'

###### NOTE: If I ever want to compute a tally about floral data independent
#### of vac data, I will want to NOT remove the floral samples below.

####The following floral observations match the 'vac' data duplicates removed above.
{
  # record_floral,	sample_ident_floral
  # 517	  28_CRC_CONTROL_4
  # 518	  28_CRC_MOWED_4
  # 881	  30_CRC_CONTROL_2
  # 880	  30_CRC_MOWED_2
  # 252	  25_SWMREC_CONTROL_3
  # 267	  25_SWMREC_MOWED_3
  # 325	  26_SWMREC_COLA5_2
  # 2142	37_SWMREC_CONTROL_4
  # 2141	37_SWMREC_MOWED_4
}
#Remove (9 lines)
{
  floral<-floral[which(floral$record !="517" & floral$record !="267" & 
                         floral$record !="518" & floral$record !="325" & 
                         floral$record !="881" & floral$record !="2142" & 
                         floral$record !="880" & floral$record !="2141" & 
                         floral$record !="252"),]
}

####The following floral observations have duplicate lines.
{
# record_floral,	sample_ident_floral
# 161   24_NWMHRC_POSI2_1
# 962.1 31_SWMREC_HEST_2
# 1139  32_SWMREC_CESTM_4 ---> line is labeled as block 2, which is wrong
# 1736  34_NWMHRC_HYPR_1
}
#Remove extra lines (4 lines)
{
floral<-floral[which(floral$record !="161" & floral$record !="1736" & 
                       floral$record !="962.1" & floral$record !="1139"),]
}

####The following floral observations were never vac sampled
{
# record_floral,	sample_ident_floral
#   31  	23_CRC_HERI_2
#   42  	23_CRC_POSI2_3
#   33  	23_CRC_POSI2_4
#   43  	23_CRC_SEOB2_4
#   102 	24_CRC_HERI_1
#   90	  24_CRC_SEOB2_1
#   98	  24_CRC_POSI2_3
#   95  	24_CRC_COLA5_4
#   96  	24_CRC_POSI2_4
#   189 	25_CRC_HERI_1
#   864 	30_CRC_OEFR_3
#   156	  24_NWMHRC_SEOB2_2
#   736 	29_NWMHRC_CARO2_3
#   1956	35_NWMHRC_ASVE_2
#   1921	35_NWMHRC_CONTROL_2
#   1922	35_NWMHRC_MOWED_2
#   1955	35_NWMHRC_OLRI_2
#   1953	35_NWMHRC_PYPI_2
#   15	  22_SWMREC_MOWED_2
#   48	  23_SWMREC_SEOB2_1
#   55	  23_SWMREC_PEHI_2
#   65	  23_SWMREC_POSI2_4
#   70	  23_SWMREC_SEOB2_4
#   200	  25_SWMREC_ROCA4_1
#   949	  31_SWMREC_SONE_1
#   979 	31_SWMREC_SONE_3
 }
#Remove (26 lines)
{
  floral<-floral[which(floral$record !="31" & floral$record !="1956" & 
                         floral$record !="42" & floral$record !="1921" & 
                         floral$record !="33" & floral$record !="1922" & 
                         floral$record !="43" & floral$record !="1955" & 
                         floral$record !="102" & floral$record !="1953" & 
                         floral$record !="90" & floral$record !="15" & 
                         floral$record !="98" & floral$record !="48" & 
                         floral$record !="95" & floral$record !="55" & 
                         floral$record !="96" & floral$record !="65" & 
                         floral$record !="189" & floral$record !="70" & 
                         floral$record !="864" & floral$record !="200" & 
                         floral$record !="156" & floral$record !="949" & 
                         floral$record !="979" & floral$record !="736"),]
}

####The following floral observations are missing vac samples, but the cause is still unknown
#as of 2.8.2017. **Not removing these yet**, but keeping here for reference.
{
  # record_floral,	sample_ident_floral
  # 89  	24_CRC_HERI_4
  # 188 	25_CRC_HERI_2
  # 1856	35_CRC_POAR7_2
  # 155	  24_NWMHRC_LUPE3_1
  # 580	  28_NWMHRC_ROCA4_1
  # 592	  28_NWMHRC_HERI_2
  # 1213	31_NWMHRC_VEST_2
  # 2225	38_SWMREC_SOSP2_4
  
} # (8 lines)


##########################################################################-
#### ADD PEAK BLOOM REFERENCE MATRIX ####
##########################################################################-

#Bring in reference matrix
peakref.old<-read.csv("peak_reference_matrix2015.csv")
peakref<-read.csv("peak_reference_matrix2015_updated_2.9.2017.csv")

#melt the matrix, rename columns
library("reshape")
mpeakref<-melt(peakref, id=c("site","week"))
names(mpeakref)[names(mpeakref) == 'variable']<-'species'
names(mpeakref)[names(mpeakref) == 'value']<-'peak'



#Merge mpeakref into floral. Subset to only peak bloom observations.

#Merge mpeakref into floral.
floral<-merge(floral,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
floral.peak<-subset(floral, peak == "y")#total=1673 , now 1642
#Outgroup of subset by peakbloom from mpeakref
floral.notpeak<-subset(floral, peak == "n")#Total=720 , now 729

#Merge mpeakref into vac. Subset to only peak bloom observations.

#Merge mpeakref into vac. 
vac<-merge(vac,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
vac.peak<-subset(vac, peak == "y")#total=1595 , now 1634
#Outgroup of subset by peakbloom from mpeakref
vac.notpeak<-subset(vac, peak == "n")#total=45 , now 75


#### MERGE VAC and FLORAL DATA FRAMES #### ---- 
### Merges working properly as of 2-8-2017. 
# floral<--vac == 8 lines unmatched (same as known list of missing), no duplicates
# vac<--floral == 0 unmatched or duplicates

#merge peak.floral.clean into vac.peak
all.data<-merge(vac.peak,floral.peak,by=c('site','week','species','block'),all.x=TRUE)

#Merge vac.peak into floral.peak
all.data2<-merge(floral.peak,vac.peak,by=c('site','week','species','block'),all.x=TRUE)




#### CLEANING: Finding floral and vac merge problems #### ----

## Find lines that failed to merge ##
{
#subset NAs from all.data and all.data2 to for easier to deal with "error lists"
floral.missing<-subset(all.data, is.na(date_vac))
vac.missing<-subset(all.data2, is.na(date_floral))
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
write.csv(vac.missing, 'vac.missing.2.7.2017.csv')
write.csv(floral.missing, 'floral.missing.2.7.2017.csv')
write.csv(vac.duplicates, 'vac.duplicates.2.7.2017.csv')
write.csv(floral.duplicates, 'floral.duplicates.2.7.2017.csv')
write.csv(vac,'cleaned vac data.2.7.2017.csv')
write.csv(floral,'cleaned floral data.2.7.2017.csv')
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




#### Bulk Package Loading #### ----

## The warning "the following objects are masked" means that two packages loaded have
## identical commands, and R will use the command in the package loaded LAST.
## To specify which package for R to use, use the double colon operator(::)
#     package::function
#     plyr::summarise
## simply adding 'library(package)' above the current command DOES NOT fix this!
## To see the order of package loading, type 'search()'

library(lubridate)
library(ISOweek)
library(reshape)
library(plyr)

#Charts Code Packages
library(ggplot2)
library(plyr)
library(reshape)
library(viridis)
library(dplyr) #for the 'rbind()' command. 'rank()' is base R.

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


