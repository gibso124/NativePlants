##########################################################################
# BRING IN DATA
########################################################################## 

#*******************
#Floral area reference
#*******************

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

#*******************
#Bring in primary data
#*******************

#Bring in floral data (complete version)
floral.RAW<-read.csv("2015_floral_Data_20160302.csv",header=TRUE)
floral<-floral.RAW

#Bring in Vacuum data
vac.RAW<-read.csv("Vac_Data_2015_JK_03182016.csv",header=TRUE)
vac<-vac.RAW

##########################################################################
# CLEAN AND STANDARDIZE
########################################################################## 

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

#Lubridate weeks start on the first day of the year. ISOweek week one starts on the first sunday of the year.
library(ISOweek)
floral$week<-isoweek(floral$date)
vac$week<-isoweek(vac$date)


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

floral$sample_ident<-paste(floral$week,floral$site,floral$species,floral$block,sep='_')
vac$sample_ident<-paste(vac$week,vac$site,vac$species,vac$block,sep='_')


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


##########################################################################
# ADD PEAK BLOOM REFERENCE MATRIX
########################################################################## 

#Bring in reference matrix
peakref<-read.csv("peak_reference_matrix2015.csv")

#melt the matrix, rename columns
library("reshape")
mpeakref<-melt(peakref, id=c("site","week"))
names(mpeakref)[names(mpeakref) == 'variable']<-'species'
names(mpeakref)[names(mpeakref) == 'value']<-'peak'


#*******************
#Merge mpeakref into floral and vac files. Subset to only peak bloom observations.
#*******************

#Merge mpeakref into floral.
floral<-merge(floral,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
floral.peak<-subset(floral, peak == "y")#total=1657
#Outgroup of subset by peakbloom from mpeakref
floral.notpeak<-subset(floral, peak == "n")#Total=720

#*******************

#Merge mpeakref into vac. 
vac<-merge(vac,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
vac.peak<-subset(vac, peak == "y")#total=1595
#Outgroup of subset by peakbloom from mpeakref
vac.notpeak<-subset(vac, peak == "n")#total=45

#*******************
#Merge peak vac and floral files. Failed merges useful for data cleaning
#*******************

#Merge vac.peak into floral.peak
all.data<-merge(floral.peak,vac.peak,by=c('site','week','species','block'),all.x=TRUE)

#merge peak.floral.clean into vac.peak
floral.into.vac<-merge(vac.peak,floral.peak,by=c('site','week','species','block'),all.x=TRUE)



##########################################################################
# CREATE NE/HERBIVORE SUM VARIABLES
##########################################################################
#A single NA each in the Bombyliidae, herb_mridiae, and curculionidae prevented NE variable from properly calculating
all.data$bombyliidae<-replace(all.data$bombyliidae,which(is.na(all.data$bombyliidae)),0)
all.data$curculionidae<-replace(all.data$curculionidae,which(is.na(all.data$curculionidae)),0)
all.data$herb_miridae<-replace(all.data$herb_miridae,which(is.na(all.data$herb_miridae)),0)


#Calculate total NEs in new variable
all.data$ne_total<-rowSums(all.data[,c("arachnida","anthocoridae","nabidae","reduviidae",
                                       "cantharidae","carabidae","coccinellidae","chrysopidae",
                                       "hemerobiidae","parasitoid_wasps","sphecidae","tiphiidae",
                                       "vespidae","bombyliidae","syrphidae","tachinidae")])

#Calculate total herbivores in new variable
all.data$herb_total<-rowSums(all.data[,c("aphidae","cercopidae","cicadellidae","herb_miridae",
                                         "pentatomidae","tingidae","imm_homopteran","cerambicidae",
                                         "chrysomelidae","curculionidae","elateridae","scarabidae",
                                         "orthoptera","lep_adult","lep_caterpillar")])

##########################################################################
# USING CONTROLS AS NORMALIZING FACTORS FOR DATA
########################################################################## 
#Remove observations without vac data pair (REMOVE ONCE DATA IS BETTER CLEANED)
all.data<-subset(all.data, !is.na(sample))

#Remove controls from data
all.nocontrols<-all.data[which(all.data$species !="MOWED" & all.data$species !="CONTROL"),]

#Create data frame for controls only
controls<-subset(all.data, species == "MOWED" | species == "CONTROL")

#Summarise controls
#Concatenate week, site
controls$week_site<-paste(controls$week,controls$site,sep='_')
#summarise week_site using ddply
library("plyr")
controls.summary<-ddply(controls, c("site", 'week'),summarise,
                       control_ne=mean(ne_total),control_herb=mean(herb_total))

#Merge control summary into all.nocontrols 
all.with.controls<-merge(all.nocontrols,controls.summary,by=c('site','week'),all.x=TRUE)







##########################################################################

##########################################################################
# Miscellaneous Reference code
########################################################################## 
#Export Data
write.csv(all.data, 'Merged_Data2.csv')

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


