floral.area<-read.csv("Floral_Area_20160302.csv",header=TRUE)
#Check on data quality

summary(floral.area)

#The CSV imported some blank columns. Getting rid of them.
floral.area$X<-NULL
floral.area$X.1<-NULL
summary(floral.area)
#AMCA6 is mislabeled as AMCA2 in this file.
floral.area$Plant<-gsub('AMCA2','AMCA6',floral.area$Plant)


#Bring in PeakBloom Floral Data

floral.data<-read.csv("PeakBloomdata.csv",header=TRUE)
summary(floral.data)

#Getting rid of extra columns

floral.data$X<-NULL
floral.data$X.1<-NULL
floral.data$X.2<-NULL
floral.data$X.3<-NULL
floral.data$X.4<-NULL
floral.data$X.5<-NULL
floral.data$X.6<-NULL
floral.data$X.7<-NULL
floral.data$X.8<-NULL
floral.data$X.9<-NULL
summary(floral.data)

#Get floral.area ready to merge with floral.data. 
#Calculate average area

floral.area$avgarea<-rowMeans(floral.area[,5:9])

#Change header from Plant to Species
names(floral.area)[names(floral.area)=='Plant']<-'Species'

#Merge floral.area into floral.data

floral.data<-merge(floral.data,floral.area,by="Species",all.x=TRUE)
summary(floral.data)

##########################################################################################
#***Starting over. This section of code shows my path as I corrected mistakes.***(has ERRORS, DO NOT USE)***

#Bring in floral area
floral.area<-read.csv("floral_area.csv",header=TRUE)

#Check on data quality
summary(floral.area)

#Bring in PeakBloom Floral Data
floral.data<-read.csv("peakbloomdatawithcontrols_DG_20160315.csv",header=TRUE)
summary(floral.data)

#Bring in Vacuum data
vac.data<-read.csv("VAc_Data_2015_JK_03142016.csv",header=TRUE)
summary(vac.data)

#Getting rid of the mean area calculated in Excel
floral.area$mean_area<-NULL

#Get floral.area ready to merge with floral.data. 
#Calculate average area
floral.area$avgarea<-rowMeans(floral.area[,5:9])

#Merge floral.area into floral.data
floral.data<-merge(floral.data,floral.area,by="species",all.x=TRUE)
summary(floral.data)

#Merge new floral.area with vac.data
floral.data<-merge(floral.data,floral.area,by="species",all.x=TRUE)
summary(floral.data)

#Concatenate date, site, block, species in floral.data to create a single variable that uniquely identifies each sample.
#This column will then be created in the vac.data file so that the two files can be merged.
floral.data$sample_ident<-paste(floral.data$date,floral.data$site.x,floral.data$block,floral.data$species,sep='')

#Concatenate date, site, block, species in vac.data
vac.data$sample_ident<-paste(vac.data$date,vac.data$site.x,vac.data$block,vac.data$species,sep='')

#I mislabeled site, so the new variable is wrong.  Delete concatenated variable and create new one.
vac.data$sample_ident<-NULL
vac.data$sample_ident<-paste(vac.data$date,vac.data$site,vac.data$block,vac.data$species,sep='')
summary(vac.data)

#Merge vac.data into floral.data to create new data frame "all.data"
all.data<-merge(floral.data,vac.data,by="sample_ident",all.x=TRUE)
summary(all.data)

#Problem: How do I identify the samples in floral.data that did not recieve paired data from vac.data?
#Almost half of the floral.data did not get paired.  Probably differences within sample_ident variable.
#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days, potential mistakes)
#One solution is to use week number instead of date in the concatenation. In Excel or R?
#Control titles are insconsistent. vac.data -- 'MOWED' 'CONTROL' floral.data -- 'mowed' 'control'.  Fix to all CAPS.

#Fixed CAPS problem in floral.data in the 'sample_ident' column but not in the species 'column'
floral.data$sample_ident_CAPS1<-gsub('control','CONTROL',floral.data$sample_ident)
floral.data$sample_ident_CAPS2<-gsub('mowed','MOWED',floral.data$sample_ident_CAPS1)

#Rename 'sample_ident' to 'sample_ident_CAPS2' in vac.data
vac.data$sample_ident_CAPS2<-vac.data$sample_ident

#Merge 'floral.data' and 'vac.data' again with corrected CAPS. New data frame 'all.data2'
all.data2<-merge(floral.data,vac.data,by="sample_ident_CAPS2",all.x=TRUE)
summary(all.data)

#Use Lubridate package to convert dates to week of year.
library(lubridate)
#format original dates in a new column in ISO format
floral.data$newdate<-mdy(floral.data$date)
vac.data$newdate<-mdy(vac.data$date)

#extract year (Reference only)
floral.data$year<-year(floral.data$newdate)
#extract month of year. Week is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
floral.data$week<-week(floral.data$newdate)

##########################################################################
# *USE THIS FOR NOW (7-8-2016)*
########################################################################## 
#Bring in floral area
floral.area<-read.csv("floral_area.csv",header=TRUE)

#Check on data quality
summary(floral.area)

#Bring in PeakBloom Floral Data
floral.data<-read.csv("peakbloomdatawithcontrols_DG_20160315.csv",header=TRUE)

#Bring in Vacuum data
vac.data<-read.csv("Vac_Data_2015_JK_03182016.csv",header=TRUE)

#***Corrections and standardizations***

#Getting rid of the 'floral.area' mean area calculated in Excel
floral.area$mean_area<-NULL

#Calculate new mean area in 'floral.area'
floral.area$avgarea<-rowMeans(floral.area[,5:9])

#Remove site from floral.area
floral.area$site<-NULL

#Sample variable in vac.data is reading as character, which doesn't wort properly.  
is.character(vac.data$sample)
is.factor(vac.data$sample) #data type is factor, but it sorts as if character
vac.data$sample<-gsub('--',9999,vac.data$sample)#9999 indicates no sample vial/number. Don't use quotes, b/c that means text string
vac.data$sample<-as.numeric(vac.data$sample)

#There are multiple cases in which there are two different samples with the same identifier. I cannot verify which is correct or
#what the identity of the other is, so I must remove the whole pair.
#this subsetting function also removes "NA" observations

#25SWMREC3CONTROL
#26SWMREC2COLA5
#30CRC2CONTROL
#37SWMREC4CONTROL
#38NWMHRC1HEST
#28_CRC_4_MOWED
vac.corrected<-vac.data[which(vac.data$sample !="127" & vac.data$sample !="105" &
                              vac.data$sample !="78" & vac.data$sample !="79" &
                              vac.data$sample !="768" & vac.data$sample !="773" &
                              vac.data$sample !="409" & vac.data$sample !="412" &
                              vac.data$sample !="1567" & vac.data$sample !="1569" &
                              vac.data$sample !="727" & vac.data$sample !="725"),]




#8/4/15 4 CESTM (32SWMREC4CESTM) was accidentally measured twice. Delete second measurement
floral.corrected<-floral.data[which(floral.data$new_record !="1139"),]
summary(floral.data)
summary(floral.corrected)

#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days)
#extract month of year. Week is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
#Use Lubridate package to convert dates to week of year.
library(lubridate)
#format original dates in a new column in ISO format.
floral.corrected$floraldate<-mdy(floral.corrected$date)
floral.corrected$date<-NULL
is.numeric(vac.corrected$sample)
vac.corrected$vacdate<-mdy(vac.corrected$date)

#Lubridate weeks start on the first day of the year. ISOweek week one starts on the first sunday of the year.
library(ISOweek)
floral.corrected$week<-isoweek(floral.corrected$floraldate)
vac.corrected$week<-isoweek(vac.corrected$vacdate)


#Control titles are insconsistent. vac.data -- 'MOWED' 'CONTROL' floral.data -- 'mowed' 'control'.  Fix to all CAPS.
#Fixed CAPS problem in floral.data in the 'species' column
floral.corrected$species<-gsub('control','CONTROL',floral.corrected$species)
floral.corrected$species<-gsub('mowed','MOWED',floral.corrected$species)

#Vac.data also inconsistent. Fix to all CAPS
vac.corrected$species<-gsub('control','CONTROL',vac.corrected$species)
vac.corrected$species<-gsub('mowed','MOWED',vac.corrected$species)

#Concatenate week, site, block, species in 'floral.data' to create a single variable that uniquely identifies each sample.
#This column will then be created in the vac.data file so that the two files can be merged.
floral.corrected$sample_ident<-paste(floral.corrected$week,floral.corrected$site,floral.corrected$species,floral.corrected$block,sep='_')

#Concatenate week, site, block, species in 'vac.data'
vac.corrected$sample_ident<-paste(vac.corrected$week,vac.corrected$site,vac.corrected$species,vac.corrected$block,sep='_')

#***Merging Files****

#Merge floral.area into floral.data under new data frame 'floral.data.area'
floral.data.area<-merge(floral.corrected,floral.area,by=c("species"),all.x=TRUE)

#Merge vac.data into floral.data.area to create new data frame "all.data"
all.data<-merge(floral.data.area,vac.corrected,by=c("sample_ident","site","block","species","week"),all.x=TRUE)
summary(all.data)

#Merge floral.data.area into vac.data to create new data frame "all.data.vacfirst"
all.data.vacfirst<-merge(vac.corrected,floral.data.area,by=c("sample_ident","site","block","species","week"),all.x=TRUE)
Summary(all.data.vacfirst)

#Export Data
write.csv(all.data, 'Merged_Data2.csv')


###################
###################

#Finding unmatched pairs in all.data
is.factor(all.data$sample)
as.factor(all.data$sample)

all.data.sample.NA2<-all.data[is.na(all.data$sample),]
write.csv(all.data.sample.NA2, 'all.data.sample.NA2.csv')


floral.test<-floral.corrected[which(floral.corrected$sample_ident =="22_SWMREC_1_MOWED"),]
vac.test<-vac.corrected[which(vac.corrected$sample_ident =="22_SWMREC_1_MOWED"),]

all.test<-merge(floral.test,vac.test,by=c("sample_ident","site","block","species2","week"),all.x=TRUE)
summary(all.test)

#######################################
#######################################
+
#Experimenting with alternative ways to extract peak bloom data.
floral.subset<-floral.corrected[which(floral.corrected$site =="SWMREC" & floral.corrected$species == "ACMI2" & floral.corrected$week == 25,26,27),]

floral.subset2<-subset(floral.corrected, (site == "SWMREC" & species == "MOWED") 
                       | (site == "SWMREC" & species == "CONTROL")
                       | (site == "SWMREC" & species == "ACMI2" & (week == 25 | week == 26 | week ==27))
                       | (site == "SWMREC" & species == "ACMI2" & (week == 25 | week == 26 | week ==27)))

floral.subset2<-subset(floral.corrected, (site == "SWMREC" & species == "MOWED"))







#####################################
#####################################
# For April 8 SARE Meeting. Merge scientific names and common names into vac data file.
####################################

#Bring in Vacuum data
vac.data<-read.csv("Vac_Data_April8.csv",header=TRUE)

# Bring in Names file.
names.data<-read.csv('Names.csv',header=TRUE)

# Bring in Phenology file.
phenol.data<-read.csv('Phenology.csv',header=TRUE)

#***Corrections and standardizations***

#There are multiple cases in which there are two different samples with the same identifier. I cannot verify which is correct or
#what the identity of the other is, so I must remove the whole pair.
#this subsetting function also removes "NA" observations

#25SWMREC3CONTROL
#26SWMREC2COLA5
#30CRC2CONTROL
#37SWMREC4CONTROL
#38NWMHRC1HEST
#28_CRC_4_MOWED
vac.corrected<-vac.data[which(vac.data$sample !="127" & vac.data$sample !="105" &
                                vac.data$sample !="78" & vac.data$sample !="79" &
                                vac.data$sample !="768" & vac.data$sample !="773" &
                                vac.data$sample !="409" & vac.data$sample !="412" &
                                vac.data$sample !="1567" & vac.data$sample !="1569" &
                                vac.data$sample !="727" & vac.data$sample !="725"),]


#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days)
#extract month of year. Week is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
#Use Lubridate package to convert dates to week of year.
library(lubridate)
#format original dates in a new column in ISO format.
vac.corrected$vacdate<-mdy(vac.corrected$date)
vac.data$date<-TRUE

#Lubridate weeks start on the first day of the year. ISOweek week one starts on the first sunday of the year.
library(ISOweek)
vac.corrected$week<-isoweek(vac.corrected$vacdate)

#Vac.data also inconsistent. Fix to all CAPS
vac.corrected$species<-gsub('control','CONTROL',vac.corrected$species)
vac.corrected$species<-gsub('mowed','MOWED',vac.corrected$species)


#Concatenate week, site, block, species2 in 'vac.data'
vac.corrected$sample_ident<-paste(vac.corrected$week,vac.corrected$site,vac.corrected$block,vac.corrected$species,sep='_')

#***Merging Files****

#Merge names.data into vac.data.area to create new data frame "all.data"
all.data<-merge(vac.corrected,names.data,by=c("species"),all.x=TRUE)
all.data<-merge(all.data,phenol.data,by=c('species'),all.x=TRUE)
summary(all.data)

#Export Data
write.csv(all.data, 'data_with_names.csv')


#******************
# Experimenting with Bipartite Graphs
#******************
ne<-read.csv("NE_Taxa_by_Plant.csv",header=TRUE,`rownames<-`(x,1))

library("GGally")
bip = network(ne,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")
bip
ggnet2(bip, label = TRUE)

#for the interaction matrix:
library("sna")
library("bipartite") #Plotweb info on pg 93 of Bipartite PDF

web<-plotweb(ne, method="normal", empty= TRUE) #export as 10" x 16" landscape PDF
web
PDI(ne, normalise=TRUE, log=FALSE)

#******************
# Using the Controls as normalizing factors for data
#******************


#Copied from Lampyrid code
# weather1<-ddply(weather, c("year", "week"), summarise,
#                 Tmax=max(air_temp_max_clean), Tmin=min(air_temp_min_clean), 
#                 dd.accum=max(dd.accum), prec.accum=max(prec.accum), 
#                 rain.days=sum(rain.days), prec.accum.0=max(prec.accum.0))

#Remove controls from vacuum data
vac.nocontrols<-vac.corrected[which(vac.corrected$species !="MOWED" & vac.corrected$species !="CONTROL"),]

#Create data frame for controls only
control.data<-subset(vac.corrected, species == "MOWED" | species == "CONTROL")

#Summarise controls
#Concatenate week, site
control.data$week_site<-paste(control.data$week,control.data$site,sep='_')
#summarise week_site using ddply
library("plyr")
control.summary<-ddply(control.data, c("site", 'week'),summarise,
                      arachnida=mean(arachnida),anthocoridae=mean(anthocoridae))


#******************
# extracting peak bloom observations using reference matrix
#******************

#Bring in reference matrix
peakref<-read.csv("peak_reference_matrix2015.csv")

#melt the matrix, rename columns
library("reshape")
mpeakref<-melt(peakref, id=c("site","week"))
names(mpeakref)[names(mpeakref) == 'variable']<-'species'
names(mpeakref)[names(mpeakref) == 'value']<-'peak'


#******************
# Bring in full dataset of floral data and apply same corrections as to PeakBloomFloral
#******************

#Bring in floral data (complete version)
floral<-read.csv("2015_floral_Data_20160302.csv",header=TRUE)

#8/4/15 4 CESTM (32SWMREC4CESTM) was accidentally measured twice. Delete second measurement
floral.clean<-floral[which(floral$record !="1139"),]
summary(floral)
summary(floral.clean)

#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days)
#extract month of year. Week is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
#Use Lubridate package to convert dates to week of year.
is.factor(floral.clean$date)

library(lubridate)
#format original dates in a new column in ISO format.
floral.clean$date<-mdy(floral.clean$date)

#Lubridate weeks start on the first day of the year. ISOweek week one starts on the first sunday of the year.
library(ISOweek)
floral.clean$week<-isoweek(floral.clean$date)


#Control titles are insconsistent. vac.data -- 'MOWED' 'CONTROL' floral.data -- 'mowed' 'control'.  Fix to all CAPS.
#Fixed CAPS problem in floral.data in the 'species' column
floral.clean$species<-gsub('control','CONTROL',floral.clean$species)
floral.clean$species<-gsub('mowed','MOWED',floral.clean$species)


#******************
# Merge mpeakref into other files
#******************

#Merge mpeakref into floral.clean.
floral.clean2<-merge(floral.clean,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
peak.floral.clean<-subset(floral.clean2, peak == "y")#total=1657
#Outgroup of subset by peakbloom from mpeakref
floral.notpeak<-subset(floral.clean2, peak == "n")#Total=720

########

#Merge mpeakref into vac.corrected. 
vac.corr2<-merge(vac.corrected,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
vac.peak<-subset(vac.corr2, peak == "y")#total=1595
#Outgroup of subset by peakbloom from mpeakref
vac.notpeak<-subset(vac.corr2, peak == "n")#total=45

##########

#Merge mpeakref into floral.corrected.
floral.corr2<-merge(floral.corrected,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
floral.corrpeak<-subset(floral.corr2, peak == "y")#total=1617
#Outgroup of subset by peakbloom from mpeakref
floral.corr.notpeak<-subset(floral.corr2, peak == "n")#total=29

#RESULTS: There is much disagreement between Logan's "PeakBloomData" and
#the peak bloom data set produced with the full data and refe matrix.
#There are both ommissions and insertions.
##########

#Merge vac.peak into peak.floral.clean
vac.into.floral<-merge(peak.floral.clean,vac.peak,by=c('site','week','species','block'),all.x=TRUE)

#merge peak.floral.clean into vac.peak
floral.into.vac<-merge(vac.peak,peak.floral.clean,by=c('site','week','species','block'),all.x=TRUE)


