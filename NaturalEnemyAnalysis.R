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
#***Starting over. This section of code shows my path as I corrected mistakes.***

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
##########################################################################
#Bring in floral area
floral.area<-read.csv("floral_area.csv",header=TRUE)

#Check on data quality
summary(floral.area)

#Bring in PeakBloom Floral Data
floral.data<-read.csv("peakbloomdatawithcontrols_DG_20160315.csv",header=TRUE)

#Bring in Vacuum data
vac.data<-read.csv("VAc_Data_2015_JK_03142016.csv",header=TRUE)

#***Corrections and standardizations***

#Getting rid of the 'floral.area' mean area calculated in Excel
floral.area$mean_area<-NULL

#Calculate new mean area in 'floral.area'
floral.area$avgarea<-rowMeans(floral.area[,5:9])

#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days)
#extract month of year. Week is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
#Use Lubridate package to convert dates to week of year.
library(lubridate)
#format original dates in a new column in ISO format
floral.data$newdate<-mdy(floral.data$date)
floral.data$week<-week(floral.data$newdate)
vac.data$newdate<-mdy(vac.data$date)
vac.data$week<-week(vac.data$newdate)

#Control titles are insconsistent. vac.data -- 'MOWED' 'CONTROL' floral.data -- 'mowed' 'control'.  Fix to all CAPS.
#Fixed CAPS problem in floral.data in the 'species' column
floral.data$species1<-gsub('control','CONTROL',floral.data$species)
floral.data$species2<-gsub('mowed','MOWED',floral.data$species1)

#Rename 'species' to 'species2' in vac.data
vac.data$species2<-vac.data$species

#Concatenate week, site, block, species2 in 'floral.data' to create a single variable that uniquely identifies each sample.
#This column will then be created in the vac.data file so that the two files can be merged.
floral.data$sample_ident<-paste(floral.data$week,floral.data$site,floral.data$block,floral.data$species2,sep='')

#Concatenate week, site, block, species2 in 'vac.data'
vac.data$sample_ident<-paste(vac.data$week,vac.data$site,vac.data$block,vac.data$species2,sep='')

#***Merging Files****

#Merge floral.area into floral.data under new data frame 'floral.data.area'
floral.data.area<-merge(floral.data,floral.area,by="species",all.x=TRUE)

#Merge vac.data into floral.data.area to create new data frame "all.data"
all.data<-merge(floral.data.area,vac.data,by="sample_ident",all.x=TRUE)
summary(all.data)
