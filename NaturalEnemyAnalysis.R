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