##########################################################################-
#### BRING IN DATA #### ----
##########################################################################-

#Bring in primary data
#*******************


# Import floral data -----
library(readr)
floral_raw <-
  read_csv(
    "2015_floral_Data_5_2_2017_Refined.csv",
    col_types = cols(
      block = col_factor(levels = c("1",
                                    "2", "3", "4")),
      coverage = col_double(),
      date_floral = col_date(format = "%Y-%m-%d"),
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
      adj_total_flowers = col_double(),
      units = col_double(),
      vac_time = col_double()
    )
  )
View(floral_raw)
floral<-floral_raw


# Import Vac data ----
library(readr)
vac_raw <-
  read_csv(
    "Vac_Data_2015_DG_2-7-2017 Refined2.csv",
    col_types = cols(
      block = col_factor(levels = c("1",
                                    "2", "3", "4")),
      date_vac = col_date(format = "%Y-%m-%d"),
      site = col_factor(levels = c("SWMREC",
                                   "CRC", "NWMHRC")),
      species = col_factor(
        levels = c(
          "ACMI2",
          "AMCA6",
          "ASSY",
          "ASTU",
          "ASVE",
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
  floral$totalarea<- floral$adj_total_flowers * floral$avgarea
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
    "C:/Users/gibso124/Dropbox/MSU Data Analysis/Natural Enemies Data Analysis/NativePlants/floral_color_4_21.csv",
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
floral$totalpollen<- floral$adj_total_flowers * floral$poll_per_flower

#### CLEAN AND STANDARDIZE #### ----
##########################################################################-


#I know that the dates of floral data do not always match the dates of vac data (multiple sampling days)
#extract month of year. Converting to week for merging solves this problem.
#Get day of year (1-365)
library(lubridate)
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
#Defines 'site' and 'sitenum' as factors whose order in df can determine chart order
floral$site<-factor(floral$site, levels=unique(floral$site))
floral$sitenum<-factor(floral$sitenum, levels=unique(floral$sitenum))


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

#SILA3 is actually SITE. Throw out all SILA3 samples
vac<-vac[which(vac$species !="SILA3"),] # Removes 5 lines
floral<-floral[which(floral$species !="SILA3"),] #Removes 7 lines


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
# peakref.old<-read.csv("peak_reference_matrix2015.csv")
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
floral.peak<-subset(floral, peak == "y")#total=1639 #These last updated after adding ASSY peak weeks 32,33 at CRC
#Outgroup of subset by peakbloom from mpeakref
floral.notpeak<-subset(floral, peak == "n")#Total=725

#Merge mpeakref into vac. Subset to only peak bloom observations.

#Merge mpeakref into vac. 
vac<-merge(vac,mpeakref,by=c('site','week','species'),all.x=TRUE)

#Subset by peakbloom from mpeakref
vac.peak<-subset(vac, peak == "y")#total=1631
#Outgroup of subset by peakbloom from mpeakref
vac.notpeak<-subset(vac, peak == "n")#total=73

#### MERGE VAC and FLORAL DATA FRAMES #### ---- 
### Merges working properly as of 2-8-2017. 
# floral<--vac == 8 lines unmatched (same as known list of missing), no duplicates
# vac<--floral == 0 unmatched or duplicates

#merge peak.floral.clean into vac.peak
all.data<-merge(vac.peak,floral.peak,by=c('site','week','species','block'),all.x=TRUE)

# #Merge vac.peak into floral.peak
# all.data2<-merge(floral.peak,vac.peak,by=c('site','week','species','block'),all.x=TRUE)




#### CLEANING: Finding floral and vac merge problems #### ----

## Find lines that failed to merge ##
{
#subset NAs from all.data and all.data2 to for easier to deal with "error lists"
floral.missing<-subset(all.data, is.na(date_floral))
vac.missing<-subset(all.data2, is.na(date_vac))
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
write.csv(vac.missing, 'vac.missing.5.12.2017.csv')
write.csv(floral.missing, 'floral.missing.5.12.2017.csv')
write.csv(vac.duplicates, 'vac.duplicates.5.12.2017.csv')
write.csv(floral.duplicates, 'floral.duplicates.5.12.2017.csv')
write.csv(vac,'cleaned vac data.5.12.2017.csv')
write.csv(floral,'cleaned floral data.5.12.2017.csv')
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





#### T-Tests #### ----

# Independent 2-group t-test of species mean~mowed mean
#Base code:
t.test(x,y) # where x and y are numeric
# R defaults to running a Welch's t-test.
# You can use the "var.equal = TRUE" argument to specify equal variances and a pooled variance estimate. 
# You can use the alternative="less" or alternative="greater" option to specify a one tailed test.
# " alternative = 'greater' " is the alternative hypothesis that x has a larger mean than y 
## e.g. SPECIES > mowed
# Only run t-tests on species where mean is above mowed control.


#Subset all.data into sites for t-tests
{
  SWMREC.t<-subset(all.data, site == "SWMREC")
  CRC.t<-subset(all.data, site == "CRC")
  NWMHRC.t<-subset(all.data, site == "NWMHRC")
}

# EXPERIMENTATION
{
# subset for smaller dataset that my laptop can handle
{
  ttests<-subset(all.data, site == "SWMREC")
  library(plyr)
  ttests <- subset(ttests, select = c(1,2,3,4,7,77,79))
  ttests<-subset(ttests, species == "ACMI2" | species == 'AMCA6' |
                           species == "POSI2" | species == 'HEST' |
                          species == "MOFI" | species == 'PYVI' |
                           species == "SOSP2" | species == 'HIGR3' |
                          species == "MOWED")
  #Export Data
  write.csv(ttests, 'ttests.csv')
}

#Import ttests.csv
ttests<-read.csv("ttests.csv",header=TRUE)
ttests$total_ne<-as.numeric(ttests$total_ne)


# hmm, I might be able to subset by week and species, so I get a list of one species and its controls.
# With any luck I'll be able to run the test without further splitting the file.
# ACMI2<-subset(ttests, species == "ACMI2" | species == 'MOWED')
ACMI2<-subset(ttests, species == 'ACMI2')
ACMI2mowed<-subset(ttests, week == 25 & species == 'MOWED'| 
                           week == 26 & species == 'MOWED'| 
                           week == 27 & species == 'MOWED')

#Run T-test
t.test(ACMI2$total_ne,ACMI2mowed$total_ne)

#Run 1-tailed T-test
t.test(ACMI2$total_ne,ACMI2mowed$total_ne, alternative = 'greater')

# Run 1-tailed T-test
t.test(subset(SWMREC.t$total_ne, 
                SWMREC.t$species == 'SPECIES'),
       subset(SWMREC.t$total_ne, 
                SWMREC.t$week == 00 & SWMREC.t$species == 'MOWED'| 
                SWMREC.t$week == 00 & SWMREC.t$species == 'MOWED'| 
                SWMREC.t$week == 00 & SWMREC.t$species == 'MOWED'),
       alternative = 'greater') 
}

## SWMREC T-TESTS ####-
{
  # SPECIES (TEMPLATE)
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'SPECIES'),
           subset(SWMREC.t$total_ne, 
                    SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # ACMI2
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'ACMI2'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week == 25 & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week == 26 & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week == 27 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # CEAM
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'CEAM'),
          subset(SWMREC.t$total_ne, 
                 # SWMREC.t$week == 00 & SWMREC.t$species == 'MOWED'| 
                 #   SWMREC.t$week == 00 & SWMREC.t$species == 'MOWED'| 
                 SWMREC.t$week == 27 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # COLA5
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'COLA5'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 24 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 25 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 26 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # COTR4
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'COTR4'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 33 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 35 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # DAFR6
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'DAFR6'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 30 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 31 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 32 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ERYU
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'ERYU'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 32 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 33 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # HEOC2
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'HEOC2'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 33 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 35 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # HEST
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'HEST'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 35 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 36 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # LOCO6
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'LOCO6'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 25 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 26 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 27 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # MOFI
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'MOFI'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 29 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 30 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 31 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # MOPU
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'MOPU'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 32 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 33 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # OLRI
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'OLRI'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 35 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 36 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 37 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # POSI2
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'POSI2'),
          subset(SWMREC.t$total_ne, 
                 # SWMREC.t$week == 00 & SWMREC.t$species == 'MOWED'| 
                 SWMREC.t$week == 22 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 23 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # PYPI
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'PYPI'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 32 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 33 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # PYVI
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'PYVI'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 30 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 31 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 32 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # RAPI
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'RAPI'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 30 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 31 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 32 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SIIN2
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'SIIN2'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 35 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 36 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SOJU
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'SOJU'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 33 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 35 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SONE
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'SONE'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 30 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 31 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 32 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SOSP2
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'SOSP2'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 38 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 39 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 40 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SYOO
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'SYOO'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 38 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 39 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 40 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SYSE2
  {t.test(subset(SWMREC.t$total_ne, 
                 SWMREC.t$species == 'SYSE2'),
          subset(SWMREC.t$total_ne, 
                 SWMREC.t$week == 36 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 37 & SWMREC.t$species == 'MOWED'| 
                   SWMREC.t$week == 38 & SWMREC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
}

## CRC T-TESTS ####-
{
# SPECIES (TEMPLATE)
{t.test(subset(CRC.t$total_ne, 
               CRC.t$species == 'SPECIES'),
        subset(CRC.t$total_ne, 
               CRC.t$week ==  & CRC.t$species == 'MOWED'| 
               CRC.t$week ==  & CRC.t$species == 'MOWED'| 
               CRC.t$week ==  & CRC.t$species == 'MOWED'),
        alternative = 'greater') 
}

  # ACMI2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ACMI2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 26 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 27 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 28 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ASVE
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ASVE'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 32 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # CESTM
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'CESTM'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # COLA5
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'COLA5'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 25 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 26 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 27 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # COTR4
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'COTR4'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # DAFR6
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'DAFR6'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ECPU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ECPU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ERYU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ERYU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # HEOC2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'HEOC2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # HEST
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'HEST'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # LOCO6
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'LOCO6'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 28 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 29 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 30 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # MOFI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'MOFI'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 29 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 30 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # MOPU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'MOPU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # OLRI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'OLRI'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 35 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 37 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # POSI2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'POSI2'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week == 00 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 23 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 24 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # PYVI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'PYVI'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 30 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # RAPI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'RAPI'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # RUHI2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'RUHI2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 30 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SIIN2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SIIN2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SOJU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SOJU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 32 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SONE
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SONE'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 30 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }

  # SOSP2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SOSP2'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week == 00 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 38 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 39 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SYOO
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SYOO'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week == 00 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 38 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 39 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SYSE2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SYSE2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 37 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 38 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 39 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # VEST
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'VEST'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 29 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 30 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }

}

## NWMHRC T-TESTS ####-
{
  # SPECIES (TEMPLATE)
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SPECIES'),
            subset(NWMHRC.t$total_ne, 
                  NWMHRC.t$week == 00 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 00 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 00 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ASVE
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'ASVE'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ACMI2
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'ACMI2'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 28 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # CESTM
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'CESTM'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # CHANA2
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'CHANA2'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # COLA5
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'COLA5'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 28 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # COPA10
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'COPA10'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # COTR4
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'COTR4'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # DAFR6
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'DAFR6'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # DAPU5
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'DAPU5'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ECPU
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'ECPU'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ERYU
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'ERYU'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # HEOC2
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'HEOC2'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # HEST
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'HEST'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 39 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # HYPR
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'HYPR'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LECA8
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'LECA8'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LOCO6
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'LOCO6'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 28 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # MOFI
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'MOFI'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # MOPU
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'MOPU'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # OLRI
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'OLRI'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # PEHI
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'PEHI'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 25 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # PYPI
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'PYPI'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # PYVI
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'PYVI'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # RAPI
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'RAPI'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # RUHI2
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'RUHI2'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SIIN2
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SIIN2'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SITE - Onlyu 1 sample, cannot test
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SITE'),
          subset(NWMHRC.t$total_ne, 
                   # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'| 
                   # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SOJU
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SOJU'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SONE
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SONE'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SOSP2
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SOSP2'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 39 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 40 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 41 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SYOO
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SYOO'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 39 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 40 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 41 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SYSE2
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'SYSE2'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 39 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # VEST
  {t.test(subset(NWMHRC.t$total_ne, 
                 NWMHRC.t$species == 'VEST'),
          subset(NWMHRC.t$total_ne, 
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'| 
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
}
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

write.csv(vac.peak, 'vac.peak.2.20.2017.csv')
