#Bring in primary data
#*******************
# Must set Working Directory First!!
# Bulk Package Loading #### ----

## The warning "the following objects are masked" means that two packages loaded have
## identical commands, and R will use the command in the package loaded LAST.
## To specify which package for R to use, use the double colon operator(::)
#     package::function
#     plyr::summarise
## simply adding 'library(package)' above the current command DOES NOT fix this!
## To see the order of package loading, type 'search()'
library(readr)
library(lubridate)
library(ISOweek)
library(stringr)
library(reshape)
library(plyr)

#Charts Code Packages
library(magrittr)
library(ggplot2)
library(plyr)
library(reshape)
library(viridis)
library(RColorBrewer)
library(dplyr) #for the 'rbind()' command. 'rank()' is base R.

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
# View(floral_raw)
floral<-floral_raw

# floral_problems<-problems(floral_raw)


#----
# Import Vac data ----
library(readr)
vac_raw <-
  read_csv(
    "vac2016_7_12_2017.csv",
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
# View(vac_raw)
vac<-vac_raw
# vac_problems<-problems(vac_raw)


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
# View(color)

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
floral$week<-as.numeric(floral$week)
vac$week<-str_sub(vac$week, start= -2)
vac$week<-as.numeric(vac$week)

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
# write.csv(all.data, 'all.data2016.csv')

#Calculate Summary Variables ----
#Calculate total NEs in new variable
all.data$total_ne<-rowSums(all.data[,c("anthocoridae","nabidae","reduviidae",
                                       "cantharidae","carabidae","coccinellidae","chrysopidae",
                                       "hemerobiidae","sphecidae","tiphiidae",
                                       "vespidae","bombyliidae","syrphidae","tachinidae",
                                       "geocoridae", "aeolothripidae", "phlaeothripidae", 
                                       "ichneumonidae", "braconidae", "chalcidoidea", "cynipoidea",
                                       "aranae", "opiliones")])

#Calculate total herbivores in new variable
all.data$total_herb<-rowSums(all.data[,c("aphidae","miridae","rhyparochromidae",
                                         "pentatomidae","tingidae","cerambicidae","nitidulidae",
                                         "chrysomelidae","curculionidae","elateridae","scarabaeidae",
                                         "orthoptera","lep_adult","lep_caterpillar")])

#Calculate total NE Arachnida in new variable
all.data$arachnida<-rowSums(all.data[,c("aranae","opiliones")])

#Calculate total NE Diptera in new variable
all.data$diptera_ne<-rowSums(all.data[,c("bombyliidae","syrphidae","tachinidae",
                                         "dolichopodidae","empididae")])

#Calculate total NE Lacewings in new variable
all.data$neuroptera_ne<-rowSums(all.data[,c("chrysopidae","hemerobiidae")])

#Calculate total NE Coleoptera in new variable
all.data$coleoptera_ne<-rowSums(all.data[,c("cantharidae","carabidae","coccinellidae")])

#Calculate total NE Hymenoptera in new variable
all.data$hymenoptera_ne<-rowSums(all.data[,c("ichneumonidae","braconidae","chalcidoidea",
                                             "cynipoidea","sphecidae","tiphiidae",
                                             "vespidae")])

#Calculate total NE Hemiptera in new variable
all.data$hemiptera_ne<-rowSums(all.data[,c("anthocoridae","nabidae",
                                           "reduviidae","geocoridae")])

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

## SWMREC T-TESTS ####-
{
  # ACMI2
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'ACMI2'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week == 24 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 25 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 26 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # ASSY  
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'ASSY'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week == 24  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 25  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 26  & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # ASVE
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'ASVE'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week == 31  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 32  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 33  & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # COLA5
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'COLA5'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week == 23  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 24  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 25  & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # COTR4
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'COTR4'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week == 32  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 33 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week == 34 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # DAFR6
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'DAFR6'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  35 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  36 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  37 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # HEOC2
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'HEOC2'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  31 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  32 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  33 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # HEST
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'HEST'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  33 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  34 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  35 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # HYPR
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'HYPR'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  27 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  28 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  29 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # LIAS
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'LIAS'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  34 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  35 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  36 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # LOCO6
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'LOCO6'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  25 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  26 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  27 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # MOFI
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'MOFI'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  28 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  29 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  30 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # MOPU
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'MOPU'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  30 & SWMREC.t$species == 'MOWED'| 
                  # SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  31 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # OEBI
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'OEBI'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  34 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  35 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  36 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  
  # OLRI
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'OLRI'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  35 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  36 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  37 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
 
 # POSI2
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'POSI2'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  20 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  21 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  22 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # PYPI
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'PYPI'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  30 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  31 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  32 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # PYVI
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'PYVI'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  28 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  29 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  30 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # RHCO 
  #Cannot test--only one observation
  {
  #   t.test(subset(SWMREC.t$total_ne, 
  #                 SWMREC.t$species == 'RHCO'),
  #          subset(SWMREC.t$total_ne, 
  #                 # SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
  #                 # SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
  #                 SWMREC.t$week ==  32 & SWMREC.t$species == 'MOWED'),
  #          alternative = 'greater') 
  }
  
  # SIIN2
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'SIIN2'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  33 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  34 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  35 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # SOJU
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'SOJU'),
           subset(SWMREC.t$total_ne, 
                  # SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  31 & SWMREC.t$species == 'MOWED'| 
                  SWMREC.t$week ==  32 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # SONE
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'SONE'),
           subset(SWMREC.t$total_ne, 
                  SWMREC.t$week ==  28 & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week == 29  & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  30 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # SOSP2
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'SOSP2'),
           subset(SWMREC.t$total_ne, 
                    # SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
                    # SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  37 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # SYOO
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'SYOO'),
           subset(SWMREC.t$total_ne, 
                  # SWMREC.t$week ==  & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  37 & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  40 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
  
  # SYSE2
  {
    t.test(subset(SWMREC.t$total_ne, 
                  SWMREC.t$species == 'SYSE2'),
           subset(SWMREC.t$total_ne, 
                    SWMREC.t$week ==  35 & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  36 & SWMREC.t$species == 'MOWED'| 
                    SWMREC.t$week ==  37 & SWMREC.t$species == 'MOWED'),
           alternative = 'greater') 
  }
}
  
## CRC T-TESTS ####-
{
  # SPECIES (TEMPLATE)
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SPECIES'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 0 & CRC.t$species == 'MOWED'| 
                 CRC.t$week == 0 & CRC.t$species == 'MOWED'| 
                 CRC.t$week == 0 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ACMI2
  {t.test(subset(CRC.t$total_ne,
                 CRC.t$species == 'ACMI2'),
          subset(CRC.t$total_ne,
                 CRC.t$week ==  25 & CRC.t$species == 'MOWED'|
                   CRC.t$week ==  26 & CRC.t$species == 'MOWED'|
                   CRC.t$week ==  27 & CRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ASSY
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ASSY'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 26 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 27 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 28  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ASTU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ASTU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 26  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 27 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 28  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ASVE
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ASVE'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 28  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 29  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # CEAM
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'CEAM'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week ==   & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 26 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 27  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # CESTM
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'CESTM'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 32  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # COPA10
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'COPA10'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 27  & CRC.t$species == 'MOWED'| 
                 CRC.t$week == 28  & CRC.t$species == 'MOWED'| 
                 CRC.t$week == 29  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # COTR4
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'COTR4'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 33  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # DAFR6
  {t.test(subset(CRC.t$total_ne, 
                   CRC.t$species == 'DAFR6'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 31  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # DAPU5
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'DAPU5'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32  & CRC.t$species == 'MOWED'| 
                   CRC.t$week ==  33 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ECPU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ECPU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  32 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  #ERYU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ERYU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }

  # HEOC2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'HEOC2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 31  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # HEST
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'HEST'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week ==   & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # HYPR
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'HYPR'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 27  & CRC.t$species == 'MOWED'| 
                   CRC.t$week ==  28 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 29  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # LIAS
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'LIAS'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }

  # LOCO6 
  ## We also sampled at the second peak, weeks 31-33
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$week ==  25 & CRC.t$species == 'LOCO6'| 
                   CRC.t$week == 26  & CRC.t$species == 'LOCO6'| 
                   CRC.t$week == 27  & CRC.t$species == 'LOCO6'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  25 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 26  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 27  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # MOFI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'MOFI'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  27 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 28  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 29  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # MOPU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'MOPU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # OLRI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'OLRI'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 34  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 35 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 36  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # POSI2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'POSI2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  21 & CRC.t$species == 'MOWED'| 
                   CRC.t$week ==  22 & CRC.t$species == 'MOWED'| 
                   CRC.t$week ==  23 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # PYVI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'PYVI'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week ==  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 29  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # RAPI
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'RAPI'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  29 & CRC.t$species == 'MOWED'| 
                   CRC.t$week ==  31 & CRC.t$species == 'MOWED'| 
                   CRC.t$week ==  32 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # ROCA4
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'ROCA4'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week ==  & CRC.t$species == 'MOWED'| 
                   # CRC.t$week ==  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 25  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # RUHI2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'RUHI2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  27 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 28  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 29  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SEOB2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SEOB2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 21  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 22 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 23  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SIIN2
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SIIN2'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 32  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 33 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SILA3
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SILA3'),
          subset(CRC.t$total_ne, 
                 CRC.t$week ==  29 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 32  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SITE
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SITE'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 33  & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 34 & CRC.t$species == 'MOWED'| 
                   CRC.t$week ==  35 & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }

  # SOJU
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SOJU'),
          subset(CRC.t$total_ne, 
                 CRC.t$week == 31 & CRC.t$species == 'MOWED'|
                 CRC.t$week == 32 & CRC.t$species == 'MOWED'| 
                 CRC.t$week == 33  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SONE
  {t.test(subset(CRC.t$total_ne, 
                 CRC.t$species == 'SONE'),
          subset(CRC.t$total_ne, 
                 # CRC.t$week ==   & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 29 & CRC.t$species == 'MOWED'| 
                   CRC.t$week == 31  & CRC.t$species == 'MOWED'),
          alternative = 'greater') 
  }
  
  # SOSP2
  {t.test(subset(CRC.t$total_ne,
                 CRC.t$species == 'SOSP2'),
          subset(CRC.t$total_ne,
                 # CRC.t$week ==  & CRC.t$species == 'MOWED'|
                   CRC.t$week == 38  & CRC.t$species == 'MOWED'|
                   CRC.t$week == 40  & CRC.t$species == 'MOWED'),
          alternative = 'greater')
  }

  # SYOO
  {t.test(subset(CRC.t$total_ne,
                 CRC.t$species == 'SYOO'),
          subset(CRC.t$total_ne,
                 # CRC.t$week ==  & CRC.t$species == 'MOWED'|
                   CRC.t$week == 38  & CRC.t$species == 'MOWED'|
                   CRC.t$week == 40  & CRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SYSE2
  {t.test(subset(CRC.t$total_ne,
                 CRC.t$species == 'SYSE2'),
          subset(CRC.t$total_ne,
                 CRC.t$week == 36  & CRC.t$species == 'MOWED'|
                   CRC.t$week == 37 & CRC.t$species == 'MOWED'|
                   CRC.t$week == 38  & CRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # VEST
  {t.test(subset(CRC.t$total_ne,
                 CRC.t$species == 'VEST'),
          subset(CRC.t$total_ne,
                 CRC.t$week ==  29 & CRC.t$species == 'MOWED'|
                   CRC.t$week ==  31 & CRC.t$species == 'MOWED'|
                   CRC.t$week ==  32 & CRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
}

## NWMHRC T-TESTS ####-
{
  # SPECIES (TEMPLATE)
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'SPECIES'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }

  # ACMI2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'ACMI2'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 25 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # AMCA6
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'AMCA6'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ASSY
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'ASSY'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ASTU
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'ASTU'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ASVE
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'ASVE'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # BAALM
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'BAALM'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 24 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # CARO2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'CARO2'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # CEAM
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'CEAM'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # CESTM
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'CESTM'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # COLA5
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'COLA5'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 25 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # COPA10
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'COPA10'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'),
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
                 NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # DAPU5
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'DAPU5'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ECPU
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'ECPU'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ERYU
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'ERYU'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # HEOC2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'HEOC2'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # HERI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'HERI'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 24 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 25 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # HEST
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'HEST'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # HYPR
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'HYPR'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LECA8
  # cAN'T RUN TEST -- ONLY ONE OBS
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'LECA8'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LEHI2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'LEHI2'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LIAS
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'LIAS'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 35 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LICY
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'LICY'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LOCO6
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'LOCO6'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 24 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 25 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # LUPE3
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'LUPE3'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 22 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 23 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 24 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # MOFI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'MOFI'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # MOPU
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'MOPU'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # OEFR
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'OEFR'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # OLRI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'OLRI'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # PEDI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'PEDI'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # PEHI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'PEHI'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 24 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 25 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # POSI2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'POSI2'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 21 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 22 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 23 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # PYPI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'PYPI'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # PYVI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'PYVI'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # RAPI
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'RAPI'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # ROCA4
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'ROCA4'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # RUHI2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'RUHI2'),
          subset(NWMHRC.t$total_ne,
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SEOB2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'SEOB2'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 21 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 22 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 23 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SIIN2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'SIIN2'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SITE
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'SITE'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 32 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 33 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 34 & NWMHRC.t$species == 'MOWED'|
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
                 # NWMHRC.t$week ==  & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 39 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 40 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SYOO
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'SYOO'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 39 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 40 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # SYSE2
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'SYSE2'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 36 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 37 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 38 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # TROH
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'TROH'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 25 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 26 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 27 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
  
  # VEST
  {t.test(subset(NWMHRC.t$total_ne,
                 NWMHRC.t$species == 'VEST'),
          subset(NWMHRC.t$total_ne,
                 NWMHRC.t$week == 29 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 30 & NWMHRC.t$species == 'MOWED'|
                   NWMHRC.t$week == 31 & NWMHRC.t$species == 'MOWED'),
          alternative = 'greater')
  }
}