library(readxl)
library(ggplot2)
library(tidyverse)
library(janitor)
library(data.table)  
library(openxlsx)
library(splitstackshape)
library(naniar)
library(ggpubr)
library(ggpmisc)
options(scipen=999)
library(caret)

setwd()


#USGS DATA----

# The below file import is USGS NWIS water-quality database
# File created on 2023-10-09 10:05:11 EDT
# U.S. Geological Survey
# This file contains selected water-quality data for stations in the National Water Information System

#import
#create USGS WQ database
usgs_wq<-read.csv("Data//usgs_data.csv")%>%
  clean_names()

#this subsets the data so it only the USGS descriptions: dates, codes, what the data is, etc.
description<-usgs_wq%>%
  filter(x_2=='')

#below we select between the starting and ending rows, just outside of the row descriptions
start=which (description$x == '# https://help.waterdata.usgs.gov/tutorials/water-quality-data/help-using-the-water-quality-data-retrieval-system#Data_retrievals_precautions')+2
end=which (description$x == '# Description of sample_start_time_datum_cd:')-2

#select only row descriptions, and split into two columns with the short and long name for each column
description<-usgs_wq%>%
  filter(row_number()>=start&row_number()<=end)%>%
  select(x)%>%
  separate_wider_delim(x, " -", names = c("short", "long"))%>%
  mutate(short=gsub("#  ","",short))

#filter usgs data to remove the data description
usgs_wq<-usgs_wq%>%
  filter(x_2!='')

#set column names to the descritions in the first row and replace the column short names with the long names from the data description
colnames=usgs_wq[1,]
colnames<-t(colnames)%>%
  as.data.frame()%>%
  mutate(id=row_number())
colnames(colnames)<-c("short","id")
colnames<-colnames%>%
  mutate(short=gsub(" ", "", short),
         short=tolower(short))%>%
  merge(description%>%
          mutate(short=gsub(" ", "", short),
                 short=tolower(short)),all.x = T,by="short")%>%
  arrange(id)
colnames<-colnames$long
usgs_wq<-usgs_wq[3:length(t(usgs_wq)),]
colnames(usgs_wq)<-colnames

#data cleaning
usgs_wq<-usgs_wq%>%
  clean_names()%>%
  filter(!is.na(station_number))%>%#select rows with a station ID
  mutate(station_number=round(as.numeric(station_number)))%>%# change the station numbers to numeric form
  lapply(gsub, pattern = "<", replacement = "", fixed = TRUE)%>%#replace data that is < value with that value
  as.data.frame()%>%
  lapply(gsub, pattern = "E ", replacement = "", fixed = TRUE)%>%#remove characters from the numeric columns
  as.data.frame()

usgs_gw<-usgs_wq%>%
  mutate(begin_date=as.Date(begin_date,"%m/%d/%Y"),#clean the date formats
         year=year(begin_date))%>%
  filter(year>=2010)%>%# only select dates after 2010
  group_by(station_number,year)%>%#below we calculate the average values at each location in each year
  mutate(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen=ifelse(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen=='',nitrate_plus_nitrite_water_unfiltered_milligrams_per_liter_as_nitrogen,nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen))%>%
  summarise(nitrate_n_results=sum(!is.na(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen))),
            nitrate_source_mean=mean(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen),na.rm=T),
            ph_n_results=sum(!is.na(as.numeric(p_h_water_unfiltered_laboratory_standard_units))),
            ph_source_mean=mean(as.numeric(p_h_water_unfiltered_laboratory_standard_units),na.rm=T),
            alkalinity_total_caco3_n_results=sum(!is.na(as.numeric(acid_neutralizing_capacity_water_unfiltered_fixed_endpoint_p_h_4_5_titration_field_milligrams_per_liter_as_calcium_carbonate))),
            alkalinity_total_caco3_source_mean=mean(as.numeric(acid_neutralizing_capacity_water_unfiltered_fixed_endpoint_p_h_4_5_titration_field_milligrams_per_liter_as_calcium_carbonate),na.rm=T),
            manganese_n_results=sum(!is.na(as.numeric(manganese_water_unfiltered_recoverable_micrograms_per_liter))),
            manganese_source_mean=mean(as.numeric(manganese_water_unfiltered_recoverable_micrograms_per_liter),na.rm=T),
            arsenic_n_results=sum(!is.na(as.numeric(arsenic_water_filtered_micrograms_per_liter))),
            arsenic_source_mean=mean(as.numeric(arsenic_water_filtered_micrograms_per_liter),na.rm=T),
            uranium_n_results=sum(!is.na(as.numeric(uranium_natural_water_filtered_micrograms_per_liter))),
            uranium_source_mean=mean(as.numeric(uranium_natural_water_filtered_micrograms_per_liter),na.rm=T),
            source='USGS')


#import site coordinates for each USGS station, clean this data and merge it with the stations from the GW database
usgs_sites<-read.csv("Data//usgs_sites.csv")%>%
  clean_names()

usgs_sites<-usgs_sites%>%
  filter(x_2!='')
colnames=usgs_sites[1,]

usgs_sites<-usgs_sites[3:length(t(usgs_sites)),]
colnames(usgs_sites)<-colnames

usgs_gw<-usgs_gw%>%
  merge(usgs_sites%>%
          mutate(latitude=dec_lat_va,
                 longitude=dec_long_va,
                 station_number=site_no)%>%
          select(station_number,latitude,longitude),all.x=T)

#This is a dataset of the coordinates of USGS wells that had been merged with the town it is located in in ArcGIS
usgs_well_coords<-read.csv("Data//usgs_well_coords.csv",sep=";")%>%
  clean_names()%>%
  mutate(latitude=dec_lat_va,
         longitude=dec_long_va,
         station_number=site_no)%>%
  select(town,latitude,longitude)%>%
  distinct()

#Below we summarize the data at all USGS locations (not by year), using data even before 2010
water_quality_all_wells<-usgs_wq%>%
  mutate(begin_date=as.Date(begin_date,"%m/%d/%Y"),
         year=year(begin_date),
         month=month(begin_date),
         day=day(begin_date))%>%
  group_by(station_number)%>%
  mutate(max_year=max(year),
         max_month=max(month[year==max_year]),
         max_day=max(day[year==max_year&month==max_month]))%>%
  ungroup()%>%
  mutate(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen=ifelse(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen=='',nitrate_plus_nitrite_water_unfiltered_milligrams_per_liter_as_nitrogen,nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen))%>%
  group_by(station_number)%>%
  summarise(nitrate_n_results=sum(!is.na(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen))),
            nitrate_source_mean=mean(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen),na.rm=T),
            nitrate_recent_result=mean(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen[day==max_day&year==max_year&month==max_month],na.rm=T)),
            ph_n_results=sum(!is.na(as.numeric(p_h_water_unfiltered_laboratory_standard_units))),
            ph_source_mean=mean(as.numeric(p_h_water_unfiltered_laboratory_standard_units),na.rm=T),
            alkalinity_total_caco3_n_results=sum(!is.na(as.numeric(acid_neutralizing_capacity_water_unfiltered_fixed_endpoint_p_h_4_5_titration_field_milligrams_per_liter_as_calcium_carbonate))),
            alkalinity_total_caco3_source_mean=mean(as.numeric(acid_neutralizing_capacity_water_unfiltered_fixed_endpoint_p_h_4_5_titration_field_milligrams_per_liter_as_calcium_carbonate),na.rm=T),
            manganese_n_results=sum(!is.na(as.numeric(manganese_water_unfiltered_recoverable_micrograms_per_liter))),
            manganese_source_mean=mean(as.numeric(manganese_water_unfiltered_recoverable_micrograms_per_liter),na.rm=T),
            arsenic_n_results=sum(!is.na(as.numeric(arsenic_water_filtered_micrograms_per_liter))),
            arsenic_source_mean=mean(as.numeric(arsenic_water_filtered_micrograms_per_liter),na.rm=T),
            arsenic_recent_result=mean(as.numeric(arsenic_water_filtered_micrograms_per_liter[day==max_day&year==max_year&month==max_month],na.rm=T)),
            uranium_n_results=sum(!is.na(as.numeric(uranium_natural_water_filtered_micrograms_per_liter))),
            uranium_source_mean=mean(as.numeric(uranium_natural_water_filtered_micrograms_per_liter),na.rm=T),
            source='USGS')%>%
  merge(usgs_sites%>%
          mutate(latitude=dec_lat_va,
                 longitude=dec_long_va,
                 station_number=site_no)%>%
          select(station_number,latitude,longitude),all.x=T)%>%
  merge(usgs_well_coords,by=c("latitude","longitude"),all.x=T)

#EEA DATA----

#import 2021 SDWIS data describing each MA PWS
PWS<-read.csv("Data\\MA PWSs.csv")

#import MASS PWS Sources data from https://www.mass.gov/info-details/massgis-data-public-water-supplies
pws_sources<-read.csv("Data/pws-source-id.csv")%>%
  filter(TYPE!='SW')


#download all water quality from EEA: https://eeaonline.eea.state.ma.us/portal#!/search/drinking-water
#split over multiple files to to download size constraints
wq_data <- list.files(path = "Data/wq_data",    
                      pattern = "*.xlsx",
                      full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()%>%
  filter(`PWS ID`!='PWS ID')%>%
  mutate(PWS.ID=paste('MA',`PWS ID`,sep=""))%>%#clean to match formatting of other databases
  dplyr::select(-`PWS ID`)%>%
  #below, if we find G, or Well in the ID we set it as a groundwater location (column gw)
  mutate(gw=ifelse(grepl('G',`Location ID`),'Y','N'),
         gw=ifelse(grepl('WELL',`Location Name`),'Y',gw))%>%
  merge(PWS%>%
          dplyr::select(-PWS.Name,-EPA.Region,-Primacy.Agency),all.x=T)%>%#merge with SDWIS information
 #cleaning date columns below
   mutate(date=as.Date(`Collected Date`),
         year=year(date),
         month=month(date),
         day=day(date),
         Result=as.numeric(Result),
         gw=ifelse(Class!='COM'&grepl('round',`Primary.Source`),'Y',gw))%>%#Set any additional locations which are a NTNC or TNC
  #and have G"round" water as their source, to be a gw source
  group_by(`Chemical Name`)%>%
  mutate(min=min(Result,na.rm=T),
         rdl=as.numeric(`Required Detection Limit`))%>%
  filter(year>=2010)%>%#select only data after 2010 due to detection limit issues before then
  separate(`Lab Reported Detection Limit`, 
           " ",
           into = c("ld","units"))%>%
  mutate(ld=as.numeric(ld),
         nd=ifelse(!is.na(Result),'N','Y'),
         Result=ifelse(is.na(Result),.5*ld,Result),
         Result=ifelse(is.na(Result),.5*rdl,Result),
         Result=ifelse(is.na(Result),.5*min,Result))%>%
  filter(`Chemical Name`!='ARSENIC'|ld!=1)# select on arsenic data, with lower detection limit

#below we clean arsenic data below the detection limy
arsenic_all<-wq_data%>%
  filter(`Chemical Name`=='ARSENIC')%>%
  mutate(ld=ifelse(ld==0,0.0001,ld))%>%#change detection limits reported as 0 to 0.0001 mg/L
  arrange(ld)

#below we assign arsenic values below the detection limit to the average of all As in the database that were detected below that limit
for (i in 1:length(t(arsenic_all$Result))){
  arsenic_all$Result[i]<-ifelse(arsenic_all$ld[i]>0.001&arsenic_all$nd[i]=='Y',mean(arsenic_all$Result[arsenic_all$Result<=arsenic_all$ld[i]&(arsenic_all$ld!=arsenic_all$ld[i]|arsenic_all$nd=='N')]),arsenic_all$Result[i])
}

#add back in the non-detect As to the database
wq_data<-wq_data%>%
  filter(`Chemical Name`!='ARSENIC')%>%
  rbind(arsenic_all%>%
          filter(nd=='N'|ld<=0.005))



#Water Location Summaries
wq_loc_sum2<-wq_data%>%
  mutate(source_id=paste(gsub('MA','',PWS.ID),`Location ID`,sep='-'),
         `Chemical Name`=ifelse(`Chemical Name`=='ALKALINITY (CACO3), TOTAL','ALKALINITY-TOTAL (CACO3)',`Chemical Name`))%>%
  group_by(source_id,`Chemical Name`,`Raw or Finished`,`Location Name`,gw)%>%
  mutate(max_year=max(year),
         max_month=max(month[year==max_year]),
         max_day=max(day[year==max_year&month==max_month]))%>%
  dplyr::select(-`Location ID`)%>%
  #calculate the number of results, most recent result, and average at each site for all the below contaminants
  summarise(n_results=sum(!is.na(Result)),
            source_mean=mean(Result),
            recent_result=mean(Result[day==max_day&year==max_year&month==max_month],na.rm=T))%>%
  ungroup()%>%
  filter(`Chemical Name`=='ALKALINITY-TOTAL (CACO3)'|`Chemical Name`=='ARSENIC'|`Chemical Name`=='CHLORIDE'|
           `Chemical Name`=='MANGANESE'|`Chemical Name`=='NITRATE'|`Chemical Name`=='NITRITE'|
           `Chemical Name`=='PH'|`Chemical Name`=='URANIUM'|`Chemical Name`=="BROMODICHLOROMETHANE"|`Chemical Name`=="CHROMIUM"|
           `Chemical Name`=="DIBROMOCHLOROMETHANE"|`Chemical Name`=="CHLOROFORM"|
           `Chemical Name`=="BROMOFORM"|`Chemical Name`=="TRICHLOROACETIC ACID"|
           `Chemical Name`=="DICHLOROACETIC ACID"|`Chemical Name`=="BROMATE"|
           `Chemical Name`=="RADIUM - 226"|`Chemical Name`=="RADIUM - 228"|
           `Chemical Name`=="DIBROMOCHLOROPROPANE"|`Chemical Name`=="1,2,3-TRICHLOROPROPANE"|
           `Chemical Name`=="TETRACHLOROETHYLENE"|`Chemical Name`=="1,4-DIOXANE"|
           `Chemical Name`=="TRICHLOROETHYLENE"|`Chemical Name`=="PERFLUOROOCTANOIC ACID-PFOA"|
           `Chemical Name`=="PERFLUOROOCTANESULFONIC ACID-PFOS"|`Chemical Name`=="PFAS6"|
           `Chemical Name`=="PERFLUORONONANOIC ACID-PFNA"|`Chemical Name`=="PERFLUOROHEXANESULFONIC ACID-PFHXS"|
           `Chemical Name`=="PERFLUOROHEPTANOIC ACID-PFHPA"|`Chemical Name`=="PERFLUORODECANOIC ACID - PFDA"
  )%>%
  pivot_wider(names_from=c(`Chemical Name`),values_from =c(n_results,source_mean,recent_result),names_glue = "{`Chemical Name`}_{.value}")


#below we merge the EEA water quality data with the MassGIS PWS suources
wq_loc_sum2<-pws_sources%>%
  select(SOURCE_ID,SITE_NAME,TOWN,LATITUDE,LONGITUDE)%>%
  mutate(SOURCE_ID_merge=SOURCE_ID,
         SOURCE_ID=gsub('-0','-',SOURCE_ID),#remove-0 from the IDs
         SITE_NAME=gsub('#','',SITE_NAME),#remove # from IDs
         SITE_NAME=gsub(' RAW WATER','',SITE_NAME))%>%
  merge(wq_loc_sum2%>%
          mutate(SOURCE_ID=source_id,
                 #all the below formatting is so the IDs match between the two databases
                 SOURCE_ID=ifelse(grepl('-RW',source_id),paste(source_id,'G',sep=''),source_id),#replace -RW (raw water) with G so the IDs match
                 SOURCE_ID=gsub('RW','',SOURCE_ID),#replace RW (raw water) with G so the IDs match
                 SOURCE_ID=gsub('-0','-',SOURCE_ID),
                 SOURCE_ID=gsub('--','-',SOURCE_ID),
                 SOURCE_ID=gsub('GG','G',SOURCE_ID),
                 `Location Name`=gsub('#','',`Location Name`),
                 `Location Name`=gsub(' RAW WATER','',`Location Name`),
                 #These were changed after manual investigation
                 SOURCE_ID=case_when(
                   `Location Name`=='WELL #3 SOURCE WATER TAP'~'1022026-3G',
                   `Location Name`=='CHESHIRE HARBOR 2A'~'1004000-2G',
                   `Location Name`=='CHESHIRE HARBOR 3'~'1004000-3G',
                   `Location Name`=='CHESHIRE HARBOR 4'~'1004000-4G',
                   T~SOURCE_ID
                 )),all.y=T)%>%
  mutate(PWS.ID=paste('MA',gsub("-.*","",SOURCE_ID),sep=''))%>%
  select(-SOURCE_ID,-SOURCE_ID_merge,-SITE_NAME)#ony keep the original source_id from the EEA database


#MWRA cleaning
#below is a MassDEP dataset with a list of PWSs in MA, that has a column indicating if it is in the MWRA
mwra<-read_excel("Data//EAP_PWS Compiled List 11082022.xlsx")%>%
  clean_names()%>%
  filter(mwra_consecutive=='Y')%>%
  mutate(source_id=paste('MA',pwsid,sep=''))%>%
  select(source_id,mwra_consecutive,town)

#list of all PWS Ids in MWRA
mwra_pws<-mwra$source_id

#select all the source IDs in MWRA and assign them to the various PWSs in MWRA
mwra<-wq_loc_sum2%>%
  filter(PWS.ID=='MA6000000')%>%
  select(source_id)%>%
  merge(mwra_pws,all=T)%>%
  mutate(PWS.ID=y)%>%
  select(-y)

#This same process is repeated for the following water systems

#Springfield
swsc_pws<-c('MA1281000','MA1005000','MA1085000','MA1159000','MA1325000') 
swsc<-wq_loc_sum2%>%
  filter(PWS.ID=='MA1281000')%>%
  merge(swsc_pws,all=T)%>%
  mutate(PWS.ID=y)%>%
  select(-y)

#Holbrook/Randolph
holbrook_randolph<-c('MA4133000','MA4244000') 
hr<-wq_loc_sum2%>%
  filter(PWS.ID=='MA4244001')%>%
  merge(holbrook_randolph,all=T)%>%
  mutate(PWS.ID=y)%>%
  select(-y)


#Salem/Beverly
salem_bev<-c('MA3258000','MA3030000') 
sb<-wq_loc_sum2%>%
  filter(PWS.ID=='MA3030001')%>%
  merge(salem_bev,all=T)%>%
  mutate(PWS.ID=y)%>%
  select(-y)
  
#merge data back into original dataset
wq_loc_sum2<-wq_loc_sum2%>%
  filter(PWS.ID!='MA4244001')%>%#remove the joint holbrook/randolph PWS
  filter(PWS.ID!='MA3030001')%>%#remove the joint salem/beverly PWS
  rbind(hr)%>%
  rbind(sb)%>%
  merge(mwra,all=T)%>%
  merge(swsc,all=T)


#Below we take a different approach, assigning WQ data only to the PWS we can identify it to

#filter sources to be ground waer
pws_sources<-pws_sources%>%
  filter(TYPE!='SW')%>%
  select(SOURCE_ID,SITE_NAME,TOWN,LATITUDE,LONGITUDE)%>%
  mutate(SOURCE_ID_merge=SOURCE_ID,
         SOURCE_ID=gsub('-0','-',SOURCE_ID),
         SITE_NAME=gsub('#','',SITE_NAME),
         SITE_NAME=gsub(' RAW WATER','',SITE_NAME))%>%
  clean_names()

#create a second EEA WQ database
wq_data2<-wq_data

#Holbrook/Randolph
holbrook_randolph<-c('MA4133000','MA4244000') 
hr<-wq_data2%>%
  ungroup()%>%
  filter(PWS.ID=='MA4244001')%>%
  merge(holbrook_randolph,all=T)%>%
  mutate(PWS.ID=y)%>%
  select(-y)

#Salem/Beverly
salem_bev<-c('MA3258000','MA3030000') 
sb<-wq_data2%>%
  ungroup()%>%
  filter(PWS.ID=='MA3030001')%>%
  merge(salem_bev,all=T)%>%
  mutate(PWS.ID=y)%>%
  select(-y)

#For MWRA and springfield we create a list of PWSs within the larger umbrella PWS
mwra_swsc<-swsc_pws%>%
  as.data.frame()%>%
  mutate(PWS.ID="MA1281000")%>%
  rbind(mwra_pws%>%
          as.data.frame()%>%
          mutate(PWS.ID='MA6000000'))
colnames(mwra_swsc)<-c("PWSID2",'PWS.ID')

#Replace the Holbrook/Randolph and Salem/Beverly PWSs, with duplicate tets for each of the smaller PWSs in the water quality database
wq_data2<-wq_data2%>%
  filter(PWS.ID!='MA4244001')%>%
  filter(PWS.ID!='MA3030001')%>%
  rbind(hr)%>%
  rbind(sb)%>%
  merge(mwra_swsc,all=T)%>%#merge with different PWSs, so all the Springfield and MWRA water quality results are repeated for each PWS within the larger supplier
  mutate(PWS.ID=ifelse(is.na(PWSID2),PWS.ID,PWSID2))%>%
  select(-PWSID2)
  
#Clean and merge this dataset the same as above
pwss<-wq_data2%>%
  mutate(source_id=paste(gsub('MA','',PWS.ID),`Location ID`,sep='-'),
         `Chemical Name`=ifelse(`Chemical Name`=='ALKALINITY (CACO3), TOTAL','ALKALINITY-TOTAL (CACO3)',`Chemical Name`))%>%
  group_by(PWS.ID,`Chemical Name`,`Raw or Finished`)%>%
  mutate(max_year=max(year),
         max_month=max(month[year==max_year]),
         max_day=max(day[year==max_year&month==max_month]))%>%
  dplyr::select(-`Location ID`)%>%
  summarise(n_results=sum(!is.na(Result)),
            source_mean=mean(Result),
            recent_result=mean(Result[day==max_day&year==max_year&month==max_month],na.rm=T))%>%
  ungroup()%>%
  filter(`Chemical Name`=='ALKALINITY-TOTAL (CACO3)'|`Chemical Name`=='ARSENIC'|`Chemical Name`=='CHLORIDE'|
           `Chemical Name`=='MANGANESE'|`Chemical Name`=='NITRATE'|`Chemical Name`=='NITRITE'|
           `Chemical Name`=='PH'|`Chemical Name`=='URANIUM'|`Chemical Name`=="BROMODICHLOROMETHANE"|`Chemical Name`=="CHROMIUM"|
           `Chemical Name`=="DIBROMOCHLOROMETHANE"|`Chemical Name`=="CHLOROFORM"|
           `Chemical Name`=="BROMOFORM"|`Chemical Name`=="TRICHLOROACETIC ACID"|
           `Chemical Name`=="DICHLOROACETIC ACID"|`Chemical Name`=="BROMATE"|
           `Chemical Name`=="RADIUM - 226"|`Chemical Name`=="RADIUM - 228"|
           `Chemical Name`=="DIBROMOCHLOROPROPANE"|`Chemical Name`=="1,2,3-TRICHLOROPROPANE"|
           `Chemical Name`=="TETRACHLOROETHYLENE"|`Chemical Name`=="1,4-DIOXANE"|
           `Chemical Name`=="TRICHLOROETHYLENE"|`Chemical Name`=="PERFLUOROOCTANOIC ACID-PFOA"|
           `Chemical Name`=="PERFLUOROOCTANESULFONIC ACID-PFOS"|`Chemical Name`=="PFAS6"|
           `Chemical Name`=="PERFLUORONONANOIC ACID-PFNA"|`Chemical Name`=="PERFLUOROHEXANESULFONIC ACID-PFHXS"|
           `Chemical Name`=="PERFLUOROHEPTANOIC ACID-PFHPA"|`Chemical Name`=="PERFLUORODECANOIC ACID - PFDA"
  )%>%
  pivot_wider(names_from=c(`Chemical Name`),values_from =c(n_results,source_mean,recent_result),names_glue = "{`Chemical Name`}_{.value}")%>%
  filter(`Raw or Finished`=='F')%>%
  clean_names()%>%
  mutate(source='EEA PWS')%>%
  clean_names()%>%
  mutate(source_id=pws_id)%>%
  select(-raw_or_finished,-pws_id)%>%
  merge(PWS%>%
          clean_names()%>%
          mutate(source_id=pws_id)%>%
          select(source_id,pws_type,primary_source,population_served_count,city_name,gw_or_sw_code,is_school_or_daycare),all=T)



#Here we create a database of only raw GW samples and GW samples from NTNCs
raw_gw_and_ntncs<-wq_loc_sum2%>%
  mutate(source='EEA')%>%#source of this database is from the EEA portal
  clean_names()%>%
  merge(water_quality_all_wells%>%#merge with USGS data
          ungroup()%>%
          mutate(source_id=station_number,
                 raw_or_finished='R')%>% #all USGS data is raw
          select(-station_number),
        all=T)%>%
  merge(PWS%>%#merge with information for each PWS from SDWIS
          clean_names()%>%
          mutate(source_id=pws_id)%>%
          select(pws_id,pws_type,primary_source,population_served_count,city_name,gw_or_sw_code,is_school_or_daycare),all=T)%>%
  mutate(gw_or_sw_code=ifelse(source=="USGS",'GW',gw_or_sw_code),#format all USGS data similarly to the cleaned EEA by indicating it is groundwater, not a PWS, etc.
         gw=ifelse(source=="USGS",'Y',gw),
         pws_type=ifelse(source=="USGS",'NOT PWS',pws_type))%>%
  filter(gw_or_sw_code!='SW' & gw!='N')%>% #remove all potential surface sources
  filter(raw_or_finished=='R'|pws_type!='Community water system')%>% #reoved potential finished water from CWSs
  mutate(town=ifelse(is.na(town),city_name,town))%>%
  select(-city_name,-gw,-location_name)%>%
  mutate_all(~ifelse(is.nan(.), NA, .))%>%
  as.data.frame()%>%
  #assign clean names for the source type
  mutate(source=ifelse(source=='EEA'&raw_or_finished=='F','EEA Finished NTNC or TNC',source),
         source=ifelse(source=='EEA'&raw_or_finished=='R','EEA Raw GW',source),
         latitude=as.numeric(latitude),
         longitude=as.numeric(longitude))


#below is a dataset that summarises the raw results at each well location and finished results at each CWS across all years, including pre-2010
pw_vs_pws_all_years<-wq_loc_sum2%>%
  #below we merge and clean the data the same as above, except we don't remove finished water from CWSs
  mutate(source='EEA')%>%
  clean_names()%>%
  merge(water_quality_all_wells%>%
          ungroup()%>%
          mutate(source_id=station_number,
                 raw_or_finished='R')%>%
          select(-station_number),
        all=T)%>%
  merge(PWS%>%
          clean_names()%>%
          mutate(source_id=pws_id)%>%
          select(pws_id,pws_type,primary_source,population_served_count,city_name,gw_or_sw_code,is_school_or_daycare),all=T)%>%
  mutate(gw_or_sw_code=ifelse(source=="USGS",'GW',gw_or_sw_code),
         gw=ifelse(source=="USGS",'Y',gw),
         pws_type=ifelse(source=="USGS",'NOT PWS',pws_type))%>%
  filter((gw_or_sw_code!='SW' & gw!='N')|(raw_or_finished=='F'&pws_type=='Community water system'))%>%#only select surface water if it is finished from a CWS
  mutate(town=ifelse(is.na(town),city_name,town))%>%
  select(-city_name,-gw,-location_name)%>%
  mutate_all(~ifelse(is.nan(.), NA, .))%>%
  as.data.frame()%>%
  #assign clean names for the source type
  mutate(source=ifelse(source=='EEA'&raw_or_finished=='F'& pws_type=='Community water system','EEA Finished CWS',source),
         source=ifelse(source=='EEA'&raw_or_finished=='F'& pws_type!='Community water system','EEA Finished NTNC or TNC',source),
         source=ifelse(source=='EEA'&raw_or_finished=='R','EEA Raw GW',source),
         latitude=as.numeric(latitude),
         longitude=as.numeric(longitude))


#write this datset to be used in GIS
openxlsx::write.xlsx(pw_vs_pws_all_years,"Data\\gw_pts.xlsx",sheetName="Sheet1")


#create the same USGS dataset as previously created, except only with data after 2010
water_quality_all_wells<-usgs_wq%>%
  mutate(begin_date=as.Date(begin_date,"%m/%d/%Y"),
         year=year(begin_date),
         month=month(begin_date),
         day=day(begin_date))%>%
  filter(year>=2010)%>%
  group_by(station_number)%>%
  mutate(max_year=max(year),
         max_month=max(month[year==max_year]),
         max_day=max(day[year==max_year&month==max_month]))%>%
  ungroup()%>%
  mutate(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen=ifelse(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen=='',nitrate_plus_nitrite_water_unfiltered_milligrams_per_liter_as_nitrogen,nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen))%>%
  group_by(station_number)%>%
  summarise(nitrate_n_results=sum(!is.na(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen))),
            nitrate_source_mean=mean(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen),na.rm=T),
            nitrate_recent_result=mean(as.numeric(nitrate_water_unfiltered_milligrams_per_liter_as_nitrogen[day==max_day&year==max_year&month==max_month],na.rm=T)),
            ph_n_results=sum(!is.na(as.numeric(p_h_water_unfiltered_laboratory_standard_units))),
            ph_source_mean=mean(as.numeric(p_h_water_unfiltered_laboratory_standard_units),na.rm=T),
            alkalinity_total_caco3_n_results=sum(!is.na(as.numeric(acid_neutralizing_capacity_water_unfiltered_fixed_endpoint_p_h_4_5_titration_field_milligrams_per_liter_as_calcium_carbonate))),
            alkalinity_total_caco3_source_mean=mean(as.numeric(acid_neutralizing_capacity_water_unfiltered_fixed_endpoint_p_h_4_5_titration_field_milligrams_per_liter_as_calcium_carbonate),na.rm=T),
            manganese_n_results=sum(!is.na(as.numeric(manganese_water_unfiltered_recoverable_micrograms_per_liter))),
            manganese_source_mean=mean(as.numeric(manganese_water_unfiltered_recoverable_micrograms_per_liter),na.rm=T),
            arsenic_n_results=sum(!is.na(as.numeric(arsenic_water_filtered_micrograms_per_liter))),
            arsenic_source_mean=mean(as.numeric(arsenic_water_filtered_micrograms_per_liter),na.rm=T),
            arsenic_recent_result=mean(as.numeric(arsenic_water_filtered_micrograms_per_liter[day==max_day&year==max_year&month==max_month],na.rm=T)),
            uranium_n_results=sum(!is.na(as.numeric(uranium_natural_water_filtered_micrograms_per_liter))),
            uranium_source_mean=mean(as.numeric(uranium_natural_water_filtered_micrograms_per_liter),na.rm=T),
            source='USGS')%>%
  merge(usgs_sites%>%
          mutate(latitude=dec_lat_va,
                 longitude=dec_long_va,
                 station_number=site_no)%>%
          select(station_number,latitude,longitude),all.x=T)%>%
  merge(usgs_well_coords,by=c("latitude","longitude"),all.x=T)

#same as in previous section, except with only post 2010 USGS data
raw_gw_and_ntncs<-wq_loc_sum2%>%
  mutate(source='EEA')%>%
  clean_names()%>%
  merge(water_quality_all_wells%>%
          ungroup()%>%
          mutate(source_id=station_number,
                 raw_or_finished='R')%>%
          select(-station_number),
        all=T)%>%
  merge(PWS%>%
          clean_names()%>%
          mutate(source_id=pws_id)%>%
          select(pws_id,pws_type,primary_source,population_served_count,city_name,gw_or_sw_code,is_school_or_daycare),all=T)%>%
  mutate(gw_or_sw_code=ifelse(source=="USGS",'GW',gw_or_sw_code),
         gw=ifelse(source=="USGS",'Y',gw),
         pws_type=ifelse(source=="USGS",'NOT PWS',pws_type))%>%
  filter(gw_or_sw_code!='SW' & gw!='N')%>%
  filter( raw_or_finished=='R'|pws_type!='Community water system')%>%
  mutate(town=ifelse(is.na(town),city_name,town))%>%
  select(-city_name,-gw,-location_name)%>%
  mutate_all(~ifelse(is.nan(.), NA, .))%>%
  as.data.frame()%>%
  mutate(source=ifelse(source=='EEA'&raw_or_finished=='F','EEA Finished NTNC or TNC',source),
         source=ifelse(source=='EEA'&raw_or_finished=='R','EEA Raw GW',source),
         latitude=as.numeric(latitude),
         longitude=as.numeric(longitude))


#Same dataset as pw_vs_pws_all_years, but with only post 2010 data
pw_vs_pws<-wq_loc_sum2%>%
  mutate(source='EEA')%>%
  clean_names()%>%
  merge(water_quality_all_wells%>%
          ungroup()%>%
          mutate(source_id=station_number,
                 raw_or_finished='R')%>%
          select(-station_number),
        all=T)%>%
  merge(wq_data%>%
          ungroup()%>%
          mutate(Town2=Town)%>%
          select(PWS.ID,Town2)%>%
          clean_names()%>%
          distinct(),all.x=T)%>%
  mutate(town=ifelse(is.na(town),town2,town))%>%
  merge(PWS%>%
          clean_names()%>%
          mutate(source_id=pws_id)%>%
          select(pws_id,pws_type,primary_source,population_served_count,city_name,gw_or_sw_code,is_school_or_daycare),all=T)%>%
  mutate(gw_or_sw_code=ifelse(source=="USGS",'GW',gw_or_sw_code),
         gw=ifelse(source=="USGS",'Y',gw),
         pws_type=ifelse(source=="USGS",'NOT PWS',pws_type))%>%
  filter((gw_or_sw_code!='SW' & gw!='N')|(raw_or_finished=='F'&pws_type=='Community water system'))%>%
  #filter(raw_or_finished=='R'|pws_type!='Community water system')%>%
  mutate(town=ifelse(is.na(town),city_name,town))%>%
  select(-city_name,-gw,-location_name)%>%
  mutate_all(~ifelse(is.nan(.), NA, .))%>%
  as.data.frame()%>%
  mutate(source=ifelse(source=='EEA'&raw_or_finished=='F'& pws_type=='Community water system','EEA Finished CWS',source),
          source=ifelse(source=='EEA'&raw_or_finished=='F'& pws_type!='Community water system','EEA Finished NTNC or TNC',source),
         source=ifelse(source=='EEA'&raw_or_finished=='R','EEA Raw GW',source),
         latitude=as.numeric(latitude),
         longitude=as.numeric(longitude))


#export this dataset for use in ArcGIS
openxlsx::write.xlsx(pw_vs_pws,"Data\\gw_pts_post_2010.xlsx",sheetName="Sheet1")



#Model Evaluation----
#after creating maps in GIS, this dataset represents the predicted and actual values for water quality parameters at each well site in our dataaset
#this is the training data (wells after 2010)

gw_extracted<-read.csv("Data\\wells_training_as.csv")%>%
  filter(source!='EEA Finished CWS')%>%#filter out CWS data
  mutate(as_int_lci=as_int-1.96*as_int_se,#estimate interpolated map upper and lower confidence intervals, based on interpolation
         as_int_lci=ifelse(as_int_lci<0,0,as_int_lci),
         as_int_uci=as_int+1.96*as_int_se)%>%
  merge(pw_vs_pws%>%
  select(source_id)%>%
  distinct()%>%
  mutate(training='Y'),all.x=T)%>%#set all this data as training data
  mutate(training=ifelse(is.na(training),'N',training),
         arsenic_so=ifelse(source=='USGS',arsenic_so/1000,arsenic_so))%>%
  #we assign indicator variables as_over_1, as_over_5, and as_over_10 based on weather the average
  #arsenic (arsenic_so) at that location is above the value
  #we then do the same on the probabilty map predictions, based on the percent likelihood it
  #is over the threshold of 1, 5, 10. We do the same for the 95th confidence by taking the likleihood at the upper confidence intervale (_ci)
  mutate(as_over_1=ifelse(arsenic_so>=0.001,'Y','N'),
         as_over_5=ifelse(arsenic_so>=0.005,'Y','N'),
         as_over_10=ifelse(arsenic_so>=0.01,'Y','N'),
         as_over_1_pred=ifelse(as_1_prob>0.5,'Y','N'),
         as_over_5_pred=ifelse(as_5_prob>0.5,'Y','N'),
         as_over_10_pred=ifelse(as_10_prob>0.5,'Y','N'),
         as_over_1_pred_ci=case_when(as_1_lci<0.5&as_1_uci<0.5~'N',as_1_lci<0.5&as_1_uci>=0.5~'Y or N',as_1_lci>0.5&as_1_uci>0.5~'Y'),
         as_over_5_pred_ci=case_when(as_5_lci<0.5&as_5_uci<0.5~'N',as_5_lci<0.5&as_5_uci>=0.5~'Y or N',as_5_lci>0.5&as_5_uci>0.5~'Y'),
         as_over_10_pred_ci=case_when(as_10_lci<0.5&as_10_uci<0.5~'N',as_10_lci<0.5&as_10_uci>=0.5~'Y or N',as_10_lci>0.5&as_10_uci>0.5~'Y'),
         as_over_1_ci=ifelse(as_over_1_pred_ci=='Y or N',as_over_1,as_over_1_pred_ci),
         as_over_5_ci=ifelse(as_over_5_pred_ci=='Y or N',as_over_5,as_over_5_pred_ci),
         as_over_10_ci=ifelse(as_over_10_pred_ci=='Y or N',as_over_10,as_over_10_pred_ci))


#exact confusion matricies indicating whether predictions are correct
confusionMatrix(as_ext$as_over_1_pred%>%as.factor(),as_ext$as_over_1%>%
                  as.factor())
confusionMatrix(as_ext$as_over_5_pred%>%as.factor(),as_ext$as_over_5%>%
                  as.factor())
confusionMatrix(as_ext$as_over_10_pred%>%as.factor(),as_ext$as_over_10%>%
                  as.factor())


#95th confidence confusion matricies indicating whether predictions are correct
confusionMatrix(as_ext$as_over_1_ci%>%as.factor(),as_ext$as_over_1%>%
                  as.factor())
confusionMatrix(as_ext$as_over_5_ci%>%as.factor(),as_ext$as_over_5%>%
                  as.factor())
confusionMatrix(as_ext$as_over_10_ci%>%as.factor(),as_ext$as_over_10%>%
                  as.factor())

#below we create a database indicating if each well is actually above the threshold, if it is predicted to be above the threshold (at .5 likelihood), and if the 95th confidence interval is above the threshold
as_mod_accuracy1=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_1_pred_ci)%>%
  select(source,contains('1'),-contains('10'))%>%
  mutate(threshold='1')
colnames(as_mod_accuracy1)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy5=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_5_pred_ci)%>%
  select(source,contains('5'))%>%
  mutate(threshold='5')
colnames(as_mod_accuracy5)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy10=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_10_pred_ci)%>%
  select(source,contains('10'))%>%
  mutate(threshold='10')
colnames(as_mod_accuracy10)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy<-rbind(as_mod_accuracy1,as_mod_accuracy5,as_mod_accuracy10)


#Below we calculate true positives, true negatives, false positives, false negatives
#and associated sensitivities, accuracy, specificity, for the predictions made at each threshold
#Bth for the total predictions, but also split into predictions at each source subset (USGS wells, Raw GW, Finished NTNC and TNC)
as_acc_metrics1<-as_mod_accuracy%>%
  mutate(source='Total')%>%
  group_by(source,threshold)%>%
  summarise(TP=sum(actual_over=='Y'&pred_over=='Y'),
            TP_ci=sum(actual_over=='Y'&ci_over=='Y'),
            FP=sum(actual_over=='N'&pred_over=='Y'),
            FP_ci=sum(actual_over=='N'&ci_over=='Y'),
            TN=sum(actual_over=='N'&pred_over=='N'),
            TN_ci=sum(actual_over=='N'&ci_over=='N'),
            FN=sum(actual_over=='Y'&pred_over=='N'),
            FN_ci=sum(actual_over=='Y'&ci_over=='N'),
            sensitivity=TP/(TP+FN),
            sensitivity_ci=TP_ci/(TP_ci+FN_ci),
            specificity=TN/(TN+FP),
            specificity_ci=TN_ci/(TN_ci+FP_ci),
            accuracy=(TP+TN)/(TP+TN+FP+FN),
            accuracy_ci=(TP_ci+TN_ci)/(TP_ci+TN_ci+FP_ci+FN_ci))

as_acc_metrics<-as_mod_accuracy%>%
  group_by(source,threshold)%>%
  summarise(TP=sum(actual_over=='Y'&pred_over=='Y'),
            TP_ci=sum(actual_over=='Y'&ci_over=='Y'),
            FP=sum(actual_over=='N'&pred_over=='Y'),
            FP_ci=sum(actual_over=='N'&ci_over=='Y'),
            TN=sum(actual_over=='N'&pred_over=='N'),
            TN_ci=sum(actual_over=='N'&ci_over=='N'),
            FN=sum(actual_over=='Y'&pred_over=='N'),
            FN_ci=sum(actual_over=='Y'&ci_over=='N'),
            sensitivity=TP/(TP+FN),
            sensitivity_ci=TP_ci/(TP_ci+FN_ci),
            specificity=TN/(TN+FP),
            specificity_ci=TN_ci/(TN_ci+FP_ci),
            accuracy=(TP+TN)/(TP+TN+FP+FN),
            accuracy_ci=(TP_ci+TN_ci)/(TP_ci+TN_ci+FP_ci+FN_ci))%>%
  rbind(as_acc_metrics1)

#function for calculating R2
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}



as_error=as_ext%>%#extracted GW As dataset
  mutate(arsenic_so=arsenic_so*1000,#mutate so that data is in ug/L
         as_int=1000*as_int)%>%
  mutate(training=ifelse(training=='N','USGS Wells not used in interpolation (pre 2010)','Interpolated Data'))%>%#indicate this is training data
  filter(as_int>=0)%>%#remove all any undefined extractions (set as -9999)
  filter(!is.na(arsenic_so))#filter out points with no data

#USGS pre 2010 wells R squared (testing data)
R2_as_tr = round(RSQUARE(as_error$arsenic_so[as_error$training=='USGS Wells not used in interpolation (pre 2010)'],as_error$as_int[as_error$training=='USGS Wells not used in interpolation (pre 2010)']),2)

#all other wells R squared (training data)
R2_as_te = round(RSQUARE(as_error$arsenic_so[as_error$training=='Interpolated Data'],as_error$as_int[as_error$training=='Interpolated Data']),2)

#figure labels
as_error_lab=as_error%>%
  select(training)%>%
  distinct()%>%
  mutate(n=paste('R2=',ifelse(training=='USGS Wells not used in interpolation (pre 2010)',R2_as_tr,R2_as_te),sep=''))

asw1<-as_ext%>%
  mutate(arsenic_so=arsenic_so*1000,
         as_int=1000*as_int)%>%
  mutate(training=ifelse(training=='N','USGS Wells not used in interpolation (pre 2010)','Interpolated Data'))%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int,color=source))+
  geom_text(data=as_error_lab,aes(y=400,x=10,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=19),legend.position = "bottom",legend.title = element_blank())

asw2<-as_ext%>%
  mutate(arsenic_so=arsenic_so*1000,
         as_int=1000*as_int)%>%
  mutate(training=ifelse(training=='N','USGS Wells not used in interpolation (pre 2010)','Interpolated Data'))%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int,color=source))+
  geom_text(data=as_error_lab,aes(y=250,x=30,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=19),legend.position = "bottom",legend.title = element_blank())


png("Data\\Figures\\as_well_pred2.png",height = 10,width=10,res=300,units = "in")
ggarrange(asw1,asw2,
          ncol = 1, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

#Probabilty Map Performance Figures
png("Data\\Figures\\as_well_pred_bars.png",height = 8,width=12,res=300,units = "in")
as_acc_metrics%>%
  group_by(threshold,source)%>%
  summarise(Total=sum(FN,TP),
            Correct=TP,
            Incorrect=FN)%>%
  mutate(Actual='Above')%>%
  rbind(as_acc_metrics%>%
          group_by(threshold,source)%>%
          summarise(Total=sum(TN,FP),
            Correct=TN,
            Incorrect=FP)%>%
  mutate(Actual='Below'))%>%
  mutate(Correct=100*Correct/Total,
         Incorrect=100*Incorrect/Total,
         Total=paste('n=',Total,sep=""),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")),
         Actual=as.factor(Actual),
         Actual=fct_relevel(Actual,"Below","Above"))%>%
  pivot_longer(cols=c('Correct','Incorrect'))%>%
  mutate(name=as.factor(name),
         name=fct_relevel(name,"Incorrect","Correct"))%>%
  ggplot(aes(y=source,x=value,fill=name))+
  geom_bar(position='stack',stat='identity')+
  geom_label(aes(label=Total,x=50),fill='white',size=8)+
  scale_fill_manual(values = c("firebrick","skyblue"))+
  xlab('Percent')+
  ylab('')+
  facet_grid(Actual~threshold)+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "bottom",legend.title = element_blank())
dev.off()



png("Data\\Figures\\as_well_pred_bars_ci.png",height = 8,width=12,res=300,units = "in")
as_acc_metrics%>%
  group_by(threshold,source)%>%
  summarise(Total=sum(FN_ci,TP_ci),
            Correct=TP_ci,
            Incorrect=FN_ci)%>%
  mutate(Actual='Above')%>%
  rbind(as_acc_metrics%>%
          group_by(threshold,source)%>%
          summarise(Total=sum(TN_ci,FP_ci),
                    Correct=TN_ci,
                    Incorrect=FP_ci)%>%
          mutate(Actual='Below'))%>%
  mutate(Correct=100*Correct/Total,
         Incorrect=100*Incorrect/Total,
         Total=paste('n=',Total,sep=""),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")),
         Actual=as.factor(Actual),
         Actual=fct_relevel(Actual,"Below","Above"))%>%
  pivot_longer(cols=c('Correct','Incorrect'))%>%
  mutate(name=as.factor(name),
         name=fct_relevel(name,"Incorrect","Correct"))%>%
  ggplot(aes(y=source,x=value,fill=name))+
  geom_bar(position='stack',stat='identity')+
  geom_label(aes(label=Total,x=50),fill='white',size=8)+
  scale_fill_manual(values = c("firebrick","skyblue"))+
  xlab('Percent')+
  ylab('')+
  facet_grid(Actual~threshold)+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "bottom",legend.title = element_blank())
dev.off()

#Below we export Accuracy Tables
as_acc_metrics%>%
  ungroup()%>%
  mutate(total_below=TN_ci+FP_ci,
         total_above=FN_ci+TP_ci,
         sensitivity=paste(round(100*sensitivity,1),"\\% (",round(100*sensitivity_ci,1),"\\%)",sep=""),
         specificity=paste(round(100*specificity,1),"\\% (",round(100*specificity_ci,1),"\\%)",sep=""),
         accuracy=paste(round(100*accuracy,1),"\\% (",round(100*accuracy_ci,1),"\\%)",sep=""),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")))%>%
  select(source,threshold,total_above,total_below,accuracy,sensitivity,specificity)%>%
  arrange(desc(source),threshold)%>%
  write.csv("Tables\\prob_maps_acc.csv",row.names = F,quote = F)


as_acc_metrics%>%
  ungroup()%>%
  mutate(total_below=TN_ci+FP_ci,
         total_above=FN_ci+TP_ci,
         sensitivity=paste(round(100*sensitivity,1),"\\% (",round(100*sensitivity_ci,1),"\\%)",sep=""),
         specificity=paste(round(100*specificity,1),"\\% (",round(100*specificity_ci,1),"\\%)",sep=""),
         accuracy=paste(round(100*accuracy,1),"\\% (",round(100*accuracy_ci,1),"\\%)",sep=""),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")))%>%
  select(source,threshold,total_above,total_below,accuracy,sensitivity,specificity)%>%
  arrange(desc(source),threshold)%>%
  filter(source=="Total")%>%
  select(-source)%>%
  write.csv("Tables\\prob_maps_acc_tot.csv",row.names = F,quote = F)



#These data sets below are the datasets of all residential parcels in MA served by privat wells
#and all residentialparcels served by PWSs, as described in Text S1

resi_wells<-read.csv("Data//ma_resi_parcels_wells.csv")%>%
  mutate(sm_water_source="Well")
resi_pws<-read.csv("Data//ma_resi_parcels_pws.csv")%>%
  mutate(sm_water_source="PWS")


#combined data set of all domestic parcels
dom_parcels<-resi_wells%>%
  rbind(resi_pws)


#this dataset is all the locations in the well viewer database with all the parcels
#that contain a well in the well driller database. This analysis was done in ArcGIS
well_loc<-read.csv("Data//well_parcels.csv")

#This merges the two sets together, with two separate columns indicating if the parcel is served by a well, using each method (PWS Maps and Well Driller Database)
dom_parcels_wells<-dom_parcels%>%
  merge(well_loc%>%
          mutate(well='Y')%>%
          select(well,LOC_ID)%>%
          distinct(),by="LOC_ID",all.x=T)%>%
  clean_names()%>%
  mutate(well=ifelse(is.na(well),'N',well),
         year_built=ifelse(year_built==0,NA,year_built)) #cleaning the year built field



counties<-read.csv("Data\\county-list.csv")%>% #list of towns by county in MA
  clean_names()%>%
  mutate(town=toupper(town20),
         county=toupper(county20))%>%
  select(town,county)%>%
  mutate(region=case_when(#Determining the MassDEP region for each town/county based on the county
    county=='WORCESTER'~'CERO',
    county=='BERKSHIRE'~'WERO',
    county=='FRANKLIN'~'WERO',
    county=='HAMPSHIRE'~'WERO',
    county=='HAMPDEN'~'WERO',
    county=='ESSEX'~'NERO',
    county=='MIDDLESEX'~'NERO',
    county=='SUFFOLK'~'NERO',
    county=='NORFOLK'~'SERO',
    county=='BRISTOL'~'SERO',
    county=='PLYMOUTH'~'SERO',
    county=='BARNSTABLE'~'SERO',
    county=='DUKES'~'SERO',
    county=='NANTUCKET'~'SERO'
  ),
  town=ifelse(town=='MANCHESTER-BY-THE-SEA','MANCHESTER',town)) #cleaning this town name


#Summry stats for well locations by method
pws_parcel_summmary<-dom_parcels_wells%>%
  summarise(Parcels=sum(!is.na(loc_id)),
            `Total Wells`=length(t(well_loc[,1])),
            `Parcels with Wells`=sum(well=='Y',na.rm=T),
            `Parcels in PWS Bounds`=sum(sm_water_source=='PWS',na.rm=T),
            `Parcels with Well and PWS`=sum(well=='Y'&sm_water_source=='PWS',na.rm=T),
            `Parcels with Neither a Well or PWS`=sum(well=='N'&sm_water_source=='Well',na.rm=T))

pws_parcel_summmary%>%
  mutate_each(funs(prettyNum(., big.mark=",")))%>%
  write.table("Tables/pws_parcel_summmary.csv", quote = FALSE,sep=";",row.names = F)


#Summary statistics of well locations by methods by town
town_parcels<-dom_parcels_wells%>%
  group_by(city)%>%
  summarise(parcels=sum(!is.na(loc_id)),
            wells=sum(well=='Y'),
            served_by_pws=sum(sm_water_source=='PWS',na.rm=T),
            well_and_pws=sum(well=='Y'&sm_water_source=='PWS',na.rm=T),
            neither=sum(well=='N'&sm_water_source=='Well',na.rm=T))%>%
  mutate(well_percent=round((wells-well_and_pws)/parcels,1),
         pws_percent=round(100*(served_by_pws-well_and_pws)/parcels,1),
         unsure=round(100*(well_and_pws+neither)/parcels,1))

write.csv(town_parcels,"Tables//town_water_sources.csv",row.names = F)

#Summary statistics of well locations by methods by county
county_parcels<-dom_parcels_wells%>%
  merge(counties%>%
          mutate(city=town,
                 County=county),all.x=T)%>%
  group_by(County)%>%
  summarise(parcels=sum(!is.na(loc_id)),
            wells=sum(well=='Y'),
            served_by_pws=sum(sm_water_source=='PWS',na.rm=T),
            well_and_pws=sum(well=='Y'&sm_water_source=='PWS',na.rm=T),
            neither=sum(well=='N'&sm_water_source=='Well',na.rm=T))%>%
  mutate(`Well Viewer Assigned Wells`=paste(round(100*(wells)/parcels,1),"",sep="%"),
         `Parcels Not in PWS Bounds`=paste(round(100*(1-((served_by_pws-well_and_pws))/parcels),1),"",sep="%"),
         `Well and PWS`=paste(round(100*(well_and_pws)/parcels,1),"",sep="%"),
         `No Well or PWS`=paste(round(100*(neither)/parcels,1),"",sep="%"),
         County=str_to_title(County),
         Parcels=prettyNum(parcels,big.mark=","))%>%
  select(-wells,-served_by_pws,-well_and_pws,-neither)

write.table(county_parcels%>%
              mutate(County=str_to_title(County)),"Tables\\well_assignment_county_pres.csv",row.names = F,quote = F,sep=",")


#CENSUS CLEANING----

#Below we call in different Census datasets and clean them into one large dataset with statistics by town

#INCOME
income<-read.csv("Data/Census/ACSST5Y2020.S1901-2023-04-22T171611.csv")%>%
  clean_names()
rownames<-income[1]%>%t()%>%as.data.frame()%>%clean_names()# set rownames as the first row in the dataset (all the towns)
#clean the names below
colnames(rownames)<-rownames
rownames<-colnames(rownames%>%clean_names())
rownames<-sub("*a_a_a_", "", rownames) 
rownames<-sub(".*_a_a_a_a_", "", rownames)
rownames<-sub(".*a_a_a_a_", "", rownames)

#only selecting the estimates for each town
income<-income%>%
  dplyr::select(ends_with("massachusetts_households_estimate"))%>%
  dplyr::select(!starts_with("county"))#removing county average

#cleaning names to only be town name
colnames<-colnames(income)
colnames<-sub("_city.*", "", colnames)      
colnames<-sub("_town.*", "", colnames)   
colnames<-gsub("_"," ",colnames)

colnames(income)<-colnames
rownames(income)<-rownames
income<-income%>%t()%>%as.data.frame()
income<-income%>%
  mutate(CITY=toupper(rownames(income)),
         CITY=ifelse(CITY=="MANCHESTER BY THE SEA","MANCHESTER",CITY)) #clean town name

#Now we do the same for several different census datasets

#POVERTY LEVEL
poverty<-read.csv("Data/Census/ACSST5Y2020.S1701-2023-04-22T172050.csv")%>%
  clean_names()
rownames<-poverty[1]%>%t()%>%as.data.frame()%>%clean_names()
colnames(rownames)<-rownames
rownames<-colnames(rownames%>%clean_names())
rownames<-sub("*a_a_a_a_", "", rownames) 

poverty<-poverty%>%
  dplyr::select(ends_with("percent_below_poverty_level_estimate"))%>%
  dplyr::select(!starts_with("county"))

colnames<-colnames(poverty)
colnames<-sub("_city.*", "", colnames)      
colnames<-sub("_town.*", "", colnames)   
colnames<-gsub("_"," ",colnames)

colnames(poverty)<-colnames
rownames(poverty)<-rownames
poverty<-poverty%>%t()%>%as.data.frame()
poverty<-poverty%>%
  mutate(CITY=toupper(rownames(poverty)),
         CITY=ifelse(CITY=="MANCHESTER BY THE SEA","MANCHESTER",CITY))%>%
  dplyr::select(population_for_whom_poverty_status_is_determined,CITY)

#MED H AGE
med_h_age<-read.csv("Data/Census/ACSDT5Y2020.B25035-2023-04-22T164934.csv")%>%
  clean_names()
rownames<-med_h_age[1]%>%t()%>%as.data.frame()%>%clean_names()
colnames(rownames)<-rownames
rownames<-colnames(rownames%>%clean_names())

med_h_age<-med_h_age%>%
  dplyr::select(ends_with("_massachusetts_estimate"))%>%
  dplyr::select(!starts_with("county"))

colnames<-colnames(med_h_age)
colnames<-sub("_city.*", "", colnames)      
colnames<-sub("_town.*", "", colnames)   
colnames<-gsub("_"," ",colnames)
rownames<-sub(".*_a_a_a_a_", "", rownames)
rownames<-sub(".*a_a_a_a_", "", rownames)

colnames(med_h_age)<-colnames
rownames(med_h_age)<-rownames
med_h_age<-med_h_age%>%t()%>%as.data.frame()
med_h_age<-med_h_age%>%
  mutate(CITY=toupper(rownames(med_h_age)),
         CITY=ifelse(CITY=="MANCHESTER BY THE SEA","MANCHESTER",CITY))
#Race
race<-read.csv("Data/Census/ACSDP5Y2020.DP05-2023-04-22T170421.csv")%>%
  clean_names()%>%
  distinct()
race<-rbind(race[2:19,],race[2:19,],race[63:68,],race[70,],race[75,])
rownames<-race[1]%>%t()%>%as.data.frame()%>%clean_names()
colnames(rownames)<-rownames
rownames<-colnames(rownames%>%clean_names())

race<-race%>%
  dplyr::select(ends_with("_massachusetts_estimate"))%>%
  dplyr::select(!starts_with("county"))

colnames<-colnames(race)
colnames<-sub("_city.*", "", colnames)      
colnames<-sub("_town.*", "", colnames)
rownames<-sub(".*_a_a_a_a_", "", rownames)
rownames<-sub(".*a_a_a_a_", "", rownames)
colnames<-gsub("_"," ",colnames)

colnames(race)<-colnames
rownames(race)<-rownames
race<-race%>%t()%>%as.data.frame()
race<-race%>%
  mutate(CITY=toupper(rownames(race)),
         CITY=ifelse(CITY=="MANCHESTER BY THE SEA","MANCHESTER",CITY))

#HOUSING
housing<-read.csv("Data/Census/ACSDP5Y2020.DP04-2023-04-22T190638.csv")%>%
  clean_names()%>%
  distinct()
housing<-rbind(housing[2:6,],housing[18:27,],housing[48:58,])
rownames<-housing[1]%>%t()%>%as.data.frame()%>%clean_names()
colnames(rownames)<-rownames
rownames<-colnames(rownames%>%clean_names())

housing<-housing%>%
  dplyr::select(ends_with("_massachusetts_estimate"))%>%
  dplyr::select(!starts_with("county"))

colnames<-colnames(housing)
colnames<-sub("_city.*", "", colnames)      
colnames<-sub("_town.*", "", colnames)
rownames<-sub(".*_a_a_a_a_", "", rownames)
rownames<-sub(".*a_a_a_a_", "", rownames)
colnames<-gsub("_"," ",colnames)

colnames(housing)<-colnames
rownames(housing)<-rownames
housing<-housing%>%t()%>%as.data.frame()
housing<-housing%>%
  mutate(CITY=toupper(rownames(housing)),
         CITY=ifelse(CITY=="MANCHESTER BY THE SEA","MANCHESTER",CITY))

#CLEAN all the census data and combine into one dataset
housing2<-housing%>%
  dplyr::select(-homeowner_vacancy_rate,-rental_vacancy_rate,-average_household_size_of_owner_occupied_unit,-average_household_size_of_renter_occupied_unit,-year_householder_moved_into_unit)
housing2[1:21]=lapply(housing2[1:21],function(x){ as.numeric(as.character(gsub(",", "", x))) })
housing2[,2:21]=housing2[,2:21]/housing2[,1]

#RACE

race2<-race%>%
  dplyr::select(-ends_with("2"))%>%
  dplyr::select(-sex_ratio_males_per_100_females)

race2[1:25]=lapply(race2[1:25],function(x){ as.numeric(as.character(gsub(",", "", x))) })
race2[,2:16]=race2[,2:16]/race2[,1]
race2[,18:25]=race2[,18:25]/race2[,1]

#POVERTY

poverty2<-poverty
poverty2[1]=lapply(poverty2[1],function(x){ as.numeric(as.character(gsub("%", "", x)))/100})

#INCOME

income2<-income%>%
  dplyr::select(-total,-household_income_in_the_past_12_months,-family_income_in_the_past_12_months,-nonfamily_income_in_the_past_12_months,-percent_allocated)
income2[1:10]=lapply(income2[1:10],function(x){ as.numeric(as.character(gsub("%", "", x)))/100})
income2[11:12]=lapply(income2[11:12],function(x){ as.numeric(as.character(gsub(",", "", x)))})

#COMBINING----
censusdata<-race2%>%
  merge(income2,all=T)%>%
  merge(poverty2,all=T)%>%
  merge(housing2,all=T)%>%
  merge(med_h_age,all=T)
colnames<-colnames(censusdata)
colnames<-sub(".*a_", "", colnames)
colnames(censusdata)<-colnames

#Export fully cleaned census dataset
censusdata%>%
  write.csv("Data\\census_data.csv")



#TOWN ANALYSIS----

#This calculates the PWS exposure to different contaminants by town,
#combining towns served by multiple CWSs into one
pws_groups<-pwss%>%
  filter(!is.na(population_served_count))%>%
  mutate(town=city_name,
         population_served_count=gsub(',','',population_served_count),#cleaning number format
         population_served_count=as.numeric(population_served_count))%>%
  group_by(town,pws_type)%>%#grouping by town and PWS type (CWS, NTNC, TNC)
  #Done for several contaminants of interest below
  mutate(total_no3_exp=nitrate_source_mean*population_served_count,#Calculate the total exposure to each contaminant by multiplying the dose by number of people served by the system
         total_as_exp=arsenic_source_mean*population_served_count,
         total_mn_exp=manganese_source_mean*population_served_count,
         total_cr_exp=chromium_source_mean*population_served_count,
         total_pfas6_exp=pfas6_source_mean*population_served_count)%>%
  summarise(total_no3_exposure=sum(total_no3_exp,na.rm=T),#Total townwide exposure is added between each PWS type
            pp_no3_exposure=sum(total_no3_exp,na.rm=T)/sum(population_served_count,na.rm=T),#Total exposure is then divided by the population served by that system type in the town to get an average
            total_as_exposure=sum(total_as_exp,na.rm=T),
            pp_as_exposure=sum(total_as_exp,na.rm=T)/sum(population_served_count,na.rm=T),
            total_mn_exposure=sum(total_mn_exp,na.rm=T),
            pp_mn_exposure=sum(total_mn_exp,na.rm=T)/sum(population_served_count,na.rm=T),
            total_cr_exposure=sum(total_cr_exp,na.rm=T),
            pp_cr_exposure=sum(total_cr_exposure,na.rm=T)/sum(population_served_count,na.rm=T),
            total_pfas6_exp=sum(total_pfas6_exp,na.rm=T),
            pp_pfas6_exposure=sum(total_pfas6_exp,na.rm=T)/sum(population_served_count,na.rm=T))

town_data_sum<-PWS%>%
  clean_names()%>%
  mutate(source_id=pws_id)%>%
  select(source_id,pws_type,primary_source,population_served_count,city_name,gw_or_sw_code,is_school_or_daycare)%>%
  filter(source_id!='MA6000000'&source_id!='MA3030001'&source_id!='MA4244001')%>%
  mutate(town=city_name,
         population_served_count=as.numeric(gsub(",","",population_served_count)),
         town=ifelse(town=='WEST YARMOUTH','YARMOUTH',town),
         population_served_count=ifelse(source_id=='MA1281000',population_served_count-16053-15358-28613-28391,population_served_count))%>%
  group_by(town,pws_type)%>%
  summarise(pwss=sum(!is.na(unique(source_id))),
            population_served=sum(population_served_count,na.rm=T))%>%
  filter(pws_type=="Non-Transient non-community system"|pws_type=="Community water system")%>%
  merge(pws_groups,all.x=T)%>%
  mutate(pws_type=ifelse(pws_type=='Non-Transient non-community system','NTNC','CWS'))%>%
  pivot_wider(names_from = pws_type,values_from = c(3:12))


census_data<-read.csv("Data\\census_data.csv")%>%
  clean_names()%>%
  mutate(town=city)
#Below is another dataset, the EEA Well Drilling Databese, without location data: https://eeaonline.eea.state.ma.us/portal#!/search/welldrilling
well_by_town<-read_xlsx("Data\\WellDrilling.xlsx")%>%
  clean_names()

#Database created for wells analysis
well_gs<-well_by_town%>%
  mutate(depthto_bedrock=as.numeric(depthto_bedrock),
         total_depth=as.numeric(total_depth),
         depth_below_bedrock=total_depth-depthto_bedrock,
         well_type=ifelse(depth_below_bedrock>=0,'Bedrock Well',NA),
         well_type=ifelse(depth_below_bedrock<0,'Not Bedrock Well',well_type))%>%
  filter(total_depth>0&total_depth<4000)%>%
  filter(depthto_bedrock>=0&depthto_bedrock<4000)

library(scales)

png("Data\\Figures\\well_depths.png",height = 3,width=5,res=300,units = "in")
well_gs%>%
  ggplot()+
  geom_histogram(aes(x=total_depth),bins=40,fill='skyblue2',color='black')+
  scale_x_log10()+
  scale_y_continuous(labels = comma)+
  ylab('Wells')+
  xlab('Depth (ft)')+
  theme_bw()
dev.off()

png("Data\\Figures\\well_bedrock_depths.png",height = 4,width=7,res=300,units = "in")
a<-well_gs%>%
  filter(well_type=='Bedrock Well')%>%
  ggplot()+
  geom_histogram(aes(x=total_depth),bins=40,fill='gold4',color='black')+
  scale_x_log10()+
  scale_y_continuous(labels = comma)+
  #scale_y_log10()+
  ylab('Wells')+
  xlab('Depth (ft)')+
  facet_wrap(~well_type)+
  theme_bw()
b<-well_gs%>%
  filter(well_type=='Not Bedrock Well')%>%
  ggplot()+
  geom_histogram(aes(x=total_depth),bins=20,fill='gold4',color='black')+
  scale_x_log10()+
  scale_y_continuous(labels = comma)+
  #scale_y_log10()+
  ylab('Wells')+
  xlab('Depth (ft)')+
  facet_wrap(~well_type)+
  theme_bw()
ggarrange(b,a)
dev.off()



#ARSENIC HEALTH ANALYSIS----
#Summarize stats for wells
well_by_town<-read_xlsx("Data\\WellDrilling.xlsx")%>%
  clean_names()

well_by_town_as<-well_by_town%>%
  filter(work_performed!="Decommission")%>%#Only active wells
  count(town)%>%
  mutate(wells_db=n)%>%#Count the number of wells per town in the EEA well database
  select(-n)%>%
  merge(dom_parcels%>%
          mutate(town=CITY)%>%
          count(town),all=T)%>%
  mutate(parcels=n)%>%#Count the number of parcels per town
  select(-n)%>%
  merge(well_loc%>%
          clean_names()%>%
          count(town),all=T)%>%
  mutate(wells_gdb=n)%>%#count the number of Well Driller Wells per town
  select(-n)%>%
  merge(census_data%>%#select relevant census data for each town
          select(town,total_population,male,female,under_5_years,x5_to_9_years,total_housing_units,x10_to_14_years,x15_to_19_years),all=T)%>%
  merge(pw_vs_pws%>%
          filter(!is.na(arsenic_source_mean)&source!='EEA Finished CWS')%>%#Only slect groundwater arsenic data
          mutate(arsenic_source_mean=ifelse(source=='USGS',arsenic_source_mean/1000,arsenic_source_mean),#mutate USGS data to be in the same units
                 arsenic_recent_result=ifelse(source=='USGS',arsenic_recent_result/1000,arsenic_recent_result))%>%
          group_by(town)%>%
          #below are summary statistics by town of the groundwater aresnic measurements in our database
          summarise(locations=sum(!is.na(town)),
                    locations_with_coords=sum(!is.na(latitude)),
                    usgs_sites=sum(source=='USGS'),
                    raw_water_sites=sum(source=='EEA Raw GW'),
                    finished_water_sites=sum(source=='EEA Finished NTNC or TNC'),
                    mean_usgs=mean(arsenic_source_mean[source=='USGS']),
                    mean_raw_eea=mean(arsenic_source_mean[source=='EEA Raw GW']),
                    mean_finished_ntnc=mean(arsenic_source_mean[source=='EEA Finished NTNC or TNC']),
                    mean_total=mean(arsenic_source_mean,na.rm=T),
                    percent_over_1_town=sum(arsenic_source_mean>0.001)/sum(!is.na(arsenic_source_mean)),
                    percent_over_5_town=sum(arsenic_source_mean>0.005)/sum(!is.na(arsenic_source_mean)),
                    percent_over_10_town=sum(arsenic_source_mean>0.01)/sum(!is.na(arsenic_source_mean))),all=T)%>%
  merge(town_data_sum,all=T)%>%
  filter(!is.na(total_population))%>%
  #Below we calculate the ingestion rate to bodyweight ratios for each Census group, based on
  #methods described in the manuscript
  mutate(adults=1-under_5_years-x5_to_9_years-x10_to_14_years-x15_to_19_years,
         under_five_ir_bw=(.595/7.8+.245/11.4+3*.337/17.4)/5,
         five_to_nine_ir_bw=(.337/17.4+4*.455/31.8)/5,
         ten_to_fourteen_ir_bw=.562/56.8,
         fifteen_to_nineteen_ir_bw=(.562/56.8+4*.722/71.6)/5,
         adults_ir_bw=1.313/80,
         EF_non_cancer=1,
         EF_cancer_children=21/78,
         EF_cancer_adults=33/78,
         #Below is the calculation for the average dose/contaminat concentration for each town
         d_c_noncancer=(under_5_years*under_five_ir_bw+x5_to_9_years*five_to_nine_ir_bw+x10_to_14_years*ten_to_fourteen_ir_bw+fifteen_to_nineteen_ir_bw*x15_to_19_years+adults_ir_bw*adults),
         d_c_cancer_children=(21/78)*(under_5_years*under_five_ir_bw+x5_to_9_years*five_to_nine_ir_bw+x10_to_14_years*ten_to_fourteen_ir_bw+fifteen_to_nineteen_ir_bw*x15_to_19_years),
         d_c_cancer_adults=(33/78)*(adults_ir_bw*adults))

#below are the estimated interpolated arsenic values for wells in the well driller database (per ArcGIS)
int_wq_as<-read.csv("Data\\wells_as.csv")%>%
  clean_names()%>%
  mutate(town=city)%>%
  select(town,c(119:129))%>% 
  replace_with_na(replace = list(as_int = -9999))

#below are the estimated interpolated arsenic values for wells outside of PWSs (per ArcGIS)
sm_int_wq_as<-read.csv("Data\\sm_wells_as.csv")%>%
  clean_names()%>%
  mutate(town=city)%>%
  select(town,starts_with("as"))%>% 
  replace_with_na(replace = list(as_int = -9999))

#import town centroid cordinates as calculated in ArcGIS
town_centroids<-read.csv("Data\\town_centroids.csv")%>%
  clean_names()%>%
  select(town,lat,lon)

town_as<-int_wq_as%>%#database with interpolated arsenic values for wells in the well driller database
  group_by(town)%>%
  summarise(assignment_method='gdb',
            #summary stats for the geodatabase wells
            #below are interpolated from the generated arsenic maps
            n_int=sum(!is.na(as_int)),
            over_1_int=sum(as_int>.001,na.rm=T)/n_int,
            over_5_int=sum(as_int>.005,na.rm=T)/n_int,
            over_10_int=sum(as_int>.010,na.rm=T)/n_int,
            as_int=mean(as_int,na.rm=T),
            #below are stats for the exctracted probabilities from probability maps
            n_modeled=sum(!is.na(as_1_prob)),
            over_1=sum(as_1_prob,na.rm=T)/n_modeled,
            over_5=sum(as_5_prob,na.rm=T)/n_modeled,
            over_10=sum(as_10_prob,na.rm=T)/n_modeled)%>%
  merge(well_by_town_as%>%
          select(town),all=T)%>%#merge with the full list of towns
  mutate(n_int=ifelse(is.na(n_int),0,n_int),
         n_modeled=ifelse(is.na(n_modeled),0,n_modeled),
         assignment_method='gdb')%>%
  rbind(sm_int_wq_as%>%
          group_by(town)%>%
          #do the same as above, except this time using the wells outside PWS boundaries that we estimated
          summarise(assignment_method='service_maps',
                    n_int=sum(!is.na(as_int)),
                    over_1_int=sum(as_int>.001,na.rm=T)/n_int,
                    over_5_int=sum(as_int>.005,na.rm=T)/n_int,
                    over_10_int=sum(as_int>.010,na.rm=T)/n_int,
                    as_int=mean(as_int,na.rm=T),
                    n_modeled=sum(!is.na(as_1_prob)),
                    over_1=sum(as_1_prob,na.rm=T)/n_modeled,
                    over_5=sum(as_5_prob,na.rm=T)/n_modeled,
                    over_10=sum(as_10_prob,na.rm=T)/n_modeled)%>%
          merge(well_by_town_as%>%
                  select(town),all=T)%>%
          mutate(n_int=ifelse(is.na(n_int),0,n_int),
                 n_modeled=ifelse(is.na(n_modeled),0,n_modeled),
                 assignment_method='service_maps'))%>%
  filter(town!='-9999')%>%
  merge(well_by_town_as,all.y=T)%>%
  merge(town_centroids,all.y=T)%>%
  mutate(mean_int_method=NA,
         mean_total_method=NA)

#below for towns that don't have values for interpolated maps (because they were outside map boundaries)
#we replace them with data from the town nearest to them, measured by centroid distances
not_missing<-town_as%>%
  filter(!is.na(as_int))

for (i in 1:length(as.data.frame(t(town_as)))){
  lat1<-town_as[i,'lat']
  lon1<-town_as[i,'lon']
  wa<-town_as[i,"assignment_method"]
  newdf<-not_missing%>%
    mutate(dist=sqrt((lat-lat1)^2+(lon-lon1)^2))%>%
    filter(dist==min(dist)&assignment_method==wa)
  town_as[i,]<-town_as[i,]%>%
    mutate(mean_int_method=ifelse(is.na(as_int),'Closest Neighbor','Calculated'))%>%
    mutate(as_int=ifelse(is.na(as_int),newdf$as_int,as_int))
}

not_missing<-town_as%>%
  filter(!is.na(mean_total))

for (i in 1:length(as.data.frame(t(town_as)))){
  lat1<-town_as[i,'lat']
  lon1<-town_as[i,'lon']
  wa<-town_as[i,"assignment_method"]
  newdf<-not_missing%>%
    mutate(dist=sqrt((lat-lat1)^2+(lon-lon1)^2))%>%
    filter(dist==min(dist)&assignment_method==wa)
  town_as[i,]<-town_as[i,]%>%
    mutate(mean_total_method=ifelse(is.na(mean_total),'Closest Neighbor','Calculated'),
           mean_total=ifelse(is.na(mean_total),newdf$mean_total,mean_total))
}


not_missing<-town_as%>%
  filter(!is.na(percent_over_1_town))

for (i in 1:length(as.data.frame(t(town_as)))){
  lat1<-town_as[i,'lat']
  lon1<-town_as[i,'lon']
  wa<-town_as[i,"assignment_method"]
  newdf<-not_missing%>%
    mutate(dist=sqrt((lat-lat1)^2+(lon-lon1)^2))%>%
    filter(dist==min(dist)&assignment_method==wa)
  town_as[i,]<-town_as[i,]%>%
    mutate(percent_over_1_town_method=ifelse(is.na(percent_over_1_town),'Closest Neighbor','Calculated'),
           percent_over_1_town=ifelse(is.na(percent_over_1_town),newdf$percent_over_1_town,percent_over_1_town))
}

not_missing<-town_as%>%
  filter(!is.na(percent_over_5_town))

for (i in 1:length(as.data.frame(t(town_as)))){
  lat1<-town_as[i,'lat']
  lon1<-town_as[i,'lon']
  wa<-town_as[i,"assignment_method"]
  newdf<-not_missing%>%
    mutate(dist=sqrt((lat-lat1)^2+(lon-lon1)^2))%>%
    filter(dist==min(dist)&assignment_method==wa)
  town_as[i,]<-town_as[i,]%>%
    mutate(percent_over_5_town_method=ifelse(is.na(percent_over_5_town),'Closest Neighbor','Calculated'),
           percent_over_5_town=ifelse(is.na(percent_over_5_town),newdf$percent_over_5_town,percent_over_5_town))
}


not_missing<-town_as%>%
  filter(!is.na(percent_over_10_town))

for (i in 1:length(as.data.frame(t(town_as)))){
  lat1<-town_as[i,'lat']
  lon1<-town_as[i,'lon']
  wa<-town_as[i,"assignment_method"]
  newdf<-not_missing%>%
    mutate(dist=sqrt((lat-lat1)^2+(lon-lon1)^2))%>%
    filter(dist==min(dist)&assignment_method==wa)
  town_as[i,]<-town_as[i,]%>%
    mutate(percent_over_10_town_method=ifelse(is.na(percent_over_10_town),'Closest Neighbor','Calculated'),
           percent_over_10_town=ifelse(is.na(percent_over_10_town),newdf$percent_over_10_town,percent_over_10_town))
}


town_as<-town_as%>%
  mutate(over_1_town=percent_over_1_town,
         over_5_town=percent_over_5_town,
         over_10_town=percent_over_10_town)%>%
  merge(sm_int_wq_as%>%
  count(town)%>%
  mutate(wells_sm=n)%>%
  merge(town_as%>%
          select(town)%>%
            distinct(),all=T)%>%
  select(town,wells_sm),all=T)%>%
  mutate(wells_sm=ifelse(is.na(wells_sm),0,wells_sm))


#Below we create a dtabase summarizing all of the town-wide data by method
as_exp_assesment<-town_as%>%
  mutate(pph=total_population/parcels,#total_population/total_housing_units,
         pph=ifelse(is.na(pph),2.5,pph),# average of 2.5 people per house assigned to any locations that were not assigned
         well_users=ifelse(assignment_method=='gdb',round(wells_gdb*pph),round(wells_sm*pph)),
         as_int_total=well_users*as_int,
         as_town_avg=well_users*mean_total,
         percent_over_1_mod=well_users*over_1,
         percent_over_1_int=well_users*over_1_int,
         percent_over_1_town=well_users*over_1_town,
         percent_over_5_mod=well_users*over_5,
         percent_over_5_int=well_users*over_5_int,
         percent_over_5_town=well_users*over_5_town,
         percent_over_10_mod=well_users*over_10,
         percent_over_10_int=well_users*over_10_int,
         percent_over_10_town=well_users*over_10_town)%>%
  merge(pwss%>%
  mutate(population_served_count=as.numeric(gsub(",","",population_served_count)),
         percent_over_1_cws=population_served_count*(arsenic_source_mean>0.001),
         percent_over_5_cws=population_served_count*(arsenic_source_mean>0.005),
         percent_over_10_cws=population_served_count*(arsenic_source_mean>0.01),
         town=city_name)%>%
  group_by(town,pws_type)%>%
  summarise(percent_over_1=sum(percent_over_1_cws,na.rm=T),
         percent_over_5=sum(percent_over_5_cws,na.rm=T),
         percent_over_10=sum(percent_over_10_cws,na.rm=T))%>%
    filter(pws_type!="Transient non-community system")%>%
    pivot_wider(names_from = pws_type,values_from = c(3:5)),all.x=T)

#Arsenic Concnetrations Exposed To By Method----
#Based on the above dataset we calculate the number of users exposed to each arsenic threshold using different methods and different sources
over_as_exp<-as_exp_assesment%>%
  select(town,well_users,total_population,population_served_CWS,population_served_NTNC,starts_with("percent_over"),assignment_method)%>%
  pivot_longer(cols = c(-town,-well_users,-total_population,-population_served_CWS,-population_served_NTNC,-assignment_method), names_to = c("threshold","method"), 
               names_pattern ="percent_over_(.*)_(.*)",values_to = "pop")%>%
  group_by(town)%>%
  mutate(population_served_CWS=mean(population_served_CWS,na.rm=T),
         population_served_NTNC=mean(population_served_NTNC,na.rm=T))%>%
  distinct()%>%
  group_by(threshold,method,assignment_method)%>%
  summarise(well_users=sum(well_users,na.rm = T),
            cws_users=sum(population_served_CWS,na.rm = T),
            ntnc_users=sum(population_served_NTNC,na.rm = T),
            total_population=sum(total_population,na.rm=T),
            total_exposed=sum(pop,na.rm=T))%>%
  mutate(method=ifelse(method=="Community water system","CWS",method),
         method=ifelse(method=="Non-Transient non-community system","NTNC",method),
         percent_exposed=ifelse(method=="CWS",total_exposed/cws_users,total_exposed/well_users),
         percent_exposed=ifelse(method=="NTNC",total_exposed/ntnc_users,percent_exposed),
         labels=paste(round(100*percent_exposed,1),"%",sep=""))

#Below is the code for Figures and Table of that
png("Data\\Figures\\as_threshold_exposed.png",width=12,height=6,res=300,units = "in")
over_as_exp%>%
  filter(assignment_method=='service_maps')%>%
  mutate(Source=method,
         Source=ifelse(Source=='mod','Domestic Wells: Probability Maps',Source),
         Source=ifelse(Source=='int','Domestic Wells: Interpolated',Source),
         Source=ifelse(Source=='town','Domestic Wells: Town Average',Source),
         Source=ifelse(Source=='CWS','PWS: CWSs',Source),
         Source=ifelse(Source=='NTNC','PWS: NTNCs',Source),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")))%>%
  #mutate(Source=fct_relevel(Source,c("CWS Average","NTNC Average","Domestic Model","Public Model","Interpolated","Town Average")))%>%
  mutate(threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")))%>%
  ggplot(aes(x=threshold,y=percent_exposed,fill=Source))+
  geom_bar(stat="identity",position = "dodge",color='black')+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(labels = scales::percent)+
  xlab("Arsenic (mg/L) threshold")+
  ylab("Percent Exposed")+
  theme_classic2()+
  theme(text = element_text(size=20),legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2))
dev.off()


town_as%>%
  filter(assignment_method=='service_maps')%>%
  summarise(sites=sum(locations,na.rm=T),
            towns_with_sites=sum(locations>=1,na.rm=T),
            sites_with_coords=sum(locations_with_coords,na.rm=T),
            towns_with_coords=sum(locations_with_coords>=1,na.rm=T),
            usgs_sites=sum(usgs_sites,na.rm = T),
            towns_usgs=sum(usgs_sites>=1,na.rm=T),
            raw_gw_sites=sum(raw_water_sites,na.rm = T),
            towns_raw_gw=sum(raw_water_sites>=1,na.rm=T),
            finished_ntnc_sites=sum(finished_water_sites,na.rm = T),
            towns_finished_water=sum(finished_water_sites>=1,na.rm=T))%>%
  write.csv("Tables/as_sites_summary.csv",row.names = F)


labs=pwss%>%
  filter(!is.na(arsenic_source_mean))%>%
  count(pws_type,gw_or_sw_code)%>%
  filter(gw_or_sw_code=='SW'|gw_or_sw_code=='GW')%>%
  mutate(gw_or_sw_code=ifelse(gw_or_sw_code=='GW','Groundwater','Surface water'))%>%
  filter(!is.na(pws_type))

png("Data\\Figures\\arsenic_pws.png",width=12,height=10,res=300,units = "in")
pwss%>%
  filter(!is.na(arsenic_source_mean))%>%
  mutate(arsenic_source_mean=1000*arsenic_source_mean)%>%
  filter(gw_or_sw_code=='SW'|gw_or_sw_code=='GW')%>%
  mutate(gw_or_sw_code=ifelse(gw_or_sw_code=='GW','Groundwater','Surface water'))%>%
  filter(!is.na(pws_type))%>%
  ggplot(aes(y=arsenic_source_mean,x=gw_or_sw_code))+
  geom_boxplot(aes(fill=pws_type))+
  geom_text(data=labs,aes(label=n,y=-2,color=pws_type),position = position_dodge(width=.75),size=12)+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  geom_hline(yintercept = 10,colour="red",linewidth=1.5)+
  xlab('')+
  ylab('Mean Arsenic (ug/L)')+
  theme_bw()+
  theme(text = element_text(size=20),legend.title = element_blank(),legend.position = "bottom")
dev.off()

labs=pw_vs_pws%>%
  filter(!is.na(arsenic_source_mean))%>%
  count(source)

png("Data\\Figures\\arsenic_pw_vs_pws.png",width=14,height=6,res=300,units = "in")
pw_vs_pws%>%
  filter(!is.na(arsenic_source_mean))%>%
  mutate(arsenic_source_mean=ifelse(source!='USGS',1000*arsenic_source_mean,arsenic_source_mean))%>%
  ggplot(aes(y=source,x=arsenic_source_mean,fill=source))+
  geom_vline(xintercept = 10,colour="red",linewidth=1.5)+
  geom_boxplot()+
  ylab('')+
  xlab('Mean Arsenic Concentration (ug/l)')+
  geom_text(data=labs,aes(label=n,x=-10,color=source),size=8)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw()+
  theme(text = element_text(size=20),legend.title = element_blank(),legend.position = "bottom")
dev.off()

png("Data\\Figures\\arsenic_pw_vs_pws_zoom.png",width=14,height=6,res=300,units = "in")
pw_vs_pws%>%
  filter(!is.na(arsenic_source_mean))%>%
  mutate(arsenic_source_mean=ifelse(source!='USGS',1000*arsenic_source_mean,arsenic_source_mean))%>%
  ggplot(aes(y=source,x=arsenic_source_mean,fill=source))+
  geom_boxplot()+
  ylab('')+
  xlab('Mean Arsenic Concentration (ug/l)')+
  scale_x_continuous(limits = c(0,10))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_bw()+
  theme(text = element_text(size=20),legend.title = element_blank(),legend.position = "bottom")
dev.off()


#Arsenic by County----
#Below we follow the exact same methods as before in the Town-Analysis, to summarize over the counties
county_as<-well_by_town%>%
  filter(work_performed!="Decommission")%>%
  merge(counties,all.x=T)%>%
  count(county)%>%
  mutate(wells_db=n)%>%
  select(-n)%>%
  merge(dom_parcels%>%
          mutate(town=CITY)%>%
          merge(counties,all.x=T)%>%
          count(county),all=T)%>%
  mutate(parcels=n)%>%
  select(-n)%>%
  merge(well_loc%>%
          clean_names()%>%
          merge(counties,all.x=T)%>%
          count(county),all=T)%>%
  mutate(wells_gdb=n)%>%
  select(-n)%>%
  merge(pw_vs_pws%>%
          filter(!is.na(arsenic_source_mean)&source!='EEA Finished CWS')%>%
          mutate(arsenic_source_mean=ifelse(source=='USGS',arsenic_source_mean/1000,arsenic_source_mean),
                 arsenic_recent_result=ifelse(source=='USGS',arsenic_recent_result/1000,arsenic_recent_result))%>%
          merge(counties,all.x=T)%>%
          group_by(county)%>%
          summarise(locations=sum(!is.na(town)),
                    locations_with_coords=sum(!is.na(latitude)),
                    usgs_sites=sum(source=='USGS'),
                    raw_water_sites=sum(source=='EEA Raw GW'),
                    finished_water_sites=sum(source=='EEA Finished NTNC or TNC'),
                    mean_usgs=mean(arsenic_source_mean[source=='USGS']),
                    mean_raw_eea=mean(arsenic_source_mean[source=='EEA Raw GW']),
                    mean_finished_ntnc=mean(arsenic_source_mean[source=='EEA Finished NTNC or TNC']),
                    mean_total=mean(arsenic_source_mean,na.rm=T),
                    sd_total=sd(arsenic_source_mean,na.rm=T),
                    fifth_total=quantile(arsenic_source_mean, .05),
                    ninety_fifth_total=quantile(arsenic_source_mean, .95),
                    percent_over_one_county=sum(arsenic_source_mean>0.001)/sum(!is.na(arsenic_source_mean)),
                    percent_over_five_county=sum(arsenic_source_mean>0.005)/sum(!is.na(arsenic_source_mean)),
                    percent_over_ten_county=sum(arsenic_source_mean>0.01)/sum(!is.na(arsenic_source_mean))),all=T)%>%
  mutate(mean_total=ifelse(is.na(mean_total)&county=='SUFFOLK',mean_total[county=='NORFOLK'],mean_total),
         percent_over_one_county=ifelse(is.na(percent_over_one_county)&county=='SUFFOLK',percent_over_one_county[county=='NORFOLK'],percent_over_one_county),
         percent_over_five_county=ifelse(is.na(percent_over_five_county)&county=='SUFFOLK',percent_over_five_county[county=='NORFOLK'],percent_over_five_county),
         percent_over_ten_county=ifelse(is.na(percent_over_ten_county)&county=='SUFFOLK',percent_over_ten_county[county=='NORFOLK'],percent_over_ten_county))

#And summarise by regions (not included in the final analysis)
region_as<-well_by_town%>%
  filter(work_performed!="Decommission")%>%
  merge(counties,all.x=T)%>%
  count(region)%>%
  mutate(wells_db=n)%>%
  select(-n)%>%
  merge(dom_parcels%>%
          mutate(town=CITY)%>%
          merge(counties,all.x=T)%>%
          count(region),all=T)%>%
  mutate(parcels=n)%>%
  select(-n)%>%
  merge(well_loc%>%
          clean_names()%>%
          merge(counties,all.x=T)%>%
          count(region),all=T)%>%
  mutate(wells_gdb=n)%>%
  select(-n)%>%
  merge(pw_vs_pws%>%
          filter(!is.na(arsenic_source_mean)&source!='EEA Finished CWS')%>%
          mutate(arsenic_source_mean=ifelse(source=='USGS',arsenic_source_mean/1000,arsenic_source_mean),
                 arsenic_recent_result=ifelse(source=='USGS',arsenic_recent_result/1000,arsenic_recent_result))%>%
          merge(counties,all.x=T)%>%
          group_by(region)%>%
          summarise(locations=sum(!is.na(town)),
                    locations_with_coords=sum(!is.na(latitude)),
                    usgs_sites=sum(source=='USGS'),
                    raw_water_sites=sum(source=='EEA Raw GW'),
                    finished_water_sites=sum(source=='EEA Finished NTNC or TNC'),
                    mean_usgs=mean(arsenic_source_mean[source=='USGS']),
                    mean_raw_eea=mean(arsenic_source_mean[source=='EEA Raw GW']),,
                    mean_finished_ntnc=mean(arsenic_source_mean[source=='EEA Finished NTNC or TNC']),
                    mean_total=mean(arsenic_source_mean,na.rm=T),
                    sd_total=sd(arsenic_source_mean,na.rm=T),
                    fifth_total=quantile(arsenic_source_mean, .05),
                    ninety_fifth_total=quantile(arsenic_source_mean, .95),
                    percent_over_one_region=sum(arsenic_source_mean>=0.001)/sum(!is.na(arsenic_source_mean)),
                    percent_over_five_region=sum(arsenic_source_mean>=0.005)/sum(!is.na(arsenic_source_mean)),
                    percent_over_ten_region=sum(arsenic_source_mean>=0.01)/sum(!is.na(arsenic_source_mean)),
                    ),all=T)


colnames(county_as) <- paste(colnames(county_as),"county",sep="_") 
county_as<-county_as%>%mutate(county=county_county)%>%select(-county_county)
colnames(region_as) <- paste(colnames(region_as),"region",sep="_") 
region_as<-region_as%>%mutate(region=region_region)%>%select(-region_region)

#Below we create the full database, combining the previous datasets to summarize the number of wells
#average concnetrations, percent above threshold, etc. in each town. All methods described in the manuscript are included
#in this database
as_summary<-as_exp_assesment%>%
  select(c(1:15),c(23:31),well_users,over_1_town,over_5_town,over_10_town,wells_sm)%>%
  distinct()%>%
  merge(counties,all.x = T)%>%
  merge(county_as,all.x=T)%>%
  merge(region_as,all.x=T)%>% 
  mutate_all(~ifelse(is.nan(.), NA, .))%>%
  mutate(n_int=ifelse(is.na(n_int),0,n_int),
         over_1_int=ifelse(is.na(over_1_int),0,over_1_int),
         over_5_int=ifelse(is.na(over_5_int),0,over_5_int),
         over_10_int=ifelse(is.na(over_10_int),0,over_10_int),
         n_modeled=ifelse(is.na(n_modeled),0,n_modeled),
         over_1=ifelse(is.na(over_1),0,over_1),
         over_5=ifelse(is.na(over_5),0,over_5),
         over_10=ifelse(is.na(over_10),0,over_10),
         wells_db=ifelse(is.na(wells_db),0,wells_db),
         wells_gdb=ifelse(is.na(wells_gdb),0,wells_gdb),
         parcels=ifelse(is.na(parcels),total_population/2.5,parcels),
         locations=ifelse(is.na(locations),0,locations),
         locations_with_coords=ifelse(is.na(locations_with_coords),0,locations_with_coords),
         raw_water_sites=ifelse(is.na(raw_water_sites),0,raw_water_sites),
         usgs_sites=ifelse(is.na(usgs_sites),0,usgs_sites),
         finished_water_sites=ifelse(is.na(finished_water_sites),0,finished_water_sites))

as_summary%>%
  write.xlsx("Tables\\as_county_summary.xlsx")


#Below the number of users exposed to each threshold is summarized at a county level
as_summary_fg<-as_summary%>%
  group_by(assignment_method)%>%
  summarise(threshold=1,method='county_summary',
            well_users1=(sum(well_users,na.rm=T)),
            cws_users=NA,
            ntnc_users=NA,
            total_population=sum(total_population),
            total_exposed=sum(percent_over_one_county_county*well_users,na.rm=T),
            percent_exposed=total_exposed/well_users1,
            labels=paste(round(100*percent_exposed,1),"%",sep="")
            )%>%
rbind(as_summary%>%
        group_by(assignment_method)%>%
        summarise(threshold=5,method='county_summary',
                  well_users1=(sum(well_users,na.rm=T)),
                  cws_users=NA,
                  ntnc_users=NA,
                  total_population=sum(total_population),
                  total_exposed=sum(percent_over_five_county_county*well_users,na.rm=T),
                  percent_exposed=total_exposed/well_users1,
                  labels=paste(round(100*percent_exposed,1),"%",sep="")
        ))%>%
  rbind(as_summary%>%
          group_by(assignment_method)%>%
          summarise(threshold=10,method='county_summary',
                    well_users1=(sum(well_users,na.rm=T)),
                    cws_users=NA,
                    ntnc_users=NA,
                    total_population=sum(total_population),
                    total_exposed=sum(percent_over_ten_county_county*well_users,na.rm=T),
                    percent_exposed=total_exposed/well_users1,
                    labels=paste(round(100*percent_exposed,1),"%",sep="")
          ))%>%
  rbind(as_summary%>%
          group_by(assignment_method)%>%
          summarise(threshold=1,method='region_summary',
                    well_users1=(sum(well_users,na.rm=T)),
                    cws_users=NA,
                    ntnc_users=NA,
                    total_population=sum(total_population),
                    total_exposed=sum(percent_over_one_region_region*well_users,na.rm=T),
                    percent_exposed=total_exposed/well_users1,
                    labels=paste(round(100*percent_exposed,1),"%",sep="")
          ))%>%
  rbind(as_summary%>%
          group_by(assignment_method)%>%
          summarise(threshold=5,method='region_summary',
                    well_users1=(sum(well_users,na.rm=T)),
                    cws_users=NA,
                    ntnc_users=NA,
                    total_population=sum(total_population),
                    total_exposed=sum(percent_over_five_region_region*well_users,na.rm=T),
                    percent_exposed=total_exposed/well_users1,
                    labels=paste(round(100*percent_exposed,1),"%",sep="")
          ))%>%
  rbind(as_summary%>%
          group_by(assignment_method)%>%
          summarise(threshold=10,method='region_summary',
                    well_users1=(sum(well_users,na.rm=T)),
                    cws_users=NA,
                    ntnc_users=NA,
                    total_population=sum(total_population),
                    total_exposed=sum(percent_over_ten_region_region*well_users,na.rm=T),
                    percent_exposed=total_exposed/sum(well_users1),
                    labels=paste(round(100*percent_exposed,1),"%",sep="")
          ))%>%
  mutate(threshold=as.character(threshold),
         well_users=well_users1)%>%
  select(colnames(over_as_exp))


#Aresnic Town Exposure Distribution----
well_as_dist<-as_summary%>%
  mutate(pph=total_population/parcels,#total_population/total_housing_units,
         pph=ifelse(is.na(pph),2.5,pph),
         well_users=ifelse(assignment_method=='gdb',round(wells_gdb*pph),round(wells_sm*pph)),
         as_int_total=as_int,
         as_town_avg=mean_total)%>%
  select(well_users,assignment_method,as_int_total,as_town_avg,mean_total_county,mean_total_region)%>%
  mutate(well_users=ifelse(is.na(well_users),0,well_users))%>%
  pivot_longer(cols=c('as_int_total','as_town_avg','mean_total_county','mean_total_region'),names_to = "Method",values_to = "as_exposed")%>%
  uncount(well_users)%>%
  mutate(Source="Private Well")%>%
  rbind(town_as%>%
          select(population_served_CWS,pp_as_exposure_CWS,pp_as_exposure_NTNC,population_served_NTNC)%>%
          mutate(population_served_CWS=ifelse(is.na(population_served_CWS),0,population_served_CWS))%>%
          pivot_longer(cols=c('pp_as_exposure_CWS','pp_as_exposure_NTNC'),names_to = "Method",values_to = "as_exposed")%>%
          mutate(population_served=ifelse(Method=='pp_as_exposure_CWS',population_served_CWS,0),
                 population_served=ifelse(Method=='pp_as_exposure_NTNC',population_served_NTNC,population_served),
                 assignment_method='SDWIS')%>%
          filter(!is.na(as_exposed))%>%
          select(population_served,assignment_method,Method,as_exposed)%>%
          uncount(population_served)%>%
          mutate(Method=ifelse(Method=='pp_as_exposure_CWS',"CWS Average",Method),
                 Method=ifelse(Method=='pp_as_exposure_NTNC',"NTNC Average",Method),
                 Source='PWS'))

#Distribution based on service map wells (SM)
png("Data\\Figures\\town_cum_dist_as_sm.png",width=18,height=10,res=300,units = "in")
well_as_dist%>%
  filter(assignment_method=='service_maps'|assignment_method=='SDWIS')%>%
  #clean names
  mutate(Method=ifelse(Method=="as_town_avg","Town Average",Method),
         Method=ifelse(Method=="as_int_total","Spatial Interpolation",Method),
         Method=ifelse(Method=="mean_total_county","County Average",Method),
         Method=ifelse(Method=="mean_total_region","Region Average",Method),
         Method=as.factor(Method))%>%
  mutate(Method=fct_relevel(Method,"CWS Average","NTNC Average","Spatial Interpolation","Town Average","County Average","Region Average"))%>%
  filter(Method!="NTNC Average")%>%
  ggplot(aes(as_exposed,color=Method,linetype=Source)) +
  geom_line(aes(y = 1 - ..y..), stat='ecdf',linewidth=1.5)+
  xlab('Arsenic (mg/L)')+
  ylab('Percent Exposed to or Above Arsenic Concentration')+
  xlim(0,.02)+
  #geom_text(aes(x=5,y=1,label=n))+
  theme_classic()+
  theme(text = element_text(size=20),legend.title = element_blank(),legend.position = "bottom")
dev.off()

#Distribution based on Well Driller Database (GDB)
png("Data\\Figures\\town_cum_dist_as_gdb.png",width=18,height=10,res=300,units = "in")
well_as_dist%>%
  filter(assignment_method=='gdb'|assignment_method=='SDWIS')%>%
  mutate(Method=ifelse(Method=="as_town_avg","Town Average",Method),
         Method=ifelse(Method=="as_int_total","Spatial Interpolation",Method),
         Method=ifelse(Method=="mean_total_county","County Average",Method),
         Method=ifelse(Method=="mean_total_region","Region Average",Method),
         Method=as.factor(Method))%>%
  mutate(Method=fct_relevel(Method,"CWS Average","NTNC Average","Spatial Interpolation","Town Average","County Average","Region Average"))%>%
  filter(Method!="NTNC Average")%>%
  ggplot(aes(as_exposed,color=Method,linetype=Source)) +
  geom_line(aes(y = 1 - ..y..), stat='ecdf',linewidth=1.5)+
  xlab('Arsenic (mg/L)')+
  ylab('Percent Exposed to or Above Arsenic Concentration')+
  xlim(0,.02)+
  #geom_text(aes(x=5,y=1,label=n))+
  theme_classic()+
  theme(text = element_text(size=20),legend.title = element_blank(),legend.position = "bottom")
dev.off()




#Arsenic Threshold Exposed----
library(ggpattern)
#SM Figure
png("Data\\Figures\\as_threshold_exposed_sm.png",width=12,height=6,res=300,units = "in")
over_as_exp%>%
  rbind(as_summary_fg)%>%
  filter(assignment_method=='service_maps'|assignment_method=='SDWIS')%>%
  mutate(Source=method,
         #Cleaning names, and reordering for figure
         Source=ifelse(Source=='mod','Domestic Wells: Probability Maps',Source),
         Source=ifelse(Source=='int','Domestic Wells: Interpolated',Source),
         Source=ifelse(Source=='town','Domestic Wells: Town Average',Source),
         Source=ifelse(Source=='CWS','PWS: CWSs',Source),
         Source=ifelse(Source=='NTNC','PWS: NTNCs',Source),
         Source=ifelse(Source=='county_summary','Domestic Wells: County Average',Source),
         Source=ifelse(Source=='region_summary','Domestic Wells: Region Average',Source),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")))%>%
  mutate(Source=fct_relevel(Source,c('PWS: CWSs','PWS: NTNCs','Domestic Wells: Probability Maps','Domestic Wells: Interpolated','Domestic Wells: Town Average','Domestic Wells: County Average','Domestic Wells: Region Average')))%>%
  mutate(threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         `PWS or Well`=ifelse(grepl('PWS',Source),'PWS','Domestic Well'))%>%
  filter(Source!='PWS: NTNCs')%>%#Remove NTNC data
  ggplot(aes(x=threshold,y=percent_exposed,fill=Source,pattern=`PWS or Well`))+
  geom_bar(stat="identity",position = "dodge",color='black')+
  geom_bar_pattern(position = "dodge",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   stat="identity") +
  scale_fill_brewer(palette = "Set3")+
  scale_pattern_manual(values = c('PWS' = "stripe", 'Domestic Well' = "none")) +
  scale_y_continuous(labels = scales::percent)+
  xlab(expression(paste("Arsenic (",mu,"g/L) threshold")))+
  ylab("Percent Exposed")+
  theme_classic2()+
  theme(text = element_text(size=16))+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none")))
dev.off()


#GDB Figure
png("Data\\Figures\\as_threshold_exposed_all_gdb.png",width=12,height=6,res=300,units = "in")
over_as_exp%>%
  rbind(as_summary_fg)%>%
  filter(assignment_method=='gdb'|assignment_method=='SDWIS')%>%
  mutate(Source=method,
         Source=ifelse(Source=='mod','Domestic Wells: Probability Maps',Source),
         Source=ifelse(Source=='int','Domestic Wells: Interpolated',Source),
         Source=ifelse(Source=='town','Domestic Wells: Town Average',Source),
         Source=ifelse(Source=='CWS','PWS: CWSs',Source),
         Source=ifelse(Source=='NTNC','PWS: NTNCs',Source),
         Source=ifelse(Source=='county_summary','Domestic Wells: County Average',Source),
         Source=ifelse(Source=='region_summary','Domestic Wells: Region Average',Source),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")))%>%
  mutate(Source=fct_relevel(Source,c('PWS: CWSs','PWS: NTNCs','Domestic Wells: Probability Maps','Domestic Wells: Interpolated','Domestic Wells: Town Average','Domestic Wells: County Average','Domestic Wells: Region Average')))%>%
  mutate(threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         `PWS or Well`=ifelse(grepl('PWS',Source),'PWS','Domestic Well'))%>%
  filter(Source!='PWS: NTNCs')%>%
  ggplot(aes(x=threshold,y=percent_exposed,fill=Source,pattern=`PWS or Well`))+
  geom_bar(stat="identity",position = "dodge",color='black')+
  geom_bar_pattern(position = "dodge",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   stat="identity") +
  scale_fill_brewer(palette = "Set3")+
  scale_pattern_manual(values = c('PWS' = "stripe", 'Domestic Well' = "none")) +
  scale_y_continuous(labels = scales::percent)+
  xlab(expression(paste("Arsenic (",mu,"g/L) threshold")))+
  ylab("Percent Exposed")+
  theme_classic2()+
  theme(text = element_text(size=16))+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
dev.off()

as_summary2<-as_summary
colnames(as_summary2) <- sub("over_", "town_over_", colnames(as_summary2))
colnames(as_summary2) <- sub("as_int", "town_as_int", colnames(as_summary2))


#Prediction Evaluation----
#As error is a dataset of the prediced and actual arsenic concentrations at all the wells we have data for
as_ext<-gw_extracted%>%
  filter(arsenic_n_>0)

as_error_tot=as_ext%>%
  mutate(arsenic_so=arsenic_so*1000,
         as_int=1000*as_int)%>%
  mutate(training=ifelse(training=='N','USGS Wells not used in interpolation (pre 2010)','Interpolated Data'))%>%
  filter(as_int>=0)%>%
  select(town,source,arsenic_so,c(96:126))%>%
  merge(as_summary2,by="town")%>%
  mutate(mean_total=mean_total*1000,
         mean_total_county=mean_total_county*1000,
         mean_total_region=mean_total_region*1000)


as_error=as_error_tot%>%
  filter(assignment_method=='service_maps')

df<-as_error%>%
  select(source,training)%>%
  distinct()


stats_test_greater=df%>%mutate(alternative='greater')
stats_test_lesser=df%>%mutate(alternative='lesser')

#below are statistical test for if each method over or estimates As concnetrations for
#locations winch we have actual arsenic measurements for
for (i in 1:length(t(df[,1]))) {
  stats_test_greater[i,'as_int']=wilcox.test(as_error$as_int[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'as_int']=wilcox.test(as_error$as_int[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "l",paired=T)$p.value 
  stats_test_greater[i,'mean_total']=wilcox.test(as_error$mean_total[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'mean_total']=wilcox.test(as_error$mean_total[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "l",paired=T)$p.value 
  stats_test_greater[i,'mean_total_county']=wilcox.test(as_error$mean_total_county[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'mean_total_county']=wilcox.test(as_error$mean_total_county[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "l",paired=T)$p.value 
  stats_test_greater[i,'mean_total_region']=wilcox.test(as_error$mean_total_region[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'mean_total_region']=wilcox.test(as_error$mean_total_region[as_error$source==df[i,'source']&as_error$training==df[i,'training']],as_error$arsenic_so[as_error$source==df[i,'source']&as_error$training==df[i,'training']],alternative = "l",paired=T)$p.value 
}

#Create database with p values of the above tests
stats_test=rbind(stats_test_greater,stats_test_lesser)%>%
  mutate(as_int=round(as_int,4),
         mean_total=round(mean_total,4),
         mean_total_county=round(mean_total_county,4),
         mean_total_region=round(mean_total_region,4))

full_stats_test=stats_test

#define R-squared function
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

#calculate R-Squared for the INTERPOLATION method

R2_as_tr = round(RSQUARE(as_error$arsenic_so[as_error$training=='USGS Wells not used in interpolation (pre 2010)'],as_error$as_int[as_error$training=='USGS Wells not used in interpolation (pre 2010)']),2)
R2_as_te = round(RSQUARE(as_error$arsenic_so[as_error$training=='Interpolated Data'],as_error$as_int[as_error$training=='Interpolated Data']),2)

#Create abel for R-squared on figure
as_error_lab=as_error%>%
  select(training)%>%
  distinct()%>%
  mutate(n=paste('R2=',ifelse(training=='USGS Wells not used in interpolation (pre 2010)',R2_as_tr,R2_as_te),sep=''))

#Plot predicted vs actual data for INTERPOLATION method
asw1<-as_error%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int,color=source))+
  geom_text(data=as_error_lab,aes(y=500,x=10,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

#Zoomed in version of previous figure
asw2<-as_error%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int,color=source))+
  geom_text(data=as_error_lab,aes(y=250,x=30,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

as_error_lab2=as_error_lab%>%
  filter(training!="USGS Wells not used in interpolation (pre 2010)")

#Plot of accuracy on training dataset
accuracy_met_int<-as_error%>%
  filter(training!="USGS Wells not used in interpolation (pre 2010)")%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int,color=source))+
  geom_text(data=as_error_lab2,aes(y=200,x=5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  #facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=19),legend.position = "bottom",legend.title = element_blank())

#Log scale of previos plot
accuracy_met_int_log<-as_error%>%
  filter(training!="USGS Wells not used in interpolation (pre 2010)")%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int,color=source))+
  geom_text(data=as_error_lab2,aes(y=100,x=0.1,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  scale_x_log10()+scale_y_log10()+
  #facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=19),legend.position = "bottom",legend.title = element_blank())

#Zoomed version of previous plot
accuracy_met_int_zoom<-as_error%>%
  filter(training!="USGS Wells not used in interpolation (pre 2010)")%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int,color=source))+
  #geom_text(data=as_error_lab2,aes(y=10,x=5,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  #facet_wrap(~training)+
  theme_bw()+
  ylim(0,15)+
  xlim(0,15)+
  theme(text = element_text(size=19),legend.position = "bottom",legend.title = element_blank())

#Below we do the same as above for the testing data
as_error_lab2=as_error_lab%>%
  filter(training=="USGS Wells not used in interpolation (pre 2010)")

accuracy_met_int_usgs<-as_error%>%
  filter(training=="USGS Wells not used in interpolation (pre 2010)")%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int),color='blue')+
  geom_text(data=as_error_lab2,aes(y=450,x=2,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  #facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=19),legend.position = "bottom",legend.title = element_blank())

accuracy_met_int_zoom_usgs<-as_error%>%
  #mutate(arsenic_so=arsenic_so*1000,
  #      as_int=1000*as_int)%>%
  filter(training=="USGS Wells not used in interpolation (pre 2010)")%>%
  filter(as_int>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=as_int),color='blue')+
  #geom_text(data=as_error_lab2,aes(y=10,x=5,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Interpolated Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  #facet_wrap(~training)+
  theme_bw()+
  ylim(0,15)+
  xlim(0,15)+
  theme(text = element_text(size=19),legend.position = "bottom",legend.title = element_blank())


png("Data\\Figures\\as_well_pred2.png",height = 10,width=10,res=300,units = "in")
ggarrange(asw1,asw2,
          ncol = 1, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

#BELOW IS THE SAME AS WHAT WAS DONE ABOVE FOR THE INTERPOLATED METHODS
#NOW WITH TOWN AVERAGES
R2_as_tr = round(RSQUARE(as_error$arsenic_so[as_error$training=='USGS Wells not used in interpolation (pre 2010)'],as_error$mean_total[as_error$training=='USGS Wells not used in interpolation (pre 2010)']),2)
R2_as_te = round(RSQUARE(as_error$arsenic_so[as_error$training=='Interpolated Data'],as_error$mean_total[as_error$training=='Interpolated Data']),2)

as_error_lab=as_error%>%
  select(training)%>%
  distinct()%>%
  mutate(n=paste('R2=',ifelse(training=='USGS Wells not used in interpolation (pre 2010)',R2_as_tr,R2_as_te),sep=''))

asw1<-as_error%>%
  filter(mean_total>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total,color=source))+
  geom_text(data=as_error_lab,aes(y=500,x=5,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

asw2<-as_error%>%
  filter(mean_total>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total,color=source))+
  geom_text(data=as_error_lab,aes(y=250,x=30,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())

as_error_lab2<-as_error_lab%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')


accuracy_met_town<-as_error%>%
  filter(mean_total>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total,color=source))+
  geom_text(data=as_error_lab2,aes(y=200,x=3,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

accuracy_met_town_log<-as_error%>%
  filter(mean_total>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total,color=source))+
  geom_text(data=as_error_lab2,aes(y=100,x=0.1,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

accuracy_met_town_zoom<-as_error%>%
  filter(mean_total>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total,color=source))+
  #geom_text(data=as_error_lab2,aes(y=200,x=5,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

as_error_lab2<-as_error_lab%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')


accuracy_met_town_usgs<-as_error%>%
  filter(mean_total>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total),color='blue')+
  geom_text(data=as_error_lab2,aes(y=400,x=1,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

accuracy_met_town_zoom_usgs<-as_error%>%
  filter(mean_total>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total),color='blue')+
  #geom_text(data=as_error_lab2,aes(y=200,x=5,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())



png("Data\\Figures\\as_well_pred_town_avg.png",height = 10,width=10,res=300,units = "in")
ggarrange(asw1,asw2,
          ncol = 1, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

#BELOW IS THE SAME AS WHAT WAS DONE ABOVE FOR THE INTERPOLATED METHODS
#NOW WITH COUNTY AVERAGES
R2_as_tr = round(RSQUARE(as_error$arsenic_so[as_error$training=='USGS Wells not used in interpolation (pre 2010)'],as_error$mean_total_county[as_error$training=='USGS Wells not used in interpolation (pre 2010)']),2)
R2_as_te = round(RSQUARE(as_error$arsenic_so[as_error$training=='Interpolated Data'],as_error$mean_total_county[as_error$training=='Interpolated Data']),2)

as_error_lab=as_error%>%
  select(training)%>%
  distinct()%>%
  mutate(n=paste('R2=',ifelse(training=='USGS Wells not used in interpolation (pre 2010)',R2_as_tr,R2_as_te),sep=''))

asw1<-as_error%>%
  filter(mean_total_county>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_county,color=source))+
  geom_text(data=as_error_lab,aes(y=500,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

asw2<-as_error%>%
  filter(mean_total_county>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_county,color=source))+
  geom_text(data=as_error_lab,aes(y=250,x=30,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

as_error_lab2<-as_error_lab%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')

accuracy_met_county<-as_error%>%
  filter(mean_total_county>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_county,color=source))+
  geom_text(data=as_error_lab2,aes(y=200,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('County Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

accuracy_met_county_log<-as_error%>%
  filter(mean_total_county>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_county,color=source))+
  geom_text(data=as_error_lab2,aes(y=100,x=0.2,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('County Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  scale_x_log10(limits=c(.1,10))+
  scale_y_log10()+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

accuracy_met_county_zoom<-as_error%>%
  filter(mean_total_county>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_county,color=source))+
  #geom_text(data=as_error_lab2,aes(y=200,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('County Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())


as_error_lab2<-as_error_lab%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')

accuracy_met_county_usgs<-as_error%>%
  filter(mean_total_county>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_county),color='blue')+
  geom_text(data=as_error_lab2,aes(y=400,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('County Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())

accuracy_met_county_zoom_usgs<-as_error%>%
  filter(mean_total_county>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_county),color='blue')+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('County Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  theme_bw()+
  theme(text = element_text(size=17),legend.position = "bottom",legend.title = element_blank())


png("Data\\Figures\\as_well_pred_county_avg.png",height = 10,width=10,res=300,units = "in")
ggarrange(asw1,asw2,
          ncol = 1, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

#BELOW IS THE SAME AS WHAT WAS DONE ABOVE FOR THE INTERPOLATED METHODS
#NOW WITH REGION AVERAGES

R2_as_tr = round(RSQUARE(as_error$arsenic_so[as_error$training=='USGS Wells not used in interpolation (pre 2010)'],as_error$mean_total_region[as_error$training=='USGS Wells not used in interpolation (pre 2010)']),2)
R2_as_te = round(RSQUARE(as_error$arsenic_so[as_error$training=='Interpolated Data'],as_error$mean_total_region[as_error$training=='Interpolated Data']),2)

as_error_lab=as_error%>%
  select(training)%>%
  distinct()%>%
  mutate(n=paste('R2=',ifelse(training=='USGS Wells not used in interpolation (pre 2010)',R2_as_tr,R2_as_te),sep=''))

asw1<-as_error%>%
  filter(mean_total_region>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_region,color=source))+
  geom_text(data=as_error_lab,aes(y=500,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())

asw2<-as_error%>%
  filter(mean_total_region>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_region,color=source))+
  geom_text(data=as_error_lab,aes(y=250,x=30,label=n),size=5)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Town Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  facet_wrap(~training)+
  theme_bw()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())

as_error_lab2<-as_error_lab%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')

accuracy_met_region<-as_error%>%
  filter(mean_total_region>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_region,color=source))+
  geom_text(data=as_error_lab2,aes(y=200,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Region Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())

accuracy_met_region_log<-as_error%>%
  filter(mean_total_region>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_region,color=source))+
  geom_text(data=as_error_lab2,aes(y=100,x=.2,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Region Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  scale_x_log10(limits=c(0.1,10))+
  scale_y_log10()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())

accuracy_met_region_zoom<-as_error%>%
  filter(mean_total_region>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training!='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_region,color=source))+
  #geom_text(data=as_error_lab2,aes(y=200,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Region Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  theme_bw()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())

as_error_lab2<-as_error_lab%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')

accuracy_met_region_usgs<-as_error%>%
  filter(mean_total_region>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_region),color='blue')+
  geom_text(data=as_error_lab2,aes(y=400,x=1.5,label=n),size=7)+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Region Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  theme_bw()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())

accuracy_met_region_zoom_usgs<-as_error%>%
  filter(mean_total_region>=0)%>%
  filter(source!='EEA Finished CWS')%>%
  filter(training=='USGS Wells not used in interpolation (pre 2010)')%>%
  ggplot()+
  geom_point(aes(y=arsenic_so,x=mean_total_region),color='blue')+
  ylab('Actual Arsenic Concentration (ug/L)')+
  xlab('Region Average Arsenic Concentration (ug/L)')+
  geom_abline(slope=1,intercept=0)+
  ylim(0,15)+
  xlim(0,15)+
  theme_bw()+
  theme(text = element_text(size=15),legend.position = "bottom",legend.title = element_blank())


png("Data\\Figures\\as_well_pred_region_avg.png",height = 12,width=12,res=300,units = "in")
ggarrange(asw1,asw2,
          ncol = 1, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()


png("Data\\Figures\\as_well_pred_all_dif.png",height = 12,width=12,res=300,units = "in")
ggarrange(accuracy_met_int,accuracy_met_town,accuracy_met_county,accuracy_met_region,
          ncol = 2, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

png("Data\\Figures\\as_well_pred_all_dif_log.png",height = 12,width=12,res=300,units = "in")
ggarrange(accuracy_met_int_log,accuracy_met_town_log,accuracy_met_county_log,accuracy_met_region_log,
          ncol = 2, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

png("Data\\Figures\\as_well_pred_all_dif_zoom.png",height = 10,width=10,res=300,units = "in")
ggarrange(accuracy_met_int_zoom,accuracy_met_town_zoom,accuracy_met_county_zoom,accuracy_met_region_zoom,
          ncol = 2, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

png("Data\\Figures\\as_well_pred_all_dif_usgs.png",height = 12,width=12,res=300,units = "in")
ggarrange(accuracy_met_int_usgs,accuracy_met_town_usgs,accuracy_met_county_usgs,accuracy_met_region_usgs,
          ncol = 2, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()

png("Data\\Figures\\as_well_pred_all_dif_zoom_usgs.png",height = 10,width=10,res=300,units = "in")
ggarrange(accuracy_met_int_zoom_usgs,accuracy_met_town_zoom_usgs,accuracy_met_county_zoom_usgs,accuracy_met_region_zoom_usgs,
          ncol = 2, nrow = 2,labels="AUTO",
          common.legend = TRUE, legend = "bottom")
dev.off()


#Below we do the significance testing described earlier for each method
#to test if each method is over or underestimating As concentrations
as_error_town_tot<-as_error_tot%>%
  group_by(town,source,training,assignment_method)%>%
  summarise(arsenic_so=mean(arsenic_so,na.rm=T),
            as_int=mean(as_int),
            mean_total=mean(mean_total,na.rm=T),
            mean_total_county=mean(mean_total_county,na.rm=T),
            mean_total_region=mean(mean_total_region,na.rm=T))%>%
  ungroup()


as_error_town=as_error_town_tot%>%
  filter(assignment_method=='service_maps')

df<-as_error%>%
  select(source,training)%>%
  distinct()


stats_test_greater=df%>%mutate(alternative='greater')
stats_test_lesser=df%>%mutate(alternative='lesser')

#Below are the town averages

for (i in 1:length(t(df[,1]))) {
  stats_test_greater[i,'town_sum_avg']=wilcox.test(as_error_town$mean_total[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],as_error_town$arsenic_so[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'town_sum_avg']=wilcox.test(as_error_town$mean_total[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],as_error_town$arsenic_so[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],alternative = "l",paired=T)$p.value 
  stats_test_greater[i,'int_town_sum_avg']=wilcox.test(as_error_town$as_int[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],as_error_town$arsenic_so[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'int_town_sum_avg']=wilcox.test(as_error_town$as_int[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],as_error_town$arsenic_so[as_error_town$source==df[i,'source']&as_error_town$training==df[i,'training']],alternative = "l",paired=T)$p.value 
}


stats_test=rbind(stats_test_greater,stats_test_lesser)%>%
  mutate(town_sum_avg=round(town_sum_avg,4),
         int_town_sum_avg=round(int_town_sum_avg,4))

#Added to the full dataset
full_stats_test<-full_stats_test%>%
  merge(stats_test)

#Below is county

as_error_county_tot<-as_error_tot%>%
  group_by(county,source,training,assignment_method)%>%
  summarise(arsenic_so=mean(arsenic_so,na.rm=T),
            as_int=mean(as_int),
            mean_total=mean(mean_total,na.rm=T),
            mean_total_county=mean(mean_total_county,na.rm=T),
            mean_total_region=mean(mean_total_region,na.rm=T))%>%
  ungroup()

as_error_county=as_error_county_tot%>%
  filter(assignment_method=='service_maps')

df<-as_error%>%
  select(source,training)%>%
  distinct()


stats_test_greater=df%>%mutate(alternative='greater')
stats_test_lesser=df%>%mutate(alternative='lesser')


as_error_county$arsenic_so[as_error_county$source==stats_test_greater[i,'source']&as_error_county$training==stats_test_greater[i,'training']]

for (i in 1:length(t(df[,1]))) {
  stats_test_greater[i,'county_sum_avg']=wilcox.test(as_error_county$mean_total_county[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],as_error_county$arsenic_so[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'county_sum_avg']=wilcox.test(as_error_county$mean_total_county[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],as_error_county$arsenic_so[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],alternative = "l",paired=T)$p.value 
  stats_test_greater[i,'int_county_sum_avg']=wilcox.test(as_error_county$as_int[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],as_error_county$arsenic_so[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'int_county_sum_avg']=wilcox.test(as_error_county$as_int[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],as_error_county$arsenic_so[as_error_county$source==df[i,'source']&as_error_county$training==df[i,'training']],alternative = "l",paired=T)$p.value 
}


stats_test=rbind(stats_test_greater,stats_test_lesser)%>%
  mutate(county_sum_avg=round(county_sum_avg,4),
         int_county_sum_avg=round(int_county_sum_avg,4))

full_stats_test<-full_stats_test%>%
  merge(stats_test)

#Below is region

as_error_region_tot<-as_error_tot%>%
  group_by(region,source,training,assignment_method)%>%
  summarise(arsenic_so=mean(arsenic_so,na.rm=T),
            as_int=mean(as_int),
            mean_total=mean(mean_total,na.rm=T),
            mean_total_county=mean(mean_total_county,na.rm=T),
            mean_total_region=mean(mean_total_region,na.rm=T))%>%
  ungroup()


as_error_region=as_error_region_tot%>%
  filter(assignment_method=='service_maps')

df<-as_error%>%
  select(source,training)%>%
  distinct()


stats_test_greater=df%>%mutate(alternative='greater')
stats_test_lesser=df%>%mutate(alternative='lesser')


for (i in 1:length(t(df[,1]))) {
  stats_test_greater[i,'region_sum_avg']=wilcox.test(as_error_region$mean_total_region[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],as_error_region$arsenic_so[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'region_sum_avg']=wilcox.test(as_error_region$mean_total_region[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],as_error_region$arsenic_so[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],alternative = "l",paired=T)$p.value 
  stats_test_greater[i,'int_region_sum_avg']=wilcox.test(as_error_region$as_int[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],as_error_region$arsenic_so[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],alternative = "g",paired=T)$p.value 
  stats_test_lesser[i,'int_region_sum_avg']=wilcox.test(as_error_region$as_int[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],as_error_region$arsenic_so[as_error_region$source==df[i,'source']&as_error_region$training==df[i,'training']],alternative = "l",paired=T)$p.value 
}


stats_test=rbind(stats_test_greater,stats_test_lesser)%>%
  mutate(region_sum_avg=round(region_sum_avg,4),
         int_region_sum_avg=round(int_region_sum_avg,4))

full_stats_test<-full_stats_test%>%
  merge(stats_test)

full_stats_test%>%
  arrange(alternative)%>%
  write.csv("Tables\\well_comp_pvals.csv",quote = F,row.names = F)



#ARSENIC HEALTH ASSESSMENT----

#CURRNT CSF AND RFD----
#Below we create the base dataset for the arsenic health assessment
town_as_health<-town_as%>%
  mutate(pph=total_population/parcels,#total_population/total_housing_units,
         pph=ifelse(is.na(pph),2.5,pph),
         well_users=ifelse(assignment_method=='gdb',round(wells_gdb*pph),round(wells_sm*pph)),
         as_int_total=as_int,
         as_town_avg=mean_total)%>%
  select(town,well_users,assignment_method,as_int_total,d_c_noncancer,d_c_cancer_children,d_c_cancer_adults,as_town_avg,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
  distinct()%>%
  mutate(well_users=ifelse(is.na(well_users),0,well_users))%>%
  pivot_longer(cols=c('as_int_total','as_town_avg'),names_to = "Method",values_to = "as_exposed")%>%
  mutate(Source="Private Well",
         population_served=well_users)%>%
  select(Method,assignment_method,as_exposed,Source,population_served,d_c_noncancer,d_c_cancer_children,d_c_cancer_adults,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
  rbind(town_as%>%
          select(town,assignment_method,population_served_CWS,pp_as_exposure_CWS,pp_as_exposure_NTNC,population_served_NTNC,d_c_noncancer,d_c_cancer_children,d_c_cancer_adults,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
          mutate(population_served_CWS=ifelse(is.na(population_served_CWS),0,population_served_CWS))%>%
          distinct()%>%
          pivot_longer(cols=c('pp_as_exposure_CWS','pp_as_exposure_NTNC'),names_to = "Method",values_to = "as_exposed")%>%
          mutate(population_served=ifelse(Method=='pp_as_exposure_CWS',population_served_CWS,0),
                 population_served=ifelse(Method=='pp_as_exposure_NTNC',population_served_NTNC,population_served))%>%
          filter(!is.na(as_exposed))%>%
          mutate(Method=ifelse(Method=='pp_as_exposure_CWS',"CWS Average",Method),
                 Method=ifelse(Method=='pp_as_exposure_NTNC',"NTNC Average",Method),
                 Source='PWS')%>%
          select(Method,assignment_method,as_exposed,Source,population_served,d_c_noncancer,d_c_cancer_children,d_c_cancer_adults,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults))%>%
  mutate(d_non_cancer=d_c_noncancer*population_served*as_exposed,
         d_cancer_children=d_c_cancer_children*population_served*as_exposed,#multiply dose/concentration by the avg town arsenic concnetration for each method
         #to get an average dose, then multiply by the population served to get the total town-wide dose. This is done separate for children and adults
         d_cancer_adults=d_c_cancer_adults*population_served*as_exposed,
         as_cancer_children=1.5*d_cancer_children,#Multiply by the CSF to get cancer cases by town
         as_cancer_adults=1.5*d_cancer_adults)#Multiply by the CSF


#Summary stats of cancer cases
town_as_health%>%
  group_by(Method,assignment_method)%>%#group by different methods
  summarise(Population=sum(population_served),#add pupulation served by each town
            `Children Cancer Cases`=round(sum(as_cancer_children,na.rm=T),1),#calculate the total cancer cases 
            `Adult Cancer Cases`=round(sum(as_cancer_adults,na.rm=T),1),
            `Cases per Million`=round(((`Children Cancer Cases`+`Adult Cancer Cases`)/Population)*1000000,1))%>%
  merge(well_as_dist%>%
  mutate(as_exposed=as_exposed*1000,
         as_exposed=ifelse(is.na(as_exposed),0,as_exposed))%>%
  group_by(Method)%>%
  summarise(`Population Exposed to 10+ ug/L`=sum(as_exposed>10),
         `Cancer Cases from Drinking Water Risk`=round(sum(as_exposed)*10^-5,1)))%>%
  filter(Method!='NTNC Average')%>%
  ungroup()%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         assignment_method=ifelse(assignment_method=='gdb','Well Viewer',assignment_method),
         assignment_method=ifelse(assignment_method=='service_maps','PWS Service Maps',assignment_method))%>%
  write.csv("Tables//state_as_exp.csv",quote=FALSE,row.names = F)


#Non-cancer dataset

#Below we create columns for each town with the upper and lower confidence intrvals of the number of
#wells in each town above each threshold using different methods
#Analysis with the interpolated data was removed, but they remain in for consistency
#The Probability maps were multiplied by 100 and then divided by 100 at the end, the data for ech row
#is the estimated percentage of wells above the threshold in each town
int_wq_uncertainty<-int_wq_as%>%
  group_by(town)%>%
  summarise(over_1=100*sum(as_1_prob)/sum(!is.na(as_1_prob)),
            over_5=100*sum(as_5_prob)/sum(!is.na(as_5_prob)),
            over_10=100*sum(as_10_prob)/sum(!is.na(as_10_prob)),
            bound='actual_prediction',
            method='probability maps',
            n=sum(!is.na(as_1_prob)))%>%
  rbind(int_wq_as%>%
          group_by(town)%>%
          summarise(over_1=100*sum(as_1_lci)/sum(!is.na(as_1_lci)),
                    over_5=100*sum(as_5_lci)/sum(!is.na(as_5_lci)),
                    over_10=100*sum(as_10_lci)/sum(!is.na(as_10_lci)),
                    bound='lower_confidence',
                    method='probability maps',
                    n=sum(!is.na(as_1_lci))))%>%
  rbind(int_wq_as%>%
          group_by(town)%>%
          summarise(over_1=100*sum(as_1_uci)/sum(!is.na(as_1_uci)),
                    over_5=100*sum(as_5_uci)/sum(!is.na(as_5_uci)),
                    over_10=100*sum(as_10_uci)/sum(!is.na(as_10_uci)),
                    bound='upper_confidence',
                    method='probability maps',
                    n=sum(!is.na(as_1_uci))))%>%
  rbind(int_wq_as%>%
          group_by(town)%>%
          summarise(over_1=sum(as_int>=0.001,na.rm=T),
                    over_5=sum(as_int>=0.005,na.rm=T),
                    over_10=sum(as_int>=0.01,na.rm=T),
                    bound='actual_prediction',
                    method='interpolation',
                    n=sum(!is.na(as_int))))%>%
  rbind(int_wq_as%>%
          group_by(town)%>%
          mutate(as_int=as_int-1.96*as_int_se)%>%
          summarise(over_1=sum(as_int>=0.001,na.rm=T),
                    over_5=sum(as_int>=0.005,na.rm=T),
                    over_10=sum(as_int>=0.01,na.rm=T),
                    bound='lower_confidence',
                    method='interpolation',
                    n=sum(!is.na(as_int))))%>%
  rbind(int_wq_as%>%
          group_by(town)%>%
          mutate(as_int=as_int+1.96*as_int_se)%>%
          summarise(over_1=sum(as_int>=0.001,na.rm=T),
                    over_5=sum(as_int>=0.005,na.rm=T),
                    over_10=sum(as_int>=0.01,na.rm=T),
                    bound='upper_confidence',
                    method='interpolation',
                    n=sum(!is.na(as_int))))%>%
  mutate(over_1=over_1/100,
         over_5=over_5/100,
         over_10=over_10/100)%>%
  pivot_wider(values_from = c('over_1','over_5','over_10'),names_from = c('bound'))%>%
  mutate(Method=ifelse(method=='interpolation','as_int_total',method),
         Method=ifelse(method=='probability maps','probability_maps',method),
         assignment_method='gdb')%>%
  select(-method)%>%
  rbind(sm_int_wq_as%>%
          group_by(town)%>%
          summarise(over_1=100*sum(as_1_prob)/sum(!is.na(as_1_prob)),
                    over_5=100*sum(as_5_prob)/sum(!is.na(as_5_prob)),
                    over_10=100*sum(as_10_prob)/sum(!is.na(as_10_prob)),
                    bound='actual_prediction',
                    method='probability maps',
                    n=sum(!is.na(as_1_prob)))%>%
          rbind(sm_int_wq_as%>%
                  group_by(town)%>%
                  summarise(over_1=100*sum(as_1_lci)/sum(!is.na(as_1_lci)),
                            over_5=100*sum(as_5_lci)/sum(!is.na(as_5_lci)),
                            over_10=100*sum(as_10_lci)/sum(!is.na(as_10_lci)),
                            bound='lower_confidence',
                            method='probability maps',
                            n=sum(!is.na(as_1_lci))))%>%
          rbind(sm_int_wq_as%>%
                  group_by(town)%>%
                  summarise(over_1=100*sum(as_1_uci)/sum(!is.na(as_1_uci)),
                            over_5=100*sum(as_5_uci)/sum(!is.na(as_5_uci)),
                            over_10=100*sum(as_prob_10_uci)/sum(!is.na(as_prob_10_uci)),
                            bound='upper_confidence',
                            method='probability maps',
                            n=sum(!is.na(as_1_uci))))%>%
          rbind(sm_int_wq_as%>%
                  group_by(town)%>%
                  summarise(over_1=sum(as_int>=0.001,na.rm=T),
                            over_5=sum(as_int>=0.005,na.rm=T),
                            over_10=sum(as_int>=0.01,na.rm=T),
                            bound='actual_prediction',
                            method='interpolation',
                            n=sum(!is.na(as_int))))%>%
          rbind(sm_int_wq_as%>%
                  group_by(town)%>%
                  mutate(as_int=as_int-1.96*as_int_se)%>%
                  summarise(over_1=sum(as_int>=0.001,na.rm=T),
                            over_5=sum(as_int>=0.005,na.rm=T),
                            over_10=sum(as_int>=0.01,na.rm=T),
                            bound='lower_confidence',
                            method='interpolation',
                            n=sum(!is.na(as_int))))%>%
          rbind(sm_int_wq_as%>%
                  group_by(town)%>%
                  mutate(as_int=as_int+1.96*as_int_se)%>%
                  summarise(over_1=sum(as_int>=0.001,na.rm=T),
                            over_5=sum(as_int>=0.005,na.rm=T),
                            over_10=sum(as_int>=0.01,na.rm=T),
                            bound='upper_confidence',
                            method='interpolation',
                            n=sum(!is.na(as_int))))%>%
          mutate(over_1=over_1/100,
                 over_5=over_5/100,
                 over_10=over_10/100)%>%
          pivot_wider(values_from = c('over_1','over_5','over_10'),names_from = c('bound'))%>%
          mutate(Method=ifelse(method=='interpolation','as_int_total',method),
                 Method=ifelse(method=='probability maps','probability_maps',method),
                 assignment_method='service_maps')%>%
          select(-method))

#This dataset below incorporates all the arsenic estimates for all water sources across all methods
#and calculates the health outcomes for each age interval in each town
#Using the new CSF and RfD
town_as_health_new<-as_summary%>%
  merge(town_parcels%>%
          mutate(town=city,
                 wells_no_pws=1-pws_percent/100)%>%
          select(town,wells_no_pws),all.x=T)%>%
  mutate(wells_no_pws=ifelse(is.na(wells_no_pws),0,wells_no_pws))%>%
  merge(town_as%>%
          select(town,assignment_method,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
          distinct(),all.x=T)%>%
  mutate(pph=total_population/parcels,#total_population/total_housing_units,
         pph=ifelse(is.na(pph),2.5,pph),
         well_users=ifelse(assignment_method=='gdb',round(wells_gdb*pph),round(wells_sm*pph)),
         as_int_total=as_int,
         as_town_avg=mean_total)%>%
  select(town,assignment_method,well_users,as_int_total,as_town_avg,mean_total_county,mean_total_region,starts_with("over"),under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
  mutate(well_users=ifelse(is.na(well_users),0,well_users),
         probability_maps=NA)%>%
  pivot_longer(cols=c('as_int_total','as_town_avg','mean_total_county','mean_total_region','probability_maps'),names_to = "Method",values_to = "as_exposed")%>%
  mutate(Source="Private Well",
         population_served=well_users)%>%
  select(town,assignment_method,Method,as_exposed,Source,population_served,starts_with("over"),under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
  rbind(town_as%>%
          select(town,assignment_method,population_served_CWS,pp_as_exposure_CWS,pp_as_exposure_NTNC,population_served_NTNC,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
          mutate(population_served_CWS=ifelse(is.na(population_served_CWS),0,population_served_CWS))%>%
          pivot_longer(cols=c('pp_as_exposure_CWS','pp_as_exposure_NTNC'),names_to = "Method",values_to = "as_exposed")%>%
          mutate(population_served=ifelse(Method=='pp_as_exposure_CWS',population_served_CWS,0),
                 population_served=ifelse(Method=='pp_as_exposure_NTNC',population_served_NTNC,population_served))%>%
          filter(!is.na(as_exposed))%>%
          mutate(Method=ifelse(Method=='pp_as_exposure_CWS',"CWS Average",Method),
                 Method=ifelse(Method=='pp_as_exposure_NTNC',"NTNC Average",Method),
                 Source='PWS',
                 over_1_int=NA,
                 over_5_int=NA,
                 over_10_int=NA,
                 over_1=NA,
                 over_5=NA,
                 over_10=NA,
                 over_1_town=NA,
                 over_5_town=NA,
                 over_10_town=NA)%>%
          select(town,assignment_method,Method,as_exposed,Source,population_served,starts_with("over"),under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults))%>%
  pivot_longer(cols=c('under_5_years','x5_to_9_years','x10_to_14_years','x15_to_19_years','adults'),names_to = "Age",values_to = "Pop")%>%
  mutate(Pop_served=round(Pop*population_served),
         ir_bw=ifelse(Age=='under_5_years',under_five_ir_bw,0),
         ir_bw=ifelse(Age=='x5_to_9_years',five_to_nine_ir_bw,ir_bw),
         ir_bw=ifelse(Age=='x10_to_14_years',ten_to_fourteen_ir_bw,ir_bw),
         ir_bw=ifelse(Age=='x15_to_19_years',fifteen_to_nineteen_ir_bw,ir_bw),
         ir_bw=ifelse(Age=='adults',adults_ir_bw,ir_bw))%>%
  select(-under_five_ir_bw,-five_to_nine_ir_bw,-ten_to_fourteen_ir_bw,-fifteen_to_nineteen_ir_bw,-adults_ir_bw)%>%
  #below are the health outcome calculations using updated health analysis
  #equations are described in the manuscript
  mutate(dose=ifelse(Age=='adults',1000*as_exposed*ir_bw*(33/78),1000*as_exposed*ir_bw*(21/78)),
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0061,0.0011*dose^2+0.0059*dose),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.0003,dose*0.0003),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0127,0.001*dose^3+0.0037*dose^2+0.0121*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0186,0.003*dose^2+0.0181*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*0.0008,dose*0.0008),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*0.0462,0.0041*dose^3+0.0152*dose^2+0.0437*dose),
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.053,dose*0.053),
         old_csf=dose*1.5/1000,
         cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
         cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
         fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
         fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
         ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
         ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
         fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
         fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
         proposed_rfd=0.031,
         current_rfd=0.3)%>%
  select(-starts_with("over_"))%>%
  merge(int_wq_uncertainty,all.x=T)%>%
  distinct()


as_cases_per_million=town_as_health_new%>%
  mutate(Pop_served=ifelse(Source=="PWS",population_served*Pop,Pop_served),
         assignment_method=ifelse(Source=="PWS",'SDWIS',assignment_method),
         Pop_served=round(Pop_served,0))%>%
  distinct()%>%
  filter(assignment_method!='gdb')%>%
  group_by(Method,Age)%>%
  summarise(Population=sum(Pop_served),
            `Bladder Cancer Cases per Million`=round((sum(bladder_cancer_prob*Pop_served,na.rm=T)/Population)*1000000,1),
            `Bladder Cancer Cases per Million 5th`=round((sum(bladder_cancer_prob_5*Pop_served,na.rm=T)/Population)*1000000,1),
            `Bladder Cancer Cases per Million 95th`=round((sum(bladder_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million`=round((sum(lung_cancer_prob*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million 5th`=round((sum(lung_cancer_prob_5*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million 95th`=round((sum(lung_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Combined Cancer Cases per Million`= `Bladder Cancer Cases per Million`+`Lung Cancer Cases per Million`,
            `Combined Cancer Cases per Million 5th`= `Lung Cancer Cases per Million 5th`+ `Bladder Cancer Cases per Million 5th`,
            `Combined Cancer Cases per Million 95th`=round((sum(combined_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Old Slope Factor Cases per Million`=round((sum(old_csf*Pop_served,na.rm=T)/Population)*1000000,1))%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method),
         Age=ifelse(Age=='under_5_years','<5 years',Age),
         Age=ifelse(Age=='x5_to_9_years','5-9 years',Age),
         Age=ifelse(Age=='x10_to_14_years','10-14 years',Age),
         Age=ifelse(Age=='x15_to_19_years','15-19 years',Age),
         Age=ifelse(Age=='adults','20+ years',Age))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')),
         Age=fct_relevel(Age, c('<5 years','5-9 years','10-14 years','15-19 years','20+ years')))


#Below the data for each method is summed for all age groups/towns
#to get estimated cancer cases across the state for each method
as_cases_per_million2=town_as_health_new%>%
  mutate(Pop_served=ifelse(Source=="PWS",population_served*Pop,Pop_served),
         assignment_method=ifelse(Source=="PWS",'SDWIS',assignment_method),
         Pop_served=round(Pop_served,0))%>%
  distinct()%>%
  group_by(Method,assignment_method)%>%
  summarise(Population=sum(Pop_served),
            `Bladder Cancer Cases per Million`=round((sum(bladder_cancer_prob*Pop_served,na.rm=T)/Population)*1000000,1),
            `Bladder Cancer Cases per Million 5th`=round((sum(bladder_cancer_prob_5*Pop_served,na.rm=T)/Population)*1000000,1),
            `Bladder Cancer Cases per Million 95th`=round((sum(bladder_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million`=round((sum(lung_cancer_prob*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million 5th`=round((sum(lung_cancer_prob_5*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million 95th`=round((sum(lung_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Combined Cancer Cases per Million`= `Bladder Cancer Cases per Million`+`Lung Cancer Cases per Million`,
            `Combined Cancer Cases per Million 5th`= `Lung Cancer Cases per Million 5th`+ `Bladder Cancer Cases per Million 5th`,
            `Combined Cancer Cases per Million 95th`=round((sum(combined_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Old Slope Factor Cases per Million`=round((sum(old_csf*Pop_served,na.rm=T)/Population)*1000000,1))%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  pivot_longer(cols =c('Combined Cancer Cases per Million','Old Slope Factor Cases per Million'),names_to = 'Slope Factor',values_to = 'Cases' )%>%
  mutate(`Slope Factor`=ifelse(`Slope Factor`=='Old Slope Factor Cases per Million','Current Slope Factor','Proposed Slope Factor'),
         `Combined Cancer Cases per Million 95th`=ifelse(`Slope Factor`=='Current Slope Factor',NA,`Combined Cancer Cases per Million 95th`),
         `Combined Cancer Cases per Million 5th`=ifelse(`Slope Factor`=='Current Slope Factor',NA,`Combined Cancer Cases per Million 5th`))


as_new_long<-town_as_health_new%>%
  uncount(Pop_served)%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method),
         Age=ifelse(Age=='under_5_years','<5 years',Age),
         Age=ifelse(Age=='x5_to_9_years','5-9 years',Age),
         Age=ifelse(Age=='x10_to_14_years','10-14 years',Age),
         Age=ifelse(Age=='x15_to_19_years','15-19 years',Age),
         Age=ifelse(Age=='adults','20+ years',Age))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average')),
         Age=fct_relevel(Age, c('<5 years','5-9 years','10-14 years','15-19 years','20+ years')))

#similar datframe, broken up by different cancer types
as_cases_per_million2=town_as_health_new%>%
  distinct()%>%
  group_by(Method,assignment_method)%>%
  summarise(Population=sum(Pop_served),
            `Bladder Cancer Cases per Million`=round((sum(bladder_cancer_prob*Pop_served,na.rm=T)/Population)*1000000,1),
            `Bladder Cancer Cases per Million 95th`=round((sum(bladder_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million`=round((sum(lung_cancer_prob*Pop_served,na.rm=T)/Population)*1000000,1),
            `Lung Cancer Cases per Million 95th`=round((sum(lung_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Combined Cancer Cases per Million`= `Bladder Cancer Cases per Million`+`Lung Cancer Cases per Million`,
            `Combined Cancer Cases per Million 95th`=round((sum(combined_cancer_prob_95*Pop_served,na.rm=T)/Population)*1000000,1),
            `Old Slope Factor Cases per Million`=round((sum(old_csf*Pop_served,na.rm=T)/Population)*1000000,1))%>%
  ungroup()%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average')))%>%
  filter(Method!="probability_maps")


#AS non-cancer Figures and Tables----
#Below are plots of the stade distribution of arsenic doses
labs=town_as_health_new%>%
  filter(assignment_method!='gdb')%>%
  group_by(Method)%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  summarise(n=paste("n=",prettyNum(sum(population_served/5,na.rm=T),big.mark=","),sep=""))


png("Data\\Figures\\as_non_cancer.png",width=10,height=6,res=300,units = "in")
town_as_health_new%>%
  filter(assignment_method!='gdb')%>%
  group_by(Method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0001,dose_nc))%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  ggplot(aes(x = dose_nc)) + 
  geom_histogram(aes(weight = pop_percent,fill=Source),color='black',bins=20)+
  geom_vline(aes(xintercept = proposed_rfd,color="Proposed RfD"),size=1.2)+
  geom_vline(aes(xintercept = current_rfd,color="Current RfD"),size=1.2)+
  geom_text(data=labs,aes(y=0.39,x=0.001,label=n))+
  scale_colour_manual("", 
                      breaks = c("Proposed RfD", "Current RfD"),
                      values = c("darkred", "darkblue")) +
  scale_fill_manual(values = c("coral","darkseagreen1"))+
  scale_x_log10()+
  scale_y_continuous(labels = scales::percent)+
  ylab('Percent of Users')+
  xlab('Dosage (ug/kg-d)')+
  facet_wrap(~Method)+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top")
dev.off()

labs=town_as_health_new%>%
  filter(assignment_method!='service_maps')%>%
  group_by(Method)%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  summarise(n=paste("n=",prettyNum(sum(population_served/5,na.rm=T),big.mark=","),sep=""))

png("Data\\Figures\\as_non_cancer_gdb.png",width=10,height=6,res=300,units = "in")
town_as_health_new%>%
  filter(assignment_method!='service_maps')%>%
  group_by(Method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0001,dose_nc))%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  ggplot(aes(x = dose_nc)) + 
  geom_histogram(aes(weight = pop_percent,fill=Source),color='black',bins=20)+
  geom_vline(aes(xintercept = proposed_rfd,color="Proposed RfD"),size=1.2)+
  geom_vline(aes(xintercept = current_rfd,color="Current RfD"),size=1.2)+
  geom_text(data=labs,aes(y=0.39,x=0.001,label=n))+
  scale_colour_manual("", 
                      breaks = c("Proposed RfD", "Current RfD"),
                      values = c("darkred", "darkblue")) +
  scale_fill_manual(values = c("coral","darkseagreen1"))+
  scale_x_log10()+
  scale_y_continuous(labels = scales::percent)+
  ylab('Percent of Users')+
  xlab('Dosage (ug/kg-d)')+
  facet_wrap(~Method)+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top")
dev.off()
  
labs=town_as_health_new%>%
  filter(assignment_method!='gdb')%>%
  group_by(Method)%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  summarise(n=paste("n=",prettyNum(sum(population_served/5,na.rm=T),big.mark=","),sep=""))


a<-town_as_health_new%>%
  filter(assignment_method!='gdb')%>%
  group_by(Method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0001,dose_nc))%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  ggplot(aes(x = dose_nc)) + 
  geom_histogram(aes(weight = pop_percent,fill=Source),color='black',bins=20)+
  geom_vline(aes(xintercept = proposed_rfd,color="Proposed RfD"),size=1.2)+
  geom_vline(aes(xintercept = current_rfd,color="Current RfD"),size=1.2)+
  geom_text(data=labs,aes(y=0.39,x=0.001,label=n))+
  scale_colour_manual("", 
                      breaks = c("Proposed RfD", "Current RfD"),
                      values = c("darkred", "darkblue")) +
  scale_fill_manual(values = c("coral","darkseagreen1"))+
  scale_x_log10()+
  scale_y_continuous(labels = scales::percent)+
  ylab('Percent of Users')+
  xlab('Dosage (ug/kg-d)')+
  facet_wrap(~Method)+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top")


labs=town_as_health_new%>%
  filter(assignment_method!='service_maps')%>%
  group_by(Method)%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  summarise(n=paste("n=",prettyNum(sum(population_served/5,na.rm=T),big.mark=","),sep=""))


b<-town_as_health_new%>%
  filter(assignment_method!='service_maps')%>%
  group_by(Method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0001,dose_nc))%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  ggplot(aes(x = dose_nc)) + 
  geom_histogram(aes(weight = pop_percent,fill=Source),color='black',bins=20)+
  geom_vline(aes(xintercept = proposed_rfd,color="Proposed RfD"),size=1.2)+
  geom_vline(aes(xintercept = current_rfd,color="Current RfD"),size=1.2)+
  geom_text(data=labs,aes(y=0.39,x=0.001,label=n))+
  scale_colour_manual("", 
                      breaks = c("Proposed RfD", "Current RfD"),
                      values = c("darkred", "darkblue")) +
  scale_fill_manual(values = c("coral","darkseagreen1"))+
  scale_x_log10()+
  scale_y_continuous(labels = scales::percent)+
  ylab('Percent of Users')+
  xlab('Dosage (ug/kg-d)')+
  facet_wrap(~Method)+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top")


png("Data\\Figures\\as_non_cancer_all_methods.png",width=9,height=10,res=300,units = "in")

ggarrange(
  a, b,labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom",
  ncol = 1
)
dev.off()

png("Data\\Figures\\as_non_cancer_all_sm.png",width=9,height=6,res=300,units = "in")
a
dev.off()

#Create Summary Tables
non_cancer_as2<-town_as_health_new%>%
  mutate(dose_nc=ifelse(dose_nc==0,0.0001,dose_nc),
         HQ_Current=dose_nc/current_rfd,
         HQ_Proposed=dose_nc/proposed_rfd)%>%
  mutate(`Well Assignment Method`=assignment_method)%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method),
         `Well Assignment Method`=ifelse(`Well Assignment Method`=="gdb","MassDEP DWP Well Viewer",`Well Assignment Method`),
         `Well Assignment Method`=ifelse(`Well Assignment Method`=="service_maps","MassDEP PWS Service Area Maps",`Well Assignment Method`),
         `Well Assignment Method`=ifelse(Method=="CWS Average","SDWIS",`Well Assignment Method`),
         `Well Assignment Method`=ifelse(Method=="NTNC Average","SDWIS",`Well Assignment Method`))%>%
  select(-assignment_method)%>%
  distinct()%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  group_by(Method, `Well Assignment Method`)%>%
  summarise(`Total Population Served`=sum(population_served/5,na.rm=T),
            `Population Over 1 HQ Current RfD`=paste(prettyNum(sum(Pop_served[HQ_Current>1],na.rm=T),big.mark=",")," (",round((100*sum(Pop_served[HQ_Current>1],na.rm=T))/`Total Population Served`,1),"\\%)",sep=""),
            `Population Over 1 HQ Proposed RfD`=paste(prettyNum(sum(Pop_served[HQ_Proposed>1],na.rm=T),big.mark=",")," (",round((100*sum(Pop_served[HQ_Proposed>1],na.rm=T))/`Total Population Served`,1),"\\%)",sep=""))%>%
  mutate(`Total Population Served`=prettyNum(`Total Population Served`,big.mark=","))%>%
  filter(Method!="NTNC Average")
  
write.table(non_cancer_as2,"Tables\\non_cancer_as2.csv",row.names = F,quote = F,sep=";")

non_cancer_as2%>%
  filter(`Well Assignment Method`!="MassDEP DWP Well Viewer")%>%
  ungroup()%>%
  select(-`Well Assignment Method`)%>%
  write.table("Tables\\non_cancer_as2_cut.csv",row.names = F,quote = F,sep=";")

non_cancer_as_risk<-town_as_health_new%>%
  mutate(dose_nc=ifelse(dose_nc==0,0.0001,dose_nc),
         HQ_Current=dose_nc/current_rfd,
         HQ_Proposed=dose_nc/proposed_rfd)%>%
  mutate(`Well Assignment Method`=assignment_method)%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method),
         `Well Assignment Method`=ifelse(`Well Assignment Method`=="gdb","MassDEP DWP Well Viewer",`Well Assignment Method`),
         `Well Assignment Method`=ifelse(`Well Assignment Method`=="service_maps","MassDEP PWS Service Area Maps",`Well Assignment Method`),
         `Well Assignment Method`=ifelse(Method=="CWS Average"|Method=="NTNC Average","SDWIS",`Well Assignment Method`))%>%
  select(-assignment_method)%>%
  distinct()%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  group_by(Method, `Well Assignment Method`)%>%
  summarise(`Total Population Served`=sum(population_served/5,na.rm=T),
            `Average Extra CVD Incidence Rate (95th percentile) (per 10000)`=paste(round(10000*(sum(cvd_extra_risk*Pop_served,na.rm=T)/`Total Population Served`),2)," (",round(10000*(sum(cvd_extra_risk_95*Pop_served,na.rm=T)/`Total Population Served`),2),")",sep=""),
            `Average Extra Fatal CVD Incidence Rate (95th percentile) (per 10000)`=paste(round(10000*(sum(fatal_cvd_extra_risk*Pop_served,na.rm=T)/`Total Population Served`),2)," (",round(10000*(sum(fatal_cvd_extra_risk_95*Pop_served,na.rm=T)/`Total Population Served`),2),")",sep=""),
            `Average Extra IHD Incidence Rate (95th percentile) (per 10000)`=paste(round(10000*(sum(ihd_extra_risk*Pop_served,na.rm=T)/`Total Population Served`),2)," (",round(10000*(sum(ihd_extra_risk_95*Pop_served,na.rm=T)/`Total Population Served`),2),")",sep=""),
            `Average Extra Fatal IHD Incidence Rate (95th percentile) (per 10000)`=paste(round(10000*(sum(fatal_ihd_extra_risk*Pop_served,na.rm=T)/`Total Population Served`),2)," (",round(10000*(sum(fatal_ihd_extra_risk_95*Pop_served,na.rm=T)/`Total Population Served`),2),")",sep=""))%>%
  filter(Method!='NTNC Average')%>%
  mutate(`Total Population Served`=prettyNum(`Total Population Served`,big.mark=","))

write.table(non_cancer_as_risk,"Tables\\non_cancer_as_risk.csv",row.names = F,quote = F,sep=";")

non_cancer_as_risk%>%
  ungroup()%>%
  filter(`Well Assignment Method`!="MassDEP DWP Well Viewer")%>%
  select(-`Well Assignment Method`)%>%
  write.table("Tables\\non_cancer_as_risk_cut.csv",row.names = F,quote = F,sep=";")

#Arsenic Threshold based analysis----

#Below, we do the same analysis, except instead of concentration data
#we calculate the percentage of wells above 1, 5, and 10 ug/L
town_as_health_threshold<-as_summary%>%
  merge(town_parcels%>%
          mutate(town=city,
                 wells_no_pws=1-pws_percent/100)%>%
          select(town,wells_no_pws),all.x=T)%>%
  mutate(wells_no_pws=ifelse(is.na(wells_no_pws),0,wells_no_pws))%>%
  merge(town_as%>%
          select(town,assignment_method,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
          distinct(),all.x=T)%>%
  mutate(pph=total_population/parcels,#total_population/total_housing_units,
         pph=ifelse(is.na(pph),2.5,pph),
         well_users=ifelse(assignment_method=='gdb',round(wells_gdb*pph),round(wells_sm*pph)),
         as_int_total=as_int,
         as_town_avg=mean_total)%>%
  select(town,assignment_method,well_users,as_int_total,as_town_avg,mean_total_county,mean_total_region,starts_with("over"),starts_with("percent"),under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
  mutate(well_users=ifelse(is.na(well_users),0,well_users),
         probability_maps=NA)%>%
  pivot_longer(cols=c('as_int_total','as_town_avg','mean_total_county','mean_total_region','probability_maps'),names_to = "Method",values_to = "as_exposed")%>%
  mutate(Source="Private Well",
         population_served=well_users)%>%
  select(town,assignment_method,Method,as_exposed,Source,population_served,starts_with("over"),starts_with("percent"),under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
  rbind(town_as%>%
          select(town,assignment_method,population_served_CWS,pp_as_exposure_CWS,pp_as_exposure_NTNC,population_served_NTNC,under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults)%>%
          mutate(population_served_CWS=ifelse(is.na(population_served_CWS),0,population_served_CWS))%>%
          pivot_longer(cols=c('pp_as_exposure_CWS','pp_as_exposure_NTNC'),names_to = "Method",values_to = "as_exposed")%>%
          mutate(population_served=ifelse(Method=='pp_as_exposure_CWS',population_served_CWS,0),
                 population_served=ifelse(Method=='pp_as_exposure_NTNC',population_served_NTNC,population_served))%>%
          filter(!is.na(as_exposed))%>%
          mutate(Method=ifelse(Method=='pp_as_exposure_CWS',"CWS Average",Method),
                 Method=ifelse(Method=='pp_as_exposure_NTNC',"NTNC Average",Method),
                 Source='PWS',
                 over_1_int=ifelse(as_exposed>0.001,1,0),
                 over_5_int=ifelse(as_exposed>0.005,1,0),
                 over_10_int=ifelse(as_exposed>0.01,1,0),
                 over_1=ifelse(as_exposed>0.001,1,0),
                 over_5=ifelse(as_exposed>0.005,1,0),
                 over_10=ifelse(as_exposed>0.01,1,0),
                 over_1_town=ifelse(as_exposed>0.001,1,0),
                 over_5_town=ifelse(as_exposed>0.005,1,0),
                 over_10_town=ifelse(as_exposed>0.01,1,0),
                 percent_over_one_county_county=ifelse(as_exposed>0.001,1,0),
                 percent_over_five_county_county= ifelse(as_exposed>0.005,1,0),
                 percent_over_ten_county_county=ifelse(as_exposed>0.01,1,0),
                 percent_over_one_region_region= ifelse(as_exposed>0.001,1,0),
                 percent_over_five_region_region=ifelse(as_exposed>0.005,1,0),
                 percent_over_ten_region_region=ifelse(as_exposed>0.01,1,0)
                 )%>%
          select(town,assignment_method,Method,as_exposed,Source,population_served,starts_with("over"),starts_with("percent"),under_five_ir_bw,five_to_nine_ir_bw,ten_to_fourteen_ir_bw,fifteen_to_nineteen_ir_bw,adults_ir_bw,under_5_years,x5_to_9_years,x10_to_14_years,x15_to_19_years,adults))%>%
  pivot_longer(cols=c('under_5_years','x5_to_9_years','x10_to_14_years','x15_to_19_years','adults'),names_to = "Age",values_to = "Pop")%>%
  mutate(ir_bw=ifelse(Age=='under_5_years',under_five_ir_bw,0),
         ir_bw=ifelse(Age=='x5_to_9_years',five_to_nine_ir_bw,ir_bw),
         ir_bw=ifelse(Age=='x10_to_14_years',ten_to_fourteen_ir_bw,ir_bw),
         ir_bw=ifelse(Age=='x15_to_19_years',fifteen_to_nineteen_ir_bw,ir_bw),
         ir_bw=ifelse(Age=='adults',adults_ir_bw,ir_bw))%>%
  select(-under_five_ir_bw,-five_to_nine_ir_bw,-ten_to_fourteen_ir_bw,-fifteen_to_nineteen_ir_bw,-adults_ir_bw)%>%
  merge(int_wq_uncertainty,all.x=T)%>%
  distinct()

#cleaning column names
colnames(town_as_health_threshold)<-gsub("_county_county","_county",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("region_region","region",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("percent_over","over",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("_one_","_1_",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("_five_","_5_",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("_ten_","_10_",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("_actual_prediction","_probabilitymaps",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("_upper_confidence","_probabilitymapsui",colnames(town_as_health_threshold))
colnames(town_as_health_threshold)<-gsub("_lower_confidence","_probabilitymapsli",colnames(town_as_health_threshold))

#Below we pivor the data so that there is ll the percentages/ averages are in one column
#split between rows based on methods
town_as_health_threshold_clean<-town_as_health_threshold%>%
  mutate(Source=ifelse(Source=="PWS",Method,Source))%>%
  select(-over_1,-over_5,-over_10,-Method)%>%
  pivot_longer(cols = c(starts_with("over")), names_to = c("threshold","method"), 
               names_pattern ="over_(.*)_(.*)",values_to = "percent")%>%
  select(-n)%>%
  mutate(as_exposed=ifelse(threshold==1,0.001,as_exposed),
         as_exposed=ifelse(threshold==5,0.005,as_exposed),
         as_exposed=ifelse(threshold==10,0.01,as_exposed),
         method=ifelse(Source!='Private Well',Source,method))%>%
  filter(!is.na(percent))%>%
  distinct()%>%
  mutate(dose=ifelse(Age=='adults',1000*as_exposed*ir_bw*(33/78),1000*as_exposed*ir_bw*(21/78)),
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0061,0.0011*dose^2+0.0059*dose),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.0003,dose*0.0003),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0127,0.001*dose^3+0.0037*dose^2+0.0121*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0186,0.003*dose^2+0.0181*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*0.0008,dose*0.0008),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*0.0462,0.0041*dose^3+0.0152*dose^2+0.0437*dose),
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.053,dose*0.053),
         proposed_csf=dose*53/1000,
         old_csf=dose*1.5/1000,
         cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
         cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
         fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
         fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
         ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
         ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
         fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
         fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
         proposed_rfd=0.031,
         current_rfd=0.3)%>%
  distinct()%>%
  mutate(Pop_served=round(population_served*Pop))

lab=paste(prettyNum(sum(town_as_health_threshold_clean%>%
                          filter(assignment_method!='service_maps')%>%
                          ungroup()%>%
                          filter(Source=='Private Well')%>%
                          select(town,Source,population_served)%>%
                          distinct()%>%
                          select(population_served)%>%
                          as.matrix()),big.mark = ","),"well users")

#GDB Threshold Method, New CSF
png("Data\\Figures\\as_cancer_threshold_method_gdb.png",width=9,height=6,res=300,units = "in")
town_as_health_threshold_clean%>%
  filter(assignment_method!='service_maps')%>%
  filter(!is.na(percent))%>%
  group_by(threshold,method,Source)%>%
  summarise(pop=sum(Pop_served,na.rm=T),
            cumulative_cancer_cases=sum(Pop_served*percent*proposed_csf,na.rm=T),
            per_million_cancer_cases=1000000*(cumulative_cancer_cases/pop))%>%
  ungroup()%>%
  #filter(Source!="PWS")%>%
  group_by(threshold,Source)%>%
mutate(li=mean(per_million_cancer_cases[method=="probabilitymapsli"]),
       ui=mean(per_million_cancer_cases[method=="probabilitymapsui"]),
       li=ifelse(method=="probabilitymaps",li,NA),
       ui=ifelse(method=="probabilitymaps",ui,NA))%>%
  filter(method!="probabilitymapsli"&method!="probabilitymapsui")%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),)%>%
  mutate(method=fct_relevel(method, c('Interpolated','Town Average','County Average','Region Average','Probability Maps: Lower Confidence','Probability Maps','Probability Maps: Upper Confidence')),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         Method2=ifelse(grepl("Probability",method),"Probability Maps Confidence",""),
         Method2=ifelse(grepl("CWS",method),"PWS",Method2),
         Method2=ifelse(grepl("NTNC",method),"PWS",Method2),
         Method2=fct_relevel(as.factor(Method2),"PWS","","Probability Maps Confidence"))%>%
ggplot(aes(x=threshold,fill=method))+
  geom_col(aes(y=ui),position = "dodge",size=1.1,alpha=.25)+
  geom_col(aes(y=li),position = "dodge",size=1.1)+
  geom_col(aes(y=per_million_cancer_cases),color='black',position = "dodge",size=.9,alpha=.5)+
  theme_bw()+
  ylab("Per Million Attributable Cancer Cases")+
  xlab("Threshold Used")+
  geom_text(aes(x="5",y=800,label=lab),size=8)+
  theme(text=element_text(size = 16),legend.title = element_blank(),legend.position = "top")
dev.off()

#SM Threshold Method, New CSF
lab=paste(prettyNum(sum(town_as_health_threshold_clean%>%
                          filter(assignment_method!='gdb')%>%
                          ungroup()%>%
                          filter(Source=='Private Well')%>%
                          select(town,Source,population_served)%>%
                          distinct()%>%
                          select(population_served)%>%
                          as.matrix()),big.mark = ","),"well users")

png("Data\\Figures\\as_cancer_threshold_method_sm.png",width=9,height=6,res=300,units = "in")
town_as_health_threshold_clean%>%
  filter(assignment_method!='gdb')%>%
  filter(!is.na(percent))%>%
  group_by(threshold,method,Source)%>%
  summarise(pop=sum(Pop_served,na.rm=T),
            cumulative_cancer_cases=sum(Pop_served*percent*proposed_csf,na.rm=T),
            per_million_cancer_cases=1000000*(cumulative_cancer_cases/pop))%>%
  ungroup()%>%
  #filter(Source!="PWS")%>%
  group_by(threshold,Source)%>%
  mutate(li=mean(per_million_cancer_cases[method=="probabilitymapsli"]),
         ui=mean(per_million_cancer_cases[method=="probabilitymapsui"]),
         li=ifelse(method=="probabilitymaps",li,NA),
         ui=ifelse(method=="probabilitymaps",ui,NA))%>%
  filter(method!="probabilitymapsli"&method!="probabilitymapsui")%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),)%>%
  mutate(method=fct_relevel(method, c('Interpolated','Town Average','County Average','Region Average','Probability Maps: Lower Confidence','Probability Maps','Probability Maps: Upper Confidence')),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         Method2=ifelse(grepl("Probability",method),"Probability Maps Confidence",""),
         Method2=ifelse(grepl("CWS",method),"PWS",Method2),
         Method2=ifelse(grepl("NTNC",method),"PWS",Method2),
         Method2=fct_relevel(as.factor(Method2),"PWS","","Probability Maps Confidence"))%>%
  ggplot(aes(x=threshold,fill=method))+
  geom_col(aes(y=ui),position = "dodge",size=1.1,alpha=.25)+
  geom_col(aes(y=li),position = "dodge",size=1.1)+
  geom_col(aes(y=per_million_cancer_cases),color='black',position = "dodge",size=.9,alpha=.5)+
  theme_bw()+
  ylab("Per Million Attributable Cancer Cases")+
  xlab("Threshold Used")+
  geom_text(aes(x="5",y=800,label=lab),size=8)+
  theme(text=element_text(size = 16),legend.title = element_blank(),legend.position = "top")
dev.off()


#Number of cancer cases by age, base on each thrshold and method
town_as_health_threshold_clean%>%
  select(threshold,Age,dose_nc,dose,old_csf,proposed_csf)%>%
  distinct()%>%
  mutate(dose_nc=round(dose_nc,3),
         dose=round(dose,3),
         old_csf=round(old_csf*1000000, 1),
         proposed_csf=round(proposed_csf*1000000, 1),
         Age=ifelse(Age=='under_5_years','0-4 years',Age),
         Age=ifelse(Age=='x5_to_9_years','5-9 years',Age),
         Age=ifelse(Age=='x10_to_14_years','10-14 years',Age),
         Age=ifelse(Age=='x15_to_19_years','15-19 years',Age),
         Age=ifelse(Age=='adults','20+ years',Age))%>%
  write.csv("Tables\\as_cancer_threshold_nums.csv",quote = F,row.names = F)


town_as_health_threshold_clean2<-town_as_health_threshold%>%
  mutate(Source=ifelse(Source=="PWS",Method,Source))%>%
  select(-over_1,-over_5,-over_10,-Method)%>%
  pivot_longer(cols = c(starts_with("over")), names_to = c("threshold","method"), 
               names_pattern ="over_(.*)_(.*)",values_to = "percent")%>%
  select(-n)%>%
  mutate(as_exposed=ifelse(threshold==1,0.001,as_exposed),
         as_exposed=ifelse(threshold==5,0.005,as_exposed),
         as_exposed=ifelse(threshold==10,0.01,as_exposed),
         method=ifelse(Source!='Private Well',Source,method))%>%
  filter(!is.na(percent))%>%
  distinct()%>%
  group_by(town,assignment_method,Age,method)%>%
  mutate(percent=ifelse(threshold==1,2*percent-sum(percent),percent),
         percent=ifelse(threshold==5,percent-mean(percent[threshold==10]),percent),
         percent=ifelse(percent<0,0,percent))%>%
  mutate(dose=ifelse(Age=='adults',1000*as_exposed*ir_bw*(33/78),1000*as_exposed*ir_bw*(21/78)),
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0061,0.0011*dose^2+0.0059*dose),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.0003,dose*0.0003),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0127,0.001*dose^3+0.0037*dose^2+0.0121*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0186,0.003*dose^2+0.0181*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*0.0008,dose*0.0008),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*0.0462,0.0041*dose^3+0.0152*dose^2+0.0437*dose),
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.053,dose*0.053),
         proposed_csf=dose*53/1000,
         old_csf=dose*1.5/1000,
         cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
         cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
         fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
         fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
         ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
         ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
         fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
         fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
         proposed_rfd=0.031,
         current_rfd=0.3,
         Pop_served=round(population_served*Pop))


png("Data\\Figures\\as_cancer_threshold_method_methods.png",width=10,height=14,res=300,units = "in")
town_as_health_threshold_clean2%>%
  filter(!is.na(percent))%>%
  mutate(`Well Assignment Method`=assignment_method)%>%
  #pivot_longer(cols = c('Pop_gdb','Pop_service_maps'),names_to = 'Well Assignment Method',values_to = 'Pop_gdb')%>%
  group_by(method,Source,`Well Assignment Method`)%>%
  summarise(pop=sum(Pop_served,na.rm=T),
            Current_CSF_cases_50=sum(Pop_served*percent*old_csf,na.rm=T),
            Proposed_Bladder_CSF_cases_5=sum(Pop_served*percent*bladder_cancer_prob_5,na.rm=T),
            Proposed_Bladder_CSF_cases_50=sum(Pop_served*percent*bladder_cancer_prob,na.rm=T),
            Proposed_Bladder_CSF_cases_95=sum(Pop_served*percent*bladder_cancer_prob_95,na.rm=T),
            Proposed_Lung_CSF_cases_5=sum(Pop_served*percent*lung_cancer_prob_5,na.rm=T),
            Proposed_Lung_CSF_cases_50=sum(Pop_served*percent*lung_cancer_prob,na.rm=T),
            Proposed_Lung_CSF_cases_95=sum(Pop_served*percent*lung_cancer_prob_95,na.rm=T),
            Proposed_Combined_CSF_cases_50=sum(Pop_served*percent*proposed_csf,na.rm=T),
            #proposed_csf_cases_5=lung_cases_5+bladder_cases_5,
            #proposed_csf_cases_95=lung_cases_95+bladder_cases_95
            )%>%
  ungroup()%>%
  pivot_longer(cols = c(ends_with("50"),ends_with("5"),ends_with("95")), names_to = c("csf","ci"), 
               names_pattern ="(.*)_cases_(.*)",values_to = "cases")%>%
  mutate(cases_per_million=1000000*(cases/pop))%>%
  pivot_wider(names_from = ci,values_from = c(cases_per_million,cases))%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps: Lower Confidence',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps: Upper Confidence',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         `Well Assignment Method`=ifelse( `Well Assignment Method`=='gdb','MassDEP Well Viewer',`Well Assignment Method`),
         `Well Assignment Method`=ifelse( `Well Assignment Method`=='service_maps','MassDEP PWS Service Maps',`Well Assignment Method`))%>%
  mutate(method=fct_relevel(method, c('Interpolated','Town Average','County Average','Region Average','Probability Maps: Lower Confidence','Probability Maps','Probability Maps: Upper Confidence')),
        # threshold=as.factor(threshold),
         #threshold=fct_relevel(threshold,c("1","5","10")),
         Method2=ifelse(grepl("Probability",method),"Probability Maps (95%)","Other Methods"),
        csf=as.factor(gsub("_"," ",csf)),
        csf=fct_relevel(csf,'Proposed Bladder CSF','Proposed Lung CSF','Proposed Combined CSF','Current CSF'))%>%
  mutate(divide_min=ifelse(method=='Probability Maps: Lower Confidence',0,NA),
         divide_max=ifelse(method=='Probability Maps: Lower Confidence',max(cases_50,na.rm=T),NA))%>%
  #view()
  filter(method!="NTNC Average")%>%
  #view()
  ggplot(aes(x=method,color=`Well Assignment Method`))+
  geom_linerange(aes(xmin=method,xmax=method,ymin=cases_5,ymax=cases_95),position = position_dodge(width = .75))+
  geom_point(aes(y=cases_50),size=4,shape=18,position = position_dodge(width = 0.75))+
  geom_point(aes(y=cases_5),shape="|",size=4,position = position_dodge(width = 0.75))+
  geom_point(aes(y=cases_95),shape="|",size=4,position = position_dodge(width = 0.75))+
  facet_wrap(~csf,ncol = 1)+
  scale_color_brewer(palette = "Accent")+
  xlab('')+
  ylab('Statewide Attributable Cases')+
  theme_classic()+
  coord_flip()+
  theme(legend.title = element_blank(),legend.position = "top",text = element_text(size=18))
dev.off()


lab=paste(prettyNum(sum(town_as_health_threshold_clean2%>%
                ungroup()%>%
                filter(Source=='Private Well')%>%
                filter(assignment_method!='service_maps')%>%
                select(town,population_served)%>%
                distinct()%>%
                select(population_served)%>%
                as.matrix()),big.mark = ","),"well users")


png("Data\\Figures\\as_cancer_threshold_method_stacked_gdb.png",width=12.5,height=8,res=300,units = "in")
town_as_health_threshold_clean2%>%
  filter(!is.na(percent)&assignment_method!='service_maps')%>%
  group_by(threshold,method,Source)%>%
  summarise(pop=sum(Pop_served,na.rm=T),
            cumulative_cancer_cases=sum(Pop_served*percent*proposed_csf,na.rm=T),
            per_million_cancer_cases=1000000*(cumulative_cancer_cases/pop))%>%
  ungroup()%>%
  group_by(threshold,Source)%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps: Lower Confidence',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps: Upper Confidence',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method))%>%
  mutate(method=fct_relevel(method, c('Interpolated','Town Average','County Average','Region Average','Probability Maps: Lower Confidence','Probability Maps','Probability Maps: Upper Confidence')),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         Method2=ifelse(grepl("Probability",method),"Probability Maps Confidence",""),
         Method2=ifelse(grepl("CWS",method),"PWS",Method2),
         Method2=ifelse(grepl("NTNC",method),"PWS",Method2),
         Method2=fct_relevel(as.factor(Method2),"PWS","","Probability Maps Confidence"))%>%
  filter(method!="NTNC Average")%>%
  group_by(method)%>%
  mutate(per_million_cancer_cases_10=ifelse(threshold==10,sum(per_million_cancer_cases),NA),
         per_million_cancer_cases_5=ifelse(threshold==5,sum(per_million_cancer_cases)-mean(per_million_cancer_cases[threshold==10]),NA),
         per_million_cancer_cases_1=ifelse(threshold==1,mean(per_million_cancer_cases[threshold==1]),NA),
         outline=mean(per_million_cancer_cases_10,na.rm=T),
         Method=method)%>%
  ggplot(aes(x=Method2,fill=Method),pattern_key_scale_factor=.5)+
  geom_col_pattern(aes(y=per_million_cancer_cases_10,pattern='10'),position = "dodge",size=.9,pattern_spacing=0.02,color='black')+
  geom_col_pattern(aes(y=per_million_cancer_cases_5,pattern='5'),position = "dodge",size=.9,pattern_spacing=0.02,color='black')+
  geom_col_pattern(aes(y=per_million_cancer_cases_1,pattern='1'),position = "dodge",size=.9,color='black')+
  scale_pattern_manual(name = 'Threshold', 
                       values =c('10'='wave','5'='stripe','1'='none'),
                       breaks=c("1", "5", "10"))+
  #guides(fill = guide_legend(nrow = 4,override.aes = list(pattern = "none")),pattern = guide_legend(nrow = 3,override.aes = list(fill = "white")))+
  guides(fill = guide_legend(nrow = 4,override.aes = list(pattern = "none")),pattern = guide_legend(nrow = 3,override.aes = list(fill = "white")))+
  #geom_col(aes(y=outline),color='black',position = "dodge",size=.9,alpha=0)+
  theme_classic()+
  ylab("Per Million Attributable Cancer Cases")+
  xlab("")+
  #geom_text(aes(x="",y=900,label=lab),size=8)+
  theme(text=element_text(size = 16),legend.position = "top",legend.key.size = unit(1, 'cm'))
dev.off()

lab=paste(prettyNum(sum(town_as_health_threshold_clean2%>%
                          ungroup()%>%
                          filter(Source=='Private Well')%>%
                          filter(assignment_method!='gdb')%>%
                          select(town,population_served)%>%
                          distinct()%>%
                          select(population_served)%>%
                          as.matrix()),big.mark = ","),"well users")

png("Data\\Figures\\as_cancer_threshold_method_stacked_sm.png",width=12.5,height=8,res=300,units = "in")
town_as_health_threshold_clean2%>%
  filter(!is.na(percent)&assignment_method!='gdb')%>%
  group_by(threshold,method,Source)%>%
  summarise(pop=sum(Pop_served,na.rm=T),
            cumulative_cancer_cases=sum(Pop_served*percent*proposed_csf,na.rm=T),
            per_million_cancer_cases=1000000*(cumulative_cancer_cases/pop))%>%
  ungroup()%>%
  #filter(Source!="PWS")%>%
  group_by(threshold,Source)%>%
  # mutate(li=mean(per_million_cancer_cases[method=="probabilitymapsli"]),
  #        ui=mean(per_million_cancer_cases[method=="probabilitymapsui"]),
  #        li=ifelse(method=="probabilitymaps",li,NA),
  #        ui=ifelse(method=="probabilitymaps",ui,NA))%>%
  #filter(method!="probabilitymapsli"&method!="probabilitymapsui")%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps: Lower Confidence',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps: Upper Confidence',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method))%>%
  mutate(method=fct_relevel(method, c('Interpolated','Town Average','County Average','Region Average','Probability Maps: Lower Confidence','Probability Maps','Probability Maps: Upper Confidence')),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         Method2=ifelse(grepl("Probability",method),"Probability Maps Confidence",""),
         Method2=ifelse(grepl("CWS",method),"PWS",Method2),
         Method2=ifelse(grepl("NTNC",method),"PWS",Method2),
         Method2=fct_relevel(as.factor(Method2),"PWS","","Probability Maps Confidence"))%>%
  filter(method!="NTNC Average")%>%
  group_by(method)%>%
  mutate(per_million_cancer_cases_10=ifelse(threshold==10,sum(per_million_cancer_cases),NA),
         per_million_cancer_cases_5=ifelse(threshold==5,sum(per_million_cancer_cases)-mean(per_million_cancer_cases[threshold==10]),NA),
         per_million_cancer_cases_1=ifelse(threshold==1,mean(per_million_cancer_cases[threshold==1]),NA),
         outline=mean(per_million_cancer_cases_10,na.rm=T),
         Method=method)%>%
  ggplot(aes(x=Method2,fill=Method),pattern_key_scale_factor=.5)+
  #geom_col(aes(y=ui),position = "dodge",size=1.1,alpha=.25)+
  #geom_col(aes(y=li),position = "dodge",size=1.1)+
  geom_col_pattern(aes(y=per_million_cancer_cases_10,pattern='10'),position = "dodge",size=.9,pattern_spacing=0.02,color='black')+
  geom_col_pattern(aes(y=per_million_cancer_cases_5,pattern='5'),position = "dodge",size=.9,pattern_spacing=0.02,color='black')+
  geom_col_pattern(aes(y=per_million_cancer_cases_1,pattern='1'),position = "dodge",size=.9,color='black')+
  scale_pattern_manual(name = 'Threshold', 
                       values =c('10'='wave','5'='stripe','1'='none'),
                       breaks=c("1", "5", "10"))+
  #guides(fill = guide_legend(nrow = 4,override.aes = list(pattern = "none")),pattern = guide_legend(nrow = 3,override.aes = list(fill = "white")))+
  guides(fill = guide_legend(nrow = 4,override.aes = list(pattern = "none")),pattern = guide_legend(nrow = 3,override.aes = list(fill = "white")))+
  #geom_col(aes(y=outline),color='black',position = "dodge",size=.9,alpha=0)+
  theme_classic()+
  ylab("Per Million Attributable Cancer Cases")+
  xlab("")+
  #geom_text(aes(x="",y=900,label=lab),size=8)+
  theme(text=element_text(size = 16),legend.position = "top",legend.key.size = unit(1, 'cm'))
dev.off()


town_as_health_threshold_clean2%>%
  filter(!is.na(percent))%>%
  group_by(method,threshold,assignment_method)%>%
  summarise(pop=sum(Pop_served,na.rm=T),
            cumulative_cancer_cases=round(sum(Pop_served*percent*proposed_csf,na.rm=T)),
            per_million_cancer_cases=round(1000000*(cumulative_cancer_cases/pop),1))%>%
  ungroup()%>%
  group_by(assignment_method,method)%>%
  mutate(percent=round(100*cumulative_cancer_cases/sum(cumulative_cancer_cases),1))%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps: LCI',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps: UCI',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=as.factor(method),
         assignment_method=ifelse(assignment_method=="gdb","MassDEP DWP Well Viewer",assignment_method),
         assignment_method=ifelse(assignment_method=="service_maps","MassDEP PWS Service Area Maps",assignment_method)
         )%>%
  mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps: LCI','Probability Maps','Probability Maps: UCI')),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")))%>%
  arrange(assignment_method,method,threshold)%>%
  mutate_each(funs(prettyNum(., big.mark=",")))%>%
  write.table("Tables\\threshold_percent.csv",quote = F,row.names = F,sep=";")


#THRESHOLD NO THRESHOLD COMPARISON----
all_as_canc<-town_as_health_threshold_clean2%>%
  filter(!is.na(percent))%>%
  distinct()%>%
  group_by(method,assignment_method)%>%
  summarise(`Calculation Method`='Threshold',
            pop=sum(Pop_served,na.rm=T)/3,
            cumulative_cancer_cases_current_csf=sum(Pop_served*percent*old_csf,na.rm=T),
            per_million_cancer_cases_Current_CSF=1000000*(cumulative_cancer_cases_current_csf/pop),
            cumulative_cancer_cases_proposed_csf=sum(Pop_served*percent*proposed_csf,na.rm=T),
            per_million_cancer_cases_Proposed_CSF=1000000*(cumulative_cancer_cases_proposed_csf/pop))%>%
  ungroup()%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps: Lower Confidence',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps: Upper Confidence',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=as.factor(method))%>%
  mutate(method=fct_relevel(method, c('CWS Average', 'NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps: Lower Confidence','Probability Maps','Probability Maps: Upper Confidence')),
         Method2=ifelse(grepl("Probability",method),"Probability Maps Confidence",""),
         Method2=ifelse(grepl("CWS",method),"PWS",Method2),
         Method2=ifelse(grepl("NTNC",method),"PWS",Method2),
         Method2=fct_relevel(as.factor(Method2),"PWS","","Probability Maps Confidence"))%>%
  rbind(town_as_health_new%>%
    mutate(Population=round(Pop_served,0),
           method=Method)%>%
    distinct()%>%
    group_by(method,assignment_method)%>%
    summarise(`Calculation Method`='Assigned Value',
              pop=sum(Pop_served,na.rm=T),
              cumulative_cancer_cases_current_csf=sum(Pop_served*old_csf,na.rm=T),
              per_million_cancer_cases_Current_CSF=1000000*(cumulative_cancer_cases_current_csf/pop),
              cumulative_cancer_cases_proposed_csf=sum(Pop_served*combined_cancer_prob_95,na.rm=T),
              per_million_cancer_cases_Proposed_CSF=1000000*(cumulative_cancer_cases_proposed_csf/pop))%>%
    ungroup()%>%
    mutate(method=ifelse(method=='as_int_total','Interpolated',method),
           method=ifelse(method=='as_town_avg','Town Average',method),
           method=ifelse(method=='mean_total_county','County Average',method),
           method=ifelse(method=='mean_total_region','Region Average',method),
           method=ifelse(method=='probabilitymapsli','Probability Maps: Lower Confidence',method),
           method=ifelse(method=='probabilitymapsui','Probability Maps: Upper Confidence',method),
           method=ifelse(method=='probabilitymaps','Probability Maps',method),
           method=as.factor(method))%>%
    mutate(method=fct_relevel(method, c('CWS Average', 'NTNC Average','Interpolated','Town Average','County Average','Region Average')),
           Method2=ifelse(grepl("Probability",method),"Probability Maps Confidence",""),
           Method2=ifelse(grepl("CWS",method),"PWS",Method2),
           Method2=ifelse(grepl("NTNC",method),"PWS",Method2),
           Method2=fct_relevel(as.factor(Method2),"PWS","")))%>%
  filter(method!="probability_maps")%>%
  mutate(assignment_method=ifelse(assignment_method=='gdb','DWP Well Viewer',assignment_method),
         assignment_method=ifelse(assignment_method=='service_maps','PWS Service Maps',assignment_method))
  


png("Data\\Figures\\as_threshold_vs_no_threshold.png",width=14,height=6,res=300,units = "in")
all_as_canc%>%
  filter(method!="NTNC Average")%>%
  pivot_longer(cols = starts_with("per_million"),names_pattern = "per_million_cancer_cases_(.*)",names_to = "Slope Factor")%>%
  mutate(`Slope Factor`=gsub("_"," ",`Slope Factor`))%>%
  filter(assignment_method=='PWS Service Maps')%>%
  ggplot(aes(y=method))+
  geom_point(aes(x=value,shape=`Calculation Method`,color=`Slope Factor`),size=5)+
  scale_shape_manual(values=c(16,18))+
  xlab("Per Million Attributable Cancer Cases")+
  ylab("")+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+
  theme(text=element_text(size = 16))
dev.off()

png("Data\\Figures\\as_threshold_vs_no_threshold2.png",width=14,height=6,res=300,units = "in")
all_as_canc%>%
  filter(method!="NTNC Average")%>%
  pivot_longer(cols = starts_with("per_million"),names_pattern = "per_million_cancer_cases_(.*)",names_to = "Slope Factor")%>%
  mutate(`Slope Factor`=gsub("_"," ",`Slope Factor`))%>%
  filter(assignment_method=='DWP Well Viewer')%>%
  ggplot(aes(y=method))+
  geom_point(aes(x=value,shape=`Calculation Method`,color=`Slope Factor`),size=5)+
  scale_shape_manual(values=c(16,18))+
  xlab("Per Million Attributable Cancer Cases")+
  ylab("")+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+
  theme(text=element_text(size = 16))
dev.off()

png("Data\\Figures\\as_threshold_vs_no_threshold_currentcsf.png",width=14,height=8,res=300,units = "in")
all_as_canc%>%
  filter(method!='NTNC')%>%
  ggplot(aes(y=method))+
  geom_point(aes(x=cumulative_cancer_cases_current_csf,shape=`Calculation Method`),color='red',size=5)+
  scale_shape_manual(values=c(16,18))+
  facet_wrap(~assignment_method,ncol = 1)+
  xlab("Total Attributable Cancer Cases")+
  ylab("")+
  theme_bw()+
  ggtitle('Current CSF')+
  theme(text=element_text(size = 16),legend.position = "top")
dev.off()

png("Data\\Figures\\as_threshold_vs_no_threshold_proposedcsf.png",width=14,height=8,res=300,units = "in")
all_as_canc%>%
  filter(method!='NTNC')%>%
  ggplot(aes(y=method))+
  geom_point(aes(x=cumulative_cancer_cases_proposed_csf,shape=`Calculation Method`),color='blue',size=5)+
  scale_shape_manual(values=c(16,18))+
  facet_wrap(~assignment_method,ncol = 1)+
  xlab("Total Attributable Cancer Cases")+
  ylab("")+
  theme_bw()+
  ggtitle('Proposed CSF')+
theme(text=element_text(size = 16),legend.position = "top")
dev.off()

a<-all_as_canc%>%
  filter(assignment_method=="PWS Service Maps")%>%
  filter(method!='NTNC Average')%>%
  ggplot(aes(y=method))+
  geom_point(aes(x=cumulative_cancer_cases_current_csf,shape=`Calculation Method`),color='red',size=5)+
  scale_shape_manual(values=c(16,18))+
  xlab("Total Attributable Cancer Cases")+
  ylab("")+
  theme_bw()+
  geom_text(aes(x=35,y="Probability Maps: Upper Confidence",label='Current CSF'),size=8)+
  theme(text=element_text(size = 20),legend.position = "bottom")

b<-all_as_canc%>%
  filter(assignment_method=="PWS Service Maps")%>%
  filter(method!='NTNC Average')%>%
  ggplot(aes(y=method))+
  geom_point(aes(x=cumulative_cancer_cases_proposed_csf,shape=`Calculation Method`),color='blue',size=5)+
  scale_shape_manual(values=c(16,18))+
  xlab("Total Attributable Cancer Cases")+
  ylab("")+
  theme_bw()+
  geom_text(aes(x=1200,y="Probability Maps: Upper Confidence",label='Proposed CSF'),size=8)+
  theme(text=element_text(size = 20),legend.position = "bottom")

png("Data\\Figures\\as_threshold_vs_no_threshold_both.png",width=14,height=8,res=300,units = "in")
ggarrange(a,b,labels = "auto",ncol = 1,common.legend = TRUE,font.label = list(size=20))
dev.off()

#THRESHOLD SUMMARY STATS----
town_as_health_threshold%>%
  filter(Method!="NTNC Average")%>%
  select(-over_1,-over_5,-over_10,-Method)%>%
  pivot_longer(cols = c(starts_with("over")), names_to = c("threshold","method"), 
               names_pattern ="over_(.*)_(.*)",values_to = "percent")%>%
  mutate(Pop_served=population_served*Pop,
         as_exposed=ifelse(threshold==1,0.001,as_exposed),
         as_exposed=ifelse(threshold==5,0.005,as_exposed),
         as_exposed=ifelse(threshold==10,0.01,as_exposed),
         dose=ifelse(Age=='adults',1000*as_exposed*ir_bw*(33/78),1000*as_exposed*ir_bw*(21/78)),
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0061,0.0011*dose^2+0.0059*dose),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.0003,dose*0.0003),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0127,0.001*dose^3+0.0037*dose^2+0.0121*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0186,0.003*dose^2+0.0181*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*0.0008,dose*0.0008),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*0.0462,0.0041*dose^3+0.0152*dose^2+0.0437*dose),
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.053,dose*0.053),
         proposed_csf=dose*53/1000,
         old_csf=dose*1.5/1000,
         cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
         cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
         fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
         fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
         ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
         ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
         fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
         fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
         proposed_rfd=0.031,
         current_rfd=0.3)%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method))%>%
  select(-n)%>%
  filter(!is.na(percent))%>%
  mutate(Source=ifelse(Source=="PWS","CWS Average",method),
         as_exposed=as_exposed*1000)%>%
  mutate(Source=fct_relevel(Source, c('CWS Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')))%>%
  distinct()%>%
  select(-method)%>%
  mutate(assignment_method=ifelse(assignment_method=='gdb',"MassDEP DWP Well Viewer",assignment_method),
         assignment_method=ifelse(assignment_method=='service_maps',"PWS Service Maps",assignment_method))%>%
  distinct()%>%
  group_by(assignment_method,Source,as_exposed)%>%
  summarise(users=sum(Pop_served,na.rm=T),
            over=paste(prettyNum(round(sum(Pop_served*percent,na.rm=T)),big.mark=","),' (',(round(100*round(sum(Pop_served*percent,na.rm=T))/users,1)),"\\%)",sep=""),
  )%>%
  mutate(users=prettyNum(users,big.mark=","))%>%pivot_wider(names_from = as_exposed,values_from = over)%>%
  mutate(assignment_method=ifelse(Source=='CWS Average','SDWIS',assignment_method))%>%
  distinct()%>%
  write.table("Tables\\as_threshold_summary.csv",quote = F,row.names = F,sep=";")


town_as_health_threshold%>%
  filter(Method!="NTNC Average")%>%
  select(-over_1,-over_5,-over_10,-Method)%>%
  pivot_longer(cols = c(starts_with("over")), names_to = c("threshold","method"), 
               names_pattern ="over_(.*)_(.*)",values_to = "percent")%>%
  mutate(Pop_served=population_served*Pop,
         as_exposed=ifelse(threshold==1,0.001,as_exposed),
         as_exposed=ifelse(threshold==5,0.005,as_exposed),
         as_exposed=ifelse(threshold==10,0.01,as_exposed),
         dose=ifelse(Age=='adults',1000*as_exposed*ir_bw*(33/78),1000*as_exposed*ir_bw*(21/78)),
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0061,0.0011*dose^2+0.0059*dose),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.0003,dose*0.0003),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0127,0.001*dose^3+0.0037*dose^2+0.0121*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0186,0.003*dose^2+0.0181*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*0.0008,dose*0.0008),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*0.0462,0.0041*dose^3+0.0152*dose^2+0.0437*dose),
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.053,dose*0.053),
         proposed_csf=dose*53/1000,
         old_csf=dose*1.5/1000,
         cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
         cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
         fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
         fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
         ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
         ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
         fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
         fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
         proposed_rfd=0.031,
         current_rfd=0.3)%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method))%>%
  select(-n)%>%
  filter(!is.na(percent))%>%
  mutate(Source=ifelse(Source=="PWS","CWS Average",method),
         as_exposed=as_exposed*1000)%>%
  mutate(Source=fct_relevel(Source, c('CWS Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')))%>%
  distinct()%>%
  select(-method)%>%
  filter(assignment_method!='gdb')%>%
  mutate(assignment_method=ifelse(assignment_method=='gdb',"MassDEP DWP Well Viewer",assignment_method),
         assignment_method=ifelse(assignment_method=='service_maps',"PWS Service Maps",assignment_method))%>%
  distinct()%>%
  group_by(assignment_method,Source,as_exposed)%>%
  summarise(users=sum(Pop_served,na.rm=T),
            over=paste(prettyNum(round(sum(Pop_served*percent,na.rm=T)),big.mark=","),' (',(round(100*round(sum(Pop_served*percent,na.rm=T))/users,1)),")",sep=""),
            )%>%
  mutate(users=prettyNum(users,big.mark=","))%>%
  pivot_wider(names_from = as_exposed,values_from = over)%>%
  mutate(assignment_method=ifelse(Source=='CWS Average','SDWIS',assignment_method))%>%
  distinct()%>%
  ungroup()%>%
  select(-assignment_method)%>%
  write.table("Tables\\as_threshold_summary_cut.csv",quote = F,row.names = F,sep = ";")

town_as_health_threshold%>%
  filter(Method!="NTNC Average")%>%
  select(-over_1,-over_5,-over_10,-Method)%>%
  pivot_longer(cols = c(starts_with("over")), names_to = c("threshold","method"), 
               names_pattern ="over_(.*)_(.*)",values_to = "percent")%>%
  mutate(Pop_served=population_served*Pop,
         as_exposed=ifelse(threshold==1,0.001,as_exposed),
         as_exposed=ifelse(threshold==5,0.005,as_exposed),
         as_exposed=ifelse(threshold==10,0.01,as_exposed),
         dose=ifelse(Age=='adults',1000*as_exposed*ir_bw*(33/78),1000*as_exposed*ir_bw*(21/78)),
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0061,0.0011*dose^2+0.0059*dose),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.0003,dose*0.0003),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0127,0.001*dose^3+0.0037*dose^2+0.0121*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0186,0.003*dose^2+0.0181*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*0.0008,dose*0.0008),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*0.0462,0.0041*dose^3+0.0152*dose^2+0.0437*dose),
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.053,dose*0.053),
         proposed_csf=dose*53/1000,
         old_csf=dose*1.5/1000,
         cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
         cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
         fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
         fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
         ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
         ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
         fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
         fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
         proposed_rfd=0.031,
         current_rfd=0.3)%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method))%>%
  select(-n)%>%
  filter(!is.na(percent))%>%
  mutate(Source=ifelse(Source=="PWS","CWS Average",method),
         as_exposed=as_exposed*1000)%>%
  mutate(Source=fct_relevel(Source, c('CWS Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')))%>%
  distinct()%>%
  #pivot_longer(cols = c('Pop_gdb','Pop_service_maps'),names_to = "population_method",values_to = "Pop_served")%>%
  select(-method)%>%
  mutate(assignment_method=ifelse(assignment_method=='gdb',"MassDEP DWP Well Viewer",assignment_method),
         assignment_method=ifelse(assignment_method=='service_maps',"PWS Service Maps",assignment_method))%>%
  distinct()%>%
  group_by(assignment_method,Source)%>%
  summarise(users=sum(Pop_served,na.rm=T),
            over=paste(prettyNum(round(sum(Pop_served*percent,na.rm=T)),big.mark=","),' (',(round(100*round(sum(Pop_served*percent,na.rm=T))/users,1)),"\\%)",sep=""),
  )%>%
  mutate(users=prettyNum(users/3,big.mark=","))%>%
  mutate(assignment_method=ifelse(Source=='CWS Average','SDWIS',assignment_method))%>%
  distinct()%>%
  write.table("Tables\\as_threshold_summary_rfd.csv",quote = F,row.names = F,sep=";")


#Non-Cancer Threshold Tables and Figures----
all_as_non_cancer<-town_as_health_threshold%>%
  mutate(Source=ifelse(Source=="PWS",Method,Source))%>%
  select(-over_1,-over_5,-over_10,-Method)%>%
  pivot_longer(cols = c(starts_with("over")), names_to = c("threshold","method"), 
               names_pattern ="over_(.*)_(.*)",values_to = "percent")%>%
  select(-n)%>%
  mutate(as_exposed=ifelse(threshold==1,0.001,as_exposed),
         as_exposed=ifelse(threshold==5,0.005,as_exposed),
         as_exposed=ifelse(threshold==10,0.01,as_exposed),
         method=ifelse(Source!='Private Well',Source,method))%>%
  filter(!is.na(percent))%>%
  distinct()%>%
  group_by(town,assignment_method,Age,method)%>%
  mutate(percent=ifelse(threshold==1,2*percent-sum(percent),percent),
         percent=ifelse(threshold==5,percent-mean(percent[threshold==10]),percent),
         percent=ifelse(percent<0,0,percent))%>%
  group_by(town,assignment_method,Source,population_served,Pop,Age,ir_bw,method)%>%
  group_modify(~ add_row(.x,.before=0))%>%
  mutate(threshold=ifelse(is.na(threshold),0,threshold),
         as_exposed=ifelse(is.na(as_exposed),0.0005,as_exposed),
         percent=ifelse(threshold==0,1-sum(percent,na.rm=T),percent))%>%
  ungroup()%>%
  mutate(dose=ifelse(Age=='adults',1000*as_exposed*ir_bw*(33/78),1000*as_exposed*ir_bw*(21/78)),
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0061,0.0011*dose^2+0.0059*dose),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.0003,dose*0.0003),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0127,0.001*dose^3+0.0037*dose^2+0.0121*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0186,0.003*dose^2+0.0181*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*0.0008,dose*0.0008),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*0.0462,0.0041*dose^3+0.0152*dose^2+0.0437*dose),
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.053,dose*0.053),
         proposed_csf=dose*53/1000,
         old_csf=dose*1.5/1000,
         cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
         cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
         fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
         fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
         ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
         ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
         fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
         fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
         proposed_rfd=0.031,
         current_rfd=0.3,
         calc_method="Threshold",
         Pop_served=round(percent*population_served*Pop),
         assignment_method=ifelse(assignment_method=='gdb',"MassDEP DWP Well Viewer",assignment_method),
         assignment_method=ifelse(assignment_method=='service_maps',"PWS Service Maps",assignment_method))%>%
  distinct()%>%
  select(town,assignment_method,method,calc_method,as_exposed,Age,Pop_served,ir_bw,dose,dose_nc,starts_with("cvd"),starts_with("ihd"),starts_with("fatal"),current_rfd,proposed_rfd,old_csf,combined_cancer_prob_95)%>%
rbind(town_as_health_new%>%
  group_by(Method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0001,dose_nc))%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method),
         calc_method="Value",
         assignment_method=ifelse(assignment_method=='gdb',"MassDEP DWP Well Viewer",assignment_method),
         assignment_method=ifelse(assignment_method=='service_maps',"PWS Service Maps",assignment_method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')),
         method=Method)%>%
  select(town,assignment_method,method,calc_method,as_exposed,Age,Pop_served,ir_bw,dose,dose_nc,starts_with("cvd"),starts_with("ihd"),starts_with("fatal"),current_rfd,proposed_rfd,old_csf,combined_cancer_prob_95))


as_threshold_summary_rfd<-all_as_non_cancer%>%
  filter(calc_method=='Threshold')%>%
  group_by(method,assignment_method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0005,dose_nc))%>%
  ungroup()%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method),
  )%>%
  mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')),
         Source=ifelse(method=='CWS Average'|method=='NTNC Average','PWS','Private Well'),
         assignment_method=ifelse(method=='CWS Average'|method=='NTNC Average','SDWIS',assignment_method),
         `Assignment Method`=as.factor(assignment_method),
         `Assignment Method`=fct_relevel(`Assignment Method`,'SDWIS','PWS Service Maps','MassDEP DWP Well Viewer'))%>%
  distinct()%>%
  group_by(`Assignment Method`,method)%>%
  summarise(users=sum(Pop_served,na.rm=T),
            over_current_rfd=paste(prettyNum(round(sum(Pop_served[dose_nc>=current_rfd],na.rm=T)),big.mark=','),' (',(round(100*round(sum(Pop_served[dose_nc>=current_rfd],na.rm=T))/users,1)),"\\%)",sep=""),
            over_proposed_rfd=paste(prettyNum(round(sum(Pop_served[dose_nc>=proposed_rfd],na.rm=T)),big.mark=','),' (',(round(100*round(sum(Pop_served[dose_nc>=proposed_rfd],na.rm=T))/users,1)),"\\%)",sep=""))%>%
  mutate(users=prettyNum(users,big.mark=","))%>%
  distinct()%>%
  filter(method!="NTNC Average")

write.csv(as_threshold_summary_rfd,"Tables\\as_threshold_summary_rfd.csv",quote = F,row.names = F)


#Figure Generation
library(scales)
png("Data\\Figures\\as_nc_frd_value.png",width=12,height=8,res=300,units = "in")
all_as_non_cancer%>%
  filter(calc_method!='Threshold')%>%
  group_by(method,assignment_method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0001,dose_nc))%>%
  ungroup()%>%
  #pivot_wider(names_from = assignment_method,values_from = dose_nc)%>%
  #view()
  #filter(Method!='probability_maps')%>%
  mutate(method=ifelse(method=='as_int_total','Interpolated',method),
         method=ifelse(method=='as_town_avg','Town Average',method),
         method=ifelse(method=='mean_total_county','County Average',method),
         method=ifelse(method=='mean_total_region','Region Average',method))%>%
  mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')),
         Source=ifelse(method=='CWS Average'|method=='NTNC Average','PWS','Private Well'),
         assignment_method=ifelse(method=='CWS Average'|method=='NTNC Average','SDWIS',assignment_method),
         `Assignment Method`=as.factor(assignment_method),
         `Assignment Method`=fct_relevel(`Assignment Method`,'SDWIS','PWS Service Maps','MassDEP DWP Well Viewer'))%>%
  filter(Source=='Private Well')%>%
  distinct()%>%
  ggplot(aes(x = dose_nc)) + 
  geom_histogram(aes(weight = Pop_served,fill=`Assignment Method`),color='black',bins=30,position='identity')+
  geom_vline(aes(xintercept = proposed_rfd,color="Proposed RfD"),size=1.2)+
  geom_vline(aes(xintercept = current_rfd,color="Current RfD"),size=1.2)+
  #geom_text(data=labs,aes(y=0.39,x=0.001,label=n))+
  scale_colour_manual("", 
                      breaks = c("Proposed RfD", "Current RfD"),
                      values = c("darkred", "darkblue")) +
  scale_fill_manual(values = c("goldenrod2","slategray3"))+
  scale_x_log10()+
  scale_y_continuous(label=comma)+
  ylab('Number of Users')+
  xlab('Dosage (ug/kg-d)')+
  facet_wrap(~method)+
  ggtitle("A) Value Based Method")+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top")
dev.off()

png("Data\\Figures\\as_nc_frd_thr.png",width=12,height=12,res=300,units = "in")
all_as_non_cancer%>%
  filter(calc_method=='Threshold')%>%
  group_by(method,assignment_method)%>%
  mutate(pop_percent=(Pop_served/sum(Pop_served)),
         dose_nc=ifelse(dose_nc==0,0.0005,dose_nc))%>%
  #view()
  ungroup()%>%
  #pivot_wider(names_from = assignment_method,values_from = dose_nc)%>%
  #view()
  #filter(Method!='probability_maps')%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method),
         )%>%
  mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')),
         Source=ifelse(method=='CWS Average'|method=='NTNC Average','PWS','Private Well'),
         assignment_method=ifelse(method=='CWS Average'|method=='NTNC Average','SDWIS',assignment_method),
         `Assignment Method`=as.factor(assignment_method),
         `Assignment Method`=fct_relevel(`Assignment Method`,'SDWIS','PWS Service Maps','MassDEP DWP Well Viewer'))%>%
  filter(Source=='Private Well')%>%
  distinct()%>%
  ggplot(aes(x = dose_nc)) + 
  geom_histogram(aes(weight = Pop_served,fill=`Assignment Method`),color='black',bins=20,position='identity')+
  geom_vline(aes(xintercept = proposed_rfd,color="Proposed RfD"),size=1.2)+
  geom_vline(aes(xintercept = current_rfd,color="Current RfD"),size=1.2)+
  #geom_text(data=labs,aes(y=0.39,x=0.001,label=n))+
  scale_colour_manual("", 
                      breaks = c("Proposed RfD", "Current RfD"),
                      values = c("darkred", "darkblue")) +
  scale_fill_manual(values = c("goldenrod2","slategray3"))+
  scale_x_log10()+
  scale_y_continuous(label=comma)+
  ylab('Number of Users')+
  xlab('Dosage (ug/kg-d)')+
  facet_wrap(~method,ncol = 2)+
  ggtitle("B) Threshold Based Method")+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top")
dev.off()

#CVD----
#CVD Figures based on equations from new IRIS Draft
cvd_risk<-all_as_non_cancer%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method),
  )%>%
  mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')),
         Source=ifelse(method=='CWS Average'|method=='NTNC Average','PWS','Private Well'),
         assignment_method=ifelse(method=='CWS Average'|method=='NTNC Average','SDWIS',assignment_method),
         `Assignment Method`=as.factor(assignment_method),
         `Assignment Method`=fct_relevel(`Assignment Method`,'SDWIS','PWS Service Maps','MassDEP DWP Well Viewer'))%>%
  distinct()%>%
  group_by(method,`Assignment Method`,calc_method)%>%
  summarise(est="Mean",
            Pop=sum(Pop_served),
            cvd_risk=sum(Pop_served*cvd_extra_risk,na.rm=T),
            cvd_risk_per_10000=10000*cvd_risk/Pop,
            fatal_cvd_risk=sum(Pop_served*fatal_cvd_extra_risk,na.rm=T),
            fatal_cvd_risk_per_10000=10000*fatal_cvd_risk/Pop)%>%
  rbind(all_as_non_cancer%>%
          mutate(method=ifelse(method=='int','Interpolated',method),
                 method=ifelse(method=='town','Town Average',method),
                 method=ifelse(method=='county','County Average',method),
                 method=ifelse(method=='region','Region Average',method),
                 method=ifelse(method=='probabilitymaps','Probability Maps',method),
                 method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
                 method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method),
          )%>%
          mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')),
                 Source=ifelse(method=='CWS Average'|method=='NTNC Average','PWS','Private Well'),
                 assignment_method=ifelse(method=='CWS Average'|method=='NTNC Average','SDWIS',assignment_method),
                 `Assignment Method`=as.factor(assignment_method),
                 `Assignment Method`=fct_relevel(`Assignment Method`,'SDWIS','PWS Service Maps','MassDEP DWP Well Viewer'))%>%
          distinct()%>%
          group_by(method,`Assignment Method`,calc_method)%>%
          summarise(est="95th Percentile",
                    Pop=sum(Pop_served),
                    cvd_risk=sum(Pop_served*cvd_extra_risk_95,na.rm=T),
                    cvd_risk_per_10000=10000*cvd_risk/Pop,
                    fatal_cvd_risk=sum(Pop_served*fatal_cvd_extra_risk_95,na.rm=T),
                    fatal_cvd_risk_per_10000=10000*fatal_cvd_risk/Pop))%>%
  mutate(est=as.factor(est),
         est=fct_relevel(est,"Mean","95th Percentile"))

#Value Method GDB
png("Data\\Figures\\cvd_risk_sm.png",width=12,height=8,res=300,units = "in")
cvd_risk%>%
  filter(calc_method!='Threshold')%>%
  filter(`Assignment Method`!='MassDEP DWP Well Viewer')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=cvd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_cvd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set2")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra CVD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()

#Value Method SM
png("Data\\Figures\\cvd_risk_gdb.png",width=12,height=8,res=300,units = "in")
cvd_risk%>%
  filter(calc_method!='Threshold')%>%
  filter(`Assignment Method`!='PWS Service Maps')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=cvd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_cvd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set2")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra CVD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()

#Threshold Method GDB
png("Data\\Figures\\cvd_risk_sm_thr.png",width=16,height=8,res=300,units = "in")
cvd_risk%>%
  filter(calc_method=='Threshold')%>%
  filter(`Assignment Method`!='MassDEP DWP Well Viewer')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=cvd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_cvd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set2")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra CVD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()

#Threshold Method SM
png("Data\\Figures\\cvd_risk_gdb_thr.png",width=16,height=8,res=300,units = "in")
cvd_risk%>%
  filter(calc_method=='Threshold')%>%
  filter(`Assignment Method`!='PWS Service Maps')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=cvd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_cvd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set2")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra CVD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()


#IHD----
ihd_risk<-all_as_non_cancer%>%
  #filter(calc_method!='Threshold')%>%
  mutate(method=ifelse(method=='int','Interpolated',method),
         method=ifelse(method=='town','Town Average',method),
         method=ifelse(method=='county','County Average',method),
         method=ifelse(method=='region','Region Average',method),
         method=ifelse(method=='probabilitymaps','Probability Maps',method),
         method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
         method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method),
  )%>%
  mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')),
         Source=ifelse(method=='CWS Average'|method=='NTNC Average','PWS','Private Well'),
         assignment_method=ifelse(method=='CWS Average'|method=='NTNC Average','SDWIS',assignment_method),
         `Assignment Method`=as.factor(assignment_method),
         `Assignment Method`=fct_relevel(`Assignment Method`,'SDWIS','PWS Service Maps','MassDEP DWP Well Viewer'))%>%
  distinct()%>%
  group_by(method,`Assignment Method`,calc_method)%>%
  summarise(est="Mean",
            Pop=sum(Pop_served),
            ihd_risk=sum(Pop_served*ihd_extra_risk,na.rm=T),
            ihd_risk_per_10000=10000*ihd_risk/Pop,
            fatal_ihd_risk=sum(Pop_served*fatal_ihd_extra_risk,na.rm=T),
            fatal_ihd_risk_per_10000=10000*fatal_ihd_risk/Pop)%>%
  rbind(all_as_non_cancer%>%
          mutate(method=ifelse(method=='int','Interpolated',method),
                 method=ifelse(method=='town','Town Average',method),
                 method=ifelse(method=='county','County Average',method),
                 method=ifelse(method=='region','Region Average',method),
                 method=ifelse(method=='probabilitymaps','Probability Maps',method),
                 method=ifelse(method=='probabilitymapsli','Probability Maps (LCI)',method),
                 method=ifelse(method=='probabilitymapsui','Probability Maps (UCI)',method),
          )%>%
          mutate(method=fct_relevel(method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average','Probability Maps (LCI)','Probability Maps','Probability Maps (UCI)')),
                 Source=ifelse(method=='CWS Average'|method=='NTNC Average','PWS','Private Well'),
                 assignment_method=ifelse(method=='CWS Average'|method=='NTNC Average','SDWIS',assignment_method),
                 `Assignment Method`=as.factor(assignment_method),
                 `Assignment Method`=fct_relevel(`Assignment Method`,'SDWIS','PWS Service Maps','MassDEP DWP Well Viewer'))%>%
          distinct()%>%
          group_by(method,`Assignment Method`,calc_method)%>%
          summarise(est="95th Percentile",
                    Pop=sum(Pop_served),
                    ihd_risk=sum(Pop_served*ihd_extra_risk_95,na.rm=T),
                    ihd_risk_per_10000=10000*ihd_risk/Pop,
                    fatal_ihd_risk=sum(Pop_served*fatal_ihd_extra_risk_95,na.rm=T),
                    fatal_ihd_risk_per_10000=10000*fatal_ihd_risk/Pop))%>%
  mutate(est=as.factor(est),
         est=fct_relevel(est,"Mean","95th Percentile"))

#Value Method GDB
png("Data\\Figures\\ihd_risk_sm.png",width=12,height=8,res=300,units = "in")
ihd_risk%>%
  filter(calc_method!='Threshold')%>%
  filter(`Assignment Method`!='MassDEP DWP Well Viewer')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=ihd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_ihd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set3")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra IHD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()

#Value Method SM
png("Data\\Figures\\ihd_risk_gdb.png",width=12,height=8,res=300,units = "in")
ihd_risk%>%
  filter(calc_method!='Threshold')%>%
  filter(`Assignment Method`!='PWS Service Maps')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=ihd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_ihd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set3")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra IHD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()

#Threshold Method GDB
png("Data\\Figures\\ihd_risk_sm_thr.png",width=16,height=8,res=300,units = "in")
ihd_risk%>%
  filter(calc_method=='Threshold')%>%
  filter(`Assignment Method`!='MassDEP DWP Well Viewer')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=ihd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_ihd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set3")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra IHD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()

#Threshold Method SM
png("Data\\Figures\\ihd_risk_gdb_thr.png",width=16,height=8,res=300,units = "in")
ihd_risk%>%
  filter(calc_method=='Threshold')%>%
  filter(`Assignment Method`!='PWS Service Maps')%>%
  ggplot(aes(x=method,fill=est))+
  geom_bar(aes(y=ihd_risk_per_10000),stat = "identity",position = "dodge")+
  geom_bar(aes(y=fatal_ihd_risk_per_10000,color="Fatal Cases"),stat = "identity",position = "dodge",alpha=0,size=1.2)+
  scale_fill_brewer(palette = "Set3")+
  scale_colour_manual("", 
                      breaks = c("Fatal Cases"),
                      values = c("black"))+
  ylab('Extra IHD Risk (per 10,000)')+
  xlab('')+
  theme_bw()+
  theme(text=element_text(size = 16),legend.position = "top",legend.title = element_blank())
dev.off()


#Hazard Quotient Figure Geberation----
a<-town_as_health_new%>%
  filter(assignment_method!='service_maps')%>%
  mutate(dose_nc=ifelse(dose_nc==0,0.0001,dose_nc),
         HQ_Current=dose_nc/current_rfd,
         HQ_Proposed=dose_nc/proposed_rfd)%>%
  pivot_longer(cols = c(HQ_Current,HQ_Proposed), names_to = c("HQ"),
               names_pattern ="HQ_(.*)",values_to = "hq")%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  filter(Method!='NTNC Average')%>%
  uncount(Pop_served)%>%
  ggplot(aes(hq,color=Method,linetype=Source)) +
  geom_line(aes(y = 1 - ..y..), stat='ecdf',linewidth=1.5)+
  xlab('HQ')+
  ylab('Percent Exposed HQ')+
  xlim(0,2)+
  theme_classic()+
  facet_wrap(~HQ)+
  theme(text = element_text(size=20),legend.title = element_blank(),legend.position = "bottom")

b<-town_as_health_new%>%
  filter(assignment_method!='gdb')%>%
  mutate(dose_nc=ifelse(dose_nc==0,0.0001,dose_nc),
         HQ_Current=dose_nc/current_rfd,
         HQ_Proposed=dose_nc/proposed_rfd)%>%
  pivot_longer(cols = c(HQ_Current,HQ_Proposed), names_to = c("HQ"),
               names_pattern ="HQ_(.*)",values_to = "hq")%>%
  ungroup()%>%
  filter(Method!='probability_maps')%>%
  mutate(Method=ifelse(Method=='as_int_total','Interpolated',Method),
         Method=ifelse(Method=='as_town_avg','Town Average',Method),
         Method=ifelse(Method=='mean_total_county','County Average',Method),
         Method=ifelse(Method=='mean_total_region','Region Average',Method))%>%
  mutate(Method=fct_relevel(Method, c('CWS Average','NTNC Average','Interpolated','Town Average','County Average','Region Average')))%>%
  filter(Method!='NTNC Average')%>%
  uncount(Pop_served)%>%
  ggplot(aes(hq,color=Method,linetype=Source)) +
  geom_line(aes(y = 1 - ..y..), stat='ecdf',linewidth=1.5)+
  xlab('HQ')+
  ylab('Percent Exposed HQ')+
  xlim(0,2)+
  theme_classic()+
  facet_wrap(~HQ)+
  theme(text = element_text(size=20),legend.title = element_blank(),legend.position = "bottom")


png("Data\\Figures\\arsenic_hq_ov.png",width=10,height=11,res=300,units = "in")
ggarrange(
  a, b,labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom",
  ncol = 1
 )
 dev.off()

png("Data\\Figures\\arsenic_hq_ov1.png",width=12,height=6,res=300,units = "in")
a
dev.off()

png("Data\\Figures\\arsenic_hq_ov2.png",width=12,height=6,res=300,units = "in")
b
dev.off()


#Arsenic Probability Maps Performance Evaluation----


#Below the accuracy rates are assessed at different likelihoods
#The previous evaluation was done with a likelihood of 0.5, but this
#is supplemental with other likelihoods

#0.4
gw_extracted2<-read.csv("Data\\wells_training_as.csv")%>%
  filter(source!='EEA Finished CWS')%>%
  mutate(as_int_lci=as_int-1.96*as_int_se,
         as_int_lci=ifelse(as_int_lci<0,0,as_int_lci),
         as_int_uci=as_int+1.96*as_int_se)%>%
  merge(pw_vs_pws%>%
          select(source_id)%>%
          distinct()%>%
          mutate(training='Y'),all.x=T)%>%
  mutate(training=ifelse(is.na(training),'N',training),
         arsenic_so=ifelse(source=='USGS',arsenic_so/1000,arsenic_so))%>%
  mutate(as_over_1=ifelse(arsenic_so>=0.001,'Y','N'),
         as_over_5=ifelse(arsenic_so>=0.005,'Y','N'),
         as_over_10=ifelse(arsenic_so>=0.01,'Y','N'),
         as_over_1_pred=ifelse(as_1_prob>0.4,'Y','N'),
         as_over_5_pred=ifelse(as_5_prob>0.4,'Y','N'),
         as_over_10_pred=ifelse(as_10_prob>0.4,'Y','N'),
         as_over_1_pred_ci=case_when(as_1_lci<0.4&as_1_uci<0.4~'N',as_1_lci<0.4&as_1_uci>=0.4~'Y or N',as_1_lci>0.4&as_1_uci>0.4~'Y'),
         as_over_5_pred_ci=case_when(as_5_lci<0.4&as_5_uci<0.4~'N',as_5_lci<0.4&as_5_uci>=0.4~'Y or N',as_5_lci>0.4&as_5_uci>0.4~'Y'),
         as_over_10_pred_ci=case_when(as_10_lci<0.4&as_10_uci<0.4~'N',as_10_lci<0.4&as_10_uci>=0.4~'Y or N',as_10_lci>0.4&as_10_uci>0.4~'Y'),
         as_over_1_ci=ifelse(as_over_1_pred_ci=='Y or N',as_over_1,as_over_1_pred_ci),
         as_over_5_ci=ifelse(as_over_5_pred_ci=='Y or N',as_over_5,as_over_5_pred_ci),
         as_over_10_ci=ifelse(as_over_10_pred_ci=='Y or N',as_over_10,as_over_10_pred_ci))

as_ext<-gw_extracted2%>%
  filter(arsenic_n_>0)%>%
  filter(training=='Y')

as_mod_accuracy1=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_1_pred_ci)%>%
  select(source,contains('1'),-contains('10'))%>%
  mutate(threshold='1')
colnames(as_mod_accuracy1)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy5=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_5_pred_ci)%>%
  select(source,contains('5'))%>%
  mutate(threshold='5')
colnames(as_mod_accuracy5)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy10=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_10_pred_ci)%>%
  select(source,contains('10'))%>%
  mutate(threshold='10')
colnames(as_mod_accuracy10)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy<-rbind(as_mod_accuracy1,as_mod_accuracy5,as_mod_accuracy10)


as_acc_metrics12<-as_mod_accuracy%>%
  mutate(source='Total')%>%
  group_by(source,threshold)%>%
  summarise(TP=sum(actual_over=='Y'&pred_over=='Y'),
            TP_ci=sum(actual_over=='Y'&ci_over=='Y'),
            FP=sum(actual_over=='N'&pred_over=='Y'),
            FP_ci=sum(actual_over=='N'&ci_over=='Y'),
            TN=sum(actual_over=='N'&pred_over=='N'),
            TN_ci=sum(actual_over=='N'&ci_over=='N'),
            FN=sum(actual_over=='Y'&pred_over=='N'),
            FN_ci=sum(actual_over=='Y'&ci_over=='N'),
            sensitivity=TP/(TP+FN),
            sensitivity_ci=TP_ci/(TP_ci+FN_ci),
            specificity=TN/(TN+FP),
            specificity_ci=TN_ci/(TN_ci+FP_ci),
            accuracy=(TP+TN)/(TP+TN+FP+FN),
            accuracy_ci=(TP_ci+TN_ci)/(TP_ci+TN_ci+FP_ci+FN_ci))

as_acc_metrics2<-as_mod_accuracy%>%
  group_by(source,threshold)%>%
  summarise(TP=sum(actual_over=='Y'&pred_over=='Y'),
            TP_ci=sum(actual_over=='Y'&ci_over=='Y'),
            FP=sum(actual_over=='N'&pred_over=='Y'),
            FP_ci=sum(actual_over=='N'&ci_over=='Y'),
            TN=sum(actual_over=='N'&pred_over=='N'),
            TN_ci=sum(actual_over=='N'&ci_over=='N'),
            FN=sum(actual_over=='Y'&pred_over=='N'),
            FN_ci=sum(actual_over=='Y'&ci_over=='N'),
            sensitivity=TP/(TP+FN),
            sensitivity_ci=TP_ci/(TP_ci+FN_ci),
            specificity=TN/(TN+FP),
            specificity_ci=TN_ci/(TN_ci+FP_ci),
            accuracy=(TP+TN)/(TP+TN+FP+FN),
            accuracy_ci=(TP_ci+TN_ci)/(TP_ci+TN_ci+FP_ci+FN_ci))%>%
  rbind(as_acc_metrics12)

RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}


png("Data\\Figures\\as_well_pred_bars_4.png",height = 8,width=12,res=300,units = "in")
as_acc_metrics2%>%
  group_by(threshold,source)%>%
  summarise(Total=sum(FN,TP),
            Correct=TP,
            Incorrect=FN)%>%
  mutate(Actual='Above')%>%
  rbind(as_acc_metrics2%>%
          group_by(threshold,source)%>%
          summarise(Total=sum(TN,FP),
                    Correct=TN,
                    Incorrect=FP)%>%
          mutate(Actual='Below'))%>%
  mutate(Correct=100*Correct/Total,
         Incorrect=100*Incorrect/Total,
         Total=paste('n=',Total,sep=""),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")),
         Actual=as.factor(Actual),
         Actual=fct_relevel(Actual,"Below","Above"))%>%
  pivot_longer(cols=c('Correct','Incorrect'))%>%
  mutate(name=as.factor(name),
         name=fct_relevel(name,"Incorrect","Correct"))%>%
  ggplot(aes(y=source,x=value,fill=name))+
  geom_bar(position='stack',stat='identity')+
  geom_label(aes(label=Total,x=50),fill='white',size=8)+
  scale_fill_manual(values = c("firebrick","skyblue"))+
  xlab('Percent')+
  ylab('')+
  facet_grid(Actual~threshold)+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "bottom",legend.title = element_blank())
dev.off()



png("Data\\Figures\\as_well_pred_bars_ci4.png",height = 8,width=12,res=300,units = "in")
as_acc_metrics2%>%
  group_by(threshold,source)%>%
  summarise(Total=sum(FN_ci,TP_ci),
            Correct=TP_ci,
            Incorrect=FN_ci)%>%
  mutate(Actual='Above')%>%
  rbind(as_acc_metrics2%>%
          group_by(threshold,source)%>%
          summarise(Total=sum(TN_ci,FP_ci),
                    Correct=TN_ci,
                    Incorrect=FP_ci)%>%
          mutate(Actual='Below'))%>%
  mutate(Correct=100*Correct/Total,
         Incorrect=100*Incorrect/Total,
         Total=paste('n=',Total,sep=""),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")),
         Actual=as.factor(Actual),
         Actual=fct_relevel(Actual,"Below","Above"))%>%
  pivot_longer(cols=c('Correct','Incorrect'))%>%
  mutate(name=as.factor(name),
         name=fct_relevel(name,"Incorrect","Correct"))%>%
  ggplot(aes(y=source,x=value,fill=name))+
  geom_bar(position='stack',stat='identity')+
  geom_label(aes(label=Total,x=50),fill='white',size=8)+
  scale_fill_manual(values = c("firebrick","skyblue"))+
  xlab('Percent')+
  ylab('')+
  facet_grid(Actual~threshold)+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "bottom",legend.title = element_blank())
dev.off()


as_acc_metrics2%>%
  mutate(total_below=TN_ci+FP_ci,
         total_above=FN_ci+TP_ci,
         sensitivity=paste(round(100*sensitivity,1),"\\% (",round(100*sensitivity_ci,1),"\\%)",sep=""),
         specificity=paste(round(100*specificity,1),"\\% (",round(100*specificity_ci,1),"\\%)",sep=""),
         accuracy=paste(round(100*accuracy,1),"\\% (",round(100*accuracy_ci,1),"\\%)",sep=""),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")))%>%
  select(source,threshold,total_above,total_below,accuracy,sensitivity,specificity)%>%
  arrange(desc(source),threshold)%>%
  write.csv("Tables\\prob_maps_acc4.csv",row.names = F,quote = F)




gw_extracted2<-read.csv("Data\\wells_training_as.csv")%>%
  filter(source!='EEA Finished CWS')%>%
  mutate(as_int_lci=as_int-1.96*as_int_se,
         as_int_lci=ifelse(as_int_lci<0,0,as_int_lci),
         as_int_uci=as_int+1.96*as_int_se)%>%
  merge(pw_vs_pws%>%
          select(source_id)%>%
          distinct()%>%
          mutate(training='Y'),all.x=T)%>%
  mutate(training=ifelse(is.na(training),'N',training),
         arsenic_so=ifelse(source=='USGS',arsenic_so/1000,arsenic_so))%>%
  mutate(as_over_1=ifelse(arsenic_so>=0.001,'Y','N'),
         as_over_5=ifelse(arsenic_so>=0.005,'Y','N'),
         as_over_10=ifelse(arsenic_so>=0.01,'Y','N'),
         as_over_1_pred=ifelse(as_1_prob>0.25,'Y','N'),
         as_over_5_pred=ifelse(as_5_prob>0.25,'Y','N'),
         as_over_10_pred=ifelse(as_10_prob>0.25,'Y','N'),
         as_over_1_pred_ci=case_when(as_1_lci<0.25&as_1_uci<0.25~'N',as_1_lci<0.25&as_1_uci>=0.25~'Y or N',as_1_lci>0.25&as_1_uci>0.25~'Y'),
         as_over_5_pred_ci=case_when(as_5_lci<0.25&as_5_uci<0.25~'N',as_5_lci<0.25&as_5_uci>=0.25~'Y or N',as_5_lci>0.25&as_5_uci>0.25~'Y'),
         as_over_10_pred_ci=case_when(as_10_lci<0.25&as_10_uci<0.25~'N',as_10_lci<0.25&as_10_uci>=0.25~'Y or N',as_10_lci>0.25&as_10_uci>0.25~'Y'),
         as_over_1_ci=ifelse(as_over_1_pred_ci=='Y or N',as_over_1,as_over_1_pred_ci),
         as_over_5_ci=ifelse(as_over_5_pred_ci=='Y or N',as_over_5,as_over_5_pred_ci),
         as_over_10_ci=ifelse(as_over_10_pred_ci=='Y or N',as_over_10,as_over_10_pred_ci))

as_ext<-gw_extracted2%>%
  filter(arsenic_n_>0)%>%
  filter(training=='Y')

as_mod_accuracy1=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_1_pred_ci)%>%
  select(source,contains('1'),-contains('10'))%>%
  mutate(threshold='1')
colnames(as_mod_accuracy1)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy5=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_5_pred_ci)%>%
  select(source,contains('5'))%>%
  mutate(threshold='5')
colnames(as_mod_accuracy5)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy10=as_ext%>%
  select(source,starts_with('as_over'))%>%
  select(-as_over_10_pred_ci)%>%
  select(source,contains('10'))%>%
  mutate(threshold='10')
colnames(as_mod_accuracy10)<-c('source','actual_over','pred_over','ci_over','threshold')

as_mod_accuracy<-rbind(as_mod_accuracy1,as_mod_accuracy5,as_mod_accuracy10)


as_acc_metrics12<-as_mod_accuracy%>%
  mutate(source='Total')%>%
  group_by(source,threshold)%>%
  summarise(TP=sum(actual_over=='Y'&pred_over=='Y'),
            TP_ci=sum(actual_over=='Y'&ci_over=='Y'),
            FP=sum(actual_over=='N'&pred_over=='Y'),
            FP_ci=sum(actual_over=='N'&ci_over=='Y'),
            TN=sum(actual_over=='N'&pred_over=='N'),
            TN_ci=sum(actual_over=='N'&ci_over=='N'),
            FN=sum(actual_over=='Y'&pred_over=='N'),
            FN_ci=sum(actual_over=='Y'&ci_over=='N'),
            sensitivity=TP/(TP+FN),
            sensitivity_ci=TP_ci/(TP_ci+FN_ci),
            specificity=TN/(TN+FP),
            specificity_ci=TN_ci/(TN_ci+FP_ci),
            accuracy=(TP+TN)/(TP+TN+FP+FN),
            accuracy_ci=(TP_ci+TN_ci)/(TP_ci+TN_ci+FP_ci+FN_ci))

as_acc_metrics2<-as_mod_accuracy%>%
  group_by(source,threshold)%>%
  summarise(TP=sum(actual_over=='Y'&pred_over=='Y'),
            TP_ci=sum(actual_over=='Y'&ci_over=='Y'),
            FP=sum(actual_over=='N'&pred_over=='Y'),
            FP_ci=sum(actual_over=='N'&ci_over=='Y'),
            TN=sum(actual_over=='N'&pred_over=='N'),
            TN_ci=sum(actual_over=='N'&ci_over=='N'),
            FN=sum(actual_over=='Y'&pred_over=='N'),
            FN_ci=sum(actual_over=='Y'&ci_over=='N'),
            sensitivity=TP/(TP+FN),
            sensitivity_ci=TP_ci/(TP_ci+FN_ci),
            specificity=TN/(TN+FP),
            specificity_ci=TN_ci/(TN_ci+FP_ci),
            accuracy=(TP+TN)/(TP+TN+FP+FN),
            accuracy_ci=(TP_ci+TN_ci)/(TP_ci+TN_ci+FP_ci+FN_ci))%>%
  rbind(as_acc_metrics12)


png("Data\\Figures\\as_well_pred_bars_3.png",height = 8,width=12,res=300,units = "in")
as_acc_metrics2%>%
  group_by(threshold,source)%>%
  summarise(Total=sum(FN,TP),
            Correct=TP,
            Incorrect=FN)%>%
  mutate(Actual='Above')%>%
  rbind(as_acc_metrics2%>%
          group_by(threshold,source)%>%
          summarise(Total=sum(TN,FP),
                    Correct=TN,
                    Incorrect=FP)%>%
          mutate(Actual='Below'))%>%
  mutate(Correct=100*Correct/Total,
         Incorrect=100*Incorrect/Total,
         Total=paste('n=',Total,sep=""),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")),
         Actual=as.factor(Actual),
         Actual=fct_relevel(Actual,"Below","Above"))%>%
  pivot_longer(cols=c('Correct','Incorrect'))%>%
  mutate(name=as.factor(name),
         name=fct_relevel(name,"Incorrect","Correct"))%>%
  ggplot(aes(y=source,x=value,fill=name))+
  geom_bar(position='stack',stat='identity')+
  geom_label(aes(label=Total,x=50),fill='white',size=8)+
  scale_fill_manual(values = c("firebrick","skyblue"))+
  xlab('Percent')+
  ylab('')+
  facet_grid(Actual~threshold)+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "bottom",legend.title = element_blank())
dev.off()



png("Data\\Figures\\as_well_pred_bars_ci3.png",height = 8,width=12,res=300,units = "in")
as_acc_metrics2%>%
  group_by(threshold,source)%>%
  summarise(Total=sum(FN_ci,TP_ci),
            Correct=TP_ci,
            Incorrect=FN_ci)%>%
  mutate(Actual='Above')%>%
  rbind(as_acc_metrics2%>%
          group_by(threshold,source)%>%
          summarise(Total=sum(TN_ci,FP_ci),
                    Correct=TN_ci,
                    Incorrect=FP_ci)%>%
          mutate(Actual='Below'))%>%
  mutate(Correct=100*Correct/Total,
         Incorrect=100*Incorrect/Total,
         Total=paste('n=',Total,sep=""),
         threshold=as.factor(threshold),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")),
         Actual=as.factor(Actual),
         Actual=fct_relevel(Actual,"Below","Above"))%>%
  pivot_longer(cols=c('Correct','Incorrect'))%>%
  mutate(name=as.factor(name),
         name=fct_relevel(name,"Incorrect","Correct"))%>%
  ggplot(aes(y=source,x=value,fill=name))+
  geom_bar(position='stack',stat='identity')+
  geom_label(aes(label=Total,x=50),fill='white',size=8)+
  scale_fill_manual(values = c("firebrick","skyblue"))+
  xlab('Percent')+
  ylab('')+
  facet_grid(Actual~threshold)+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "bottom",legend.title = element_blank())
dev.off()


as_acc_metrics2%>%
  ungroup()%>%
  mutate(total_below=TN_ci+FP_ci,
         total_above=FN_ci+TP_ci,
         sensitivity=paste(round(100*sensitivity,1),"\\% (",round(100*sensitivity_ci,1),"\\%)",sep=""),
         specificity=paste(round(100*specificity,1),"\\% (",round(100*specificity_ci,1),"\\%)",sep=""),
         accuracy=paste(round(100*accuracy,1),"\\% (",round(100*accuracy_ci,1),"\\%)",sep=""),
         threshold=fct_relevel(threshold,c("1","5","10")),
         source=as.factor(source),
         source=fct_relevel(source,c("EEA Finished NTNC or TNC","EEA Raw GW","USGS","Total")))%>%
  select(source,threshold,total_above,total_below,accuracy,sensitivity,specificity)%>%
  arrange(desc(source),threshold)%>%
  #view()
  write.csv("Tables\\prob_maps_acc3.csv",row.names = F,quote = F)


#CLEAN SUMMARY TABLES----

#Town Summaries
export_as<-as_summary%>%
  group_by(county)%>%
  mutate(parcels_county=sum(parcels),
         wells_gdb_county=sum(wells_gdb),
         wells_sm_county=sum(wells_sm))%>%
  ungroup()%>%
  group_by(region)%>%
  mutate(parcels_region=sum(parcels),
         wells_gdb_region=sum(wells_gdb),
         wells_sm_region=sum(wells_sm))%>%
  ungroup()%>%
  select(town,county,region,total_population,starts_with("parcels"),starts_with("wells_gdb"),starts_with("wells_sm"),starts_with("locations"),ends_with("sites"),starts_with("mean"),starts_with("over"),starts_with("percent"))%>%
  select(-ends_with("_int"),-over_1,-over_5,-over_10)%>%
mutate(percent_wells_gdb=wells_gdb/parcels,
       percent_wells_gdb_county=wells_gdb_county/parcels_county,
       percent_wells_gdb_region=wells_gdb_region/parcels_region,
       percent_wells_sm=wells_sm/parcels,
       percent_wells_sm_county=wells_sm_county/parcels_county,
       percent_wells_sm_region=wells_sm_region/parcels_region,
       difference=percent_wells_sm-percent_wells_gdb,
       difference_county=percent_wells_sm_county-percent_wells_gdb_county,
       difference_region=percent_wells_sm_region-percent_wells_gdb_region)%>%
  distinct()

colnames(export_as)<-gsub("_county_county","_county",colnames(export_as))
colnames(export_as)<-gsub("_region_region","_region",colnames(export_as))

write.csv(export_as,"Tables\\as_town_summary.csv",row.names = F,quote = F)



total_cancer_burden<-all_as_canc%>%
  mutate(current_csf=paste(round(cumulative_cancer_cases_current_csf,1)," (",round(per_million_cancer_cases_Current_CSF,1),")",sep=""),
         proposed_csf=paste(round(cumulative_cancer_cases_proposed_csf,1)," (",round(per_million_cancer_cases_Proposed_CSF,1),")",sep=""))%>%
  select(-starts_with("per"),-starts_with("cumulative"),-Method2)%>%
  pivot_wider(names_from = `Calculation Method`,values_from = c(ends_with("csf")))%>%
  filter(!grepl("Confidence",method))%>%
  mutate(assignment_method=ifelse(method=="CWS Average"|method=="NTNC Average",'SDWIS',assignment_method))%>%
  filter(method!="NTNC Average")%>%
  distinct()%>%
  mutate(pop=prettyNum(pop,big.mark=","))

write.table(total_cancer_burden,"Tables\\total_cancer_burden.csv",row.names = F,quote = F,sep = ";")

total_cancer_burden%>%
  filter(assignment_method!="DWP Well Viewer")%>%
  select(-assignment_method)%>%
  write.table("Tables\\total_cancer_burden_cut.csv",row.names = F,quote = F,sep=";")
