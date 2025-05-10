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
library(EnvStats)
options(scipen=999)

#THIS FILE IS THE SENSITIVITY ANALYSIS USING THRESHOLD METHODS
#Some of the code is commented out, which is the initial code which is very computationally extensice
#The first time this code is run that should be uncommented to generate the initial file
setwd('C:\\Users\\liama\\OneDrive - University of Massachusetts\\Amery MS Thesis\\Wells')

pw_vs_pws<-read.xlsx("Data\\gw_pts.xlsx")

#Below we generate a range of potential arsenic concentrations per county
counties<-read.csv("Data\\county-list.csv")%>%
  clean_names()%>%
  mutate(town=toupper(town20),
         county=toupper(county20))%>%
  select(town,county)%>%
  mutate(region=case_when(
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
  town=ifelse(town=='MANCHESTER-BY-THE-SEA','MANCHESTER',town))

county_sensitivity<-pw_vs_pws%>%
          filter(!is.na(arsenic_source_mean)&source!='EEA Finished CWS')%>%
          mutate(arsenic_source_mean=ifelse(source=='USGS',arsenic_source_mean/1000,arsenic_source_mean),
                 arsenic_recent_result=ifelse(source=='USGS',arsenic_recent_result/1000,arsenic_recent_result),
                 log_arsenic_source_mean=log(arsenic_source_mean))%>%
          merge(counties,all=T)%>%
          group_by(county)%>%
  summarise(locations=sum(!is.na(town)),
            mean_log_scale=log(mean(arsenic_source_mean,na.rm=T))-.5*log((sd(arsenic_source_mean,na.rm=T)/mean(arsenic_source_mean,na.rm=T))^2+1),
            sd_log_scale=sqrt(log((sd(arsenic_source_mean,na.rm=T)/mean(arsenic_source_mean,na.rm=T))^2+1)))%>%
  filter(!is.na(county))%>%
  mutate(mean_log_scale=ifelse(county=="NANTUCKET",mean_log_scale[county=="DUKES"],mean_log_scale),
         sd_log_scale=ifelse(county=="NANTUCKET",sd_log_scale[county=="DUKES"],sd_log_scale),
         locations=ifelse(county=="NANTUCKET",locations[county=="DUKES"],locations))

#Generate upper and lower confidence intervals
county_sensitivity_list <- empty_df <- data.frame(Var1 = numeric(0), 
                                                         Var2 = numeric(0), 
                                                         Var3 = numeric(0),
                                                         Var4 = numeric(0))

 for (i in 1:length(county_sensitivity$county)){
   int_table<-rlnorm(1000, as.numeric(county_sensitivity[i,"mean_log_scale"]), as.numeric(county_sensitivity[i,"sd_log_scale"]))
   county_sensitivity_list<-county_sensitivity_list%>%
     rbind(expand.grid(county_sensitivity$county[i],t(int_table)))
   }
colnames(county_sensitivity_list)<-c("county","as_conc")

county_sensitivity_int<-county_sensitivity_list%>%
  group_by(county)%>%
  summarise(as_county_lci=1000*quantile(as_conc,0.05),
            as_county_prob=1000*mean(as_conc),
            as_county_uci=1000*quantile(as_conc,0.95))


#Do the same by generating a range of arsenic concnetrations baed on the interpolated measurements
sm_int_wq_as<-read.csv("Data\\sm_wells_as.csv")%>%
  clean_names()%>%
  mutate(town=city)%>%
  select(town,starts_with("as"))%>% 
  replace_with_na(replace = list(as_int = -9999))


private_well_sensitivity<-sm_int_wq_as%>%
  mutate(int_avg_log_scale=log(as_int)-.5*log((as_int_se/as_int)^2+1),
         int_sd_log_scale=sqrt(log((as_int_se/as_int)^2+1)),
         as_int_lci=1000*exp(int_avg_log_scale-1.96*int_sd_log_scale),
         as_int_uci=1000*exp(int_avg_log_scale+1.96*int_sd_log_scale),
         as_int_prob=1000*as_int)%>%
  merge(counties,all.x = T)%>%
  merge(county_sensitivity_int,all.x=T)%>%
  rowid_to_column("id")

#Replace towns without information with the nearest town
town_centroids<-read.csv("Data\\town_centroids.csv")%>%
  clean_names()%>%
  select(town,lat,lon)

int_sum<-private_well_sensitivity%>%
  group_by(town)%>%
  summarise(lci=mean(as_int_lci,na.rm=T),
            mean=mean(as_int_prob,na.rm=T),
            uci=mean(as_int_uci,na.rm=T))%>%
  merge(town_centroids,all.y=T)

not_missing<-int_sum%>%
  filter(!is.na(mean))

for (i in 1:length(as.data.frame(t(int_sum)))){
  lat1<-int_sum[i,'lat']
  lon1<-int_sum[i,'lon']
  newdf<-not_missing%>%
    mutate(dist=sqrt((lat-lat1)^2+(lon-lon1)^2))%>%
    filter(dist==min(dist))
  int_sum[i,]<-int_sum[i,]%>%
    mutate(mean=ifelse(is.na(mean),newdf$mean,mean),
           lci=ifelse(is.na(lci),newdf$lci,lci),
           uci=ifelse(is.na(uci),newdf$uci,uci))
}

private_well_sensitivity<-private_well_sensitivity%>%
  merge(int_sum%>%
          select(lci,mean,uci,town),all.x=T)%>%
  mutate(as_int_lci=ifelse(is.na(as_int_lci),lci,as_int_lci),
         as_int_prob=ifelse(is.na(as_int_prob),mean,as_int_prob),
         as_int_uci=ifelse(is.na(as_int_uci),uci,as_int_uci))%>%
  select(-lci,-mean,-uci)

#generate range of possible wells per town above each threshold based on the different GW As measurements
#and upper/lower confidence intervals
well_sensitivity_town<-private_well_sensitivity%>%
  group_by(town,county,region)%>%
  summarise(wells=sum(!is.na(town)),
            as_probmaps_1_lci=sum(as_1_lci),
            as_probmaps_1_prob=sum(as_1_prob),
            as_probmaps_1_uci=sum(as_1_uci),
            as_probmaps_5_lci=sum(as_5_lci),
            as_probmaps_5_prob=sum(as_5_prob),
            as_probmaps_5_uci=sum(as_5_uci),
            as_probmaps_10_lci=sum(as_10_lci),
            as_probmaps_10_prob=sum(as_10_prob),
            as_probmaps_10_uci=sum(as_prob_10_uci),
            as_int_1_lci=sum(as_int_lci>1),
            as_int_1_prob=sum(as_int_prob>1),
            as_int_1_uci=sum(as_int_uci>1),
            as_int_5_lci=sum(as_int_lci>5),
            as_int_5_prob=sum(as_int_prob>5),
            as_int_5_uci=sum(as_int_uci>5),
            as_int_10_lci=sum(as_int_lci>10),
            as_int_10_prob=sum(as_int_prob>10),
            as_int_10_uci=sum(as_int_uci>10))

#Range of ingestion rates and bodyweights based on EPA documents
ir_chart<-read_excel("Data//irs.xlsx")%>%
  clean_names()
bw_chart<-read_excel("Data//bws.xlsx")%>%
  clean_names()

#below we clean the data and assign the age ranges to census age ranges based on the overlaps
ir_chart_clean<-ir_chart%>%
  t()%>%
  as.data.frame()%>%
  row_to_names(1)%>%
  clean_names()

ir_chart_clean[1:18]<-lapply(ir_chart_clean[1:18],as.numeric)


ir_chart_clean<-ir_chart_clean%>%
  mutate(x0_to_5=birth_to_1_month/60+x1_to_3_months*3/60+x3_to_6_months*3/60+x6_to_12_months*6/60+x1_2_years*12/60+x2_3_years*12/60+x3_6_years*24/60,
         x5_to_9=x3_6_years*1/5+x6_11_years*4/5,
         x10_to_14=x6_11_years*1/5+x11_16_years*4/5,
         x15_to_19=x16_21_years,
         x20_to_24=x16_21_years*1/5+x21_30*4/5,
         x25_to_34=x21_30*.5+x30_40*.5,
         x35_to_44=x30_40*.5+x40_50*.5,
         x45_to_54=x40_50*.5+x50_60*.5,
         x55_to_59=x50_60,
         x60_to_64=x60_70,
         x65_to_74=x60_70*.5+x70_80*.5,
         x75_to_84=x70_80*.5+x80*.5,
         x85_and_over=x80)%>%
  t()%>%
  as.data.frame()

colnames(ir_chart_clean)<-paste(colnames(ir_chart_clean),"ir",sep="_")

ir_chart_clean<-ir_chart_clean%>%
  rownames_to_column(var="age")

#Do the same for bodyweights
bw_chart_clean<-bw_chart%>%
  t()%>%
  as.data.frame()%>%
  row_to_names(1)%>%
  clean_names()

bw_chart_clean[1:17]<-lapply(bw_chart_clean[1:17],as.numeric)

bw_chart_clean<-bw_chart_clean%>%
  mutate(x0_to_5=birth_to_1_month/60+x1_to_3_months*3/60+x3_to_6_months*3/60+x6_to_12_months*6/60+x1_2_years*12/60+x2_3_years*12/60+x3_6_years*24/60,
         x5_to_9=x3_6_years*1/5+x6_11_years*4/5,
         x10_to_14=x6_11_years*1/5+x11_16_years*4/5,
         x15_to_19=x16_21_years,
         x20_to_24=x16_21_years*1/5+x21_30*4/5,
         x25_to_34=x21_30*.5+x30_40*.5,
         x35_to_44=x30_40*.5+x40_50*.5,
         x45_to_54=x40_50*.5+x50_60*.5,
         x55_to_59=x50_60,
         x60_to_64=x60_70,
         x65_to_74=x60_70*.5+x70_80*.5,
         x75_to_84=x70_80*.5+x80*.5,
         x85_and_over=x80)%>%
  t()%>%
  as.data.frame()

colnames(bw_chart_clean)<-paste(colnames(bw_chart_clean),"bw",sep="_")

bw_chart_clean<-bw_chart_clean%>%
  rownames_to_column(var="age")
  
ir_bw_clean<-ir_chart_clean%>%
  merge(bw_chart_clean,all=T)

town_sources<-read.csv("Data//town_water_sources.csv")#descrition of water source percentages by town

census_data<-read.csv("Data\\census_data.csv") #summarized census data

census_clean<-census_data%>%
  clean_names()%>%
  select(-median_age_years,-median_year_structure_built)%>%
  pivot_longer(cols=contains("year"),values_to = "percent_pop",names_to = "age")%>%
  select(city,total_population,age,percent_pop)%>%
  mutate(age=gsub("_years","",age),
         age=ifelse(age=="under_5",'x0_to_5',age),
         town=city)%>%
  merge(ir_bw_clean,all.x=T)%>%
  mutate(ir_5th=mean_ir/1000-1.96*se_ir/1000,
         ir_mean=mean_ir/1000,
         ir_95th=mean_ir/1000+1.96*se_ir/1000,
         percent_consuming_lci=percent_consuming_ir/100,
         percent_consuming_mean=0.9,
         percent_consuming_uci=1,
         pop=percent_pop*total_population)%>%
  mutate(ef_non_cancer=1,
         ef_cancer=ifelse(age=='x0_to_5'|age=='x5_to_9'|age=='x10_to_14'|age=='x15_to_19',21/78,33/78))%>%
  select(town,age,total_population,percent_consuming_lci,percent_consuming_mean,percent_consuming_uci,ef_non_cancer,ef_cancer,pop,x5th_bw,mean_bw,x95th_bw,ir_5th,ir_mean,ir_95th)%>%
  merge(town_sources%>%
          mutate(wells_lci=wells/parcels,
                 wells_uci=1-(served_by_pws/parcels),
                 town=city)%>%
          select(wells_lci,wells_uci,town),all.x=T)

#Generate range of poential arsenic concentrations
as_conc_chart<-well_sensitivity_town%>%
  pivot_longer(cols = starts_with("as"), names_to = c("method","threshold","confidence_as_conc"), 
               names_pattern ="as_(.*)_(.*)_(.*)",values_to = "as_conc")%>%
  mutate(as_exposed=ifelse(threshold==1,0.001,NA),
         as_exposed=ifelse(threshold==5,0.005,as_exposed),
         as_exposed=ifelse(threshold==10,0.01,as_exposed),
         percent=as_conc/wells)%>%
  group_by(town,county,region,wells,method,confidence_as_conc)%>%
  mutate(percent=ifelse(threshold==5,percent-mean(percent[threshold==10]),percent),
         percent=ifelse(threshold==1,2*percent-sum(percent),percent),
         percent=ifelse(percent<0,0,percent))%>%
  group_by(town,county,region,wells,method,confidence_as_conc)%>%
  group_modify(~ add_row(.x,.before=0))%>%
  mutate(threshold=ifelse(is.na(threshold),0,threshold),
         as_exposed=ifelse(is.na(as_exposed),0,as_exposed),
         percent=ifelse(threshold==0,1-sum(percent,na.rm=T),percent))%>%
  select(-as_conc)

#clean column names
census_clean2=census_clean%>%
  pivot_longer(cols = ends_with("_bw"), names_to = c("confidence_bw"), 
               names_pattern ="(.*)_bw",values_to = "bw")%>%
  pivot_longer(cols = starts_with("ir_"), names_to = c("confidence_ir"), 
               names_pattern ="ir_(.*)",values_to = "ir")%>%
  pivot_longer(cols = starts_with("wells_"), names_to = c("confidence_wells"), 
               names_pattern ="wells_(.*)",values_to = "wells")%>%
  mutate(ir_bw=ir/bw)

#Write as summary table of values used
census_clean%>%
  select(age,percent_consuming_lci,x5th_bw,mean_bw,x95th_bw,ir_5th,ir_mean,ir_95th)%>%
  distinct()%>%
  mutate(percent_consuming_lci=round(percent_consuming_lci,2),
         x5th_bw=round(x5th_bw,1),
         mean_bw=round(mean_bw,1),
         x95th_bw=round(x95th_bw,1),
         ir_5th=round(ir_5th,3),
         ir_mean=round(ir_mean,3),
         ir_95th=round(ir_95th,3),
         age=gsub("x","",age),
         age=gsub("_","",age),
         age=gsub("to","-",age),
         age=gsub("andover","+",age),
         age2=as.numeric(gsub("\\-.*", "", age)))%>%
  arrange(age2)%>%
  write.table("Tables\\ir_bw_sa.csv",row.names = F,quote = F,sep=";")

#RUN BELOW CODE ONCE WEHN GENERATING ANALYSIS

# full_sensitivity_analysis<-census_clean2%>%
#   ungroup()%>%
#   merge(as_conc_chart%>%
#           ungroup()%>%
#           select(-wells),all.x=T)%>%
#   mutate(dose=1000*as_exposed*ir_bw*ef_cancer,
#          dose_nc=1000*as_exposed*ir_bw,
#          bladder_cancer_prob=ifelse(dose<0.22,dose*0.0062,0.0046*dose^2+dose*0.0053),
#          bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.00006,.00001*dose^2+dose*.00007),
#          bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0177,0.0184*dose^2+0.0137*dose),
#          lung_cancer_prob=ifelse(dose<0.22,dose*0.0078,0.0025*dose^2+.0077*dose),
#          lung_cancer_prob_5=ifelse(dose<0.22,dose*.0002,dose^2*.00004+.0002*dose),
#          lung_cancer_prob_95=ifelse(dose<0.22,dose*.0214,0.0075*dose^2+0.0205*dose),
#          combined_cancer_prob_5=bladder_cancer_prob_5+lung_cancer_prob_5,
#          combined_cancer_prob=bladder_cancer_prob+lung_cancer_prob,
#          combined_cancer_prob_95=ifelse(dose<0.22,dose*0.0317,dose*0.0317),
#          old_csf=dose*1.5/1000,
#          #cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
#          #cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
#          #fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
#          #fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
#          #ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
#          #ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
#          #fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
#         # fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
#          proposed_rfd=0.031,
#          current_rfd=0.3,
#          percent_treatment_lci=0,
#          percent_treatment_mean=.4,
#          percent_treatment_uci=.7,
#          treatment_effectiveness_lci=.2,
#          treatment_effectiveness_uci=0.99)
# 
# full_sensitivity_analysis<-full_sensitivity_analysis%>%
#   select(-starts_with("lung"),-starts_with("bladder"),-starts_with("cvd"),-starts_with("ihd"),-starts_with("fatal"))%>%
#   pivot_longer(cols = starts_with("percent_consuming"), names_to = c("confidence_percent_consuming"),
#                names_pattern ="percent_consuming_(.*)",values_to = "percent_consuming")%>%
#   pivot_longer(cols = starts_with("percent_treatment"), names_to = c("confidence_percent_treatment"),
#                names_pattern ="percent_treatment_(.*)",values_to = "percent_treatment")%>%
#   pivot_longer(cols = starts_with("treatment_effectiveness"), names_to = c("confidence_treatment_effectiveness"),
#                names_pattern ="treatment_effectiveness_(.*)",values_to = "treatment_effectiveness")#%>%
# 
# full_sensitivity_analysis<-full_sensitivity_analysis%>%
#   select(-proposed_rfd,-current_rfd,-dose_nc,-town,-region,-old_csf)%>%
#   pivot_longer(cols = starts_with("combined_cancer"), names_to = c("confidence_combined_cancer"),
#                names_pattern ="combined_cancer_(.*)",values_to = "combined_cancer")
# 
# 
# sensitivity_results<-full_sensitivity_analysis%>%
#   group_by(confidence_wells,confidence_ir,confidence_bw,method,confidence_as_conc,confidence_percent_consuming,confidence_percent_treatment,confidence_treatment_effectiveness,confidence_combined_cancer)%>%
#   summarise(cancer_burden=sum(pop*wells*combined_cancer*percent*(percent_consuming*((percent_treatment)*(1-treatment_effectiveness)+(1-percent_treatment))),na.rm=T))
# 
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('avg',"mean",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('prob_5',"lci",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('prob_95',"uci",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('prob',"avg",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('x5th',"lci",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('x95th',"uci",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('95th',"uci",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('5th',"lci",x))
# sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('avg',"mean",x))
# 
# sensitivity_results<-sensitivity_results%>%
#   mutate(cancer_burden=as.numeric(cancer_burden))
# 
# write.xlsx(sensitivity_results,"Data\\sensitivity_results.xlsx")

# pws_arsenic_data<-read.csv("Data\\pws_arsenic_data.csv")
# 
# 
# pws_sensitivity<-pws_arsenic_data%>%
#   select(ends_with("result"),population_served_count,town)%>%
#   pivot_longer(cols = ends_with("result"), names_to = c("confidence_as_conc"),
#                names_pattern ="(.*)_result",values_to = "as_conc")%>%
#   mutate(threshold_0=0,
#          threshold_1=0,
#          threshold_5=0,
#          threshold_10=0)%>%
#   pivot_longer(cols = starts_with("threshold"), names_to = c("threshold"),
#                names_pattern ="threshold_(.*)",values_to = "population")%>%
#   mutate(population=ifelse(as_conc<=0.001&threshold==0,population_served_count,population),
#          population=ifelse(as_conc>0.001&as_conc<=0.005&threshold==1,population_served_count,population),
#          population=ifelse(as_conc>0.005&as_conc<=0.01&threshold==5,population_served_count,population),
#          population=ifelse(as_conc>0.01&threshold==10,population_served_count,population))%>%
#   group_by(town,confidence_as_conc,threshold)%>%
#   summarise(population_served_count=sum(population_served_count),
#             population=sum(population))%>%
#   merge(census_clean2%>%
#           mutate(pop=pop/total_population),all=T)%>%
#   mutate(as_exposed=as.numeric(threshold))
# 
# pws_sensitivity<-pws_sensitivity%>%
#   mutate(dose=as_exposed*ir_bw*ef_cancer,
#          dose_nc=as_exposed*ir_bw,
#          bladder_cancer_prob=ifelse(dose<0.22,dose*0.0062,0.0046*dose^2+dose*0.0053),
#          bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.00006,.00001*dose^2+dose*.00007),
#          bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0177,0.0184*dose^2+0.0137*dose),
#          lung_cancer_prob=ifelse(dose<0.22,dose*0.0078,0.0025*dose^2+.0077*dose),
#          lung_cancer_prob_5=ifelse(dose<0.22,dose*.0002,dose^2*.00004+.0002*dose),
#          lung_cancer_prob_95=ifelse(dose<0.22,dose*.0214,0.0075*dose^2+0.0205*dose),
#          combined_cancer_prob_5=bladder_cancer_prob_5+lung_cancer_prob_5,
#          combined_cancer_prob=bladder_cancer_prob+lung_cancer_prob,
#          combined_cancer_prob_95=ifelse(dose<0.22,dose*0.0317,dose*0.0317),
#          old_csf=dose*1.5/1000,
#          cvd_extra_risk=-0.0078*dose_nc^2+0.1611*dose_nc,
#          cvd_extra_risk_95=-0.093*dose_nc^2+0.5434*dose_nc,
#          fatal_cvd_extra_risk=0.0039*dose_nc^2+0.039*dose_nc,
#          fatal_cvd_extra_risk_95=0.0362*dose_nc^2+0.1123*dose_nc,
#          ihd_extra_risk=0.0174*dose_nc^2+0.131*dose_nc,
#          ihd_extra_risk_95=0.0585*dose_nc^3-0.026*dose_nc^2+0.361*dose_nc,
#          fatal_ihd_extra_risk=0.0074*dose_nc^2+0.0325*dose_nc,
#          fatal_ihd_extra_risk_95=0.0127*dose_nc^3+0.05*dose_nc^2+0.0861*dose_nc,
#          proposed_rfd=0.031,
#          current_rfd=0.3,
#          percent_treatment_lci=0,
#          percent_treatment_mean=.4,
#          percent_treatment_uci=.7,
#          treatment_effectiveness_lci=.2,
#          treatment_effectiveness_uci=0.99)
# 
# pws_sensitivity<-pws_sensitivity%>%
#   select(-starts_with("lung"),-starts_with("bladder"),-starts_with("cvd"),-starts_with("ihd"),-starts_with("fatal"))%>%
#   select(-proposed_rfd,-current_rfd,-dose_nc,-town,-old_csf)%>%
#   pivot_longer(cols = starts_with("percent_consuming"), names_to = c("confidence_percent_consuming"),
#                names_pattern ="percent_consuming_(.*)",values_to = "percent_consuming")%>%
#   pivot_longer(cols = starts_with("percent_treatment"), names_to = c("confidence_percent_treatment"),
#                names_pattern ="percent_treatment_(.*)",values_to = "percent_treatment")%>%
#   pivot_longer(cols = starts_with("treatment_effectiveness"), names_to = c("confidence_treatment_effectiveness"),
#                names_pattern ="treatment_effectiveness_(.*)",values_to = "treatment_effectiveness")
# 
# pws_sensitivity<-pws_sensitivity%>%
#   pivot_longer(cols = starts_with("combined_cancer"), names_to = c("confidence_combined_cancer"),
#                names_pattern ="combined_cancer_(.*)",values_to = "combined_cancer")
# 
# pws_sensitivity_results<-pws_sensitivity%>%
#   group_by(confidence_ir,confidence_bw,confidence_as_conc,confidence_percent_consuming,confidence_percent_treatment,confidence_treatment_effectiveness,confidence_combined_cancer)%>%
#   summarise(cancer_burden=sum(population*pop*combined_cancer*(percent_consuming*((percent_treatment)*(1-treatment_effectiveness)+(1-percent_treatment))),na.rm=T))
# 
# 
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('avg',"mean",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('prob_5',"lci",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('prob_95',"uci",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('prob',"avg",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('x5th',"lci",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('x95th',"uci",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('95th',"uci",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('5th',"lci",x))
# pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('avg',"mean",x))
# 
# pws_sensitivity_results<-pws_sensitivity_results%>%
#   mutate(cancer_burden=as.numeric(cancer_burden))
# 
# write.xlsx(pws_sensitivity_results,"Data\\pws_sensitivity_results.xlsx")

#Read in previously generated tables of range of values for PWS and Private Well Cancer Cases based on different methods
sensitivity_results<-read.xlsx("Data\\sensitivity_results.xlsx")%>%
  drop_na()

pws_sensitivity_results<-read.xlsx("Data\\pws_sensitivity_results.xlsx")%>%
  drop_na()

#Set baseline conditions for sensitivity analysis
sensitivity_results_chart<-sensitivity_results%>%
  ungroup()%>%
  filter(confidence_wells=="uci"& #PWS Maps
           confidence_ir=="mean"&
           confidence_bw=="mean"&
           method=="int"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           confidence_percent_treatment=="lci"& #no people using treatment
           confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")

#Below we generat a range of values based on changing each assumption
sensitivity_results_chart_all<-sensitivity_results%>%
  ungroup()%>%
  filter(confidence_wells=="uci"&
           confidence_ir=="mean"&
           confidence_bw=="mean"&
           method=="int"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           #confidence_percent_treatment=="lci"&
           #confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")%>%
  select(cancer_burden)%>%
  mutate(Variable="Treatment Use and Effectiveness")%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   #confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Percent Consuming"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   #method=="int"&
                   #confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Arsenic Concentration"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   #confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Ingestion Rate"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   #confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Body Weights"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci")%>%
                   #confidence_combined_cancer=="mean")
          select(cancer_burden)%>%
          mutate(Variable="Cancer Slope Factor"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(#confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Well Estimate"))%>%
  mutate(Baseline=sensitivity_results_chart$cancer_burden,
         cancer_burden=cancer_burden-Baseline)

#Threshold Sensitivity Analysis Figures and tables----
png("Data\\Figures\\sens_anal.png",height = 4.5,width=14,res=300,units = "in")
sens_anal<-sensitivity_results_chart_all%>%
  group_by(Variable)%>%
  filter(cancer_burden==min(cancer_burden)|cancer_burden==max(cancer_burden))%>%
  ungroup()%>%
  mutate(grp=ifelse(cancer_burden<0,"neg","pos"),
         Variable=as.factor(Variable))%>%
  mutate(Variable=fct_rev(fct_relevel(Variable,"Well Estimate","Arsenic Concentration","Treatment Use and Effectiveness","Percent Consuming","Ingestion Rate","Body Weights",
                                      "Cancer Slope Factor")),
         loc=ifelse(grp=="neg",cancer_burden-15,cancer_burden+20),
         n=round(Baseline+cancer_burden))%>%
  ggplot(aes(fill=grp))+
  geom_bar(aes(x=cancer_burden,y=Variable),stat="identity")+
  geom_vline(aes(xintercept=Baseline-Baseline))+
  geom_text(aes(x=loc,y=Variable,label=n),size=6.5)+
  ylab('')+
  xlab('Statewide Cancer Cases Relative to Baseline')+
  scale_fill_manual(values = c("skyblue3",'darkred'))+
  scale_x_continuous(sec.axis = sec_axis(~ sensitivity_results_chart$cancer_burden + ., name = "Statewide Cancer Cases"),limits = c(-sensitivity_results_chart$cancer_burden-25 ,-sensitivity_results_chart$cancer_burden+620))+
  theme_classic2()+
  theme(text = element_text(size=24),legend.position = "none",axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank(), axis.title.x.bottom = element_blank())
sens_anal
dev.off()  


sensitivity_results_chart_all%>%
  mutate(cases=cancer_burden+Baseline)%>%
  group_by(Variable)%>%
  filter(cases==min(cases)|cases==max(cases))%>%
  mutate(range=ifelse(cases==min(cases),"min","max"))%>%
  select(Variable,cases,range)%>%
  distinct()%>%
  mutate(cases=round(cases,1))%>%
  pivot_wider(values_from =cases,names_from = range)%>%
  select(Variable,min,max)%>%
  write.csv("Tables\\private_wells_ci.csv",row.names = F,quote = F)

pws_sensitivity_results_chart<-pws_sensitivity_results%>%
  ungroup()%>%
  filter(confidence_ir=="mean"&
           confidence_bw=="mean"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           confidence_percent_treatment=="lci"&
           confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")

pws_sensitivity_results_chart_all<-pws_sensitivity_results%>%
  ungroup()%>%
  filter(confidence_ir=="mean"&
           confidence_bw=="mean"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           #confidence_percent_treatment=="lci"&
           #confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")%>%
  select(cancer_burden)%>%
  mutate(Variable="Treatment Use and Effectiveness")%>%
  rbind(pws_sensitivity_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   #confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Percent Consuming"))%>%
  rbind(pws_sensitivity_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Arsenic Concentration"))%>%
  rbind(pws_sensitivity_results%>%
          ungroup()%>%
          filter(#confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Ingestion Rate"))%>%
  rbind(pws_sensitivity_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
            #confidence_bw=="mean"&
            confidence_as_conc=="mean"&
              confidence_percent_consuming=="uci"&
              confidence_percent_treatment=="lci"&
              confidence_treatment_effectiveness=="uci"&
              confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Body Weights"))%>%
  rbind(pws_sensitivity_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci")%>%
          #confidence_combined_cancer=="uci")
          select(cancer_burden)%>%
          mutate(Variable="Cancer Slope Factor"))%>%
  mutate(Baseline=pws_sensitivity_results_chart$cancer_burden,
         cancer_burden=cancer_burden-Baseline)

png("Data\\Figures\\pws_sens_anal.png",height = 4,width=14,res=300,units = "in")
pws_sens_anal<-pws_sensitivity_results_chart_all%>%
  group_by(Variable)%>%
  filter(cancer_burden==min(cancer_burden)|cancer_burden==max(cancer_burden))%>%
  ungroup()%>%
  mutate(grp=ifelse(cancer_burden<0,"neg","pos"),
         Variable=as.factor(Variable))%>%
  mutate(Variable=fct_rev(fct_relevel(Variable,"Arsenic Concentration","Treatment Use and Effectiveness","Percent Consuming","Ingestion Rate","Body Weights",
                                      "Cancer Slope Factor")),
         loc=ifelse(grp=="neg",cancer_burden-20,cancer_burden+20),
         n=round(Baseline+cancer_burden))%>%
  ggplot(aes(fill=grp))+
  geom_bar(aes(x=cancer_burden,y=Variable),stat="identity")+
  geom_vline(aes(xintercept=Baseline-Baseline))+
  geom_text(aes(x=loc,y=Variable,label=n),size=6.5)+
  ylab('')+
  xlab('Statewide Cancer Cases Relative to Baseline')+
  scale_fill_manual(values = c("skyblue3",'darkred'))+
  scale_x_continuous(sec.axis = sec_axis(~ pws_sensitivity_results_chart$cancer_burden + ., name = ""),limits = c(-pws_sensitivity_results_chart$cancer_burden-25 ,-pws_sensitivity_results_chart$cancer_burden+620))+
  theme_classic2()+
  theme(text = element_text(size=24),legend.position = "none",axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank(), axis.title.x.bottom = element_blank())
pws_sens_anal
dev.off()  

pws_sensitivity_results_chart_all%>%
  mutate(cases=cancer_burden+Baseline)%>%
  group_by(Variable)%>%
  filter(cases==min(cases)|cases==max(cases))%>%
  mutate(range=ifelse(cases==min(cases),"min","max"))%>%
  select(Variable,cases,range)%>%
  distinct()%>%
  mutate(cases=round(cases,1))%>%
  pivot_wider(values_from =cases,names_from = range)%>%
  select(Variable,min,max)%>%
  write.csv("Tables\\cws_ci.csv",row.names = F,quote = F)

sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('mean',"Middle Estimate",x))
sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('lci',"Lower Estimate",x))
sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('xuci',"Upper Estimate",x))
sensitivity_results[]<-lapply(sensitivity_results,function(x) gsub('uci',"Upper Estimate",x))
sensitivity_results<-sensitivity_results%>%
  mutate(cancer_burden=as.numeric(cancer_burden))

pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('mean',"Middle Estimate",x))
pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('lci',"Lower Estimate",x))
pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('xuci',"Upper Estimate",x))
pws_sensitivity_results[]<-lapply(pws_sensitivity_results,function(x) gsub('uci',"Upper Estimate",x))
pws_sensitivity_results<-pws_sensitivity_results%>%
     mutate(cancer_burden=as.numeric(cancer_burden))

a<-sensitivity_results%>%
  mutate(confidence_ir=ifelse(confidence_ir=="9Lower Estimate","Upper Estimate",confidence_ir),
         confidence_bw=gsub(" Estimate","",confidence_bw))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_ir,fill=confidence_bw))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Ingestion Rate Estimate')+
  ggtitle('Ingestion Rate and Bodyweight')+
  guides(fill=guide_legend(title="Bodyweight Estimate"))+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

b<-sensitivity_results%>%
  mutate(method=ifelse(method=="int","Interpolated","Probability Maps"))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_as_conc,fill=method))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Arsenic Concentration Estimate')+
  ggtitle('Groundwater Arsenic Estimate')+
  guides(fill=guide_legend(title= "Arsenic Calculation Method"))+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

c<-sensitivity_results%>%
  mutate(confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Lower Estimate","0.2 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Upper Estimate","0.99 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_percent_treatment=gsub(" Estimate","",confidence_percent_treatment),
         confidence_percent_consuming=gsub(" Estimate","",confidence_percent_consuming))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(fill=confidence_percent_consuming,x=confidence_percent_treatment))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Percent using Treatment Estimate')+
  ggtitle('Treatment Use')+
  guides(fill=guide_legend(title= "Percent Consuming Estimate"))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  facet_wrap(~confidence_treatment_effectiveness)+
  theme(text = element_text(size=24),legend.position = "top")

d<-sensitivity_results%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_combined_cancer),fill='firebrick')+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('CSF Estimate')+
  ggtitle('Cancer Toxicity')+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")


png("Data\\Figures\\sens_anal_wells.png",height = 15,width=22,res=300,units = "in")
ggarrange(a,b,c,d,ncol = 2,nrow=2)
dev.off()


a<-pws_sensitivity_results%>%
  mutate(confidence_ir=ifelse(confidence_ir=="9Lower Estimate","Upper Estimate",confidence_ir),
         confidence_bw=gsub(" Estimate","",confidence_bw))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_ir,fill=confidence_bw))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Ingestion Rate Estimate')+
  ggtitle('Ingestion Rate and Bodyweight')+
  guides(fill=guide_legend(title="Bodyweight Estimate"))+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

b<-pws_sensitivity_results%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_as_conc),fill='blue3',color='black')+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Arsenic Concentration Estimate')+
  ggtitle('CWS Arsenic Estimate')+
  #guides(fill=guide_legend(title= "Arsenic Calculation Method"))+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

c<-pws_sensitivity_results%>%
  mutate(confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Lower Estimate","0.2 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Upper Estimate","0.99 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_percent_treatment=gsub(" Estimate","",confidence_percent_treatment),
         confidence_percent_consuming=gsub(" Estimate","",confidence_percent_consuming))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(fill=confidence_percent_consuming,x=confidence_percent_treatment))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Percent using Treatment Estimate')+
  ggtitle('Treatment Use')+
  guides(fill=guide_legend(title= "Percent Consuming Estimate"))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  facet_wrap(~confidence_treatment_effectiveness)+
  theme(text = element_text(size=24),legend.position = "top")

d<-pws_sensitivity_results%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_combined_cancer),fill='firebrick')+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('CSF Estimate')+
  ggtitle('Cancer Toxicity')+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")


png("Data\\Figures\\sens_anal_cws.png",height = 15,width=22,res=300,units = "in")
ggarrange(a,b,c,d,ncol = 2,nrow=2)
dev.off()


png("Data\\Figures\\sens_anal_range_cws.png",height = 8,width=12,res=300,units = "in")
pws_sensitivity_results%>%
  ggplot()+
  geom_histogram(aes(x=cancer_burden),fill='white',color='black',size=1.25)+
  scale_x_log10()+
  ylab('Count')+
  xlab('Statewide Cancer Cases')+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")
dev.off()

png("Data\\Figures\\sens_anal_range_wells.png",height = 8,width=12,res=300,units = "in")
sensitivity_results%>%
  ggplot()+
  geom_histogram(aes(x=cancer_burden),fill='white',color='black',size=1.25)+
  scale_x_log10()+
  ylab('Count')+
  xlab('Statewide Cancer Cases')+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")
dev.off()

#VALUE BASED----

#The same analysis as above is done with the value-based analysis vs the threshold analysis

well_val_sens<-private_well_sensitivity%>%
  group_by(town,county,region)%>%
  summarise(wells=sum(!is.na(town)),
            as_int_lci=mean(as_int_lci),
            as_int_prob=mean(as_int_prob),
            as_int_uci=mean(as_int_uci),
            as_county_lci=mean(as_county_lci),
            as_county_prob=mean(as_county_prob),
            as_county_uci=mean(as_county_uci))%>%
  pivot_longer(cols = starts_with("as"), names_to = c("method","confidence_as_conc"),
                names_pattern ="as_(.*)_(.*)",values_to = "as_conc")%>%
  filter(method=="int")%>%
  select(-wells)%>%
  merge(census_clean2,all=T)%>%
  mutate(dose=as_conc*ir_bw*ef_cancer,
         dose_nc=as_conc*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0062,0.0046*dose^2+dose*0.0053),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.00006,.00001*dose^2+dose*.00007),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0177,0.0184*dose^2+0.0137*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0078,0.0025*dose^2+.0077*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*.0002,dose^2*.00004+.0002*dose),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*.0214,0.0075*dose^2+0.0205*dose),
         combined_cancer_prob_5=bladder_cancer_prob_5+lung_cancer_prob_5,
         combined_cancer_prob=bladder_cancer_prob+lung_cancer_prob,
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.0317,dose*0.0317),
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
         percent_treatment_lci=0,
         percent_treatment_mean=.4,
         percent_treatment_uci=.7,
         treatment_effectiveness_lci=.2,
         treatment_effectiveness_uci=0.99)%>%
    select(-starts_with("lung"),-starts_with("bladder"),-starts_with("cvd"),-starts_with("ihd"),-starts_with("fatal"))%>%
    pivot_longer(cols = starts_with("percent_consuming"), names_to = c("confidence_percent_consuming"),
                 names_pattern ="percent_consuming_(.*)",values_to = "percent_consuming")%>%
    pivot_longer(cols = starts_with("percent_treatment"), names_to = c("confidence_percent_treatment"),
                 names_pattern ="percent_treatment_(.*)",values_to = "percent_treatment")%>%
    pivot_longer(cols = starts_with("treatment_effectiveness"), names_to = c("confidence_treatment_effectiveness"),
                 names_pattern ="treatment_effectiveness_(.*)",values_to = "treatment_effectiveness")%>%
    pivot_longer(cols = starts_with("combined_cancer"), names_to = c("confidence_combined_cancer"),
                 names_pattern ="combined_cancer_(.*)",values_to = "combined_cancer")

well_sensitivity_val_results<-well_val_sens%>%
  group_by(confidence_ir,confidence_bw,confidence_as_conc,confidence_wells,confidence_percent_consuming,confidence_percent_treatment,confidence_treatment_effectiveness,confidence_combined_cancer)%>%
  summarise(cancer_burden=sum(wells*pop*combined_cancer*(percent_consuming*((percent_treatment)*(1-treatment_effectiveness)+(1-percent_treatment))),na.rm=T))


well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('avg',"mean",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('prob_5',"lci",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('prob_95',"uci",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('prob',"avg",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('x5th',"lci",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('x95th',"uci",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('95th',"uci",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('5th',"lci",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('avg',"mean",x))

well_sensitivity_val_results<-well_sensitivity_val_results%>%
  mutate(cancer_burden=as.numeric(cancer_burden))

write.xlsx(well_sensitivity_val_results,"Data\\well_sensitivity_val_results.xlsx")


pws_arsenic_data<-read.csv("Data\\pws_arsenic_data.csv")%>%
  mutate(lci_result=ifelse(lci_result==0,0.0000500000,lci_result))

pws_sensitivity_val<-pws_arsenic_data%>%
  select(ends_with("result"),population_served_count,town)%>%
  pivot_longer(cols = ends_with("result"), names_to = c("confidence_as_conc"),
               names_pattern ="(.*)_result",values_to = "as_conc")%>%
group_by(town,confidence_as_conc)%>%
  summarise(population=sum(population_served_count),
             as_exposed=sum(population_served_count*as_conc)/sum(population_served_count))%>%
  merge(census_clean2,all=T)

pws_sensitivity_val<-pws_sensitivity_val%>%
  mutate(dose=1000*as_exposed*ir_bw*ef_cancer,
         dose_nc=1000*as_exposed*ir_bw,
         bladder_cancer_prob=ifelse(dose<0.22,dose*0.0062,0.0046*dose^2+dose*0.0053),
         bladder_cancer_prob_5=ifelse(dose<0.22,dose*0.00006,.00001*dose^2+dose*.00007),
         bladder_cancer_prob_95=ifelse(dose<0.22,dose*0.0177,0.0184*dose^2+0.0137*dose),
         lung_cancer_prob=ifelse(dose<0.22,dose*0.0078,0.0025*dose^2+.0077*dose),
         lung_cancer_prob_5=ifelse(dose<0.22,dose*.0002,dose^2*.00004+.0002*dose),
         lung_cancer_prob_95=ifelse(dose<0.22,dose*.0214,0.0075*dose^2+0.0205*dose),
         combined_cancer_prob_5=bladder_cancer_prob_5+lung_cancer_prob_5,
         combined_cancer_prob=bladder_cancer_prob+lung_cancer_prob,
         combined_cancer_prob_95=ifelse(dose<0.22,dose*0.0317,dose*0.0317),
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
         percent_treatment_lci=0,
         percent_treatment_mean=.4,
         percent_treatment_uci=.7,
         treatment_effectiveness_lci=.2,
         treatment_effectiveness_uci=0.99)

pws_sensitivity_val<-pws_sensitivity_val%>%
  select(-starts_with("lung"),-starts_with("bladder"),-starts_with("cvd"),-starts_with("ihd"),-starts_with("fatal"))%>%
  pivot_longer(cols = starts_with("percent_consuming"), names_to = c("confidence_percent_consuming"),
               names_pattern ="percent_consuming_(.*)",values_to = "percent_consuming")%>%
  pivot_longer(cols = starts_with("percent_treatment"), names_to = c("confidence_percent_treatment"),
               names_pattern ="percent_treatment_(.*)",values_to = "percent_treatment")%>%
  pivot_longer(cols = starts_with("treatment_effectiveness"), names_to = c("confidence_treatment_effectiveness"),
               names_pattern ="treatment_effectiveness_(.*)",values_to = "treatment_effectiveness")%>%
  pivot_longer(cols = starts_with("combined_cancer"), names_to = c("confidence_combined_cancer"),
               names_pattern ="combined_cancer_(.*)",values_to = "combined_cancer")

pws_sensitivity_val_results<-pws_sensitivity_val%>%
  group_by(confidence_ir,confidence_bw,confidence_as_conc,confidence_percent_consuming,confidence_percent_treatment,confidence_treatment_effectiveness,confidence_combined_cancer)%>%
  summarise(cancer_burden=sum(pop*combined_cancer*(percent_consuming*((percent_treatment)*(1-treatment_effectiveness)+(1-percent_treatment))),na.rm=T))


pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('avg',"mean",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('prob_5',"lci",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('prob_95',"uci",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('prob',"avg",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('x5th',"lci",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('x95th',"uci",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('95th',"uci",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('5th',"lci",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('avg',"mean",x))

pws_sensitivity_val_results<-pws_sensitivity_val_results%>%
  mutate(cancer_burden=as.numeric(cancer_burden))

write.xlsx(pws_sensitivity_val_results,"Data\\pws_sensitivity_val_results.xlsx")

well_sensitivity_val_results<-read.xlsx("Data\\well_sensitivity_val_results.xlsx")%>%
  drop_na()

well_sensitivity_results_val_chart<-well_sensitivity_val_results%>%
  ungroup()%>%
  filter(confidence_wells=="uci",
         confidence_ir=="mean"&
           confidence_bw=="mean"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           confidence_percent_treatment=="lci"&
           confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")

well_sensitivity_results_val_chart_all<-well_sensitivity_val_results%>%
  ungroup()%>%
  filter(confidence_wells=="uci",
         confidence_ir=="mean"&
           confidence_bw=="mean"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           #confidence_percent_treatment=="lci"&
           #confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")%>%
  select(cancer_burden)%>%
  mutate(Variable="Treatment Use and Effectiveness")%>%
  rbind(well_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci",
                 confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   #confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Percent Consuming"))%>%
  rbind(well_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci",
                 confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Arsenic Concentration"))%>%
  rbind(well_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci",
                # confidence_ir=="mean"&
            confidence_bw=="mean"&
              confidence_as_conc=="mean"&
              confidence_percent_consuming=="uci"&
              confidence_percent_treatment=="lci"&
              confidence_treatment_effectiveness=="uci"&
              confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Ingestion Rate"))%>%
  rbind(well_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci",
                 confidence_ir=="mean"&
                   #confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Body Weights"))%>%
  rbind(well_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci",
                 confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci")%>%
          select(cancer_burden)%>%
          mutate(Variable="Cancer Slope Factor"))%>%
  rbind(well_sensitivity_val_results%>%
          ungroup()%>%
          filter(#confidence_wells=="uci"&
            confidence_ir=="mean"&
              confidence_bw=="mean"&
              confidence_as_conc=="mean"&
              confidence_percent_consuming=="uci"&
              confidence_percent_treatment=="lci"&
              confidence_treatment_effectiveness=="uci"&
              confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Well Estimate"))%>%
  mutate(Baseline=well_sensitivity_results_val_chart$cancer_burden,
         cancer_burden=cancer_burden-Baseline)

png("Data\\Figures\\well_val_sens_anal.png",height = 4,width=14,res=300,units = "in")
well_sensitivity_results_val_chart_all%>%
  group_by(Variable)%>%
  filter(cancer_burden==min(cancer_burden)|cancer_burden==max(cancer_burden))%>%
  ungroup()%>%
  mutate(grp=ifelse(cancer_burden<0,"neg","pos"),
         Variable=as.factor(Variable))%>%
  mutate(Variable=fct_rev(fct_relevel(Variable,"Well Estimate","Arsenic Concentration","Treatment Use and Effectiveness","Percent Consuming","Ingestion Rate","Body Weights",
                                      "Cancer Slope Factor")),
         loc=ifelse(grp=="neg",cancer_burden-15,cancer_burden+15),
         n=round(Baseline+cancer_burden))%>%
  ggplot(aes(fill=grp))+
  geom_bar(aes(x=cancer_burden,y=Variable),stat="identity")+
  geom_vline(aes(xintercept=Baseline-Baseline))+
  geom_text(aes(x=loc,y=Variable,label=n),size=6.5)+
  ylab('')+
  xlab('Statewide Cancer Cases Relative to Baseline')+
  scale_fill_manual(values = c("skyblue3","darkred"))+
  scale_x_continuous(sec.axis = sec_axis(~ well_sensitivity_results_val_chart$cancer_burden + ., name = "Statewide Cancer Cases"),limits = c(-200,500))+
  theme_classic2()+
  theme(text = element_text(size=24),legend.position = "none",axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank(), axis.title.x.bottom = element_blank())
dev.off()  

well_sensitivity_results_val_chart_all%>%
  ungroup()%>%
  mutate(cases=cancer_burden+Baseline)%>%
  group_by(Variable)%>%
  filter(cases==min(cases)|cases==max(cases))%>%
  mutate(range=ifelse(cases==min(cases),"min","max"))%>%
  select(Variable,cases,range)%>%
  distinct()%>%
  mutate(cases=round(cases,1))%>%
  pivot_wider(values_from =cases,names_from = range)%>%
  select(Variable,min,max)%>%
  write.csv("Tables\\well_ci_val.csv",row.names = F,quote = F)

well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('mean',"Middle Estimate",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('lci',"Lower Estimate",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('xuci',"Upper Estimate",x))
well_sensitivity_val_results[]<-lapply(well_sensitivity_val_results,function(x) gsub('uci',"Upper Estimate",x))
well_sensitivity_val_results<-well_sensitivity_val_results%>%
  mutate(cancer_burden=as.numeric(cancer_burden))

a<-well_sensitivity_val_results%>%
  mutate(confidence_ir=ifelse(confidence_ir=="9Lower Estimate","Upper Estimate",confidence_ir),
         confidence_bw=gsub(" Estimate","",confidence_bw))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_ir,fill=confidence_bw))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Ingestion Rate Estimate')+
  ggtitle('Ingestion Rate and Bodyweight')+
  guides(fill=guide_legend(title="Bodyweight Estimate"))+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

b<-well_sensitivity_val_results%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_as_conc),fill='blue3',color='black')+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Arsenic Concentration Estimate')+
  ggtitle('Interpolated Arsenic Estimate')+
  #guides(fill=guide_legend(title= "Arsenic Calculation Method"))+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

c<-well_sensitivity_val_results%>%
  mutate(confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Lower Estimate","0.2 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Upper Estimate","0.99 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_percent_treatment=gsub(" Estimate","",confidence_percent_treatment),
         confidence_percent_consuming=gsub(" Estimate","",confidence_percent_consuming))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(fill=confidence_percent_consuming,x=confidence_percent_treatment))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Percent using Treatment Estimate')+
  ggtitle('Treatment Use')+
  guides(fill=guide_legend(title= "Percent Consuming Estimate"))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  facet_wrap(~confidence_treatment_effectiveness)+
  theme(text = element_text(size=24),legend.position = "top")

d<-well_sensitivity_val_results%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_combined_cancer),fill='firebrick')+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('CSF Estimate')+
  ggtitle('Cancer Toxicity')+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")


png("Data\\Figures\\sens_anal_well_val.png",height = 15,width=22,res=300,units = "in")
ggarrange(a,b,c,d,ncol = 2,nrow=2)
dev.off()


pws_sensitivity_val_results<-read.xlsx("Data\\pws_sensitivity_val_results.xlsx")%>%
  drop_na()

pws_sensitivity_results_val_chart<-pws_sensitivity_val_results%>%
  ungroup()%>%
  filter(confidence_ir=="mean"&
           confidence_bw=="mean"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           confidence_percent_treatment=="lci"&
           confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")

pws_sensitivity_results_val_chart_all<-pws_sensitivity_val_results%>%
  ungroup()%>%
  filter(confidence_ir=="mean"&
           confidence_bw=="mean"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           #confidence_percent_treatment=="lci"&
           #confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")%>%
  select(cancer_burden)%>%
  mutate(Variable="Treatment Use and Effectiveness")%>%
  rbind(pws_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   #confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Percent Consuming"))%>%
  rbind(pws_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Arsenic Concentration"))%>%
  rbind(pws_sensitivity_val_results%>%
          ungroup()%>%
          filter(#confidence_ir=="mean"&
            confidence_bw=="mean"&
              confidence_as_conc=="mean"&
              confidence_percent_consuming=="uci"&
              confidence_percent_treatment=="lci"&
              confidence_treatment_effectiveness=="uci"&
              confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Ingestion Rate"))%>%
  rbind(pws_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
                   #confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Body Weights"))%>%
  rbind(pws_sensitivity_val_results%>%
          ungroup()%>%
          filter(confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci")%>%
          #confidence_combined_cancer=="uci")
          select(cancer_burden)%>%
          mutate(Variable="Cancer Slope Factor"))%>%
  mutate(Baseline=pws_sensitivity_results_val_chart$cancer_burden,
         cancer_burden=cancer_burden-Baseline)

png("Data\\Figures\\pws_val_sens_anal.png",height = 4,width=14,res=300,units = "in")
pws_sensitivity_results_val_chart_all%>%
  group_by(Variable)%>%
  filter(cancer_burden==min(cancer_burden)|cancer_burden==max(cancer_burden))%>%
  ungroup()%>%
  mutate(grp=ifelse(cancer_burden<0,"neg","pos"),
         Variable=as.factor(Variable))%>%
  mutate(Variable=fct_rev(fct_relevel(Variable,"Arsenic Concentration","Treatment Use and Effectiveness","Percent Consuming","Ingestion Rate","Body Weights",
                                      "Cancer Slope Factor")),
         loc=ifelse(grp=="neg",cancer_burden-60,cancer_burden+75),
         n=round(Baseline+cancer_burden))%>%
  ggplot(aes(fill=grp))+
  geom_bar(aes(x=cancer_burden,y=Variable),stat="identity")+
  geom_vline(aes(xintercept=Baseline-Baseline))+
  geom_text(aes(x=loc,y=Variable,label=n),size=6.5)+
  ylab('')+
  xlab('Statewide Cancer Cases Relative to Baseline')+
  scale_fill_manual(values = c("skyblue3","darkred"))+
  theme_classic2()+
  scale_x_continuous(sec.axis = sec_axis(~ pws_sensitivity_results_val_chart$cancer_burden + ., name = "Statewide Cancer Cases"),limits = c(-1000,1250))+
  theme_classic2()+
  theme(text = element_text(size=24),legend.position = "none",axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank(), axis.title.x.bottom = element_blank())
dev.off()  

pws_sensitivity_results_val_chart_all%>%
  mutate(cases=cancer_burden+Baseline)%>%
  group_by(Variable)%>%
  filter(cases==min(cases)|cases==max(cases))%>%
  mutate(range=ifelse(cases==min(cases),"min","max"))%>%
  select(Variable,cases,range)%>%
  distinct()%>%
  mutate(cases=round(cases,1))%>%
  pivot_wider(values_from =cases,names_from = range)%>%
  select(Variable,min,max)%>%
  write.csv("Tables\\cws_ci_val.csv",row.names = F,quote = F)


sensitivity_results<-read.xlsx("Data\\sensitivity_results.xlsx")%>%
  drop_na()


sensitivity_results_chart<-sensitivity_results%>%
  ungroup()%>%
  filter(confidence_wells=="uci"&
           confidence_ir=="mean"&
           confidence_bw=="mean"&
           method=="int"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           confidence_percent_treatment=="lci"&
           confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")

sensitivity_results_chart_all<-sensitivity_results%>%
  ungroup()%>%
  filter(confidence_wells=="uci"&
           confidence_ir=="mean"&
           confidence_bw=="mean"&
           method=="int"&
           confidence_as_conc=="mean"&
           confidence_percent_consuming=="uci"&
           #confidence_percent_treatment=="lci"&
           #confidence_treatment_effectiveness=="uci"&
           confidence_combined_cancer=="mean")%>%
  select(cancer_burden)%>%
  mutate(Variable="Treatment Use and Effectiveness")%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   #confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Percent Consuming"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   #confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Arsenic Concentration"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   #confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Ingestion Rate"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   #confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci"&
                   confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Body Weights"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(confidence_wells=="uci"&
                   confidence_ir=="mean"&
                   confidence_bw=="mean"&
                   method=="int"&
                   confidence_as_conc=="mean"&
                   confidence_percent_consuming=="uci"&
                   confidence_percent_treatment=="lci"&
                   confidence_treatment_effectiveness=="uci")%>%
          #confidence_combined_cancer=="mean")
          select(cancer_burden)%>%
          mutate(Variable="Cancer Slope Factor"))%>%
  rbind(sensitivity_results%>%
          ungroup()%>%
          filter(#confidence_wells=="uci"&
            confidence_ir=="mean"&
              confidence_bw=="mean"&
              method=="int"&
              confidence_as_conc=="mean"&
              confidence_percent_consuming=="uci"&
              confidence_percent_treatment=="lci"&
              confidence_treatment_effectiveness=="uci"&
              confidence_combined_cancer=="mean")%>%
          select(cancer_burden)%>%
          mutate(Variable="Well Estimate"))%>%
  mutate(Baseline=sensitivity_results_chart$cancer_burden,
         cancer_burden=cancer_burden-Baseline)

png("Data\\Figures\\sens_anal_only_int.png",height = 4.5,width=14,res=300,units = "in")
sensitivity_results_chart_all%>%
  group_by(Variable)%>%
  filter(cancer_burden==min(cancer_burden)|cancer_burden==max(cancer_burden))%>%
  ungroup()%>%
  mutate(grp=ifelse(cancer_burden<0,"neg","pos"),
         Variable=as.factor(Variable))%>%
  mutate(Variable=fct_rev(fct_relevel(Variable,"Well Estimate","Arsenic Concentration","Treatment Use and Effectiveness","Percent Consuming","Ingestion Rate","Body Weights",
                                      "Cancer Slope Factor")),
        loc=ifelse(grp=="neg",cancer_burden-20,cancer_burden+20),
         n=round(Baseline+cancer_burden))%>%
  ggplot(aes(fill=grp))+
  geom_bar(aes(x=cancer_burden,y=Variable),stat="identity")+
  geom_vline(aes(xintercept=Baseline-Baseline))+
  geom_text(aes(x=loc,y=Variable,label=n),size=6.5)+
  ylab('')+
  xlab('Statewide Cancer Cases Relative to Baseline')+
  scale_fill_manual(values = c("skyblue3",'darkred'))+
  scale_x_continuous(sec.axis = sec_axis(~ sensitivity_results_chart$cancer_burden + ., name = "Statewide Cancer Cases"),limits = c(-200,500))+
  theme_classic2()+
  theme(text = element_text(size=24),legend.position = "none",axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank(), axis.title.x.bottom = element_blank())
dev.off()  


sensitivity_results_chart_all%>%
  mutate(cases=cancer_burden+Baseline)%>%
  group_by(Variable)%>%
  filter(cases==min(cases)|cases==max(cases))%>%
  mutate(range=ifelse(cases==min(cases),"min","max"))%>%
  select(Variable,cases,range)%>%
  distinct()%>%
  mutate(cases=round(cases,1))%>%
  pivot_wider(values_from =cases,names_from = range)%>%
  select(Variable,min,max)%>%
  write.csv("Tables\\private_wells_int_ci.csv",row.names = F,quote = F)

pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('mean',"Middle Estimate",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('lci',"Lower Estimate",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('xuci',"Upper Estimate",x))
pws_sensitivity_val_results[]<-lapply(pws_sensitivity_val_results,function(x) gsub('uci',"Upper Estimate",x))
pws_sensitivity_val_results<-pws_sensitivity_val_results%>%
  mutate(cancer_burden=as.numeric(cancer_burden))

a<-pws_sensitivity_val_results%>%
  mutate(confidence_ir=ifelse(confidence_ir=="9Lower Estimate","Upper Estimate",confidence_ir),
         confidence_bw=gsub(" Estimate","",confidence_bw))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_ir,fill=confidence_bw))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Ingestion Rate Estimate')+
  ggtitle('Ingestion Rate and Bodyweight')+
  guides(fill=guide_legend(title="Bodyweight Estimate"))+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

b<-pws_sensitivity_val_results%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_as_conc),fill='blue3',color='black')+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Arsenic Concentration Estimate')+
  ggtitle('CWS Arsenic Estimate')+
  #guides(fill=guide_legend(title= "Arsenic Calculation Method"))+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")

c<-pws_sensitivity_val_results%>%
  mutate(confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Lower Estimate","0.2 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_treatment_effectiveness=ifelse(confidence_treatment_effectiveness=="Upper Estimate","0.99 Treatment Efficiency",confidence_treatment_effectiveness),
         confidence_percent_treatment=gsub(" Estimate","",confidence_percent_treatment),
         confidence_percent_consuming=gsub(" Estimate","",confidence_percent_consuming))%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(fill=confidence_percent_consuming,x=confidence_percent_treatment))+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('Percent using Treatment Estimate')+
  ggtitle('Treatment Use')+
  guides(fill=guide_legend(title= "Percent Consuming Estimate"))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  facet_wrap(~confidence_treatment_effectiveness)+
  theme(text = element_text(size=24),legend.position = "top")

d<-pws_sensitivity_val_results%>%
  ggplot(aes(y=cancer_burden))+
  geom_boxplot(aes(x=confidence_combined_cancer),fill='firebrick')+
  scale_y_log10()+
  ylab('Statewide Cancer Cases')+
  xlab('CSF Estimate')+
  ggtitle('Cancer Toxicity')+
  theme_bw()+
  theme(text = element_text(size=24),legend.position = "top")


png("Data\\Figures\\sens_anal_cws_val.png",height = 15,width=22,res=300,units = "in")
ggarrange(a,b,c,d,ncol = 2,nrow=2)
dev.off()


#COMPARISON Between Threshold and Values----
pws_arsenic_data%>%
  mutate(lci_result=ifelse(lci_result>0.001,population_served_count,0),
         mean_result=ifelse(mean_result>0.001,population_served_count,0),
         uci_result=ifelse(uci_result>0.001,population_served_count,0))%>%
  summarise(sum(lci_result),
            sum(mean_result),
            sum(uci_result))

pws_arsenic_data%>%
  mutate(lci_result=ifelse(lci_result>0.005,population_served_count,0),
         mean_result=ifelse(mean_result>0.005,population_served_count,0),
         uci_result=ifelse(uci_result>0.005,population_served_count,0))%>%
  summarise(sum(lci_result),
            sum(mean_result),
            sum(uci_result))

pws_arsenic_data%>%
  mutate(lci_result=ifelse(lci_result>0.01,population_served_count,0),
         mean_result=ifelse(mean_result>0.01,population_served_count,0),
         uci_result=ifelse(uci_result>0.01,population_served_count,0))%>%
  summarise(sum(lci_result),
            sum(mean_result),
            sum(uci_result))


png("Data\\Figures\\combined_sens_anal.png",height = 7,width=14,res=300,units = "in")
ggarrange(sens_anal,pws_sens_anal,labels = c("a) Private Wells","b) CWSs           "),ncol = 1,font.label = list(size=20),align = "hv",vjust = 17,hjust = .001)
dev.off()

pw1<-read.csv("Tables\\private_wells_ci.csv")
colnames(pw1)<-c("Variable","min_th","max_thr")
pw2<-read.csv("Tables\\well_ci_val.csv")
colnames(pw2)<-c("Variable","min_val","max_val")
pw1%>%
  merge(pw2)%>%
  write.csv("Tables\\private_wells_ci_all.csv",row.names = F,quote = F)


cws1<-read.csv("Tables\\cws_ci.csv")
colnames(cws1)<-c("Variable","min_th","max_thr")
cws2<-read.csv("Tables\\cws_ci_val.csv")
colnames(cws2)<-c("Variable","min_val","max_val")
cws1%>%
  merge(cws2)%>%
  write.csv("Tables\\cws_ci_all.csv",row.names = F,quote = F)
