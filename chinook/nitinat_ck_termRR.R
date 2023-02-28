
####### BEFORE STARTING IN R: ####### 

####### Extract files from databases -------------------
# 1. CREST biodata: 'WCVI Chinook Run Reconstruction Project Biological Data With FOS' Report > most recent 4 years, all months, Nitinat > Save to Excel as 'CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_Nitinat[years-years].xlsx'
# 2. CREST catch: Salmon$ > FMCR_Fishery_Monitoring_Catch_Reporting > Recreational_CM > Catch_Data > save 'SC Sport Catch Creel Sub-area Disposition (Master Do No Edit).xlsx' as 'SC Sport Catch Creel Sub-area Disposition.xlsx'  
# 3. FOS: ... 
# 4. Oto Manager: Recovery Specimens > most recent 4 years, chinook, PFMA 21 and 22, include age > Generate > Save to Excel as 'OTOMGR_Export_RecoverySpecimens_PFMA21-22_[years-years].xlsx'
####### Gather auxiliary files (these are individual Excel files not extracted from databases) -------------------
# 0. Spatial lookup table ('spatialLookup.xlsx' from github repo - should already clone in with the project)
# 1. Escapement file from Nitinat Hatchery (Rob Brouwer or Caroline Cherry)
# 2. SEP age/comp data from EPRO (will shift to above when StA gets access)
####### 

# RUN_RECON_AREA: sector, (gear for comm), area
# UFID: year, RUN_RECON_AREA, MONTH, (statweek for comm)




# **** in dev- noted spots where changes are needed after data are extracted
# tbd; could change 'data' folder structure to be specific to areas or keep as dumps... not all data are pushed so could be generic, then copy files to network folder?? 
# maybe this workflow is weird. could push files useful to all areas (big dumps) and copy area specific data files in as needed 







######################################################################################################################################################


# NITINAT Chinook terminal run reconstruction background script to accompany Rmd 
# June 2022


# load packages -------------------
library(tidyverse)
library(readxl)
library(here)
library(leaflet)


# set wd and settings -------------------
#setwd(here("data"))
options(scipen = 999999)


# Other inputs not from fish data -------------------
RR_year <- 2022
RR_ages <- data.frame(RESOLVED_AGE=c(2:6))
spatLookup <- read_excel(here("data", "gitData", "spatialLookup.xlsx"), sheet="nitinat")


# Define function to covert date to stat week for commercial data -------------------
statWeek <- function(date_variable){
  as_date_variable <- as.Date(date_variable)
  m <- lubridate::month(as_date_variable)
  wk <- function(x) as.numeric(format(x, "%U"))
  paste(m, wk(as_date_variable)-wk(as.Date(cut(as_date_variable, "month")))+1, sep="")
} 


######################################################################################################################################################

#                                                              0. LOAD AND CLEAN DATA 


# =================== DATABASE EXTRACTS ===================   [this may eventually be replaced with direct database connections]

# CREST: Rec catch -------------------
catRec <- read_excel(here("data", "SC Sport Catch Creel Sub-area Disposition.xlsx"), sheet="YTD") %>%
  mutate(sector="sport",
         input_data_type="catch",
         MONTH = factor(MONTH, levels=month.name),
         RUN_RECON_AREA = case_when(CREEL_SUB_AREA%in%c("21A","21B","Area 21") ~ "sport Area 21 Terminal",
                                    CREEL_SUB_AREA%in%c("121C") ~ "sport Outer Nitinat corridor",
                                    CREEL_SUB_AREA%in%c("22A") ~ "sport Nitinat Lake and River"),
         # Below is for groupings for calculating release mortality/catch sample ratios styled after lookup table (from Yi discussion) ---
         ## pfmaMonth = paste(PFMA, MONTH, sep=", "),
         ## FishCatReg = case_when(PFMA=="PFMA 10" ~ "outSmith(10)",
         ##                        PFMA %in% c("PFMA 11", "PFMA 111") ~ "upperQCS(11,111)",
         ##                         PFMA=="PFMA 12" ~ "nJST(12)",
         ##                         PFMA %in% c("PFMA 13", "PFMA 14", "PFMA 15", "PFMA 16") ~ "nGST(13,14,15,16)",
         ##                         PFMA %in% c("PFMA 17", "PFMA 18", "PFMA 28", "PFMA 29") | CREEL_SUB_AREA %in% c("19A", "19B", "Area 19 (GS)") ~ "sGST(17,18,19AB,28,29)",
         ##                         CREEL_SUB_AREA %in% c("20DB", "20DI", "20DO", "19C", "19D", "19E", "20C", "20D", "Area 19 (JDF)", "Area 20 (East)") ~ "JDFeast(19CDE,20CD)",
         ##                         CREEL_SUB_AREA %in% c("20A","20B","20E", "Area 20 (West)", "Area 20 (WCVI)") ~ "JDFwest(Renfrew 20ABE)",
         ##                         PFMA=="PFMA 21" ~ "Nitinat",
         ##                         PFMA=="PFMA 23" & CREEL_SUB_AREA %in% c("23A","23B", "Area 23 (Alberni Canal)")  ~ "Alberni(23A,B)",
         ##                         PFMA=="PFMA 23" & CREEL_SUB_AREA %in% c("23C","23D","23E","23F","23G","23H","23I","23J","23K","23L","23M","23N","23O","23P","23Q","Area 23","Area 23 (Barkley)") ~ "Barkley(23C-Q)",
         ##                       PFMA=="PFMA 24"~ "Clayoquot",
         ##                     PFMA=="PFMA 25" ~ "Nootka/Esp",
         ##                     PFMA=="PFMA 26" ~ "Kyuquot",
         ##                     PFMA=="PFMA 27" ~ "Quatsino",
         ##                     PFMA %in% c("PFMA 121", "PFMA 122", "PFMA 123", "PFMA 124") ~ "SWVIoutside-1nm121-124",
         ##                     PFMA %in% c("PFMA 125", "PFMA 126", "PFMA 127") ~ "NWVI-1nm-corridor125-127",
         ##                     grepl("US7", PFMA) ~ "US7"),
         ## regionMonth = paste(FishCatReg, MONTH, sep=", "),
         UFID = paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"),
         PFMA = as.numeric(substr(PFMA,6,8))) %>% 
  filter(SPECIES=="CHINOOK SALMON") %>%        
  select(-c(PUBLISHED_DATE,STATUS,MANAGEMENT,SPECIES,SPECIES_CODE,SCIENTIFIC_NAME)) %>%
  print()

# CREST: Biodata ------------------- 
bioCREST <- read_excel(here("data", "nitinat_data", "CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_RenfrewSwift2018-2022.xlsx"), 
                       sheet="WCVI_Chinook_Run_Rec",  guess_max = 10000) %>% 
  select(-c(RECEIVED_SCALES:FISH_NO, REFERENCE_NO, SAMPLER_NAME:SAMPLER_ID,GUIDED,SPECIES, OTOLITH_BOX:OTOLITH_SPECIMEN,THERMALMARK,OTO_STOCK,SCALE_BOOK,SCALE_NO,SCALE_FORMAT:PART_AGE_CODE,CWT_HEAD_LABEL,DNA_CONTAINER_TYPE,SPECIMEN_REFERENCE_DNA_NO,DNA_STOCK_3:COMMENTS)) %>%
  mutate(input_data_type = case_when(!is.na(RESOLVED_AGE) & is.na(RESOLVED_STOCK_ORIGIN) ~ "age",
                                     is.na(RESOLVED_AGE) & !is.na(RESOLVED_STOCK_ORIGIN) ~ "stock comp",
                                     !is.na(RESOLVED_AGE) & !is.na(RESOLVED_STOCK_ORIGIN) ~ "age and stock comp"),
         gear = case_when(grepl("TROLL", SAMPLE_TYPE) ~ "Troll",
                          grepl("SEINE", SAMPLE_TYPE) ~ "Seine",
                          grepl("GILL NET", SAMPLE_TYPE) ~ "Gillnet"),
         sector = case_when(SAMPLE_TYPE=="Sport" ~ "sport",
                            grepl("GILL NET|TROLL", SAMPLE_TYPE) ~ "commercial",
                            SAMPLE_TYPE=="Escapement" ~ "escapement"),
         CREEL_SUB_AREA = ifelse(sector=="sport" & SUBAREA%in%c("25PI","25JO","125Q","25JI","25L","25N","25PO","25F","125A","25E","25D","125G","125K",
                                                                "25O","25I","21A","21B","22A","121C"),SUBAREA, NA),
         statweek = ifelse(sector=="commercial", statWeek(COLLECTION_DATE), NA),
         RUN_RECON_AREA2 = case_when(sector=="sport" & SUBAREA%in%c("21A","21B","Area 21") ~ paste(sector,"Area 21 Terminal", sep=" "),
                                     sector=="sport" & SUBAREA=="22A" ~ paste(sector,"Nitinat Lake and River", sep=" "),
                                     sector=="sport" & SUBAREA=="121C" ~ paste(sector,"Outer Nitinat corridor", sep=" "))) %>%
  rename(PFMA=AREA,
         RUN_RECON_AREA_old=RUN_RECON_AREA,
         RUN_RECON_AREA=RUN_RECON_AREA2,
         DNA_STOCK_1 = DNA_RESULTS_STOCK_1) %>%
  select(-c(RUN_RECON_AREA_old)) %>%
  mutate(UFID = case_when(sector=="sport" ~ paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"),
                          sector=="commercial" ~ paste(YEAR,RUN_RECON_AREA,MONTH,statweek,sep="-"),
                          sector=="escapement" ~ paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"))) %>%
  print()


# FOS: Commercial catch -------------------   [not run for Nit]
#catComm <- read_excel("FOS Dump for 2021 Fisheries (Feb 10, 2022).xlsx", sheet="fos_VANWILLP", n_max=Inf, guess_max=20000, skip=2) %>%
#  mutate(input_data_type="catch",
#         sector="commercial",
#         YEAR = lubridate::year(FISHING_DATE),
#         MONTH = as.character(lubridate::month(FISHING_DATE, label=T, abbr=F)),
#         statweek = as.numeric(statWeek(FISHING_DATE)),
#         gear = case_when(grepl("Gillnet", LICENCE_AREA) ~ "Gillnet",
#                          grepl("Troll", LICENCE_AREA) ~ "Troll",
#                          grepl("Seine", LICENCE_AREA) ~ "Seine"),
#         RUN_RECON_AREA = paste(sector, gear, AREA_NAME),
#         UFID=paste(YEAR,RUN_RECON_AREA,MONTH,statweek,sep="-")) %>%
#  select(-c(STAT_WEEK,ESTIMATE_TYPE,TARGETS_SOCKEYE:TARGETS_CHUM,HRS_OPEN:CHUM_RELD,COMMENTS)) %>%
#  print()


# OTOMANAGER: Thermal mark -------------------     [not run]
#bioTM <- read_excel("2020 Thermal Mark Samples Status_26Feb2021.xlsx", sheet="2019_Specimen_Hatch_Age",skip=2, guess_max=20000) %>%
#  filter(SPECIES=="Chinook") %>%
#  mutate(input_data_type="stock composition") %>% 
#  print()




# =================== AUX FILES ===================

# HATCHERY ESCAPEMENT AND REMOVALS -------------------         
nitReport <- read_excel(here("data", "nitinat_data", "2021 Nitinat River CN Escapement_Draft_rob nov_15_21.xlsx"), sheet="forR") %>% 
  filter(!details %in% c("Number of fish given to others", "Culls", "Morts", "0", "Total", "Additional estimate NOT counted or sampled for marks:",
                         "Total Hatchery estimate of Native Fishery:", "Total Hatchery staff estimate of natural spawners:"),
         !grepl("TOTAL", details), !grepl("(If Applicable)", details), !grepl("River was open", details), !grepl("* Not including broodstock",details)) %>%
  pivot_longer(U:I, names_to="sex", values_to="n") %>% 
  mutate(sector = case_when(estimate_type=="TOTAL NATURAL SPAWNING RIVER POPULATION" ~ "escapement",
                            grepl("FRESH WATER SPORT FISHERY", estimate_type) ~ "sport", 
                            grepl("NATIVE FISHERY", estimate_type) ~ "FN",
                            grepl("FISH REMOVED", estimate_type) ~ "escapement"),
         RUN_RECON_AREA = case_when(sector=="escapement" & estimate_type=="TOTAL NATURAL SPAWNING RIVER POPULATION" & 
                                      grepl("River",location) ~ paste(sector,"Nitinat River Spawners",sep=" ") ,
                                    sector=="escapement" & estimate_type=="FISH REMOVED FROM THE SYSTEM:" & 
                                      grepl("surplus",details) ~ paste(sector,"ESSR", sep=" "), 
                                    sector=="escapement" & grepl("for broodstock|given to natives|other purposes",details) ~ 
                                      paste(sector,"Hatchery broodstock, morts",sep=" "), 
                                    sector=="sport" & grepl("River",location) ~ paste(sector,"Area 22 Nitinat River",sep=" "),
                                    sector=="FN" & grepl("ESSR",details) ~ paste(sector,"EO (Native ESSR)",sep=" "),
                                    sector=="FN" & grepl("Food Fish", details) ~ paste(sector,"FSC")),
         MONTH = case_when(sector=="sport" ~ "September",
                           sector=="escapement" & estimate_type=="FISH REMOVED FROM THE SYSTEM:" ~ "September",
                           sector=="escapement" & estimate_type=="TOTAL NATURAL SPAWNING RIVER POPULATION" ~ "October",
                           sector=="FN" ~ "September"),
         PFMA = 22,
         stock = "Nitinat",
         input_data_type = case_when(sector%in%c("sport", "FN") ~ "catch",
                                     sector=="escapement" ~ "escapement"),
         CREEL_SUB_AREA = ifelse(sector=="sport", "22A", NA),
         UFID = case_when(sector=="sport" & !is.na(RUN_RECON_AREA) ~ paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"),
                          #sector=="commercial" & !is.na(RUN_RECON_AREA) ~ paste(YEAR,RUN_RECON_AREA,statweek,sep="-"),
                          sector=="FN" & !is.na(RUN_RECON_AREA) ~ paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"),
                          sector=="escapement" & !is.na(RUN_RECON_AREA) ~ paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"))) %>%
  mutate_at("n", as.numeric) %>%
  print()


# data check: is there any unassigned catch 
# View(nitReport %>% filter(is.na(RUN_RECON_AREA), n>0))








# SEP: Age and sex data -------------------   [not run -- replace with direct NuSEDS query]
#bioSEP <- read_excel("All_PADS_2021_CN_Ages_to_22Feb2022.xlsx", sheet="2021 Data", skip=1, guess_max=20000) %>%
#  filter(`Fiscal Year`==2021, Project=="CONUMA RIVER HATCHERY -2021", `Geographic Location`=="WCVI", 
#         `GR Age`%in%c("21","31","41","51","1M","2M","3M","4M")) %>%
#  mutate(`GR Age2` = case_when(`GR Age`%in%c("21","1M")~"2",
#                               `GR Age`%in%c("31","2M")~"3",
#                               `GR Age`%in%c("41","3M")~"4",
#                               `GR Age`%in%c("51","4M")~"5",
#                               `GR Age`%in%c("61","5M")~"6")) %>%
#  mutate_at("GR Age2", as.numeric) %>%
#  group_by(`Fiscal Year`,Location, `GR Age`, `GR Age2`) %>%
#  summarize(age_n=n(), Project=unique(Project)) %>%
#  group_by(`Fiscal Year`,Location) %>%
#  mutate(age_total=sum(age_n), age_propn = age_n/sum(age_n)) %>%
#  group_by(`Fiscal Year`,Location, `GR Age2`) %>%
#  summarize(age_n = sum(age_n), age_total=unique(age_total), age_propn = sum(age_propn), Project=unique(Project)) %>%
#  rename(Esc_location = Location) %>%
#  arrange(Esc_location, `GR Age2`) %>%
#  mutate(age_data_source = paste("SEP Conuma Hatchery broodstock age data", Esc_location, sep=", "),
#         data_class="age") %>%
#  print()





##########################################################################################################################################################



#                                    COMPILING CATCH FOR REVIEW, SPATIAL LINKING, CALCULATING TOTAL MORTALITY 


# Rec catch release mortality 
catEscNitFull <- full_join(
  # JOIN marine sport catch data ----
  left_join(
    # within sport data, combine catch data to the spatial lookup file to georeference --
    catRec %>%
      filter(PFMA%in%c(21,22), YEAR%in%seq(RR_year,RR_year-4)) %>% 
      #group_by(YEAR, MONTH, PFMA, CREEL_SUB_AREA, RUN_RECON_AREA, UFID, DISPOSITION) %>% 
      # summing est, no variance for now but could add in later: ---- 
    #summarize(sumEST = sum(ESTIMATE)) %>% 
    pivot_wider(names_from=DISPOSITION, values_from=ESTIMATE) %>% 
      mutate(releaseMort=`Released Legal`*0.2,
             TOTAL = case_when(is.na(releaseMort) ~ Kept,
                                   !is.na(Kept) & !is.na(releaseMort) ~ round(Kept+releaseMort,0),
                                   is.na(Kept) & !is.na(releaseMort) ~ round(releaseMort,0))),
    spatLookup %>% 
      filter(sector=="sport") %>%
      select(-c(notes))
  ),
  # TO freshwater sport catch data ----    
  left_join(
      # within commercial data, combine catch data to the spatial lookup file to georeference --
    nitReport %>% 
      filter(RUN_RECON_AREA=="sport Area 22 Nitinat River" & sex%in%c("M","F","U")) %>%
      group_by(PFMA, YEAR, sector, RUN_RECON_AREA, UFID, MONTH, CREEL_SUB_AREA, input_data_type) %>% 
      summarize(Kept = sum(n))%>%
      mutate(TOTAL=Kept),
    spatLookup %>%
      filter(sector=="sport") %>%
      select(-c(notes))
  )
) %>%
  # JOIN marine+fw sport catch TO First Nations catch ----     
  full_join(.,
     # Within FN catch data, join catch to the spatial lookup file to georeference --
    left_join(
      nitReport %>% 
        filter(sector=="FN" & sex%in%c("M","F","U") & !is.na(RUN_RECON_AREA)) %>%
        group_by(PFMA, YEAR, sector, MONTH, RUN_RECON_AREA, UFID, input_data_type) %>% 
        summarize(Kept = sum(n)) %>%
        mutate(TOTAL=Kept),
      spatLookup %>%
        filter(sector=="FN") %>%
        select(-c(CREEL_SUB_AREA, notes))
      )
) %>%
  full_join(.,
            left_join(
              nitReport %>% 
                filter(sector=="escapement" & !is.na(RUN_RECON_AREA) & sex%in%c("M","F","U","J")) %>%
                group_by(PFMA, YEAR, sector, RUN_RECON_AREA, MONTH, UFID, input_data_type, sex) %>%
                summarize(TOTAL=sum(n)),
              spatLookup %>% 
                filter(sector=="escapement") %>%
                select(-c(CREEL_SUB_AREA,notes))
            )
) %>%
  mutate(label = paste(RUN_RECON_AREA,input_data_type,sep=" ")) %>%
  print()



# Total mortality - do we want to include this is a terminal RR? It assumes the released fish are all local stocks but possible they aren't
### could look at stock comp to see % local vs. non-local maybe ? 



##########################################################################################################################################################


#                                          COMPILING BIODATA FOR REVIEW, SPATIAL LINKING





### **** NEXT DAY :  -******* 
# Add more bio data -- escapement age and TM data 
# also look at how to dynamicaly name rmd files for archive exports 








bioNitFull <- 
  # JOIN sport data ---- 
#full_join(
  # within sport data, combine bio data to the spatial lookup file to georeference --  
  left_join(
    bioCREST %>%
      filter(sector=="sport", PFMA%in%c(21,22)),
    spatLookup %>%
      filter(sector=="sport") %>%
      select(-c(notes, RUN_RECON_AREA))
  ) %>% #,
  # TO commercial data ----
  # within commercial data, combine bio data to the spatial lookup file to georeference --
  #left_join(
  #  bioCREST %>%
  #    filter(sector=="commercial", PFMA=="25"),
  #  spatLookup %>% 
  #    filter(sector=="commercial") %>% 
  #    select(-c(CREEL_SUB_AREA,notes))
  #)
#) %>% 
  # JOIN rec+comm data TO escapement data ---
#  full_join(
#    .,
    # within escapement data, combine bio data to the spatial lookup file to georeference --
#    left_join(
#      bioCREST %>%
#        filter(grepl("escapement",sector)),
#      spatLookup %>% 
#        filter(grepl("escapement",sector)) %>%
#        select(-c(CREEL_SUB_AREA,notes))
#    )
#  ) %>%
#  # JOIN rec+comm+esc
#  full_join(
#    .,
#    left_join(
#      bioEO_GSI,
#      spatLookup %>%
#        filter(sector=="5 Nations EO")%>%
#        select(-c(notes,CREEL_SUB_AREA))
#    )
#  ) %>%
  mutate(label = paste(RUN_RECON_AREA,input_data_type,sep=" ")) %>%
  print()



# UFIDS: to link to UBIDs for lookup table 




##########################################################################################################################################################


# STOP HERE AND CONSULT RMD OUTPUT!

# > where are your fisheries and escapement occurring? 
# > do they have complementary biodata?
# > if not, are there nearby sectors with same-year data that can be borrowed from that are both temporally and spatially appropriate?
# > if not, are there data from a previous year that can be used? or a long-term average?


##########################################################################################################################################################


#                                          DEFINE LOOKUP TABLE FOR LINKING ABUNDANCE AND BIODATA 



# Match fisheries with existing run reconstruction year biodata ---------------
# Fisheries to match with data: 
#conUFIDSampSum21 <- left_join(
#  catConFull %>% 
#    filter(!is.na(RUN_RECON_AREA) & YEAR==RR_year) %>% 
#    group_by(sector,RUN_RECON_AREA, UFID) %>% 
#    summarize(totalKept = sum(Kept,na.rm=T))%>%
#    filter(totalKept>0),
#  
#  bioConFull %>% 
#    filter(!is.na(RUN_RECON_AREA) & !is.na(RESOLVED_AGE)) %>%    #joining based on UFID which has year, so don't need to filter biosamples by year
#    group_by(sector,RUN_RECON_AREA, UFID) %>% 
#    summarize(n_age_samples=n())
#) %>%
#  left_join(.,
#            bioConFull %>% 
#              filter(!is.na(RUN_RECON_AREA) & !is.na(RESOLVED_STOCK_ORIGIN)) %>% 
#              group_by(RUN_RECON_AREA, UFID) %>% 
#              summarize(n_stockcomp_samples=n())
#  ) %>%
#  mutate(AGE_SAMPLESIZE_FLAG = ifelse(is.na(n_age_samples)|n_age_samples<10, "FLAG", NA),
#         STOCKCOMP_SAMPLESIZE_FLAG = ifelse(is.na(n_stockcomp_samples)|n_stockcomp_samples<10, "FLAG", NA)) %>%
#  print()



# Filter out fisheries without biodata to manually create lookup table ---------------
# Fisheries to match with data: 
#conOrphanUFIDs <- conUFIDSampSum21 %>% 
#  filter(AGE_SAMPLESIZE_FLAG=="FLAG" | STOCKCOMP_SAMPLESIZE_FLAG=="FLAG") %>% 
#  print()

#View(bioConFull %>% 
#       filter(RUN_RECON_AREA %in% conOrphanUFIDs$RUN_RECON_AREA))





























