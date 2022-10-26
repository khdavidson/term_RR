
####### BEFORE STARTING IN R: ####### 

####### Extract files from databases -------------------
# 1. CREST biodata: 'WCVI Chinook Run Reconstruction Project Biological Data With FOS' Report > most recent 4 years, all months, Nitinat > Save to Excel as 'CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_Nitinat[years-years].xlsx'
# 2. CREST catch: Salmon$ > FMCR_Fishery_Monitoring_Catch_Reporting > Recreational_CM > Catch_Data > save 'SC Sport Catch Creel Sub-area Disposition (Master Do No Edit).xlsx' as 'SC Sport Catch Creel Sub-area Disposition.xlsx'  
# 3. FOS: ... 
# 4. Oto Manager: Recovery Specimens > most recent 4 years, chinook, PFMA 21 and 22, include age > Generate > Save to Excel as 'OTOMGR_Export_RecoverySpecimens_PFMA21-22[years-years].xlsx'
####### Gather auxiliary files (these are individual Excel files not extracted from databases) -------------------
# 0. Spatial lookup table ('spatialLookup.xlsx' from github repo - should already clone in with the project)
# 1. EO data from RM (may now be in FOS, tbd)
# 2. SEP age/comp data from EPRO (will shift to above when StA gets access)
# 3. Misc GSI files (e.g., EO GSI files not in CREST or FOS)
# 4. Escapement enumeration data 
####### 

# RUN_RECON_AREA: sector, (gear for comm), area
# UFID: year, RUN_RECON_AREA, MONTH, (statweek for comm)




# **** in dev- noted spots where changes are needed after data are extracted










######################################################################################################################################################


# NITINAT Chinook terminal run reconstruction background script to accompany Rmd 
# June 2022


# load packages -------------------
library(tidyverse)
library(readxl)
library(here)
library(leaflet)


# set wd and settings -------------------
setwd(here("data"))
options(scipen = 999999)


# Other inputs not from fish data -------------------
RR_year <- 2022
RR_ages <- data.frame(RESOLVED_AGE=c(2:6))
spatLookup <- read_excel("spatialLookup.xlsx",sheet="nitinat")


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
catRec <- read_excel("SC Sport Catch Creel Sub-area Disposition.xlsx", sheet="YTD") %>%
  mutate(sector="sport",
         input_data_type="catch",
         MONTH = factor(MONTH, levels=month.name),
         RUN_RECON_AREA = case_when(CREEL_SUB_AREA%in%c("21A","21B") ~ "Area 21 terminal"),
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
         UFID = paste(YEAR,RUN_RECON_AREA,MONTH,sep="-")) %>% 
  filter(SPECIES=="CHINOOK SALMON") %>%        #PFMA=="PFMA 25", YEAR==RR_year, MONTH%in%c("July", "August", "September"), !is.na(CREEL_SUB_AREA2)
  select(-c(PUBLISHED_DATE,STATUS,MANAGEMENT,SPECIES,SPECIES_CODE,SCIENTIFIC_NAME)) %>%
  print()

# CREST: Biodata ------------------- 
bioCREST <- read_excel("CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_Nitinat2018-2021.xlsx", sheet="WCVI_Chinook_Run_Rec") %>% 
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
                                                                "25O","25I"),SUBAREA, NA),
         statweek = ifelse(sector=="commercial", statWeek(COLLECTION_DATE), NA),
         RUN_RECON_AREA = ifelse(RUN_RECON_AREA=="Tlupana Inlet","Tlupana",RUN_RECON_AREA),
         RUN_RECON_AREA2 = case_when(sector=="sport" & SUBAREA=="25D"~paste(sector,"Tlupana", sep=" "),
                                     sector=="sport" & SUBAREA=="25L"~paste(sector,"Muchalat", sep=" "),
                                     sector=="sport" & SUBAREA%in%c("25E","25F","25JI")~paste(sector,"Inner Esperanza", sep=" "),
                                     sector=="sport" & SUBAREA%in%c("25I","25PO")~paste(sector,"Outer Nootka/corridor", sep=" "),
                                     sector=="sport" & SUBAREA%in%c("25JO","25K")~paste(sector,"Outer Esperanza/corridor", sep=" "),
                                     sector=="sport" & SUBAREA%in%c("25M","25N","25O","25PI")~paste(sector,"Inner Nootka"),
                                     sector=="commercial"~paste(sector,gear,RUN_RECON_AREA,sep=" "),
                                     sector=="escapement" & LANDING_SITE!="Conuma R" ~ paste(sector,"Misc",LANDING_SITE,sep=" "),
                                     sector=="escapement" & LANDING_SITE=="Conuma R" ~ paste(sector,LANDING_SITE,sep=" "))) %>%
  rename(PFMA=AREA,
         RUN_RECON_AREA_old=RUN_RECON_AREA,
         RUN_RECON_AREA=RUN_RECON_AREA2,
         DNA_STOCK_1 = DNA_RESULTS_STOCK_1) %>%
  filter(!grepl("Area 26",SUBAREA)) %>%
  select(-c(RUN_RECON_AREA_old)) %>%
  mutate(UFID = case_when(sector=="sport" ~ paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"),
                          sector=="commercial" ~ paste(YEAR,RUN_RECON_AREA,MONTH,statweek,sep="-"),
                          sector=="escapement" ~ paste(YEAR,RUN_RECON_AREA,MONTH,sep="-"))) %>%
  print()


# FOS: Commercial catch -------------------
catComm <- read_excel("FOS Dump for 2021 Fisheries (Feb 10, 2022).xlsx", sheet="fos_VANWILLP", n_max=Inf, guess_max=20000, skip=2) %>%
  mutate(input_data_type="catch",
         sector="commercial",
         YEAR = lubridate::year(FISHING_DATE),
         MONTH = as.character(lubridate::month(FISHING_DATE, label=T, abbr=F)),
         statweek = as.numeric(statWeek(FISHING_DATE)),
         gear = case_when(grepl("Gillnet", LICENCE_AREA) ~ "Gillnet",
                          grepl("Troll", LICENCE_AREA) ~ "Troll",
                          grepl("Seine", LICENCE_AREA) ~ "Seine"),
         RUN_RECON_AREA = paste(sector, gear, AREA_NAME),
         UFID=paste(YEAR,RUN_RECON_AREA,MONTH,statweek,sep="-")) %>%
  select(-c(STAT_WEEK,ESTIMATE_TYPE,TARGETS_SOCKEYE:TARGETS_CHUM,HRS_OPEN:CHUM_RELD,COMMENTS)) %>%
  print()


# OTOMANAGER: Thermal mark -------------------     [not run]
#bioTM <- read_excel("2020 Thermal Mark Samples Status_26Feb2021.xlsx", sheet="2019_Specimen_Hatch_Age",skip=2, guess_max=20000) %>%
#  filter(SPECIES=="Chinook") %>%
#  mutate(input_data_type="stock composition") %>% 
#  print()




# =================== AUX FILES ===================

# EO CATCH -------------------          [FOR NITINAT MAKE THIS ESSR??]
# [**REMOVED FOR NITINAT**]


# GSI -------------------              
# [**REMOVED FOR NITINAT**]


# SEP: Age and sex data -------------------   [not run]
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


# [Conuma Hatchery & River Escapement] -------------------  [not run]
# escCon <- [tbd]
# escMisc <-  [tbd]



##########################################################################################################################################################



#                                    COMPILING CATCH FOR REVIEW, SPATIAL LINKING, CALCULATING TOTAL MORTALITY 


# Rec catch release mortality 
catConFull <- full_join(
  # JOIN catch data ----
  left_join(
    # within sport data, combine catch data to the spatial lookup file to georeference --
    catRec %>%
      filter(PFMA=="PFMA 25", YEAR%in%seq(RR_year,RR_year-3)) %>% 
      #group_by(YEAR, MONTH, PFMA, CREEL_SUB_AREA, RUN_RECON_AREA, UFID, DISPOSITION) %>% 
      # summing est, no variance for now but could add in later: ---- 
    #summarize(sumEST = sum(ESTIMATE)) %>% 
    pivot_wider(names_from=DISPOSITION, values_from=ESTIMATE) %>% 
      mutate(releaseMort=`Released Legal`*0.2,
             TotalMort = case_when(is.na(releaseMort) ~ Kept,
                                   !is.na(Kept) & !is.na(releaseMort) ~ round(Kept+releaseMort,0),
                                   is.na(Kept) & !is.na(releaseMort) ~ round(releaseMort,0)),
             PFMA = as.numeric(substr(PFMA,6,9))),
    spatLookup %>% 
      filter(sector=="sport") %>%
      select(-c(notes))
  ),
  # TO commercial data ----
  left_join(
    # within commercial data, combine catch data to the spatial lookup file to georeference --
    catComm %>% 
      filter(MGMT_AREA==25) %>%
      #group_by(YEAR, MONTH, statweek, MGMT_AREA, AREA_NAME, RUN_RECON_AREA, UFID, gear) %>% 
      #summarize(Kept=sum(CHINOOK_KEPT), Released=sum(CHINOOK_RELD)) %>%
      mutate(releaseMort = case_when(gear=="Gillnet" ~ CHINOOK_RELD*0.5,
                                     gear=="Seine" ~ CHINOOK_RELD*0.2,
                                     gear=="Troll" ~ CHINOOK_RELD*0.2),
             TotalMort = round(CHINOOK_KEPT+releaseMort,0)) %>%
      rename(PFMA = MGMT_AREA,
             `Released Unk` = CHINOOK_RELD,
             Kept=CHINOOK_KEPT),
    spatLookup %>%
      filter(sector=="commercial") %>%
      select(-c(notes,CREEL_SUB_AREA))
  )
) %>%
  # JOIN commercial&rec catch TO 5 nations catch ----
full_join(.,
          # Within 5 nations data, join catch to the spatial lookup file to georeference --
          left_join(
            catEO %>%
              filter(MONTH%in%c("June","July","August","September"))%>%
              mutate(TotalMort=Kept),
            #group_by(YEAR, MONTH, RUN_RECON_AREA, UFID) %>% 
            #summarize(Kept=sum(Kept)),
            spatLookup %>%
              filter(sector=="5 Nations EO") %>%
              select(-c(CREEL_SUB_AREA, notes))
          )
) %>%
  mutate(label = paste(RUN_RECON_AREA,input_data_type,sep=" ")) %>%
  print()



# Total mortality - do we want to include this is a terminal RR? It assumes the released fish are all local stocks but possible they arent
### could look at stock comp to see % local vs. non-local maybe ? 


##########################################################################################################################################################


#                                          COMPILING BIODATA FOR REVIEW, SPATIAL LINKING



bioCREST %>% 
  group_by(YEAR, SAMPLE_TYPE) %>%
  summarize(n())


# rename to bioConFull once comm/ eo/ fsc are in

bioConFull <- 
  # JOIN sport data ---- 
full_join(
  # within sport data, combine bio data to the spatial lookup file to georeference --  
  left_join(
    bioCREST %>%
      filter(sector=="sport", PFMA=="25"),
    spatLookup %>%
      filter(sector=="sport") %>%
      select(-c(notes, RUN_RECON_AREA))
  ),
  # TO commercial data ----
  # within commercial data, combine bio data to the spatial lookup file to georeference --
  left_join(
    bioCREST %>%
      filter(sector=="commercial", PFMA=="25"),
    spatLookup %>% 
      filter(sector=="commercial") %>% 
      select(-c(CREEL_SUB_AREA,notes))
  )
) %>% 
  # JOIN rec+comm data TO escapement data ---
  full_join(
    .,
    # within escapement data, combine bio data to the spatial lookup file to georeference --
    left_join(
      bioCREST %>%
        filter(grepl("escapement",sector)),
      spatLookup %>% 
        filter(grepl("escapement",sector)) %>%
        select(-c(CREEL_SUB_AREA,notes))
    )
  ) %>%
  # JOIN rec+comm+esc
  full_join(
    .,
    left_join(
      bioEO_GSI,
      spatLookup %>%
        filter(sector=="5 Nations EO")%>%
        select(-c(notes,CREEL_SUB_AREA))
    )
  ) %>%
  mutate(label = paste(RUN_RECON_AREA,input_data_type,sep=" ")) %>%
  print()



### NEXT: add in FSC catch and biosamples (GSI?) to georeference --- how ? ?



# UFIDS: to link to UBIDs for lookup table 
# create visual matrix - RR_year catch UFIDs along top, RR_year(s) bio samples along left 




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
conUFIDSampSum21 <- left_join(
  catConFull %>% 
    filter(!is.na(RUN_RECON_AREA) & YEAR==RR_year) %>% 
    group_by(sector,RUN_RECON_AREA, UFID) %>% 
    summarize(totalKept = sum(Kept,na.rm=T))%>%
    filter(totalKept>0),
  
  bioConFull %>% 
    filter(!is.na(RUN_RECON_AREA) & !is.na(RESOLVED_AGE)) %>%    #joining based on UFID which has year, so don't need to filter biosamples by year
    group_by(sector,RUN_RECON_AREA, UFID) %>% 
    summarize(n_age_samples=n())
) %>%
  left_join(.,
            bioConFull %>% 
              filter(!is.na(RUN_RECON_AREA) & !is.na(RESOLVED_STOCK_ORIGIN)) %>% 
              group_by(RUN_RECON_AREA, UFID) %>% 
              summarize(n_stockcomp_samples=n())
  ) %>%
  mutate(AGE_SAMPLESIZE_FLAG = ifelse(is.na(n_age_samples)|n_age_samples<10, "FLAG", NA),
         STOCKCOMP_SAMPLESIZE_FLAG = ifelse(is.na(n_stockcomp_samples)|n_stockcomp_samples<10, "FLAG", NA)) %>%
  print()



# Filter out fisheries without biodata to manually create lookup table ---------------
# Fisheries to match with data: 
conOrphanUFIDs <- conUFIDSampSum21 %>% 
  filter(AGE_SAMPLESIZE_FLAG=="FLAG" | STOCKCOMP_SAMPLESIZE_FLAG=="FLAG") %>% 
  print()

View(bioConFull %>% 
       filter(RUN_RECON_AREA %in% conOrphanUFIDs$RUN_RECON_AREA))





























