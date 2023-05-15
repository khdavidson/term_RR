# EPRO Biosample linking
# To support TRR
# EPRO-PADS link has been lost because this year PADS has moved. Otolith stock ID is not transferring well to EPRO, so we need to do this manually
# Also need to link PADS and EPRO because some stocks don't look like they are loading correctly
# March 2023 


# ======================== SET UP ========================

# Load packages ---------------------------
library(tidyverse)
library(readxl)
library(writexl)       # for write_xlsx()
library(httr)          # for authenticate()
library(askpass)       # for askpass()
library(stringr)       # for str_sub()


# ======================== FUNCTIONS ========================

# Misc helper functions ---------------------------
"%notin%" <- Negate("%in%")
analysis_year <- 2022


# PADS data extraction functions --------------------------- (ty Nick Komick & Michael O'Brien!)
# --- Compile credentials into http authentication function to access online DFO databases ---
auth <- authenticate(user = Sys.info()["login"], 
                     password = askpass::askpass(),        # Enter your DFO network password.
                     type = 'ntlm')

# --- Function for getting container metadata (gives 'CtnStartDate' field) ---
get_container_meta <- function(batch_id){
  url <- paste0('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/odata/GetBatchContainers')
  request_body <- paste0(r"({"batch_id":")", batch_id, r"("})")
  x <- httr::POST(url, auth, body=request_body, encode="json", httr::verbose(), httr::content_type_json())
  y <-
    jsonlite::fromJSON(httr::content(x, "text"))$value |>
    dplyr::as_tibble() |>
    dplyr::mutate(across(c(FieldContainerId, ContainerNotes, AnalystName), as.character),
                  across(c(CntStartDate,CntEndDate), lubridate::as_date))
  return(y)
}

# --- Function for getting batch metadata (gives 'Location' field) ---
get_batch_meta <- function(batch_id){
  url <- paste0('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/api/AgeBatchDetail/GetAgeBatchDetail/', batch_id)
  x <- httr::GET(url, auth)
  batch_df <-
    httr::content(x, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  url <- 
    paste0("http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/odata/Project?$filter=Id eq ", batch_id) |>
    URLencode()
  y <- 
    httr::GET(url, auth) %>%
    httr::content(., "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() 
  y <- 
    select(y$value, Id, Location) %>%
    inner_join(batch_df, by="Id")
  
  return(y)
}

# --- Function for extracting batch age results ---
get_age_data <- function(batch_id){
  url <- paste0('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/api/AgeBatchDetail/ScaleAgeDetail/', 
                batch_id)
  x <- httr::GET(url, auth)
  y <- batch_df_sc <- jsonlite::fromJSON(httr::content(x, "text"))
  return(y)
}




# MRP CWT data extraction function --------------------------- (ty Nick Komick!)
getExtractorData <- function(query_doc, password = NULL) {
  extractor_url <- "http://pac-salmon.dfo-mpo.gc.ca/Api.DataExtractor.v2/api/DynamicQuery/QueryResult"
  if(file.exists(query_doc)) {
    query_file <- file(query_doc, "r")
    query_doc <- readLines(query_file)
    close(query_file)
  }
  user_name <- Sys.getenv("username")
  if(is.null(password)) {
    password <- askpass(paste0("Please enter your password for ",
                               user_name,
                               ":"))
  }
  data_response <-
    POST(extractor_url,
         authenticate(user_name, password, "ntlm"),
         encode = "json",
         content_type_json(),
         body = query_doc)
  cwt_data <- content(data_response)
  cwt_col_names <- unlist(cwt_data$Names)
  extractor_data <-
    lapply(cwt_data$Rows,
           function(.) {
             as_tibble(.$Cells, .name_repair = ~ cwt_col_names)
           }) %>%
    bind_rows()
  col_types <- unlist(cwt_data$Types)
  int_col <- cwt_col_names[grepl("int", col_types, ignore.case = TRUE)]
  dbl_col <- cwt_col_names[grepl("single", col_types, ignore.case = TRUE)]
  extractor_data <-
    extractor_data %>%
    mutate(across(all_of(int_col), as.integer)) %>%
    mutate(across(all_of(dbl_col), as.double))
  return(extractor_data)
}






# ======================== HOUSE KEEPING (READ THIS FIRST) ========================

# NOTE: If you are trying to read a SharePoint file and get an "Error... __ not found" message it might be because you have the file open in the app. Close it and re-run the code

# Two types of data sources exist with regard to this script: direct queries to databases, and side-loaded auxiliary files 

# Pre-steps for aux files:
# 1. Download all SEP files from EPRO: https://epro-stage.azure.cloud.dfo-mpo.gc.ca/EProWeb/#home
# 2. Download Otolith recovery specimens for chinook areas 20-26 from OtoManager: http://devios-intra.dfo-mpo.gc.ca/Otolith/Reports/recoveryspecimen.aspx
# 3. Read NPAFC chinook mark master file from network: Z:\Spec_Projects\Thermal_Mark_Project\Marks\All CN Marks from NPAFC Otolith Database to [_date_].xlsx


# One big issue is that similar variables do not share a common naming convention (e.g., "Otolith Box" vs. "Bag Number" etc.)
# Arbitrarily have gone with:
# Introduced at start for joining:
#   Otolith box or bag is called `(R) OTOLITH BOX NUM`
#   Otolith vial is called `(R) OTOLITH VIAL NUM`
#   Otolith box-vial combined is called `(R) OTOLITH BOX-VIAL CONCAT`
#   Scale book is called `(R) SCALE BOOK NUM`
#   Scale book cell is called `(R) SCALE CELL NUM`
#   Scale book-cell combined is called `(R) SCALE BOOK-CELL CONCAT`
#   Tag code is being called `(R) TAGCODE`
# Introduced later for calculations etc or eventual joins:
# Brood year is being called `(R) BROOD YEAR`
# Year of interest is called `(R) SAMPLE YEAR`
# Hatch code is called `(R) HATCH CODE`




#############################################################################################################################################################
  
#                                                                 0. READ/FORMAT DATA


# ======================== EPRO DATA (aux load) ========================

# Read EPRO base files ---------------------------
# NOTE: All files need to be the same format first - EPRO has a habit of providing a mix of .csv and .xlsx files :(
# Load files from Sharepoint:
epro.csvs <- lapply(list.files(paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                      "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/TEST/data_prep_files/EPRO/"), 
                               pattern = "All_Adult_Biosampling_", full.names = T), 
                    read.csv)

# Load all csv files as a Large List:
names(epro.csvs) <- list.files(paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                      "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/TEST/data_prep_files/EPRO/"), 
                               pattern = "All_Adult_Biosampling_", full.names = F)

# Convert the Large List into a useable R dataframe ---------------------------
allEPRO_2022 <- do.call("rbind", epro.csvs) %>%
  filter(Spawning.Stock !="") %>%
  tibble::rownames_to_column(var="file_source") %>%
  mutate(`(R) OTOLITH BOX NUM` = Bag.No,
         `(R) OTOLITH VIAL NUM` = Vial.No,
         `(R) OTOLITH BOX-VIAL CONCAT` = case_when(!is.na(Bag.No) & !is.na(Vial.No) ~ paste0(Bag.No,sep="-",Vial.No)),
         `(R) SCALE BOOK NUM` = Book.No,
         `(R) SCALE CELL NUM` = Scale.Sample.No,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(Book.No) & !is.na(Scale.Sample.No) ~ paste0(Book.No,sep="-",Scale.Sample.No)),
         `(R) TAGCODE` = `CWT.Tag.Code`) %>%
  mutate_at(c("(R) SCALE BOOK NUM", "Book.No"), as.character) %>%
  print()
remove(epro.csvs)





# ======================== PADS DATA (DIRECT QUERY) ========================

# Extract all South Coast scale batch IDs for SampleYear of interest and store in a vector of IDs called "batch_list_2022" ---------------------------
PADS_batch_ids_2022 <- httr::GET('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/api/Report/GetAgeBatchList',
                        auth) %>%
  # Get list of available age result batches:
  httr::content(x = .,  'text') %>%
  jsonlite::fromJSON() %>%
  # Filter batch list to keep only South Coast samples from 2022:
  filter(Sector == "SC",
         SampleYear == analysis_year) %>%
  pull(Id) # Extract column of age batch Ids from resulting dataframe



# Extract batch metadata, container metadata, and age data for scales in batch_list_2022; Join into 1 dataframe ---------------------------
### intermediate tables are ignored for now, but can be run to help with diagnosing code issues if needed
allPADS_2022 <- 
  full_join(
    # Batch metadata ---
    #PADS_batch_meta <- 
    PADS_batch_ids_2022 %>%
      purrr::map_dfr(get_batch_meta) %>%
      rename(BatchId = Id), 
    # Container metadata ---
    #PADS_container_meta <- 
    PADS_batch_ids_2022 %>%
      purrr::map_dfr(get_container_meta) %>%
      rename(ContainerId = Id), 
    by="BatchId") %>%
  full_join(.,
            # Age data ---
            #PADS_age_data <- 
            PADS_batch_ids_2022 %>%
              purrr::map_dfr(get_age_data) %>%
              rename(BatchId = Id),
            by=c("BatchId", "ProjectName", "Region", "Area", "Species", "RecoveryYear", "LifeHistory", "ContainerId")) %>%   # do not join by all fields as there are entry inconsistencies, e.g., "ProjectPriority==H vs ==High")
  mutate(`(R) SCALE BOOK NUM` = ContainerId,
         `(R) SCALE CELL NUM` = FishNumber,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(ContainerId) & !is.na(FishNumber) ~ paste0(ContainerId,sep="-",FishNumber))) %>%
  print()


# Data QC to make sure PADS join worked ---------------------------
nrow(PADS_batch_ids_2022 %>%
       purrr::map_dfr(get_age_data) %>%
       rename(BatchId = Id))
nrow(allPADS_2022)
# There will likely be a difference in the number of data rows in the Join vs. the number that have been aged - this is likely because the lab hasn't
# completed all aging yet. 

# Do the math to confirm that the unaged samples account for the difference (roughly), should result in TRUE: 
# (total # samples in PADS join) - (# unaged samples in the PADS join) == (total # samples available) 
nrow(allPADS_2022) - allPADS_2022%>%filter(is.na(GrAge)&is.na(ScaleCondition))%>%summarize(n=n())%>%pull(n) == nrow(PADS_batch_ids_2022 %>%
                                                                                                                      purrr::map_dfr(get_age_data) %>%
                                                                                                                      rename(BatchId = Id))
# note that there are sometimes a handful of issues, e.g., coho in a sockeye test fishery that won't always match up properly 






# ======================== OTOMANAGER/OTOLITH HATCH CODE DATA (aux load) ========================

# Read 2022 Otomanager data from SharePoint ---------------------------
allOTOMGR_2022 <- read_excel(path = paste0("C:/Users", sep="/", Sys.info()[6], sep="/",
                                   "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/TEST/data_prep_files/Otolith/RcvySpecAge.xlsx"),
                            sheet="RcvySpecAge", skip=1, guess_max=20000) %>%
  # Facility names in OtoMgr export for Robertson and Conuma don't align with NPAFC mark database... lol why would they? 
  mutate(FACILITY = case_when(FACILITY=="H-ROBERTSON CR" ~ "H-ROBERTSON CREEK H",
                              FACILITY=="H-CONUMA R " ~ "H-CONUMA RIVER H ",
                              TRUE~as.character(FACILITY)),
         `(R) OTOLITH BOX NUM` = `BOX CODE`,
         `(R) OTOLITH VIAL NUM` = `CELL NUMBER`,
         `(R) OTOLITH BOX-VIAL CONCAT` = case_when(!is.na(`BOX CODE`) & !is.na(`CELL NUMBER`) ~ paste0(`BOX CODE`,sep="-",`CELL NUMBER`))) %>%
  mutate_at("(R) OTOLITH VIAL NUM", as.character) %>%
  print()



# Read NPAFC marks applied spreadsheet from network ---------------------------
# << This may need updating from Aidan! >> 
allNPAFCmarks <- read_excel("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to May 1, 2023.xlsx",
           sheet="AllNPAFC CNReleasestoJun8,2022") %>%
  filter(STATE_PROVINCE != "ALASKA") %>%
  select(c(BROOD_YEAR, AGENCY, FACILITY, RELEASE_YEAR, STOCK, HATCH_CODE, STATE_PROVINCE, SITE)) %>%
  mutate(`(R) BROOD YEAR` = BROOD_YEAR,
         `(R) HATCH CODE` = HATCH_CODE) %>%
  print()




# ======================== CWT MRP DATA (DIRECT QUERY) ========================
# Note query doc is only for 2015-2022 releases
allMRP_2022 <- getExtractorData(query_doc = paste0("C:/Users", sep="/", Sys.info()[6], sep="/",
                                                   "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/TEST/data_prep_files/MRP/mrp_CNreleases_2015-2022_querydoc.json")) %>%
  select(c(Tagcode, `Species Name`, `Project Name`, `Brood Year`, `Release Year`,`Stock Site Name`,`Stock PSC Region Name`, `Hatchery Site Name`, 
           `Hatchery PSC Region Name`, `Release Site Name`, `Release PSC Region Name`, `Stage Code`)) %>%
  mutate(`(R) TAGCODE` = Tagcode)

# error troubleshooting: 
# "Error in cwt_data$Names : $ operator is invalid for atomic vectors" means you messed up either inside the query doc, or calling the location of the query doc
# loaded dataframe that is empty - you mistyped your password








# Introduced later for calculations etc or eventual joins:
# Brood year is being called `(R) BROOD YEAR`
# Year of interest is called `(R) SAMPLE YEAR`
# Hatch code is called `(R) HATCH CODE`







#############################################################################################################################################################


#                                                                  DATA QC (skip for now)


# ======================== NPAFC SUMMARY CODES ========================

# Look at NPAFC hatch code releases by facility/stock and releases per BY summary table - reference table 
View(npafc_marks %>% 
  filter(BROOD_YEAR%in%c(2015:lubridate::year(Sys.Date())), STATE_PROVINCE=="BRITISH COLUMBIA") %>%
  group_by(`HATCH CODE`,FACILITY,STOCK,BROOD_YEAR) %>%
  summarize(NUMBER_RELEASED=sum(NUMBER_RELEASED)) %>%
  pivot_wider(names_from = BROOD_YEAR, values_from = NUMBER_RELEASED))


# Look at duplicated hatch codes w/in this time frame 
#npafc_marks %>% 
#  filter(BROOD_YEAR%in%c(2015:2020), STATE_PROVINCE=="BRITISH COLUMBIA") %>%
#  group_by(`HATCH CODE`) %>%
#  summarize(facility=unique(FACILITY), stock=unique(STOCK)) %>%
#  group_by(`HATCH CODE`) %>%
#  summarize(times_used=n()) %>%
#  filter(times_used>1)





# ======================== EPRO QC: otolith box/vial duplicates ========================  FOLLOWING UP WITH JEFF ON THIS - FILTER THESE OUT FOR NOW. 

# Identify duplicate bag_vial_no codes ---------------------------
epro.duplicates <- epro %>%
  filter(!is.na(bag_vial_no)) %>% 
  group_by(bag_vial_no) %>%
  filter(n()>1) %>%
  group_by(bag_vial_no) %>%
  summarize(unique(bag_vial_no)) %>%
  pull(bag_vial_no)
# This ends up being 31 rows of 13 tag codes (various #s of duplicates)


# Filter out the duplicates ---------------------------
eproGOOD <- epro %>%
  filter(!bag_vial_no %in% epro.duplicates)









#############################################################################################################################################################

# 1.                                                    JOIN EPRO to PADS and OTOMGR (incl NPAFC) and MRP 


# ======================== JOIN EPRO + PADS ========================  
epro_pads_join <- left_join(allEPRO_2022, allPADS_2022,
                       by=c("(R) SCALE BOOK NUM","(R) SCALE CELL NUM", "(R) SCALE BOOK-CELL CONCAT")) %>%
  mutate_at("(R) OTOLITH BOX NUM", as.character)


# QC: See if any new ages were added from those that are available ---------------------------
# by testing whether the # of NAs changed between base EPRO and EPRO+PADS join. Should be TRUE.
nrow(allEPRO_2022 %>% filter(is.na(Scale.Gilbert.Age) & is.na(Book.No))) == nrow(epro_pads_join %>% filter(is.na(Scale.Gilbert.Age) & is.na(Book.No)))


# QC: See if there are any discrepancies in GR ages applied ---------------------------
View(epro_pads_join %>%
       select(BatchId, `(R) SCALE BOOK-CELL CONCAT`, Scale.Gilbert.Age, GrAge) %>%
       mutate(dif = case_when(Scale.Gilbert.Age==GrAge ~ "TRUE",
                              TRUE~"FALSE")) %>%
       filter(dif=="FALSE") %>% 
       filter(!is.na(BatchId)|!is.na(`(R) SCALE BOOK-CELL CONCAT`)))
# Only discrepancies so far are where NAs are in one column or the other and usually due to 3M/4M etc ages. 



# ======================== JOIN EPRO+PADS + OTOMANAGER ======================== 
epro_pads_oto_join <- left_join(epro_pads_join, allOTOMGR_2022,
                                by=c("(R) OTOLITH BOX NUM","(R) OTOLITH VIAL NUM","(R) OTOLITH BOX-VIAL CONCAT")) %>%
  print()



# ======================== JOIN EPRO+PADS+OTOMANAGER + MRP ======================== 
epro_pads_oto_mrp_join <- left_join(epro_pads_oto_join, allMRP_2022,
                                     by="(R) TAGCODE") %>%
  mutate(across(c(Scale.Gilbert.Age, Scale.Part.Age, Total.Age..yrs., GrAge, `PART AGE CODE`, `AGE GR`), ~as.character(.)),
         # Clean up GR Age for applying total age:
         `(R) RESOLVED GR AGE` = case_when(!is.na(Total.Age..yrs.) ~ Scale.Gilbert.Age,
                                        is.na(Scale.Gilbert.Age) & grepl("1$|2$", GrAge) ~ GrAge,
                                        Scale.Part.Age%in%c("RG","UD","LL","NS") & grepl("1$|2$", GrAge) ~ GrAge,
                                        Scale.Part.Age%in%c("RG","UD","LL","NS") & !grepl("1$|2$", GrAge) ~ as.character(NA),
                                        #Scale.Part.Age%notin%c("RG","UD","LL","NS","") ~ Scale.Part.Age,
                                        TRUE ~ as.character(NA)),
         # Apply a total age to calculate BY
         `(R) RESOLVED TOTAL AGE` = case_when(!is.na(`Brood Year`) ~ as.character(analysis_year-as.numeric(`Brood Year`)),
                                              grepl("^[0-9]",`(R) RESOLVED GR AGE`) & endsWith(GrAge, "1")|endsWith(GrAge, "2") ~ 
                                                str_sub(`(R) RESOLVED GR AGE`, start=1, end=1)),
         `(R) SAMPLE YEAR` = analysis_year,
         # Calculate BY: priority is: CWT > scales 
         `(R) BROOD YEAR` = case_when(!is.na(`Brood Year`) ~ as.numeric(`Brood Year`),
                                      is.na(`Brood Year`) ~ `(R) SAMPLE YEAR`-as.numeric(`(R) RESOLVED TOTAL AGE`)),
         `(R) HATCH CODE` = `HATCH CODE`) %>%
  print()
  
  





# ======================== JOIN TO NPAFC MARK MASTER FILE ======================== 

allEPROBIO_2022 <- left_join(epro_pads_oto_mrp_join, allNPAFCmarks,
                             by=c("(R) BROOD YEAR", "(R) HATCH CODE")) %>%
  # Apply stock ID. Priority is: CWT > Otolith 
  mutate(`(R) RESOLVED STOCK ID` = case_when(!is.na(`Stock Site Name`) ~ `Stock Site Name`,
                                             is.na(`Stock Site Name`) ~ str_to_title(str_sub(STOCK, start=3)))) %>%
  print()



# ======================== Export megafile ======================== 

# Create worksheets --------------------------- 
sheet_list <- list("1 - all EPRO+biodata" = data.frame(allEPROBIO_2022),
                   "2 - allEPRO_2022 dump" = data.frame(allEPRO_2022),
                   "3 - allPADS_2022 dump" = data.frame(allPADS_2022),
                   "4 - allOTOMGR_2022 dump" = data.frame(allOTOMGR_2022),
                   "5 - allMRP_2022 dump" = data.frame(allMRP_2022),
                   "6 - allNPAFCmarks master" = data.frame(allNPAFCmarks))

sheet_list <- sheet_list %>%
  append(list("metadata" = data.frame("sheets" = names(sheet_list),
                                      "description" = c("Final joined data. Based on EPRO 'All Adult Biosampling' files as the base, then joined PADS, Otolith Manager, MRP releasess, and NPAFC mark information. CWT data was priority for stock ID and ages, followed by otolith stock ID and scale data (respectively).",
                                                        "Raw EPRO 'All Adult Biosampling' dump original files pre-join (auxiliary download). Downloaded manually from EPRO https://epro-stage.azure.cloud.dfo-mpo.gc.ca/EProWeb/#home",
                                                        "Raw PADS output directly queried from http://pac-salmon.dfo-mpo.gc.ca/CwtDataEntry/#/AgeBatchList via API addresses.",
                                                        "Raw Otolith Manager dump original file (auxiliary download) from http://devios-intra.dfo-mpo.gc.ca/Otolith/Reports/recoveryspecimen.aspx > 2022 > PFMA 20-26 Chinook.",
                                                        "Raw MRP Releases dump directly queried from http://pac-salmon.dfo-mpo.gc.ca/DataExtractor/ using API and .json query document 'mrp_CNreleases_2015-2022_querydoc.json'.",
                                                        "NPAFC mark master sheet managed by Aidan Gooddall/Jeff Till located on our network //dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to May 1, 2023.xlsx"))))



# Create workbook --------------------------- 
writexl::write_xlsx(x=sheet_list,
                    col_names=T, 
                    path = 
                      paste0(
                        paste0(
                          "C:/Users", sep="/", Sys.info()[6], sep="/", 
                          "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/TEST/data_prep_files"
                        ),
                        sep="/",
                        paste0("allEPRO_biodata_output", sep="_", format(Sys.time(), sep="_", "%Y-%m-%d_%H%M"), ".xlsx")
                      )
)

 

















































# ======================== EXPORTS ========================

# Upload EPRO master file to SharePoint ---------------------------
#write_xlsx(epro,
#           path = 
#             # Create the directory
#             paste0(
#               # Create the parent directory: "C:/Users/[DFO USERNAME autodetect]/DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data/"
#               paste0(
#                 "C:/Users", sep="/", Sys.info()[6], sep="/", 
#                 "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data/EPRO"
#               ),
#               sep="/",
#               # File name:  
#               "EPRO_ALL_FACILITIES", sep="_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".xlsx"
#             ))




# Upload EPRO+OTOLITH join file to SharePoint ---------------------------
#write_xlsx(epro_oto_NPAFCBY,
#           path = 
#             # Create the directory
#             paste0(
#               # Create the parent directory: "C:/Users/[DFO USERNAME autodetect]/DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data/"
#               paste0(
#                 "C:/Users", sep="/", Sys.info()[6], sep="/", 
#                 "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data/BiodataResults"
#               ),
#               sep="/",
#               # File name:  
#               "EPRO_PADS_OTOMGR_NPAFC_JOIN", sep="_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".xlsx"
#             ))


#############################################################################################################################################################



































