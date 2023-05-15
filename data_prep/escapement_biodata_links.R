# Escapement Biosample linking
# To support TRR
# May 2023 



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
# 1. Locate most recent escapement biodata file on the network: SCD_StAD\SC_BioData_Management\2-Escapement\2015-[yyyy]_WCVI_Escapement-FSC_BioData.xlsx
# 2. Download Otolith recovery specimens for all species areas 20-27 and 120-127 from OtoManager: http://devios-intra.dfo-mpo.gc.ca/Otolith/Reports/recoveryspecimen.aspx
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


# ======================== ESCAPEMENT BIODATA (aux load) ========================

# 1. Examine escapement biodata files avialable: 
list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/"), 
           recursive=F, pattern="^[^~]*.xlsx") 

# 2. Select the most recent one. This is manual because the naming convention sucks: 
esc_biodata_recent_filename <- list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement"),
                                          recursive=F, pattern="^[^~]*.xlsx")[4]

#3. Read in the file and reformat:
esc_biodata <- cbind(
  read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="F1:AX10000"),
  read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CC1:CE10000"),
  read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CI1:CM10000"),
  read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CP1:CP10000"),
  read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CY1:CZ10000")) %>%
  mutate(`(R) OTOLITH BOX NUM`=`Otolith Box #`,
         `(R) OTOLITH VIAL NUM` = `Otolith Specimen #`,
         `(R) OTOLITH BOX-VIAL CONCAT` = case_when(!is.na(`Otolith Box #`) & !is.na(`Otolith Specimen #`) ~ paste0(`Otolith Box #`,sep="-",`Otolith Specimen #`)),
         `(R) SCALE BOOK NUM` = `Scale Book #`,
         `(R) SCALE CELL NUM` = `Scale #`,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`Scale Book #`) & !is.na(`Scale #`) ~ paste0(`Scale Book #`,sep="-",`Scale #`)),
         `(R) SAMPLE YEAR` = Year)





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










#############################################################################################################################################################

#                                                       1. JOIN ESCAPEMENT BIODATA to PADS and OTOMGR 













