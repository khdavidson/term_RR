# Term RR stock group coding 
# Automating Karin's Termxx grouping decisions
# February 2023

# ======================== SET UP ========================

# Load packages ---------------------------
library(dplyr)
library(readxl)
library(writexl)
library(httr)
library(askpass)
library(stringr)


# Define helper functions ---------------------------
"%notin%" <- Negate("%in%")

getStreams <- function(query_doc, password = NULL) {
  nuseds_url <- "http://pac-salmon.dfo-mpo.gc.ca/Api.NuSEDS.v2/api/DynamicQuery/QueryResult"
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
    POST(nuseds_url,
         authenticate(user_name, password, "ntlm"),
         encode = "json",
         content_type_json(),
         body = query_doc)
  nuseds_data <- content(data_response)
  nuseds_col_names <- unlist(nuseds_data$Names)
  nuseds_data <-
    lapply(nuseds_data$Rows,
           function(.) {
             as_tibble(.$Cells, .name_repair = ~ nuseds_col_names)
           }) %>%
    bind_rows()
  col_types <- unlist(nuseds_data$Types)
  int_col <- nuseds_col_names[grepl("int", col_types, ignore.case = TRUE)]
  dbl_col <- nuseds_col_names[grepl("single", col_types, ignore.case = TRUE)]
  nuseds_data <-
    nuseds_data %>%
    mutate(across(all_of(int_col), as.integer)) %>%
    mutate(across(all_of(dbl_col), as.double))
  return(nuseds_data)
}




# Read data from SharePoint ---------------------------
# Find the CREST biodata on the SharePoint site - must have synced locally first. Ask KD how to do this if you aren't sure.
crest_biodata <- read_excel(path = paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                             "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data/WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS (Wednesday, February 22, 2023 5 51 PM).xlsx"),
                          sheet="WCVI_Chinook_Run_Rec", guess_max=20000) 


# Build stream-area aux file ---------------------------
# This is for applying the "area.origin" field so that we can group fish as being from "Other Area 23", "Other Area 25", etc.
streamAreas <- #left_join(
  # Load stream by area from NuSEDS query (formerly 'NUstreams') - a little slow.
  #NUstreams <- 
  getStreams(paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                    "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/Escapement/nuseds_stream-area.json"), password="babysharkd0d0!") %>% 
    group_by(`Waterbody Name`) %>% 
    summarize(Area=unique(Area)) %>% 
    # Create new columns:
    mutate(Area = as.numeric(gsub('[A-z]+', '', Area)),                                                              # Remove the sub-area letter from Area name
           `Waterbody Name` = str_to_title(`Waterbody Name`)) %>%                                                    # Make waterbody name title case ('Title Case') so that it can match CREST
    rename(area.origin=Area,
           RESOLVED_STOCK_ORIGIN=`Waterbody Name`)%>%
  # Had to remove Salmon River in Fraser for now because it is causing issues:
  filter(RESOLVED_STOCK_ORIGIN!="Salmon River" & area.origin!=29) %>%                                                
  # Make some manual adjustments to names so that they match the CREST stock IDs:
  mutate(RESOLVED_STOCK_ORIGIN = case_when(RESOLVED_STOCK_ORIGIN=="Qualicum River" ~ "Big Qualicum River",           
                                           TRUE ~ as.character(RESOLVED_STOCK_ORIGIN)),
         RESOLVED_STOCK_ORIGIN = case_when(RESOLVED_STOCK_ORIGIN=="Tranquil Creek" ~ "Tranquil River",
                                           TRUE ~ as.character(RESOLVED_STOCK_ORIGIN)),
         RESOLVED_STOCK_ORIGIN = gsub("Toquart", "Toquaht", RESOLVED_STOCK_ORIGIN)) %>%
  ungroup() %>%
  # More manual adjustments... ugh... adding systems that are not in NuSEDS:
  add_row(RESOLVED_STOCK_ORIGIN="Robertson Creek", area.origin=23) %>%        
  add_row(RESOLVED_STOCK_ORIGIN="Omega Pacific Hatchery", area.origin=23) %>%
  print()



################################################################################################################################################################


#                                                                CODING TERM RUN GROUPS 


# Define helper variables ---------------------------
# Focal streams in each area to highlight
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")

# Used to remove the river/creek suffix later
stopwords <- c(" River", " Creek")


# ======================== CODE TERM RUN GROUPS ========================
crest_biodata_coded <- 
  # Join CREST biodata and streamAreas aux file ---------------------------
  left_join(crest_biodata, 
            streamAreas) %>% 
         
  mutate(
    #1. Create 'Hat/Nat' column ---------------------------
    `Hat/Nat (R)` = case_when(
      #1.1 If HATCHERY ORIGIN is a "Y", make it "Hatchery"
      HATCHERY_ORIGIN=="Y" ~ "Hatchery",
      #1.2 If it's not clipped and the thermal mark indicates "Not Marked", make it "Natural"
      ADIPOSE_FIN_CLIPPED=="N" & THERMALMARK=="Not Marked" ~ "Natural",
      #1.3 If it's neither of these scenarios, make it "Unknown"
      TRUE ~ "Unknown"),
    
    #2. Create 'Term RR Roll Ups' column ---------------------------
    `Term RR Roll Ups (R)` = case_when(
      #2.0 Base case if is.na(RESOLVED_STOCK_ORIGIN) make it "Unknown"
      is.na(RESOLVED_STOCK_ORIGIN) ~ "Unknown", 
      #2.2 If it is NOT from NWVI or SWVI, it gets "NON-WCVI"
      RESOLVED_STOCK_ROLLUP%notin%c("NWVI", "SWVI") ~ "NON-WCVI",
      #2.4 Special case: Change "Tofino Hatchery" to "Bedwell"
      RESOLVED_STOCK_ORIGIN=="Tofino Hatchery" ~ "BEDWELL",
      #2.3 If it IS from NWVI or SWVI, this bit takes the stock ID from RESOLVED_STOCK_ORIGIN and removes 'creek' or 'river' so it just becomes uppercase BURMAN, CONUMA, etc.
      RESOLVED_STOCK_ROLLUP%in%c("NWVI", "SWVI") ~ toupper(gsub(paste0("\\b(",paste(stopwords, collapse="|"),")\\b"), "", RESOLVED_STOCK_ORIGIN))),
      
    
    #3. Create 'TermSum' column ---------------------------
    `Term Sum (R)` = paste(`Hat/Nat (R)`, `Term RR Roll Ups (R)`, sep=" "),
    
    #4. Create 'TermCON' column ---------------------------
    `TermCON (R)` = case_when(
      #4.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `Term Sum (R)`,
      #4.1 Identify all NON-WCVI stocks and carry through the "NON-WCVI" tag from 'Term Sum (R)'
      `Term RR Roll Ups (R)`=="NON-WCVI" ~ `Term Sum (R)`,
      #4.2 Identify all of the focal rivers above that get their own group throughout the term RR process:
      `Term RR Roll Ups (R)`%in%focal_a25 ~ `Term Sum (R)`,
      #4.3 Identify all systems not in focal_a25 above, but still in area 25 (using "area.origin" created above) and make them "Other Area 25"
      `Term RR Roll Ups (R)`%notin%focal_a25 & area.origin==25 ~ paste(`Hat/Nat (R)`, "Other Area 25", sep=" "),
      #4.4 Same as above but for "Other Area 23"
      `Term RR Roll Ups (R)`%notin%focal_a25 & area.origin==23 ~ paste(`Hat/Nat (R)`, "Other Area 23", sep=" "),
      #4.5 Identify all systems NOT IN focal_a25, and also NOT assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" 
      `Term RR Roll Ups (R)`%notin%focal_a25 & area.origin%notin%c(23,25) ~ paste(`Hat/Nat (R)`, "Other WCVI", sep=" ")),
    
    #5. Create TermNIT column ---------------------------
    `TermNIT (R)` = case_when(
      #5.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `Term Sum (R)`,
      #5.1 same as 4.1
      `Term RR Roll Ups (R)`=="NON-WCVI" ~ `Term Sum (R)`,
      #5.2 same as 4.2 but area 22
      `Term RR Roll Ups (R)`%in%focal_a22 ~ `Term Sum (R)`,
      #5.3 same as 4.3 but area 22
      `Term RR Roll Ups (R)`%notin%focal_a22 & area.origin==25 ~ paste(`Hat/Nat (R)`, "Other Area 25", sep=" "),
      #5.4 same as 4.4 but area 22
      `Term RR Roll Ups (R)`%notin%focal_a22 & area.origin==23 ~ paste(`Hat/Nat (R)`, "Other Area 23", sep=" "),
      #5.5 same as 4.5 but area 22
      `Term RR Roll Ups (R)`%notin%focal_a22 & area.origin%notin%c(23,25) ~ paste(`Hat/Nat (R)`, "Other WCVI", sep=" ")),
    
    #6. Create TermArea23 column ---------------------------
    `TermArea23 (R)` = case_when(
      #6.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `Term Sum (R)`,
      #6.1 same as 4.1
      `Term RR Roll Ups (R)`=="NON-WCVI" ~ `Term Sum (R)`,
      #6.2 same as 4.2
      `Term RR Roll Ups (R)`%in%focal_a23 ~ `Term Sum (R)`,
      #6.3 same as 4.3
      `Term RR Roll Ups (R)`%notin%focal_a23 & area.origin==25 ~ paste(`Hat/Nat (R)`, "Other Area 25", sep=" "),
      #6.4 same as 4.4  
      `Term RR Roll Ups (R)`%notin%focal_a23 & area.origin==23 ~ paste(`Hat/Nat (R)`, "Other Area 23", sep=" "),
      #6.5 same as 4.5
      `Term RR Roll Ups (R)`%notin%focal_a23 & area.origin%notin%c(23,25) ~ paste(`Hat/Nat (R)`, "Other WCVI", sep=" ")) ,
    
    # Create QC flag columns to see cases where Term labels don't match ---------------------------
    `qcFlag_TermCON=TermNIT?` = case_when(`TermCON (R)` == `TermNIT (R)` ~ "TRUE"),
    `qcFlag_TermCON=Term23?` = case_when(`TermCON (R)` == `TermArea23 (R)` ~ "TRUE"),
    `qcFlag_TermNIT=Term23?` = case_when(`TermNIT (R)` == `TermArea23 (R)` ~ "TRUE")
    ) %>%
  print()






# ======================== EXPORT TO SHAREPOINT ========================
# note, can't append to existing Excel workbook because it's just too big - everything crashes and burns. 
write_xlsx(crest_biodata_coded,
           # Tell R where to write the file to
           path = 
             # Create the directory
             paste0(
               # Create the parent directory: "C:/Users/[DFO USERNAME autodetect]/DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data/"
               paste0(
                 "C:/Users", sep="/", Sys.info()[6], sep="/", 
                 "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data"
                 ),
               sep="/",
               # Remove the .xlsx ending of the files we find below
               gsub(".xlsx", "", 
                    # Look for files that start with "WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS" in the parent directory
                    grep("WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS",
                         list.files(path = paste0("C:/Users", sep="/", Sys.info()[6], sep="/",
                                                  "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/Communal data" )),
                         value=T
                         )
                    ),
             # Append "_CODED_[today's date and time].xlsx" to the file name we find above:   
             sep="_", "CODED", sep="_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".xlsx"
             )
)
           













 