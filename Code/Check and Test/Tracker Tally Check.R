# Script to determine if all records in HP Tracker are also in my PHESS extract prior to matching process
# See Esther email on 5 June 2025 re: 35% success rate of matching in the Tracker
# Odd since the matching process prior to merging into the Tracker was at 95%

# Load libraries and functions
library(here)
library(tidyverse)
library(janitor)
library(readxl)

# Load the two data sets - HP Tracker and Final matching table
tracker <- read_excel(here("Data", "Health Protection Tracker.xlsx")) %>% 
  clean_names()

matches <- read_excel(here("Data", "final_table_Jan2024_May2025.xlsx"))
nrow(distinct(matches, phess_id))
anyNA(matches$phess_id)

dup_matches <- matches %>% # matches data set has 8 (4 x 2) duplicates from linelist
  group_by(phess_id) %>% 
  filter(n()>1)

# are all phess_id in matches in linelist?
matches_in_linelist <- linelist %>% 
  filter(phess_id %in% distinct(matches, phess_id))

# exploratory findings
tabyl(tracker, condition)
sum(is.na(tracker$phess_id))


# anti-join function to determine the obs in Tracker that were not found in the matches dataset
tracker_unmatch <- anti_join(tracker, matches, by="phess_id")
tracker_match <- semi_join(tracker, matches, by="phess_id")


# Load the linelist dataset
nephu_linelist_file <- "NEPHUCaseLinelistGPExtraction_Jan2024May2025.xlsx" # Combined files 1 Jan 2024 - 31 May 2025
nephu_assign_linelist_file <- "NEPHUCaseLinelistGPExtractionAssignedLPHU_Jan2024May2025.xlsx" # Combined files 1 Jan 2024 - 31 May 2025


linelist.raw <- readxl::read_xlsx(here::here("Data", nephu_linelist_file)) %>% 
  clean_names() # use NEPHU extract

linelist_assign.raw <- readxl::read_xlsx(here::here("Data", nephu_assign_linelist_file)) %>% 
  clean_names() # use NEPHU extract

provider_config2 <- c("Melbourne Pathology", "Royal Children's Hospital Pathology")

linelist <- linelist.raw %>% 
  filter(case_found_by=="Clinical Presentation") %>% 
  filter(most_recent_event_classfication %in% c("Confirmed", "Probable", "Suspected", "At risk")) %>% 
  filter(!is.na(elr_hl7_message)) %>% 
  rename(lab = notifying_laboratory, 
         provider = referring_provider_details) %>% 
  select(phess_id, condition, first_name, last_name, birth_date, 
         event_date, investigation_status, re_investigation_status, 
         lab, provider, elr_hl7_message) %>% 
  # remove subsequent entries from lab and provider fields - retain only first entry
  mutate(lab1 = if_else(str_detect(lab, ".+;.+"), stringr::word(lab, sep=";"), lab),  
         #provider1 = if_else(str_detect(provider, ".+;.+"), str_extract(provider, "^.+(?=;)"), provider), 
         provider1 = if_else(str_detect(provider, ".+;.+"), stringr::word(provider, sep=";"), provider)) %>% 
  # extract Clinician name and practice name from the provider field
  mutate(Prov_full_name = if_else(!lab1 %in% provider_config2, 
                                  str_split(provider1, "\\|", simplify = TRUE)[, 1], 
                                  paste(str_split(provider1, "\\|", simplify = TRUE)[, 1], str_split(provider1, "\\|", simplify = TRUE)[, 2])),  
         # remove excess blank spaces
         Prov_full_name = if_else(lab1 %in% provider_config2, str_replace(Prov_full_name, "\\s+", " "), Prov_full_name), 
         # remove blank space at end of last name
         Prov_full_name = str_remove(Prov_full_name, "\\s$"), 
         Prov_clinic = if_else(!lab1 %in% provider_config2, 
                               str_split(provider1, "\\|", simplify = TRUE)[, 2], 
                               str_split(provider1, "\\|", simplify = TRUE)[, 3])) %>% 
  mutate(Prov_first_name = str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,1]), 
         Prov_last_name = str_extract(Prov_full_name, "\\b\\w+$"), # str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,2]), 
         Prov_clinic = str_trim(Prov_clinic, side="both"), 
         Prov_provider_num = str_extract(provider1, "\\b\\d\\S{6}[a-zA-Z]$"))

linelist_assign <- linelist_assign.raw %>% 
  filter(case_found_by=="Clinical Presentation") %>% 
  filter(most_recent_event_classfication %in% c("Confirmed", "Probable", "Suspected", "At risk")) %>% 
  filter(!is.na(elr_hl7_message)) %>% 
  rename(lab = notifying_laboratory, 
         provider = referring_provider_details) %>% 
  select(phess_id, condition, first_name, last_name, birth_date, 
         event_date, investigation_status, re_investigation_status, 
         lab, provider, elr_hl7_message) %>% 
  # remove subsequent entries from lab and provider fields - retain only first entry
  mutate(lab1 = if_else(str_detect(lab, ".+;.+"), stringr::word(lab, sep=";"), lab),  
         #provider1 = if_else(str_detect(provider, ".+;.+"), str_extract(provider, "^.+(?=;)"), provider), 
         provider1 = if_else(str_detect(provider, ".+;.+"), stringr::word(provider, sep=";"), provider)) %>% 
  # extract Clinician name and practice name from the provider field
  mutate(Prov_full_name = if_else(!lab1 %in% provider_config2, 
                                  str_split(provider1, "\\|", simplify = TRUE)[, 1], 
                                  paste(str_split(provider1, "\\|", simplify = TRUE)[, 1], str_split(provider1, "\\|", simplify = TRUE)[, 2])),  
         # remove excess blank spaces
         Prov_full_name = if_else(lab1 %in% provider_config2, str_replace(Prov_full_name, "\\s+", " "), Prov_full_name), 
         # remove blank space at end of last name
         Prov_full_name = str_remove(Prov_full_name, "\\s$"), 
         Prov_clinic = if_else(!lab1 %in% provider_config2, 
                               str_split(provider1, "\\|", simplify = TRUE)[, 2], 
                               str_split(provider1, "\\|", simplify = TRUE)[, 3])) %>% 
  mutate(Prov_first_name = str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,1]), 
         Prov_last_name = str_extract(Prov_full_name, "\\b\\w+$"), # str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,2]), 
         Prov_clinic = str_trim(Prov_clinic, side="both"), 
         Prov_provider_num = str_extract(provider1, "\\b\\d\\S{6}[a-zA-Z]$"))

# anti-join function to determine the obs in Tracker that were not found in the linelist dataset
tkr_unmatch_linelist <- anti_join(tracker, linelist, by="phess_id")

# anti-join function to determine the obs in Tracker that were not found in the linelist.raw dataset
tkr_unmatch_linelist.raw <- anti_join(tracker, linelist.raw, by="phess_id") %>% 
  arrange(phess_event_date)

tabyl(tkr_unmatch_linelist.raw, condition)


sum(is.na(tkr_unmatch_linelist$phess_id)) 
nrow(distinct(unmatch_linelist, phess_id)) # includes 383 NA rows + 271 obs = 654

sum(is.na(tkr_unmatch_linelist.raw$phess_id)) 
nrow(distinct(unmatch_linelist.raw, phess_id)) # includes 383 NA rows + 162 obs = 545

tkr_unmatch_linelist <- arrange(tkr_unmatch_linelist, as.numeric(phess_id))

# ascertaining the extra rows in unmatch_linelist since linelist is smaller than linelist.raw
linelist_extra <- anti_join(tkr_unmatch_linelist, tkr_unmatch_linelist.raw, by="phess_id")
linelist.raw_extra_check <- filter(linelist.raw, phess_id %in% linelist_extra$phess_id)
# --> this is correct since 654 obs were not matched in the tracker and 545 obs were not matched, 
# it would be that the difference of 109 was due to filter from linelist.raw to linelist.

# anti-join unmatch_linelist with unmatch_linelist.raw to determine which cases got removed in the linelist.raw cleaning step
removed_linelist <- anti_join(tkr_unmatch_linelist, tkr_unmatch_linelist.raw, by="phess_id")

# reasons for removal - missing elr field, rejected case defn, ...

# what about the remaining 545 obs?

# Check against the linelist_assign.raw dataset to see if it contains the missing 545
linelist_assign.raw_extras <- anti_join(linelist_assign.raw, linelist.raw, by="phess_id") #936 obs
linelist_assign_extras <- anti_join(linelist_assign, linelist, by="phess_id") #326 obs


tkr_match_assignedlphu_extras <- semi_join(tracker, linelist_assign.raw_extras, by="phess_id") #23 obs

# check details of matched assignedLPHU extras obs
match_assignedLPHU_extras_details <- linelist_assign.raw %>% 
  filter(phess_id %in% tkr_match_assignedlphu_extras$phess_id)

# check details of remove_linelist
removed_linelist_rawset <- linelist.raw %>% 
  filter(phess_id %in% removed_linelist$phess_id)


unmatch_linelist.raw_nophessid <- unmatch_linelist.raw %>% 
  filter(is.na(phess_id)) %>% 
  filter(condition != "Gastro Outbreaks")

tabyl(unmatch_linelist.raw_nophessid, condition)

# filter out unmatch_linelist.raw - missing phess_ids, gastroOBs and pre-2024
# pre-2024, post-May 2025, gastroOBs, filter out from raw data, assigned_lphu
unmatch_linelist.raw_remaining <- tkr_unmatch_linelist.raw %>% 
  filter(!is.na(phess_id)) %>% 
  filter(condition!="Gastro Outbreaks") %>% 
  mutate(phess_event_date = as.Date(phess_event_date)) %>% 
  filter(phess_event_date>= ymd("2024-01-01")) %>% 
  filter(phess_event_date<ymd("2025-06-01")) %>% 
  filter(!phess_id %in% tkr_match_assignedlphu_extras$phess_id) %>% 
  left_join(., linelist_assign.raw, by="phess_id")

tabyl(tkr_unmatch_linelist.raw, condition)

gastroOB <- tkr_unmatch_linelist.raw %>% 
  filter(condition=="Gastro Outbreaks")

pre_2024 <- tkr_unmatch_linelist.raw %>% 
  mutate(phess_event_date = as.Date(phess_event_date)) %>% 
  filter(phess_event_date < ymd("2024-01-01"))

missing_ids<- tkr_unmatch_linelist.raw %>% 
  filter(is.na(phess_id)) %>% 
  filter(is.na(condition))

# check details of unmatch_linelist.raw after removing 



# find those with missing elr fields from linelist.raw dataset
elr_na <- linelist.raw %>% 
  filter(is.na(elr_hl7_message))

writexl::write_xlsx(elr_na, "elr_na.xlsx")
