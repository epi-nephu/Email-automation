# MAIN EXTRACTION AND MATCHING CODE

# Use referring_provider_details to obtain Dr and Clinic name. 
# 1 issue is that clinic name is sometimes not a proper name but sometimes address or name begins with shop number
# However could use Dr name from referring_provider_details to confirm which line to pull from the ELR field


# Version control notes:
# Incorporated extraction of HL7 field by using name in Provider field as reference - see issue identified above
# Resulting dr name details naming format as eg: Prov_firstname rather than dr_first_name, ...
# Combined all cleaning steps into single piped process
# Fuzzy matching code section transferred onto separate script file (FuzzyMatching.R) - fuzzy matching is now optional in the process



# Load libraries and functions
library(here)
library(tidyverse)
library(janitor)

source(here("Code", "CommonFunctions.R"))


# Set Configurations ----
## provider field configurations
## config1: Full name | Medical centre | Provider number
## config2: First name | Last name | Medical centre | Provider number

provider_config2 <- c("Melbourne Pathology", "Royal Children's Hospital Pathology")

## string pattern to extract provider number from provider field
pattern_provider <- "\\b(?<=|)(?<!\\s)\\d{6}[A-Z0-9][a-zA-Z]\\b"

# Load files ----
#nephu_linelist_file <- "NEPHUCaseLinelistGPExtractionAssignedLPHU_7-20Jul2025.xlsx" 

nephu_linelist_file <- datafile

linelist.raw <- readxl::read_xlsx(here::here("Data", nephu_linelist_file)) %>% clean_names() # use NEPHU extract


## Load NEPHU GP list data and cleaning code ----
source(here("Code", "PrepGPList.R"))


# Prep Linelist file ----
## Clean up linelist file for extraction and matching exercise
## Filter data to include only cases found by Clinical Presentation. Remove Rejected and Not Notifiable cases
linelist <- linelist.raw %>% 
  filter(case_found_by=="Clinical Presentation") %>% 
  filter(most_recent_event_classfication %in% c("Confirmed", "Probable", "Suspected", "At risk")) %>% 
  #filter(!is.na(elr_hl7_message)) %>% 
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
         # replace obscure names as NA
         Prov_full_name = if_else(str_detect(Prov_full_name, 
                                          regex("HOSPITAL|LABORATORY|PATHOLOGY|LAB|MSHC|Path|Melbourne|Document|Department|
                                                Unknown|Form|Request|C/-|Coordinator|Nurse|Study|Doctor|A/Prof|Missing|Uncoded|
                                                Address|Council|Environmental|Team|Leader|BUPA|Administration|Medical", 
                                                ignore_case=TRUE)), 
                               NA_character_, 
                               Prov_full_name), 
         # replace as NA if name contains numbers
         Prov_full_name = if_else(str_detect(Prov_full_name, "\\d+"), NA_character_, Prov_full_name), 
         Prov_clinic = if_else(!lab1 %in% provider_config2, 
                             str_split(provider1, "\\|", simplify = TRUE)[, 2], 
                             str_split(provider1, "\\|", simplify = TRUE)[, 3])) %>% 
  mutate(Prov_first_name = str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,1]), 
         Prov_last_name = str_extract(Prov_full_name, "\\b\\w+$"), # str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,2]), 
         Prov_clinic = str_trim(Prov_clinic, side="both"), 
         Prov_provider_num = str_extract(provider1, "\\b\\d\\S{6}[a-zA-Z]$")) %>% 

  # process dataframe row by row (required for next mutate step which involves working with lists)
  rowwise() %>% 
  
  # clean up elr_hl7_field - extract only the ORC and PV1 lines
  mutate(reduce_hl7 = modify_HL7_message(elr_hl7_message), 
         specific_hl7 = specific_hl7_message(reduce_hl7, Prov_last_name)) %>% 
  ungroup() %>% 
  
  # remove duplicate rows created from above function
  distinct(phess_id, .keep_all=TRUE) %>% 
  
  # further clean hl7 field to remove unnecessary symbols
  mutate(specific_hl7 = further_clean(specific_hl7)) %>% 
  
  # extract provider number from HL7 field
  mutate(hl7_provider_number = str_extract(specific_hl7, pattern_provider), 
         provider_number_final = ifelse(!is.na(hl7_provider_number), hl7_provider_number, NA)) %>% 
  
  # extract practice name, address and tel number from HL7 field
  mutate(
    # detect error in hl7 field that will not be able to be processed in next step)
    error_hl7 = if_else(str_detect(specific_hl7, "DETECTED"), "ERROR", NA_character_), 
    # capitalise all letters
    specific_hl7 = toupper(specific_hl7), 
    # replace all short and abbreviated words with full spelling
    specific_hl7 = str_replace_all(specific_hl7, "\\s\\bCTRE\\b", "CENTRE"), 
    specific_hl7 = str_replace_all(specific_hl7, "\\s\\bCTR\\b", "CENTRE"), 
    specific_hl7 = str_replace_all(specific_hl7,"AUSHIC\\sMC",""), 
    specific_hl7 = str_replace_all(specific_hl7,"\\sM/C"," MEDICAL CENTRE"), 
    specific_hl7 = str_replace_all(specific_hl7,"\\sMC", " MEDICAL CENTRE"), 
    specific_hl7 = str_replace_all(specific_hl7,"Y PUBLIC HEALTH","")) %>% 
  
  # filter out rows that had errors in hl7 field and therefore cannot be processed further
  filter(is.na(error_hl7)) %>% 
  
  # remove the error_hl7 column
  select(-error_hl7) %>% 
  
  # extract phone number, address and practice name in rowwise order using pre-defined user functions
  rowwise() %>%
  mutate(
    hl7_phone_number = extract_first_phone_number(specific_hl7), #phone clean function
    hl7_address = extract_address(specific_hl7), #address clean function
    hl7_medical_centre = extract_medical_centre(specific_hl7) #medical centre clean funtion
  ) %>%
  ungroup() %>%
  
  # Remove original long HL7 message column (to conserve space for conversion of df to Excel file later)
  dplyr::select(-c(elr_hl7_message)) %>%
  
  # Update medical centres to remove merged strings - AUSHICPR, DR_noted, AUS M, etc.
  mutate(hl7_medical_centre = str_replace_all(hl7_medical_centre, "AUSHICPR", "") %>% 
           str_replace_all("DR_noted", "") %>%
           str_replace_all("AUS M", "") %>% 
           str_replace_all("\\\\T\\\\", "AND")) %>% 
  
  # doing a second extract on GPlist_GPname if medical centre field is empty
  mutate(hl7_practice_name = if_else(hl7_medical_centre == "",
                                    str_extract(specific_hl7, 
                                                "(?<=\\|)\\s*([^|]*\\b(?:CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP)\\s*[^|\\d\\s]*)"),
                                    hl7_medical_centre)) %>%
  
  # Replace NA values with empty strings across df
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% 
  
  # Remove extraneous sections from hl7_practice_name
  mutate(
    hl7_practice_name = if_else(provider_number_final == "" | hl7_practice_name == "",
                               hl7_practice_name,
                               str_replace_all(hl7_practice_name, provider_number_final, "")), 
    hl7_practice_name = if_else(Prov_full_name == "" | hl7_practice_name == "",
                                hl7_practice_name,
                                str_replace_all(hl7_practice_name, Prov_full_name, "")),
    hl7_practice_name = if_else(hl7_address == "" | hl7_practice_name == "",
                               hl7_practice_name,
                               str_replace_all(hl7_practice_name, hl7_address, "")),
    hl7_practice_name = if_else(hl7_phone_number == "" | hl7_practice_name == "",
                               hl7_practice_name,
                               str_replace_all(hl7_practice_name, hl7_phone_number, "")),
    
    hl7_practice_name = str_replace_all(hl7_practice_name, "AUSHICPR", ""),
    hl7_practice_name = gsub("[0-9]+", "", hl7_practice_name),  # Remove all numbers
    hl7_practice_name = str_remove_all(hl7_practice_name, "[[:punct:]]"),  # Remove all punctuation
    hl7_practice_name = str_remove_all(hl7_practice_name, "\\b(SHOP|SHP|L|G|UNIT|LEVEL|SUITE|PO BOX)\\b"), #Remove any mention of shops, level, etc
    hl7_practice_name = str_remove_all(hl7_practice_name, "^\\b\\d+\\s*"), # Remove any numbers before practice name
    hl7_practice_name = str_trim(hl7_practice_name),  # Remove leading and trailing whitespace
    # Extract address if dr_address is empty
    hl7_address = ifelse(hl7_address == "", 
                         str_extract(specific_hl7, "\\b\\d+\\s+[A-Z]+\\s+[A-Z]+\\s+[A-Z]+\\s+\\d{4}"), 
                         hl7_address)
  ) %>% 
  
  # Further manual cleaning of the extracted results from functions ####
  mutate(hl7_phone_number = gsub("^03", "", hl7_phone_number), #removing 03 in phone number
         hl7_practice_name = gsub(" UNIT $", "", hl7_practice_name)) %>% #removing UNIT in address
  # mutate(hl7_DR_noted = str_replace_all(hl7_DR_noted, "(?i)(\\sDR)", "")) %>%  # Remove 'DR' and initials
  # mutate(hl7_DR_noted = str_replace_all(hl7_DR_noted, "\\s\\w\\s", " ")) %>% 
  # mutate(hl7_match_switch = sapply(hl7_DR_noted, switch_words), 
  #        hl7_match_switch = str_remove(hl7_match_switch, "^\\s")) %>% 
  mutate(#hl7_address = toupper(hl7_address), #change everything to uppercase to match GPlist
         hl7_address = str_replace_all(hl7_address, "\\bST\\b", "STREET"), #replace ST with STREET
         hl7_address = str_replace_all(hl7_address, "\\bRD\\b", "ROAD"), #replace RD with ROAD
         hl7_address = str_replace_all(hl7_address, "\\bDR\\b", "DRIVE"), #replace DR with DRIVE
         hl7_address = str_replace_all(hl7_address, "\\bDVE\\b", "DRIVE"), #replace DRE with DRIVE
         hl7_address = str_replace_all(hl7_address, "\\bAV\\b", "AVENUE"), #replace AV with AVENUE
         hl7_address = str_replace_all(hl7_address, "\\bAVE\\b", "AVENUE"), #replace AVE with AVENUE
         hl7_address = str_replace_all(hl7_address, "\\bBVD\\b", "BOULEVARD"), #replace BVD with BOULEVARD
         hl7_address = str_replace_all(hl7_address, "\\bCRT\\b", "COURT"), #replace CRT with COURT
         hl7_address = str_replace_all(hl7_address, "\\bPDE\\b", "PARADE"), # replace PDE with PARADE
         hl7_address = str_replace_all(hl7_address, "\\bCL\\b", "CLOSE"), #replace CL with CLOSE
         hl7_address = str_replace_all(hl7_address, "\\bHWY\\b", "HIGHWAY"), #replace HWY with HIGHWAY
         hl7_address = str_replace_all(hl7_address, "\\bCRES\\b", "CRESCENT"), #replace CRES with CRESCENT
         hl7_address = str_trim(hl7_address)) %>%  #remove spaces at the start and end 
  
  # splitting the address to better match with the GP list #### 
  split_address() %>% 
  
  # clean up hl7_practice_name by spelling out short abbreviations
  mutate(hl7_practice_name = str_replace_all(hl7_practice_name, "\\bST\\b", "STREET"), #replace ST with STREET
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bRD\\b", "ROAD"), #replace RD with ROAD
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bDR\\b", "DRIVE"), #replace DR with DRIVE
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bDVE\\b", "DRIVE"), #replace DRE with DRIVE
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bAV\\b", "AVENUE"), #replace AV with AVENUE
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bAVE\\b", "AVENUE"), #replace AVE with AVENUE
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bBVD\\b", "BOULEVARD"), #replace BVD with BOULEVARD
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bCRT\\b", "COURT"), #replace CRT with COURT
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bPDE\\b", "PARADE"), # replace PDE with PARADE
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bCL\\b", "CLOSE"), #replace CL with CLOSE
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bHWY\\b", "HIGHWAY"), #replace HWY with HIGHWAY
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bCRES\\b", "CRESCENT"), #replace CRES with CRESCENT
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bCNTR\\b", "CENTRE"), 
         hl7_practice_name = str_replace_all(hl7_practice_name, "\\bCNT\\b", "CENTRE"), 
         # remove any words from medical practice name that may have been captured in suburb
         dr_suburb_2 = str_remove(dr_suburb_2, ".*\\s\\b(CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MEDICINE|MED|SURGERY|GROUP)\\s"), 
         dr_suburb_2 = str_remove(dr_suburb_2, "^(CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MEDICINE|MED|SURGERY|GROUP)\\s"), 
         # remove duplicated words
         dr_suburb_3 = sapply(dr_suburb_2, function(x) paste(unique(unlist(str_split(x,"\\s"))), collapse = ",")), 
         dr_suburb_3 = str_replace_all(dr_suburb_3, ",", " ")) %>% 
  
  # Extra cleaning of fields to facilitate matching 
  mutate(hl7_practice_name = if_else(hl7_practice_name=="COBURG FAMILY MEDICAL CENTRE CNR SYDNEY ROAD AND BELL STREET COBURG VIC", 
                                     "COBURG FAMILY MEDICAL CENTRE", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="COBURG FAMILY MEDICAL CENTRE CRN SYDNEY ROAD AND BELL STREET COBURG VIC", 
                                     "COBURG FAMILY MEDICAL CENTRE", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="BOXHILL CENTRO MEDICAL CLINIC", 
                                     "BOXHILL CENTRO CLINIC", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="EPPING PLAZA MEDICAL", 
                                     "EPPING PLAZA MEDICAL AND DENTAL CENTRE", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="A CNR ROCHDALE SQ  NEWTON CRESCENT ROCHDALE MEDICAL CENTRE", 
                                     "ROCHDALE MEDICAL CENTRE", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="SOMERTON ROAD MEDCENTRE", 
                                     "SOMERTON ROAD MEDICAL CENTRE", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="MOONEE PONDS MEDICAL CENTRE", 
                                     "MOONEE PONDS MEDICAL AND DENTAL CENTRE", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="PMKTC PRAHRAN MARKET CLINIC PO", 
                                     "PRAHRAN MARKET CLINIC", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="ACCESS HEALTH", 
                                     "ACCESS HEALTH AND COMMUNITY - RICHMOND", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="ACCESS HEALTH  COMM RICHMOND", 
                                     "ACCESS HEALTH AND COMMUNITY - RICHMOND", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="BOX HILL CLINIC", 
                                     "SEXUAL HEALTH VICTORIA", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="THE CLINIC EAST KEW", 
                                     "EAST KEW CLINIC", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="MSHC", 
                                     "MELBOURNE SEXUAL HEALTH CENTRE", hl7_practice_name), 
         # change STREET back to ST
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET ALBANS"), 
                                     "ST ALBANS", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET ANDREWS"), 
                                     "ST ANDREWS", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET JAMES"), 
                                     "ST JAMES", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET JOHN"), 
                                     "ST JOHN", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET KILDA"), 
                                     "ST KILDA", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET LEONARDS"), 
                                     "ST LEONARDS", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET LUKE"), 
                                     "ST LUKE", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET VINCENTS"), 
                                     "ST VINCENTS", hl7_practice_name), 
         # adjust for hospitals
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "ALFRED HOSPITAL"), 
                                     "ALFRED HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "AUSTIN HEALTH"), 
                                     "AUSTIN HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "AUSTIN HOSPITAL"), 
                                     "AUSTIN HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "BOX\\s?HILL HOSPITAL"), 
                                     "BOX HILL HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "BROADMEADOWS HEALTH"), 
                                     "BROADMEADOWS HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "CABRINI HOSPITAL"), 
                                     "CABRINI PRIVATE HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "EPWORTH HOSPITAL"), 
                                     "EPWORTH HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "KNOX .*\\s?HOSPITAL"), # KNOX PRIV(ATE) HOSPITAL
                                     "KNOX PRIVATE HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "MULGRAVE PRIVATE HOSPITAL"), 
                                     "MULGRAVE PRIVATE HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "NORTHERN HOSPITAL"), 
                                     "NORTHERN HEALTH", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "NORTHERN HEALTH"), 
                                     "NORTHERN HEALTH", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "STREET VINCENTS .* HOSPITAL"), 
                                     "ST VINCENTS HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "ST VINCENTS"), 
                                     "ST VINCENTS HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(str_detect(hl7_practice_name, "WARRINGAL HOSPITAL"), 
                                     "WARRINGAL HOSPITAL", hl7_practice_name), 
         hl7_practice_name = if_else(hl7_practice_name=="LATROBE UNI MEDICAL CENTRE", 
                                     "LATROBE UNIVERSITY MEDICAL CENTRE", hl7_practice_name), 
         # clean up/correct phone numbers
         hl7_phone_number = if_else(hl7_phone_number=="90812411", # Greenvale Family Medical Practice
                                    "90812141", hl7_phone_number)
  )


# Matching Process ----
## Join by GP Practice name ####
result_exactmatch_practice <- linelist %>%
  left_join(gplist %>% 
              mutate(general_practice_name_dupe = practice_name), 
            by = c('hl7_practice_name' = 'general_practice_name_dupe')  ) %>% # Initial join based on 'general_practice_name'
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""


## Join by GP address ####
## Based on 'dr_address_split' for rows where 'general_practice_name' didn't match
result_exactmatch_address <- result_exactmatch_practice %>% 
  filter(practice_name == "")%>% 
  select(phess_id, lab, hl7_practice_name, hl7_phone_number, dr_address_split, dr_suburb_3, dr_postcode) %>% 
  left_join(gplist %>% 
              mutate(gplist_address2 = address),
            by = c('dr_address_split' = 'gplist_address2')) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""


## Join by GP phone number ####
## I: Based on phone number and postcode (to avoid clinic branches that may share same phone number) for rows where 'general_practice_name' didn't match
result_exactmatch_phone_poa <- result_exactmatch_address %>% 
  filter(practice_name == "")%>% 
  select(phess_id, lab, hl7_practice_name, hl7_phone_number, dr_address_split, dr_suburb_3, dr_postcode) %>% 
  left_join(gplist %>% 
              mutate(gplist_phone = phone, 
                     gplist_postcode = postcode),
            by = c('hl7_phone_number' = 'gplist_phone', "dr_postcode" = "gplist_postcode")) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""


## II: Based on phone number only (but do not include those that have >=2 clinics for a phone number)
result_exactmatch_phone <- result_exactmatch_phone_poa %>% 
  filter(practice_name == "") %>% 
  filter(! hl7_phone_number %in% gplist_dup_phone) %>% 
  select(phess_id, lab, hl7_practice_name, hl7_phone_number, dr_address_split, dr_suburb_3, dr_postcode) %>% 
  left_join(gplist %>% 
              mutate(gplist_phone = phone, 
                     gplist_postcode = postcode),
            by = c('hl7_phone_number' = 'gplist_phone')) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""


## Combine all match results ####
### Left join the three results on 'phess_id'
result_exactmatch_combined_prelim <- result_exactmatch_practice %>%
  left_join(result_exactmatch_address %>% dplyr::select(phess_id,
                                                   practice_name,
                                                   phone,
                                                   address,
                                                   suburb,
                                                   postcode,
                                                   email),
            by = "phess_id", suffix = c(".gp1", ".address")) %>% 
  left_join(result_exactmatch_phone_poa %>% select(phess_id,
                                                   practice_name,
                                                   phone,
                                                   address,
                                                   suburb,
                                                   postcode,
                                                   email), 
            by = "phess_id") %>% 
  left_join(result_exactmatch_phone %>% select(phess_id, 
                                               practice_name, 
                                               phone, 
                                               address, 
                                               suburb, 
                                               postcode, 
                                               email), 
            by = "phess_id", suffix = c(".telpoa", ".tel")) %>% 
  mutate(practice_name = ifelse(practice_name.gp1 == "" & practice_name.address!="",
                                practice_name.address,
                                practice_name.gp1), 
         practice_name = ifelse(practice_name == "" & practice_name.telpoa!="", 
                                practice_name.telpoa, 
                                practice_name), 
         practice_name = ifelse(practice_name == "" & practice_name.tel!="", 
                                practice_name.tel, 
                                practice_name), 
         phone = ifelse(phone.gp1 == "" & phone.address!="", phone.address, phone.gp1), 
         phone = ifelse(phone =="" & phone.telpoa!="", phone.telpoa, phone), 
         phone = ifelse(phone =="" & phone.tel!="", phone.tel, phone), 
         address = ifelse(address.gp1 == "" & address.address!="", address.address, address.gp1), 
         address = ifelse(address =="" & address.telpoa!="", address.telpoa, address), 
         address = ifelse(address =="" & address.tel!="", address.tel, address), 
         suburb = ifelse(suburb.gp1 == "" & suburb.address!="", suburb.address, suburb.gp1), 
         suburb = ifelse(suburb =="" & suburb.telpoa!="", suburb.telpoa, suburb), 
         suburb = ifelse(suburb =="" & suburb.tel!="", suburb.tel, suburb), 
         postcode = ifelse(postcode.gp1 == "" & postcode.address!="", postcode.address, postcode.gp1), 
         postcode = ifelse(postcode =="" & postcode.telpoa!="", postcode.telpoa, postcode), 
         postcode = ifelse(postcode =="" & postcode.tel!="", postcode.tel, postcode), 
         email = ifelse(email.gp1 == "" & email.address!="", email.address, email.gp1), 
         email = ifelse(email =="" & email.telpoa!="", email.telpoa, email), 
         email = ifelse(email =="" & email.tel!="", email.tel, email)) %>% 
  dplyr::select(-ends_with(".gp1"), -ends_with(".address"), -ends_with(".telpoa"), -ends_with(".tel")) %>% 
  rename(gplist_email = email,
         gplist_practicename = practice_name,
         gplist_phone = phone, 
         gplist_address = address) %>% 
  #replace NA with ""
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) 

# resolving duplicate rows on PHESS ID (due to multiple phone numbers for practice name because for eg: branches)

## Identify duplicate rows on PHESS ID
exactmatch_dup <- result_exactmatch_combined_prelim %>% 
  filter(duplicated(select(., phess_id))) %>% 
  arrange(as.numeric(phess_id))

## extract duplicated rows from the matched df
exactmatch_duplicaterows <- result_exactmatch_combined_prelim %>% 
  filter(phess_id %in% exactmatch_dup$phess_id)

## filter out the accepted row from duplicate rows using GP phone number
exactmatch_duplicaterows_accept <- exactmatch_duplicaterows %>% 
  mutate(dup_check = if_else(hl7_phone_number == gplist_phone, "ACCEPT", "REJECT")) %>% 
  filter(dup_check=="ACCEPT")

exactmatch_duplicaterows_either <- exactmatch_dup %>% 
  filter(! phess_id %in% exactmatch_duplicaterows_accept$phess_id)

exactmatch_duplicaterows_resolve <- bind_rows(exactmatch_duplicaterows_accept, exactmatch_duplicaterows_either) %>% 
  select(-dup_check) %>% 
  arrange(as.numeric(phess_id))
  

# replace duplicated rows with resolved rows
result_exactmatch_combined <- result_exactmatch_combined_prelim %>% 
  filter(! phess_id %in% exactmatch_duplicaterows_resolve$phess_id) %>% 
  bind_rows(exactmatch_duplicaterows_resolve) %>% 
  arrange(event_date)


# Final matches result and match stats ----
gpdetails_table <- result_exactmatch_combined %>% 
  # select key variables
  select(phess_id, 
         gplist_practicename, gplist_email, gplist_address, suburb, postcode, 
         Prov_first_name, Prov_last_name, Prov_full_name, Prov_provider_num)


# if you want to perform fuzzy matching as well, uncomment the next line and run it
# source(here("Code", "FuzzyMatching.R"))


# Process final table for incorporation into Esther's data ----
final_table <- linelist %>% 
  select(phess_id, condition, first_name, last_name, birth_date, 
         event_date, investigation_status, re_investigation_status) %>% 
  left_join(gpdetails_table, by="phess_id") %>% 
  # rename fields
  rename(gplist_suburb = suburb, 
         gplist_postcode = postcode) %>% 
  # arrange with matched rows at top by phess_id
  mutate(section = if_else(gplist_practicename=="", 2, 1)) %>% 
  # Convert first and last name to 2by2 name field
  mutate(name2by2 = paste0(str_extract(first_name, "\\w{2}"), str_extract(last_name, "\\w{2}"))) %>% 
  # convert DoB and event_date fields from date-time to date only field as text string
  mutate(birth_date = lubridate::ymd(birth_date)) %>% 
  mutate(dob_str = as.character(birth_date)) %>% 
  mutate(event_date = as.character(lubridate::ymd(event_date))) %>% 
  # change case for dr name
  mutate(dr_firstname_final = str_to_sentence(Prov_first_name), 
         dr_lastname_final = str_to_title(Prov_last_name), 
         dr_fullname_final = str_to_title(Prov_full_name)) %>% 
  # rename provider number variable
  rename(provider_number_final = Prov_provider_num) %>% 
  # rearrange variables
  select(phess_id, condition, name2by2, dob_str, event_date, investigation_status, re_investigation_status, 
         gplist_practicename, gplist_email, gplist_address, gplist_suburb, gplist_postcode, 
         dr_firstname_final, dr_lastname_final, dr_fullname_final, provider_number_final, 
         section) %>% 
  arrange(section, phess_id)

writexl::write_xlsx(final_table, here("Output", "final_table.xlsx"))
#length(unique(final_table$phess_id))

# export file to HP folder for next stage - integration into tracker. Save file name as YYYY-MM-DD-finaltable.xlsx - Carried out in ProcessRun.R file
# one_drive_folder <- str_extract(getwd(), "^.*Health/")
# HP_folder <- paste0(one_drive_folder, "Communicable Disease Secure/Other Docs/GP Extract Lists (Automated - Do Not Edit) test")
# 
# writexl::write_xlsx(final_table, paste0(HP_folder, "/", Sys.Date(), "-finaltable.xlsx"))
