# MAIN EXTRACTION AND MATCHING CODE

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
## Load NEPHU Case Linelist file ----
# nephu_linelist_file <- "NEPHUCaseLinelistGPExtractionAssignedLPHU_Jan2024May2025.xlsx" # Combined files 1 Jan 2024 - 31 May 2025 Assigned LPHU
# nephu_linelist_file <- "NEPHUCaseLinelistGPExtractionAssignedLPHU_1-17Jun2025.xlsx" 
 nephu_linelist_file <- "NEPHUCaseLinelistGPExtractionAssignedLPHU_18Jun-6Jul.xlsx" 
nephu_linelist_file <- "NEPHUCaseLinelistGPExtractionAssignedLPHU_7-20Jul2025.xlsx" 


linelist.raw <- readxl::read_xlsx(here::here("Data", nephu_linelist_file)) %>% clean_names() # use NEPHU extract

# saveRDS(linelist.raw, here("Data", "phessextract_Jan2023Aug2024.rds"))


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
         Prov_clinic = if_else(!lab1 %in% provider_config2, 
                             str_split(provider1, "\\|", simplify = TRUE)[, 2], 
                             str_split(provider1, "\\|", simplify = TRUE)[, 3])) %>% 
  mutate(Prov_first_name = str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,1]), 
         Prov_last_name = str_extract(Prov_full_name, "\\b\\w+$"), # str_trim(str_split(Prov_full_name, "\\s+", simplify=TRUE)[,2]), 
         Prov_clinic = str_trim(Prov_clinic, side="both"), 
         Prov_provider_num = str_extract(provider1, "\\b\\d\\S{6}[a-zA-Z]$"))

# Note: can probably remove all Prov_* fields since not using them for the matching process.  Use the HL7-derived ones

# Clean up HL7 field ----
## Reduce HL7 field to only ORC and PV1 lines
hl7.clean <- linelist %>% 
  rowwise() %>% 
  mutate(reduce_hl7 = list(modify_HL7_message(elr_hl7_message))) %>% 
  unnest(reduce_hl7) %>% 
  ungroup() %>% 
  # remove duplicate rows created from above function
  distinct(phess_id, .keep_all=TRUE)

## Further cleaning of HL7 field to remove unnecessary symbols
hl7.clean$reduce_hl7 <- hl7.clean$reduce_hl7 %>%
  { 
    if ("PV1" %in% .) {
      .[grep("^PV1\\|", .):length(.)] %>%
        paste(collapse = "\n") %>%
        str_replace_all("\\^+", " ") %>%
        str_replace_all("\\|+", "|")
    } else {
      .
    }
  } %>%
  str_replace_all("\\^+", " ") %>%  # This replaces multiple occurrences of ^ with a space " "
  str_replace_all("\\|+", "|") %>%  # This replaces multiple occurrences of | with a single  |
  str_replace_all("[\\[\\]()]", "")  # This removes all brackets


# Extract Provider number from HL7 field ----
## hl7 extracted provider number may differ from provider field provider number. 
## This is due to hospital or lab follow-up which replace the GP number.
## Hence, retain the hl7 provider number as final provider number
hl7.clean_prov <- hl7.clean %>% 
  mutate(hl7_provider_number = str_extract(reduce_hl7, pattern_provider), 
         provider_number_final = ifelse(!is.na(hl7_provider_number), hl7_provider_number, NA))

# Extract Dr name from HL7 field ----
## Create doctor column based off provider number from HL7 field
## hl7_DR_noted - pulled Dr name from hl7 based on pattern: PR#, name, DR
## Code below does not pick up Dr name details from VIDRL lab entries
## For Dorevitch lab entries, Dr name details may contain 4 alpha-numeric code before DR
hl7.clean_dr <- hl7.clean_prov
if (!all(is.na(hl7.clean_dr$provider_number_final))) {
  hl7.clean_dr$provider_number_final <- str_replace(hl7.clean_dr$provider_number_final, "\\|", "") # Remove the initial pipe if needed
  
  pattern_DR <- paste0(hl7.clean_dr$provider_number_final, " (.*?)[^DR\\w](?i)(DR|A\\/PR|\\|)")
  
  # Step 3: Extract the required text
  hl7.clean_dr$hl7_DR_noted <- str_extract(hl7.clean_dr$reduce_hl7, pattern_DR)
  
  # Clean up the extracted text to remove the provider number itself
  hl7.clean_dr$hl7_DR_noted <- if_else(!is.na(hl7.clean_dr$hl7_DR_noted), str_replace(hl7.clean_dr$hl7_DR_noted, hl7.clean_dr$provider_number_final, ""), NA)
  
  # Clean up the extracted text by removing | and trim off white spaces
  hl7.clean_dr$hl7_DR_noted <- str_trim(str_replace(hl7.clean_dr$hl7_DR_noted, "\\|", ""), side="both")
}

## For all else where above code does not extract Dr name
hl7.clean_dr <- hl7.clean_dr %>% 
  mutate(hl7_DR_noted = if_else(is.na(hl7_DR_noted), str_extract(reduce_hl7, "(?<=\\|)[^|]+\\sDR\\b"), hl7_DR_noted), 
         # remove DR at the end
         hl7_DR_noted2 = str_remove(hl7_DR_noted, "\\b\\sDR$"), 
         # remove the 3-4 letter code at end of name for Dorevitch Lab entries and PRN~ codes for Eastern Path Lab and PRN PID codes for VIDRL
         hl7_DR_noted2 = if_else(lab1=="Dorevitch Pathology", str_remove(hl7_DR_noted2, "\\b\\w{3,4}$"), hl7_DR_noted2), 
         hl7_DR_noted2 = if_else(lab1=="Eastern Pathology", str_remove(hl7_DR_noted2, "^PRN\\~\\S+\\b\\s"), hl7_DR_noted2), 
         hl7_DR_noted2 = if_else(lab1=="Victorian Infectious Diseases Reference Laboratory", str_remove(hl7_DR_noted2, "^PRN\\sPID\\|.+\\~\\sPN\\|"), hl7_DR_noted2), 
         # switch words [surname][firstname] in name field
         dr_fullname = switch_words((hl7_DR_noted2)), 
         dr_fullname = if_else(str_detect(dr_fullname, 
                                                regex("HOSPITAL|LABORATORY|PATHOLOGY|LAB|MSHC|Path|Melbourne|Document|Department|
                                                Unknown|Form|Request|C/-|Coordinator|Nurse|Study|Doctor|A/Prof|Missing|Uncoded|
                                                Address|Council|Environmental|Team|Leader|BUPA", 
                                                      ignore_case=TRUE)), 
                                     NA_character_, 
                                     dr_fullname), 
         # replace as NA if name contains numbers
         dr_fullname = if_else(str_detect(dr_fullname, "\\d+"), NA_character_, dr_fullname)
         ) 

# hl7.clean2 <- hl7.clean %>% 
#   mutate(
#     # remove DR at the end
#     hl7_DR_noted2 = str_remove(hl7_DR_noted, "\\b\\sD[Rr]$"), 
#     # remove the 3-4 letter code at end of name for Dorevitch Lab entries and PRN~ codes for Eastern Path Lab
#     hl7_DR_noted2 = if_else(lab1=="Dorevitch Pathology", str_remove(hl7_DR_noted2, "\\b\\w{3,4}$"), hl7_DR_noted2), 
#     hl7_DR_noted2 = if_else(lab1=="Eastern Pathology", str_remove(hl7_DR_noted2, "^PRN\\~\\S+\\b\\s"), hl7_DR_noted2)
#     ) %>% 
#   #select(-error_hl7) %>% 
#   relocate(reduce_hl7, .before=lab1) %>% 
#   mutate(name_switch = switch_words((hl7_DR_noted2)))

# Finalise Dr name fields
hl7.clean_dr <- hl7.clean_dr %>% 
  #mutate(dr_match_status = if_else(hl7_DR_noted=Prov_full_name, "Match", "No Match")) %>% 
  mutate(dr_firstname_final = str_trim(str_split(dr_fullname, "\\s+", simplify=TRUE)[,1]), 
         dr_lastname_final = str_remove(dr_fullname, "^\\S+\\b\\s"),  
         dr_lastname_final = extract_last_name(dr_lastname_final), 
         dr_fullname_final = paste(dr_firstname_final, dr_lastname_final, recycle0=TRUE), 
         dr_fullname_final = if_else(dr_fullname_final=="NA NA", NA_character_, dr_fullname_final))

# test <- hl7.clean_dr %>% 
#   relocate(Prov_full_name, .after=dr_fullname_final)
# 
# numeric_names <- test %>% 
#   filter(str_detect(dr_fullname_final, "\\d+")) %>% 
#   mutate(resolve = str_remove(hl7_DR_noted, "[:alnum:]{4}\\d{2}[:alnum:]{2}\\s"), 
#          resolve = if_else(lab1=="Victorian Infectious Diseases Reference Laboratory", str_remove(hl7_DR_noted, "PRN.+\\~.+?\\|"), resolve))
# quick_excel(hl7.clean %>% select(-elr_hl7_message), "hl7_clean.xlsx")
# 
# error_hl7 <- hl7.clean %>% 
#   mutate(error = if_else(str_detect(reduce_hl7, "DETECTED"), "ERROR", NA_character_)) %>% 
#   filter(error=="ERROR")
# 
# quick_excel(error_hl7 %>% select(-elr_hl7_message), "error_hl7.xlsx")


# Extract Practice name, Address and Phone number from HL7 field ----
## further formatting to enhance data for use with practice name and address extraction
hl7.clean_clinic <- hl7.clean_dr %>% 
  mutate(
    # detect error in hl7 field that will not be able to be processed in next step)
    error_hl7 = if_else(str_detect(reduce_hl7, "DETECTED"), "ERROR", NA_character_), 
    # capitalise all letters
    reduce_hl7 = toupper(reduce_hl7), 
    # replace all short and abbreviated words with full spelling
    reduce_hl7 = str_replace_all(reduce_hl7, "\\s\\bCTRE\\b", "CENTRE"), 
    reduce_hl7 = str_replace_all(reduce_hl7, "\\s\\bCTR\\b", "CENTRE"), 
    reduce_hl7 = str_replace_all(reduce_hl7,"AUSHIC\\sMC",""), 
    reduce_hl7 = str_replace_all(reduce_hl7,"\\sM/C"," MEDICAL CENTRE"), 
    reduce_hl7 = str_replace_all(reduce_hl7,"\\sMC", " MEDICAL CENTRE"), 
    reduce_hl7 = str_replace_all(reduce_hl7,"Y PUBLIC HEALTH","")) %>% 
  filter(is.na(error_hl7))

#quick_csv(hl7.clean, "hl7.clean.csv")

hl7.clean_clinic <- hl7.clean_clinic %>% 
  select(-error_hl7) %>% 
  #mutate(clean_HL7 = str_remove(clean_HL7,provider_number_final)) %>% 
  rowwise() %>%
  mutate(
    hl7_phone_number = extract_first_phone_number(reduce_hl7), #phone clean function
    hl7_address = extract_address(reduce_hl7), #address clean function
    hl7_medical_centre = extract_medical_centre(reduce_hl7) #medical centre clean funtion
  ) %>%
  ungroup() %>%
  # Remove long HL7 message column 
  dplyr::select(-c(elr_hl7_message)) %>%
  # Update medical centres to remove merged strings - AUSHICPR, DR_noted, AUS M, etc.
  mutate(hl7_medical_centre = str_replace_all(hl7_medical_centre, "AUSHICPR", "") %>%
           str_replace_all("DR_noted", "") %>%
           str_replace_all("AUS M", "") %>% 
           str_replace_all("\\\\T\\\\", "AND")) %>%
  # doing a second extract on GPlist_GPname if medical centre field is empty
  mutate(hl7_practice_name = ifelse(hl7_medical_centre == "",
                              str_extract(reduce_hl7, "(?<=\\|)\\s*([^|]*\\b(?:CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP)\\s*[^|\\d\\s]*)"),
                              hl7_medical_centre)) %>%
  # Replace NA values with empty strings
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% 
  # Remove extraneous sections from hl7_practice_name
  mutate(
    hl7_practice_name = ifelse(provider_number_final == "" | hl7_practice_name == "",
                         hl7_practice_name,
                         str_replace_all(hl7_practice_name, provider_number_final, "")),
    hl7_practice_name = ifelse(hl7_address == "" | hl7_practice_name == "",
                         hl7_practice_name,
                         str_replace_all(hl7_practice_name, hl7_address, "")),
    hl7_practice_name = ifelse(hl7_phone_number == "" | hl7_practice_name == "",
                         hl7_practice_name,
                         str_replace_all(hl7_practice_name, hl7_phone_number, "")),
    hl7_practice_name = ifelse(hl7_DR_noted == "" | hl7_practice_name == "",
                         hl7_practice_name,
                         str_replace_all(hl7_practice_name, hl7_DR_noted, "")),
    hl7_practice_name = str_replace_all(hl7_practice_name, "AUSHICPR", ""),
    hl7_practice_name = gsub("[0-9]+", "", hl7_practice_name),  # Remove all numbers
    hl7_practice_name = str_remove_all(hl7_practice_name, "[[:punct:]]"),  # Remove all punctuation
    hl7_practice_name = str_remove_all(hl7_practice_name, "\\b(SHOP|SHP|L|G|UNIT|LEVEL|SUITE|PO BOX)\\b"), #Remove any mention of shops, level, etc
    hl7_practice_name = str_remove_all(hl7_practice_name, "^\\b\\d+\\s*"), # Remove any numbers before practice name
    hl7_practice_name = str_trim(hl7_practice_name),  # Remove leading and trailing whitespace
    # Extract address if dr_address is empty
    hl7_address = ifelse(hl7_address == "", 
                         str_extract(reduce_hl7, "\\b\\d+\\s+[A-Z]+\\s+[A-Z]+\\s+[A-Z]+\\s+\\d{4}"), 
                         hl7_address)
  )


## further manual cleaning of the results of the functions ####
hl7.clean_ext <- hl7.clean_clinic %>% 
  mutate( hl7_phone_number = gsub("^03", "", hl7_phone_number), #removing 03 in phone number
          hl7_practice_name = gsub(" UNIT $", "", hl7_practice_name)) %>% #removing UNIT in address
  # mutate(hl7_DR_noted = str_replace_all(hl7_DR_noted, "(?i)(\\sDR)", "")) %>%  # Remove 'DR' and initials
  # mutate(hl7_DR_noted = str_replace_all(hl7_DR_noted, "\\s\\w\\s", " ")) %>% 
  # mutate(hl7_match_switch = sapply(hl7_DR_noted, switch_words), 
  #        hl7_match_switch = str_remove(hl7_match_switch, "^\\s")) %>% 
  mutate(hl7_address = toupper(hl7_address), #change everything to uppercase to match GPlist
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
         hl7_address = str_trim(hl7_address)) #remove spaces at the start and end


## splitting the address to better match with the GP list ####
result_split_address <- split_address(hl7.clean_ext) %>% 
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
         dr_suburb_3 = str_replace_all(dr_suburb_3, ",", " "))

# Extra cleaning of fields to facilitate matching
result_split_address <- result_split_address %>% 
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
result_exactmatch_practice <- result_split_address %>%
  left_join(gplist %>% 
              mutate(general_practice_name_dupe = practice_name), 
            by = c('hl7_practice_name' = 'general_practice_name_dupe')  ) %>% # Initial join based on 'general_practice_name'
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

#tabyl(result_exactmatch_practice, practice_name)

## Join by GP address ####
## Based on 'dr_address_split' for rows where 'general_practice_name' didn't match
result_exactmatch_address <- result_exactmatch_practice %>% 
  filter(practice_name == "")%>% 
  select(phess_id, lab, hl7_practice_name, hl7_phone_number, dr_address_split, dr_suburb_3, dr_postcode) %>% 
  left_join(gplist %>% 
              mutate(gplist_address2 = address),
            by = c('dr_address_split' = 'gplist_address2')) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

#tabyl(result_exactmatch_address, practice_name)

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

#tabyl(result_exactmatch_phone_poa, practice_name)

## II: Based on phone number only (but do not include those that have >=2 clinics for a phone number)
#dup_phone_numbers <- c("98103000", "95251941", "1300342255", "95091811", "94350711", "98416111", "83734646", "94193000")

result_exactmatch_phone <- result_exactmatch_phone_poa %>% 
  filter(practice_name == "") %>% 
  filter(! hl7_phone_number %in% gplist_dup_phone) %>% 
  select(phess_id, lab, hl7_practice_name, hl7_phone_number, dr_address_split, dr_suburb_3, dr_postcode) %>% 
  left_join(gplist %>% 
              mutate(gplist_phone = phone, 
                     gplist_postcode = postcode),
            by = c('hl7_phone_number' = 'gplist_phone')) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

#tabyl(result_exactmatch_phone, practice_name)

## Combine all match results ####
## Left join the three results on 'phess_id'
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
  # rename(practice_name.phone = practice_name, 
  #        phone.phone = phone, 
  #        address.phone = address, 
  #        suburb.phone = suburb, 
  #        postcode.phone = postcode, 
  #        email.phone = email) %>% 
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

# Check for duplicate PHESS ID (due to one to many joins)
length(unique(result_exactmatch_combined_prelim$phess_id))

exactmatch_dup <- result_exactmatch_combined_prelim %>% 
  filter(duplicated(select(., phess_id))) %>% 
  arrange(as.numeric(phess_id))

length(unique(exactmatch_dup$phess_id))

exactmatch_duplicaterows <- result_exactmatch_combined_prelim %>% 
  filter(phess_id %in% exactmatch_dup$phess_id)

exactmatch_duplicaterows_accept <- exactmatch_duplicaterows %>% 
  mutate(dup_check = if_else(hl7_phone_number == gplist_phone, "ACCEPT", "REJECT")) %>% 
  filter(dup_check=="ACCEPT")

exactmatch_duplicaterows_either <- exactmatch_dup %>% 
  filter(! phess_id %in% exactmatch_duplicaterows_accept$phess_id)

exactmatch_duplicaterows_resolve <- bind_rows(exactmatch_duplicaterows_accept, exactmatch_duplicaterows_either) %>% 
  select(-dup_check) %>% 
  arrange(as.numeric(phess_id))
  
# all.equal(exactmatch_dup, exactmatch_duplicaterows_resolve)

# replace duplicated rows with resolved rows
result_exactmatch_combined <- result_exactmatch_combined_prelim %>% 
  filter(! phess_id %in% exactmatch_duplicaterows_resolve$phess_id) %>% 
  bind_rows(exactmatch_duplicaterows_resolve) %>% 
  arrange(event_date)

#tabyl(result_exactmatch_combined, gplist_practicename)

# Fuzzy Join ----
## Start fuzzy matching rows that did not join exact in above

## Fuzzy match on both Practice name **AND** Address ####
## Note: fuzzyjoin::stringdist_ function is not very reliable - create many inaccurate matches. Therefore use 2 fields to join
## Use the fuzzyjoin:: regex_ function to fuzzy join on 1 field
result_fuzzyjoin <- result_exactmatch_combined %>%
  filter(gplist_practicename == "") %>% 
  filter(dr_state=="VIC" | dr_state=="") %>% 
  dplyr::select(-c(gplist_practicename,
                   gplist_phone,
                   gplist_address,
                   suburb,
                   postcode,
                   gplist_email)) %>%
  fuzzyjoin::stringdist_left_join(
    gplist, 
    by = c('hl7_practice_name' = 'practice_name', 'dr_address_split' = 'address'),
    method = 'jw',  # Jaro-Winkler distance
    max_dist = 0.2,  # Adjust the maximum distance threshold as needed
    ignore_case = TRUE
  ) 

#tabyl(result_fuzzyjoin, practice_name)
#quick_excel(result_fuzzyjoin, "fuzzyjoin_practice_address.xlsx")

## Proceed to fuzzy join on Practice name and Address separately in 2 steps with the regex_function

## Fuzzy match on Practice_name ####
result_fuzzyjoin_gp <- result_exactmatch_combined %>%
  filter(gplist_practicename == "") %>% 
  dplyr::select(-c(gplist_practicename,
                   gplist_phone,
                   gplist_address,
                   suburb,
                   postcode,
                   gplist_email)) %>%
  fuzzyjoin::regex_left_join(
    gplist, 
    by = c('hl7_practice_name' = 'practice_name')
  ) %>% 
  ungroup()
#tabyl(result_fuzzyjoin_gp, practice_name)

## Second fuzzy match on Address using the unmatched rows from previous join result_fuzzyjoin_gp ####
result_fuzzyjoin_address <- result_fuzzyjoin_gp %>% 
  filter(is.na(practice_name)) %>% 
  dplyr::select(-c(practice_name,
                   phone,
                   address,
                   suburb,
                   postcode,
                   email)) %>% 
  fuzzyjoin::regex_left_join(
    gplist,
    by = c('dr_address_split' = 'address')
  ) %>%
  ungroup()

#tabyl(result_fuzzyjoin_address, practice_name)

## Combine fuzzy join results ####
result_fuzzyjoin_combined <- result_fuzzyjoin %>% 
  filter(!is.na(practice_name)) %>% 
  rows_append(result_fuzzyjoin_gp %>% filter(!is.na(practice_name))) %>% 
  rows_append(result_fuzzyjoin_address %>% filter(!is.na(practice_name))) %>% 
  mutate(fuzzyjoin="Yes") %>% 
  mutate(practice_match = stringdist::stringsim(hl7_practice_name, practice_name), 
         address_match = stringdist::stringsim(dr_address_split_2, address), 
         phone_match = stringdist::stringsim(hl7_phone_number, phone)) %>% 
  mutate(ave_match = rowMeans(select(., practice_match, address_match, phone_match), na.rm=T)) %>% 
  rename(gplist_email = email,
         gplist_practicename = practice_name,
         gplist_phone = phone, 
         gplist_address = address) %>% 
  arrange(desc(ave_match))

quick_excel(result_fuzzyjoin_combined, "result_fuzzyjoin_combined.xlsx")

# Final matches result and match stats ----
## combine exact and fuzzy join results
result_final_matching <- result_exactmatch_combined %>% 
  filter(!phess_id %in% result_fuzzyjoin_combined$phess_id) %>% 
  mutate(fuzzyjoin=NA_character_, 
         practice_match = NA_real_, 
         address_match=NA_real_, 
         phone_match=NA_real_,
         ave_match=NA_real_) %>% 
  rows_append(result_fuzzyjoin_combined)

quick_excel(result_final_matching, "final_matching_result.xlsx")


# Process final table for incorporation into Esther's data ----
## This step needs to be manually checked before compiling so as to code in the fuzzy matched rows to be excluded, normally = 0.32
fuzz_cutoff <- 0.71

gpdetails_table <- result_final_matching %>% 
  # replace gp details wtih NA for fuzzy rows that are less than fuzz_cutoff value
  mutate(gplist_practicename = if_else(((fuzzyjoin=="Yes" & ave_match > fuzz_cutoff) | is.na(fuzzyjoin)), 
                                       gplist_practicename, NA_character_), 
         gplist_email = if_else(((fuzzyjoin=="Yes" & ave_match > fuzz_cutoff) | is.na(fuzzyjoin)), 
                                gplist_email, NA_character_), 
         gplist_address = if_else(((fuzzyjoin=="Yes" & ave_match > fuzz_cutoff) | is.na(fuzzyjoin)), 
                                  gplist_address, NA_character_), 
         suburb = if_else(((fuzzyjoin=="Yes" & ave_match > fuzz_cutoff) | is.na(fuzzyjoin)), 
                          suburb, NA_character_), 
         postcode = if_else(((fuzzyjoin=="Yes" & ave_match > fuzz_cutoff) | is.na(fuzzyjoin)), 
                            postcode, NA_character_)
  ) %>% 
  # remove rows from fuzzy join that are incorrect - ave_match score <0.32
  # filter((fuzzyjoin=="Yes" & ave_match>=0.32)|is.na(fuzzyjoin)) %>% 
  # replace practice_name = Moores Road Medical Centre with Somerton Road Medical Centre details
  # mutate(gplist_practicename = if_else(fuzzyjoin=="Yes" & gplist_practicename=="MOORES ROAD MEDICAL CENTRE", 
  #                                      "SOMERTON ROAD MEDICAL CENTRE", 
  #                                      gplist_practicename), 
  #        gplist_email = if_else(fuzzyjoin=="Yes" & gplist_practicename=="MOORES ROAD MEDICAL CENTRE", 
  #                               "info@srmc.net.au", 
  #                               gplist_email)) %>% 
  # select key variables
  select(phess_id, 
         gplist_practicename, gplist_email, gplist_address, suburb, postcode, 
         dr_firstname_final, dr_lastname_final, dr_fullname_final, provider_number_final)


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
  mutate(dr_firstname_final = str_to_sentence(dr_firstname_final), 
         dr_lastname_final = str_to_title(dr_lastname_final), 
         dr_fullname_final = str_to_title(dr_fullname_final)) %>% 
  # rearrange variables
  select(phess_id, condition, name2by2, dob_str, event_date, investigation_status, re_investigation_status, 
         gplist_practicename, gplist_email, gplist_address, gplist_suburb, gplist_postcode, 
         dr_firstname_final, dr_lastname_final, dr_fullname_final, provider_number_final, 
         section) %>% 
  arrange(section, phess_id)


writexl::write_xlsx(final_table, here("Output", "final_table_7-20Jul2025.xlsx"))

# test_table <- final_table %>% 
#   filter(condition %in% c("Gonococcal infection", "Chlamydia trachomatis infection", "Syphilis - Late")) %>% 
#   head(20) 
# 
# writexl::write_xlsx(test_table, here("Output", "test_table.xlsx"))

one_drive_folder <- str_extract(getwd(), "^.*Health/")
HP_folder <- paste0(one_drive_folder, "Communicable Disease Secure/Other Docs/GP Extract Lists (Automated - Do Not Edit) test")

# export file to HP folder for next stage - integration into tracker. Save file name as YYYY-MM-DD-DrTable.xlsx
writexl::write_xlsx(final_table, paste0(HP_folder, "/", Sys.Date(), "-finaltable.xlsx"))


# Non-matches

non_match <- result_final_matching %>% 
  filter(gplist_practicename=="") %>% 
  arrange(hl7_medical_centre)

quick_excel(non_match, "Non-matching.xlsx")

fuzzy_non_match <- result_fuzzyjoin_combined %>% 
  filter(ave_match < 0.34)

quick_excel(fuzzy_non_match, "fuzzy_non-matching.xlsx")


# test of if_else 

ifelse_test <- result_final_matching %>% 
  # replace gp details wtih NA for fuzzy rows that are less than fuzz_cutoff value
  mutate(gplist_test = if_else(fuzzyjoin=="Yes" & ave_match < fuzz_cutoff, 
                                       NA_character_, gplist_practicename 
                                       #, missing=gplist_practicename
                               )) %>% 
  select(phess_id, gplist_practicename, gplist_test, fuzzyjoin, ave_match)
