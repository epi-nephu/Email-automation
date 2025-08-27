# NEPHU Testing Area
# This script is use to test WPHU code on NEPHU or test data

# Load Libaries
library(here)
library(tidyverse)
library(janitor)
# library(keyring)
# library(odbc)
# library(officer)
# library(stringdist)
# library(stringr)
# library(readxl)
# library(tinytex)
# library(doconv)
# library(fuzzyjoin)
# library(openxlsx)


# test the HL7 cleaning function and matching function
# Load PHESS extract sample data (with HL7 field)
phess_extract_file <- "AutomategonoESFrequests_20240911094903.xlsx"
nephu_linelist_file <- "NEPHUCaseLinelist.xlsx" # extracted from PHESS 28 Jan 2025

#extract <- readxl::read_xlsx(here::here("Data", phess_extract_file)) %>% clean_names() # use WPHU extract
extract <- readxl::read_xlsx(here::here("Data", nephu_linelist_file)) %>% clean_names() # use NEPHU extract

saveRDS(extract, here("Data", "phessextract_9785obs.rds"))

extract <- readRDS(here("Data", "phessextract_9785obs.rds"))
range(extract$event_date)

# quick function to write df to Excel sheet for in-depth viewing
quick_excel <- function(df, outfile="testoutput.xlsx") {
  writexl::write_xlsx(df, here("Output", "Test", outfile))
}

quick_csv <- function(df, outfile="testoutput.csv") {
  write.csv(df, here("output", "Test", outfile))
}

# Function taken from WPHU code (1. Cleaning.R)
clean_HL7_message <- function(hl7_message) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  obr_index <- which(grepl("OBR", lines))
  if (length(obr_index) > 0) {
    cleaned_lines <- lines[seq_len(obr_index - 1)]
  } else {
    cleaned_lines <- lines
  }
  paste(cleaned_lines, collapse = "\n")
}

# Function to reduce HL7 field to single line containing GP details (modified from clean_HL7_message function)
reduce_HL7_message <- function(hl7_message) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  obr_index <- which(grepl("OBR", lines))
  if (length(obr_index) > 0) {
    cleaned_lines <- lines[obr_index-1: obr_index] # take in PV1 line (for Dorevitch) and OBR line
  } else {
    cleaned_lines <- lines
  }
  paste(cleaned_lines, collapse = "\n")
}

 

orc_HL7_message <- function(hl7_message) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  orc <- lines[which(grepl("ORC\\|", lines))]
  paste(orc, collapse="\n")
 
}


test <- ext_data %>% 
  select(phess_id, condition, lab, elr_hl7_message) %>% 
  slice(26:35)

test1 <- test %>% 
  mutate(lines = str_split(elr_hl7_message, "\n"))

test2 <- test %>% 
  mutate(lines = str_split(elr_hl7_message, "\n") %>% unlist())

# function applied onto vector
test2 <- str_split(test$elr_hl7_message, "\n") %>% unlist()
test2_index <- which(grepl("OBR", test2))
clean_test2_index <- test2[test2_index-1: test2_index]

# apply new modified function
# function creates new rows when lab field has multiple entries <- need to remove duplicate rows
test3 <- test %>% 
  rowwise() %>% 
  mutate(modified_hl7 = list(modify_HL7_message(elr_hl7_message))) %>% 
  unnest(modified_hl7) %>% 
  ungroup() 

quick_csv(test3, "modifiyHL7_test3.csv")


# provider field configurations
# config1: Full name | Medical centre | Provider number
# config2: First name | Last name | Medical centre | Provider number

provider_config2 <- c("Melbourne Pathology", "Royal Children's Hospital Pathology")

# subset extract data into first 500 lines
ext_data <- extract[1:500,] %>% 
  filter(case_found_by=="Clinical Presentation") %>% 
  filter(event_classification %in% c("Confirmed", "Probable", "Suspected", "At risk")) %>% 
  filter(!is.na(elr_hl7_message)) %>% 
  rename(lab = notifying_laboratory, 
         provider = referring_provider_details) %>% 
  select(phess_id, condition, full_name, birth_date, lab, provider, elr_hl7_message) %>% 
  # remove subsequent entries from lab and provider fields - retain only first entry
  mutate(lab1 = if_else(str_detect(lab, ".+;.+"), stringr::word(lab, sep=";"), lab),  
         #provider1 = if_else(str_detect(provider, ".+;.+"), str_extract(provider, "^.+(?=;)"), provider), 
         provider1 = if_else(str_detect(provider, ".+;.+"), stringr::word(provider, sep=";"), provider)) %>% 
  mutate(Dr_full_name = if_else(!lab1 %in% provider_config2, 
                                str_split(provider1, "\\|", simplify = TRUE)[, 1], 
                                paste(str_split(provider1, "\\|", simplify = TRUE)[, 1], str_split(provider1, "\\|", simplify = TRUE)[, 2])),  
         # remove excess blank spaces
         Dr_full_name = if_else(lab1 %in% provider_config2, str_replace(Dr_full_name, "\\s+", " "), Dr_full_name), 
         # remove blank space at end of last name
         Dr_full_name = str_remove(Dr_full_name, "\\s$"), 
         Dr_clinic = if_else(!lab1 %in% provider_config2, 
                             str_split(provider1, "\\|", simplify = TRUE)[, 2], 
                             str_split(provider1, "\\|", simplify = TRUE)[, 3])) %>% 
  mutate(Dr_first_name = str_trim(str_split(Dr_full_name, "\\s+", simplify=TRUE)[,1]), 
         Dr_last_name = str_trim(str_split(Dr_full_name, "\\s+", simplify=TRUE)[,2]), 
         Dr_clinic = str_trim(Dr_clinic, side="both"), 
         Dr_provider_num = str_extract(provider1, "\\b\\d\\S{6}[a-zA-Z]$"))





# lines <- str_split(ext_data[1,2], "\n") %>% unlist()
# obr_index <- which(grepl("OBR", str_split(ext_data[1, 2], "\n") %>% unlist()))

# ext_data_HL7 <- ext_data %>% 
#   rowwise() %>% 
#   mutate(clean_hl7 = clean_HL7_message(elr_hl7_message)) %>% 
#   ungroup()

test_data <- ext_data %>% 
  rowwise() %>% 
  mutate(reduce_hl7 = list(modify_HL7_message(elr_hl7_message))) %>% 
  unnest(reduce_hl7) %>% 
  ungroup() %>% 
  # remove duplicate rows created from above function
  distinct(phess_id, .keep_all=TRUE)

quick_excel(test_data, "reduceHL7_testdata.xlsx")

write.csv(test_data, here("Output", "Test", "testdata2.csv"))

## cleaning HL7 message to remove conflicting symbols and spaces ####
# ext_data_HL7$clean_hl7 <- ext_data_HL7$clean_hl7 %>%
#   { 
#     if ("PV1" %in% .) {
#       .[grep("^PV1\\|", .):length(.)] %>%
#         paste(collapse = "\n") %>%
#         str_replace_all("\\^+", " ") %>%
#         str_replace_all("\\|+", "|")
#     } else {
#       .
#     }
#   } %>%
#   str_replace_all("\\^+", " ") %>%  # This replaces multiple occurrences of ^ with a space " "
#   str_replace_all("\\|+", "|") %>%  # This replaces multiple occurrences of | with a single  |
#   str_replace_all("[\\[\\]()]", "")  # This removes all brackets

test_data$reduce_hl7 <- test_data$reduce_hl7 %>%
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



#writexl::write_xlsx(ext_data_HL7, here("Output", "Test", "cleanHL7.xlsx"))

##Create a function to extract provider number ####
#pattern_provider <- "\\b\\d\\S{6}[a-zA-Z]\\b" #establish provider number pattern - [digit, 6 characters (not a space),upper/lowercase letter]
pattern_provider <- "\\b(?<=|)(?<!\\s)\\d{6}[A-Z0-9][a-zA-Z]\\b"

test_data <- test_data %>% 
  mutate(hl7_provider_number = str_extract(reduce_hl7, pattern_provider))

#quick_excel(ext_data_HL7)


# ext_data_HL7_2 <- ext_data_HL7 %>%
#   mutate(provider_number = str_extract(clean_PHESSextract$clean_HL7, pattern_provider)) %>% #extract the Provider number
#   mutate(provider_number = ifelse(!is.na(provider_number), str_replace(provider_number, "\\|", ""), NA)) #using the provider number as a reference, extract doctor

# Check if referred_provider_number doesn't match provider_number. -->> can probably skip this and just use details from provider_numprint
# and if referred provider number is found in clean_PHESSextract$clean_HL7
test_data <- test_data %>%
  mutate(provider_number_check = if_else(Dr_provider_num==hl7_provider_number, "Match", "No match"), 
         # provider_number_final = ifelse(!is.na(Dr_provider_num) & 
         #                                  !is.na(hl7_provider_number) & 
         #                                  Dr_provider_num != hl7_provider_number & 
         #                                  str_detect(reduce_hl7, Dr_provider_num),
         #                                Dr_provider_num,
         #                                hl7_provider_number), 
         provider_number_final = ifelse(!is.na(hl7_provider_number), hl7_provider_number, NA))

tabyl(test_data, provider_number_check)



# Create doctor column based off provider number from HL7 field
# hl7_DR_noted - pulled Dr name from hl7 based on pattern: PR#, name, DR
if (!all(is.na(test_data$provider_number_final))) {
  test_data$provider_number_final <- str_replace(test_data$provider_number_final, "\\|", "") # Remove the initial pipe if needed
  
  pattern_DR <- paste0(test_data$provider_number_final, " (.*?)[^DR\\w](?i)(DR|A\\/PR|\\|)")
  
  # Step 3: Extract the required text
  test_data$hl7_DR_noted <- str_extract(test_data$reduce_hl7, pattern_DR)
  
  # Clean up the extracted text to remove the provider number itself
  test_data$hl7_DR_noted <- if_else(!is.na(test_data$hl7_DR_noted), str_replace(test_data$hl7_DR_noted, test_data$provider_number_final, ""), NA)
  
  # Clean up the extracted text by removing | and trim off white spaces
  test_data$hl7_DR_noted <- str_trim(str_replace(test_data$hl7_DR_noted, "\\|", ""), side="both")
}

# not picking up Dr name details from VIDRL lab
# Dorevitch Dr name details contain 4 alpha-numeric code before DR


## general cleaning/updating shorthand phrases ####
test_data <- test_data %>% 
  mutate(hl7_DR_noted = ifelse(is.na(hl7_DR_noted),str_extract(reduce_hl7, "(?<=\\|)[^|]+\\sDR\\b"), hl7_DR_noted),
         reduce_hl7 = str_replace_all(reduce_hl7, "\\s\\bCTRE\\b", "CENTRE"), 
         reduce_hl7 = str_replace_all(reduce_hl7, "\\s\\bCTR\\b", "CENTRE"), 
         reduce_hl7 = str_replace_all(reduce_hl7,"AUSHIC\\sMC",""),
         reduce_hl7 = str_replace_all(reduce_hl7,"\\sM/C"," MEDICAL CENTRE"),
         reduce_hl7 = str_replace_all(reduce_hl7,"\\sMC", " MEDICAL CENTRE"),
         reduce_hl7 = str_replace_all(reduce_hl7,"Y PUBLIC HEALTH",""))

quick_csv(test_data, "test_data.csv")

## Create a function to extract the first phone number ####
extract_first_phone_number <- function(hl7_message) {
  pattern_phone <- "\\b03\\d{8}\\b" #starts with 03 followed by 8 digits
  pattern_phone1 <- "\\b8\\d{7}\\b" #starts with 8 followed by 7 digits
  pattern_phone2 <- "\\b1800\\d{6}\\b" #starts with 1800 followed by 6 digits
  pattern_phone3 <- "\\b9\\d{7}\\b" #starts with 9 followed by 7 digits
  pattern_phone4 <- "\\b1300\\d{6}\\b" #starts with 1300 followed by 7 digits
  pattern_phone5 <- "\\b04\\d{8}\\b" #starts with 04 followed by 8 digits
  pattern_phone6 <- "\\b7\\d{7}\\b" #starts with 7 followed by 7 digits
  pattern_phone7 <- "\\b5\\d{7}\\b" #starts with 5 followed by 7 digits
  
  dr_phone_number <- str_extract(hl7_message, pattern_phone)
  if (is.na(dr_phone_number)) {
    dr_phone_number <- str_extract(hl7_message, pattern_phone1)
  }
  if (is.na(dr_phone_number)) {
    dr_phone_number <- str_extract(hl7_message, pattern_phone2)
  }
  if (is.na(dr_phone_number)) {
    dr_phone_number <- str_extract(hl7_message, pattern_phone3)
  }
  if (is.na(dr_phone_number)) {
    dr_phone_number <- str_extract(hl7_message, pattern_phone4)
  }  
  if (is.na(dr_phone_number)) {
    dr_phone_number <- str_extract(hl7_message, pattern_phone5)
  }
  if (is.na(dr_phone_number)) {
    dr_phone_number <- str_extract(hl7_message, pattern_phone6)
  }
  if (is.na(dr_phone_number)) {
    dr_phone_number <- str_extract(hl7_message, pattern_phone7)
  }
  return(ifelse(is.na(dr_phone_number), "", dr_phone_number))
}

## Create a function to extract Dr's address ####
extract_address <- function(hl7_message) {
  #numbers/ /number-letters/ /letters/ /3 letter state/4 number postcode
  # pattern_address1 <- "\\d+\\s+([A-Za-z0-9\\s]+)\\s+[A-Za-z\\s]+\\s+[A-Z]{3}\\s+\\d{4}"
  
  #numbers/-/number-letters/ /letters/ /3 letter state/4 number postcode
  pattern_address1 <- "\\d+\"-\"\\d+\\s+([A-Za-z0-9\\s]+)\\s+[A-Za-z\\s]+\\s+[A-Z]{3}\\s+\\d{4}"
  
  #numbers/ /number-letters/ /letters/ /3 letter state/4 number postcode
  pattern_address2 <- "\\d+\\s+([A-Za-z0-9\\s]+)\\s+[A-Za-z\\s]+\\s+[A-Z]{3}\\s+\\d{4}"
  
  #"|"/ /number-letters/ /letters/ /3 letter state/4 number postcode
  pattern_address3 <- "(?<=\\|)\\s*([A-Za-z0-9\\s]+)\\s+[A-Za-z\\s]+\\s+[A-Z]{3}\\s+\\d{4}"
  #number-letters/ /letters/ /3 letter state/4 number postcode
  pattern_address4 <- "\\b([A-Za-z0-9\\s]+)\\s+[A-Za-z\\s]\\d{4}"
  
  # Try to extract address using the first pattern
  address <- str_extract(hl7_message, pattern_address1)
  
  # If the first pattern did not match, try the second pattern
  if (is.na(address)) {
    address <- str_extract(hl7_message, pattern_address2)
  }
  if (is.na(address)) {
    address <- str_extract(hl7_message, pattern_address3)
  }  
  if (is.na(address)) {
    address <- str_extract(hl7_message, pattern_address4)
  }
  # Return the address, or an empty string if no pattern matched
  return(ifelse(is.na(address), "", address))
}

## Create a function to extract General Practice ####
extract_medical_centre <- function(hl7_message) {
  # looking for "DR AUSHICPR |"
  if (grepl("DR AUSHICPR\\s\\|", hl7_message, ignore.case = TRUE)) {
    # extract everything after "DR AUSHICPR |" if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?i)(?<=DR AUSHICPR\\s\\|)[^|]*(\\b[A-Z\\s]+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP)\\s*[^|0-9]*)"
    
    # looking for " DR AUSHICPR "
  } else if (grepl("\\sDR AUSHICPR\\s", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR AUSHICPR " followed by optional | if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?i)(?<=\\sDR AUSHICPR\\s)[^|]*(\\b[A-Z\\s])+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP)[^0-9]"

    # looking for "DR AUSPROV |"
  } else if (grepl("DR AUSPROV\\s\\|", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR AUSHICPR " if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?<=DR AUSPROV\\s\\|)[^|]*\\b(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP)\\s*[^|0-9]*" 
    
    # looking for "UPDOC"
  } else if (grepl("UPDOC", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "UPDOC"
    
    # looking for "MYHEALTH"
  } else if (grepl("MYHEALTH", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "MYHEALTH(\\s[A-Z]+)+"

    # looking for "INSTANT SCRIPTS"
  } else if (grepl("INSTANT SCRIPTS", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "INSTANT SCRIPTS"  
    
    # looking for "EQUINOX"
  } else if (grepl("EQUINOX", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "EQUINOX"
    
    # looking for "PRONTO"
  } else if (grepl("PRONTO", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "PRONTO"
    
    # looking for "DAME PHYLLIS FROST CENTRE"
  } else if (grepl("DAME PHYLLIS FROST CENTRE", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "DAME PHYLLIS FROST CENTRE"  
    
    # looking for "METROPOLITAN REMAND CENTRE"
  } else if (grepl("METROPOLITAN REMAND CENTRE", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "MELBOURNE METROPOLITAN REMAND CENTRE"   
    
    # looking for "MELBOURNE ASSESSMENT PRISON"
  } else if (grepl("MELBOURNE ASSESSMENT PRISON", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "MELBOURNE ASSESSMENT PRISON" 
    
    # looking for "MELBOURNE ASSESSMENT PRISON"
  } else if (grepl("RAVENHALL PRISON", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "RAVENHALL PRISON"     
    
    # looking for "WESTERN HEALTH"
  } else if (grepl("WESTERN HEALTH", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "WESTERN HEALTH"  
    
    # looking for "RCH"
  } else if (grepl("RCH", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "RCH"    
    
    # looking for "RWH"
  } else if (grepl("RWH", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "RWH" 
    
    # looking for "RMH"
  } else if (grepl("RMH", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "ROYAL MELBOURNE HOSPITAL" 
    
    # looking for "Hospital"
  } else if (grepl("Hospital", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "(?i)(?<=\\|)\\s*([^|]*\\b(?:HOSPITAL)\\s*[^|0-9]*)"
    
    # looking for " DR "
  } else if (grepl("\\sDR\\s", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR " followed by optional | if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?i)(?<=\\sDR\\s)[^|]*(\\b[A-Z\\s]+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP)\\s*[^|0-9]*)"
    
  } else {
    # extract everything after "|" with optional space or "|" followed by CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE and optional space or "|"
    pattern_medical <- "(?i)(?<=\\|)\\s*([^|]*\\b(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP)\\s*[^|]*)"
  }
  extracted_text <- str_extract(hl7_message, pattern_medical)
  return(ifelse(is.na(extracted_text), "", extracted_text))
}

#=================          Apply the functions          =======================
## Applying 3 cleaning functions and some manual cleaning to extract GP name, 2 versions of addresses and phone number ####
first_extract_HL7 <- test_data %>%
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
  mutate(hl7_gp_name = ifelse(hl7_medical_centre == "",
                             str_extract(reduce_hl7, "(?<=\\|)\\s*([^|]*\\b(?:CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE)\\s*[^|\\d\\s]*)"),
                             hl7_medical_centre)) %>%
  # Replace NA values with empty strings
  # mutate_all(~ifelse(is.na(.), "", .)) %>%
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% 
  # Remove extraneous sections from hl7_gp_name
  mutate(
    # hl7_gp_name = str_replace_all(hl7_gp_name, provider_number_final, ""), 
    # hl7_gp_name = str_replace_all(hl7_gp_name, hl7_address, ""), 
    # hl7_gp_name = str_replace_all(hl7_gp_name, hl7_phone_number, ""), 
    # hl7_gp_name = str_replace_all(hl7_gp_name, hl7_DR_noted, ""), 

    hl7_gp_name = ifelse(provider_number_final == "" | hl7_gp_name == "",
                        hl7_gp_name,
                        str_replace_all(hl7_gp_name, provider_number_final, "")),
    hl7_gp_name = ifelse(hl7_address == "" | hl7_gp_name == "",
                        hl7_gp_name,
                        str_replace_all(hl7_gp_name, hl7_address, "")),
    hl7_gp_name = ifelse(hl7_phone_number == "" | hl7_gp_name == "",
                        hl7_gp_name,
                        str_replace_all(hl7_gp_name, hl7_phone_number, "")),
    hl7_gp_name = ifelse(hl7_DR_noted == "" | hl7_gp_name == "",
                        hl7_gp_name,
                        str_replace_all(hl7_gp_name, hl7_DR_noted, "")),
    hl7_gp_name = str_replace_all(hl7_gp_name, "AUSHICPR", ""),
    hl7_gp_name = gsub("[0-9]+", "", hl7_gp_name),  # Remove all numbers
    hl7_gp_name = str_remove_all(hl7_gp_name, "[[:punct:]]"),  # Remove all punctuation
    hl7_gp_name = str_remove_all(hl7_gp_name, "\\b(SHOP|SHP|L|G|UNIT|LEVEL|SUITE|PO BOX)\\b"), #Remove any mention of shops, level, etc
    hl7_gp_name = str_trim(hl7_gp_name),  # Remove leading and trailing whitespace
    # Extract address if dr_address is empty
    hl7_address = ifelse(hl7_address == "", 
                        str_extract(reduce_hl7, "\\b\\d+\\s+[A-Z]+\\s+[A-Z]+\\s+[A-Z]+\\s+\\d{4}"), 
                        hl7_address)
    )

quick_excel(first_extract_HL7, "first_extract.xlsx")

# Function: Use regex to capture two words and switch their positions
switch_words <- function(name) {
  
  str_replace(name, "(\\w+)\\s(\\w+)", "\\2 \\1")
}

## further manual cleaning of the results of the functions ####
clean_first_extract <- first_extract_HL7 %>% 
  mutate( hl7_phone_number = gsub("^03", "", hl7_phone_number), #removing 03 in phone numbers
          hl7_gp_name = gsub(" UNIT $", "", hl7_gp_name)) %>% #removing UNIT in addresses
  # dplyr::select(phess_id, condition, event_date, full_name, birth_date,
  #               patient_address, DR_noted, hl7_full_name, hl7_phone_number,
  #               hl7_address, hl7_gp_name) %>% 
  mutate(hl7_DR_noted = str_replace_all(hl7_DR_noted, "(?i)(\\sDR)", "")) %>%  # Remove 'DR' and initials
  mutate(hl7_DR_noted = str_replace_all(hl7_DR_noted, "\\s\\w\\s", " ")) %>% 
  mutate(hl7_match_switch = sapply(hl7_DR_noted, switch_words), 
         hl7_match_switch = str_remove(hl7_match_switch, "^\\s")) %>% 
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

## Calculate matching stats
clean_first_extract <- clean_first_extract %>% 
  mutate(dr_name_match =  stringdist::stringsim(hl7_DR_noted %>% str_remove(., "\\sDR"), Dr_full_name)) %>%
  mutate(dr_name_swich_match = stringdist::stringsim(hl7_match_switch, Dr_full_name)) %>% # hl7_match_2 should be a higher score
  mutate(dr_clinic_match = stringdist::stringsim(hl7_gp_name, Dr_clinic))
  
quick_excel(clean_first_extract, "clean_first_extract.xlsx")

  #  mutate(match_hl7_perc = if_else(hl7_match >= hl7_match_2, hl7_match, hl7_match_2)) 
  # dplyr::select(phess_id, condition, event_date, full_name, birth_date,
  #               patient_address, DR_noted, hl7_full_name, hl7_phone_number,
  #               hl7_address, hl7_gp_name) 
#### match_hl7_perc not showing from here


##function for spliting the address into Street, Suburb, State, Postcode
split_address <- function(df) {
  # Regex patterns to capture address, suburb, state, and postcode
  address_pattern <- "^(\\d*.*?(STREET|ROAD|DRIVE|AVENUE|BOULEVARD|COURT|PLACE|WAY|PARADE|CLOSE|HIGHWAY|CRESCENT)\\b)"
  secondary_address_pattern <- "\\b(?<=CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MEDICINE|MED|SURGERY)\\s*(.*?\\b(STREET|ROAD|DRIVE|AVENUE|BOULEVARD|COURT|PLACE|WAY|PARADE||CLOSE|HIGHWAY|CRESCENT)\\b)"
  
  suburb_pattern <- "(?<=\\b(STREET|ROAD|DRIVE|AVENUE|BOULEVARD|COURT|PLACE|WAY|PARADE|CLOSE|HIGHWAY|CRESCENT)\\b\\s)(.*?)(?=\\s+\\b(?:VIC|NSW|QLD|SA|WA|TAS|NT|ACT|\\d{4})\\b)"
  secondary_suburb_pattern <- "(?<=\\b(?:CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MEDICINE|MED|SURGERY)\\b\\s)(.*?)(?=\\s+\\b(?:VIC|NSW|QLD|SA|WA|TAS|NT|ACT|\\d{4})\\b)"
  
  state_pattern <- "\\b(VIC|NSW|QLD|SA|WA|TAS|NT|ACT)\\b"
  postcode_pattern <- "(?<=\\b(VIC|NSW|QLD)\\s)(\\d{4})"
  
  df <- df %>%
    mutate(
      dr_address_split = str_extract(hl7_address, address_pattern),
      dr_suburb = str_extract(hl7_address, suburb_pattern),
      dr_state = str_extract(hl7_address, state_pattern),
      dr_postcode = str_extract(hl7_address, postcode_pattern),
      dr_address_split_2 = ifelse(is.na(dr_address_split) & !is.na(dr_state),
                                  str_extract(hl7_address, secondary_address_pattern), dr_address_split),
      dr_suburb_2 = ifelse(is.na(dr_suburb) & !is.na(dr_state),
                           str_extract(hl7_address, secondary_suburb_pattern), dr_suburb)
    ) %>% 
    mutate(dr_address_split = ifelse(is.na(dr_address_split),"",dr_address_split))
  
  return(df)
}

## splitting the address to better match with the GP list ####
results_split_address <- split_address(clean_first_extract) %>% 
  # clean up hl7_gp_name by spelling out short abbreviations
  mutate(hl7_gp_name = str_replace_all(hl7_gp_name, "\\bST\\b", "STREET"), #replace ST with STREET
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bRD\\b", "ROAD"), #replace RD with ROAD
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bDR\\b", "DRIVE"), #replace DR with DRIVE
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bDVE\\b", "DRIVE"), #replace DRE with DRIVE
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bAV\\b", "AVENUE"), #replace AV with AVENUE
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bAVE\\b", "AVENUE"), #replace AVE with AVENUE
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bBVD\\b", "BOULEVARD"), #replace BVD with BOULEVARD
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bCRT\\b", "COURT"), #replace CRT with COURT
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bPDE\\b", "PARADE"), # replace PDE with PARADE
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bCL\\b", "CLOSE"), #replace CL with CLOSE
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bHWY\\b", "HIGHWAY"), #replace HWY with HIGHWAY
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bCRES\\b", "CRESCENT"), #replace CRES with CRESCENT
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bCNTR\\b", "CENTRE"), 
         hl7_gp_name = str_replace_all(hl7_gp_name, "\\bCNT\\b", "CENTRE"), 
         # remove any words from medical practice name that may have been captured in suburb
         dr_suburb_2 = str_remove(dr_suburb_2, ".*\\s\\b(CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MEDICINE|MED|SURGERY|GROUP)\\s"), 
         dr_suburb_2 = str_remove(dr_suburb_2, "^(CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MEDICINE|MED|SURGERY|GROUP)\\s"), 
         # remove duplicated words
         dr_suburb_3 = sapply(dr_suburb_2, function(x) paste(unique(unlist(str_split(x,"\\s"))), collapse = ",")), 
         dr_suburb_3 = str_replace_all(dr_suburb_3, ",", " "))

quick_excel(results_split_address, "split_address.xlsx")

# Prepare GP list ----
gplist.raw <- read.csv(here("Data", "healthmap - key health services - general practice.csv")) %>% 
  clean_names()

gplist_vic <- gplist.raw %>% 
  select(site_name, email, web_site, address, suburb, postcode, service_phone) %>% 
  mutate(address = toupper(address),
         site_name = toupper(site_name),
         postcode = as.character(postcode)) %>%   # convert to character
  mutate(across(where(is.character), ~ coalesce(., "") ),  # Replace NA in character columns
         across(where(is.numeric), ~ coalesce(., 0) ),  # Replace NA in numeric columns
         phone = str_extract(service_phone, 'phone","phoneNo":"03\\s[:digit:]{4}\\s[:digit:]{4}') %>% 
           str_extract("[:digit:]{4}\\s[:digit:]{4}") %>% 
           str_remove(., "\\s"))   

# Load NEPHU map objects
load(file.path(str_extract(getwd(), "^.+Epidemiology"), "Population Health Data/NEPHU Maps", "NEPHU_basemaps_sf.RData"))

# upload preconfigured NEPHU GP list
gplist <- readxl::read_xlsx(here("Data", "NEPHU_GPlist.xlsx")) %>% 
  clean_names() %>% 
  #select(-web_site, -service_phone) %>% 
  #rename(practice_name = site_name) %>% 
  # update small name changes to facilitate matching process
  subset(., practice_name!="COBURG JUNCTION MEDICAL CENTRE") %>% 
  mutate(practice_name = case_when(practice_name=="ZEST MEDICAL" ~ "ZEST MEDICAL GROUP", 
                                   practice_name=="POLARIS MEDICAL CENTRE" ~ "POLARIS MEDICAL", 
                                   practice_name=="MODERN MEDICAL - CRAIGIEBURN" ~ "MODERN MEDICAL CRAIGIEBURN", 
                                   practice_name=="BURGUNDY STREET FAMILY MEDICAL" ~ "BURGUNDY STREET FAMILY MEDICAL SUITE", 
                                   practice_name=="CAMBERWELL JUNCTION MEDICAL CLINIC" ~ "CAMBERWELL JUNCTION MEDICAL", 
                                   practice_name=="CHILDS ROAD MEDICAL CLINIC" ~ "CHILDS ROAD MEDICAL CENTRE", 
                                   practice_name=="CRAIGIEBURN MEDICAL & DENTAL CENTRE" ~ "CRAIGIEBURN MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="FIRST HEALTH MEDICAL CENTRE - ROWVILLE" ~ "FIRST HEALTH MEDICAL CENTRE", 
                                   practice_name=="GREENSBOROUGH MEDICAL & DENTAL CENTRE" ~ "GREENSBOROUGH MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="HARP FAMILY MEDICAL" ~ "HARP FAMILY MEDICAL CENTRE", 
                                   practice_name=="HIGH STREET MEDICAL AND DENTAL CENTRE PRESTON" ~ "HIGH STREET MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="KEW GENERAL PRACTICE" ~ "KEW GENERAL PRACTICE SPORTS MED", 
                                   practice_name=="LAURIMAR MEDICAL CENTRE" ~ "LAURIMAR MEDICAL", 
                                   practice_name=="MASON POINT  MEDICAL CENTRE" ~ "MASON POINT MEDICAL CENTRE SUITE", 
                                   practice_name=="MEDICAL ONE - VICTORIA GARDENS" ~ "MEDICAL ONE VIC GARDENS", 
                                   practice_name=="NORTHEND MEDICAL" ~ "NORTHEND MEDICAL CENTRE", 
                                   practice_name=="SEYMOUR STREET MEDICAL AND DENTAL CENTRE RINGWOOD" ~ "SEYMOUR STREET MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="TURN THE CORNER MEDICAL CLINIC - NORTHCOTE" ~ "TURN THE CORNER MEDICAL CLINIC", 
                                   practice_name=="VIEWPOINT MEDICAL CENTRE" ~ "VIEWPOINT MEDICAL", 
                                   practice_name=="YARRA MEDICAL - ABBOTSFORD" ~ "YARRA MEDICAL", 
                                   TRUE ~ practice_name))

# check for duplicates via phone variable
gplist_dup <- gplist %>% 
  group_by(phone) %>% 
  filter(n()>1) %>% 
  filter(!is.na(phone))

# filter gp list to NEPHU catchment: nephu_poa_allinclu.sf = 660 obs, poa.sf = 538 obs
gplist <- gplist_vic %>% 
  filter(postcode %in% nephu_poa_allinclu.sf$poa_code) %>% 
  select(-web_site, -service_phone) %>% 
  rename(practice_name = site_name) %>% 
  # update small name changes to facilitate matching process
  subset(., practice_name!="COBURG JUNCTION MEDICAL CENTRE") %>% 
  mutate(practice_name = case_when(practice_name=="ZEST MEDICAL" ~ "ZEST MEDICAL GROUP", 
                                   practice_name=="POLARIS MEDICAL CENTRE" ~ "POLARIS MEDICAL", 
                                   practice_name=="MODERN MEDICAL - CRAIGIEBURN" ~ "MODERN MEDICAL CRAIGIEBURN", 
                                   practice_name=="BURGUNDY STREET FAMILY MEDICAL" ~ "BURGUNDY STREET FAMILY MEDICAL SUITE", 
                                   practice_name=="CAMBERWELL JUNCTION MEDICAL CLINIC" ~ "CAMBERWELL JUNCTION MEDICAL", 
                                   practice_name=="CHILDS ROAD MEDICAL CLINIC" ~ "CHILDS ROAD MEDICAL CENTRE", 
                                   practice_name=="CRAIGIEBURN MEDICAL & DENTAL CENTRE" ~ "CRAIGIEBURN MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="FIRST HEALTH MEDICAL CENTRE - ROWVILLE" ~ "FIRST HEALTH MEDICAL CENTRE", 
                                   practice_name=="GREENSBOROUGH MEDICAL & DENTAL CENTRE" ~ "GREENSBOROUGH MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="HARP FAMILY MEDICAL" ~ "HARP FAMILY MEDICAL CENTRE", 
                                   practice_name=="HIGH STREET MEDICAL AND DENTAL CENTRE PRESTON" ~ "HIGH STREET MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="KEW GENERAL PRACTICE" ~ "KEW GENERAL PRACTICE SPORTS MED", 
                                   practice_name=="LAURIMAR MEDICAL CENTRE" ~ "LAURIMAR MEDICAL", 
                                   practice_name=="MASON POINT  MEDICAL CENTRE" ~ "MASON POINT MEDICAL CENTRE SUITE", 
                                   practice_name=="MEDICAL ONE - VICTORIA GARDENS" ~ "MEDICAL ONE VIC GARDENS", 
                                   practice_name=="NORTHEND MEDICAL" ~ "NORTHEND MEDICAL CENTRE", 
                                   practice_name=="SEYMOUR STREET MEDICAL AND DENTAL CENTRE RINGWOOD" ~ "SEYMOUR STREET MEDICAL AND DENTAL CENTRE", 
                                   practice_name=="TURN THE CORNER MEDICAL CLINIC - NORTHCOTE" ~ "TURN THE CORNER MEDICAL CLINIC", 
                                   practice_name=="VIEWPOINT MEDICAL CENTRE" ~ "VIEWPOINT MEDICAL", 
                                   practice_name=="YARRA MEDICAL - ABBOTSFORD" ~ "YARRA MEDICAL", 
                                   TRUE ~ practice_name))


quick_excel(gplist, "NEPHU_GPlist.xlsx")

## Finding matches with the GP list    =========================================

# check for exact matches with GP ####
result_exact_gp1 <- results_split_address %>%
  left_join(gplist %>% 
              mutate(general_practice_name_dupe = practice_name), 
            by = c('hl7_gp_name' = 'general_practice_name_dupe')  ) %>% # Initial join based on 'general_practice_name'
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

quick_excel(result_exact_gp1, "gpmatch_test.xlsx")
tabyl(result_exact_gp1, practice_name)

# Secondary join based on 'dr_address_split' for rows where 'general_practice_name' didn't match ####
result_exact_address <- result_exact_gp1 %>% 
  filter(practice_name == "")%>% 
  select(phess_id, lab, hl7_gp_name, hl7_phone_number, dr_address_split, dr_suburb_3, dr_postcode) %>% 
  # dplyr::select(-c(general_practice_name,
  #                  phone_number,
  #                  GPlist_address,
  #                  suburb,
  #                  postcode,
  #                  email_to_use_for_distribution)) %>% 
  left_join(gplist %>% 
              mutate(gplist_address2 = address),
            by = c('dr_address_split' = 'gplist_address2')) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

tabyl(result_exact_address, practice_name)
quick_excel(result_exact_address, "gpaddressmatch_test.xlsx")

# Secondary join based on phone number and postcode for rows where 'general_practice_name' didn't match ####
result_exact_phone <- result_exact_address %>% 
  filter(practice_name == "")%>% 
  select(phess_id, lab, hl7_gp_name, hl7_phone_number, dr_address_split, dr_suburb_3, dr_postcode) %>% 
  left_join(gplist %>% 
              mutate(gplist_phone = phone, 
                     gplist_postcode = postcode),
            by = c('hl7_phone_number' = 'gplist_phone', "dr_postcode" = "gplist_postcode")) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

tabyl(result_exact_phone, practice_name)
quick_excel(result_exact_phone, "gpphonematch_test.xlsx")

# Left join the three results on 'phess_id' ####
final_exact_result <- result_exact_gp1 %>%
  left_join(result_exact_address %>% dplyr::select(phess_id,
                                                   practice_name,
                                                   phone,
                                                   address,
                                                   suburb,
                                                   postcode,
                                                   email),
            by = "phess_id", suffix = c(".gp1", ".address")) %>% 
  left_join(result_exact_phone %>% select(phess_id,
                                          practice_name,
                                          phone,
                                          address,
                                          suburb,
                                          postcode,
                                          email), 
            by = "phess_id") %>% 
  rename(practice_name.phone = practice_name, 
         phone.phone = phone, 
         address.phone = address, 
         suburb.phone = suburb, 
         postcode.phone = postcode, 
         email.phone = email) %>% 
  mutate(practice_name = ifelse(practice_name.gp1 == "" & practice_name.address!="",
                                        practice_name.address,
                                        practice_name.gp1), 
         practice_name = ifelse(practice_name == "" & practice_name.phone!="", 
                                 practice_name.phone, 
                                 practice_name), 
         phone = ifelse(phone.gp1 == "" & phone.address!="", phone.address, phone.gp1), 
         phone = ifelse(phone =="" & phone.phone!="", phone.phone, phone), 
         address = ifelse(address.gp1 == "" & address.address!="", address.address, address.gp1), 
         address = ifelse(address =="" & address.phone!="", address.phone, address), 
         suburb = ifelse(suburb.gp1 == "" & suburb.address!="", suburb.address, suburb.gp1), 
         suburb = ifelse(suburb =="" & suburb.phone!="", suburb.phone, suburb), 
         postcode = ifelse(postcode.gp1 == "" & postcode.address!="", postcode.address, postcode.gp1), 
         postcode = ifelse(postcode =="" & postcode.phone!="", postcode.phone, postcode), 
         email = ifelse(email.gp1 == "" & email.address!="", email.address, email.gp1), 
         email = ifelse(email =="" & email.phone!="", email.phone, email)) %>% 
  dplyr::select(-ends_with(".gp1"), -ends_with(".address"), -ends_with(".phone")) %>% 
  # mutate(dr_postcode = as.numeric(dr_postcode),
  #        postcode = as.numeric(postcode)) %>% 
  rename(gplist_email = email,
         gplist_practicename = practice_name,
         gplist_phone = phone, 
         gplist_address = address) %>% 
  #replace NA with ""
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) 

quick_excel(final_exact_result, "final_exact_match.xlsx")

## start fuzzy matching the stuff that didnt join exact    =====================

# fuzzy match on both general_practice_name **AND** GP Address - this did not yield any results
# fuzzyjoin::stringdist_ function is not very reliable
# if fuzzy match - best to fuzzymatch on both practice name and address
result_fuzzyjoin <- final_exact_result %>%
  filter(gplist_practicename == "") %>% 
  dplyr::select(-c(gplist_practicename,
                   gplist_phone,
                   gplist_address,
                   suburb,
                   postcode,
                   gplist_email)) %>%
  fuzzyjoin::stringdist_left_join(
    gplist, 
    #by = c('hl7_gp_name' = 'practice_name'),
    by = c('hl7_gp_name' = 'practice_name', 'dr_address_split' = 'address'),
    method = 'jw',  # Jaro-Winkler distance
    max_dist = 0.2,  # Adjust the maximum distance threshold as needed
    ignore_case = TRUE
  ) 

quick_excel(result_fuzzyjoin, "result_fuzzyjoin.xlsx")

# best to fuzzy join gp name first and then unmatched ones to address (2 step)
# fuzzy match on general_practice_name 
# stringdist function on gp practice name only is not reliable even though many 'matches' made. Use regex function instead
result_fuzzyjoin_gp <- final_exact_result %>%
  filter(gplist_practicename == "") %>% 
  dplyr::select(-c(gplist_practicename,
                   gplist_phone,
                   gplist_address,
                   suburb,
                   postcode,
                   gplist_email)) %>%
  fuzzyjoin::regex_left_join(
    gplist, 
    by = c('hl7_gp_name' = 'practice_name'))
    #, method = "jw", max_dist=0.2, ignore_case=T)

tabyl(result_fuzzyjoin_gp, practice_name)
length(unique(result_fuzzyjoin_gp$phess_id))

# Optionally rename columns to avoid conflicts in the next join ####
# result_fuzzyjoin_gp <- result_fuzzyjoin_gp %>% 
#   rename_with(~gsub("\\.y$", "_gp", .x), ends_with(".y")) %>% 
#   rename_with(~gsub("\\.x$", "_gp_address", .x), ends_with(".x"))


# second fuzzy match using the result of the first join result_fuzzyjoin_gp ####
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

############ trial stringdist_join
# second fuzzy match using the result of the first join result_fuzzyjoin_gp ####
# stringdist function is not a reliable joining function - 1 to many matches and not accurate
result_fuzzyjoin_address_stringdist <- result_fuzzyjoin_gp %>%
  fuzzyjoin::stringdist_left_join(
    gplist, 
    by = c('dr_address_split' = 'address')) %>% 
  ungroup()
############

quick_excel(result_fuzzyjoin_address, "result_fuzzyjoin_address.xlsx")

# exact join on phone number ####
# result_leftjoin_phone <- result_fuzzyjoin_address %>%
#   left_join(
#     gplist %>% 
#       mutate(phone_number2 = phone),
#     by = c('hl7_phone_number' = 'phone_number2')) %>% 
#   rename_with(~gsub("\\.x$", "_address", .x), ends_with(".x")) %>%
#   rename_with(~gsub("\\.y$", "_phone", .x), ends_with(".y")) 
#   rename(`Email_address` = email_address) %>%
#   rename(`Email_phone` = email_phone) 

quick_excel(result_leftjoin_phone, "leftjoin_phone_results.xlsx")


## Combine fuzzy join results
final_fuzzyjoin_result <- result_fuzzyjoin %>% 
  filter(!is.na(practice_name)) %>% 
  rows_append(result_fuzzyjoin_gp %>% filter(!is.na(practice_name))) %>% 
  rows_append(result_fuzzyjoin_address %>% filter(!is.na(practice_name))) %>% 
  mutate(fuzzyjoin="Yes") %>% 
  rename(gplist_email = email,
         gplist_practicename = practice_name,
         gplist_phone = phone, 
         gplist_address = address)

# cleaning up table names ####
final_fuzzyjoin_result <- result_leftjoin_phone %>% 
  mutate(GPlist_GPname = ifelse(!is.na(site_name_gp_address),site_name_gp_address,
                                ifelse(!is.na(site_name_gp),site_name_gp,
                                       ifelse(!is.na(site_name_address),site_name_address,
                                              ifelse(!is.na(site_name_phone),site_name_phone, NA)))),
         address = ifelse(!is.na(address_gp_address),address_gp_address,
                                 ifelse(!is.na(address_gp),address_gp,
                                        ifelse(!is.na(address_address),address_address,
                                               ifelse(!is.na(address_phone),address_phone, NA)))),
         GPlist_email = ifelse(!is.na(email_gp_address),email_gp_address,
                               ifelse(!is.na(email_gp),email_gp,
                                      ifelse(!is.na(Email_address),Email_address,
                                             ifelse(!is.na(Email_phone),Email_phone, NA)))),
         GPlist_phone = ifelse(!is.na(phone_gp_address),phone_gp_address,
                                      ifelse(!is.na(phone_gp),phone_gp,
                                             ifelse(!is.na(phone_address),phone_address,
                                                    ifelse(!is.na(phone_phone),phone_phone, NA)))),
         suburb = ifelse(!is.na(suburb_gp_address), suburb_gp_address,
                         ifelse(!is.na(suburb_gp), suburb_gp,
                                ifelse(!is.na(suburb_address), suburb_address,
                                       ifelse(!is.na(suburb_phone), suburb_phone, NA)))),
         postcode = ifelse(!is.na(postcode_gp_address), postcode_gp_address,
                           ifelse(!is.na(postcode_gp), postcode_gp,
                                  ifelse(!is.na(postcode_address), postcode_address,
                                         ifelse(!is.na(postcode_phone), postcode_phone, NA))))) %>% 
  dplyr::select(phess_id,
                #condition, 
                #full_name,
                #birth_date,
                #patient_address, 
                match_dr_perc, 
                dr_full_name, 
                DR_noted,
                dr_address_split, 
                dr_postcode, 
                #dr_phone,
                GPlist_phone, 
                dr_gp_name,
                GPlist_GPname,
                address,
                suburb,
                postcode,
                GPlist_email) %>%
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% #replace NA with ""
  distinct()

quick_excel(final_fuzzyjoin_result, "fuzzyjoin_result.xlsx")

# rm(result_leftjoin_phone,result_fuzzyjoin_address2,result_fuzzyjoin_address)

## joining the exact and fuzzy GP matches   ====================================

final_combined_result <-  final_exact_result %>% 
  left_join(final_fuzzyjoin_result %>% dplyr::select(phess_id,
                                                     `GPlist_GPname`,
                                                     GPlist_address,
                                                     suburb,
                                                     postcode,
                                                     GPlist_email,
                                                     dr_address_split,
                                                     GPlist_phone_number),
            by = c('phess_id' = 'phess_id')) %>%
  mutate(GPlist_GPname = ifelse(GPlist_GPname.x== "",GPlist_GPname.y,GPlist_GPname.x),
         GPlist_address = ifelse(GPlist_address.x== "",GPlist_address.y,GPlist_address.x), 
         GPlist_email = ifelse(GPlist_email.x== "",GPlist_email.y,GPlist_email.x),
         dr_address_split = ifelse(dr_address_split.x== "",dr_address_split.y,dr_address_split.x),
         GPlist_suburb = ifelse(suburb.x== "",suburb.y,suburb.x),
         GPlist_phone_number = ifelse(GPlist_phone_number.x== "",GPlist_phone_number.y,GPlist_phone_number.x),
         GPlist_postcode = ifelse(postcode.x== "",postcode.y,postcode.x)) %>% 
  dplyr::select(-matches("\\.x$|\\.y$"))  %>% 
  mutate(across(where(is.character), ~if_else(. == "", NA_character_, .))) %>% 
  # mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% #replace NA with ""
  mutate(gp_match =  stringsim(dr_gp_name, GPlist_GPname),
         address_match =  stringsim(dr_address_split, GPlist_address),
         phone_match = stringsim(dr_phone_number, GPlist_phone_number)) %>% 
  mutate(ave_match = rowMeans(dplyr::select(., gp_match, address_match, phone_match),na.rm = TRUE)) %>% 
  distinct(.) %>% 
  group_by(phess_id) %>%
  mutate(ave_match = ifelse(is.nan(ave_match), 0, ave_match)) %>% 
  filter(ave_match == max(ave_match, na.rm = TRUE)) %>%
  ungroup() %>% 
  distinct(phess_id, .keep_all = TRUE) # Keep only the first occurrence of each phess_id


final_combined_result_notHC <- final_combined_result %>% 
  filter(!dr_gp_name %in% high_load,!GPlist_GPname %in% high_load) #filter out high case loads 


final_combined_result_pull <- final_combined_result_notHC %>% ungroup() %>% dplyr::select(phess_id) %>% distinct(phess_id) %>% pull(phess_id)

# Final matches result (combine exact and fuzzy join results)
final_matching_result <- final_exact_result %>% 
  filter(!phess_id %in% final_fuzzyjoin_result$phess_id) %>% 
  mutate(fuzzyjoin=NA_character_) %>% 
  rows_append(final_fuzzyjoin_result) %>% 
  mutate(practice_match = stringdist::stringsim(hl7_gp_name, gplist_practicename), 
         address_match = stringdist::stringsim(dr_address_split_2, gplist_address), 
         phone_match = stringdist::stringsim(hl7_phone_number, gplist_phone)) %>% 
  mutate(ave_match = rowMeans(select(., practice_match, address_match, phone_match), na.rm=T), 
         match_prob = practice_match * address_match * phone_match)

quick_excel(final_matching_result, "final_matching_result.xlsx")
