#=================       cleaning tables        ================================

## Create a function to process each HL7 message ####
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
#### end function

## Apply the function to PHESS extract ####
clean_PHESSextract <- PHESS_extract %>%
  rowwise() %>%  #group data frames by rows
  mutate(clean_HL7 = clean_HL7_message(elr_hl7_message)) %>%
  ungroup()

## cleaning HL7 message to remove conflicting symbols and spaces ####
clean_PHESSextract$clean_HL7 <- clean_PHESSextract$clean_HL7 %>%
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


##Create a function to extract provider number ####
pattern_provider <- "\\b\\d\\S{6}[a-zA-Z]\\b" #establish provider number pattern - [digit, 6 characters (not a space),upper/lowercase letter]


clean_PHESSextract2 <- clean_PHESSextract %>%
  mutate(provider_number = str_extract(clean_PHESSextract$clean_HL7, pattern_provider)) %>% #extract the Provider number
  mutate(provider_number = ifelse(!is.na(provider_number), str_replace(provider_number, "\\|", ""), NA)) #using the provider number as a reference, extract doctor

# Check if referred_provider_number doesn't match provider_number
# and if referred provider number is found in clean_PHESSextract$clean_HL7
clean_PHESSextract2 <- clean_PHESSextract2 %>%
  mutate(provider_number_final = ifelse(!is.na(referring_provider_number) & 
                                          !is.na(provider_number) & 
                                          referring_provider_number != provider_number & 
                                          str_detect(clean_PHESSextract$clean_HL7, referring_provider_number),
                                        referring_provider_number,
                                        provider_number))

# Create doctor column based off provider number
if (!all(is.na(clean_PHESSextract2$provider_number_final))) {
  clean_PHESSextract2$provider_number_final <- str_replace(clean_PHESSextract2$provider_number_final, "\\|", "") # Remove the initial pipe if needed
  pattern_DR <- paste0(clean_PHESSextract2$provider_number_final, " (.*?)(?i)DR")
  
  # Step 3: Extract the required text
  clean_PHESSextract2$DR_noted <- str_extract(clean_PHESSextract$clean_HL7, pattern_DR)
  
  # Clean up the extracted text to remove the provider number itself
  clean_PHESSextract2$DR_noted <- ifelse(!is.na(clean_PHESSextract2$DR_noted), str_replace(clean_PHESSextract2$DR_noted, clean_PHESSextract2$provider_number_final, ""), NA)
}


## general cleaning/updating shorthand phrases ####
clean_PHESSextract2 <- clean_PHESSextract2 %>% 
  mutate(DR_noted = ifelse(is.na(DR_noted),str_extract(clean_HL7, "(?<=\\|)[^|]+\\sDR\\b"),DR_noted),
         clean_HL7 = str_replace_all(clean_HL7, "\\s\\bCTRE\\b", "CENTRE"),
         clean_HL7 = str_replace_all(clean_HL7,"AUSHIC\\sMC",""),
         clean_HL7 = str_replace_all(clean_HL7,"\\sM/C"," MEDICAL CENTRE"),
         clean_HL7 = str_replace_all(clean_HL7,"\\sMC", " MEDICAL CENTRE"),
         clean_HL7 = str_replace_all(clean_HL7,"Y PUBLIC HEALTH",""))



## Create a function to extract the first phone number ####
extract_first_phone_number <- function(hl7_message) {
  pattern_phone <- "\\b03\\d{8}\\b" #starts with 03 followed by 8 digits
  pattern_phone1 <- "\\b8\\d{7}\\b" #starts with 8 followed by 7 digits
  pattern_phone2 <- "\\b1800\\d{6}\\b" #starts with 1800 followed by 6 digits
  pattern_phone3 <- "\\b9\\d{7}\\b" #starts with 9 followed by 7 digits
  pattern_phone4 <- "\\b1300\\d{6}\\b" #starts with 1300 followed by 7 digits
  pattern_phone5 <- "\\b04\\d{8}\\b" #starts with 04 followed by 8 digits
  
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
    pattern_medical <- "(?i)(?<=DR AUSHICPR\\s\\|)[^|]*(\\b[A-Z\\s]+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH)\\s*[^|0-9]*)"
    
    # looking for " DR AUSHICPR "
  } else if (grepl("\\sDR AUSHICPR\\s", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR AUSHICPR " followed by optional | if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?i)(?<=\\sDR AUSHICPR\\s)[^|]*(\\b[A-Z\\s]+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH)\\s*[^|0-9]*)"
    
    # looking for "DR AUSHICPR |"
  } else if (grepl("DR AUSPROV\\s\\|", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR AUSHICPR " if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?<=DR AUSPROV\\s\\|)[^|]*\\b(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH)\\s*[^|0-9]*" 
    
    # looking for "UPDOC"
  } else if (grepl("UPDOC", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "UPDOC"
    
    # looking for "INSTANT SCRIPTS"
  } else if (grepl("INSTANT SCRIPTS", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "INSTANT SCRIPTS"  
    
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
    
    # looking for " DR "
  } else if (grepl("\\sDR\\s", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR " followed by optional | if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?i)(?<=\\sDR\\s)[^|]*(\\b[A-Z\\s]+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH)\\s*[^|0-9]*)"
    
  } else {
    # extract everything after "|" with optional space or "|" followed by CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE and optional space or "|"
    pattern_medical <- "(?i)(?<=\\|)\\s*([^|]*\\b(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH)\\s*[^|]*)"
  }
  extracted_text <- str_extract(hl7_message, pattern_medical)
  return(ifelse(is.na(extracted_text), "", extracted_text))
}

#=================          Apply the functions          =======================
## Applying 3 cleaning functions and some manual cleaning to extract GP name, 2 versions of addresses and phone number ####
first_extract_HL7 <- clean_PHESSextract2 %>%
  mutate(clean_HL7 = str_remove(clean_HL7,provider_number_final)) %>% 
  rowwise() %>%
  mutate(
    dr_phone_number = extract_first_phone_number(clean_HL7), #phone clean function
    dr_address = extract_address(clean_HL7), #address clean function
    medical_centre = extract_medical_centre(clean_HL7) #medical centre clean funtion
  ) %>%
  ungroup() %>%
  # Remove long HL7 message column 
  dplyr::select(-c(elr_hl7_message, referring_provider_number)) %>%
  # Update medical centres to remove merged strings - AUSHICPR, DR_noted, AUS M, etc.
  mutate(medical_centre = str_replace_all(medical_centre, "AUSHICPR", "") %>%
           str_replace_all("DR_noted", "") %>%
           str_replace_all("AUS M", "")) %>%
  # doing a second extract on GPlist_GPname if medical centre field is empty
  mutate(dr_gp_name = ifelse(medical_centre == "",
                          str_extract(clean_HL7, "(?<=\\|)\\s*([^|]*\\b(?:CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE)\\s*[^|\\d\\s]*)"),
                          medical_centre)) %>%
  # Replace NA values with empty strings
  # mutate_all(~ifelse(is.na(.), "", .)) %>%
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% 
  # Remove extraneous sections from dr_gp_name
  mutate(
    dr_gp_name = ifelse(provider_number_final == "" | dr_gp_name == "", 
                     dr_gp_name, 
                     str_replace_all(dr_gp_name, provider_number_final, "")),
    dr_gp_name = ifelse(dr_address == "" | dr_gp_name == "", 
                     dr_gp_name, 
                     str_replace_all(dr_gp_name, dr_address, "")),
    dr_gp_name = ifelse(dr_phone_number == "" | dr_gp_name == "", 
                     dr_gp_name, 
                     str_replace_all(dr_gp_name, dr_phone_number, "")),
    dr_gp_name = ifelse(DR_noted == "" | dr_gp_name == "", 
                     dr_gp_name, 
                     str_replace_all(dr_gp_name, DR_noted, "")),
    dr_gp_name = str_replace_all(dr_gp_name, "AUSHICPR", ""),
    dr_gp_name = gsub("[0-9]+", "", dr_gp_name),  # Remove all numbers
    dr_gp_name = str_remove_all(dr_gp_name, "[[:punct:]]"),  # Remove all punctuation
    dr_gp_name = str_remove_all(dr_gp_name, "\\b(SHOP|L|G|UNIT)\\b"), #Remove any mention of shops etc
    dr_gp_name = str_trim(dr_gp_name),  # Remove leading and trailing whitespace
    # Extract address if dr_address is empty
    dr_address = ifelse(dr_address == "", 
                        str_extract(clean_HL7, "\\b\\d+\\s+[A-Z]+\\s+[A-Z]+\\s+[A-Z]+\\s+\\d{4}"), 
                        dr_address))

switch_words <- function(name) {
  # Use regex to capture two words and switch their positions
  str_replace(name, "(\\w+)\\s(\\w+)", "\\2 \\1")
}

## further manual cleaning of the results of the functions ####
clean_first_extract <- first_extract_HL7 %>% 
  mutate( dr_phone_number = gsub("^03", "", dr_phone_number), #removing 03 in phone numbers
          dr_gp_name = gsub(" UNIT $", "", dr_gp_name)) %>% #removing UNIT in addresses
  dplyr::select(phess_id, condition, event_date, full_name, birth_date,
                patient_address, DR_noted, dr_full_name, dr_phone_number,
                dr_address, dr_gp_name) %>% 
  mutate(DR_noted = str_replace_all(DR_noted, "(?i)(\\sDR)", "")) %>%  # Remove 'DR' and initials
  mutate(DR_noted = str_replace_all(DR_noted, "\\s\\w\\s", " ")) %>% 
  mutate(dr_match_switch = sapply(DR_noted, switch_words)) %>% 
  mutate(dr_match =  stringsim(DR_noted, dr_full_name)) %>%
  mutate(dr_match_2 = stringsim(dr_match_switch, dr_full_name)) %>% 
  mutate(match_dr_perc = if_else(dr_match >= dr_match_2, dr_match, dr_match_2)) %>% 
  dplyr::select(phess_id, condition, event_date, full_name, birth_date,
                patient_address, DR_noted, dr_full_name, dr_phone_number,
                dr_address, dr_gp_name) 
#### match_dr_perc not showing from here

## further cleaning ####
clean_first_extract <- clean_first_extract %>% 
  mutate(dr_address = toupper(dr_address), #change everything to uppercase to match GPlist
         dr_address = str_replace_all(dr_address, "\\bST\\b", "STREET"), #replace ST with STREET
         dr_address = str_replace_all(dr_address, "\\bRD\\b", "ROAD"), #replace RD with ROAD
         dr_address = str_replace_all(dr_address, "\\bDR\\b", "DRIVE"), #replace DR with DRIVE
         dr_address = str_replace_all(dr_address,"\\bDVE\\b", "DRIVE"), #replace DRE with DRIVE
         dr_address = str_replace_all(dr_address, "\\bAV\\b", "AVENUE"), #replace AV with AVENUE
         dr_address = str_replace_all(dr_address, "\\bAVE\\b", "AVENUE"), #replace AVE with AVENUE
         dr_address = str_replace_all(dr_address, "\\bBVD\\b", "BOULEVARD"), #replace BVD with BOULEVARD
         dr_address = str_trim(dr_address)) #remove spaces at the start and end

##function for spliting the address into Street, Suburb, State, Postcode
split_address <- function(df) {
  # Regex patterns to capture address, suburb, state, and postcode
  address_pattern <- "^(\\d+.*?(STREET|ROAD|DRIVE|AVENUE|BOULEVARD|COURT|PLACE|WAY|PARADE|CLOSE)\\b)"
  secondary_address_pattern <- "\\b(?:CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE)?\\s*(.*?\\b(STREET|ROAD|DRIVE|AVENUE|BOULEVARD|COURT|PLACE|WAY|PARADE||CLOSE|HWY)\\b)"
  
  suburb_pattern <- "(?<=\\b(STREET|ROAD|DRIVE|AVENUE|BOULEVARD|COURT|PLACE|WAY|PARADE|CLOSE)\\b\\s)(.*?)(?=\\s+\\b(?:VIC|NSW|QLD|SA|WA|TAS|NT|ACT|\\d{4})\\b)"
  secondary_suburb_pattern <- "(?<=\\b(STREET|ROAD|DRIVE|AVENUE|BOULEVARD|COURT|PLACE|WAY|PARADE|CLOSE|HWY)\\b\\s)(.*?)(?=\\s+\\b(?:VIC|NSW|QLD|SA|WA|TAS|NT|ACT|\\d{4})\\b)"

  state_pattern <- "\\b(VIC|NSW|QLD|SA|WA|TAS|NT|ACT)\\b"
  postcode_pattern <- "\\b(\\d{4})$"
  
  df <- df %>%
    mutate(
      dr_address_split = str_extract(dr_address, address_pattern),
      dr_suburb = str_extract(dr_address, suburb_pattern),
      dr_state = str_extract(dr_address, state_pattern),
      dr_postcode = str_extract(dr_address, postcode_pattern),
      dr_address_split_2 = ifelse(is.na(dr_address_split) & !is.na(dr_state),
                                  str_extract(dr_address, secondary_address_pattern), dr_address_split),
      dr_suburb_2 = ifelse(is.na(dr_suburb) & !is.na(dr_state) & is.na(dr_address_split_2),
                           str_extract(dr_address, secondary_suburb_pattern), dr_suburb)
    ) %>% 
    mutate(dr_address_split = ifelse(is.na(dr_address_split),"",dr_address_split))
  
  return(df)
}

## splitting the address to better match with the GP list ####
results_split_address <- split_address(clean_first_extract) 

### remove PTY LTD PO BOX