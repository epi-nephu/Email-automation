# COMMON FUNCTIONS SCRIPT

# list of functions used in the extraction and matching process

# quick function to write df to .xlsx or .csv for in-depth viewing in Excel
# note .xlsx has a character limit (not suitable for viewing large dataset with the original HL7 field)
quick_excel <- function(df, outfile="testoutput.xlsx") {
  writexl::write_xlsx(df, here("Output", "Test", outfile))
}

quick_csv <- function(df, outfile="testoutput.csv") {
  write.csv(df, here("output", "Test", outfile))
}

# Clean HL7 field (modified from WPHU codes)

## Function to reduce HL7 field to ORC and PV1 lines that contain GP details ####
modify_HL7_message <- function(hl7_message) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  orc <- lines[which(grepl("ORC\\|", lines))]
  pv1 <- lines[which(grepl("PV1\\|", lines))]
  paste(orc, pv1, collapse="\n")
  
}


# Function to filter hl7 field to GP last name as listed in Provider field
specific_hl7_message <- function(hl7_message, term) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  specific = lines[which(grepl(term, lines))]
  paste(specific, collapse="\n")
  
}


# Further clean hl7 field to remove unnecessary symbols
further_clean <- function(hl7) {
  hl7 %>% 
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
}



# Extraction Functions (modified from WPHU codes)


## Create a function to extract the first phone number ####
extract_first_phone_number <- function(hl7_message) {
  pattern_phone <- "\\b03\\d{8}\\b" #starts with 03 followed by 8 digits
  pattern_phone1 <- "\\b8\\d{7}\\b" #starts with 8 followed by 7 digits
  pattern_phone2 <- "\\b1800\\d{6}\\b" #starts with 1800 followed by 6 digits
  pattern_phone3 <- "\\b9\\d{7}\\b" #starts with 9 followed by 7 digits
  pattern_phone4 <- "\\b1300\\d{6}\\b" #starts with 1300 followed by 6 digits
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
    pattern_medical <- "(?i)(?<=DR AUSHICPR\\s\\|)[^|]*(\\b[A-Z\\s]+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP|HOSPITAL)\\s*[^|0-9]*)"
    
    # looking for " DR AUSHICPR "
  } else if (grepl("\\sDR AUSHICPR\\s", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR AUSHICPR " followed by optional | if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?i)(?<=\\sDR AUSHICPR\\s)[^|]*(\\b[A-Z\\s])+(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP|HOSPITAL)[^0-9]"
    
    # looking for "DR AUSPROV |"
  } else if (grepl("DR AUSPROV\\s\\|", hl7_message, ignore.case = TRUE)) {
    # extract everything after " DR AUSHICPR " if CENTRE|MEDICAL|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE exists, followed by a optional space or NOT a number or symbol
    pattern_medical <- "(?<=DR AUSPROV\\s\\|)[^|]*\\b(?:MEDICAL|CENTRE|CLINIC|FAMILY|HEALTH|DOCTORS|DOCTOR|PRACTICE|MYHEALTH|SURGERY|GROUP|HOSPITAL)\\s*[^|0-9]*" 
    
    # looking for "UPDOC"
  } else if (grepl("UPDOC", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "UPDOC"
    
    # looking for "MYHEALTH"
  } else if (grepl("MYHEALTH", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "MYHEALTH(\\s[A-Z]+)+"
    
    # looking for "INSTANT SCRIPTS"
  } else if (grepl("INSTANT\\s?SCRIPTS", hl7_message, ignore.case = TRUE)) {
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
    
    # looking for "RMH"
  } else if (grepl("MSHC", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "MSHC" 
    
    # looking for "MEDICLINIC"
  } else if (grepl("MEDICLINIC", hl7_message, ignore.case = TRUE)) {
    pattern_medical <- "MEDICLINIC" 
    
    # looking for "Hospital"
  } else if (grepl("HOSPITAL", hl7_message, ignore.case = TRUE)) {
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

# Function: Use regex to capture two words and switch their positions
switch_words <- function(name) {
  
  str_replace(name, "((?:\\b(?:DE|VAN|VON|DER|DA|DI|DU|LA|LE)\\b\\s*)*\\w+\\'?\\-?\\w*)\\s(\\w+\\-?\\w*)", "\\2 \\1")
}

# (\\w+\\-?\\w*)\\s(\\w+\\-?\\w*)", "\\2 \\1"))

# Function to extract last name of Dr where last name may contain prefix such as de, van, etc.
extract_last_name <- function(name) {
  str_extract(name, "^(?:\\b(?:DE|VAN|VON|DER|DA|DI|DU|LA|LE)\\b\\s*)*\\S+")
}

# "(?i)(?:\\b(?:DE|VAN|VON|DER|DA|DI|DU|LA|LE)\\b\\s*)*\\S+$"

## Function for spliting the address into Street, Suburb, State, Postcode ####
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
