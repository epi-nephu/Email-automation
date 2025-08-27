#-------------------------------------------------------------------------------#

#Project : Automating ESF requests
#Created By: Jenny Giang
#Last Updated: 22 Ausgust 2024
#Child scripts : 4

#-------------------------------------------------------------------------------#

## Set directory and load packages =============================================
# Clear the variables and set up directory ####
rm(list = ls())
myPaths <- .libPaths()
pathway <- "S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/RLibraries"
myPaths <- c(myPaths, pathway)
myPaths <- c(myPaths[3], myPaths[1], myPaths[2])
.libPaths(myPaths)
setwd(dir = dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages ####
library(tidyverse)
library(janitor)
library(keyring)
library(odbc)
library(officer)
library(stringdist)
library(stringr)
library(readxl)
library(tinytex)
library(doconv)
library(fuzzyjoin)
library(openxlsx)

# set up today's date formats ####
todays_date <- as.Date(Sys.Date(), format = "%d/%m/%Y")
todays_date_file <- format(Sys.Date(), "%Y%m%d")

# set up directory folder ####
dir <- "S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/"

# set up known clinics to separate from process ####
# High case load clinics
high_load <- c("MSHC", "NORTHSIDE CLINIC","PRAHRAN MARKET CLINIC","THE CENTRE CLINIC","EQUINOX HEALTH")

# known Hospitals and Prisons
hospital <- c("WESTERN HEALTH","RMH","RWH","RCH")
prison <- c("MELBOURNE ASSESSMENT PRISON","RAVENHALL PRISON","METROPOLITAN REMAND CENTRE")

## import files/folders ========================================================
# input/output folders ####
input_folder <-  "S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/"
output_folder <- "S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Outputs/"

# letter template (word doc) ####
`Gonococcal infection` <- read_docx("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/B - Gonococcal infection.docx") 

# GP list for reference (xlsx)  ####
GP_list_raw <- read_excel("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/WPHU GP List_MASTER.xlsx", sheet = "GP_list PQ")

# read in today's PHESS extract ####
PHESS_extract_raw <- read_excel(paste0("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/AutomategonoESFrequests_",todays_date_file,".xlsx"))
# PHESS_extract_raw <- read_excel(paste0("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/AutomategonoESFrequests_20240814.xlsx"))

# read in second PHESS extract (today) (this one is for the "For Action" spreadsheet) ####
PHESS_extract2_raw <- read_excel(paste0("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/Foractiongonocases_",todays_date_file,".xlsx")) 
# PHESS_extract2_raw <- read_excel(paste0("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/Foractiongonocases_20240814.xlsx"))

# data cleaning ===============================================================
# inital clean GP_list ####
GP_list <- GP_list_raw %>% 
  clean_names() %>% 
  rename(GPlist_address = address,
         LGA = lga ) %>% 
  dplyr::select(general_practice_name, 
                phone_number,
                best_email_for_esf_requests,   
                GPlist_address,
                suburb,
                postcode,
                LGA,
                practice_email, 
                email_to_use_for_distribution) %>% #select relevant columns
  mutate(GPlist_address = toupper(GPlist_address),
         general_practice_name = toupper(general_practice_name),
         postcode = as.character(postcode)) %>%   # convert to character
mutate(across(where(is.character), ~ coalesce(., "") ),  # Replace NA in character columns
         across(where(is.numeric), ~ coalesce(., 0) )  )    # Replace NA in numeric columns
 

# clean extract for use in extracting relevant cases ####
PHESS_extract <- PHESS_extract_raw %>% 
  filter(`Investigation status` == "New", #new cases only
          is.na(`eForm .xml message`), #ESF not returned
          `Reason for follow up` == "Enhanced surveillance required", #ESF required
          is.na(`Enhanced surveillance requested (from local doctor or case)`) ) %>% #ESF not already requested
  mutate(Dr_first_name = str_split(`Referring provider details`, "\\|", simplify = TRUE)[, 1], 
         Dr_last_name = str_split(`Referring provider details`, "\\|", simplify = TRUE)[, 2]) %>% 
  mutate(Dr_first_name = str_trim(Dr_first_name), #tidy
         Dr_last_name = str_trim(Dr_last_name)) %>% #tidy
  mutate(dr_full_name = ifelse(str_detect(Dr_first_name," "),
                               Dr_first_name,
                               paste(Dr_first_name, Dr_last_name, sep = " ")),
         referring_provider_number =  str_extract(`Referring provider details`, "(\\b\\d\\S{6}[a-zA-Z]\\b)"),
         birth_date = as.Date(`Birth Date`, format = "%d-%m-%Y"),
         patient_address = Address) %>% 
  filter(todays_date - birth_date >= 18 * 365.25) %>% 
  clean_names() %>% 
  dplyr::select(phess_id,
                condition,
                elr_hl7_message,
                referring_provider_details,
                referring_provider_number,
                dr_full_name,full_name,
                patient_address,
                birth_date,
                event_date) %>% 
  distinct() %>% 
  group_by(condition, full_name, birth_date, patient_address) %>%
  summarise_all(last) %>%
  ungroup() 

## Run cleaning script   =======================================================

source('01. Cleaning.R')

## Finding matches with the GP list    =========================================

# check for exact matches with GP ####
result_exact_gp1 <- results_split_address %>%
  left_join(GP_list %>% 
               dplyr::select(general_practice_name,
                             phone_number,
                             GPlist_address,
                             suburb,
                             postcode,
                             email_to_use_for_distribution) %>% 
            mutate(general_practice_name_dupe = general_practice_name), 
            by = c('dr_gp_name' = 'general_practice_name_dupe')  ) %>% # Initial join based on 'general_practice_name'
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

# Secondary join based on 'dr_address_split' for rows where 'general_practice_name' didn't match ####
result_exact_address <- result_exact_gp1 %>% 
  filter(general_practice_name == "")%>% 
  dplyr::select(-c(general_practice_name,
                   phone_number,
                   GPlist_address,
                   suburb,
                   postcode,
                   email_to_use_for_distribution)) %>% 
  left_join(GP_list %>% 
              dplyr::select(general_practice_name,
                            phone_number,
                            GPlist_address,
                            suburb,
                            postcode,
                            email_to_use_for_distribution) %>% 
      mutate(GPlist_address2 = GPlist_address),
    by = c('dr_address_split' = 'GPlist_address2')) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) #replace NA with ""

# Left join the two results on 'phess_id' ####
final_exact_result <- result_exact_gp1 %>%
  left_join(result_exact_address %>% dplyr::select(phess_id,
                                                   general_practice_name,
                                                   phone_number,
                                                   GPlist_address,
                                                   suburb,
                                                   postcode,
                                                   email_to_use_for_distribution),
            by = "phess_id", suffix = c(".gp1", ".address")) %>%
  mutate(general_practice_name = ifelse(general_practice_name.gp1 == "",
                                         general_practice_name.address,
                                          general_practice_name.gp1),
         phone_number = ifelse(phone_number.gp1 == "", phone_number.address, phone_number.gp1),
         GPlist_address = ifelse(GPlist_address.gp1 == "", GPlist_address.address, GPlist_address.gp1),
         suburb = ifelse(suburb.gp1 == "", suburb.address, suburb.gp1),
         postcode = ifelse(postcode.gp1 == "", postcode.address, postcode.gp1),
        email_to_use_for_distribution = ifelse(email_to_use_for_distribution.gp1 == "",
                                                 email_to_use_for_distribution.address,
                                                 email_to_use_for_distribution.gp1) ) %>%
  dplyr::select(-ends_with(".gp1"), -ends_with(".address")) %>% 
  # mutate(dr_postcode = as.numeric(dr_postcode),
  #        postcode = as.numeric(postcode)) %>% 
  rename(GPlist_email = email_to_use_for_distribution,
         `GPlist_GPname` = general_practice_name,
         GPlist_phone_number = phone_number) %>% 
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% #replace NA with ""
  dplyr::select(-c(dr_suburb_2, dr_address_split_2, dr_address)) 

## start fuzzy matching the stuff that didnt join exact    =====================

# fuzzy match on both general_practice_name **AND** GP Address
result_fuzzyjoin <- final_exact_result %>%
  filter(GPlist_GPname == "") %>% 
  dplyr::select(-c(GPlist_GPname,
                   GPlist_phone_number,
                   GPlist_address,
                   suburb,
                   postcode,
                   GPlist_email)) %>%
  stringdist_left_join(
    GP_list %>% dplyr::select(general_practice_name,
                              GPlist_address,
                              phone_number,
                              suburb,
                              postcode,
                              email_to_use_for_distribution), 
    by = c('dr_gp_name' = 'general_practice_name', 'dr_address_split' = 'GPlist_address'),
    method = 'jw',  # Jaro-Winkler distance
    max_dist = 0.2,  # Adjust the maximum distance threshold as needed
    ignore_case = TRUE
  ) 


# fuzzy match on general_practice_name ####
result_fuzzyjoin_gp <- result_fuzzyjoin %>%
  regex_left_join(
    GP_list %>% dplyr::select(general_practice_name,
                              GPlist_address,
                              phone_number,
                              suburb,
                              postcode,
                              email_to_use_for_distribution), 
    by = c('dr_gp_name' = 'general_practice_name'))

# Optionally rename columns to avoid conflicts in the next join ####
result_fuzzyjoin_gp <- result_fuzzyjoin_gp %>%
  rename_with(~gsub("\\.y$", "_gp", .x), ends_with(".y")) %>% 
  rename_with(~gsub("\\.x$", "_gp_address", .x), ends_with(".x"))
  

# # second fuzzy match using the result of the first join result_fuzzyjoin_gp ####
# result_fuzzyjoin_address <- result_fuzzyjoin_gp %>%
#   regex_left_join(
#     GP_list %>% select('general_practice_name', GPlist_address,suburb,postcode,phone_number, email_to_use_for_distribution),
#     by = c('dr_address_split' = 'GPlist_address')
#   ) %>%
#   ungroup()

############ trial stringdist_join
# second fuzzy match using the result of the first join result_fuzzyjoin_gp ####
result_fuzzyjoin_address <- result_fuzzyjoin_gp %>%
  stringdist_left_join(
    GP_list %>% dplyr::select(general_practice_name,
                              GPlist_address,
                              suburb,
                              postcode,
                              phone_number,
                              email_to_use_for_distribution), 
    by = c('dr_address_split' = 'GPlist_address')) %>% 
  ungroup()
############


# exact join on phone number ####
result_leftjoin_phone <- result_fuzzyjoin_address %>%
  left_join(
    GP_list %>% dplyr::select(general_practice_name,
                              phone_number,
                              GPlist_address,
                              suburb,
                              postcode,
                              email_to_use_for_distribution) %>% 
              mutate(phone_number2 = phone_number),
    by = c('dr_phone_number' = 'phone_number2')) %>% 
  rename_with(~gsub("\\.x$", "_address", .x), ends_with(".x")) %>%
  rename_with(~gsub("\\.y$", "_phone", .x), ends_with(".y")) %>%
  rename(`Email_address` = email_to_use_for_distribution_address) %>%
  rename(`Email_phone` = email_to_use_for_distribution_phone) 


# cleaning up table names ####
final_fuzzyjoin_result <- result_leftjoin_phone %>% 
  mutate(GPlist_GPname = ifelse(!is.na(general_practice_name_gp_address),general_practice_name_gp_address,
                                  ifelse(!is.na(general_practice_name_gp),general_practice_name_gp,
                                         ifelse(!is.na(general_practice_name_address),general_practice_name_address,
                                           ifelse(!is.na(general_practice_name_phone),general_practice_name_phone, NA)))),
         GPlist_address = ifelse(!is.na(GPlist_address_gp_address),GPlist_address_gp_address,
                                   ifelse(!is.na(GPlist_address_gp),GPlist_address_gp,
                                        ifelse(!is.na(GPlist_address_address),GPlist_address_address,
                                          ifelse(!is.na(GPlist_address_phone),GPlist_address_phone, NA)))),
         GPlist_email = ifelse(!is.na(email_to_use_for_distribution_gp_address),email_to_use_for_distribution_gp_address,
                                 ifelse(!is.na(email_to_use_for_distribution_gp),email_to_use_for_distribution_gp,
                                      ifelse(!is.na(Email_address),Email_address,
                                        ifelse(!is.na(Email_phone),Email_phone, NA)))),
         GPlist_phone_number = ifelse(!is.na(phone_number_gp_address),phone_number_gp_address,
                                     ifelse(!is.na(phone_number_gp),phone_number_gp,
                                        ifelse(!is.na(phone_number_address),phone_number_address,
                                               ifelse(!is.na(phone_number_phone),phone_number_phone, NA)))),
         suburb = ifelse(!is.na(suburb_gp_address), suburb_gp_address,
                         ifelse(!is.na(suburb_gp), suburb_gp,
                                ifelse(!is.na(suburb_address), suburb_address,
                                       ifelse(!is.na(suburb_phone), suburb_phone, NA)))),
         postcode = ifelse(!is.na(postcode_gp_address), postcode_gp_address,
                           ifelse(!is.na(postcode_gp), postcode_gp,
                                  ifelse(!is.na(postcode_address), postcode_address,
                                         ifelse(!is.na(postcode_phone), postcode_phone, NA))))) %>% 
  dplyr::select(phess_id,
                condition, 
                full_name,
                birth_date,
                patient_address, 
                dr_full_name, 
                DR_noted,
                dr_address_split, 
                dr_postcode, 
                dr_phone_number,
                GPlist_phone_number, 
                dr_gp_name,
                GPlist_GPname,
                GPlist_address,
                suburb,
                postcode,
                GPlist_email) %>%
  mutate(across(where(is.character), ~if_else(is.na(.), "", .))) %>% #replace NA with ""
  distinct()

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

# high case load table ####
high_case_load <- final_combined_result %>% 
  filter(dr_gp_name %in% high_load | GPlist_GPname %in% high_load) %>% #pulled GP name OR matched GP name in high case load list
  select(phess_id, event_date, full_name,birth_date, GPlist_GPname, GPlist_address,GPlist_phone_number, GPlist_email,gp_match,address_match,phone_match,ave_match)

## identify cases to send emails and drafts to create ==========================

# create a new table filtering for PHESS cases to automate ####
automate_this_list <- final_combined_result %>% 
  filter(
    ave_match >=0.60, #filter match %
    !is.na(GPlist_GPname), #filter where GPs exsist?
    GPlist_email != "0", #filter where an email has been listed in GPlist
    !dr_gp_name %in% high_load,
    !GPlist_GPname %in% high_load,
  ) %>%  ### add more to this
  mutate(ESF_status = "Email sent") #the resulting cases are emails to send

automate_PHESS_ID_pull <- automate_this_list %>% ungroup() %>% dplyr::select(phess_id) %>% distinct(phess_id) %>% pull(phess_id)

# Create the other table with everything else not in automate_this_list ####
drafts_this_list <- final_combined_result %>% 
  anti_join(automate_this_list, by = "phess_id") %>% 
  filter(!dr_gp_name %in% high_load,
         !GPlist_GPname %in% high_load) %>% 
  mutate(ESF_status = case_when(
    GPlist_email == "0" ~ "Draft saved",
    dr_gp_name %in% hospital  | GPlist_GPname %in% hospital ~ "Hospital",
    dr_gp_name %in% prison | GPlist_GPname %in% prison~ "Prison",
    ave_match <0.60 ~ "No match",
    is.na(ave_match) & is.na(dr_gp_name) ~ "No match",
    is.na(GPlist_GPname) & is.na(ave_match) ~ "No match",
    TRUE ~ "Draft saved"
  ))
  
draft_PHESS_ID_pull <- drafts_this_list %>% ungroup() %>% dplyr::select(phess_id) %>% distinct(phess_id) %>% pull(phess_id)


# creating working spreadsheet ####
working_spreadsheet <- bind_rows(automate_this_list, drafts_this_list) %>% 
  group_by(condition, full_name, birth_date, patient_address) %>%
  summarise_all(last) %>%
  ungroup() %>% 
#rearrage columns
select(phess_id, event_date, full_name, birth_date,
       dr_gp_name, GPlist_GPname,
       dr_address_split, GPlist_address,
       dr_suburb, GPlist_suburb,
       dr_phone_number, GPlist_phone_number,
       GPlist_email,
       gp_match, address_match, phone_match, ave_match, ESF_status)


# create a clean working spreadsheet for admin ####
ESF_status_1 <- working_spreadsheet %>% 
  dplyr::select(phess_id, ESF_status)

source('02. Generate requests.R')


# add file path for attaching to emails ####
final_combined_result_attach <- working_spreadsheet %>% 
  mutate(pdf_file_path = paste0(output_pdf, phess_id, " - GN.pdf"))

## load sending email script====================================================
#source(03. Send emails.R)
# source('04. Final tables.R')


## save file ===================================================================
# save inital spreadsheet ####
wb <- createWorkbook()

#add a sheet - raw table
addWorksheet(wb, "Raw Result")
writeData(wb, "Raw Result", working_spreadsheet)

#add a sheet - clean table
addWorksheet(wb, "ESF status")
writeData(wb, "ESF status", ESF_status)

#add a sheet - for actioning clinical notifications
addWorksheet(wb, "For action")
writeData(wb, "For action",other_list)
  
#add a sheet - for high case load clinic notifications
addWorksheet(wb, "High case load")
writeData(wb, "High case load",high_case_load)

#save the excel spreadsheet
saveWorkbook(wb, paste0(output_folder,"PHESS_extract_",todays_date_file,".xlsx"), overwrite = TRUE)

# ##save table for further manual interagation
# write.xlsx(working_spreadsheet, paste0("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Outputs/checking_matches_",todays_date_file,".xlsx"))

