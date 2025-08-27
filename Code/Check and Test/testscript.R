# test resid cases

tabyl(extract, work_study_care_status)

resid_cases <- extract %>% 
  filter(work_study_care_status=="In residential care")

names(resid_cases)

char_list <- list(c("apple", "banana", "cherry"), c("dog", "elephant"), c("fish", "giraffe"))

df <- data.frame(name = c("Alice", "Bob"), age = c(25, 30))



# Extract the value from the first row, second column (Alice's age)

single_cell_value <- df[1, 2] 



# load test data
testdata <- readxl::read_xlsx(here("Data", "TestingData.xlsx"))

testdata_linelist <- head(linelist.raw, 15)

# original modify hl7 message function:
modify_HL7_message <- function(hl7_message) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  orc <- lines[which(grepl("ORC\\|", lines))]
  pv1 <- lines[which(grepl("PV1\\|", lines))]
  paste(orc, pv1, collapse="\n")
  
}

# modify function above to explore how the function works
modify_HL7_message <- function(hl7_message) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  return(lines)
  
}


# modify function to include a if else for different labs to select either ORC or PV1 line
modify_HL7_message <- function(hl7_message) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  orc <- lines[which(grepl("ORC\\|", lines))]
  pv1 <- lines[which(grepl("PV1\\|", lines))]
  
  if(lab1=="Dorevitch Pathology") {
    pv1
  } else {
    orc
  }
  
  
}


test_elr <- test %>% 
  slice(1) %>% 
  select(phess_id, elr_hl7_message) %>% 
  mutate(hl7_split = unnest(list(str_split(elr_hl7_message, "\n")))) %>% 
  mutate(hl7_specific = hl7_split[which(grepl("SHORTIS", hl7_split))])

test_elr$hl7_specific


# this is the working code (lines 52 - 65)
specific_hl7_message <- function(hl7_message, term) {
  lines <- str_split(hl7_message, "\n") %>% unlist()
  specific = lines[which(grepl(term, lines))]
  paste(specific, collapse="\n")

}


test_elr <- test %>% 
  slice(1) %>% 
  select(phess_id, elr_hl7_message) %>% 
  mutate(hl7_split = str_split(elr_hl7_message, "\n")) %>% 
  mutate(hl7_modify = modify_HL7_message(elr_hl7_message)) %>% 
  mutate(hl7_specific = specific_hl7_message(hl7_modify, "SHORTIS"))

test_elr$hl7_specific

quick_excel(test_elr %>% select(-hl7_split), "test_elr.xlsx")


test_elr_listvct <- test$hl7_split

quick_excel(test_1row, "test_1row.xlsx")
quick_excel(test[1,], "test.xlsx")

test1 <- test[4,]

test <- testdata %>% 
  clean_names() %>% 
  rename(lab = notifying_laboratory, 
         provider = referring_provider_details) %>% 
  select(phess_id, condition, first_name, last_name, birth_date, 
         event_date, lab, provider, notifier, elr_hl7_message) %>% 
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
         Prov_provider_num = str_extract(provider1, "\\b\\d\\S{6}[a-zA-Z]$")) %>% 
  rowwise() %>% 
  mutate(reduce_hl7 = list(modify_HL7_message(elr_hl7_message))) %>% 
  unnest(reduce_hl7) %>% 
  ungroup() %>% 
  # remove duplicate rows created from above function
  distinct(phess_id, .keep_all=TRUE)

test_1row <- test[34,] # contains multiple PV1 and ORC lines with different Dr names

# test to retain PV1 and ORC lines with only selected notifier name
test_1row_selectdr <- test_1row %>% 
  mutate(trim_hl7 = reduce_hl7[which(grepl(".*VIDOT", reduce_hl7))])

quick_excel(test_1row, "test_1row.xlsx")



# Check the type of 'single_cell_value' (it will be an atomic vector)

typeof(single_cell_value) 


vect <- test[1,4] # list in a df

unlist_vect <- unlist(vect)
unlist_vect

unlist_test <- test %>% 
  mutate(unlist_hl7 = unlist(elr_hl7_message)) %>% # no change
  mutate(unlist_str_hl7 = str_split(elr_hl7_message, "\n")) %>% # list: vector of character lines
  rowwise() %>% 
  mutate(orc_line = list(str_extract(unlist_hl7, "ORC.+\\n"))) %>% 
  mutate(clean_hl7 = clean_HL7_message(elr_hl7_message)) %>% 
  mutate(reduce_hl7 = reduce_HL7_message(elr_hl7_message)) %>% 
  mutate(orc_hl7 = orc_HL7_message(elr_hl7_message)) %>% 
  mutate(modify_hl7 = modify_HL7_message(elr_hl7_message))

unlist_test[[4, 6]]

unlist_test %>% 
  select(-unlist_str_hl7, -orc_line) %>% 
  quick_csv(., "unlist_test.csv")


typeof(unlist_test$elr_hl7_message)
typeof(unlist_test$unlist_hl7)

str_split(hl7_message, "\n") %>% unlist()


