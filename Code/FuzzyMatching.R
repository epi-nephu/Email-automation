# This is the code section for fuzzy matching

# This was used when we started the trial and it would have generated a few matches.
# However, as we subsequently tested each run and updated the GP MasterList, the fuzzy match no longer result in any new hits.
# Hence fuzzy matching is now optional and this code section is isolated on a separate script.
# Main Process script has code line to link to this in case we need to run this again.


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


## Proceed to fuzzy join on Practice name and Address separately in 2 steps with the regex_function

### Fuzzy match on Practice_name ####
result_fuzzyjoin_gp <- result_exactmatch_combined %>%
  filter(phess_id %in% (result_fuzzyjoin %>% filter(hl7_practice_name=="") %>% pull(phess_id))) %>% 
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

quick_excel(result_fuzzyjoin_combined, "fuzzy_matching_combined.xlsx")


## This step needs to be manually checked before compiling so as to code in the fuzzy matched rows to be excluded, normally = 0.32
fuzz_cutoff <- 0.71


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
  # select key variables
  select(phess_id, 
         gplist_practicename, gplist_email, gplist_address, suburb, postcode, 
         Prov_first_name, Prov_last_name, Prov_full_name, Prov_provider_num)
