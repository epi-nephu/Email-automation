# Compare GP list for updates

library(tidyverse)
library(janitor)

# Import HP copy of GPList
hp_copy <- readxl::read_xlsx(here::here("Data", "NEPHU_GPlist_COPY.xlsx")) %>% 
  clean_names() %>% 
  select(-x7, -x8, -x9) 

# Import Main Version of GPList
original <- readxl::read_xlsx(here::here("Data", "NEPHU_GPlist.xlsx")) %>% 
  clean_names() %>% 
  distinct(practice_name, .keep_all=TRUE)
  


comparison_table_fulljoin <- hp_copy %>% 
  rename_with(~paste0(.x, "_hp")) %>% 
  full_join(original, by=c("practice_name_hp" = "practice_name")) %>% 
  mutate(chk_email = if_else(email_hp == email, "Same", "Different"), 
         chk_address = if_else(address_hp == address, "Same", "Different"), 
         chk_suburb = if_else(suburb_hp == suburb, "Same", "Different"))

# Summary results
email_sum <- tabyl(comparison_table, chk_email)
address_sum <- tabyl(comparison_table, chk_address)
suburb_sum <- tabyl(comparison_table, chk_suburb)

# filter out rows that have no change (excl new clinic rows)
comparison_results <- comparison_table_fulljoin %>% 
  filter(!if_all(c(email_hp, address_hp, suburb_hp, postcode_hp), ~ is.na(.))) %>% 
  mutate(retain = case_when(chk_email=="Same" & chk_address=="Same" & chk_suburb=="Same" ~ "No", 
                            is.na(email_hp) & is.na(email) & chk_address=="Same" & chk_suburb=="Same" ~ "No", 
                            TRUE ~ "Yes")) %>% 
  filter(retain=="Yes")

# Now update the entries into the Main GP List Excel file
# Open comparison_results and update changes to the Main GP List in Epi Folder and also the COPY List


# subset clinics that are in Masterlist but not in COPY
missing_clinics <- comparison_table_fulljoin %>% 
  filter(if_all(c(email_hp, address_hp, suburb_hp, postcode_hp), ~is.na(.))) %>% 
  filter(! str_detect(practice_name_hp, "HOSPITAL"))

# Now transfer across new clinics to the COPY list

