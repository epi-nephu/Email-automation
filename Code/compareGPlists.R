# Compare GP list for updates

library(tidyverse)
library(janitor)

# Import HP copy of GPList
hp_copy <- readxl::read_xlsx(here::here("Data", "NEPHU_GPlist_COPY.xlsx")) %>% clean_names()

# Import Main Version of GPList
original <- readxl::read_xlsx(here::here("Data", "NEPHU_GPlist.xlsx")) %>% clean_names()


comparison_table <- hp_copy %>% 
  select(-x7, -x8, -x9) %>% 
  rename_with(~paste0(.x, "_hp")) %>% 
  left_join(original, by=c("practice_name_hp" = "practice_name", "postcode_hp" = "postcode", "phone_hp" = "phone")) %>% 
  mutate(chk_email = if_else(email_hp == email, "Same", "Different"), 
         chk_address = if_else(address_hp == address, "Same", "Different"), 
         chk_suburb = if_else(suburb_hp == suburb, "Same", "Different"))

# Summary results
email_sum <- tabyl(comparison_table, chk_email)
address_sum <- tabyl(comparison_table, chk_address)
suburb_sum <- tabyl(comparison_table, chk_suburb)

# filter out rows that have no change
comparison_results <- comparison_table %>% 
  mutate(retain = case_when(chk_email=="Same" & chk_address=="Same" & chk_suburb=="Same" ~ "No", 
                            is.na(email_hp) & is.na(email) & chk_address=="Same" & chk_suburb=="Same" ~ "No", 
                            TRUE ~ "Yes")) %>% 
  filter(retain=="Yes")

# Now update the entries into the Main GP List Excel file
