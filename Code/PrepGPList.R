# PREPARE GP LIST FILE

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
                                   practice_name=="RICHMOND MEDICAL" ~ "RICHMOND MEDICAL CENTRE", 
                                   practice_name=="FOREST HILL CHASE MEDICAL CENTRE" ~ "FOREST HILL CHASE MED CENTRE", 
                                   practice_name=="FOREST HILL MEDICAL" ~ "FORREST HILL MEDICAL CENTRE", 
                                   practice_name=="FOREST HILL MEDICAL AND DENTAL CENTRE" ~ "FOREST HILL MEDICAL AND DENTAL", 
                                   TRUE ~ practice_name))

# check for duplicates via phone variable (to avoid many-many joins later)
gplist_dup <- gplist %>% 
  group_by(phone) %>% 
  filter(n()>1) %>% 
  filter(!is.na(phone))

gplist_dup_phone <- unique(gplist_dup$phone)
