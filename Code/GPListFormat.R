# To classify the GP Master List into NEPHU and Non-NEPHU clinics

# Load packages ----
pacman::p_load(readxl, 
               tidyverse)

# load NEPHU map objects
load(file.path(str_extract(getwd(), "^.+Epidemiology"), 
               "Population Health Data/NEPHU Maps", 
               "NEPHU_basemaps_sf.RData"))

# Load Data ---------------------------------------------------------------
gp_file <- "NEPHU_GPlist.xlsx"

gp_data <- read_xlsx(here::here("Data", gp_file))


# Create NEPHU columns ----------------------------------------------------
gp_data <- gp_data %>% 
  mutate(nephu = if_else(postcode %in% nephu_poa_allinclu.sf$poa_code, 
                         "NEPHU", NA_character_), 
         nephu_restrict = if_else(postcode %in% nephu_poa.sf$poa_code, 
                              "NEPHU", NA_character_), 
         hosp = if_else(str_detect(practice_name, "HOSPITAL"), 
                        "Hospital", NA_character_))


# Calculations - tables ---------------------------------------------------
janitor::tabyl(gp_data, nephu)
janitor::tabyl(gp_data, nephu_restrict)
janitor::tabyl(gp_data, hosp)

janitor::tabyl(gp_data, nephu_restrict, hosp)
