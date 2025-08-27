#=============== create separate list to assign to TL ==========================
other_list <- PHESS_extract2_raw %>% 
  dplyr::select(`PHESS ID`,`Assigned to`,`Was the case pregnant at time of initial notification`,Azithromycin,Ceftriaxone,Ciprofloxacin,Penicillin,
         Spectinomycin,Streptomycin,Sulphatiazole,Tetracycline, `Partner notification (contact tracing) requirement indicated by doctor`) %>% 
  filter(is.na(`Assigned to`),`Was the case pregnant at time of initial notification` == "Yes" |
           `Partner notification (contact tracing) requirement indicated by doctor` == "Required" |
           Azithromycin == "Resistant" |Ceftriaxone	 == "Resistant" | Ciprofloxacin == "Resistant" |
           Penicillin == "Resistant" |Spectinomycin == "Resistant" |	Streptomycin == "Resistant" |	
           Sulphatiazole	== "Resistant" |Tetracycline == "Resistant" )


#=============== email success log =============================================
# email_log <- data.frame(
#   `PHESS ID` = c("320245731540", "320245732414", "320245736639", "320245730090", "320245730165"),
#   dr_full_name = c("Dr. John Doe", "Dr. Jane Smith", "Dr. Emily Davis", "Dr. Mark Johnson", "Dr. Sarah Lee"),
#   GPlist_email = c("johndoe@example.com", "janesmith@example.com", "emilydavis@example.com", "markjohnson@example.com", "sarahlee@example.com"),
#   Attachment = c("Report_01.pdf", "Report_02.pdf", "Report_03.pdf", "Report_04.pdf", "Report_05.pdf"),
#   Status = c("Email sent", "Email sent", "Failed", "Failed", "Email sent"),  check.names = FALSE
# )


ESF_status <- ESF_status_1 %>% 
  left_join(email_log %>% select(`PHESS ID`, Status), by = c("PHESS ID" = "PHESS ID")) %>% 
  mutate(ESF = case_when(
    ESF_status == Status ~ "Email sent",
    TRUE ~ Status
  )) %>% 
# mutate(ESF = ifelse(ESF_status == Status, "Email sent", Status)) %>% 
  select(-c(ESF_status,Status))
