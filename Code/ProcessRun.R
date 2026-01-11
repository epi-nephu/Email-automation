# GP EMAIL AUTOMATION PROJECT

# Process Operation Code
# This code file allows you to run the GP data extraction process using data extracted from PHESS (see instructions doc). 
# Final output will be an Excel file that contains GP details (name, clinic, address, email), case 2x2, case DOB, condition, event date
# This Excel file can be exported to the HP Communicable Disease Secure Sharepoint to be stored in the relevant folder for the Power Automate Run.

# PART 1: GP Data Extraction -----
# You must run Part 1. Ensure that the datafile name is correct and then run the source code line (line 12).
datafile <- "NEPHUCaseLinelistGPExtractionAssignedLPHU.xlsx"

source(here::here("Code", "Process_V2.R"))

# You can ignore the warning messages that are being generated. If however, you get an error message then the code will need to be reviewed as there is error syntax
# if the R object final_table appears in the Environment pane, then the data extraction process was successful and you can proceed with the next stage.





# PART 2: Run Fuzzy Match protocol (OPTIONAL) ----
# Fuzzy Match uses a fuzzy matching algorithm to try and match remaining unmatched entries to the GP list.
# Currently this step is not performed as fuzzy matching score has been very low in the past runs.  The current cut-off score is 0.7
# If you want to run Fuzzy Match, Uncomment and run the code line below (line 26)

# source(here::here("Code", "FuzzyMatching.R"))




# PART 3: Export output file to HP folder ----
# Export file to HP Sharepoint for next stage - integration into tracker which will be performed by the Power Automate Run protocol set up by Esther. 
# File will be save in Sharepoint as YYYY-MM-DD-finaltable.xlsx

one_drive_folder <- str_extract(getwd(), "^.*Health/")
HP_folder <- paste0(one_drive_folder, "NEPHU Health Protection - Communicable Disease Secure - GP Extract Lists (Automated - Do Not Edit) test")

writexl::write_xlsx(final_table, paste0(HP_folder, "/", Sys.Date(), "-finaltable.xlsx"))

# If the file was not successfully exported into the HP Communicable Disease Secure Sharepoint folder, then the folder path was probably incorrect due to OneDrive.
# In this case, uncomment and run the next line replace the argument {HP_folder} with the correct Folder path.
# Or manually copy the file (final_table.xlsx) from the Output folder to the HP Communicable Disease Secure/Other Docs/GP Extract Lists (Automated – Do Not Edit) test folder 
# and rename the file as YYYY-MM-DD-finaltable.xlsx

# writexl::write_xlsx(final_table, paste0(HP_folder, "/", Sys.Date(), "-finaltable.xlsx"))
