# GP EMAIL AUTOMATION PROJECT

# Process Operation Code
# This code file allows you to run the GP data extraction process using data extracted from PHESS (see instructions doc)

datafile <- "NEPHUCaseLinelistGPExtractionAssignedLPHU.xlsx"

source(here::here("Code", "Process_V2.R"))


# Run Fuzzy Match protocol
# Fuzzy Match uses a fuzzy matching algorithm to try and match remaining unmatched entries to the GP list.
# Currently this step is not performed as fuzzy matching score has been very low in the past runs.  The current cut-off score is 0.7
# If you want to run Fuzzy Match, run the code line below.

source(here::here("Code", "FuzzyMatching.R"))

