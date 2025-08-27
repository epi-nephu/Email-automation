library(officer)
library(rmarkdown)

# Define the output directories for Word and PDF documents ####
output_word <- "S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Outputs/Word/"
output_pdf <- "S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Outputs/PDF/"


# # Function to create Word documents  ####
# create_word_doc <- function(phess_id, data, output_dir) {
#   doc <- read_docx("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/B - Gonococcal infection.docx")
#   doc <- doc %>%
#     body_replace_all_text('<Today Date>', todays_date, only_at_cursor = FALSE) %>%
#     body_replace_all_text('PHESS ID', phess_id, only_at_cursor = FALSE, ignore.case = TRUE) %>%
#     body_replace_all_text('<Name>', data$full_name, only_at_cursor = FALSE, ignore.case = TRUE) %>%
#     body_replace_all_text('<Birth date>', data$birth_date, only_at_cursor = FALSE, ignore.case = TRUE) %>%
#     body_replace_all_text('Case Address', data$patient_address, only_at_cursor = FALSE, ignore.case = TRUE) %>%
#     body_replace_all_text('<Full name>', data$dr_full_name, only_at_cursor = FALSE, ignore.case = TRUE) %>%
#     body_replace_all_text('<Addressee details>', paste0(data$GPlist_address, ", ", data$GPlist_suburb, ", ", data$GPlist_postcode), only_at_cursor = FALSE, ignore.case = TRUE)
#   
#   file_path <- paste0(output_word,phess_id,  " - GN.docx")
#   print(doc, target = file_path)
#   return(file_path)
# }

# Example to convert today's date to a formatted string
todays_date <- as.character(Sys.Date())  # or format(Sys.Date(), "%Y-%m-%d") if you need a specific format

create_word_doc <- function(phess_id, data, output_dir) {
  # Ensure data contains only one row
  data <- data %>% slice(1)
  
  # Convert birth_date to character
  birth_date <- as.character(data$birth_date)
  
  addressee_details <- paste0(coalesce(as.character(data$GPlist_address), ""), 
                              ", ", 
                              coalesce(as.character(data$GPlist_suburb), ""), 
                              ", ", 
                              coalesce(as.character(data$GPlist_postcode), ""))
  
  doc <- read_docx("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/B - Gonococcal infection.docx")
  
  doc <- doc %>%
    body_replace_all_text('<Today Date>', todays_date, only_at_cursor = FALSE) %>%
    body_replace_all_text('PHESS ID', phess_id, only_at_cursor = FALSE, ignore.case = TRUE) %>%
    body_replace_all_text('<Name>', data$full_name, only_at_cursor = FALSE, ignore.case = TRUE) %>%
    body_replace_all_text('<Birth date>', birth_date, only_at_cursor = FALSE, ignore.case = TRUE) %>%
    body_replace_all_text('Case Address', data$patient_address, only_at_cursor = FALSE, ignore.case = TRUE) %>%
    body_replace_all_text('<Full name>', data$dr_full_name, only_at_cursor = FALSE, ignore.case = TRUE) %>%
    body_replace_all_text('<Addressee details>', addressee_details, only_at_cursor = FALSE, ignore.case = TRUE)
  
  file_path <- paste0(output_word, phess_id, " - GN.docx")
  print(doc, target = file_path)
  return(file_path)
}



# Iterate over each PHESS ID  ####
for (phess_id in final_combined_result_pull) {
  data <- filter(final_combined_result_notHC, phess_id == phess_id)
  
  if (nrow(data) > 0) {
    create_word_doc(phess_id, data)
  } else {
    print(paste("No data found for PHESS ID:", phess_id))
  }
}


# Convert Word documents to PDFs  ####
convert_to_pdf <- function(input_file, output_file) {
  # Create a temporary YAML file for Pandoc options
  yaml_file <- tempfile(fileext = ".yaml")
  cat("
---
documentclass: article
geometry: margin=2cm, top=5.5cm
header-includes: |
  \\usepackage{graphicx}
  \\usepackage[
      automark,
      headwidth=paper,
      footwidth=head
  ]{scrlayer-scrpage}
  \\DeclareNewLayer[
      background,
      topmargin,
      mode=picture,
      contents=\\putUL{\\raisebox{-\\height}{\\includegraphics[width=\\layerwidth]{\"S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/header.jpeg\"}}}
  ]{topmargin}
  \\DeclareNewLayer[
      background,
      bottommargin,
      mode=picture,
      contents=\\putLL{\\includegraphics[width=\\layerwidth]{S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/footer_full.jpeg}}
  ]{bottommargin}
  \\AddLayersToPageStyle{@everystyle@}{topmargin,bottommargin}
  \\clearpairofpagestyles
  \\chead{\\headmark}
  \\ihead{}
  \\ohead{}
---
", file = yaml_file)
  
  # Specify the Pandoc executable path (ensure it is correctly set) 
  pandoc_executable <- "pandoc"  # Replace with the correct path if necessary
  
  # Create the Pandoc command with custom margins and template
  pandoc_command <- paste(
    shQuote(pandoc_executable),             # Pandoc executable
    "--pdf-engine=xelatex",                 # Specify PDF engine (xelatex recommended for complex documents)
    "--metadata-file", shQuote(yaml_file),  # Use the YAML metadata file
    shQuote(input_file),                    # Input file
    "-o", shQuote(output_file)              # Output file
  )
  
  # Run the Pandoc command
  system(pandoc_command)
  
  # Clean up temporary file
  unlink(yaml_file)
}

# Pull list of .docx files in the output directory  ####
docx_files <- list.files(output_word, pattern = "\\.docx$", full.names = TRUE)

# Convert each .docx file to .pdf  ####
for (docx_file in docx_files) {
  output_file <- gsub("Word", "PDF", gsub("\\.docx$", ".pdf", docx_file))
  convert_to_pdf(docx_file, output_file)
}

