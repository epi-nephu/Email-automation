# Load necessary libraries
library(readxl)
library(RDCOMClient)

# Read the sent table
list_sent <- final_combined_result_attach %>% 
  filter(ESF_status == "Email sent")

#dedup names
list_sent <- list_sent %>%
  group_by(Condition, `Full Name`, `Birth Date`, patient_address) %>%
  summarise_all(first) %>%
  ungroup()

## Read the HTML signature ####
signature_path <- "S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Coding and software/R Markdown/Automate ESF Request/Inputs/WPHU_signature.htm"
email_signature <- readLines(signature_path, warn = FALSE)
email_signature <- paste(email_signature, collapse = "\n")

# Create Outlook application object
OutApp <- COMCreate("Outlook.Application")


# Create an empty dataframe to log the results
email_log <- data.frame(dr_full_name = character(),
                        GPlist_email = character(),
                        Attachment = character(),
                        Status = character(),
                        Error_Message = character(),
                        stringsAsFactors = FALSE)

# Loop through each row in the sample table and send an email
for (i in 1:nrow(list_sent)) {
  matched_gp <- list_sent[i, ]
  
  tryCatch({
    # Create an email
    outMail <- OutApp$CreateItem(0)
    
    # Configure email parameters
    outMail[["To"]] <- matched_gp$GPlist_email
    outMail[["subject"]] <- paste0("CONFIDENTIAL: Request for Doctor's Notification, ATTN: DR ", matched_gp$dr_full_name)
    
    # Set the body of the email
    body <- paste0("Dear Dr ", matched_gp$dr_full_name, ",<br><br>",
                   "The Western Public Health Unit is requesting you to complete an online notification form for one of your patients. Please see the attached letter for further details regarding this request. The secure online form for notification can be found at www.health.vic.gov.au/notify.<br><br>",
                   "If you have any further queries, please feel free to contact us on 1800 497 111 or email at WPHU@wh.org.au.<br><br>",
                   "Kind Regards,<br>",
                   email_signature)
    outMail[["HTMLBody"]] <- body
    
    # Add attachment from the 'Link to document' column
    attachmentPath <- matched_gp$pdf_file_path
    outMail[["Attachments"]]$Add(attachmentPath)
    
    # Set the "on behalf of" address
    outMail[["SentOnBehalfOfName"]] <- "WPHU@wh.org.au"
    
    # Send the email
    outMail$Send()
    
    # Log the successful email
    email_log <- rbind(email_log, data.frame(dr_full_name = matched_gp$dr_full_name,
                                             GPlist_email = matched_gp$GPlist_email,
                                             Attachment = attachmentPath,
                                             Status = "Email sent",
                                             Error_Message = NA))
  }, error = function(e) {
    # Log the failed email
    email_log <- rbind(email_log, data.frame(dr_full_name = matched_gp$dr_full_name,
                                             GPlist_email = matched_gp$GPlist_email,
                                             Attachment = matched_gp$pdf_file_path,
                                             Status = "Failed",
                                             Error_Message = e$message))
  })
}


#======================================================== This will create drafts

list_draft <- final_combined_result_attach %>% 
  filter(ESF_status == "Draft saved")

# Create Outlook application object
OutApp <- COMCreate("Outlook.Application")


# Loop through each row in the sample table and send an email
for (i in 1:nrow(list_draft)) {
  matched_gp <- list_draft[i, ]
  
  # Create an email
  outMail <- OutApp$CreateItem(0)
  
  # Configure email parameters
  outMail[["To"]] <- matched_gp$dr_full_name
  outMail[["subject"]] <- paste0("CONFIDENTIAL: Request for Doctor's Notification, ATTN: DR ", matched_gp$dr_full_name)
  
  # Set the body of the email
  body <- paste0("Dear Dr ", matched_gp$dr_full_name, ",<br><br>",
                 "The Western Public Health Unit is requesting you to complete an online notification form for one of your patients. Please see the attached letter for further details regarding this request. The secure online form for notification can be found at www.health.vic.gov.au/notify.<br><br>",
                 "If you have any further queries, please feel free to contact us on 1800 497 111 or email at WPHU@wh.org.au.<br><br>",
                 "Kind Regards,<br>",
                 email_signature)
  outMail[["HTMLBody"]] <- body
  
  # Add attachment from the 'Link to document' column
  attachmentPath <- matched_gp$pdf_file_path
  if (!is.na(attachmentPath) && file.exists(attachmentPath)) {
    outMail[["Attachments"]]$Add(attachmentPath)
  } else {
    warning(paste("Attachment not found for Dr", matched_gp$dr_full_name))
  }
  
  tryCatch({
    # Get the recipient's mailbox folder
    namespace <- OutApp$GetNamespace("MAPI")
    recipientFolder <- namespace$Folders("WPHU@wh.org.au")
    
    # Get the "Draft ESFs" subfolder of the recipient's mailbox
    recipientDraft <- recipientFolder$Folders("Drafts")$Folders("Draft ESFs")
    
    # Save the email into the "Draft ESFs" folder
    outMail$Move(recipientDraft)
  }, error = function(e) {
    message(paste("Error while moving email for Dr", matched_gp$dr_full_name, ":", e$message))
  })
}