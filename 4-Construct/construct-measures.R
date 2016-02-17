# 4-Construct\construct-measures.R

# Goals of this script are:
#   - Obtain list of regions which have completed step 3 (unpack)
#   - Process these to create measures to test my theory
#   - Profit!!!

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")

# Change in to the directory where parsed data are stored (at the end of step 3)
# setwd('~/data/zakupki/2015-06-13/zakupki-2015-06-13-parsed-data/')
data_parsed_directory <- set_data_subdirectory(data_directory, data_download_date, "parsed")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_parsed_directory)
regions_list <- as.list("Adygeja_Resp")
regions_number <- length(regions_list)

# Define target of processed data
data_constructed_directory <- set_data_subdirectory(data_directory, data_download_date, "constructed")

##############################################
# 3. Define functions to process each region #
##############################################

# Wrap the code written below in to functions

for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])

  # Begin control loop over document types
  for(d in 1:length(document_types_list)){
    document_type <- "notifications"
    # document_type <- as.character(document_types_list[d])  
    
    file_to_process <- paste0(data_parsed_directory, current_region, "/", document_type, "/",
                              current_region, "_", document_type, "_parsed_key_value_",
                              data_download_date, ".rda")
    
    # Steps for processing key-value in to wide format ready for merging
    batch_output_key_value$DocumentVersion <- as.numeric(paste(batch_output_key_value$Document,
                                                               batch_output_key_value$Version,
                                                               sep = "."))
    
    number_of_documents_parsed <- batch_output_key_value %>%
                                    filter(Key == "oos:id")
    
    # 16428 parsed, for 15276 oos:id fields
    # Try counting number of rows under each oos:id, this is how many times Counter should be rep
    simplified <- batch_output_key_value %>% select(Document, Key) %>% add_rownames()
    
    unique_numbers <- seq(16428)
    
    # Find where new oos:id appears, this - 1 is number of times to repeat each unique_number
    rows_where_unique_number_changes <- simplified %>% 
                                          filter(Key == "oos:id") %>%
                                          select(rowname)
    
    target_number_of_rows = length(batch_output_key_value$Document)
    
    shifted_vector <- rows_where_unique_number_changes[2:nrow(rows_where_unique_number_changes), 1]
    shifted_vector <- rbind(shifted_vector, (target_number_of_rows+1))
    
    number_of_times_to_repeat <- cbind(rows_where_unique_number_changes, shifted_vector) 
      colnames(number_of_times_to_repeat) <- c("FirstRow", "LastRow")
    
    
    number_of_times_to_repeat$NumberOfTimesToRepeat = as.numeric(number_of_times_to_repeat$LastRow) - 
                                                      as.numeric(number_of_times_to_repeat$FirstRow)
    
    unique_numbers_vector <- rep(unique_numbers, number_of_times_to_repeat$NumberOfTimesToRepeat)
    
    
    merged <- cbind(batch_output_key_value, unique_numbers_vector)

    unique_numbers_per_document <- merged %>%
                                    select(Document, unique_numbers_vector) %>%
                                    distinct(Document, unique_numbers_vector) %>%
                                    group_by(Document) %>%
                                    tally(n())
    
    most_fields <- merged %>%
                       # mutate(DocumentVersionUnique = paste(DocumentVersion, unique_numbers_vector, sep = ".")) %>%
                       # mutate(Document = as.numeric(Document), 
                       #        DocumentVersionUnique = as.numeric(DocumentVersionUnique)) %>%
                        group_by(Document, unique_numbers_vector) %>%
                          tally(n()) %>% ungroup() %>%
                        arrange(Document, -n, -unique_numbers_vector) %>%
                        distinct(Document) # Returns most recently parsed of doc versions with most fields

    one_version_per_document <- merged %>%
                                  right_join(most_fields, by = c("Document", "unique_numbers_vector"))
                    
    

    
    
    # 1. Determine latest version
    versions <- batch_output_key_value %>%
      group_by(Document) %>%
      summarise(HighestDocumentVersion = max(DocumentVersion)) 
    
    most_recent_document_versions <- batch_output_key_value %>%
                                      group_by(Document) %>%
                                        summarise(HighestDocumentVersion = max(DocumentVersion)) %>% ungroup() %>%
                                      inner_join(batch_output_key_value, by = c("HighestDocumentVersion" = "DocumentVersion"))
      
      filter(DocumentVersion == HighestDocumentVersion)
    
    # Number of versions per document
      versions_per_document <- batch_output_key_value %>%
                                  filter(Key == "oos:id") %>%
                                  group_by(Document, Version, Value, DocumentVersion) %>%
                                  tally(n())
      # So there are duplicate documents not separated by different version number...
      
      # For a given DocumentVersion we need to only present the most frequent rows
      duplicated_rows <- batch_output_key_value %>%
                          group_by(Document, Version, Key, Value) %>%
                          filter(n()>1)
      # Easier to change the parser to count actual documents on its way through
                                
        group_by(Document) %>%
                                  tally(length(unique(DocumentVersion)))
    
    %>%
      mutate(MoreThanOneVersion = HighestDocumentVersion - as.numeric(Document)) %>%
      filter(MoreThanOneVersion > 0.1)
    
    #     batch_output_key_value$Document <- as.numeric(levels(batch_output_key_value$Document))[batch_output_key_value$Document]
#     batch_output_key_value$Value <- as.numeric(levels(batch_output_key_value$Value))[batch_output_key_value$Value]
    

    # 
    
    # Measure 1: what is the difference between notified and contracted price?
    notifications <- batch_output_key_value %>%
                    filter(Key == "oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice") %>%
                    mutate(Notification)
                    group_by(Document, Version) %>%
                    summarise(NotificationTotalMaxPrice = sum(as.numeric(Value)))

    contracts <- 
    
################################################
# 4. Loop over regions to process them in turn #
################################################

# List from 2. above is called here
region.processed <- ("Moskva")

data.consolidated.notifications.filename <- paste(region.processed,"notifications","df","RData", sep=".")

data.consolidated.notifications <- load(data.consolidated.notifications.filename)

