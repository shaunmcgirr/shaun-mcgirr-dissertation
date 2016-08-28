# 4-Construct\clean-data.R

# Goals of this script are:
#   - Obtain list of regions which have completed step 3 (unpack)
#   - Process these to clear up data quality issues, eg duplicates

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")

# Define location of parsed data
data_parsed_directory <- set_data_subdirectory(data_directory, data_download_date, "parsed")

# Define target of cleaned data
data_cleaned_directory <- set_data_subdirectory(data_directory, data_download_date, "cleaned")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# regions_available_to_clean <- generate_regions_list(data_parsed_directory)
# regions_already_cleaned <- generate_regions_list(data_cleaned_directory)
# regions_list <- setdiff(regions_available_to_clean, regions_already_cleaned)
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

##############################################
# 3. Define functions to process each region #
##############################################

# Wrap the code written below in to functions

for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])

  # Begin control loop over document types
  for(d in 1:length(document_types_list)){
      # document_type <- "notifications"; # document_type <- "contracts"; # document_type <- "protocols"
    document_type <- as.character(document_types_list[d])
    
    # Load the next file to process
    file_to_process <- paste0(data_parsed_directory, current_region, "/", document_type, "/",
                              current_region, "_", document_type, "_parsed_key_value_",
                              data_download_date, ".rda")
    load(file=file_to_process)
    
  ### DATA QUALITY STEPS ###
    # Drop rows where version is NA, as these are very low-quality data (almost all fields actually missing)
    # STILL NECESSARY? Results in taking Moscow from 414383 to 401636 contracts
    # batch_output_key_value <- filter(batch_output_key_value, !is.na(Version))
    # gc()
    
    # Concatenate document ID with version number (doesn't resolve duplicates!)
    batch_output_key_value$DocumentVersion <- as.character(paste(batch_output_key_value$Document,
                                                               batch_output_key_value$Version,
                                                               sep = "."))
    gc()
    
    # Count number of docs parsed from the XML to produce file, generate a unique ID for each
    if(document_type == "contracts"){number_of_documents_parsed <- length(batch_output_key_value$Key[batch_output_key_value$Key == "oos:id"])}
    if(document_type == "notifications"){number_of_documents_parsed <- length(batch_output_key_value$Key[batch_output_key_value$Key == "oos:id"])}
    if(document_type == "protocols"){number_of_documents_parsed <- length(batch_output_key_value$Key[batch_output_key_value$Key == "oos:protocolNumber"])}
    UniqueID <- seq(number_of_documents_parsed)
    
    # Not slow
    # Work out which rows are associated with a change to a new document (as identified by oos:id)
    rows_with_UniqueID <- batch_output_key_value %>%
                                 add_rownames() %>%
                                 filter(Key == "oos:id" | Key == "oos:protocolNumber") %>%
                                 select(rowname) %>%
                                 cbind(UniqueID)
    
    # Very slow but no memory issues
    # Join these unique IDs on
    batch_output_key_value <- batch_output_key_value %>%
                               add_rownames() %>%
                              left_join(rows_with_UniqueID, by = "rowname") %>%
                              mutate(UniqueID = na.locf(UniqueID))
    rm(rows_with_UniqueID, UniqueID)

    # Concatenate all our identifiers to provide a completely unique identifier for each doc parsed
    batch_output_key_value$DocumentVersionUniqueID <- paste(batch_output_key_value$DocumentVersion, batch_output_key_value$UniqueID, sep = ".")
    gc()
    
    # We actually care about having the best data for each entity, in this case "notificationNumber", extract these
    business_keys <- batch_output_key_value %>%
                            filter(Key == "oos:notificationNumber" | Key == "oos:foundation/oos:order/oos:notificationNumber") %>% # Not oos:regNum!
                            # filter(Key == "oos:notificationNumber" | Key == "oos:protocolNumber") %>% 
                            select(DocumentVersionUniqueID, Value) %>%
                            rename(BusinessKey = Value) %>%
                            filter(BusinessKey != "") # Some strange empty-but-not-NA cells
  
    # Quite slow but not too rough on memory    
    # Merge those on
    batch_output_key_value <- batch_output_key_value %>%
                                left_join(business_keys, by = "DocumentVersionUniqueID")
    rm(business_keys)
    gc()
    
       # Find out which document has the best data for a given notificationNumber (protocolNumber for protocols)
    if(document_type == "contracts" | document_type == "notifications"){
      most_fields <- batch_output_key_value %>%
      filter(!is.na(Value)) %>%
      group_by(BusinessKey, DocumentVersionUniqueID) %>%
      tally(n()) %>% ungroup() %>%
      arrange(BusinessKey, -n, xtfrm(DocumentVersionUniqueID)) %>%
      distinct(BusinessKey, .keep_all = T) %>% # Returns most recently parsed doc for each notificationNumber (out of docs with most fields)
      select(-n)} 
    
    # Will need to take two passes at protocols (see https://trello.com/c/xpJCDmW9/52-parse-protocols-for-bids)
    # For protocols, 0158300029813000018 is a good case here.
    ## How many versions per protocol?
    # 0176300001511000001 is good case - fields can change order without substance changing
    # 0176300003713000019 another good case, two "version 1", one with better data
    # versions_per_protocol <- batch_output_key_value %>%
    #   group_by(Document) %>%
    #     summarize(VersionsPerDocument = n_distinct(UniqueID))
    
    if(document_type == "protocols"){
      most_fields <- batch_output_key_value %>%
        filter(!is.na(Value)) %>%
        group_by(DocumentVersion, DocumentVersionUniqueID) %>%
        tally(n()) %>% ungroup() %>%
        arrange(DocumentVersion, -n, -xtfrm(DocumentVersionUniqueID)) %>%
        distinct(DocumentVersion, .keep_all = T) %>% # Returns most recently parsed doc for each notificationNumber (out of docs with most fields)
        select(-n)} 
    
    
    # Very slow (probably a smarter way to do this, eg assign simpler number to each)
    # Might also want to preallocate this and other large objects
    # Use info above to cut down parsed data frame to just the best documents
    if(document_type == "contracts" | document_type == "notifications"){
      one_version_per_document <- batch_output_key_value %>%
      right_join(most_fields, by = c("DocumentVersionUniqueID", "BusinessKey")) %>%
      select(BusinessKey, DocumentVersionUniqueID, Key, Value) %>%
      filter(!is.na(BusinessKey))}
    
    if(document_type == "protocols"){
      one_version_per_document <- batch_output_key_value %>%
        right_join(most_fields, by = c("DocumentVersionUniqueID", "DocumentVersion")) %>%
        select(BusinessKey, DocumentVersion, DocumentVersionUniqueID, Key, Value) %>%
        filter(!is.na(BusinessKey))}
    
    # Still gives same number of unique business keys for Moscow: 548,436 notifications; 414,382 contracts
    rm(most_fields)
    
    # Save the cleaned data (renaming object to something useful beforehand)
    to_directory <- paste0(data_cleaned_directory, current_region)
      suppressWarnings(dir.create(to_directory, recursive=TRUE))
    filename <- paste0(to_directory, "/", current_region, "_", document_type, "_cleaned_",
                       data_download_date, ".rda")
    if(document_type == "contracts"){contracts_cleaned <- one_version_per_document; save(contracts_cleaned, file=filename); rm(contracts_cleaned)}
    if(document_type == "notifications"){notifications_cleaned <- one_version_per_document; save(notifications_cleaned, file=filename); rm(notifications_cleaned)}
    if(document_type == "protocols"){protocols_cleaned <- one_version_per_document; save(protocols_cleaned, file=filename); rm(protocols_cleaned)}
    
    # Print how many rows that got rid of!
    print(paste0(current_region, "/", document_type, ": ",
                 "Reducing the parsed data to one document per business key reduced the initial row count (",
                 length(batch_output_key_value$Document), ") by ",
                 round((1-length(one_version_per_document$BusinessKey)/length(batch_output_key_value$Document))*100, digits = 1),
                 "% to ", length(one_version_per_document$BusinessKey)))
    
    # Clean up
    rm(one_version_per_document); rm(batch_output_key_value)
    gc()
    
  } # Closes control loop over document_types_list in this region
  
} # Closes control loop over regions_list
                    

# ENDS
