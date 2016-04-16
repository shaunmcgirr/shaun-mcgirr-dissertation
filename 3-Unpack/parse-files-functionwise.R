# 3-Construct\parse-files-functionwise.R

# Goals of this script are:
#   - To loop through the unzipped .xml files, parsing them in to a useful data structure
#   - Use functions wherever possible, from parse-files-functions.R

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")

# Tell R where unzipped data is stored (at the end of 3-Unpack/unzip-files.R) and where to send it
data_unzipped_directory <- set_data_subdirectory(data_directory, data_download_date, "unzipped")
data_parsed_directory <- set_data_subdirectory(data_directory, data_download_date, "parsed")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Gather metadata about the regions to be worked on
# regions_list <- generate_regions_list(data_parsed_directory)
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list("Burjatija_Resp")
# regions_list <- as.list("Moskva")
# regions_list <- as.list(c("Adygeja_Resp", "Moskva"))
regions_number <- length(regions_list)

#########################################################################
# 4. Load and parse a configuration file that tells the code what to do #
#########################################################################

# Load and process the configuration file that tells later functions what to process
parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))


################################################
# 5. Begin processing #
################################################

# Begin control loop over regions (no XML processing should be done with loops)
for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])
  region_start_time <- Sys.time()
  
  # Begin control loop over document types
  for(d in 1:length(document_types_list)){
  # document_type <- "notifications"; # document_type <- "contracts"
  document_type <- as.character(document_types_list[d])
  document_type_start_time <- Sys.time()
    
    fields_to_parse <- (parsing_configuration$XMLFieldName[parsing_configuration$DocumentType==document_type]) # loads fields from configuration
    variable_names <- parsing_configuration$VariableName[parsing_configuration$DocumentType==document_type]  

    # Create directories and find the files
    from_directory <- set_data_subdirectory_region(current_region, document_type, "from")
    to_directory <- set_data_subdirectory_region(current_region, document_type, "to")
      dir.create(to_directory, recursive=TRUE)
    files_list <- generate_files_list(from_directory)
      files_list_length <- length(files_list) 
    namespace <- xml_ns(read_xml(files_list[[1]])) # Hardcode namespace based on  first file in list

    # Batch up the list in to chunks
    batch_size <- 3 # batch_size = 3 gives safe memory performance when not processing multicore
      number_of_batches <- ceiling(files_list_length/batch_size)
      batch_number <- gl(number_of_batches, batch_size, length = files_list_length)
    batch_list <- split(files_list, batch_number)

    # Subset of batches during development
    # batch_list <- batch_list[1:10]

    # Run batch_list through process_batch_key_value() which in turn calls functions that:
      # 1. Find the distinct XML documents in each batch using find_documents_in_this_batch()
      #    which in turn calls load_documents_from_file() to read the XML files
      # 2. Run each found document through output_document_key_value() using fields_to_parse
      #    loaded above, which does the actual parsing and collection of outputs
      # 3. Bind all the output together with do.call("rbind", X)
    
  # for(z in 1:length(batch_list)){
    # batch_output_list_key_value <- lapply(batch_list, process_batch_key_value)
    batch_output_list_key_value <- mclapply(batch_list, process_batch_key_value, mc.cores = number_of_cores, mc.preschedule = F)
    # batch_output_list_key_value <- process_batch_key_value(batch_to_process = batch_list[[z]])
  # }
    batch_output_key_value <- as.data.frame(do.call("rbind", batch_output_list_key_value),
                                            stringsAsFactors = F)
    # batch_output_key_value$Document <- as.factor(batch_output_key_value$Document)
      batch_output_key_value$Version <- as.numeric(batch_output_key_value$Version)
      filename <- paste0(to_directory, "/", current_region, "_", document_type, "_parsed_key_value_",
                          data_download_date, ".rda")
      save(batch_output_key_value, file=filename)
    # Clean up
    rm(list = c("files_list", "namespace", "batch_list", "batch_output_list_key_value", "batch_output_key_value"))
    gc()
    
    # Report time for this document type
    document_type_processing_time <- (Sys.time() - document_type_start_time)
    print(paste("Processing", document_type, "from", current_region, "took", sep=" "))
    print(document_type_processing_time)
  } # Closes control loop over document_types_list in this region
  
  # Report time for this region as a whole
  region_processing_time <- (Sys.time() - region_start_time)
  print(paste("Processing", current_region, "took", sep=" "))
  print(region_processing_time)
} # Closes control loop over regions_list

  
################################################
# 5. SCRATCHPAD #
################################################

STOP <- 1
if(STOP == 1) stop("Stopping here")

# Stitch it back toegether again and work out what it means
load(file = "~/data/zakupki/2015-06-13/zakupki-2015-06-13-parsed-data/94fz/Adygeja_Resp/contracts/Adygeja_Resp_contracts_parsed_key_value_2015-06-13.rda")
  contracts <- batch_output_key_value
load(file = "~/data/zakupki/2015-06-13/zakupki-2015-06-13-parsed-data/94fz/Adygeja_Resp/notifications/Adygeja_Resp_notifications_parsed_key_value_2015-06-13.rda")
  notifications <- batch_output_key_value
rm(batch_output_key_value)

contracts_missing <- contracts[!complete.cases(contracts),]
  contracts_missing_table <- as.data.frame(table(contracts_missing$Key)) %>% arrange(-Freq)
notifications_missing <- notifications[!complete.cases(notifications),]
  notifications_missing_table <- as.data.frame(table(notifications_missing$Key)) %>% arrange(-Freq)

# Add in the loaded XML and calculate documents per row, reduce to files that have useful XML
parsed_data_matrix$ParsedData <- documents_in_this_directory_list
parsed_data_matrix$DocumentCount <- as.numeric(lapply(documents_in_this_directory_list, function(x) length(x)))
parsed_data_matrix <- parsed_data_matrix[parsed_data_matrix$DocumentCount > 0, ]
sum(parsed_data_matrix$DocumentCount)


# Navigating XML documents
xml_type(document_to_parse)
xml_siblings(document_to_parse)
xml_children(document_to_parse)
xml_attr()
xml_name(document_to_parse)
xml_path(document_to_parse)
xml_structure(document_to_parse)
xml_text(xml_find_all(document_to_parse, "/*/*[4]"))

test_list <- as_list(document_to_parse)
test_list$notificationNumber


# Version using lapply on the whole list (no batch)
# Load files (loop first, then lapply, then lapply on a truncated list that fits in memory)
documents_in_this_directory_list <- lapply(files_list, load_documents_from_file)
documents_in_this_directory_list <- remove_empty_lists(documents_in_this_directory_list)
# documents_in_this_directory_list_length <- length(documents_in_this_directory_list)

# Process this list in to one object per XML node
documents_in_this_directory_list <- unlist(documents_in_this_directory_list, recursive = FALSE)

# Process a test case
testing_output <- lapply(documents_in_this_directory_list, output_document, fields_to_parse = fields_to_parse)
head(testing_output)
testing_output[[1]]
testing_output_data_frame <- do.call("rbind", testing_output)
colnames(testing_output_data_frame) <- parsing_configuration$VariableName[parsing_configuration$DocumentType == document_type]


# ENDS