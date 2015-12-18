# 3-Construct\parse-files-functions.R

# Goals of this script are:
#   - Provide functions to other parsing scripts to make their lives easier

###################
# 1. Housekeeping #
###################



#######################
# 2. Helper functions #
#######################

# Helper function for creating subdirectory paths
set_data_subdirectory <- function(data_directory, data_download_date, subdirectory){
  data_subdirectory <- paste0(data_directory, data_download_date, "/zakupki-", data_download_date, "-", subdirectory, "-data/94fz/")
  # return(data_subdirectory)
}

# Helper function for loading the per-region directories to process
set_data_subdirectory_region <- function(current_region, document_type, direction){
  if(direction=="from") return(paste0(data_unzipped_directory, current_region, "/", document_type)) # loads source directory
  if(direction=="to") return(paste0(data_parsed_directory, current_region, "/", document_type)) # loads target directory
}

# Function to generate metadata about the regions to be processed
generate_regions_list <- function(data_subdirectory){
  regions_list <- as.list(list.files(path=data_subdirectory))
  regions_list <- Filter((function(x) !grepl('Sevastopol_g', x)), regions_list) # Remove Sevastopol
  # Filter out non-regions in to their own list, remove the log files from list of regions
  # others_list <- Filter((function(x) (x %in% c("_FCS_nsi", "nsi"))), regions_list)
}

# Function to make a list of the files inside a directory
generate_files_list <- function(directory){
  files_list <- as.list(list.files(directory, pattern="xml$", recursive=TRUE, full.names=TRUE))  
  # files_list_length <- length(files_list) 
  }

# Remove empty lists from inside lists
remove_empty_lists  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

# Parse an xml field
parse_field <- function(document_to_parse, x){
  iconv(xml_text(xml_find_all(document_to_parse, x, ns = namespace)), from = "UTF-8", to = "UTF-8")
}


###########################
# 3. Processing functions #
###########################

# Load documents inside a file to a list
load_documents_from_file <- function(file_to_load){
  # file_to_load_xml <- read_xml(as.character(file_to_load))
    # namespace <- xml_ns(file_to_load_xml)
  documents_in_this_file_list <- xml_children(read_xml(as.character(file_to_load)))
  if(length(documents_in_this_file_list) > 0) return(documents_in_this_file_list)
  # if(length(documents_in_this_file_list) > 0) return(cbind(documents_in_this_file_list, rep(as.character(file_to_load), times=length(documents_in_this_file_list))))
  # documents_in_this_file_list <- (xml_find_all(file_to_parse, document_id_field, namespace))
}

# Function to process a single XML document in to a list per key-value pair within a document
output_document_key_value <- function(document_to_parse, fields_to_parse){
# for(p in 1:length(documents_in_this_batch)){ # Loop here for debugging only
  # document_to_parse <- documents_in_this_batch[[p]]
  output_fields <- lapply(fields_to_parse, function(x) rbind(x, if(length(parse_field(document_to_parse, x))>0) parse_field(document_to_parse, x) else NA ))
  output_fields <- matrix(unlist(output_fields, recursive = FALSE), ncol = 2, byrow = TRUE)
  output_fields <- cbind(output_fields[1, 2], output_fields)
    colnames(output_fields) <- c("Document", "Key", "Value")
  return(output_fields)
# }
}

# Function to process a single XML document in to a list per document
output_document <- function(document_to_parse, fields_to_parse){
  output_fields <- lapply(fields_to_parse, function(x) parse_field(document_to_parse, x))
}


# Function to turn batch_list item containing a file path in to list of documents
find_documents_in_this_batch <- function(batch_list_item){
  documents_in_this_batch <- lapply(batch_list_item, load_documents_from_file)
  documents_in_this_batch <- unlist(remove_empty_lists(documents_in_this_batch), recursive = FALSE)
}

# Function to process the whole batch at once
process_batch <- function(batch_to_process, batch_sequence){
  documents_in_this_batch <- find_documents_in_this_batch(batch_to_process)
  batch_output <- lapply(documents_in_this_batch, output_document, fields_to_parse = fields_to_parse)
  # return(batch_output)
  #   batch_output_file_name <- paste0(data_parsed_directory, current_region, "/", document_type, "/",
  #                                    current_region, "_", document_type, "_", batch_sequence, ".rda")
  # batch_output_data_frame <- do.call("rbind", batch_output)
  # save(batch_output_data_frame, file = batch_output_file_name)
  # return(batch_output_file_name)
  # output_list <- list(batch_output_file_name, batch_output_data_frame)
  # rm(list = c("documents_in_this_batch", "batch_output", "batch_output_data_frame"))
  rm("documents_in_this_batch")
  # return(output_list)
  return(batch_output)
}

# Function to process the whole batch at once in to key-value pairs
process_batch_key_value <- function(batch_to_process, batch_sequence){
  documents_in_this_batch <- find_documents_in_this_batch(batch_to_process)
  batch_output <- lapply(documents_in_this_batch, output_document_key_value, fields_to_parse = fields_to_parse)
  batch_output <- do.call("rbind", batch_output)
  rm("documents_in_this_batch")
  return(batch_output)
}


################################################
# 5. SCRATCHPAD #
################################################


# Create a matrix ready to receive raw output from parsing (before we know how many documents) including 1) document type, 2) stored output, 3) metadata
# create_parsed_data_matrix <- function(document_type, parsed_data_matrix_length){
#   matrix(c(rep(document_type, parsed_data_matrix_length), rep(NA, parsed_data_matrix_length), rep(NA, parsed_data_matrix_length)),
#          nrow=parsed_data_matrix_length, ncol=3, dimnames=list(NULL, c("DocumentType", "ParsedData", "DocumentCount")))
# }

# Separate items from a list in to standalone items
# separate_items_from_list <- function(list_to_separate){
#   number_of_items_in_list <- length(list_to_separate)
#   for (i in 1:number_of_items_in_list){
#     print(list_to_separate[i])
#   }  
#   lapply(list_to_separate, function(x) x)
# }

# Unwind the documents stuck inside lists stuck (in turn) inside cells of data frame so we have a list of singular objects
# unwind_list_of_documents <- function(list_to_unwind){
#   number_of_documents <- sum(as.numeric(lapply(list_to_unwind, function(x) length(x))))
#   list_of_documents <- vector(mode = "list", length = number_of_documents)
# }

# Create a matrix ready to receive 1) a document type, 2) document ID, and 3) a list of key-value pairs
# create_key_value_matrix <- function(document_type, key_value_matrix_length){
#   matrix(c(rep(document_type, key_value_matrix_length), rep(NA, key_value_matrix_length), rep(NA, key_value_matrix_length)),
#          nrow=key_value_matrix_length, ncol=3, dimnames=list(NULL, c("DocumentType", "DocumentID", "KeyValues")))
# }
