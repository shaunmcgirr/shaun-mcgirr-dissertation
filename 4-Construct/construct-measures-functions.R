# 4-Construct\construct-measures-functions.R

# Goals of this script are:
#   - Provide functions to other parsing scripts to make their lives easier

###################
# 1. Housekeeping #
###################



#######################
# 2. Helper functions #
#######################

# Transform region names in to English for graphing purposes
generate_english_region_name <- function(region){
                                  current_region_english <- gsub("_", " ", region) 
                                  current_region_english <- gsub("Resp", "Republic", current_region_english)
                                  current_region_english <- gsub("obl", "Oblast", current_region_english)
                                  current_region_english <- gsub("kraj", "Krai", current_region_english)
                                  current_region_english <- gsub("Moskva", "Moscow", current_region_english)
                                  }


###########################
# 3. Processing functions #
###########################

# Examine a data frame of parsed key-value output, and return the latest-parsed version of each document



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
