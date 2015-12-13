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
# regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

#########################################################################
# 4. Load and parse a configuration file that tells the code what to do #
#########################################################################

# Load and process the configuration file that tells later functions what to process
parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))


################################################
# 5. Begin processing #
################################################

# Replace this with a loop over regions, later on
r <- 1
current_region <- as.character(regions_list[r])

# Specify a document_type by hand, to pass to functions (later store it in a metadata table based off parsing config, containing eg region name, a list of files for that region, and other data, making lapply easier)
document_type <- "notifications"
  fields_to_parse <- (parsing_configuration$XMLFieldName[parsing_configuration$DocumentType==document_type]) # loads fields from configuration
  # fields_to_parse_length <- length(fields_to_parse) # Probably no longer needed
  variable_names <- parsing_configuration$VariableName[parsing_configuration$DocumentType==document_type]  

# Create directories and find the files
from_directory <- set_data_subdirectory_region(current_region, document_type, "from")
to_directory <- set_data_subdirectory_region(current_region, document_type, "to")
  dir.create(to_directory, recursive=TRUE)
files_list <- generate_files_list(from_directory)
  files_list_length <- length(files_list) 
namespace <- xml_ns(read_xml(files_list[[1]])) # Hardcode the namespace based on the first file in the list

# Batch up the list in to chunks
batch_size <- 3
  number_of_batches <- ceiling(files_list_length/batch_size)
  batch_number <- gl(number_of_batches, batch_size, length = files_list_length)
batch_list <- split(files_list, batch_number)

# Subset of batches during development
batch_list <- batch_list[1:10]

# Process the batch (this could be parallelised, also try the version that saved/removed again)
testing_batch_output <- lapply(batch_list, process_batch)
# testing_batch_output_mapply <- mapply(process_batch, batch_to_process = batch_list, 
#                                       batch_sequence = seq_along(batch_list))
testing_batch_output_data_frame <- do.call("rbind", unlist(testing_batch_output, recursive = FALSE))
colnames(testing_batch_output_data_frame) <- parsing_configuration$VariableName[parsing_configuration$DocumentType == document_type]



  
################################################
# 5. SCRATCHPAD #
################################################

STOP <- 1
if(STOP == 1) stop("Stopping here")

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