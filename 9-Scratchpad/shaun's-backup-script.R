## Shaun's backup script to parse the Scopus XML in case other methods fail
## 20 August 2015

#------- 1. Functionality ----------
library(xml2)
library(doParallel) # Allows parallel processing using foreach library


#------- 2. Configuration ----------
data_directory <- "G:/" # Hard code for now (can programatically unzip later if needed)
data_directories_to_parse <- list("G:/2015-6-25_ANI_00-xml-1/")
data_directories_to_parse_length <- length(data_directories_to_parse)


# Use the configuration below when parsing both types of XML file, ITEM and CITEDBY
#data_directories_to_parse <- as.list(c((paste(data_directory, "ANI-ITEM", sep="")),
#                                      (paste(data_directory,"ANI-CITEDBY", sep="")))) # Hard code for now
#data_directories_to_parse_length <- length(data_directories_to_parse)

# Probably also a good idea to use a helper function to generate the R code to run against each XML file (and to drive the configuration of this helper function, ie what fields where, off a config table)

#------- 3. Define helper functions ----------

get_zip_files_from_directory <- function(data_directory_to_parse){
                                 zip_files_list <- as.list(list.files(path=data_directory_to_parse, pattern="zip$",
                                                                  recursive=TRUE, full.names=TRUE))
                                 return(zip_files_list)
                                 }

get_xml_files_from_inside_zip_file <- function(zip_file_to_parse){
                                       xml_files_list <- as.list(list.files(path=zip_file_to_parse, pattern="xml$",
                                        recursive=TRUE, full.names=TRUE))
                                       return(xml_files_list)
}

list_xml_files_from_directory <- function(data_directory_to_parse){
                                 xml_files_list <- as.list(list.files(path=data_directory_to_parse, pattern="xml$",
                                                   recursive=TRUE, full.names=TRUE))
                                                   return(xml_files_list)
}


#------- 4. Exploration of one directory ----------

d <- 1 # Dummy for a loop later on
xml_files_to_parse <- vector(mode = "list", length = 0) # Set up a list to store the file paths neatly

# Use the helper function above to put the filenames in to the first item of the list preallocated above, count them
xml_files_to_parse[[d]] <- list_xml_files_from_directory(as.character(data_directories_to_parse[d]))
xml_files_to_parse_number <- length(xml_files_to_parse[[d]])


# Set up test
xml_files_to_parse_number <- 10000 # Limit test to 1000 files
fields_by_file_matrix <- matrix(NA, nrow=xml_files_to_parse_number, ncol=5)
# dimnames(fields_by_document_matrix) <- list(NULL, variable_names)
test_start_time <- Sys.time()

# Loop across those files
for (f in 1:xml_files_to_parse_number){
   xml_file_to_parse <- read_xml(as.character(lapply(xml_files_to_parse, `[[`, f)))
      namespace <- xml_ns(xml_file_to_parse)
   fields_by_file_matrix[f, 1] <- xml_text(xml_find_all(read_xml(as.character(xml_file_to_parse)),
                                               "xocs:meta/xocs:eid", namespace))
   fields_by_file_matrix[f, 2] <- xml_text(xml_find_all(read_xml(as.character(xml_file_to_parse)),
                                                        "xocs:meta/xocs:pui", namespace))
   fields_by_file_matrix[f, 3] <- xml_text(xml_find_all(read_xml(as.character(xml_file_to_parse)),
                                                        "xocs:meta/xocs:sort-year", namespace))
   fields_by_file_matrix[f, 4] <- xml_text(xml_find_all(read_xml(as.character(xml_file_to_parse)),
                                                        "xocs:meta/cto:doctype", namespace))
   fields_by_file_matrix[f, 5] <- xml_text(xml_find_all(read_xml(as.character(xml_file_to_parse)),
                                                        "xocs:meta/xocs:issn", namespace))
   #print(paste(f, " of ", xml_files_to_parse_number, " files parsed", sep=""))
}
# Return durations
test_duration <- difftime(Sys.time(), test_start_time, units="secs")
implied_task_duration <- as.numeric((test_duration/xml_files_to_parse_number)*(13000000)/3600)
print(paste("Test of ", xml_files_to_parse_number, " files took ", round(test_duration), " seconds.", sep=""))
print(paste("Test implies 13 million files will take", round(implied_task_duration), "hours to parse, ie",
            round(implied_task_duration/24, digits=1), "days."), sep="")
# Results
# 1000 files
# 1 field, 1000 files in 13 seconds (13 million = 46 hours)
# 2 fields, 1000 files in 19 seconds (13 million = 68 hours)
# 3 fields, 1000 files in 25 seconds (13 million = 89 hours)
# 5 fields, 1000 files in 37 seconds (13 million = 134 hours)
#
# 10,000 files
# 1 field, 10000 files in 164 seconds (13 million = 59 hours)
# 2 fields, 10000 files in 244 seconds (13 million = 88 hours)
# 3 fields, 10000 files in 315 seconds (13 million = 114 hours)
# 5 fields, 10000 files in 433 seconds (13 million = 156 hours)
#
# 20,000 files
# 3 fields, 20000 files in 669 seconds (13 million = 121 hours), seems to scale nicely


# Try loading all the XML docs in to memory first
test_start_time <- Sys.time()

xml_files_to_parse_number <- 10000 # Limit test to 1000 files
xml_files_to_parse_stored_in_list <- vector(mode = "list", length = xml_files_to_parse_number)
fields_by_file_matrix <- matrix(NA, nrow=xml_files_to_parse_number, ncol=5)

for (f in 1:xml_files_to_parse_number){
   xml_files_to_parse_stored_in_list[[f]] <- read_xml(as.character(lapply(xml_files_to_parse, `[[`, f)))
}

# Loop across those files
for (f in 1:xml_files_to_parse_number){
   xml_file_to_parse <- xml_files_to_parse_stored_in_list[[f]]
   namespace <- xml_ns(xml_file_to_parse)
   fields_by_file_matrix[f, 1] <- xml_text(xml_find_all(xml_file_to_parse,
                                                        "xocs:meta/xocs:eid", namespace))
   fields_by_file_matrix[f, 2] <- xml_text(xml_find_all(xml_file_to_parse,
                                                        "xocs:meta/xocs:pui", namespace))
   fields_by_file_matrix[f, 3] <- xml_text(xml_find_all(xml_file_to_parse,
                                                        "xocs:meta/xocs:sort-year", namespace))
   fields_by_file_matrix[f, 4] <- xml_text(xml_find_all(xml_file_to_parse,
                                                        "xocs:meta/cto:doctype", namespace))
   fields_by_file_matrix[f, 5] <- xml_text(xml_find_all(xml_file_to_parse,
                                                        "xocs:meta/xocs:issn", namespace))
   #print(paste(f, " of ", xml_files_to_parse_number, " files parsed", sep=""))
}
# Return durations
test_duration <- difftime(Sys.time(), test_start_time, units="secs")
implied_task_duration <- as.numeric((test_duration/xml_files_to_parse_number)*(13000000)/3600)
print(paste("Test of ", xml_files_to_parse_number, " files took ", round(test_duration), " seconds.", sep=""))
print(paste("Test implies 13 million files will take", round(implied_task_duration), "hours to parse, ie",
            round(implied_task_duration/24, digits=1), "days."), sep="")

# 10,000 files
# 5 fields, 10000 files in 101 seconds (13 million = 134 hours)


## Try all that with old-school XML package
library(XML)
f<-1

xml_file_to_parse <- xmlParse(as.character(lapply(xml_files_to_parse, `[[`, f)))
xml_children <- xmlChildren(xml_file_to_parse)
xml_to_list <- xmlToList(xml_file_to_parse)
xml_multiple_nodes <- xmlRoot(xml_file_to_parse)[["meta"]]
xml_two_values_at_once <- xml_multiple_nodes[c("eid", "pui")]
xml_everything_from_node <- xmlApply(xml_multiple_nodes, xmlValue)
xml_everything_from_node <- xmlApply(xmlRoot(xml_file_to_parse)[["meta"]], xmlValue)


# Try loading all the XML docs in to memory first
test_start_time <- Sys.time()

xml_files_to_parse_number <- 100 # Limit test to 1000 files
xml_files_to_parse_stored_in_list <- vector(mode = "list", length = xml_files_to_parse_number)
fields_by_file_matrix <- matrix(NA, nrow=xml_files_to_parse_number, ncol=5)

for (f in 1:xml_files_to_parse_number){
   xml_files_to_parse_stored_in_list[[f]] <- xmlParse(as.character(lapply(xml_files_to_parse, `[[`, f)))
}

# Loop across those files
for (f in 1:xml_files_to_parse_number){
   xml_file_to_parse <- xml_files_to_parse_stored_in_list[[f]]
      #namespace <- xml_ns(xml_file_to_parse)
   #xml_file_to_parse_as_list <- xmlToList(xml_file_to_parse)
   xml_file_to_parse_meta_node <- xmlRoot(xml_file_to_parse)[["meta"]]
   xml_file_to_parse_meta_node_children <- xmlChildren(xml_file_to_parse_meta_node)
   fields_by_file_matrix[f, 1] <- xmlValue(xml_file_to_parse_meta_node_children[["eid"]])
   fields_by_file_matrix[f, 2] <- xmlValue(xml_file_to_parse_meta_node_children[["pui"]])
   fields_by_file_matrix[f, 3] <- xmlValue(xml_file_to_parse_meta_node_children[["sort-year"]])
   fields_by_file_matrix[f, 4] <- xmlValue(xml_file_to_parse_meta_node_children[["doctype"]])
   fields_by_file_matrix[f, 5] <- xmlValue(xml_file_to_parse_meta_node_children[["issn"]])
   #print(paste(f, " of ", xml_files_to_parse_number, " files parsed", sep=""))
}
# Return durations
test_duration <- difftime(Sys.time(), test_start_time, units="secs")
implied_task_duration <- as.numeric((test_duration/xml_files_to_parse_number)*(13000000)/3600)
print(paste("Test of ", xml_files_to_parse_number, " files took ", round(test_duration), " seconds.", sep=""))
print(paste("Test implies 13 million files will take", round(implied_task_duration), "hours to parse, ie",
            round(implied_task_duration/24, digits=1), "days."), sep="")


# New internal method
# Try loading all the XML docs in to memory first
test_start_time <- Sys.time()

xml_files_to_parse_number <- 10000 # Limit test to 1000 files
xml_files_to_parse_stored_in_list <- vector(mode = "list", length = xml_files_to_parse_number)
fields_by_file_matrix <- matrix(NA, nrow=0, ncol=25)
fields_by_file_stored_in_list <- vector(mode = "list", length = xml_files_to_parse_number)

for (f in 1:xml_files_to_parse_number){
   xml_files_to_parse_stored_in_list[[f]] <- xmlParse(as.character(lapply(xml_files_to_parse, `[[`, f)))
}

# Loop across those files
for (f in 1:xml_files_to_parse_number){
   xml_file_to_parse <- xml_files_to_parse_stored_in_list[[f]]
   fields_by_file_stored_in_list[[f]] <- xmlApply(xmlRoot(xml_file_to_parse)[["meta"]], xmlValue)
}
fields_by_file_matrix <- do.call(rbind, fields_by_file_stored_in_list)
# Return durations
test_duration <- difftime(Sys.time(), test_start_time, units="secs")
implied_task_duration <- as.numeric((test_duration/xml_files_to_parse_number)*(13000000)/3600)
print(paste("Test of ", xml_files_to_parse_number, " files took ", round(test_duration), " seconds.", sep=""))
print(paste("Test implies 13 million files will take", round(implied_task_duration), "hours to parse, ie",
            round(implied_task_duration/24, digits=1), "days."), sep="")

# 141 seconds
# Note that the "give me everything" approach is a bit fragile when faced with missing fields
# A couple of variables flying round at the moment (good to test all combos)
# 1) Load files in to a list first, vs Load on the go (seems first one is faster but memory limitations may bite)








pull_out_one_field <- function(xml_file_to_parse){
   
}

test <- lapply(xml_files_to_parse, )









# Scratchpad


for(d in 1:1){
   xml_files_to_parse <- list_xml_files_from_directory(as.character(data_directories_to_parse[d]))
   zip_files_to_parse_length <- length(zip_files_to_parse)
   #print(xml_children((zip_files_to_parse[1]))) How 
   print(xml_find_all(read_xml(as.character(zipfiles_to_parse[1])), "/"))
}


# Can I really just look at a zipped XML file directly?
# No because there are multiple files inside! Need to unzip first.


# Parallel version (need rscript.exe unblocked as is tripped by registerDoParallel() command in R)
registerDoParallel()
xml_files_to_parse_number <- 1000 # Limit test to 1000 files
test_start_time <- Sys.time()
# Loop across those files
foreach(f = 1:xml_files_to_parse_number, .packages=("xml2")) %dopar%{
   xml_file_to_parse <- read_xml(as.character(lapply(xml_files_to_parse, `[[`, f)))
   namespace <- xml_ns(xml_file_to_parse)
   xml_find_all(read_xml(as.character(xml_file_to_parse)), "xocs:meta/xocs:eid", namespace)
   print(paste(f, " of ", xml_files_to_parse_number, " files parsed", sep=""))
}
# Return durations
test_duration <- (Sys.time() - test_start_time)
print(test_duration)
# Results
