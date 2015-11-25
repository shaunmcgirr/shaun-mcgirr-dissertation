# 3-Construct\parse-files.R

# Goals of this script are:
#   - To loop through the unzipped .xml files, parsing them in to a useful data structure

###################
# 1. Housekeeping #
###################

# Change in to the directory where unzipped data is stored (at the end of unzip-files.R)
data_unzipped_directory <- paste(data_directory, data_download_date, "/", "zakupki-", data_download_date, "-unzipped-data/94fz/", sep="")

# Define target directory for processed data
data_parsed_directory <- paste(data_directory, data_download_date, "/", "zakupki-", data_download_date, "-parsed-data/94fz/", sep="")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which downloaded data is available
regions_list <- as.list(list.files(path=data_unzipped_directory))

# Filter out non-regions in to their own list, remove the log files from list of regions
# others_list <- Filter((function(x) (x %in% c("_FCS_nsi", "nsi"))), regions_list)
regions_list <- Filter((function(x) !grepl('Sevastopol_g', x)), regions_list) # Remove Sevastopol
regions_number <- length(regions_list)

parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))

##############################################
# 3. Define functions to process each region #
##############################################

# Define a function to load a variable from configuration?

# Define a function to set up the key-value table for parsing a given document type
set_up_key_value_table <- function(document_type){
  key_value_ou
}

# Define a function to output the data once parsed
parse_document_key_value <- function(document_counter){
  document_id <- xml_text(xml_find_all(documents_in_this_file_list[document_counter], document_id_field, namespace))
  key_value_output <- do.call(rbind.fill.matrix, lapply(fields_to_parse, function(x) cbind(document_id[document_counter], as.character(x), iconv(xml_text(xml_find_all(documents_in_this_file_list[document_counter], x, namespace)), from = "UTF-8", to = "UTF-8"))))
  return(key_value_output)
}

parse_document_key_value_lapply <- function(document_counter){
  # document_id <- xml_text(xml_find_all(documents_in_this_file_list[document_counter], document_id_field, namespace))
  key_value_output <- do.call(rbind.fill.matrix, 
                              lapply(fields_to_parse, function(x) 
                                cbind((xml_text(xml_find_all(documents_in_this_file_list[document_counter], document_id_field, namespace)))[document_counter], 
                                      as.character(x), 
                                      iconv(xml_text(xml_find_all(documents_in_this_file_list[document_counter], 
                                                                  x, namespace)), from = "UTF-8", to = "UTF-8"
                                            ))))
  return(key_value_output)
}





# Define a function to do all the hard work parsing, taking two inputs: type of document and region
parse_files_key-value <- function(document_type, current_region){

  # Prepare relevant directories
  from_directory <- paste(data_unzipped_directory, current_region, "/", document_type, sep="") # loads source directory
  to_directory <- paste(data_parsed_directory, current_region, "/", document_type, sep="") # loads target directory
    dir.create(to_directory, recursive=TRUE)
  
  # Generate a list of files from this directory
  files_list <- as.list(list.files(path=from_directory, pattern="xml$", recursive=TRUE, full.names=TRUE))
    files_list_length <- length(files_list) 
  
  # Process the configuration for this type of document
  fields_to_parse <- (parsing_configuration$XMLFieldName[parsing_configuration$DocumentType==document_type]) # loads fields from configuration
    fields_to_parse_length <- length(fields_to_parse)
  variable_names <- parsing_configuration$VariableName[parsing_configuration$DocumentType==document_type]
  #document_id_field <- "/*/*/oos:id" # This is important, defines what is a discrete "document" inside each file

  # Set up a list in which the parsed files will be stored
  files_parsed_into_list <- vector(mode="list", length=files_list_length) # Preallocate this vector at its maximum possible length (number of files to parse)

  # Set a timer
  test_key_value_start_time <- Sys.time() # Set a timer
  
  # Try loading the documents inside these files in to a list
#   documents_per_file_list <- vector(mode="list", length=files_list_length)
#   
#   for (l in 1:files_list_length){
#     # Load a file from the list, then its documents
#     file_to_parse <- read_xml(as.character(files_list[l]))
#       namespace <- xml_ns(file_to_parse)
#     if(length(xml_children(file_to_parse)) < 1) {documents_per_file_list[[l]] <- NA}
#       else {documents_per_file_list[[l]] <- xml_children(file_to_parse)}
#     print(paste(l, " of ", files_list_length, " files loaded in memory", sep=""))
#   }    
### THIS RUNS OUT OF ROOM WITH MOSCOW, WOULD NEED TO BE 10 FILES AT A TIME AT MOST    
  
  # Loop over files in files_list, processing each in to files_parse_into_list
  for (l in 1:files_list_length){
  # temp <-  foreach (l = 1:files_list_length, .combine=rbind, .packages=c("xml2", "plyr"), .verbose=F) %dopar% { # Parallel version
    
    # Load a file from the list
    file_to_parse <- read_xml(as.character(files_list[l]))
      namespace <- xml_ns(file_to_parse)
      
    # Find the documents in this list so we have a primary key, count them
    documents_in_this_file_list <- xml_children(file_to_parse)
      # documents_in_this_file_list <- lapply(documents_in_this_file_list, function(x) xml_children(x))
      # documents_in_this_file_list <- lapply(file_to_parse, function(x) xml_children(x))
      # documents_in_this_file_list <- (xml_find_all(file_to_parse, document_id_field, namespace)) 
        # Reference <oos:id> by full path
      documents_in_this_file_list_length <- length(documents_in_this_file_list)
      documents_in_this_file_parsed <- vector(mode="list", length=documents_in_this_file_list_length)
      documents_in_this_file_list_atomic <- vector(mode="list", length=documents_in_this_file_list_length)
      
    for(d in 1:documents_in_this_file_list_length){
      documents_in_this_file_list_atomic[[d]] <- documents_in_this_file_list[d]
    }
      
    # If fewer than one document in this file, sent NA to files_parsed_into_list
    if(documents_in_this_file_list_length < 1) {files_parsed_into_list[[l]] <- NA}
      else { # Otherwise, set up a key-value table to store parsing results
#         key_value_table <- matrix(NA, nrow=1, ncol=3)
#           dimnames(key_value_table) <- list(NULL, c("document", "key", "value"))
#         key_value_list <- vector(mode = "list", length=documents_in_this_file_list_length)

        test <- sapply(documents_in_this_file_list, function(d) xml_text(xml_find_all(d, document_id_field, namespace)), simplify=T) # Probably sapply/vapply is right, and just need the right syntax
        test_atomic <- lapply(documents_in_this_file_list_atomic, function(d) xml_text(xml_find_all(d, document_id_field, namespace)))
        
        {
          # document_id <- xml_text(xml_find_all(documents_in_this_file_list[document_counter], document_id_field, namespace))
          do.call(rbind.fill.matrix, 
                                      lapply(fields_to_parse, function(x) 
                                        cbind((xml_text(xml_find_all(d, document_id_field, namespace)))[d], 
                                              as.character(x), 
                                              iconv(xml_text(xml_find_all(d, 
                                                                          x, namespace)), from = "UTF-8", to = "UTF-8"
                                              ))))
        })

## Name the list elements with the document_id_field, eg list(J = 8, y = c(1, 2, 3), sigma = c(4, 5, 6))
                
        # Set a counter for placing rows of output in the right place
        k <- 1
          
        # Hack the parser here, move to function later
        # for(d in 1:documents_in_this_file_list_length){
        temp <-  foreach (d = 1:documents_in_this_file_list_length, .packages=c("xml2", "plyr"), .verbose=T) %dopar% { # Parallel version
          # print(paste("Parsing document ", d, " of ", documents_in_this_file_list_length, sep=""))
          # document_id <- xml_text(xml_find_all(documents_in_this_file_list[d], document_id_field, namespace))
          # key_value_output <- do.call(rbind.fill.matrix, lapply(fields_to_parse, function(x) cbind(document_id[d], as.character(x), iconv(xml_text(xml_find_all(documents_in_this_file_list[d], x, namespace)), from = "UTF-8", to = "UTF-8"))))

#           key_value_output <- matrix(NA, nrow=documents_in_this_file_list_length, ncol=3)
#           key_value_output <- lapply(fields_to_parse, function(x)
#              cbind(document_id[d], as.character(x), iconv(xml_text(xml_find_all(documents_in_this_file_list[d], x, namespace)), from = "UTF-8", to = "UTF-8")))

          parse_document_key_value(d) # Probably doesn't work because of something to do with xml addressing?
                    
          # How long is this list?
          # length(key_value_output[,1])
          #key_value_table <- matrix(unlist(key_value_output), ncol=3, byrow=F)
          

          #k <- k + fields_to_parse_length
          #key_value_list[[d]] <- key_value_output
          #documents_in_this_file_parsed[[d]] <- key_value_output
          #documents_in_this_file_parsed[[d]] <- parse_document_key_value(d)
        }
        files_parsed_into_list[[l]] <- do.call(rbind, documents_in_this_file_parsed)
      } # else condition ends here
      print(paste(l, " of ", files_list_length, " files parsed", sep=""))
  }

  test_key_value_duration <- (Sys.time() - test_key_value_start_time)
  print(test_key_value_duration)
  
# Parallel test: 2.07 mins; 2.4
# Single thread: 4.09 mins; 5.23
# Foreach single: 5.3
  
test_output_matrix <- do.call(rbind, files_parsed_into_list)
  dimnames(test_output_matrix) <- list(NULL, c("document", "key", "value"))
  
  

  
  test_output_matrix <- do.call(rbind, (files_parsed_into_list[[2]]))
  test_output_matrix <- do.call(rbind, documents_in_this_file_parsed)
test_output_matrix <- do.call(rbind, lapply(files_parsed_into_list, function(x) [x])))
lapply(files_parsed_into_list, function(x) [x]))


lapply(files_parsed_into_list, "[[", "x")

        # Apply the parsing function to each document already loaded in memory
        # temp <- lapply(documents_in_this_file_list, function(x) xml_children(x))
          
        for(d in 1:documents_in_this_file_list_length){
#     foreach(d = 1:documents_in_this_file_list_length, .packages=c("xml2"), .verbose=T) %dopar% {
      document_to_parse <- xml_children(file_to_parse)[[d]]
#       for (f in 1:fields_to_parse_length){
#         extract_xml_text <- function()
#           lapply(fields_to_parse, xml_text(xml_find_all(document_to_parse, "oos:id", namespace)))
            #fields_by_document_matrix[d, ] <- unlist(lsapply(fields_to_parse, function(x) xml_text(xml_find_all(document_to_parse, x, namespace))), recursive=FALSE)
            document_parsed <- vector(mode = "list", length = fields_to_parse_length)
            document_parsed <- lapply(fields_to_parse, function(x) xml_text(xml_find_all(document_to_parse, 
                                                                                         x, namespace)))
            #document_parsed_field_lengths <- (lapply(document_parsed, function(x) length(x)))
            document_parsed[(lapply(document_parsed, function(x) length(x)))<1] <- NA
            fields_by_document_matrix[d, ] <- unlist(document_parsed)
          
#         variable_temporary <- xml_text(xml_find_all(document_to_parse, fields_to_parse[f], namespace))
#         if(length(variable_temporary) > 0){fields_by_document_matrix[d, f] <- variable_temporary} else{
#           fields_by_document_matrix[d, f] <- NA}
#         }
    }
    files_parsed_into_list[[l]] <- fields_by_document_matrix # Moving this inside the if statement so empty files do nothing
    #} #else {fields_by_document_matrix[d, ] <- NA} # Try removing this, may not be needed now
    } else {files_parsed_into_list[[l]] <- NA} # Try removing this, may not be needed now
    #files_parsed_into_list[[l]] <- fields_by_document_matrix
    #print(paste(l, " of ", files_list_length, " files parsed", sep=""))
  }  

test_parallel_duration <- (Sys.time() - test_parallel_start_time)
print(test_parallel_duration)
#stopImplicitCluster()

row.names(temp) <- NULL
#Altajskij_kraj_notifications_parsed <- data.frame(test)

  output_matrix_name <- paste(current_region, document_type, "parsed", sep="_")
  output_matrix_generate_command <- paste(output_matrix_name, "<- temp", sep="")
  eval(parse(text=output_matrix_generate_command))
  rm(temp)
  output_matrix_file_name <- paste(to_directory, "/", output_matrix_name, ".rda", sep="")
  output_matrix_save_command <- paste("save(", output_matrix_name, ", file=\"", 
                                      output_matrix_file_name, "\")", sep="")
  eval(parse(text=output_matrix_save_command))
} # Function ends here


#########################################################################
# 4. Load and parse a configuration file that tells the code what to do #
#########################################################################

parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))

# Option 2: save your metadata file as a csv (outside R) and load that directly with no need to deal with rJava/xlsx difficulties
# This is riskier as Russian characters in the "example" column may not display well on Windows
# parse_configuration <- read.csv(file="3-Unpack/how-I-parse-the-xml.csv", stringsAsFactors=FALSE)

# What documents does the configuration file tell us to parse?
document_types <- as.list(unique(parsing_configuration$DocumentType))
document_types_number <- length(document_types)

# Loop over the document types to generate a list of fields to be parsed

# Good ideas here for "big bang" approach
# http://stackoverflow.com/questions/18538772/parsing-irregular-xml-in-r
# http://stackoverflow.com/questions/12421668/import-all-fields-and-subfields-of-xml-as-dataframe
# http://stackoverflow.com/questions/22643580/combine-values-in-huge-xml-files



################################################
# 5. Loop over regions to process them in turn #
################################################

# List of regions from 2. above is called here, looped through by region, then documents loop inside
non_parallel_start_time <- Sys.time()
for (r in 1:regions_number){
current_region <- as.character(regions_list[r])

  for (d in 1:document_types_number){
    document_type <- as.character(document_types[d])
    print(paste("Processing ", document_type, " documents from ", current_region, sep=""))
    parse_files(document_type, current_region)
  }
}
non_parallel_duration <- (Sys.time() - non_parallel_start_time)
print(non_parallel_duration)

# Same thing but parallelised
registerDoParallel()

parallel_start_time <- Sys.time()
for (r in 1:regions_number){
  current_region <- as.character(regions_list[r])
  
  for (d in 1:document_types_number){
    document_type <- as.character(document_types[d])
    print(paste("Processing ", document_type, " documents from ", current_region, sep=""))
    parse_files_parallel(document_type, current_region)
  }
}
parallel_duration <- (Sys.time() - parallel_start_time)
print(parallel_duration)


# NOTES: snap! read_xml can look inside a zip file
# Maybe parse out all the contract-level in one file, organisation in another, products, suppliers

document_to_parse <- read_xml(as.character(files_list[1]))
# install.packages('xml2')
# library(xml2)
t <- as_list(document_to_parse)
s <- xml_structure(document_to_parse)
p <- xml_contents(document_to_parse)
y <- xml_ns(document_to_parse)
c <- xml_find_all(document_to_parse, "/*", xml_ns(document_to_parse)) # 3556 nodes in whole document
c <- xml_find_all(document_to_parse, "//oos:customer/oos:regNum/text()", xml_ns(document_to_parse)) # 43
c <- xml_find_all(document_to_parse, "//oos:regNum", xml_ns(document_to_parse)) # 86 (all elements)
c <- xml_find_all(document_to_parse, "/export[@xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"]/contract/oos:regNum/text()", xml_ns(document_to_parse)) # 86 (all elements)

## BINGO BINGO BINGO BINGO BINGO!
## BASED ON THIS, WILL REALLY WANT TO PARSE PRODUCTS OUT IN TO SEPARATE TABLE (or just take first)
y <- xml_ns(document_to_parse)
contract_regNum <- xml_find_all(document_to_parse, "/*/d1:contract/oos:regNum", y) # 43 (contract)
customer_regNum <- xml_find_all(document_to_parse, "/*/d1:contract/oos:customer/oos:regNum", y) # 43 (customer regNum string is subset of contract regNum string)
products_product <- xml_find_all(document_to_parse, "/*/d1:contract/oos:products/oos:product", y) # 59 products across 43 contracts, which are multiples?
products_product_first_only <- xml_find_all(document_to_parse, "/*/d1:contract/oos:products/oos:product[1]", y) # 43
products_product_multiples_only <- xml_find_all(document_to_parse, "/*/d1:contract/oos:products[count(oos:product)=2]", y) # Returns 4!

# Basic joining
contract_regNum_text <- (xml_text(xml_find_all(document_to_parse, "/*/d1:contract/oos:regNum", y))) # working, produces character vector
customer_regNum_text <- (xml_text(xml_find_all(document_to_parse, "/*/d1:contract/oos:customer/oos:regNum", y))) # working, produces character vector
document_vectors_together_manual <- cbind(contract_regNum_text, customer_regNum_text)

# Try preallocation
document_vectors_list <- vector(mode="list", length=2)
document_vectors_list[[1]] <- data.frame(contract_regNum_text)
document_vectors_list[[2]] <- data.frame(customer_regNum_text)
document_vectors_together_preallocated <- data.frame(document_vectors_list)


# Try two documents, preallocated
documents_collated_vector <- vector(mode="list", length=2)

document_vectors_list_one <- vector(mode="list", length=2)
document_vectors_list_one[[1]] <- data.frame(contract_regNum_text)
document_vectors_list_one[[2]] <- data.frame(customer_regNum_text)
document_one_collated <- data.frame(document_vectors_list_one)
documents_collated_vector[[1]] <- document_one_collated

document_vectors_list_two <- vector(mode="list", length=2)
document_vectors_list_two[[1]] <- data.frame(contract_regNum_text)
document_vectors_list_two[[2]] <- data.frame(customer_regNum_text)
document_two_collated <- data.frame(document_vectors_list_two)
documents_collated_vector[[2]] <- document_two_collated

# Combine all documents
documents_collated_together <- rbindlist(documents_collated_vector)
# Memory management: http://stackoverflow.com/questions/20689650/how-to-append-rows-to-an-r-data-frame

# Or pre-construct the data frame from configuration?


# Need to traverse by contracts, then within that send other data off to different data frames
x <- read_xml('
<root xmlns:f = "http://foo.com" xmlns:g = "http://bar.com">
<f:doc><g:baz /></f:doc>
<f:doc><g:baz /></f:doc>
</root>
')
xml_find_all(x, ".//f:doc")
xml_find_all(x, ".//f:doc", xml_ns(x))

# As per XML package
install.packages("XML")
library(XML)
data <- xmlInternalTreeParse(as.character(files_list[1]))
src <- getNodeSet(data, "/contract//regNum")

df <- data.frame(
  regNum=sapply(data["//regNum"], xmlValue))

doc = xmlParse(system.file("exampleData", "tagnames.xml", package = "XML"))
els = getNodeSet(doc, "/doc//a[@status]")

regNum <- getNodeSet(data, "//contract", namespaces = "oos")

# Old way (brute force)
temp <- xmlToList(as.character(files_list[1]), addAttributes=T)
df <- ldply(temp, .fun=function(x) {data.frame(t(unlist(x)))})

# Can I merge all the documents in to one first and then parse it in one go?
temp1 <- xmlToList(as.character(files_list[1]), addAttributes=T)
temp2 <- xmlToList(as.character(files_list[2]), addAttributes=T)
temp3 <- xmlToList(as.character(files_list[3]), addAttributes=T)
# Not going to be a good idea


# Try from here: http://stackoverflow.com/questions/20527926/convert-possibly-malformed-xml-into-data-frame-in-r
# z <- getNodeSet(data, "//*") # So each <oos:xyz> blah blah </oos:xyz> is a separate node
z <- getNodeSet(data, "//oos:customer/oos:regNum") #43 of these customer regnums
z <- getNodeSet(data, "//oos:regNum", c(oos= "http://zakupki.gov.ru/oos/types/1")) #86 if you include the other regnums (2 x 43 is good)
z <- getNodeSet(data, "/*/oos:regNum") #0 visible at top level
z <- getNodeSet(data, "/oos:regNum", c(oos= "http://zakupki.gov.ru/oos/types/1")) # zilch
getNodeSet(data, "/*[local-name()='oos:regNum']", fun=xmlToList) # zilch


# table(xpathSApply(doc, "//FEDREG/child::node()", xmlName))
table(xpathSApply(data, "/contract/oos:customer/child::node()"))



# Try those name references again for grabbing just what I need
r <- xmlRoot(data)
xmlName(r) # Name is "export"
xmlSize(r) # 3 total "docs" inside this xml file

names(r[["contract"]]) # Shows same as xmlSApply(r[[1]], xmlName)
xmlSApply(r[["contract"]], xmlSize) # Same as xmlSApply(r[[1]], xmlSize)

extract.01 <- xmlSApply(r, function(x) xmlSApply(x, xmlValue)) # Doesn't really work?
str(extract.01) # Looks quite close...values and fields are aligned
length(extract.01) # 407 elements
length(extract.01[[1]]) # 16 elements (like above)
extract.01.short <- extract.01[1] # Just take the first record
extract.01.df <- data.frame(t(extract.01),row.names=NULL)


# xpath resources
# http://xmltoolbox.appspot.com/xpath_generator.html
# http://www.zvon.org/xxl/XPathTutorial/General/examples.html



# ENDS