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


##############################################
# 3. Define functions to process each region #
##############################################

# Define a function to create a list of fields to parse
# for (f in 1:document_types_number){
#   document_type <- as.character(document_types[f])
#   fields_to_parse <- (parsing_configuration$XMLFieldLocation[parsing_configuration$DocumentType==document_type])
# }


#########################################################################
# 4. Load and parse a configuration file that tells the code what to do #
#########################################################################



################################################
# 5. SCRATCHPAD #
################################################

# Follow Chris Albon's method http://chrisalbon.com/rstats/import-xml.html
document_type <- "notifications"
r <- 1
current_region <- as.character(regions_list[r])
from_directory <- paste(data_unzipped_directory, current_region, "/", document_type, sep="") # loads source directory
to_directory <- paste(data_parsed_directory, current_region, "/", document_type, sep="") # loads target directory
files_list <- as.list(list.files(path=from_directory, pattern="xml$", recursive=TRUE, full.names=TRUE))
files_list_length <- length(files_list) 
document_id_field <- "/*/*/oos:id"

parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))
fields_to_parse <- (parsing_configuration$XMLFieldLocation[parsing_configuration$DocumentType==document_type])
fields_to_parse_length <- length(fields_to_parse)
variable_names <- parsing_configuration$VariableName[parsing_configuration$DocumentType==document_type]
files_parsed_into_list <- vector(mode="list", length=files_list_length) # Preallocate this vector at its maximum possible length (number of files to parse)


# Follow method outlined in the XML book (can't load docs in to a list first because files are too large, especially crypto signature)
library(XML)

# Getting one file right first
file_to_parse <- xmlParse(as.character(files_list[f]))

# files_to_parse_stored_in_list <- vector(mode = "list", length = files_list_length)
# fields_by_file_matrix <- matrix(NA, nrow=xml_files_to_parse_number, ncol=1)

# Non-parallel version
files_parsed_into_list <- vector(mode="list", length=files_list_length) # Preallocate this vector at its maximum possible length (number of files to parse)

files_list_1 <- files_list[1:10]
temp1 <- lapply(files_list_1, function(x) xmlParse(x, useInternalNodes=TRUE)) #FALSE is even worse
temp2 <- lapply(temp1, function(x) xmlChildren(xmlRoot(x), useInternalNodes=TRUE))

files_list_2 <- files_list[11:20]
temp1 <- lapply(files_list_2, xmlParse)
temp2 <- lapply(temp1, function(x) xmlChildren(xmlRoot(x)))
# So the problem is simply memory addressing: what you load in once tends to stick around even once processed

test_start_time <- Sys.time()
for(f in 1:files_list_length){
#  file_to_parse <- xmlParse(as.character(files_list[f]))
  file_to_parse <- sapply(as.character(files_list[f]), xmlParse)
#   documents_in_this_file_list <- xmlChildren(xmlRoot(file_to_parse))
#     documents_in_this_file_list_length <- length(documents_in_this_file_list)
#   if(documents_in_this_file_list_length > 0){
#     fields_by_document_matrix <- matrix(NA, nrow=documents_in_this_file_list_length, 
#                                         ncol=2)
#     #dimnames(fields_by_document_matrix) <- list(NULL, variable_names)
#     for(d in 1:documents_in_this_file_list_length){
#       document_to_parse <- xmlChildren(documents_in_this_file_list[[d]])
#       fields_by_document_matrix[d, 1] <- xmlValue(document_to_parse[["id"]]) # These are named references to R list objects, nothing to do with XML
#       fields_by_document_matrix[d, 2] <- xmlValue(document_to_parse[["notificationNumber"]])
#       fields_by_document_matrix[d, 3] <- xmlValue(document_to_parse[["versionNumber"]])
#       fields_by_document_matrix[d, 4] <- xmlValue(document_to_parse[["publishDate"]])
#       fields_by_document_matrix[d, 5] <- xmlValue((document_to_parse[["placingWay"]])[["name"]])
#       fields_by_document_matrix[d, 6] <- xmlValue(document_to_parse[["orderName"]])
#       fields_by_document_matrix[d, 7] <- xmlValue(((document_to_parse[["order"]])[["placer"]])[["regNum"]])
#       fields_by_document_matrix[d, 8] <- xmlValue(((document_to_parse[["order"]])[["placer"]])[["fullName"]])
#     }} else {fields_by_document_matrix <- NA}
#   files_parsed_into_list[[f]] <- fields_by_document_matrix
  #free(file_to_parse)
  print(paste(f, " of ", files_list_length, " files parsed", sep=""))
}
test_duration <- difftime(Sys.time(), test_start_time, units="secs")
print(test_duration)
# Small region
# 2 fields: 11.6 secs
# 8 fields: 22 secs
# Moskva
# 2 fields: 
# 8 fields: 


# Parallel version
files_parsed_into_list <- vector(mode="list", length=files_list_length) # Preallocate this vector at its maximum possible length (number of files to parse)

test_parallel_start_time <- Sys.time()
temp <- foreach(f = 1:files_list_length, .packages=c("XML")) %dopar% {
  file_to_parse <- xmlParse(as.character(files_list[f]))
  documents_in_this_file_list <- xmlChildren(xmlRoot(file_to_parse))
  documents_in_this_file_list_length <- length(documents_in_this_file_list)
  if(documents_in_this_file_list_length > 0){
    fields_by_document_matrix <- matrix(NA, nrow=documents_in_this_file_list_length, 
                                        ncol=8)
    #dimnames(fields_by_document_matrix) <- list(NULL, variable_names)
    for(d in 1:documents_in_this_file_list_length){
      document_to_parse <- xmlChildren(documents_in_this_file_list[[d]])
      fields_by_document_matrix[d, 1] <- xmlValue(document_to_parse[["id"]])
      fields_by_document_matrix[d, 2] <- xmlValue(document_to_parse[["notificationNumber"]])
      fields_by_document_matrix[d, 3] <- xmlValue(document_to_parse[["versionNumber"]])
      fields_by_document_matrix[d, 4] <- xmlValue(document_to_parse[["publishDate"]])
      fields_by_document_matrix[d, 5] <- xmlValue((document_to_parse[["placingWay"]])[["name"]])
      fields_by_document_matrix[d, 6] <- xmlValue(document_to_parse[["orderName"]])
      fields_by_document_matrix[d, 7] <- xmlValue(((document_to_parse[["order"]])[["placer"]])[["regNum"]])
      fields_by_document_matrix[d, 8] <- xmlValue(((document_to_parse[["order"]])[["placer"]])[["fullName"]])
    }} else {fields_by_document_matrix <- NA}
  files_parsed_into_list[[f]] <- fields_by_document_matrix
  #print(paste(f, " of ", files_list_length, " files parsed", sep=""))
}
temp <- do.call(rbind, temp)
test_parallel_duration <- difftime(Sys.time(), test_parallel_start_time, units="secs")
print(test_parallel_duration)
print(test_duration)



library(XML)
xmlfile <- xmlTreeParse(as.character(files_list[2]))
xmltop <- xmlRoot(xmlfile)
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat),row.names=NULL)


# Try dumping each document inside a file in to a list first (doesn't seem sustainable on these data)

documents_in_this_directory_list <- vector(mode="list", length=0)

# Preamble to the test
stopImplicitCluster()
registerDoParallel()
#files_list_length <- 10

# Standard loop
files_to_parse_list <- vector(mode="list", length=0)
test_start_time <- Sys.time()
for(l in 1:files_list_length){
#for(l in 1:20){
files_to_parse_list[[l]] <- read_xml(as.character(files_list[l]), verbose=F)
}
test_duration <- (Sys.time() - test_start_time)
print(test_duration)

# Foreach loop
files_to_parse_list <- vector(mode="list", length=0)
test_start_time <- Sys.time()
files_to_parse_list <- foreach(l = 1:files_list_length, .packages=c("xml2"), .verbose=F) %dopar% {
  #files_to_parse_list[[l]] <- read_xml(as.character(files_list[l]), verbose=F)
  read_xml(as.character(files_list[l]), verbose=F)
}
test_duration <- (Sys.time() - test_start_time)
print(test_duration)

# Standard non-parallel: 56 secs, 14.7 secs
# do: 
# dopar: 

#documents_in_this_directory_list_length <- length(documents_in_this_directory_list)


# Now run over this list processing it

# fields_by_document_matrix <- matrix(NA, nrow=documents_in_this_directory_list_length, 
#                                     ncol=fields_to_parse_length)
fields_by_document_matrix <- matrix(NA, nrow=0, ncol=fields_to_parse_length)
dimnames(fields_by_document_matrix) <- list(NULL, variable_names)
for(d in 1:documents_in_this_directory_list_length){
#foreach(d = 1:documents_in_this_directory_list_length, .packages=c("xml2")) %do% {
  document_to_parse <- documents_in_this_directory_list[[d]]
  document_to_parse_result_list <- vector(mode="list", length=0)
  for (f in 1:fields_to_parse_length){
    variable_temporary <- xml_text(xml_find_all(document_to_parse, fields_to_parse[f], namespace))
    #if(length(variable_temporary) > 0){fields_by_document_matrix[, f] <- variable_temporary} else{
      #fields_by_document_matrix[, f] <- NA}
    document_to_parse_result_list[[f]] <- variable_temporary
  }
  temp <- do.call(cbind, document_to_parse_result_list)
  fields_by_document_matrix <- rbind(fields_by_document_matrix, temp)
}


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