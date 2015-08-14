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


# Define a function that will extract what we need from each XML field (the set of XML fields is defined by the metadata file)
#extract_xml_node <- function(XMLFieldLocation, StoreAs, VariableName){
#test_list <- vector('list', 1)
test_list <- list('vector', 1)
extract_xml_node <- function(XMLFieldLocation){
  print(paste("Extracting field ", XMLFieldLocation, sep=""))
  fields_parsed_into_list <- vector(mode="list", length=fields_to_parse_length) # Preallocate this vector
  test <- (xml_text(xml_find_all(document_to_parse, XMLFieldLocation, namespace)))
  fields_parsed_into_list[[f]] <- data.frame(test)
  #print(xml_text(xml_find_all(document_to_parse, XMLFieldLocation, namespace)))
} # Probably need to write a big messy function that works, then abstract later

#document_vectors_list[[2]] <- data.frame(customer_regNum_text)
#document_vectors_together_preallocated <- data.frame(document_vectors_list)

#extract_xml_node("/*/d1:contract/oos:regNum", "ContractRegNum")
extract_xml_node("/*/d1:contract/oos:regNum")

# Define a function that will join up all the data extracted from a single XML file and store it safely
save_extracted_xml <- function(OutputTable, Y, Z){
  
}


# Better general approach
# 1. List of parsed files
# 2. Elements in list are matrices
# 3. Matrix is preallocated with NAs (Columns = fields_to_parse_length, Rows = number of separate documents in the file)
# 4. Parsing fills them in

# Preamble during testing of function below
r <- 48 # 48th in list is Moscow
current_region <- as.character(regions_list[r])
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(xlsx)
parsing_configuration <- na.omit(read.xlsx(file="3-Unpack/how-I-parse-the-xml.xlsx", 1, stringsAsFactors=FALSE))
l <- 1
d <- 1
f <- 1
document_type <- "contracts"

# Define a function to do all the hard work parsing, taking two inputs: type of document and region
parse_files <- function(document_type, current_region){
  from_directory <- paste(data_unzipped_directory, current_region, "/", document_type, sep="") # loads source directory
  to_directory <- paste(data_parsed_directory, current_region, "/", document_type, sep="") # loads target directory
  dir.create(to_directory, recursive=TRUE)
  fields_to_parse <- (parsing_configuration$XMLFieldName[parsing_configuration$DocumentType==document_type]) # loads fields from configuration
  fields_to_parse_length <- length(fields_to_parse)
  variable_names <- parsing_configuration$VariableName[parsing_configuration$DocumentType==document_type]
  files_list <- as.list(list.files(path=from_directory, pattern="xml$", recursive=TRUE, full.names=TRUE))
  files_list_length <- length(files_list) 
  document_id_field <- paste("/*/d1:", "contract", "/oos:id", sep="")
  files_parsed_into_list <- vector(mode="list", length=files_list_length) # Preallocate this vector at its maximum possible length (number of files to parse)
  #data_frame_preallocated <- data.frame(matrix(NA, nrow=))
  for (l in 1:files_list_length){
    # All the action goes here (call separate functions here as necessary)
    file_to_parse <- read_xml(as.character(files_list[l]))
      namespace <- xml_ns(file_to_parse)
    documents_in_this_file_list <- (xml_find_all(file_to_parse, document_id_field,
                                                 namespace)) #Reference <oos:id> by full path
      documents_in_this_file_list_length <- length(documents_in_this_file_list)
    if(documents_in_this_file_list_length > 0) {
    fields_by_document_matrix <- matrix(NA, nrow=documents_in_this_file_list_length, 
                                        ncol=fields_to_parse_length)
    dimnames(fields_by_document_matrix) <- list(NULL, variable_names)
    for(d in 1:documents_in_this_file_list_length){
      document_to_parse <- xml_children(file_to_parse)[[d]]
      for (f in 1:fields_to_parse_length){
        variable_temporary <- xml_text(xml_find_all(document_to_parse, fields_to_parse[f], namespace))
        if(length(variable_temporary) > 0){fields_by_document_matrix[d, f] <- variable_temporary} else{
          fields_by_document_matrix[d, f] <- NA}
        }
      }
    } else {fields_by_document_matrix <- NA}
    files_parsed_into_list[[l]] <- fields_by_document_matrix
  }  
  output_matrix_name <- paste(current_region, document_type, "parsed", sep="_")
  output_matrix_command <- paste(output_matrix_name, "<- do.call(\"rbind\", files_parsed_into_list)", sep="")
  eval(parse(text=output_matrix_command))
  write.csv(Adygeja_Resp_contracts_parsed, file="3-Unpack/Adygeja_Resp_contracts_parsed.csv", row.names=FALSE)
  save(Moskva_contracts_parsed, file="3-Unpack/Moskva_contracts_parsed.rda")
  
    #fields_parsed_into_list <- vector(mode="list", length=fields_to_parse_length) # Preallocate this!
    for (f in 1:fields_to_parse_length){
      fields_by_document_matrix <- matrix(NA, nrow=documents_in_this_file_list_length, ncol=fields_to_parse_length)
        dimnames(fields_by_document_matrix) <- list(NULL, variable_names)
      #tmp <- extract_xml_node(fields_to_parse[f]) #Come back here later once it is clear what this should do
      #print(paste("Extracting field ", fields_to_parse[f], sep=""))
      tmp_index <- (xml_text(xml_find_all(file_to_parse, fields_to_parse[1], namespace))) # Always index the variable by the id variable
        tmp_variable <- (xml_text(xml_find_all(file_to_parse, fields_to_parse[f], namespace)))
      # Need to grab them in the same pass, not in separate passes or it defeats the purpose
      doc <- xml_children(file_to_parse)[[1]]
      doc <- xml_children(file_to_parse)[[2]]
        tmp_combined <- cbind(tmp_index, tmp_variable)
      fields_by_document_matrix[, f] <- tmp_combined[, 2] # Replace the preallocated column with the variable extracted from XML
      #if(length(tmp)>0) fields_parsed_into_list[[f]] <- data.frame(tmp)
      #fields_parsed_into_list[[f]] <- data.frame(tmp)
      #fields_parsed_into_list[[f]]
      #Put together a named data frame here?
    }
    documents_parsed_into_list[[l]] <- (fields_by_document_matrix) # Place the constructed matrix in to a list for safekeeping
    #documents_parsed_into_list <- documents_parsed_into_list[lapply(documents_parsed_into_list,length)>0] # Trim off zero-length lists
    print(paste(l, " of ", files_list_length, " files parsed", sep=""))
    #save(documents_parsed_into_list, file="3-Unpack/test_output.rda")
    #write.xlsx(documents_parsed_into_list, file="3-Unpack/test_output.xlsx") # Works but varnames lost
  }
}


#########################################################################
# 4. Load and parse a configuration file that tells the code what to do #
#########################################################################

# Option 1: use the 'xlsx' package, which depends on java, which may require the code below be uncommented to run
# install.packages('xlsx') 
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
# library(rJava)
library(xlsx)
parsing_configuration <- na.omit(read.xlsx(file="3-Unpack/how-I-parse-the-xml.xlsx", 1, stringsAsFactors=FALSE))

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
for (r in 1:1){
current_region <- as.character(regions_list[r])

  for (d in 1:1){
    document_type <- as.character(document_types[d])
    print(paste("Processing ", document_type, " documents from ", current_region, sep=""))
    parse_files(document_type, current_region)
  }
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