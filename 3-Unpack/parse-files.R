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

# Define a function to do all the hard work parsing, taking two inputs: type of document and region

parse_files <- function(document_type, current_region){
  from_directory <- paste(data_unzipped_directory, current_region, "/", document_type, sep="")
  to_directory <- paste(data_parsed_directory, current_region, "/", document_type, sep="")
  dir.create(to_directory)
  files_list <- as.list(list.files(path=from_directory, pattern="xml$", recursive=TRUE, full.names=TRUE))
  files_list_length <- length(files_list)
  for (l in 1:files_list_length){
    # Old way (brute force)
    temp <- xmlToList(as.character(files_list[l]), addAttributes=T)
    df <- ldply(temp, .fun=function(x) {data.frame(t(unlist(x)))})    
    print(paste(l, " of ", files_list_length, " files parsed", sep=""))
  }
}


################################################
# 4. Loop over regions to process them in turn #
################################################

# List of document types to process
document_types <- as.list(c("contracts", "notifications", "protocols")) # only interested in these docs
document_types_number <- length(document_types)

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
library(xml2)
t <- as_list(document_to_parse)
s <- xml_structure(document_to_parse)
p <- xml_contents(document_to_parse)
y <- xml_ns(document_to_parse)
c <- xml_find_all(document_to_parse, "//oos:customer/oos:regNum/text()", xml_ns(document_to_parse)) # 43
c <- xml_find_all(document_to_parse, "//oos:regNum", xml_ns(document_to_parse)) # 86 (all elements)
c <- xml_find_all(document_to_parse, "/export[@xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"]/contract/oos:regNum/text()", xml_ns(document_to_parse)) # 86 (all elements)

## BINGO BINGO BINGO BINGO BINGO!
## BASED ON THIS, WILL REALLY WANT TO PARSE PRODUCTS OUT IN TO SEPARATE TABLE (or just take first)
contract_regNum <- xml_find_all(document_to_parse, "/*/*/oos:regNum", xml_ns(document_to_parse)) # 43 (contract)
customer_regNum <- xml_find_all(document_to_parse, "/*/*/oos:customer/oos:regNum", xml_ns(document_to_parse)) # 43 (customer regNum string is subset of contract regNum string)
products_product <- xml_find_all(document_to_parse, "/*/*/oos:products/oos:product", xml_ns(document_to_parse)) # 59 products across 43 contracts, which are multiples?
products_product_first_only <- xml_find_all(document_to_parse, "/*/*/oos:products/oos:product[1]", xml_ns(document_to_parse)) # 43
products_product_multiples_only <- xml_find_all(document_to_parse, "/*/*/oos:products[count(oos:product)=2]", xml_ns(document_to_parse)) # Returns 4!



c <- xml_find_all(document_to_parse, "//oos:id", xml_ns(document_to_parse)) # 43 (all docs)
c <- xml_find_all(document_to_parse, "/*", xml_ns(document_to_parse)) # 3556 nodes
c <- xml_find_all(document_to_parse, "contract", xml_ns(document_to_parse)) # no nodes called contract
xml_path(xml_find_one(document_to_parse, "*[local-name='contract']", xml_ns(document_to_parse)))
xml_parent(xml_parent(xml_children(document_to_parse)[[2]]))
(xml_parent(xml_children(document_to_parse)[[2]]))
((xml_children(document_to_parse)[[1]])) # So "contract" is just a row, not really treated as a node





xml_attrs(document_to_parse)


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



# ENDS