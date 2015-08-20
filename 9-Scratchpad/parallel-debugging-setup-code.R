#Code for setting up comparisons of parser runtime

library(xml2)
library(magrittr)
library(openxlsx)
library(plyr)
library(dplyr) # Always load last!

data_directory <- "E:/Data/zakupki/"
data_download_date <- "2015-06-13"

data_unzipped_directory <- paste(data_directory, data_download_date, "/", "zakupki-", data_download_date, "-unzipped-data/94fz/", sep="")
data_parsed_directory <- paste(data_directory, data_download_date, "/", "zakupki-", data_download_date, "-parsed-data/94fz/", sep="")

#regions_list <- as.list("Adygeja_Resp")
regions_list <- as.list("Altajskij_kraj")
regions_list <- Filter((function(x) !grepl('Sevastopol_g', x)), regions_list) # Remove Sevastopol
regions_number <- length(regions_list)

r <- 1
current_region <- as.character(regions_list[r])
parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))
document_type <- "notifications"
document_type <- "contracts"

library(foreach)
library(doParallel)
registerDoParallel()

from_directory <- paste(data_unzipped_directory, current_region, "/", document_type, sep="") # loads source directory
to_directory <- paste(data_parsed_directory, current_region, "/", document_type, sep="") # loads target directory
dir.create(to_directory, recursive=TRUE)
fields_to_parse <- (parsing_configuration$XMLFieldName[parsing_configuration$DocumentType==document_type]) # loads fields from configuration
fields_to_parse_length <- length(fields_to_parse)
variable_names <- parsing_configuration$VariableName[parsing_configuration$DocumentType==document_type]
files_list <- as.list(list.files(path=from_directory, pattern="xml$", recursive=TRUE, full.names=TRUE))
files_list_length <- 5
#files_list_length <- length(files_list) 
document_id_field <- "/*/*/oos:id"
files_parsed_into_list <- vector(mode="list", length=files_list_length)


## Not run
STOP
## Region: "Altajskij_kraj", document: "contracts", rows: 262179, time: 2.87 hours, 5.9MB saved
## Region: "Altajskij_kraj", document: "notifications", rows: 70970, time: 14.86 mins, 2.6MB saved

