#Code for setting up comparisons of parser runtime

library(xml2)
library(magrittr)
library(openxlsx)
library(foreach)
library(doParallel)
library(plyr)
library(dplyr) # Always load last!

#data_directory <- "E:/Data/zakupki/"
data_directory <- "~/data/zakupki/"
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
#document_type <- "notifications"
#document_type <- "contracts"

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
## Laptop with external HDD; Parallel
## Region: "Altajskij_kraj", document: "contracts", rows: 262179, time: 2.87 hours, 5.9MB saved
## Region: "Altajskij_kraj", document: "notifications", rows: 70970, time: 14.86 mins, 2.6MB saved

## Laptop with internal SSD; Parallel
## Region: "Altajskij_kraj", document: "contracts", rows: 262179, time: 3.46 hours, 5.9MB saved
## Region: "Altajskij_kraj", document: "notifications", rows: 70970, time: 17.36 mins, 2.6MB saved

## Laptop with internal SSD; Parallel
## Region: "Adygeja_Resp", document: "contracts", rows: 40244, time: 9.44 mins, 17MB saved (5.1 as DF)
## Region: "Adygeja_Resp", document: "notifications", rows: 20256, time: 1.46 mins, 8.7MB saved

## Laptop with external HDD; Parallel
## Region: "Adygeja_Resp", document: "contracts", rows: 40244, time:  mins, 17MB saved (5.1 as DF)
## Region: "Adygeja_Resp", document: "notifications", rows: 20256, time:  mins, 8.7MB saved

## Server with internal SSD; Non-parallel
## Region: "Adygeja_Resp", document: "contracts", rows: 40244, time: 9.95 mins, 17MB saved (5.1 as DF)
## Region: "Adygeja_Resp", document: "notifications", rows: 20256, time: 1.32 mins, 8.7MB saved

## Server with internal SSD; Parallel
## Region: "Adygeja_Resp", document: "contracts", rows: 40244, time: 7.72 mins, 17MB saved (5.1 as DF)
## Region: "Adygeja_Resp", document: "notifications", rows: 20256, time: 56.6 secs, 8.7MB saved

