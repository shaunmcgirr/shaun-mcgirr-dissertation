# Script to set up parameters for test

# Run before the loop
source(file="3-Unpack/parse-files-functions.R")

data_parsed_directory <- set_data_subdirectory(data_directory, data_download_date, "parsed")
data_cleaned_directory <- set_data_subdirectory(data_directory, data_download_date, "cleaned")

regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))

r <- 1
d <- 1


# Run within the loop
batch_output_key_value <- batch_output_key_value[1:8809165,]
