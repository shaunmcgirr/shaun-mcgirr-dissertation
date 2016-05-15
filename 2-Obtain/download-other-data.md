#Download other data

Aside from data described in "download-raw-procurement-data.md" you might wish to re-download the other data I used. I describe how to do this by each source below.

The script 3-Unpack/load-other-data.R assumes you have the necessary files and that:
- files tracked by Git are located in 2-Obtain/data_other/
- files not tracked by Git (for size or licensing reasons) are located wherever you defined downloads_directory to be in integrate.R


## Clearspending.ru data on budgets for the russian regions
These are available for 2011-2015 from http://clearspending.ru/regions/list/2015/ and the other years linked from that page.

Hovering over the top heading cell shows a blue "download" link, choose .csv format and save the file not as "download" but something informative like "clearspending-regional-budgets-YYYY.csv" in the 2-Obtain/data_other/clearspending directory.

If you open this with LibreOffice/OpenOffice/MS Office, beware it is delimited not by commas but semi-colons, and you need UTF-8 character encoding. Alternatively, run the script 3-Unpack/load-other-data.R to extract clean data from all the files and join it up.

## Global Corruption Barometer
Transparency International's Global Corruption Barometer is a semi-annual survey of citizens' experiences with corruption across more than 100 countries. Per-country summary data is available at http://www.transparency.org/research/gcb/

Pre-aggregated data covering all countries is available at the link above, and those files are tracked by this Git repository. To reproduce, download the following link and unzip the folder contained in the zip file to 2-Obtain/data_other/gcb
http://files.transparency.org/content/download/615/2602/file/GCB2013_DataPack.zip

Detailed survey microdata is only available by contacting the research department Transparency International and requesting a Memorandum of Understanding for researchers. These files are not tracked.

## Varieties of Democracy
For time-series/cross-national analysis I use several sources, including the Varieties of Democracy project at https://v-dem.net/en/data/

The project measures in fine detail many aspects of political institutions, and the "V-Dem + other" version of the dataset also provides 300+ country-year measures from other sources.

The size of the Country_Year_V-Dem_other_SPSS_v6.1.zip download is prohibitive for tracking by Git, so unzip it in to the folder specified by downloads_directory

