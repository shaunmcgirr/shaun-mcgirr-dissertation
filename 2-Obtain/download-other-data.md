#Download other data

Aside from data in "download-raw-procurement-data.md" you might wish to re-download the other data I used.

## Clearspending.ru data on budgets for the russian regions
These are available for 2011-2015 from http://clearspending.ru/regions/list/2015/ and the other years linked from that page.

Hovering over the top heading cell shows a blue "download" link, choose .csv format and save the file not as "download" but something informative like "clearspending-regional-budgets-YYYY.csv" in the 2-Obtain/data_other/clearspending directory.

If you open this with LibreOffice/OpenOffice/MS Office, beware it is delimited not by commas but semi-colons, and you need UTF-8 character encoding. Alternatively, run the script 3-Unpack/load-other-data.R to extract clean data from all the files and join it up.ß

## Global Corruption Barometer
Transparency International's Global Corruption Barometer is a semi-annual survey of citizens' experiences with corruption across more than 100 countries. Per-country summary data is available at http://www.transparency.org/research/gcb/

Pre-aggregated data covering all countries is available at the link above, and those files are tracked by this Git repository. To reproduce, download the following link and unzip the folder contained in the zip file to 2-Obtain/data_other/gcb
http://files.transparency.org/content/download/615/2602/file/GCB2013_DataPack.zip

Detailed survey microdata is only available by contacting the research department Transparency International and requesting a Memorandum of Understanding for researchers. These files are not tracked.

The script 3-Unpack/load-other-data.R assumes you have the necessary files and have defined the location of untracked files in integrate.R using the downloads_directory variable. 


