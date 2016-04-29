#Download other data

Aside from data in "download-raw-procurement-data.md" you might wish to re-download the other data I used.

## Clearspending.ru data on budgets for the russian regions
These are available for 2011-2015 from http://clearspending.ru/regions/list/2015/ and the other years linked from that page.

Hovering over the top heading cell shows a blue "download" link, choose .csv format and save the file not as "download" but something informative like "clearspending-regional-budgets-YYYY.csv" in the 2-Obtain/data_other/clearspending directory.

If you open this with LibreOffice/OpenOffice/MS Office, beware it is delimited not by commas but semi-colons, and you need UTF-8 character encoding. Alternatively, run the script 3-Unpack/load-other-data.R to extract clean data from all the files and join it up.

## Etc


