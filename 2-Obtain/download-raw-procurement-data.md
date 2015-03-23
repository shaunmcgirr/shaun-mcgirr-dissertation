#Download raw procurement data

Assuming you have the software requirements installed from step 1, these instructions help you download the raw procurement data from zakupki.gov.ru

## Software requirements for step 2 (Obtain)
2. Obtain: an ftp client and a large disk on which to store the raw data. You need a relatively full-featured ftp client, like LFTP on Linux, or FileZilla on Windows/Mac.

## Steps to download raw procurement data

1. Ensure you have a disk available with about 70GB free space.
  * You will be downloading tens of thousands of zip files en masse, and the entire dump takes this much space.

2. Define a directory structure that makes sense to you, and create these directories on your target machine.
  * I use ~/data/zakupki/YYYY-MM-DD/ ie a generic data directory, with a directory for the procurement (zakupki) dataset, and a folder inside that with the date of my download.
  * Within a download folder I then create a directory with the name of the dataset, download date, and description of the data, so my final working directory is: ~/data/zakupki/YYYY-MM-DD/zakupki-YYYY-MM-DD-raw-data/

3. If using a GUI FTP client like FileZilla, create a connection with the following details:
  * Username: free
  * Password: free
  * Host: ftp.zakupki.gov.ru (this is the ftp backend to zakupki.gov.ru)
  
4. Connect to the FTP host and check you can navigate around.
  * In your GUI tool, connect to the host and have a browse around the directories.
  * If using LFTP on the command line, do the following:
   * If on a remote machine, first open a screen (type `screen`), as this will keep your user session going even if your connection is lost during the download. You can reconnect to this session later with `screen -dr`
   * Type `lftp`
   * In the lftp interface, open your connection with `open -u free,free ftp.zakupki.gov.ru`
   * List directories on the FTP host with `ls`, change directories with `cd`
   * Check your current local working directory (ie your target download location) with `lpwd` and change it with `lcd` like so: `lcd ~/data/zakupki/YYYY-MM-DD/zakupki-YYYY-MM-DD-raw-data/`
   
5. Download Mosvka data to check your connection speed.
  * If using a GUI tool like FileZilla, navigate to the 94fz directory, then copy the Moskva subdirectory to your target download folder. This will probably take hours.
  * If using LFTP, check your target download location is correct (see step above), then change to the 94fz directory with `cd 94fz/` and download Moskva with `mirror Moskva/`
  * After a few hours, check the copy/mirror is still happening in your FTP client, and check progress to completion by monitoring the size of your Moskva directory on the target machine. You'll know how to do this in Windows/Mac, but if on Linux navigate to your target directory (`cd ~/data/zakupki/YYYY-MM-DD/zakupki-YYYY-MM-DD-raw-data/`) and type `du -sh *` to return a list of subdirectories and their sizes. Moskva should be at least 6.0G (as at March 2015).


