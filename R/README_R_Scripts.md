Majority of the files in this folder are subsegments of the files in the '/WATS_USU' folder.
All of these files need to be described and catalogued, and comments added to each script.

## collate_excelsheets.R
This script reads individual worksheets in an Excel workbook using the package 'readxl', and collates them into a single table.  A text file containing a selection of the worksheet names is used reduce the number of worksheets that are collated in the final table.

This script was originally part of the file 'gauges.R'.  The development of the R package 'dataRetrieval' (USGS) and a better personal understanding of how work with the format of the original data files has made this chunk of code obsolete.  I'm preserving this code in case I recieve another large workbook that would be better served in a plain text format.

## GoodnessofFit.R
This file has the basic script for using the HydroGoF package.  This file needs to be run and to determine how much to keep. There are other (perhaps better) versions of these commands in the most recent version of GoF_stream.R

## repeated_values.R
This script was used to help with quality control.  I used it to search our series of repeated values which could indicate sensor error.  I'm not sure if this is the final version or which sections of this script were ultimately used.

This script needs editing and attributions.

