# (djb) the following script is a modification of the scripts in the following discussion thread: https://gis.stackexchange.com/questions/206559/python-script-zonal-stats-as-table-loop-question

This solution will allow you to process all of the raster files in a selected folder instead of naming the raster files individually.  I had 936 rasters I needed to analyze, so the ability to loop through the available rasters was almost a necessity for me. 
# (djb) This script should be able to process at once all of the rasters created with kriging_djb.py

The output from this script is an individual .dbf file (and associated files) for each of the rasters that you process.

import arcpy, os, arcinfo
from arcpy import env
from arcpy.sa import *
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension("Spatial")

# Select the folder containing raster files.  This script will use ALL of the raster files in the selected folder.
env.workspace = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\kriging"

# Select the shapefile containing the polygons to use as boundaries for zonal statistics
watershedFeat = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\HUC12_djb.shp"

# Select output folder for saving the output - zonal tables (.dbf files)
outDir = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\kriging\\tbls\\" 

# Something goes wrong with this script during use, perhaps with the temporary files.  No error
# messages are given. The "print" statements inserted within the script keep track of where to
# restart.  Replace the '0' in the "for" statement with the most recently printed integer (printing
# of the variable 'ndx').

x = arcpy.ListRasters()
for raster in arcpy.ListRasters()[0:]: # the "0" can be replaced by the most recent result of "print ndx" in order to restart where the code stopped
	print raster 
	ndx = x.index(raster)#+1
	print ndx
	outTable = outDir + raster + ".dbf"
	arcpy.gp.ZonalStatisticsAsTable_sa(watershedFeat,
		"FID", # Select an attribute in the shape file to identify polygons
		raster,
		outTable,
		"NODATA","MEAN")
arcpy.CheckInExtension("Spatial")

# The script still needs some help.  The script pauses several times during the run, but the print statements make resuming the process easier. 

# I also want to combine this script with the scripts that I use before and after this script into a single script.  The steps I follow and a link to the location of these scripts are as follows:
# Kriging and zonal statistics https://github.com/davebetts/davesdata
# 	1. Kriging of gridded climate data over multiple time steps.  Individual rasters are produced for each time step.
# 	2. Zonal statistics of ALL rasters in the selected folder (produced by Step 1)
# 	3. Combine all .dbf files (zonal tables) in working directory (produced by Step 2) into a single table.
#		Columns = zones
#		Rows = time steps