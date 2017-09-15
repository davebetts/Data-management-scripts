# (djb) the following script is a modification of the scripts in the following discussion thread: https://gis.stackexchange.com/questions/206559/python-script-zonal-stats-as-table-loop-questio
# (djb) This script should be able to process at once all of the rasters created with kriging_djb.py

import arcpy, os, arcinfo
from arcpy import env

from arcpy.sa import *


env.workspace = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\kriging" # where to look for the raster files
arcpy.env.overwriteOutput = True

watershedFeat = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\HUC12_djb.shp" # the shapefile for the zones that will be averaged
outDir = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\kriging\\tbls\\" # Output folder for dbf files

arcpy.CheckOutExtension("Spatial")

# (djb) this script keeps stopping partway through the process so the "print" statements help keep track of where to restart
x = arcpy.ListRasters()
for raster in arcpy.ListRasters()[0:]: # the "0" can be replaced by the most recent result of "print ndx" in order to restart where the code stopped
    print raster 
    ndx = x.index(raster)+1
    print ndx
    outTable = outDir + raster + ".dbf"
    arcpy.gp.ZonalStatisticsAsTable_sa(watershedFeat,"FID",raster,outTable,"NODATA","MEAN") # (djb) the original script used "arcpy.sa.ZonalStatisticsAsTable(..."

arcpy.CheckInExtension("Spatial")
