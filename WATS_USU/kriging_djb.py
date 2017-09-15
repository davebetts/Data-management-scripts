#####################################################################################
# Script: Iterate kriging (spatial analysis) of point data in ArcGIS shape files
# Created: March 2017, for GSL_WEAP project
# Iterations loop through value fields of shape file
# Adapted by David J. Betts through minor modification of the following post: https://geonet.esri.com/thread/23597
# all comments from djb
#####################################################################################

import arcpy, os.path, glob, string
import string, arcpy, os, string
from arcpy import env
from arcpy.sa import *

env.workspace="C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script"
arcpy.CheckOutExtension("Spatial")

# toggle on/off comment status to select shape files for spatial analysis
shp = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\XYp1985.shp" # precipitation for 1985 to 2010
# shp = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\XYt1985.shp" # air temperature for 1985 to 2010
# shp = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\XYr1985.shp" # relative humidity for 1985 to 2010

fcList = arcpy.ListFeatureClasses()
Kriging = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\\\s"
outVarRaster = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\kriging"

for fc in fcList:
    m = []
    fieldList = arcpy.ListFields("XYp1985.shp")
    i = 0
    for fld in fieldList:
        if i >= 4: # this number determines the number of columns to skip in the data table "XY#####.shp" to find the first field with values to interpolate (kriging)
            k = str(fld.name)
            m.append(k) # ¿to make the output files easily identifiable?
        i = i + 1

    
    for fld in m:
        outKrig = fld + "k"
        arcpy.gp.Kriging_sa(shp, fld, outKrig, "Spherical 0.012628", "1.26279999998001E-02", "")
