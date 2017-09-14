##The following code creates rasters (usign kriging) for each fields of a shapefile. I was asking some parts of the code, but fixed them, so decided just to share the code in case someone needs it.

import arcpy, os.path, glob, string
import string, arcpy, os, string
from arcpy import env
from arcpy.sa import *

env.workspace="C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script"
arcpy.CheckOutExtension("Spatial")

shp = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\XYp2085.shp"

fcList = arcpy.ListFeatureClasses()
#Kriging = 0
Kriging = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\\\s"
outVarRaster = "C:\\Users\\davebetts\\Dropbox\\GIS\\Data processing\\python_script\\kriging"

for fc in fcList:
    m = []
    fieldList = arcpy.ListFields("XYp2085.shp")
    i = 0
    for fld in fieldList:
        if i >= 5:
            k = str(fld.name)
            m.append(k)
        i = i + 1

    
    for fld in m:#- this will read each value in list m.  The range of i is from 0 till the length of the list. The -1 is because the counter i starts at 0.
        outKrig = fld + "k"
        arcpy.gp.Kriging_sa(shp, fld, outKrig, "Spherical 0.012628", "1.26279999998001E-02", "")
 
