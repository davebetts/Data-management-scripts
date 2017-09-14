# ---------------------------------------------------------------------------
# averageHUCs2.py
# Created on: 2015-01-13
#   Curtis Gray
#
# Description: Identify the HUCs for each climate point
#              and then get the average value for each HUC
#
# Usage: averageHUCs.py <precip_file.shp> <HUC_file.shp>
#  
# ---------------------------------------------------------------------------

# Import modules
import os

# Import arcpy module
import arcpy
from arcpy import env
from arcpy.sa import *

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

# working directory
path = r'C:\Users\davebetts\Dropbox\GIS\Data processing\python_script'
##path = r'C:\Users\a01987147\Dropbox\GIS\Data processing\python_script'
path = path.replace('\\', '/')
outVarRaster = "outvar"

# set workspace
arcpy.env.workspace = path
arcpy.env.overwriteOutput = True
os.chdir(path)


# I had to hard code the files, above would not work
##HUC_file = "Basins_Merge_Dissolve.shp"
HUC_file = "bands_huc12.shp"
##Precip = "XYp1985.shp"
##out_cover = 'p_HUC.shp'
##Precip = "XYp2085.shp"
##out_cover = 'p2_HUC.shp'
Precip = "XYt1985.shp"
out_cover = 't_HUC.shp'
##Precip = "XYt2085.shp"
##out_cover = 't2_HUC.shp'
##Precip = "XYr1985.shp"
##out_cover = 'r_HUC.shp'
##Precip = "XYr2085.shp"
##out_cover = 'r2_HUC.shp'


# Create a new fieldmappings and add the two input feature classes.
fieldmappings = arcpy.FieldMappings()
fieldmappings.addTable(HUC_file)
fieldmappings.addTable(Precip)

# Set local variables
cellSize = 2000
outVarRaster = "outvariance"
lagSize = 2000
majorRange = 2.6
partialSill = 542
nugget = 0

# Set complex variables
kModelOrdinary = KrigingModelOrdinary("SPHERICAL", lagSize,
                                majorRange, partialSill, nugget)
kRadius = RadiusFixed(20000, 1)

# make field map merge rules for each precip field 
field_names = [f.name for f in arcpy.ListFields(Precip)]

for date in field_names[4:]: 
    print date
    FieldIndex = fieldmappings.findFieldMapIndex(date)
    fieldmap = fieldmappings.getFieldMap(FieldIndex)
    field = fieldmap.outputField # Get the output field's properties as a field object
    fieldmap.mergeRule = "mean"  # Set the merge rule to mean and then replace the old fieldmap
    fieldmappings.replaceFieldMap(FieldIndex, fieldmap)

# Execute Kriging
outKriging = Kriging(Precip, date, kModelOrdinary, "#",
                     kRadius, outVarRaster)
# Execute ZonalStatisticsAsTable
##outZSaT = ZonalStatisticsAsTable(HUC_file, zoneField, outKriging, 
##                                 outTable, "NODATA", "MEAN")
##
### Set local variables
##inZoneData = "zones.shp"
##zoneField = "Classes"
##inValueRaster = "valueforzone"

##
##
##

##    fieldmap.mergeRule = "mean"  # Set the merge rule to mean and then replace the old fieldmap
##
##    
### run Spatial Join with new fieldmappings set to 'mean' 
##arcpy.SpatialJoin_analysis(HUC_file, Precip, out_cover, "#", "#", fieldmappings)
##
##
##
##
##
##
##
##
##
### Save the output 
##outKriging.save("C:/sapyexamples/output/krigoutput02
