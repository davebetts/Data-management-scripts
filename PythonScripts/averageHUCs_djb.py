#############################################################################
# Script: Modifications to the original script "averageHUCs2.py" 
# edited by: David J. Betts
# Script last modified: 5/24/2016
# The original averageHUCs.py was corrected.  The original script omitted a fiew fields
# Further modifications were made to account for the format of the most recent input data
#############################################################################

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

arcpy.CheckOutExtension("Spatial")

# working directory
path = r'C:\Users\davebetts\Dropbox\GIS\Data processing\python_script'
path = path.replace('\\', '/')

# set workspace
arcpy.env.workspace = path
arcpy.env.overwriteOutput = True
os.chdir(path)

# Script arguments
#Precip = arcpy.GetParameterAsText(0)
#HUC_file = arcpy.GetParameterAsText(1)
#out_cover = Precip.replace('events.shp', 'avg.shp')

# Toggle on/off comment status for the variables 'Precip' and 'out_cover'
HUC_file = "HUC12_djb.shp"
##Precip = "XYp1985.shp"
##out_cover = 'p_HUC.shp'
##Precip = "XYp2085.shp"
##out_cover = 'p2_HUC.shp'
##Precip = "XYt1985.shp"
##out_cover = 't_HUC.shp'
##Precip = "XYt2085.shp"
##out_cover = 't2_HUC.shp'
##Precip = "XYr1985.shp"
##out_cover = 'r_HUC.shp'
Precip = "XYr2085.shp"
out_cover = 'r2_HUC.shp'


# Create a new fieldmappings and add the two input feature classes.
fieldmappings = arcpy.FieldMappings()
fieldmappings.addTable(HUC_file)
fieldmappings.addTable(Precip)

# make field map merge rules for each precip field 
field_names = [f.name for f in arcpy.ListFields(Precip)]

for date in field_names[4:]: 
    print date
    FieldIndex = fieldmappings.findFieldMapIndex(date)
    fieldmap = fieldmappings.getFieldMap(FieldIndex)
    field = fieldmap.outputField # Get the output field's properties as a field object
    fieldmap.mergeRule = "mean"  # Set the merge rule to mean and then replace the old fieldmap
    fieldmappings.replaceFieldMap(FieldIndex, fieldmap)

    
# run Spatial Join with new fieldmappings set to 'mean' 
arcpy.SpatialJoin_analysis(HUC_file, Precip, out_cover, "#", "#", fieldmappings)
