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

# working directory
path = r'C:\CG_data\WEAP_Kriging\GIS'
path = path.replace('\\', '/')

# set workspace
arcpy.env.workspace = path
arcpy.env.overwriteOutput = True
os.chdir(path)

# Script arguments
#Precip = arcpy.GetParameterAsText(0)
#HUC_file = arcpy.GetParameterAsText(1)
#out_cover = Precip.replace('events.shp', 'avg.shp')

# Things to change to re-run, replace all precips (or RH) with 
# whatever you are summarizing AND  change the prefix in the date
# if statement below.
# I had to hard code the files, above would not work
Precip = "Ta_events.shp"
HUC_file = "HU12_Final_proj_clean_10-14.shp"
out_cover = 'Ta_HUC.shp'

# Create a new fieldmappings and add the two input feature classes.
fieldmappings = arcpy.FieldMappings()
fieldmappings.addTable(HUC_file)
fieldmappings.addTable(Precip)

# make field map merge rules for each precip field 
field_names = [f.name for f in arcpy.ListFields(Precip)]

for date in field_names: 
    if 't' in date:
        if '_' in date:
            print date
            FieldIndex = fieldmappings.findFieldMapIndex(date)
            fieldmap = fieldmappings.getFieldMap(FieldIndex)
            field = fieldmap.outputField # Get the output field's properties as a field object
            fieldmap.mergeRule = "mean"  # Set the merge rule to mean and then replace the old fieldmap
            fieldmappings.replaceFieldMap(FieldIndex, fieldmap)
            
 
# run Spatial Join with new fieldmappings set to 'mean' 
arcpy.SpatialJoin_analysis(HUC_file, Precip, out_cover, "#", "#", fieldmappings)