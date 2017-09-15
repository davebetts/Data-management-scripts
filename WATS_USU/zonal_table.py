##http://mheris.com/zonalstatistics/

# Import modules
import os
import math

# Import arcpy module
import arcpy
from arcpy import env
from arcpy.sa import *

arcpy.CheckOutExtension("Spatial")

# working directory
path = r'C:\Users\davebetts\Dropbox\GIS\Projects\GSL_WEAP_djb\DEMs\DEM_10m'
##path = r'C:\Users\a01987147\Dropbox\GIS\Data processing\python_script'
path = path.replace('\\', '/')

#input and output parameters

##set the workspace address
arcpy.env.workspace = path
arcpy.env.overwriteOutput = True
os.chdir(path)
##set the zone feature class
zones= 'bands_at250m.shp'
##set the zone field
zoneField="FID"
##set the raster address or name if it is in your workspace.
inValueRaster = u"dem_10m"
## set the output table name
outputTable=env.workspace+"\\bands_elev"
noDataOption="NODATA"
##set statistics type: MEAN, SUM, MAX, MIN…. OR ALL
zonalSummaryType="MEAN"
##Set splite number. if you have a million feature, 10-15 would be good.
splitNumber=10
oidfield= 'FID'

rows=arcpy.SearchCursor(zones)
countFeature=0
for row in rows:
    countFeature+=1
print 'Number of features in the zone dataset is: ', countFeature

divisionNumber = int((countFeature/splitNumber)+1)

partsCount=(int((countFeature/divisionNumber)+1))
print 'each part has ' + str(divisionNumber) + ' features'
tableList=[]

for i in range(0,partsCount):
    arcpy.MakeFeatureLayer_management (zones, "zonelyr")
    selected=arcpy.SelectLayerByAttribute_management ("zonelyr", "NEW_SELECTION", '"FID" >=' +str(divisionNumber*i) + 'AND "FID" <' + str((i+1)*divisionNumber))
    print 'selection is done for part ' + str (i+1)
    Output= arcpy.CreateFeatureclass_management(env.workspace, "selected"+str(i)+".shp", "POLYGON", zones)
    print 'the layer for part ' + str(i+1)+' is created'
    arcpy.CopyFeatures_management(selected,Output)
    print 'selected features of part '+str(i+1)+' are being copied...'
    try:
        outZSaT = ZonalStatisticsAsTable("selected"+str(i)+".shp", zoneField, inValueRaster,"tblPart"+str(i), noDataOption, zonalSummaryType)
        tablePart='tblPart'+str(i)
        tableList.append(env.workspace+"\\"+tablePart)
        print 'zonal analysis for part ' +str(i+1) +' is done'
    except:
        print 'I got an error; skiping this part'

    arcpy.Delete_management("selected" +str(i) +".shp")

arcpy.Merge_management(tableList, outputTable)
print 'tables are merged'
for i in range(0,len(tableList)):
    try:    
        arcpy.Delete_management("tblPart" +str(i))
    except:
        pass
print 'Owesome!! we are done'
