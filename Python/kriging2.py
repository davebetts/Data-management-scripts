import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.env.overwriteOutput = True

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

##In_Point = r"C:\Users\davebetts\Dropbox\GIS\Data processing\python_script\XYp1985.shp" #(Point feature name:r001_mean, r002_mean.....r012_mean )
##Out_Raster = r"C:\Users\davebetts\Dropbox\GIS\Data processing\python_script\temp_djb"
In_Point = r"C:\Users\a01987147\Dropbox\GIS\Data processing\python_script\XYp1985.shp" #(Point feature name:r001_mean, r002_mean.....r012_mean )
Out_Raster = r"C:\Users\a01987147\Dropbox\GIS\Data processing\python_script\temp_djb"


points = arcpy.ListFeatureClasses()


zFields = "p19850101"

#Kriging Veriable
cellSize = 0.05
lagSize = 0.5780481172534
majorRange = 6
partialSill = 3.304292110
nugget = 0.002701348
kRadius = RadiusFixed(20000, 1)

#Mask region of interest
mask = r"D:\Gujarta Shape file\bands_huc12.shp"

for zField in zFields:
    Point = Point_Num[:4]
    kModelUniversalObj = KrigingModelUniversal("LINEARDRIFT", lagSize, majorRange, partialSill, nugget)

    OutKriging = Kriging(inPointFeatures, zField, kModelUniversalObj, cellSize, kRadius)


    #IDWMASk = ExtractByMask(outIDW, mask)
    KrigMask = ExtractByMask(OutKriging, mask)
    #Save outraster as the same name of input
    KrigMask.save("r{}.tif".format(Point_Num))
