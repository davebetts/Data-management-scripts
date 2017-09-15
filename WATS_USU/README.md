## GIS: Averaging gridded point data within polygons
The GSL_WEAP study uses three climate variables: precipitation, air temperature, relative humidity.  These climate variables have been represented primarily by single letter abbreviations ('p','t','r') to simplify reading of input data and the writing and formatting of files to be used as model inputs.

### Arithmetic Averaging
#### averageHUCs2.py
_Archived script_  This script is no longer used for the GSL_WEAP project.
'averageHUCs2.py' averages point data within the boundaries of the polygons of a selected shape file.
This script was replaced by 'averageHUCs_djb.py' which made corrections to eliminate omissions of a few fields, and to to account for the format of the most recent input data.

#### averageHUCs2_djb.py
_Archived script_  This script is no longer used for the GSL_WEAP project.
'averageHUCs_djb.py' averages point data within the boundaries of the polygons of a selected shape file.
'averageHUCs2_djb.py' uses arithmetic averaging which was unable to calculate average values for the smaller polygons and misrepresented irreglar shaped polygons that fit between the points of the 4 km gridded climate data.
This script was replaced by the following three scripts: 'kriging_djb.py', 'zonaltables.py', 'dbf_processing.r'

### Kriging and zonal statistics
Three scripts are required to complete the calculation of the average value per polygon, and format the data for use with WEAP.  
#### 1. 'kriging_djb.py'
This script has not been written to process all of the climate variables a once.  The value of the variable "shp" has to be selected for each shape file that needs to averaged.  The inputs for this process are shape files of gridded data created in ArcGIS with file names with this pattern: 'XY#####.shp'

This script uses kriging to create individual raster files interpolating the point data for each time step represented in the .shp file.

#### 2. 'zonaltables.py'
This script will summarize all of the raster files in the working directory into a single table using ArcGIS.  The raster files for the other climate variables which were created in the previous step will be ignored if they are placed in asubfolder.

#### 3. 'dbf_processing.r'
This script formats the .dbf files created in the previous step into the "ReadFromFile" format for WEAP.  The script processes all of the dbf files in the working directory.  The other climate variables will be ignored if they are placed in a subfolder.