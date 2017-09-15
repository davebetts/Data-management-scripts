# Calculate stream sinuosity for multiple stream reaches

This method uses the field calculator to calculate the sinuosity of each reach along a stream in an attribute field created by the user.  In this example, the stream being studied was divided into reaches of equal lengths because of the type of computer model that was used for the study.

## Pre-processing of the original stream layer:
### Merge stream segments
This step can be skipped if the stream layer is already subdivided into desirable reach segments.  
For this example, selected stream segments were merged to create a single line segment that spanned the entire length of the stream study.  

### Division into stream segments
Divide the stream into reaches, which can be uniform or not.  The lengths of each reach need to be recorded in _a new field_ in the attributes table, which was labled 'Length' in this example.  Reach lengths are a measure of the total channel length.

In this example, the single line segment was divided into reaches of equal lengths using the [Divide Line by Length](http://www.arcgis.com/home/item.html?id=d5d27ee47330434b9a96b91136a0118f) Add-in.  (The Add-in in this example worked with ArcGIS 10.3, in July of 2017).  The text from the preceding link follows below.

> **Divide Line By Length Add-in**
> 
> #### Overview
> 
> An editing command to split a selected line feature into new features based on a distance and retain any remainder length.
> by sean_jones
> Last Modified: December 16, 2013
> 
> #### Description
> 
> The Divide Line By Length add-in allows you to split a selected line into new features based on a length value you enter. If the length entered does not divide evenly into the line’s length, the leftover distance is not allocated among the new features. For example, you select a line that is 100 meters in length. If you enter a value of 20, the line is split into five lines that are each 20 meters long (100 / 20 = 5). However, if you split the same line by 30, the resulting features do not have equal lengths since 30 does not divide evenly into 100 (100 / 30 = 3 with a remainder). The first three features are 30 meters long (3 x 30 = 90) and the last feature’s length is the leftover 10 meters (100 - 90 = 10).
> 
> You must be in an edit session to use this command.
> 
> ##### Installation and use:
> 
> 1. Download and double-click the file.
> 2. To use the add-in, you must first add it to a toolbar in ArcMap. Click the Customize menu and click Customize Mode. Click the Commands tab and type Divide Line in the search box. Drag the Divide Line By Length command from the Editing Labs category onto any toolbar, such as the Editor or Advanced Editing toolbar.
> 3. Click the Edit tool on the Editor toolbar and select the line that you would like to split. 
> 4. Click Divide Line By Length on the toolbar to which you added it. 
> 5. Type the length value you want to use to divide the line.
> 6. Press ENTER to split the line. If the length entered does not divide evenly into the line’s length, the remaining leftover distance is not allocated among the new features.
> 
> Access and Use Constraints
>   
> No special restrictions or limitations on using the item’s content have been provided.

Another suggested solution was to use a function called "points along a line", which would allow you to add points along a line at a distance that you specify.  <-- Needs confirmation

Whether you use uniform reach lengths or not, you need to end up with a field in the attributes table that is a measure of the reach (channel) lengths.

## Add a new field, 'SinuosityIndex', to the stream layer's attributes table
This is the field within which you will calculate the sinuosity index: SI = 'channel length'/'downvalley length'.  'channel length's are the reach lengths -- the values that were calculated and recorded in the field 'Length' in the attributes table in this example.

## Calculate sinuosity index using reach (channel) and downvalley lengths.
SI values are calculated using the Field Calculator which can be accessed by right clicking on the desired field ('SinuosityIndex') within the attributes table.  Reach (channel) lengths are the numerators, and downvalley lengths are the denominators for SI.

Downvalley lengths are the straight line distances between the starting and ending points of the reach segments.  Downvalley lengths were calculated using a method found in [this thread](https://geonet.esri.com/thread/106442), and were simultaneously added to the formulas to calculate SIs within the 'SinuosityIndex' field in the attributes table.

1. Right click on 'SinuosityIndex' and open the the the Field Calculator window
2. Select the "Python" radio button
3. Check the box "Show Codeblock"
4. Enter in the following formula in the 'Pre-Logic Script Code' text box.  **Make sure you have TWO spaces** before the text in lines 2 and 3
```
def CalculateDistance(shape):
  startPoint = shape.firstPoint
  endPoint = shape.lastPoint
 
return math.sqrt( math.pow( startPoint.X - endPoint.X, 2 ) + math.pow( startPoint.Y - endPoint.Y, 2 ))
```
* The lower text box is where you will execute the function.  In this example the text box is labeled 'SinuosityIndex = '.  
5. Enter the following formula for SI in the lower text box, and click 'OK'.
```
!Length! / CalculateDistance( !Shape!)
```
* '!Length!' is the reach (channel) length.
* 'CalculateDistance( !Shape!)' uses the formula in the 'Pre-Logic Script Code' text box to calculate downvalley length.  
6. Click 'Ok' and you're done.

