# Calculate stream sinuosity for multiple stream reaches

This method uses the field calculator to calculate the sinuosity of each reach along a stream.  In this example the stream reaches are divided into equal lengths to simplify stream modeling.

## Pre-processing:
### Merge stream segments
Stream segments within a shape file were merged to create a single line segment that spanned the entire length of the stream study.

### Division into stream segments
Whether you use uniform reach lengths or not, you need to end up with a field in the attributes table that is a measure of the channel length.

This field was labeled 'Length'.

In this example, the single line segment was divided into reaches of equal lengths using the [Divide Line by Length](http://www.arcgis.com/home/item.html?id=d5d27ee47330434b9a96b91136a0118f) Add-in.  (The Add-in in this example worked with ArcGIS 10.3, in July of 2017)

> **Divide Line By Length Add-in**
> 
> Overview
> 
> Open in ArcGIS Desktop
> An editing command to split a selected line feature into new features based on a distance and retain any remainder length.
> by sean_jones
> Last Modified: December 16, 2013
> 
> Description
> 
> The Divide Line By Length add-in allows you to split a selected line into new features based on a length value you enter. If the length entered does not divide evenly into the line’s length, the leftover distance is not allocated among the new features. For example, you select a line that is 100 meters in length. If you enter a value of 20, the line is split into five lines that are each 20 meters long (100 / 20 = 5). However, if you split the same line by 30, the resulting features do not have equal lengths since 30 does not divide evenly into 100 (100 / 30 = 3 with a remainder). The first three features are 30 meters long (3 x 30 = 90) and the last feature’s length is the leftover 10 meters (100 - 90 = 10).
> 
> You must be in an edit session to use this command.
> 
> Installation and use:
> 
> Download and double-click the file.
> To use the add-in, you must first add it to a toolbar in ArcMap. Click the Customize menu and click Customize Mode. Click the Commands tab and type Divide Line in the search box. Drag the Divide Line By Length command from the Editing Labs category onto any toolbar, such as the Editor or Advanced Editing toolbar.
> Click the Edit tool on the Editor toolbar and select the line that you would like to split. 
> Click Divide Line By Length on the toolbar to which you added it. 
> Type the length value you want to use to divide the line.
> Press ENTER to split the line. If the length entered does not divide evenly into the line’s length, the remaining leftover distance is not allocated among the new features.
> 
> Access and Use Constraints
> No special restrictions or limitations on using the item’s content have been provided.

Another suggested solution was to use a function called "points along a line", which would allow you to add points along a line at a distance that you specify.  <-- Needs confirmation

Whether you use uniform reach lengths or not, you need to end up with a field in the attributes table that is a measure of the channel length.

## Add 'SinuosityIndex' field to the attributes table of the segmented stream
This is where you will calculate the sinuosity index: SI = 'channel length'/'downvalley length'.  'channel length' is the field we labeled 'Length' in our attribute table.

## Calculate 'downvalley length' and sinuosity index.
Measuring the straight line distances (downvalley length) between the start and end points of the reach segments were [calculated using the Field Calculator](https://geonet.esri.com/thread/106442) to fill the 'SinuosityIndex' field in the attribute table.

Inside the Field Calculator window:
* Select the "Python" radio button
* Check the box "Show Codeblock"
* Enter in the following formula.  **Make sure you have TWO spaces** before the text in lines 2 and 3
```
def CalculateDistance(shape):
  startPoint = shape.firstPoint
  endPoint = shape.lastPoint
 
return math.sqrt( math.pow( startPoint.X - endPoint.X, 2 ) + math.pow( startPoint.Y - endPoint.Y, 2 ))
```
* The lower box is where you will execute the function.  In our case the box is labeled 'SinuosityIndex = '.
* Enter the following formula for SI in lower box, and click 'OK'.
```
!Length! / CalculateDistance( !Shape!)
```

'!Length!' is the reach length or channel length field.  'CalculateDistance( !Shape!)' is the downvalley length.
