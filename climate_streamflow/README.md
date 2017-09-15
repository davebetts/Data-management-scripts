# Description of folder contents:

### Solar_Rad.py
This script estimates the maximum solar radiation that could be expected at a specified locatio, per timestep for each day of the year.  
The estimate is based on observed solar radation values from a single weather station, and applies to expected conditions at the same location.

Estimates of the maximum solar radiation were calulated based on:
* selective averaging of 5 years of continuous solar radiation observations
  * the script excludes potentially erroneous observations based on extreme high or low values
  * the script excludes potentially erroneous observations based on irregularities in the observed values along a time series
* If observations are either excluded or unavailable for a given time step, interpolated values are estimated using quadratic regression.
  * quadratic regressions are calculated for each day of the year from the remaining available observations for that day.

Additional details about this script are found here:  [Multiple iterations of quadratic regression in R](https://stackoverflow.com/questions/29982964/multiple-iterations-of-quadratic-regression-in-r)
