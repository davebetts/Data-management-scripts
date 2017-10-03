# DO.R, Q.R, and Temperatures.R
### These scripts are for specific data files, and are archived to keep track of work done for a term project.
*There are quality control strategies discussed within the scripts that might be worthwhile to separate into generalized scripts.*

Each of these scripts analyze the data from three water quality monitoring stations on the Little Bear River.  Comparisions were made between temperature, flow volumes, and dissolved oxygen at three sites.
Field observations from the water quality stations included data that had not been processed for quality control.  Each script processes one variable: 
* Dissolved oxygen (DO)
* water temperature (Temperatures)
* flow rates (Q).

**The original data are no longer available at the cited website.  The data may have been transfered to other data sites used by Jeff Horsburgh's lab.**

Each of these scripts assisted with the following analyses:
* Processing date and time data (POSIX)
* Quality Control (identifying potential gage errors)
* Ploting minimum, maximum and mean daily values for each gage
* Plotting mean values per day of year for each gage
* Plotting mean values for all three gages in the same plot

## Improving and combining scripts
These are early scripts in R for data processing (December 2014), but the concepts might be worth revisiting.
* There is repetition within each script and between the scripts that could be abreviated with loops.
  * Portions of each script were repeated in order to process the data at each of the three sites.
* There was additional data processing performed outside of the script that should have been integrated within the script.
  * Initial formating of the raw data
  * Plotting the raw and summarized data
  * QC analysis and processing