# DO.R, Q.R, and Temperatures.R
These scripts were written as part of a term project.  Comparisions were made between temperature, flow volumes, and dissolved oxygen at three sites along the same river.

Each of these scripts analyze the data from three water quality monitoring stations on the Little Bear River.  Field observations from the water quality stations included data that had not been processed for quality control.  Each script processes one variable: 
* Dissolved oxygen (DO)
* water temperature (Temperatures)
* flow rates (Q).

Each of these scripts include the following functions:
* Processing date and time data (POSIX)
* Quality Control (identifying potential gage errors)
* Ploting minimum, maximum and mean daily values for each gage
* Plotting mean values per day of year for each gage
* Plotting mean values for all three gages in the same plot

The scripts include some QC strategies, but the scripts could potentially be combined into a single file using some loops and file selection within the scripts. Look through the QC portions in particular to see what should be preserved or split into reference files.