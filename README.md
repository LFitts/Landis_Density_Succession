# Landis_Density_Succession

#This repository will have a compilation of the files I will be using for the Landis component of my dissertation. Each file will have a different purpose and the title should reflect what the files will do. Most of the work will be done in R unless otherwise specified.

#Disturbance_variable.R will first create a new disturbance variable from the FIA tree and plot tables. One that variable is created, it will statistically compare both the condition-level disturbance variable (from the FIA data) with the newly created disturbance variable.

#Scale.R will work with creating and populating the Landis cells. Here I will use different number of subplots to populate a 90m by 90m cell. The subplots will be treated as a statistical sample which will create a distribution for the species list. Spacial autocorrelation will be considered when using more than one subplot.

#Scenario_file.R This file will produce text files for each of the individual Landis cells. It should run through a loop that will populate each text file from the FIA tables.

#Land_use_plus.R This file will contain the instructions for the land use plus extension on Landis
