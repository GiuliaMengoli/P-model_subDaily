# P-model_subDaily
# Scipts to run the P model (Wang et al., 2017; Stocker et al., 2020) at half-hourly timestep according to the two methods: the running mean and the weighted mean approaches
# This code is deposited in this repository for the submission of the manuscript to the JAMES journal

# There are two main scripts (main_script_running_mean; main_script_weighted_mean) to use to run and obtain the model simulations at half-hourly timestep at each site, separately.
# Both main scripts require a given number of functions and a settings file. While the functions are saved in the 'R' folder the settings files are stored in the 'settings files' folder.

# All the required functions contains their own descriptions in the R scripts.

# In settings files folder there are some examples of settings files to have a template to use for each approach (e.g NOON, DAILY, 3-HOUR and 'weighted mean approach'). To run the model on another site, it is necessary to change the site name, site elevation and data paths in the setting file.

# The figures in the manuscript were generated using the R scripts stored in the 'Figs' folder.

# Author contact: Giulia Mengoli, gmengoli@ic.ac.uk or giulia.mengoli@gmail.com
# Acknowledgement: this project is funded by the ERC-funded project REALM (grant number 787203)

# Please contact the author prior to any usage of the code
